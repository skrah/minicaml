(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf
open Shared
open Types


exception ValidationError
exception CyclicType


module Make (ModuleState : ModuleState.S) =
struct
open ModuleState

module Tvar_env = Map.Make(Tyvar)

let add env k v = Tvar_env.add k v env
let find_env env v =
  try Some (Tvar_env.find v env)
  with Not_found -> None

let tmeta_of_var level v =
  let id = Tymeta.make (Tyvar.name v) in
    Tmeta (ref (Unbound (id, level)))

let id_of_tvar = function
    Tvar a -> a
  | _ -> internal_error "id_of_tvar: unexpected type"


(**********************************************************************)
(*                               Validate                             *)
(**********************************************************************)

let rec validate bound t =
  match t with
    Tany -> ()
  | Tvar _ -> if bound then ()
              else raise ValidationError
  | Tmeta {contents=Unbound _} |
    Tnil {contents=Unbound _} -> ()
  | Tmeta {contents=Link t} |
    Tnil {contents=Link t} -> validate bound t
  | Tfield (_, _, tr, tf) ->
      validate bound tr;
      validate bound tf
  | Ttuple ts -> List.iter (validate bound) ts
  | Trecord fields ->
      List.iter (fun (_, tf) -> validate bound tf) fields
  | Tarrow (t1, t2) ->
     validate bound t1;
     validate bound t2
  | Tpoly ([], body) -> validate bound body
  | Tpoly (_, body) -> validate true body
  | Tapp (Tfun (_, body), args) ->
      validate bound body;
      List.iter (validate bound) args
  | Tapp (_, args) ->
      List.iter (validate bound) args


(**********************************************************************)
(*                             Substitution                           *)
(**********************************************************************)

let rec subst env t =
  match t with
    Tany -> t
  | Tvar v ->
     begin match find_env env v with
       Some u -> u
     | None -> t
     end
  | Tmeta {contents=Link u} |
    Tnil {contents=Link u} ->
     subst env u
  | Tmeta _ | Tnil _ -> t
  | Ttuple ts ->
     let ts' = List.map (subst env) ts in
       Ttuple ts'
  | Trecord fields ->
     let fields' = List.map (fun (f, u) -> (f, subst env u)) fields in
       Trecord fields'
  | Tfield (f, flag, tr, tf) ->
     let tr' = subst env tr in
     let tf' = subst env tf in
       Tfield (f, flag, tr', tf')
  | Tarrow (u, w) ->
     let u' = subst env u in
     let w' = subst env w in
       Tarrow (u', w')
  | Tpoly (params, u) ->
     let params', u' = rename_params params u in
       Tpoly (params', subst env u')
  | Tapp (Tfun (params, body), args) ->
     let body' = subst_with_mapping params args body in
       subst env body'
  | Tapp (link, args) ->
     let args' = List.map (subst env) args in
       Tapp (link, args')

and subst_with_mapping domain range t =
  let env = List.fold_left2 add Tvar_env.empty domain range in
    subst env t

and rename_params params t =
  let dup var = Tyvar.make (Tyvar.name var) in
  let params' = List.map dup params in
  let tvars = List.map (fun v -> Tvar v) params' in
  let t' = subst_with_mapping params tvars t in
    (params', t')

and subst_with_meta level domain t =
  let range = List.map (tmeta_of_var level) domain in
    subst_with_mapping domain range t


(**********************************************************************)
(*                              Expansion                             *)
(**********************************************************************)

let rec expand = function
    Tapp (Tfun (params, body), args) ->
      subst_with_mapping params args body
  | Tapp (Tlink link, args) ->
      let tcon = TyconTable.really_find link in
        expand (Tapp (tcon, args))
  | Tmeta {contents=Link t} ->
      expand t
  | t -> t

let rec expand_links = function
  | Tmeta {contents=Link t} ->
      expand_links t
  | t -> t


(**********************************************************************)
(*                            Occurs check                            *)
(**********************************************************************)

(* Test whether a meta variable occurs in t.  Tmeta (Unbound (id_a, level_a))
 * is going to be unified with t.  All unbound meta variables in the entire
 * unified term must be updated to the lowest level of any of them. *)
let occurs a id_a level_a t =
  (* Search for cyclic types and the lowest level. *)
  let rec search n = function
    Tmeta {contents=Unbound (id_b, level_b)} |
    Tnil {contents=Unbound (id_b, level_b)} ->
      if id_a = id_b then raise CyclicType;
      min n level_b
  | Tmeta {contents=Link t} | Tnil {contents=Link t} -> search n t
  | Tfield (_, _, tr, tf) -> min (search n tr) (search n tf)
  | Ttuple ts -> List.fold_left min n (List.map (search n) ts)
  | Trecord fields -> List.fold_left min n (List.map (fun (_, t) -> search n t) fields)
  | Tarrow (t, u) -> min (search n t) (search n u)
  | Tpoly (_, t) -> search n t
  | Tapp (Tfun (_, body), args) ->
      let n' = search n body in List.fold_left min n' (List.map (search n') args)
  | Tapp (Tlink _, args) -> List.fold_left min n (List.map (search n) args)
  | Tapp (Tconstr _, args) -> List.fold_left min n (List.map (search n) args)
  | Tany | Tvar _ -> n
  in

  let rec update n = function
    Tmeta ({contents=Unbound (id_b, _)} as b) |
    Tnil ({contents=Unbound (id_b, _)} as b) ->
      b := Unbound (id_b, n)
  | Tmeta {contents=Link t} | Tnil {contents=Link t} -> update n t
  | Tfield (_, _, tr, tf) -> update n tr; update n tf
  | Ttuple ts -> List.iter (update n) ts
  | Trecord fields -> List.iter (fun (_, t) -> update n t) fields
  | Tarrow (t, u) -> update n t; update n u
  | Tpoly (_, t) -> update n t
  | Tapp (Tfun (_, body), args) ->
      update n body; List.iter (update n) args
  | Tapp (Tlink _, args) -> List.iter (update n) args
  | Tapp (Tconstr _, args) -> List.iter (update n) args
  | Tany | Tvar _ -> ()
  in

  let min_level = search level_a t in
  a := Unbound (id_a, min_level);
  update min_level t


(**********************************************************************)
(*                            Unification                             *)
(**********************************************************************)

let rec unify x y =
  match x, y with
    t1, t2 when t1 == t2 -> ()
  | t1, t2 when t1 = t2 -> ()

  | Tmeta {contents=Link t1}, t2 ->
      unify t1 t2

  | (Tmeta _ as t1), (Tapp (Tfun (_, _), _) as t2) ->
      unify t1 (expand t2)

  | (Tmeta _ as t1), Tmeta {contents=Link t2} ->
      unify t1 t2

  | Tmeta {contents=Unbound (id1, _)}, Tmeta {contents=Unbound (id2, _)}
      when id1 = id2 -> ()

  | Tmeta ({contents=Unbound (id_a, level_a)} as a), t ->
      occurs a id_a level_a t; a := Link t

  | t1, (Tmeta _ as t2) ->
      unify t2 t1

  | Tnil ({contents=Unbound (id_a, level_a)} as a), (Trecord _ as t) ->
      occurs a id_a level_a t; a := Link t
  | (Trecord _ as t), Tnil ({contents=Unbound (id_a, level_a)} as a) ->
      occurs a id_a level_a t; a := Link t

  | Tnil {contents=Link t1}, t2 |
    t1, Tnil {contents=Link t2} ->
      unify t1 t2

  | Tany, _ -> ()
  | _, Tany -> ()
  | Tvar a, Tvar b when a = b -> ()
  | Tfield (f1, flag1, tr1, tf1),
    Tfield (f2, flag2, tr2, tf2) ->
     if f1 <> f2 then
       error Location.none "field type mismatch";
     if flag1 <> flag2 then
       error Location.none "mutable-flag mismatch";
     unify tr1 tr2;
     unify tf1 tf2
  | Ttuple ts, Ttuple us ->
      List.iter2 unify ts us
  | Trecord fields1, Trecord fields2  ->
      List.iter2 (fun (f1, tf1) (f2, tf2) ->
                    if f1 <> f2 then
                      error Location.none "field type mismatch";
                    unify tf1 tf2) fields1 fields2
  | Tarrow (t1, t2), Tarrow (u1, u2) ->
      unify t1 u1;
      unify t2 u2
  | Tpoly (params1, body1), Tpoly (params2, body2) ->
      let range = List.map (fun a -> Tvar a) params1 in
      let body2' = subst_with_mapping params2 range body2 in
        unify body1 body2'


  | Tapp (Tlink l1, args1), Tapp (Tlink l2, args2) ->
     if l1 <> l2 then error Location.none "unification error"
     else List.iter2 unify args1 args2
  | Tapp (Tconstr c1, args1), Tapp (Tconstr c2, args2) ->
     if c1 <> c2 then error Location.none "unification error"
     else List.iter2 unify args1 args2
  | (Tapp (Tlink _, _) as t), u  ->
      unify (expand t) u
  | t, (Tapp (Tlink _, _) as u) ->
      unify t (expand u)
  | (Tapp (Tfun _, _) as t), u ->
      unify (expand t) u
  | t, (Tapp (Tfun _, _) as u) ->
      unify t (expand u)

  | _ -> error Location.none "unification error"


(**********************************************************************)
(*                           Generalization                           *)
(**********************************************************************)

let generalize level t =
  let params = ref [] in
  let n = ref 0 in
  let mk_var () =
    let a = Util.mk_alphanum (Util.post_incr n) in
    let v = Tyvar.make a in
      params := v :: !params; v
  in

  let rec gen t =
    match t with
      Tany -> t
    | Tvar _ -> t
    | Tmeta ({contents=Unbound (_, l)} as a) |
      Tnil ({contents=Unbound (_, l)} as a) ->
        if l > level then
          let v = Tvar (mk_var ()) in
            a := Link v; v
        else t
    | Tmeta {contents=Link t} | Tnil {contents=Link t} -> gen t
    | Tfield (fld, flag, tr, tf) ->
       let tr' = gen tr in
       let tf' = gen tf in
         Tfield (fld, flag, tr', tf')
    | Ttuple ts -> Ttuple (List.map gen ts)
    | Trecord fields ->
       let fields' = List.map (fun (fld, tf) -> (fld, gen tf)) fields in
         Trecord fields'
    | Tarrow (t1, t2) ->
       let t1' = gen t1 in
       let t2' = gen t2 in
         Tarrow (t1', t2')
    | Tpoly (params, body) -> Tpoly (params, gen body)
    | Tapp (Tfun (params, body), args) ->
      let body' = gen body in
      let args' = List.map gen args in
        Tapp (Tfun (params, body'), args')
    | Tapp (link, args) ->
      let args' = List.map gen args in
        Tapp (link, args')
  in

  let t' = gen t in Tpoly (List.rev !params, t')


(**********************************************************************)
(*                            Instantiation                           *)
(**********************************************************************)

let instantiate level t =
  match expand t with
    Tpoly (params, t) ->
      let range = List.map (tmeta_of_var level) params in
        subst_with_mapping params range t
  | t -> t


(**********************************************************************)
(*                            Compression                             *)
(**********************************************************************)

let rec compress t =
  match t with
    Tany -> t
  | Tvar _ -> t
  | Tmeta {contents=Unbound _} |
    Tnil {contents=Unbound _} -> t
  | Tmeta {contents=Link t} |
    Tnil {contents=Link t} -> compress t
  | Tfield (fld, flag, tr, tf) ->
     let tr' = compress tr in
     let tf' = compress tf in
       Tfield (fld, flag, tr', tf')
  | Ttuple ts -> Ttuple (List.map compress ts)
  | Trecord fields ->
     let fields' = List.map (fun (fld, tf) -> (fld, compress tf)) fields in
       Trecord fields'
  | Tarrow (t1, t2) ->
     let t1' = compress t1 in
     let t2' = compress t2 in
       Tarrow (t1', t2')
  | Tpoly ([], body) -> compress body
  | Tpoly (params, body) -> Tpoly (params, compress body)
  | Tapp (Tfun ([], body), []) -> compress body
  | Tapp (Tfun _, _) ->
     compress (expand t)
  | Tapp (link, args) ->
    let args' = List.map compress args in
      Tapp (link, args')


(**********************************************************************)
(*                             Requantify                             *)
(**********************************************************************)

let requantify t =
  let tbl = Hashtbl.create 16 in

  let params = ref [] in
  let n = ref 0 in
  let rename v =
    let a = Util.mk_alphanum (Util.post_incr n) in
    let u = Tyvar.make a in
      params := u :: !params;
      Hashtbl.add tbl v u
  in

  let rec search t =
    match t with
      Tany -> t
    | Tvar v -> (try Tvar (Hashtbl.find tbl v)
                 with Not_found -> rename v; Tvar (Hashtbl.find tbl v))
    | Tmeta _ -> internal_error "unexpected meta variable"
    | Tnil _ -> t
    | Tfield (fld, flag, tr, tf) ->
       let tr' = search tr in
       let tf' = search tf in
         Tfield (fld, flag, tr', tf')
    | Ttuple ts -> Ttuple (List.map search ts)
    | Trecord fields ->
        Trecord (List.map (fun (fld, tf) -> (fld, search tf)) fields)
    | Tarrow (t1, t2) ->
       let t1' = search t1 in
       let t2' = search t2 in
         Tarrow (t1', t2')
    | Tpoly ([], body) -> search body
    | Tpoly (params, body) ->
        List.iter rename params;
        search body
    | Tapp (Tfun _, _) -> internal_error "unexpected tfun"
    | Tapp (Tlink l, args) -> Tapp (Tlink l, List.map search args)
    | Tapp (Tconstr c, args) -> Tapp (Tconstr c, List.map search args)
  in

  let t' = search t in
    (List.rev !params), t'


(**********************************************************************)
(*                              Printing                              *)
(**********************************************************************)

let rec xrepr ~short ~closure ~fullname t =
  let rec _repr = function
    Tany -> "nil"
  | Tvar v -> sprintf "'%s" (Tyvar.repr v)
  | Tmeta {contents=Unbound _} | Tnil {contents=Unbound _} ->
      internal_error "repr: unbound meta variable"
  | Tmeta {contents=Link _} | Tnil {contents=Link _} ->
      internal_error "repr: unexpected meta link"
  | Tfield _ ->
      internal_error "repr: unexpected field"
  | Ttuple ts -> String.concat " * " (List.map _repr ts)
  | Trecord fields as r ->
      if short then
        match FieldTable.record_abbr fields with
          Tapp (Tlink _, args) as link ->
            let args' = List.map id_of_tvar args in
            let poly = Tpoly (args', link) in
            let poly' = instantiate 1 poly in
            unify poly' r;
            let r' = generalize 0 poly' in
              _repr r'
        | _ -> internal_error "repr: invalid field/record link"
      else let fields' = String.concat "; " (List.map repr_field fields) in
             sprintf "{ %s }" fields'
  | Tarrow (t1, t2) ->
      let t1' = _repr t1 in
      let t2' = _repr t2 in
        begin match t1 with
          Tarrow (_, _) ->
            if closure then
              sprintf "< apply : %s -> %s >" t1' t2'
            else
              sprintf "(%s) -> %s" t1' t2'
        | _ ->
            if closure then
              sprintf "< apply : %s -> %s >" t1' t2'
            else
              sprintf "%s -> %s" t1' t2'
        end
  | Tpoly (params, body) ->
      let _ = String.concat ", " (List.map (fun v -> "'" ^ Tyvar.name v) params) in
      let body' = _repr body in
        sprintf "%s" body'
  | Tapp (Tfun (_, _), _) ->
      internal_error "repr: unexpected tfun"
  | Tapp (Tconstr id, args) | Tapp (Tlink id, args) ->
      let constr = Tycon.name id in
      let args' = match List.map _repr args with
                    [] -> ""
                  | [x] -> x ^ " "
                  | xs -> "(" ^ (String.concat ", " xs) ^ ") "
      in sprintf "%s%s" args' constr

  and repr_field = function (id, typ) ->
      let flag = match FieldTable.flagof id with
                   Mutable -> "mutable "
                 | Immutable -> "" in
      sprintf "%s%s : %s" flag (Tyfield.name id) (xrepr ~short:true ~closure ~fullname typ)

  in _repr t


let repr ?(closure=false) ?(fullname=false) t = xrepr ~short:false ~closure ~fullname (compress t)
let short_repr ?(closure=false) ?(fullname=false) t = xrepr ~short:true ~closure ~fullname (compress t)

let _tfun_repr tfun repr =
  match tfun with
    Tfun (params, body) ->
      let params' =
        match List.map (fun v -> "'" ^ (Tyvar.repr v)) params with
          [] -> ""
        | [x] -> x
        | xs ->  "(" ^ (String.concat ", "  xs) ^ ")"
      in params', (repr body)
  | Tlink _ -> internal_error "tfun_repr: unexpected link"
  | Tconstr _ -> internal_error "tfun_repr: unexpected constr"

let tfun_repr ?(closure=false) ?(fullname=false) tfun = _tfun_repr tfun (repr ~closure ~fullname)


end

 



