(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf
open Shared
open ParseTree
open Ast
open Types


let nodup lst msg =
  let cmp a b = Pervasives.compare a.pvd_name b.pvd_name in
    match Util.find_dup cmp lst with
      None -> ()
    | Some a -> error a.pvd_loc msg

let nodup_type lst msg =
  let cmp a b = Pervasives.compare a.ptyp_name b.ptyp_name in
    match Util.find_dup cmp lst with
      None -> ()
    | Some a -> error a.ptyp_loc msg

let nodup_type_param lst msg =
  let cmp a b =
    match a.core_type_desc, b.core_type_desc with
      Ptyp_var x, Ptyp_var y -> Pervasives.compare x y
    | Ptyp_any, Ptyp_var _ -> -1
    | Ptyp_var _, Ptyp_any -> -1
    | Ptyp_any , Ptyp_any -> -1
    | _ -> internal_error "expected type variable"
  in
    match Util.find_dup cmp lst with
      None -> ()
    | Some a -> error a.core_type_loc msg

let nocyclic_type decs msg =
  let n = List.length decs in
  let forward = List.map (fun t -> t.ptyp_name) decs in
  let tbl = Hashtbl.create n in
  List.iter (fun s -> Hashtbl.add tbl s []) forward;

  let update t constr =
    let depends = Hashtbl.find tbl t in
    Hashtbl.replace tbl t (constr :: depends)
  in

  let rec mk_depends s t =
    match t.core_type_desc with
      Ptyp_any -> ()
    | Ptyp_var _ -> ()
    | Ptyp_arrow (t1, t2) -> mk_depends s t1; mk_depends s t2
    | Ptyp_tuple ts -> List.iter (mk_depends s) ts
    | Ptyp_record _ -> () (* cycles may go through records *)
    | Ptyp_constr (constr, args) ->
       (* Hack: scc does not distinguish between self cycles and no cycles. *)
       if constr = s then error t.core_type_loc msg;
       if List.exists (fun t -> constr = t.ptyp_name) decs then
         update s constr;
       List.iter (mk_depends s) args
  in

  List.iter (fun t ->
               match t.ptyp_expr with
                 None -> ()
               | Some x -> (mk_depends t.ptyp_name) x) decs;

  let components = Scc.scc_hash tbl in
    match Util.find_exn (fun l -> List.length l > 2) components with
      None -> ()
    | Some c ->
       let s = List.hd c in
       let d = List.find (fun d -> d.ptyp_name = s) decs in
         error d.ptyp_loc msg


(** Return a module that manages unique identifiers and scopes *)
module Make (ModuleState : ModuleState.S) =
struct
open ModuleState


module Unify = Unify.Make(ModuleState)


let rec trans_structure_item ctx = function

    Pstr_type lst ->
     let ctx', lst' = trans_typedec_block ctx lst in
       (ctx', Astr_type lst')

  | Pstr_primitive binding ->
     let ctx', (bindings' : Ast.value_binding list) = trans_bindings ctx Nonrecursive [binding] in
       (ctx', Astr_primitive (List.hd bindings'))

  | Pstr_value (rec_flag, bindings) ->
     let ctx', bindings' = trans_bindings ctx rec_flag bindings in
       (ctx', Astr_value (rec_flag, bindings'))

and trans_typedec_block ctx decs =
    nodup_type decs "duplicate type binding in declaration block";
    nocyclic_type decs "cyclic type declaration";
    let ctx', forward = first_pass (ctx, decs) in
    let decs' = List.map (trans_typedec ctx' forward) decs in
      (ctx', decs')

and first_pass (ctx, decs) =
    let f ctx d = TyconTable.add_env ctx d.ptyp_name in
    Util.fold_map f ctx decs

and trans_typedec ctx forward t =
    let ctx', params = trans_type_params ctx t.ptyp_params in
    let id = TyconTable.find_env ctx' t.ptyp_name t.ptyp_loc in
    let expr = trans_type_expr ctx' forward (Some (id, params)) t.ptyp_expr in
    let f = Tfun (params, expr) in
    let id = TyconTable.add_tbl ctx' t.ptyp_name f in
      { atyp_id = id;
        atyp_loc = t.ptyp_loc }

and trans_type_params ctx params =
    nodup_type_param params "duplicate type variable in type declaration";
    Util.fold_map TyvarTable.add ctx params

and trans_type_expr ctx forward special = function
    None -> internal_error "unsupported typedec"
  | Some t -> trans_texpr ctx forward special t

and trans_texpr ctx forward special =

  let rec trtyp t =

  match t.core_type_desc with

    Ptyp_any -> Tany

  | Ptyp_var name ->
     let id = TyvarTable.find_env ctx name t.core_type_loc in Tvar id

  | Ptyp_arrow (t1, t2) ->
     let t1' = trtyp t1 in
     let t2' = trtyp t2 in
       Tarrow (t1', t2')

  | Ptyp_tuple ts ->
     let ts' = List.map trtyp ts in
       Ttuple ts'

  | Ptyp_record fields ->
     let do_field f =
       let fld = FieldTable.add_id f in
       let t = trtyp f.pfield_type in
       let tycon, params = match special with
                             None -> internal_error "need record info"
                           | Some x -> x in
       let args = List.map (fun v -> Tvar v) params in
       let body = Tfield (fld, f.pfield_mutable,
                    Tapp (Tlink tycon, args), t) in
       let tf = Tpoly (params, body) in
       FieldTable.add_type fld tf; (fld, t)
     in
     let fields' = List.map do_field fields in
       Trecord fields'

  | Ptyp_constr (constr, args) ->
     let args' = List.map trtyp args in
     let id = TyconTable.find_env ctx constr t.core_type_loc in
     let f = if List.mem id forward then Tlink id
             else match TyconTable.find_tbl id t.core_type_loc with
                    Tfun (_, Trecord _) -> Tlink id
                  | tfun when tfun = Types.prim_array -> Tlink id
                  | tfun -> tfun in
       Tapp (f, args')

  in trtyp

and tr_constraint ctx t =

  let rec tr t = match t.core_type_desc with

    Ptyp_any -> Tany

  | Ptyp_var name -> TymetaTable.find ctx name

  | Ptyp_arrow (t1, t2) ->
     let t1' = tr t1 in
     let t2' = tr t2 in
       Tarrow (t1', t2')

  | Ptyp_tuple ts -> Ttuple (List.map tr ts)

  | Ptyp_record _ -> internal_error "record type in type constraint"

  | Ptyp_constr (constr, args) ->
     let args' = List.map tr args in
     let id = TyconTable.find_env ctx constr t.core_type_loc in
     let f = match TyconTable.find_tbl id t.core_type_loc with
               Tfun (_, Trecord _) -> Tlink id
             | tfun when tfun = Types.prim_array -> Tlink id
             | tfun -> tfun in
       Tapp (f, args')

  in tr t

and trans_constraint ctx = function
  | Some t -> let c = tr_constraint ctx t in
              let t' = Unify.generalize (-1) c in
                Some (Unify.compress t')
  | None -> None

and trans_expr ctx expr =

  let constr = trans_constraint ctx expr.pexp_type in
  let loc = expr.pexp_loc in
  let desc = match expr.pexp_desc with

    Pexp_any -> Aexp_any

  | Pexp_nil -> Aexp_nil

  | Pexp_bool b -> Aexp_bool b

  | Pexp_int s ->
      (try Aexp_int (Int64.of_string s)
       with _ -> error loc (sprintf "invalid int: %s" s))

  | Pexp_float s ->
      (try Aexp_float (float_of_string s)
       with _ -> error loc (sprintf "invalid float: %s" s))

  | Pexp_string s -> Aexp_string s

  | Pexp_uminus expr -> Aexp_uminus (trans_expr ctx expr)

  | Pexp_path path -> Aexp_path (trans_path false ctx path)

  | Pexp_assign (path, op, expr) ->
      let path' = trans_path true ctx path in
      let expr' = trans_expr ctx expr in
        Aexp_assign (path', op, expr')
 
  | Pexp_op (left, op, right) ->
      let left' = trans_expr ctx left in
      let right' = trans_expr ctx right in
        Aexp_op (left', op, right')

  | Pexp_call (expr, arg) ->
      let expr' = trans_expr ctx expr in
      let arg' = trans_expr ctx arg in
        Aexp_call (expr', arg')

  | Pexp_record fields -> Aexp_record (List.map (trfield ctx) fields)

  | Pexp_array (size, init) ->
      let size' = trans_expr ctx size in
      let init' = trans_expr ctx init in
        Aexp_array (size', init')

  | Pexp_if (test, expr, None) ->
      let test' = trans_expr ctx test in
      let expr' = trans_expr ctx expr in
        Aexp_if (test', expr', None)

  | Pexp_if (test, then_expr, Some else_expr) ->
      let test' = trans_expr ctx test in
      let then_expr' = trans_expr ctx then_expr in
      let else_expr' = trans_expr ctx else_expr in
        Aexp_if (test', then_expr', Some else_expr')

  | Pexp_while (test, body) ->
      let test' = trans_expr ctx test in
      let body' = trans_expr ctx body in
        Aexp_while (test', body')

  | Pexp_for (bindings, hi, body) ->
      let hi' = trans_expr ctx hi in
      let ctx', bindings' = trans_bindings ctx Nonrecursive bindings in
      let body' = trans_expr ctx' body in
        Aexp_for (bindings', hi', body')

  | Pexp_tuple lst -> Aexp_tuple (List.map (trans_expr ctx) lst)

  | Pexp_sequence lst -> Aexp_sequence (List.map (trans_expr ctx) lst)

  | Pexp_let (rec_flag, bindings, body) ->
      let ctx', bindings' = trans_bindings ctx rec_flag bindings in
      let body' = trans_expr ctx' body in
        Aexp_let (rec_flag, bindings', body')

  | Pexp_lambda l ->
      let ctx' = ScopeTable.add ctx l.pfun_name in
      let ctx'', params = trans_params ctx' l.pfun_params in
      let body = trans_expr ctx'' l.pfun_body in
      let l' = { afun_id = ctx'.scope;
                 afun_params = params;
                 afun_body = body } in
        Aexp_lambda l'

  in { aexp_desc = desc; aexp_type = constr; aexp_loc = loc }

and trfield ctx (name, expr) =
   let id = FieldTable.find_id name expr.pexp_loc in
   let expr' = trans_expr ctx expr in
     (id, expr')

and trans_path assign ctx =

  let rec trpath = function

    Pvar_simple (name, pos) ->
      let id = ValueTable.find_and_update assign ctx name pos in
        Avar_simple (id, ctx.scope, pos)

  | Pvar_field (path, name, pos) ->
      let id = FieldTable.find_id name pos in
      let path' = trpath path in
        Avar_field (path', id, pos)

  | Pvar_subscript (path, expr, pos) ->
      let path' = trpath path in
      let expr' = trans_expr ctx expr in
        Avar_subscript (path', expr', pos)

  in trpath


and trans_params ctx decs =
    nodup decs "duplicate parameter in function declaration";
    trans_vardec_list ctx decs

and trans_vardec_list ctx decs =
    let f ctx v =
      let t = trans_constraint ctx v.pvd_type in
      let ctx', id = ValueTable.add ctx v in
      let v' = { avd_id = id;
                 avd_constraint = t;
                 avd_loc = v.pvd_loc }
      in (ctx', v')
    in Util.fold_map f ctx decs

and trans_bindings ctx rec_flag defs =
  let decs = List.map (fun v -> v.pvb_desc) defs in
  let exprs = List.map (fun v -> v.pvb_expr) defs in
  nodup decs "duplicate variable binding in declaration block";

  let join d x = { avb_desc = d; avb_expr = x } in

  match rec_flag with
    Recursive ->
      let ctx', decs' = trans_vardec_list ctx decs in
      let exprs' = List.map (trans_expr ctx') exprs in
      let defs' = List.map2 join decs' exprs' in
        (ctx', defs')
  | Nonrecursive ->
    let exprs' = List.map (trans_expr ctx) exprs in
    let ctx', decs' = trans_vardec_list ctx decs in
    let defs' = List.map2 join decs' exprs' in
      (ctx', defs')

and trans_module ctx decls =
    let f ctx d =
      TymetaTable.clear ();
      trans_structure_item ctx d
    in Util.fold_map f ctx decls


let trans_main (Pmod_structure includes, Pmod_structure decls) =
  let ctx = ModuleState.mk_main_context "MiniCaml_main" in
  let ctx', includes' = trans_module ctx includes in
  let _, decls' = trans_module ctx' decls in
    (Amod_structure includes', Amod_structure decls')


end



