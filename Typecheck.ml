(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Shared
open Types
open Ast
open Typedtree


module Make (ModuleState : ModuleState.S) =
struct
open ModuleState

module Unify = Unify.Make(ModuleState)

let rec cx x =
  let desc =
    match x.texp_desc with
      Texp_undef | Texp_closure _ | Texp_ccall _ ->
        internal_error "unexpected expression"
    | Texp_any as d -> d
    | Texp_nil as d -> d
    | Texp_bool _  as d -> d
    | Texp_int _ as d -> d
    | Texp_float _ as d -> d
    | Texp_string _ as d -> d
    | Texp_path p -> Texp_path (cp p)
    | Texp_uminus x -> Texp_uminus (cx x)
    | Texp_op (y, op, z) -> Texp_op (cx y, op, cx z)
    | Texp_assign (p, op, z) -> Texp_assign (cp p, op, cx z)
    | Texp_tuple ys -> Texp_tuple (List.map cx ys)
    | Texp_record fs -> Texp_record (List.map (fun (f, x) -> (f, cx x)) fs)
    | Texp_array (x, y) -> Texp_array (cx x, cx y)
    | Texp_ifthen (x, y) -> Texp_ifthen (cx x, cx y)
    | Texp_ifthenelse (x, y, z) -> Texp_ifthenelse (cx x, cx y, cx z)
    | Texp_while (x, y) -> Texp_while (cx x, cx y)
    | Texp_for (vb, x, y) -> Texp_for (List.map cvb vb, cx x, cx y)
    | Texp_lambda l -> Texp_lambda (cl l)
    | Texp_call (x, y) -> Texp_call (cx x, cx y)
    | Texp_let (flag, vbs, x) -> Texp_let (flag, List.map cvb vbs, cx x)
    | Texp_sequence l -> Texp_sequence (List.map cx l)
  in { x with texp_desc = desc; texp_type = Unify.compress x.texp_type }

and cp p =
  let desc =
    match p.tpath_desc with
      Tvar_simple _ as d -> d
    | Tvar_field (p, f) -> Tvar_field (cp p, f)
    | Tvar_subscript (p, x) -> Tvar_subscript (cp p, cx x)
  in { p with tpath_desc = desc; tpath_type = Unify.compress p.tpath_type }

and cvb v = { tvb_desc = cvd v.tvb_desc; tvb_expr = cx v.tvb_expr }

and cvd v = { v with tvd_type = Unify.compress v.tvd_type }

and cl l =
   { l with tfun_params = List.map cvd l.tfun_params; tfun_body = cx l.tfun_body }

and cstr = function
    Tstr_function _ -> internal_error "unexpected lifted function"
  | Tstr_type _ as ts -> ts
  | Tstr_primitive vb -> Tstr_primitive (cvb vb)
  | Tstr_value (flag, vbs) -> Tstr_value (flag, List.map cvb vbs)

let compress_value_types () =
  let f _ v _ =
    match v.value_type with
      None -> ()
    | Some t -> let t' = Unify.compress t in
                  v.value_type <- Some t'
  in  ValueTable.fold f


let meta_of_str level str =
  let id = Tymeta.make str in
    Tmeta (ref (Unbound (id, level)))

let meta_of_vd level v =
  meta_of_str level (ValueID.name v.avd_id)

let mk_nil level =
  let id = Tymeta.make "<nil>" in
    Tnil (ref (Unbound (id, level)))


let trans_type_desc d =
  { ttyp_id = d.atyp_id;
    ttyp_loc = d.atyp_loc }


let rec trans_structure_item level = function

    Astr_type lst -> Tstr_type (List.map trans_type_desc lst)

  | Astr_primitive d ->
     begin match d.avb_desc.avd_constraint with
       None -> internal_error "missing constraint"
     | Some c -> ValueTable.add_type d.avb_desc.avd_id c;
                 let d' = { tvd_id = d.avb_desc.avd_id;
                            tvd_type = c;
                            tvd_loc = d.avb_desc.avd_loc } in
                 let expr' = trans_expr level d.avb_expr in
                 let b' = { tvb_desc = d'; tvb_expr = expr' }
                 in Tstr_primitive b'
     end

  | Astr_value (rec_flag, bindings) ->
     let bindings' = trans_bindings level rec_flag bindings in
       Tstr_value (rec_flag, bindings')


and check_forbidden_types x =
   match Unify.expand x.texp_type with
     Tnil _ | Tany -> error x.texp_loc "rhs has type 'nil' or type 'any'"
   | _ -> ()

and try_generalize level d x =
  if ValueTable.is_assigned_to d.avd_id then
    Tpoly ([], x.texp_type)
  else Unify.generalize level x.texp_type

and forward_declare level decs =
  let f v =
    let t = meta_of_vd level v in
    ValueTable.add_type v.avd_id t;
  in List.iter f decs

and forward_declare2 level v =
  let t = meta_of_vd level v in
  ValueTable.add_type v.avd_id t; t


and trans_bindings level rec_flag defs =
  let decs = List.map (fun v -> v.avb_desc) defs in
  let exprs = List.map (fun v -> v.avb_expr) defs in

  let do_expr level expr =
    let x = trans_expr level expr in
    check_forbidden_types x; x
  in

  let unify_forward d x =
    Unify.unify (ValueTable.typeof d.avd_id) x.texp_type
  in

  let join level d x =
    let t = try_generalize level d x in
    ValueTable.add_type d.avd_id t;
    { tvb_desc = { tvd_id = d.avd_id;
                   tvd_type = t;
                   tvd_loc = d.avd_loc }; 
      tvb_expr = { x with texp_type = t } }
  in

  match rec_flag with
    Recursive ->
      forward_declare (level+1) decs;
      let exprs' = List.map (do_expr (level+1)) exprs in
      List.iter2 unify_forward decs exprs';
        List.map2 (join level) decs exprs'
  | Nonrecursive ->
      let exprs' = List.map (do_expr (level+1)) exprs in
        List.map2 (join level) decs exprs'


and trans_expr level expr =
  let rec trexpr expr =
  let loc = expr.aexp_loc in
  let expr' = match expr.aexp_desc with

    Aexp_any ->
      { texp_desc = Texp_any;
        texp_type = Tany;
        texp_loc = loc }

  | Aexp_nil ->
      { texp_desc = Texp_nil;
        texp_type = mk_nil level;
        texp_loc = loc }

  | Aexp_bool b ->
      { texp_desc = Texp_bool b;
        texp_type = bool_t;
        texp_loc = loc }

  | Aexp_int z ->
      { texp_desc = Texp_int z;
        texp_type = int64_t;
        texp_loc = loc }

  | Aexp_float x ->
      { texp_desc = Texp_float x;
        texp_type = double_t;
        texp_loc = loc }

  | Aexp_string s ->
      { texp_desc = Texp_string s;
        texp_type = string_t;
        texp_loc = loc }

  | Aexp_array (size, init) ->
      let s = trexpr size in
      let i = trexpr init in
      Unify.unify s.texp_type int64_t;
      let t = Tapp (Tconstr prim_array_id, [i.texp_type]) in
        { texp_desc = Texp_array (s, i);
          texp_type = t;
          texp_loc = loc }

  | Aexp_tuple lst ->
      let lst' = List.map trexpr lst in
      let t = match lst' with
                [] -> unit_t
              | _ -> Ttuple (List.map (fun x -> x.texp_type) lst')
      in { texp_desc = Texp_tuple lst';
           texp_type = t;
           texp_loc = loc }

  | Aexp_record fields ->
      let record_id = FieldTable.validate fields loc in
      let params, tr = TyconTable.destruct record_id in
      let tr' = Unify.subst_with_meta level params tr in
      let fields' = List.map (fun (id, x) -> (id, trexpr x)) fields in
      let expected = match tr' with
                       Trecord flds -> flds
                     | _ -> internal_error "expected record" in
      List.iter2 (fun (_, tf) (_, x) -> Unify.unify tf x.texp_type) expected fields';
        { texp_desc = Texp_record fields';
          texp_type = tr';
          texp_loc = loc }

  | Aexp_uminus expr ->
      let x = trexpr expr in
      Unify.unify x.texp_type int64_t;
        { texp_desc = Texp_uminus x;
          texp_type = int64_t;
          texp_loc = loc }

  | Aexp_op (left, op, right) ->
      let l = trexpr left in
      let r = trexpr right in
      let t = match op with
                Op_plus | Op_minus | Op_times | Op_divide ->
                  Unify.unify l.texp_type int64_t;
                  Unify.unify r.texp_type int64_t;
                  int64_t
              | Op_plusdot | Op_minusdot | Op_timesdot | Op_dividedot ->
                  Unify.unify l.texp_type double_t;
                  Unify.unify r.texp_type double_t;
                  double_t
              | Op_and | Op_or ->
                  Unify.unify l.texp_type bool_t;
                  Unify.unify r.texp_type bool_t;
                  bool_t
              | Op_lt | Op_le | Op_gt | Op_ge | Op_eq | Op_eqeq | Op_ne ->
                  Unify.unify l.texp_type r.texp_type;
                  bool_t
      in { texp_desc = Texp_op (l, op, r);
           texp_type = t;
           texp_loc = loc }

  | Aexp_path path ->
      let p = trans_path level path in
        { texp_desc = Texp_path p;
          texp_type = p.tpath_type;
          texp_loc = loc }

  | Aexp_assign (path, op, expr) ->
      let p = trans_path level path in
      let x = trexpr expr in
      Unify.unify p.tpath_type x.texp_type;
        { texp_desc = Texp_assign (p, op, x);
          texp_type = unit_t;
          texp_loc = loc }

  | Aexp_if (cond, then_expr, None) ->
      let c = trexpr cond in
      let t = trexpr then_expr in
      Unify.unify c.texp_type bool_t;
      Unify.unify t.texp_type unit_t;
        { texp_desc = Texp_ifthen (c, t);
          texp_type = unit_t;
          texp_loc = loc }

  | Aexp_if (cond, then_expr, Some else_expr) ->
      let c = trexpr cond in
      let a = trexpr then_expr in
      let b = trexpr else_expr in
      Unify.unify c.texp_type bool_t;
      Unify.unify a.texp_type b.texp_type;
        { texp_desc = Texp_ifthenelse (c, a, b);
          texp_type = a.texp_type;
          texp_loc = loc }

  | Aexp_while (cond, body) ->
      let c = trexpr cond in
      let b = trexpr body in
      Unify.unify c.texp_type bool_t;
      Unify.unify b.texp_type unit_t;
        { texp_desc = Texp_while (c, b);
          texp_type = unit_t;
          texp_loc = loc }

  | Aexp_for (bindings, hi, body) ->
      let bindings' = trans_bindings level Nonrecursive bindings in
      let h = trexpr hi in
      let b = trexpr body in
      Unify.unify h.texp_type int64_t;
      Unify.unify b.texp_type unit_t;
        { texp_desc = Texp_for (bindings', h, b);
          texp_type = unit_t;
          texp_loc = loc }

  | Aexp_let (rec_flag, bindings, body) ->
      let bindings' = trans_bindings level rec_flag bindings in
      let b = trexpr body in
        { texp_desc = Texp_let (rec_flag, bindings', b);
          texp_type = b.texp_type;
          texp_loc = loc }

  | Aexp_lambda l ->
      let tr a t =
        { tvd_id = a.avd_id;
          tvd_type = t;
          tvd_loc = a.avd_loc }
      in

      let param_t, params =
        match l.afun_params with
          [] -> unit_t, []
        | [x] -> let t = forward_declare2 level x in
                   (t, [tr x t])
        | xs -> let ts = List.map (forward_declare2 level) xs in
                   (Ttuple ts, (List.map2 tr xs ts))
      in

      let return_t = meta_of_str level "<return>" in
      let fun_t = Tarrow (param_t, return_t) in

      let b = trexpr l.afun_body in
      Unify.unify return_t b.texp_type;

      let l' = { tfun_id = l.afun_id;
                 tfun_params = params;
                 tfun_body = b } in
        { texp_desc = Texp_lambda l';
          texp_type = fun_t;
          texp_loc = loc }

  | Aexp_call (expr, arg) ->
      let x = trexpr expr in
      let a = trexpr arg in
      let t = meta_of_str level "<return>" in
      Unify.unify x.texp_type (Tarrow (a.texp_type, t));
        { texp_desc = Texp_call (x, a);
          texp_type = t;
          texp_loc = loc }

  | Aexp_sequence lst ->
      let lst' = List.map trexpr lst in
      let t = match lst' with
                [] -> unit_t
              | _ -> let x = Util.last lst' in x.texp_type
      in { texp_desc = Texp_sequence lst';
           texp_type = t;
           texp_loc = loc }

  in expr'

in trexpr expr

and trans_path level path =

  let rec trpath = function

    Avar_simple (id, scope, loc) ->
      let t = ValueTable.typeof id in
      let t' = Unify.instantiate level t in
        { tpath_desc = Tvar_simple (id, scope);
          tpath_type = t';
          tpath_loc = loc }

  | Avar_field (path, id, loc) ->
      let poly = FieldTable.typeof id in
      let mutable_flag = FieldTable.flagof id in
      let selector_a = Unify.instantiate level poly in

      let p = trpath path in
      let t = meta_of_str level "<field>" in
      let selector_b = Tfield (id, mutable_flag, p.tpath_type, t) in
      Unify.unify selector_a selector_b;
        { tpath_desc = Tvar_field (p, id);
          tpath_type = t;
          tpath_loc = loc }

  | Avar_subscript (path, expr, loc) ->
      let id = prim_array_id in
      let p = trpath path in
      let x = trans_expr level expr in
      let t = meta_of_str level "<array>" in
      Unify.unify p.tpath_type (Tapp (Tconstr id, [t]));
      Unify.unify x.texp_type int64_t;
        { tpath_desc = Tvar_subscript (p, x);
          tpath_type = t;
          tpath_loc = loc }

  in trpath path



let trans_module decls = List.map (trans_structure_item 0) decls


let trans_main (Amod_structure includes, Amod_structure decls) =
  let includes' = trans_module includes in
  let includes'' = List.map cstr includes' in
  let decls' = trans_module decls in
  let decls'' = List.map cstr decls' in
  compress_value_types ();
  (Tmod_structure includes'', Tmod_structure decls'')


end



