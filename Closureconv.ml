(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


open Printf


module Make (ModuleState : ModuleState.S) =
struct

open Shared
open ModuleState
open Types
open Typedtree

module Unify = Unify.Make(ModuleState)


type context = { cl_link : Typedtree.value_description;
                 cl_env : Typedtree.value_binding }

let scope_of_env ctxtree =
  let { cl_env; _ } = List.hd ctxtree in
    (ValueTable.really_find cl_env.tvb_desc.tvd_id).value_scope

let id_of_env ctxtree =
  let { cl_env; _ } = List.hd ctxtree in cl_env.tvb_desc.tvd_id


let typedefs = ref []
let functions = ref []
let pushTypeDef b = typedefs := b :: !typedefs
let pushFunction f = functions := f :: !functions

let loc = Location.none

let mk_label id = sprintf "__f%02d_%s" (ValueID.id id) (ValueID.name id)


let fix_valname id =
  match ValueID.name id with
    "(!)" -> "bang"
  | "(:=)" -> "assign"
  | s -> s

let new_field_of_id id =
  let scope = (ValueTable.really_find id).value_scope in
  let f = sprintf "sl%d_%s_%d" (ScopeID.id scope) (fix_valname id) (ValueID.id id) in
    FieldTable.add_id_str f

let field_of_id id =
  let scope = (ValueTable.really_find id).value_scope in
  let f = sprintf "sl%d_%s_%d" (ScopeID.id scope) (fix_valname id) (ValueID.id id) in
    FieldTable.find_id f loc

let use_of_binding scope v =
  let id = v.tvb_desc.tvd_id in
    { texp_desc = Texp_path { tpath_desc = Tvar_simple (id, scope); 
                              tpath_type = v.tvb_desc.tvd_type;
                              tpath_loc = loc };
      texp_type = v.tvb_desc.tvd_type;
      texp_loc = loc }

let use_of_vd scope v =
  let id = v.tvd_id in
    { texp_desc = Texp_path { tpath_desc = Tvar_simple (id, scope); 
                              tpath_type = v.tvd_type;
                              tpath_loc = loc };
      texp_type = v.tvd_type;
      texp_loc = loc }

let path_of_binding scope v =
  let id = v.tvb_desc.tvd_id in
    { tpath_desc = Tvar_simple (id, scope); 
      tpath_type = v.tvb_desc.tvd_type;
      tpath_loc = loc }

let use_of_id typ id =
  let scope = (ValueTable.really_find id).value_scope in
    { texp_desc = Texp_path { tpath_desc = Tvar_simple (id, scope); 
                              tpath_type = typ;
                              tpath_loc = loc };
      texp_type = typ;
      texp_loc = loc }

let link_of_env scope v =
  let id_scope = (ValueTable.really_find v.tvb_desc.tvd_id).value_scope in
  let name = sprintf "sl%d" (ScopeID.id id_scope) in
  let id = ValueTable.add_name scope Parameter name in
  ValueTable.add_type id v.tvb_desc.tvd_type;
  { tvd_id = id;
    tvd_type = v.tvb_desc.tvd_type;
    tvd_loc = loc }

let mk_assign_arrow path expr =
  { texp_desc = Texp_assign (path, Op_assign_arrow, expr);
    texp_type = unit_t;
    texp_loc = loc }

let mk_ignore_vd scope kind =
  let id = ValueTable.add_name scope kind "_" in
  ValueTable.add_type id unit_t;
    { tvd_id = id;
      tvd_type = unit_t;
      tvd_loc = loc }

let mk_expr typ desc =
  { texp_desc = desc;
    texp_type = typ;
    texp_loc = loc }

let mk_binding scope kind name expr =
  let id = ValueTable.add_name scope kind name in
  ValueTable.add_type id expr.texp_type;
    { tvb_desc = { tvd_id = id;
                   tvd_type = expr.texp_type;
                   tvd_loc = loc };
      tvb_expr = expr }

let mk_rec_tblock lst =
  let rec loop l = match l with
    | [] -> []
    | (Tstr_type xs) :: tl -> xs @ loop tl
    | (Tstr_value _) :: tl -> internal_error "expected typedec block"
    | (Tstr_primitive _) :: tl -> internal_error "expected typedec block"
    | (Tstr_function _) :: tl -> internal_error "expected typedec block"
  in  Tstr_type (loop lst)


let rec conv_expression scope ctxtree =

  let rec tr_exp x = match x.texp_desc with

    Texp_undef | Texp_closure _ | Texp_ccall _ ->
      internal_error "unexpected expression"

  | Texp_nil -> x

  | Texp_any -> x

  | Texp_bool _ -> x

  | Texp_int _ -> x

  | Texp_float _ -> x

  | Texp_uminus expr ->
       let expr' = tr_exp expr in
         { x with texp_desc = Texp_uminus expr' }

  | Texp_string _ -> x

  | Texp_path path ->
       let p = tr_path scope ctxtree path in
         { x with texp_desc = Texp_path p;
                  texp_type = p.tpath_type }

  | Texp_assign (path, op, expr) ->
       let path' = tr_path scope ctxtree path in
       let expr' = tr_exp expr in
         begin match op with
           Op_assign_arrow -> { x with texp_desc = Texp_assign (path', op, expr') }
         | Op_assign_ref ->
             let fld = FieldTable.find_id "contents" loc in
             let desc = Tvar_field (path', fld) in
             let path'' = { path' with tpath_desc = desc } in
               { x with texp_desc = Texp_assign (path'', Op_assign_arrow, expr') }
         end

  | Texp_op (left, op, right) ->
       let left' = tr_exp left in
       let right' = tr_exp right in
         { x with texp_desc = Texp_op (left', op, right') }

  | Texp_call (expr, arg) ->

       let mk_call binding arg =
         let path = path_of_binding scope binding in
           { x with texp_desc = (Texp_ccall (path, arg)) }
       in

       let expr' = tr_exp expr in
       let arg' = tr_exp arg in
       let binding = mk_binding scope Vardec "f" expr' in
       let call = mk_call binding arg' in
         mk_expr x.texp_type (Texp_let (Nonrecursive, [binding], call))

  | Texp_record lst ->
       let lst' = List.map (fun (fld, expr) -> (fld, tr_exp expr)) lst in
         { x with texp_desc = Texp_record lst' }

  | Texp_array (size, init) ->
        let size' = tr_exp size in
        let init' = tr_exp init in
          { x with texp_desc = Texp_array (size', init') }

  | Texp_ifthen (test, expr) ->
        let test' = tr_exp test in
        let expr' = tr_exp expr in
          { x with texp_desc = Texp_ifthen (test', expr') }

  | Texp_ifthenelse (test, then_expr, else_expr) ->
        let test' = tr_exp test in
        let then_expr' = tr_exp then_expr in
        let else_expr' = tr_exp else_expr in
          { x with texp_desc = Texp_ifthenelse (test', then_expr', else_expr') }

  | Texp_while (test, body) ->
        let test' = tr_exp test in
        let body' = tr_exp body in
          { x with texp_desc = Texp_while (test', body') }

  | Texp_for (bindings, hi, body) ->
        let bindings' = List.map (tr_binding scope ctxtree) bindings in
        let hi' = tr_exp hi in
        let body' = tr_exp body in
          { x with texp_desc = Texp_for (bindings', hi', body') }

  | Texp_tuple lst ->
       let lst' = List.map tr_exp lst in
         { x with texp_desc = Texp_tuple lst' }

  | Texp_sequence lst ->
       let lst' = List.map tr_exp lst in
         { x with texp_desc = Texp_sequence lst' }

  | Texp_let (_, bindings, body) ->
       let bindings' = List.map (tr_binding scope ctxtree) bindings in
       let body' = tr_exp body in
         { x with texp_desc = Texp_let (Nonrecursive, bindings', body') }

  | Texp_lambda ({ tfun_id; tfun_params; tfun_body; _ } as l) ->
       (* Make a link parameter for the new scope that receives the env
          from the currrent scope. *)
       let { cl_env; cl_link } = List.hd ctxtree in
       let link = link_of_env tfun_id cl_env in
       let params = match tfun_params with
                      [] -> link :: [mk_ignore_vd tfun_id Parameter]
                    | _ -> link :: tfun_params in

       let body' =
         match (ScopeTable.find tfun_id).scope_escapes with
           [] ->
             let hd = { (List.hd ctxtree) with cl_link = link } in
             let ctxtree' = hd :: (List.tl ctxtree) in
               conv_expression tfun_id ctxtree' tfun_body
         | escapes ->
             let escapes = link.tvd_id :: escapes in
             let export_name = sprintf "sl%d" (ScopeID.id tfun_id) in
             (* Push export record type definition: typeid = typeexpr. *)
             let typedef = mkExportTypeDef export_name escapes in
             pushTypeDef (Tstr_type [typedef]);
             (* Make export record variable definition: id = expr. *)
             let env' = mkExportDef tfun_id export_name typedef.ttyp_id escapes in
             let hd = { (List.hd ctxtree) with cl_link = link } in
             let ctxtree' = hd :: (List.tl ctxtree) in
             let new_hd = { cl_link = link; cl_env = env' } in
             let expr = conv_expression tfun_id (new_hd :: ctxtree') tfun_body in
               { texp_desc = Texp_let (Nonrecursive, [env'], expr);
                 texp_type = tfun_body.texp_type;
                 texp_loc = tfun_body.texp_loc }
       in

       let label = mk_label l.tfun_id in
       let env' =
         if (ValueTable.really_find cl_env.tvb_desc.tvd_id).value_scope = scope
         then use_of_binding scope cl_env
         else use_of_vd scope cl_link
       in

       let func = { tfunction_id = tfun_id;
                    tfunction_label = label;
                    tfunction_params = params;
                    tfunction_body = body' } in

       let closure = { texp_desc = Texp_closure (env', label);
                       texp_type = x.texp_type;
                       texp_loc = x.texp_loc } in

       pushFunction func;
       closure

   in tr_exp

and mkExportTypeDef export_name escapes =

  let tycon = Tycon.make export_name in
  let fields = List.map new_field_of_id escapes in
  let types = List.map ValueTable.typeof escapes in
  let tr = Trecord (List.combine fields types) in
  let params, body = Unify.requantify tr in
  let tfun = Tfun (params, body) in
  TyconTable.unsafe_add2 tycon tfun;

  let mk_fld fld t =
    let body = Tfield (fld, Mutable, tr, t) in
    let ft = Tpoly (params, body) in
      FieldTable.add_type fld ft; (fld, t)
  in

  let _ = List.map2 mk_fld fields types in
    { ttyp_id = tycon; ttyp_loc = loc }

and mkExportDef scope export_name tycon escapes =
  let fields = TyconTable.get_record_fields_with_types tycon in
  let params, _ = TyconTable.destruct tycon in
  let args = List.map (fun v -> Tvar v) params in
  let record_type = match args with
                      [] -> Tapp (Tlink tycon, [])
                    | _ -> Tpoly (params, Tapp (Tlink tycon, args))
  in

  let mk_assign (fld, t) id =
    match (ValueTable.really_find id).value_kind with
      External -> internal_error "escaping external symbol"
    | Parameter ->
        let expr = use_of_id t id in (fld, expr)
    | _ -> (fld, mk_expr t Texp_undef)
  in

  let assignment_list = List.map2 mk_assign fields escapes in
  let expr = { texp_desc = Texp_record assignment_list; 
               texp_type = record_type;
               texp_loc = loc } in
    mk_binding scope Vardec export_name expr

and tr_path cur_scope ctxtree p = match p.tpath_desc with

    Tvar_simple (id, _) ->
      if (ValueTable.really_find id).value_escape then
        mkAccess cur_scope id ctxtree
      else p

  | Tvar_field (path, fld) ->
     let path' = tr_path cur_scope ctxtree path in
       { p with tpath_desc = Tvar_field (path', fld) }

  | Tvar_subscript (path, expr) ->
     let path' = tr_path cur_scope ctxtree path in
     let expr' = conv_expression cur_scope ctxtree expr in
       { p with tpath_desc = Tvar_subscript (path', expr') }


and searchAccess id ctxtree =
  let fld = field_of_id id in
  let rec search = function
        [] -> internal_error "Closureconv.searchAccess(): location not found"
      | { cl_env = { tvb_expr = { texp_desc = Texp_record lst; _ }; _ };
          cl_link = link } :: tl ->
          if List.exists (fun (f, _) -> f = fld) lst then [link.tvd_id; id]
          else link.tvd_id :: search tl
      | _ -> internal_error "Closureconv.mkAccess(): expected record"
  in List.rev (search ctxtree)

and mkAccess scope id ctxtree =
  let rec mk_sl = function
        [] -> internal_error "Closureconv.mkAccess(): location not found"
      | [id] -> { tpath_desc = Tvar_simple (id, scope);
                  tpath_type = ValueTable.typeof id;
                  tpath_loc = loc }
      | id :: tl ->
         let p = Tvar_field (mk_sl tl, field_of_id id) in
           { tpath_desc = p;
             tpath_type = ValueTable.typeof id;
             tpath_loc = loc }
  in
  let chain =
    if scope = scope_of_env ctxtree then
      if (ValueTable.really_find id).value_scope = scope then
        [id; id_of_env ctxtree]
      else
        searchAccess id (List.tl ctxtree)
    else
        searchAccess id ctxtree
  in mk_sl chain

and structure_item scope ctxtree = function

  | Tstr_function _ ->
     internal_error "unexpected lifted function"

  | Tstr_type _ as t -> (pushTypeDef t; Tstr_type [])

  | Tstr_primitive _ as h -> h

  | Tstr_value (_, bindings) ->
     let bindings' = List.map (tr_binding scope ctxtree) bindings in
       Tstr_value (Nonrecursive, bindings')

and tr_binding scope ctxtree v =
    let expr' = conv_expression scope ctxtree v.tvb_expr in
    let id = v.tvb_desc.tvd_id in
      if (ValueTable.really_find id).value_escape then
         let access = mkAccess scope id ctxtree in
         let assign = mk_assign_arrow access expr' in
         let lhs = mk_ignore_vd scope Vardec in
           { tvb_desc = lhs; tvb_expr = assign }
      else { v with tvb_expr = expr' }


let conv (Tmod_structure includes, Tmod_structure decls) =
  let main_id = ScopeID.unsafe_make ("MiniCaml_main", 0) in
  let expr = mk_expr unit_t (Texp_tuple []) in
  let env = mk_binding main_id Vardec "sl0" expr in
  let ctxtree = [{ cl_link = env.tvb_desc; cl_env = env }] in
    match (ScopeTable.find main_id).scope_escapes with
      [] -> let decls' = List.map (structure_item main_id ctxtree) decls in
            let types = mk_rec_tblock (List.rev !typedefs) in
            let exports = Tstr_value (Nonrecursive, [env]) in 
            let funcs = Tstr_function (!functions) in
              (Tmod_structure includes, Tmod_structure (types :: exports :: funcs :: decls'))
    | escapes ->
        let escapes = env.tvb_desc.tvd_id :: escapes in
        let export_name = "sl0" in
        (* Push export record type definition: typeid = typeexpr. *)
        let typedef = mkExportTypeDef export_name escapes in
        pushTypeDef (Tstr_type [typedef]);
        (* Make export record variable definition: id = expr. *)
        let env' = mkExportDef main_id export_name typedef.ttyp_id escapes in
        let ctxtree = [{ cl_link = env'.tvb_desc; cl_env = env' }] in
        let decls' = List.map (structure_item main_id ctxtree) decls in
        let types = mk_rec_tblock (List.rev !typedefs) in
        let exports = Tstr_value (Nonrecursive, [env']) in
        let funcs = Tstr_function (!functions) in
          (Tmod_structure includes, Tmod_structure (types :: exports :: funcs :: decls'))


end



