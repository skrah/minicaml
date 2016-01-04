(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Shared


(**********************************************************************)
(*                                Types                               *)
(**********************************************************************)

(* Translate position *)
let mk_loc = Location.make

(* type expression *)
type core_type =
  { core_type_desc: core_type_desc;
    core_type_loc:  Location.t }

(* type expression desc *)
and core_type_desc
  = Ptyp_any (* _: type parameter *)
  | Ptyp_var of string (* 'a: type parameter *)
  | Ptyp_arrow of core_type * core_type (* t1 -> t2 *)
  | Ptyp_tuple of core_type list (* t1 * ... * tn *)
  | Ptyp_record of field list (* { l1 : t1; ...; ln : tn } *)
  | Ptyp_constr of string * core_type list (* constr | t1 constr | (t1, ..., tn) constr *)
    (* More like _construction_, i.e. application of a _constructor_ to
       arguments. *)

and field =
  { pfield_name: string;
    pfield_mutable: mutable_flag;
    pfield_type: core_type;
    pfield_loc: Location.t }

type type_declaration =
  { ptyp_name: string;
    ptyp_params: core_type list;
    ptyp_expr: core_type option;
    ptyp_loc: Location.t }


let mk_typany ~pos =
  { core_type_desc = Ptyp_any;
    core_type_loc = mk_loc pos }

let mk_typvar ~name ~pos =
  { core_type_desc = Ptyp_var name;
    core_type_loc = mk_loc pos }

let mk_typarrow ~formals ~result ~pos =
  { core_type_desc = Ptyp_arrow (formals, result);
    core_type_loc = mk_loc pos }

let mk_typ_or_tuple ~lst ~pos =
  match lst with
    [] -> internal_error "mk_typ_maybe_tuple: empty list"
  | [x] -> x
  | _ -> { core_type_desc = Ptyp_tuple lst;
           core_type_loc = mk_loc pos }

let mk_typrecord ~lst ~pos =
  { core_type_desc = Ptyp_record lst;
    core_type_loc = mk_loc pos }

let mk_typconstr ~name ~args ~pos =
  { core_type_desc = Ptyp_constr (name, args);
    core_type_loc = mk_loc pos }

let mk_field ~name ~flag ~typ ~pos =
  { pfield_name = name;
    pfield_mutable = flag;
    pfield_type = typ;
    pfield_loc = mk_loc pos }


(**********************************************************************)
(*                               Values                               *)
(**********************************************************************)

type value_description =
  { pvd_name: string;
    pvd_kind: value_kind;
    pvd_type: core_type option;
    pvd_loc: Location.t }


(**********************************************************************)
(*                           Expressions                              *)
(**********************************************************************)

type expression =
  { pexp_desc: expression_desc;
    pexp_type: core_type option;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_any
  | Pexp_nil
  | Pexp_bool of bool
  | Pexp_int of string
  | Pexp_float of string
  | Pexp_string of string
  | Pexp_path of path
  | Pexp_uminus of expression
  | Pexp_op of expression * binary_operator * expression
  | Pexp_assign of path * assign_operator * expression
  | Pexp_tuple of expression list
  | Pexp_record of (string * expression) list
  | Pexp_array of expression * expression
  | Pexp_if of expression * expression * expression option
  | Pexp_while of expression * expression
  | Pexp_for of value_binding list * expression * expression
  | Pexp_lambda of lambda_expression
  | Pexp_call of expression * expression
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_sequence of expression list

and path
  = Pvar_simple of string * Location.t
  | Pvar_field of path * string * Location.t
  | Pvar_subscript of path * expression * Location.t

and value_binding =
  { pvb_desc: value_description;
    pvb_expr: expression }

and lambda_expression =
  { pfun_name: string;
    pfun_params: value_description list;
    pfun_body: expression }


(**********************************************************************)
(*                               Modules                              *)
(**********************************************************************)

type structure_item
  = Pstr_type of type_declaration list
  | Pstr_primitive of value_binding
  | Pstr_value of rec_flag * value_binding list

type structure = structure_item list

type module_expr = Pmod_structure of structure


(**********************************************************************)
(*                         Create expressions                         *)
(**********************************************************************)

(* Add type constraint *)
let add_constraint ~expr ~typ =
  match expr.pexp_type with
    None -> { expr with pexp_type = Some typ }
  | _ -> internal_error "setting type constraint twice"


(* Atoms *)
let mk_any ~pos =
  { pexp_desc = Pexp_any;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_nil ~pos =
  { pexp_desc = Pexp_nil;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_tuple ~lst ~pos =
  { pexp_desc = Pexp_tuple lst;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_record ~lst ~pos =
  { pexp_desc = Pexp_record lst;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_array ~size ~init ~pos =
  { pexp_desc = Pexp_array (size, init);
    pexp_type = None;
    pexp_loc = mk_loc pos }


(* Other expressions *)
let mk_bool ~value ~pos =
  { pexp_desc = Pexp_bool value;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_int ~number ~pos =
  { pexp_desc = Pexp_int number;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_float ~number ~pos =
  { pexp_desc = Pexp_float number;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_str ~str ~pos =
  { pexp_desc = Pexp_string str;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_path ~path ~pos =
  { pexp_desc = Pexp_path path;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_uminus ~expr ~pos =
  { pexp_desc = Pexp_uminus expr;
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_op ~left ~op ~right ~pos =
  { pexp_desc = Pexp_op (left, op, right);
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_assign ~path ~expr ~op ~pos =
  { pexp_desc = Pexp_assign (path, op, expr);
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_if ~test ~then_expr ~else_expr ~pos =
  { pexp_desc = Pexp_if (test, then_expr, else_expr);
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_while ~test ~body ~pos =
  { pexp_desc = Pexp_while (test, body);
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_for ~loop_var ~pvd_pos ~lo ~hi ~body ~for_pos =
  let desc = { pvd_name = loop_var; 
               pvd_kind = Loopvar;
               pvd_type = None;
               pvd_loc = mk_loc pvd_pos } in
  let binding = { pvb_desc = desc;
                  pvb_expr = lo } in
    { pexp_desc = Pexp_for ([binding], hi, body);
      pexp_type = None;
      pexp_loc = mk_loc for_pos }

let trans_param x =
  match x.pexp_desc with
    Pexp_path (Pvar_simple (name, pos)) ->
      { pvd_name = name;
        pvd_kind = Parameter;
        pvd_type = x.pexp_type;
        pvd_loc = pos }
  | Pexp_any ->
      { pvd_name = "_";
        pvd_kind = Ignore;
        pvd_type = None;
        pvd_loc = x.pexp_loc }
  | _ -> Location.error x.pexp_loc "unsupported parameter"

let mk_lambda ~name ~params ~body ~pos =
  let mk ~name ~params ~body ~pos =
    let desc = Pexp_lambda { pfun_name = name;
                             pfun_params = params;
                             pfun_body = body }
    in { pexp_desc = desc;
         pexp_type = None;
         pexp_loc = mk_loc pos }
  in

  let rec curry = function
        [] -> internal_error "empty list of curried parameters"
      | [p] -> let p' = trans_param p in
                   mk ~name ~params:[p'] ~body ~pos
      | p :: tl -> let p' = trans_param p in
                     mk ~name ~params:[p'] ~body:(curry tl) ~pos
  in

  match params with
    [{ pexp_desc = Pexp_tuple lst; _ }] -> let lst' = List.map trans_param lst in
                                            mk ~name ~params:lst' ~body ~pos
  | lst -> curry lst

let mk_call ~func ~args ~pos =
  let mk f arg =
    { pexp_desc = Pexp_call (f, arg);
      pexp_type = None;
      pexp_loc = mk_loc pos }
  in

  let rec curry f = function
        [] -> internal_error "empty list of curried parameters"
      | [arg] -> mk f arg
      | arg :: tl -> curry (mk f arg) tl
  in curry func args

let mk_opcall ~name ~args ~pos =
  let f = { pexp_desc = Pexp_path (Pvar_simple (name, mk_loc pos));
            pexp_type = None;
            pexp_loc = mk_loc pos }
  in
  let arg = match args with
              [] -> internal_error "empty args"
            | [x] -> x
            | _ -> { pexp_desc = Pexp_tuple args;
                     pexp_type = None;
                     pexp_loc = mk_loc pos }
  in { pexp_desc = Pexp_call (f, arg);
       pexp_type = None;
       pexp_loc = mk_loc pos }

let mk_let ~rec_flag ~bindings ~body ~pos =
  { pexp_desc = Pexp_let (rec_flag, bindings, body);
    pexp_type = None;
    pexp_loc = mk_loc pos }

let mk_sequence ~lst ~pos =
  match lst with
    [] -> internal_error "empty sequence"
  | [x] -> x
  | _ -> { pexp_desc = Pexp_sequence lst;
           pexp_type = None;
           pexp_loc = mk_loc pos }


(**********************************************************************)
(*                            Create paths                            *)
(**********************************************************************)

let mk_simplevar ~name ~pos =
  Pvar_simple (name, mk_loc pos)

let mk_fieldvar ~name ~field ~pos =
  Pvar_field (name, field, mk_loc pos)

let mk_subscriptvar ~name ~subscript ~pos =
  Pvar_subscript (name, subscript, mk_loc pos)


(**********************************************************************)
(*                         Create declarations                        *)
(**********************************************************************)

(* Type declaration: type ('a, 'b) t = texpr *)
let mk_typedec ~name ~params ~expr ~pos =
  { ptyp_name = name;
    ptyp_params = params;
    ptyp_expr = expr;
    ptyp_loc = mk_loc pos }

(* External functions *)
let mk_external ~name ~type_expr ~label ~pos =
  let expr = mk_str ~str:label ~pos in
  let desc = { pvd_name = name;
               pvd_kind = External;
               pvd_type = Some type_expr;
               pvd_loc = mk_loc pos }
  in { pvb_desc = desc; pvb_expr = expr }

let mk_binding ~name ~expr ~pos =
  let desc = { pvd_name = name;
               pvd_kind = Vardec;
               pvd_type = None;
               pvd_loc = mk_loc pos }
  in { pvb_desc = desc; pvb_expr = expr }

let mk_typed_binding ~name ~typ ~expr ~pos =
  let desc = { pvd_name = name;
               pvd_kind = Vardec;
               pvd_type = Some typ;
               pvd_loc = mk_loc pos }
  in { pvb_desc = desc; pvb_expr = expr }

let mk_ignore ~expr ~pos ~typ =
  let desc = { pvd_name = "_";
               pvd_kind = Ignore;
               pvd_type = typ;
               pvd_loc = mk_loc pos }
  in { pvb_desc = desc; pvb_expr = expr }


