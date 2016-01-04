(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


type type_description = { atyp_id : Symbol.Tycon.t; atyp_loc : Location.t; }
type value_description = {
  avd_id : Symbol.ValueID.t;
  avd_constraint : Types.t option;
  avd_loc : Location.t;
}
type expression = {
  aexp_desc : expression_desc;
  aexp_type : Types.t option;
  aexp_loc : Location.t;
}
and expression_desc =
    Aexp_any
  | Aexp_nil
  | Aexp_bool of bool
  | Aexp_int of Int64.t
  | Aexp_float of float
  | Aexp_string of string
  | Aexp_path of path
  | Aexp_uminus of expression
  | Aexp_op of expression * Shared.binary_operator * expression
  | Aexp_assign of path * Shared.assign_operator * expression
  | Aexp_tuple of expression list
  | Aexp_record of (Symbol.Tyfield.t * expression) list
  | Aexp_array of expression * expression
  | Aexp_if of expression * expression * expression option
  | Aexp_while of expression * expression
  | Aexp_for of value_binding list * expression * expression
  | Aexp_lambda of lambda_expression
  | Aexp_call of expression * expression
  | Aexp_let of Shared.rec_flag * value_binding list * expression
  | Aexp_sequence of expression list
and path =
    Avar_simple of Symbol.ValueID.t * Symbol.ScopeID.t * Location.t
  | Avar_field of path * Symbol.Tyfield.t * Location.t
  | Avar_subscript of path * expression * Location.t
and value_binding = { avb_desc : value_description; avb_expr : expression; }
and lambda_expression = {
  afun_id : Symbol.ScopeID.t;
  afun_params : value_description list;
  afun_body : expression;
}
and structure_item =
    Astr_type of type_description list
  | Astr_primitive of value_binding
  | Astr_value of Shared.rec_flag * value_binding list
type structure = structure_item list
type module_expr = Amod_structure of structure
