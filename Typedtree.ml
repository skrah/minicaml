(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Shared
open Symbol


(**********************************************************************)
(*                                Types                               *)
(**********************************************************************)

type type_declaration =
  { ttyp_id: Tycon.t;
    ttyp_loc: Location.t }


(**********************************************************************)
(*                              Values                                *)
(**********************************************************************)

type value_description =
  { tvd_id: ValueID.t;
    tvd_type: Types.t;
    tvd_loc: Location.t }


(**********************************************************************)
(*                           Expressions                              *)
(**********************************************************************)

type expression =
  { texp_desc: expression_desc;
    texp_type: Types.t;
    texp_loc: Location.t }

and expression_desc
  = Texp_undef
  | Texp_any
  | Texp_nil
  | Texp_bool of bool
  | Texp_int of Int64.t
  | Texp_float of float
  | Texp_string of string
  | Texp_path of path
  | Texp_uminus of expression
  | Texp_op of expression * Shared.binary_operator * expression
  | Texp_assign of path * Shared.assign_operator * expression
  | Texp_tuple of expression list
  | Texp_record of (Tyfield.t * expression) list
  | Texp_array of expression * expression
  | Texp_ifthen of expression * expression
  | Texp_ifthenelse of expression * expression * expression
  | Texp_while of expression * expression
  | Texp_for of value_binding list * expression * expression
  | Texp_lambda of lambda_expression
  | Texp_call of expression * expression
  | Texp_closure of expression * string
  | Texp_ccall of path * expression
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_sequence of expression list

and path =
  { tpath_desc: path_desc;
    tpath_type: Types.t;
    tpath_loc: Location.t }

and path_desc
  = Tvar_simple of ValueID.t * ScopeID.t
  | Tvar_field of path * Tyfield.t
  | Tvar_subscript of path * expression


and value_binding =
  { tvb_desc: value_description;
    tvb_expr: expression }

and lambda_expression =
  { tfun_id: ScopeID.t;
    tfun_params: value_description list;
    tfun_body: expression }


(**********************************************************************)
(*                            Lifted functions                        *)
(**********************************************************************)

type function_definition =
  { tfunction_id: ScopeID.t;
    tfunction_label: string;
    tfunction_params: value_description list;
    tfunction_body: expression }


(**********************************************************************)
(*                              Modules                               *)
(**********************************************************************)

and structure_item
  = Tstr_type of type_declaration list
  | Tstr_primitive of value_binding
  | Tstr_value of rec_flag * value_binding list
  | Tstr_function of function_definition list

type structure = structure_item list

type module_expr = Tmod_structure of structure




