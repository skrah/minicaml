(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

val mk_loc : Lexing.position -> Location.t
type core_type = {
  core_type_desc : core_type_desc;
  core_type_loc : Location.t;
}
and core_type_desc =
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_record of field list
  | Ptyp_constr of string * core_type list
and field = {
  pfield_name : string;
  pfield_mutable : Shared.mutable_flag;
  pfield_type : core_type;
  pfield_loc : Location.t;
}
type type_declaration = {
  ptyp_name : string;
  ptyp_params : core_type list;
  ptyp_expr : core_type option;
  ptyp_loc : Location.t;
}
val mk_typany : pos:Lexing.position -> core_type
val mk_typvar : name:string -> pos:Lexing.position -> core_type
val mk_typarrow :
  formals:core_type -> result:core_type -> pos:Lexing.position -> core_type
val mk_typ_or_tuple : lst:core_type list -> pos:Lexing.position -> core_type
val mk_typrecord : lst:field list -> pos:Lexing.position -> core_type
val mk_typconstr :
  name:string -> args:core_type list -> pos:Lexing.position -> core_type
val mk_field :
  name:string ->
  flag:Shared.mutable_flag -> typ:core_type -> pos:Lexing.position -> field
type value_description = {
  pvd_name : string;
  pvd_kind : Shared.value_kind;
  pvd_type : core_type option;
  pvd_loc : Location.t;
}
type expression = {
  pexp_desc : expression_desc;
  pexp_type : core_type option;
  pexp_loc : Location.t;
}
and expression_desc =
    Pexp_any
  | Pexp_nil
  | Pexp_bool of bool
  | Pexp_int of string
  | Pexp_float of string
  | Pexp_string of string
  | Pexp_path of path
  | Pexp_uminus of expression
  | Pexp_op of expression * Shared.binary_operator * expression
  | Pexp_assign of path * Shared.assign_operator * expression
  | Pexp_tuple of expression list
  | Pexp_record of (string * expression) list
  | Pexp_array of expression * expression
  | Pexp_if of expression * expression * expression option
  | Pexp_while of expression * expression
  | Pexp_for of value_binding list * expression * expression
  | Pexp_lambda of lambda_expression
  | Pexp_call of expression * expression
  | Pexp_let of Shared.rec_flag * value_binding list * expression
  | Pexp_sequence of expression list
and path =
    Pvar_simple of string * Location.t
  | Pvar_field of path * string * Location.t
  | Pvar_subscript of path * expression * Location.t
and value_binding = { pvb_desc : value_description; pvb_expr : expression; }
and lambda_expression = {
  pfun_name : string;
  pfun_params : value_description list;
  pfun_body : expression;
}
type structure_item =
    Pstr_type of type_declaration list
  | Pstr_primitive of value_binding
  | Pstr_value of Shared.rec_flag * value_binding list
type structure = structure_item list
type module_expr = Pmod_structure of structure
val add_constraint : expr:expression -> typ:core_type -> expression
val mk_any : pos:Lexing.position -> expression
val mk_nil : pos:Lexing.position -> expression
val mk_tuple : lst:expression list -> pos:Lexing.position -> expression
val mk_record :
  lst:(string * expression) list -> pos:Lexing.position -> expression
val mk_array :
  size:expression -> init:expression -> pos:Lexing.position -> expression
val mk_bool : value:bool -> pos:Lexing.position -> expression
val mk_int : number:string -> pos:Lexing.position -> expression
val mk_float : number:string -> pos:Lexing.position -> expression
val mk_str : str:string -> pos:Lexing.position -> expression
val mk_path : path:path -> pos:Lexing.position -> expression
val mk_uminus : expr:expression -> pos:Lexing.position -> expression
val mk_op :
  left:expression ->
  op:Shared.binary_operator ->
  right:expression -> pos:Lexing.position -> expression
val mk_assign :
  path:path ->
  expr:expression ->
  op:Shared.assign_operator -> pos:Lexing.position -> expression
val mk_if :
  test:expression ->
  then_expr:expression ->
  else_expr:expression option -> pos:Lexing.position -> expression
val mk_while :
  test:expression -> body:expression -> pos:Lexing.position -> expression
val mk_for :
  loop_var:string ->
  pvd_pos:Lexing.position ->
  lo:expression ->
  hi:expression -> body:expression -> for_pos:Lexing.position -> expression
val trans_param : expression -> value_description
val mk_lambda :
  name:string ->
  params:expression list ->
  body:expression -> pos:Lexing.position -> expression
val mk_call :
  func:expression ->
  args:expression list -> pos:Lexing.position -> expression
val mk_opcall :
  name:string -> args:expression list -> pos:Lexing.position -> expression
val mk_let :
  rec_flag:Shared.rec_flag ->
  bindings:value_binding list ->
  body:expression -> pos:Lexing.position -> expression
val mk_sequence : lst:expression list -> pos:Lexing.position -> expression
val mk_simplevar : name:string -> pos:Lexing.position -> path
val mk_fieldvar : name:path -> field:string -> pos:Lexing.position -> path
val mk_subscriptvar :
  name:path -> subscript:expression -> pos:Lexing.position -> path
val mk_typedec :
  name:string ->
  params:core_type list ->
  expr:core_type option -> pos:Lexing.position -> type_declaration
val mk_external :
  name:string ->
  type_expr:core_type -> label:string -> pos:Lexing.position -> value_binding
val mk_binding :
  name:string -> expr:expression -> pos:Lexing.position -> value_binding
val mk_typed_binding :
  name:string ->
  typ:core_type -> expr:expression -> pos:Lexing.position -> value_binding
val mk_ignore :
  expr:expression ->
  pos:Lexing.position -> typ:core_type option -> value_binding
