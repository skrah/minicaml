(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

val internal_error : string -> 'a
val error : Location.t -> string -> 'a
type level = int
type rec_flag = Nonrecursive | Recursive
type mutable_flag = Immutable | Mutable
type value_kind = Vardec | Parameter | Loopvar | External | Ignore
type param_kind = Param_tuple | Param_curried
type binary_operator =
    Op_plus
  | Op_minus
  | Op_times
  | Op_divide
  | Op_plusdot
  | Op_minusdot
  | Op_timesdot
  | Op_dividedot
  | Op_eq
  | Op_eqeq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
type assign_operator = Op_assign_arrow | Op_assign_ref
val var_kind_repr : value_kind -> string
val op_repr : binary_operator -> string
val assign_op_repr : assign_operator -> string
val is_boolop : binary_operator -> bool
val is_intop : binary_operator -> bool
val is_floatop : binary_operator -> bool
val is_cmpop : binary_operator -> bool
val is_eqop : binary_operator -> bool
