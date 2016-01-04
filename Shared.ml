(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


(**********************************************************************)
(*                            Error handlers                          *)
(**********************************************************************)

let internal_error = Location.internal_error
let error = Location.error


(**********************************************************************)
(*                 Variants shared by different trees                 *)
(**********************************************************************)

type level = int

type rec_flag
  = Nonrecursive
  | Recursive

type mutable_flag
  = Immutable
  | Mutable

type value_kind
  = Vardec
  | Parameter
  | Loopvar
  | External
  | Ignore

type param_kind
  = Param_tuple
  | Param_curried

type binary_operator
  = Op_plus | Op_minus | Op_times | Op_divide
  | Op_plusdot | Op_minusdot | Op_timesdot | Op_dividedot
  | Op_eq | Op_eqeq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
  | Op_and | Op_or

type assign_operator
  = Op_assign_arrow | Op_assign_ref


(**********************************************************************)
(*                           Representations                          *)
(**********************************************************************)

let var_kind_repr = function
    Vardec -> "Vardec"
  | Parameter -> "Parameter"
  | Loopvar -> "Loopvar"
  | External -> "External"
  | Ignore -> "Ignore"

let op_repr = function
  Op_plus -> "+"
| Op_minus -> "-"
| Op_times -> "*"
| Op_divide -> "/"
| Op_plusdot -> "+."
| Op_minusdot -> "-."
| Op_timesdot -> "*."
| Op_dividedot -> "/."
| Op_eq -> "="
| Op_eqeq -> "=="
| Op_ne -> "<>"
| Op_lt -> "<"
| Op_le -> "<="
| Op_gt -> ">"
| Op_ge -> ">="
| Op_and -> "&&"
| Op_or -> "||"

let assign_op_repr = function
  Op_assign_arrow -> "<-"
| Op_assign_ref -> ":="

let is_boolop = function
  Op_and | Op_or -> true
| _ -> false

let is_intop = function
  Op_plus | Op_minus | Op_times | Op_divide -> true
| _ -> false

let is_floatop = function
  Op_plusdot | Op_minusdot | Op_timesdot | Op_dividedot -> true
| _ -> false

let is_cmpop = function
  Op_eq | Op_eqeq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge -> true
| _ -> false

let is_eqop = function
  Op_eq | Op_eqeq | Op_ne -> true
| _ -> false



