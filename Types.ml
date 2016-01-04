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

type t
  = Tany
  | Tvar of Tyvar.t
  | Tnil of tmeta ref
  | Tmeta of tmeta ref
  | Tfield of Tyfield.t * mutable_flag * t * t
  | Ttuple of t list
  | Trecord of (Tyfield.t * t) list
  | Tarrow of t * t
  | Tpoly of Tyvar.t list * t
  | Tapp of tfun * t list

and tfun
  = Tfun of Tyvar.t list * t
  | Tlink of Tycon.t
  | Tconstr of Tycon.t

and tmeta
  = Unbound of Tymeta.t * int
  | Link of t


(**********************************************************************)
(*                    Initialize primitive types                      *)
(**********************************************************************)

let tycon_id = ref 1000
let tyvar_id = ref 1000

let mk_tycon_id name =
  let id = Util.post_incr tycon_id in
    if id >= Tycon.start_id then failwith "reserved tycon ids exhausted"
    else Tycon.unsafe_make (name, id)

let mk_tyvar_id name =
  let id = Util.post_incr tyvar_id in
    if id >= Tyvar.start_id then internal_error "reserved tyvar ids exhausted"
    else Tyvar.unsafe_make (name, id)

let prim_unit_id = mk_tycon_id "unit"
let prim_bool_id = mk_tycon_id "bool"
let prim_int_id = mk_tycon_id "int"
let prim_float_id = mk_tycon_id "float"
let prim_string_id = mk_tycon_id "string"
let prim_array_id = mk_tycon_id "array"
let prim_unit = Tfun ([], Tapp (Tconstr prim_unit_id, []))
let prim_bool = Tfun ([], Tapp (Tconstr prim_bool_id, []))
let prim_int = Tfun ([], Tapp (Tconstr prim_int_id, []))
let prim_float = Tfun ([], Tapp (Tconstr prim_float_id, []))
let prim_string = Tfun ([], Tapp (Tconstr prim_string_id, []))
let prim_array =
  let v = mk_tyvar_id "a" in
    Tfun ([v], Tapp (Tconstr prim_array_id, [Tvar v]))

let prim_types = [
  prim_unit_id, prim_unit;
  prim_bool_id, prim_bool;
  prim_int_id, prim_int;
  prim_float_id, prim_float;
  prim_string_id, prim_string;
  prim_array_id, prim_array ]

let unit_t = Tapp (Tconstr prim_unit_id, [])
let bool_t = Tapp (Tconstr prim_bool_id, [])
let int64_t = Tapp (Tconstr prim_int_id, [])
let double_t = Tapp (Tconstr prim_float_id, [])
let string_t = Tapp (Tconstr prim_string_id, [])



