(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

type t =
    Tany
  | Tvar of Symbol.Tyvar.t
  | Tnil of tmeta ref
  | Tmeta of tmeta ref
  | Tfield of Symbol.Tyfield.t * Shared.mutable_flag * t * t
  | Ttuple of t list
  | Trecord of (Symbol.Tyfield.t * t) list
  | Tarrow of t * t
  | Tpoly of Symbol.Tyvar.t list * t
  | Tapp of tfun * t list
and tfun = Tfun of Symbol.Tyvar.t list * t | Tlink of Symbol.Tycon.t | Tconstr of Symbol.Tycon.t
and tmeta = Unbound of Symbol.Tymeta.t * int | Link of t
val tycon_id : int ref
val tyvar_id : int ref
val mk_tycon_id : string -> Symbol.Tycon.t
val mk_tyvar_id : string -> Symbol.Tyvar.t
val prim_unit_id : Symbol.Tycon.t
val prim_bool_id : Symbol.Tycon.t
val prim_int_id : Symbol.Tycon.t
val prim_float_id : Symbol.Tycon.t
val prim_string_id : Symbol.Tycon.t
val prim_array_id : Symbol.Tycon.t
val prim_unit : tfun
val prim_bool : tfun
val prim_int : tfun
val prim_float : tfun
val prim_string : tfun
val prim_array : tfun
val prim_types : (Symbol.Tycon.t * tfun) list
val unit_t : t
val bool_t : t
val int64_t : t
val double_t : t
val string_t : t
