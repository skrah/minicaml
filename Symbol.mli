(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


module type S =
  sig
    type t = string * int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
    val unsafe_make : string * int -> t
    val make : string -> t
    val name : t -> string
    val id : t -> int
    val repr : t -> string
    val short_repr : t -> string
    val start_id : int
  end
module Make : functor (A : sig val tag : string val start_id : int end) -> S
module ScopeID : S
module Tycon : S
module Tyvar : S
module Tymeta : S
module Tyfield : S
module ValueID : S
