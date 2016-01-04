(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

module type S =
  sig
    val filename : string
    val open_comments : unit -> int
    val incr_open_comments : unit -> unit
    val decr_open_comments : unit -> unit
  end
module Make : functor (A : sig val filename : string end) -> S
