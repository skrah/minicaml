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

module Make (A: sig val filename : string end) : S =
struct
  let filename = A.filename
  let comments = ref 0
  let open_comments () = !comments
  let incr_open_comments () = Util.safe_incr comments
  let decr_open_comments () = Util.safe_decr comments
end



