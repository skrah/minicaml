(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


open Shared


(**********************************************************************)
(*                           Symbol management                        *)
(**********************************************************************)

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

module Make (A: sig val tag : string val start_id : int end) : S =
struct
  type t = string * int
  let compare (_, id1) (_, id2) = Pervasives.compare id1 id2
  let equal (_, id1) (_, id2) = (id1 = id2)
  let hash (_, id) = Hashtbl.hash id

  let unsafe_make (str, id) = (str, id)
  let make _ = internal_error "virtual method"
  let name sym = fst sym
  let id sym = snd sym

  let repr (_, id) = Printf.sprintf "%s%d" A.tag id
  let short_repr (str, id) = Printf.sprintf "%s%d" str id

  let start_id = A.start_id
end

module ScopeID : S = Make (struct let tag = "F" let start_id = -1 end)
module Tycon : S = Make (struct let tag = "t" let start_id = 1010 end)
module Tyvar : S = Make (struct let tag = "tv" let start_id = 1001 end)
module Tymeta : S = Make (struct let tag = "m" let start_id = 1000 end)
module Tyfield : S = Make (struct let tag = "f" let start_id = 1000 end)
module ValueID : S = Make (struct let tag = "v" let start_id = 1000 end)




