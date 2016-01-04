(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

exception InternalError of string
exception CompileError
type t = Lexing.position
val none : t
val make : Lexing.position -> t
val get : t -> Lexing.position
val internal_error : string -> 'a
val error : t -> string -> 'a
val repr : t -> string
