(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

exception Error of string
val token : (module LexState.S) -> Lexing.lexbuf -> Parser.token
