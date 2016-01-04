(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

val mk_lexbuf : string -> in_channel -> Lexing.lexbuf
val error : Lexing.lexbuf -> string -> 'a
val state_from_filename : string -> (module LexState.S)
val parsetree_from_file : string -> ParseTree.module_expr
val parsetree_from_string : string -> ParseTree.module_expr
