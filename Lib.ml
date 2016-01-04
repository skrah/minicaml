(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


let mk_lexbuf filename ic =
  let open Lexing in
  let buf = from_channel ic in
    buf.lex_start_p <- { buf.lex_start_p with pos_fname = filename };
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };
    buf

let error buf msg =
  let open Lexing in
  let loc = Location.make buf.lex_curr_p in
    Location.error loc msg

let state_from_filename file =
  let module State = LexState.Make (struct let filename = file end) in
    (module State : LexState.S)

let parsetree_from_file file =
  let ic = open_in file in
  let buf = mk_lexbuf file ic in
  let state = state_from_filename file in
    try
        let parsetree = Parser.program (Lexer.token state) buf in
          close_in ic; parsetree
    with Lexer.Error msg -> close_in ic; error buf msg
       | Parser.Error -> close_in ic; error buf "syntax error"

let parsetree_from_string str =
  let buf = Lexing.from_string str in
  let state = state_from_filename "<string>" in
    try
        let parsetree = Parser.program (Lexer.token state) buf in
          parsetree
    with Lexer.Error msg -> error buf msg
       | Parser.Error -> error buf "syntax error"



