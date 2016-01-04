(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf


exception InternalError of string
exception CompileError

type t = Lexing.position

let none = Lexing.dummy_pos

let make (t : Lexing.position) = (t : t)
let get (t : t) = (t : Lexing.position)

let internal_error msg = raise (InternalError msg)

let error (pos : t) msg =
  let open Lexing in
    fprintf stderr "%s:%d:%d: %s\n"
      pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
    flush stderr;
    raise CompileError

let repr (pos : t) =
  let open Lexing in
    sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)



