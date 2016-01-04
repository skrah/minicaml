(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


{
open Lexing

module P = Parser
let implode = Util.implode
let explode = Util.explode
let create_hashtable = Util.create_hashtable

exception Error of string

module StringFilter :
sig
  val unescape : string -> string
end =
struct

let dtochr (x, y, z) =
    let s = implode [x; y; z] in
      try let r = (int_of_string s) in
          if r > 127 then raise (Error "character code greater than 127")
          else Char.chr r
      with Failure s -> raise (Error s)

let rec filter lst =
    match lst with
      [] -> []
    | '\\' :: ys -> (match ys with
                        'n'  :: xs        -> '\n' :: (filter xs)
                      | 't'  :: xs        -> '\t' :: (filter xs)
                      | '\'' :: xs        -> '\'' :: (filter xs)
                      | '\\' :: xs        -> '\\' :: (filter xs)
                      | ' '  :: xs        -> (filter_skip xs)
                      | '\n' :: xs        -> (filter_skip xs)
                      | '\t' :: xs        -> (filter_skip xs)
                      | '\012' :: xs      -> (filter_skip xs)
                      | '^'  :: x :: xs   -> (Char.chr(Char.code x - 64)) :: (filter xs)
                      | x :: y :: z :: xs -> dtochr(x, y, z) :: (filter xs)
                      | _                 -> raise (Error "invalid escape"))
    | y :: ys -> y :: (filter ys)

and filter_skip lst =
    match lst with
      ' ' :: xs    -> (filter_skip xs)
    | '\n' :: xs   -> (filter_skip xs)
    | '\t' :: xs   -> (filter_skip xs)
    | '\012' :: xs -> (filter_skip xs)
    | '\\' :: xs   -> (filter xs)
    | _            -> raise (Error "invalid string continuation")

let unescape s =
    let n = String.length s in
        if n <= 2 then ""
        else let s' = String.sub s 1 (n - 2) in
             let lst = (explode s') in
                 implode (filter lst)

end

let keyword_table =
   create_hashtable 101 [
     "and", P.AND;
     "external", P.EXTERNAL;
     "do", P.DO;
     "done", P.DONE;
     "else", P.ELSE;
     "false", P.FALSE;
     "fun", P.FUN;
     "for", P.FOR;
     "if", P.IF;
     "in", P.IN;
     "let", P.LET;
     "mutable", P.MUTABLE;
     "nil", P.NIL;
     "rec", P.REC;
     "then", P.THEN;
     "true", P.TRUE;
     "to", P.TO;
     "type", P.TYPE;
     "while", P.WHILE
  ]
}

let id = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let pri = ['\032'-'\033' '\035'-'\091' '\093'-'\126']
let str = '"'
         ('\\' ['n' 't' '"' '\\']
        | '\\' '^' ['@' 'A'-'Z' '\\' '[' '^' ']' '_' ]
        | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] | ['a'-'z']
        | pri
        | '\\' ['\\' '\n' '\t' '\012']* '\\')*
          '"'

let digit = ['0'-'9']
let integer = (digit)*

let intpart = (digit)+
let fraction = '.' (digit)+
let exponent = ('e' | 'E') ('+' | '-')? (digit)+
let pointfloat = (intpart)? (fraction) | intpart '.'
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat


rule token state = parse
  | "(*"
      { let module LexState = (val state : LexState.S) in
        let open LexState in
        assert (open_comments () = 0);
        incr_open_comments ();
        comment state lexbuf }
  | "->"   { P.RARROW }
  | "<-"   { P.LARROW }
  | ":="   { P.ASSIGN }
  | ":"    { P.COLON }
  | "=="   { P.EQEQUAL }
  | "="    { P.EQUAL }
  | "<>"   { P.NOTEQUAL }
  | "<="   { P.LESSEQUAL }
  | "<"    { P.LESS }
  | ">="   { P.GREATEREQUAL }
  | ">"    { P.GREATER }
  | "||"   { P.DOUBLEBAR }
  | "&&"   { P.DOUBLEAMPER }
  | ";;"   { P.SEMISEMI }
  | "**"   { P.DOUBLESTAR }
  | "/."   { P.SLASHDOT }
  | "*."   { P.STARDOT }
  | "-."   { P.MINUSDOT }
  | "+."   { P.PLUSDOT }
  | "/"    { P.SLASH }
  | "*"    { P.STAR }
  | "-"    { P.MINUS }
  | "+"    { P.PLUS }
  | "("    { P.LPAR }
  | ")"    { P.RPAR }
  | "{"    { P.LBRACE }
  | "}"    { P.RBRACE }
  | "["    { P.LBRACK }
  | "]"    { P.RBRACK }
  | "."    { P.DOT }
  | ","    { P.COMMA }
  | ";"    { P.SEMI }
  | "!"    { P.BANG }
  | "'"    { P.QUOTE }
  | "_"    { P.UNDERSCORE }
  | str
      { let s = Lexing.lexeme lexbuf in
        P.STRING(StringFilter.unescape s) }
  | id
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> P.ID s }
  | integer          { let s = Lexing.lexeme lexbuf in P.INT s }
  | floatnumber      { let s = Lexing.lexeme lexbuf in P.FLOAT s }
  | "\n"             { new_line lexbuf; token state lexbuf }
  | [' ' '\r' '\t']+ { token state lexbuf }
  | eof              { P.EOF }
  | _                { let s = Lexing.lexeme lexbuf in
                       raise (Error (Printf.sprintf "invalid character: %s" s)) }

and comment state = parse
  | "(*"
       { let module LexState = (val state : LexState.S) in
         let open LexState in
         assert (open_comments () > 0);
         incr_open_comments ();
         comment state lexbuf }
  | "*)"
       { let module LexState = (val state : LexState.S) in
         let open LexState in
         assert (open_comments () > 0);
         decr_open_comments ();
         if open_comments () = 0 then token state lexbuf
         else comment state lexbuf }
  | "\n" { new_line lexbuf; comment state lexbuf }
  | _    { comment state lexbuf }



