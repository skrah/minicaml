(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

%{
(***** User declarations *****)
open Shared
open ParseTree

module RevList :
sig
  type 'a t
  val empty : 'a t
  val single : 'a -> 'a t
  val double : 'a -> 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val rget : 'a t -> 'a list
end =
struct
  type 'a t = 'a list
  let empty = []
  let single x = [x]
  let double x y = [x; y]
  let cons x l = x :: l
  let rget l = List.rev l
end
include RevList

let seq s pos = mk_sequence ~lst:(rget s) ~pos:pos

%}

/* Tokens */

%token AND
%token ASSIGN
%token BANG
%token COLON
%token COMMA
%token EXTERNAL
%token DO
%token DONE
%token DOUBLEAMPER
%token DOUBLEBAR
%token DOUBLESTAR
%token DOT
%token ELSE
%token EOF
%token EQUAL
%token EQEQUAL
%token FALSE
%token FUN
%token FOR
%token GREATEREQUAL
%token GREATER
%token IF
%token IN
%token LARROW
%token LBRACE
%token LBRACK
%token LESSEQUAL
%token LET
%token LPAR
%token LESS
%token MINUS
%token MINUSDOT
%token MUTABLE
%token NOTEQUAL
%token NIL
%token PLUS
%token PLUSDOT
%token QUOTE
%token RARROW
%token RBRACE
%token RBRACK
%token REC
%token RPAR
%token SEMI
%token SEMISEMI
%token SLASH
%token SLASHDOT
%token STAR
%token STARDOT
%token THEN
%token TO
%token TRUE
%token TYPE
%token UNDERSCORE
%token WHILE
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID


/* Precedence and associativity */

%nonassoc IN
%right RARROW
%nonassoc below_SEMI
%nonassoc SEMI
(*%nonassoc LET*)
(*%nonassoc AND*)
%nonassoc THEN
%nonassoc ELSE
%nonassoc LARROW
%nonassoc ASSIGN
(*%nonassoc below_COMMA*)
(*%nonassoc COMMA*)
%left DOUBLEBAR
%left DOUBLEAMPER
%left EQUAL EQEQUAL NOTEQUAL GREATER LESS GREATEREQUAL LESSEQUAL
%left PLUS MINUS PLUSDOT MINUSDOT
%left STAR SLASH STARDOT SLASHDOT
%left prec_UMINUS

/* Entry points */


%start program
%type <ParseTree.module_expr> program

%%

/* Grammar */

program:
  structure EOF { Pmod_structure (rget $1) }

structure :
  structure_item                    { single $1 }
| structure structure_item          { cons $2 $1 }
| structure SEMISEMI structure_item { cons $3 $1 }

structure_item :
  TYPE type_bindings        { Pstr_type (rget $2) }
| external_binding          { Pstr_primitive $1 }
| LET rec_flag let_bindings { Pstr_value ($2, rget $3) }

rec_flag :
  /* empty */  { Nonrecursive }
| REC          { Recursive }

mutable_flag :
  /* empty */  { Immutable }
| MUTABLE      { Mutable }

operator_function:
  LPAR BANG RPAR    { "(!)" }
| LPAR ASSIGN RPAR  { "(:=)" }


expr:
  atom                                       { $1 }
| atom atom_list                             { mk_call ~func:$1 ~args:(rget $2) ~pos:$startpos }
| MINUS expr %prec prec_UMINUS               { mk_uminus ~expr:$2 ~pos:$startpos }
| expr ASSIGN expr                           { mk_opcall ~name:"(:=)" ~args:[$1; $3] ~pos:$startpos }
| path LARROW expr                           { mk_assign ~path:$1 ~expr:$3 ~op:Op_assign_arrow ~pos:$startpos }
| expr PLUS expr                             { mk_op ~left:$1 ~op:Op_plus ~right:$3 ~pos:$startpos }
| expr MINUS expr                            { mk_op ~left:$1 ~op:Op_minus ~right:$3 ~pos:$startpos }
| expr STAR expr                             { mk_op ~left:$1 ~op:Op_times ~right:$3 ~pos:$startpos }
| expr SLASH expr                            { mk_op ~left:$1 ~op:Op_divide ~right:$3 ~pos:$startpos }
| expr PLUSDOT expr                          { mk_op ~left:$1 ~op:Op_plusdot ~right:$3 ~pos:$startpos }
| expr MINUSDOT expr                         { mk_op ~left:$1 ~op:Op_minusdot ~right:$3 ~pos:$startpos }
| expr STARDOT expr                          { mk_op ~left:$1 ~op:Op_timesdot ~right:$3 ~pos:$startpos }
| expr SLASHDOT expr                         { mk_op ~left:$1 ~op:Op_dividedot ~right:$3 ~pos:$startpos }
| expr EQUAL expr                            { mk_op ~left:$1 ~op:Op_eq ~right:$3 ~pos:$startpos }
| expr EQEQUAL expr                          { mk_op ~left:$1 ~op:Op_eqeq ~right:$3 ~pos:$startpos }
| expr NOTEQUAL expr                         { mk_op ~left:$1 ~op:Op_ne ~right:$3 ~pos:$startpos }
| expr LESS expr                             { mk_op ~left:$1 ~op:Op_lt ~right:$3 ~pos:$startpos }
| expr GREATER expr                          { mk_op ~left:$1 ~op:Op_gt ~right:$3 ~pos:$startpos }
| expr LESSEQUAL expr                        { mk_op ~left:$1 ~op:Op_le ~right:$3 ~pos:$startpos }
| expr GREATEREQUAL expr                     { mk_op ~left:$1 ~op:Op_ge ~right:$3 ~pos:$startpos }
| expr DOUBLEAMPER expr                      { mk_op ~left:$1 ~op:Op_and ~right:$3 ~pos:$startpos }
| expr DOUBLEBAR expr                        { mk_op ~left:$1 ~op:Op_or ~right:$3 ~pos:$startpos }
| LBRACK expr RBRACK DOUBLESTAR atom         { mk_array ~size:$5 ~init:$2 ~pos:$startpos }
| IF expr THEN seq_expr ELSE seq_expr        { mk_if ~test:$2
                                                     ~then_expr:(seq $4 $startpos($4))
                                                     ~else_expr:(Some (seq $6 $startpos($6)))
                                                     ~pos:$startpos }
| IF expr THEN seq_expr                      { mk_if ~test:$2
                                                     ~then_expr:(seq $4 $startpos($4))
                                                     ~else_expr:None
                                                     ~pos:$startpos }
| WHILE expr DO seq_expr DONE                { mk_while ~test:$2
                                                        ~body:(seq $4 $startpos($4))
                                                        ~pos:$startpos }
| FOR ID EQUAL expr TO expr DO seq_expr DONE { mk_for ~loop_var:$2
                                                      ~pvd_pos:$startpos($2)
                                                      ~lo:$4
                                                      ~hi:$6
                                                      ~body:(seq $8 $startpos($8))
                                                      ~for_pos:$startpos }
| FUN atom_list RARROW seq_expr              { mk_lambda ~name:"lambda"
                                                         ~params:(rget $2)
                                                         ~body:(seq $4 $startpos($4))
                                                         ~pos:$startpos }
| LET rec_flag let_bindings IN seq_expr      { mk_let ~rec_flag:$2
                                                      ~bindings:(rget $3)
                                                      ~body:(seq $5 $startpos($5))
                                                      ~pos:$startpos }

atom:
  NIL                            { mk_nil ~pos:$startpos }
| TRUE                           { mk_bool ~value:true ~pos:$startpos }
| FALSE                          { mk_bool ~value:false ~pos:$startpos }
| UNDERSCORE                     { mk_any ~pos:$startpos }
| FLOAT                          { mk_float ~number:$1 ~pos:$startpos }
| INT                            { mk_int ~number:$1 ~pos:$startpos }
| STRING                         { mk_str ~str:$1 ~pos:$startpos }
| path                           { mk_path ~path:$1 ~pos:$startpos }
| BANG atom                      { mk_opcall ~name:"(!)" ~args:[$2] ~pos:$startpos }
| LBRACE assignment_list RBRACE  { mk_record ~lst:(rget $2) ~pos:$startpos }
| LPAR tuple RPAR                { mk_tuple ~lst:(rget $2) ~pos:$startpos }
| LPAR seq_expr RPAR             { (seq $2 $startpos($2)) }
| LPAR expr COLON core_type RPAR { add_constraint ~expr:$2 ~typ:$4 }

tuple :
  /* empty */      { empty }
| expr COMMA expr  { double $3 $1 }
| tuple COMMA expr { cons $3 $1 }

seq_expr :
  expr %prec below_SEMI  { single $1 }
| seq_expr SEMI expr     { cons $3 $1 }

atom_list :
  atom                { single $1 }
| atom_list atom      { cons $2 $1 }

assignment_list :
  /* empty */                        { empty }
| ID EQUAL expr                      { single ($1, $3) }
| assignment_list SEMI ID EQUAL expr { cons ($3, $5) $1 }

path :
  ID                      { mk_simplevar ~name:$1 ~pos:$startpos }
| path DOT ID             { mk_fieldvar ~name:$1 ~field:$3 ~pos:$startpos($3) }
| path DOT LPAR expr RPAR { mk_subscriptvar ~name:$1 ~subscript:$4 ~pos:$startpos($2) }

core_type :
  simple_core_type_or_tuple
   { mk_typ_or_tuple ~lst:(rget $1) ~pos:$startpos }
| simple_core_type_or_tuple RARROW core_type
   { let formals = mk_typ_or_tuple ~lst:(rget $1) ~pos:$startpos in
       mk_typarrow ~formals ~result:$3 ~pos:$startpos }

simple_core_type_or_tuple :
  simple_core_type                                { single $1 }
| simple_core_type_or_tuple STAR simple_core_type { cons $3 $1 }

simple_core_type :
  UNDERSCORE                        { mk_typany ~pos:$startpos }
| QUOTE ID                          { mk_typvar ~name:$2 ~pos:$startpos }
| ID                                { mk_typconstr ~name:$1 ~args:[] ~pos:$startpos }
| simple_core_type ID               { mk_typconstr ~name:$2 ~args:[$1] ~pos:$startpos }
| LPAR core_type_comma_list RPAR ID { mk_typconstr ~name:$4 ~args:(rget $2) ~pos:$startpos }
| LBRACE field_list RBRACE          { mk_typrecord ~lst:(rget $2) ~pos:$startpos }
| LPAR core_type RPAR               { $2 }

core_type_comma_list :
  core_type COMMA core_type            { double $3 $1  }
| core_type_comma_list COMMA core_type { cons $3 $1 }

field : mutable_flag ID COLON core_type
  { mk_field ~name:$2 ~flag:$1 ~typ:$4 ~pos:$startpos }

field_list :
  field                 { single $1 }
| field_list SEMI field { cons $3 $1 }

external_binding :
  EXTERNAL ID COLON core_type EQUAL STRING
    { mk_external ~name:$2 ~type_expr:$4 ~label:$6 ~pos:$startpos }
| EXTERNAL operator_function COLON core_type EQUAL STRING
    { mk_external ~name:$2 ~type_expr:$4 ~label:$6 ~pos:$startpos }

type_binding : type_parameters_opt ID EQUAL core_type
  { mk_typedec ~name:$2 ~params:$1 ~expr:(Some $4) ~pos:$startpos }

type_bindings:
  type_binding                   { single $1 }
| type_bindings AND type_binding { cons $3 $1 }

type_parameters_opt:
  /* empty */                   { [] }
| type_variable                 { [$1] }
| LPAR type_parameter_list RPAR { rget $2 }

type_parameter_list:
  type_variable                           { single $1 }
| type_parameter_list COMMA type_variable { cons $3 $1 }

type_variable: QUOTE ID { mk_typvar ~name:$2 ~pos:$startpos }


variable_binding :
  ID EQUAL seq_expr                             { mk_binding ~name:$1
                                                             ~expr:(seq $3 $startpos($3))
                                                             ~pos:$startpos }
| ID COLON core_type EQUAL seq_expr             { mk_typed_binding ~name:$1
                                                                   ~typ:$3
                                                                   ~expr:(seq $5 $startpos($5))
                                                                   ~pos:$startpos }
| UNDERSCORE EQUAL seq_expr                     { mk_ignore ~typ:None
                                                            ~expr:(seq $3 $startpos($3))
                                                            ~pos:$startpos }
| UNDERSCORE COLON core_type EQUAL seq_expr     { mk_ignore ~typ:(Some $3)
                                                            ~expr:(seq $5 $startpos($5))
                                                            ~pos:$startpos }
| LPAR BANG RPAR EQUAL seq_expr                 { mk_binding ~name:"(!)"
                                                             ~expr:(seq $5 $startpos($5))
                                                             ~pos:$startpos }
| LPAR BANG RPAR COLON core_type EQUAL seq_expr { mk_typed_binding ~name:"(!)"
                                                                   ~typ:$5
                                                                   ~expr:(seq $7 $startpos($7))
                                                                   ~pos:$startpos }


lambda_binding :
  ID atom_list EQUAL seq_expr
    { let l = mk_lambda ~name:$1 ~params:(rget $2)
                        ~body:(seq $4 $startpos($4))
                        ~pos:$startpos in
      let s = mk_sequence ~lst:[l] ~pos:$startpos in
        mk_binding ~name:$1 ~expr:s ~pos:$startpos }
| operator_function atom_list EQUAL seq_expr
    { let l = mk_lambda ~name:$1 ~params:(rget $2)
                        ~body:(seq $4 $startpos($4))
                        ~pos:$startpos in
      let s = mk_sequence ~lst:[l] ~pos:$startpos in
        mk_binding ~name:$1 ~expr:s ~pos:$startpos }

let_bindings:
  let_binding                  { single $1 }
| let_bindings AND let_binding { cons $3 $1 }

let_binding :
  variable_binding { $1 }
| lambda_binding   { $1 }





