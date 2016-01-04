#!/bin/sh

# DEBUGGING: Dump all imtermediate stages up to the typed tree to stdout.
# Written as a shell script to facilitate using the superb ocaml pretty
# printer.

if [ X"$@" = X"" ]; then
   echo "./usage: ./dump_typedtree.sh file"
   exit 1
fi

OBJS="Location.cmo Env.cmo Shared.cmo Util.cmo Symbol.cmo ParseTree.cmo Types.cmo SymbolGenerator.cmo Scc.cmo Parser.cmo LexState.cmo Typedtree.cmo ModuleState.cmo Lexer.cmo Ast.cmo Unify.cmo Lib.cmo Visualize.cmo Typecheck.cmo Symtable.cmo"

cmd="ocaml -noinit -I _build unix.cma $OBJS"

$cmd << EOF
#print_length 100000;;
#print_depth 100000;;
open Types;;
open ParseTree;;
open Ast;;
open Typedtree;;

let headers = Lib.parsetree_from_file "builtins.ml";;
let tree = Lib.parsetree_from_file "$@";;
module State = ModuleState.Make (struct let filename = "$@" end);;
module Symtbl = Symtable.Make(State);;
module Typchk = Typecheck.Make(State);;
let ast = Symtbl.trans_main (headers, tree);;
let typedtree = Typchk.trans_main ast;;
EOF


