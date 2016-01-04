(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


open Printf

type options =
  Indent_parsetree
| Indent_ast
| Indent_typedtree
| Indent_cconv
| Emit_llvm
| No_option

let action = ref No_option
let source = ref None

let f opt () = action := opt

let usage = "camlc: usage: ./camlc [options] file"

let specs =
  ["-dparsetree", Arg.Unit (f Indent_parsetree), " <file>  reindent the parse tree";
   "-dast", Arg.Unit (f Indent_ast), " <file>  reindent the (annotated) ast";
   "-dtypedtree", Arg.Unit (f Indent_typedtree), " <file>  reindent the typedtree";
   "-dcconv", Arg.Unit (f Indent_cconv), " <file>  reindent the closure-converted typedtree";
   "-emit-llvm", Arg.Unit (f Emit_llvm), " <file>  emit llvm";
  ]

let _ =
  Arg.parse
    specs
    (fun name -> source := Some name)
    usage

let indent_parsetree file =
  let tree = Lib.parsetree_from_file file in
  Visualize.ParseTreePrinter.indent ([], tree)

let indent_ast file =
  let headers = Lib.parsetree_from_file "builtins.ml" in
  let tree = Lib.parsetree_from_file file in
  let module State = ModuleState.Make (struct let filename = file end) in
  let module Symtbl = Symtable.Make(State) in
  let module AstPrinter = Visualize.AstPrinter(State) in
  let ast = Symtbl.trans_main (headers, tree) in
  AstPrinter.indent ([], snd ast)

let indent_typedtree file =
  let headers = Lib.parsetree_from_file "builtins.ml" in
  let tree = Lib.parsetree_from_file file in
  let module State = ModuleState.Make (struct let filename = file end) in
  let module Symtbl = Symtable.Make(State) in
  let module Typecheck = Typecheck.Make(State) in
  let module TypedtreePrinter = Visualize.TypedtreePrinter (State) (struct let closure = false let fullname = false end) in
  let ast = Symtbl.trans_main (headers, tree) in
  let typedtree = Typecheck.trans_main ast in
  TypedtreePrinter.indent ([], snd typedtree)

let indent_cconv file =
  let headers = Lib.parsetree_from_file "builtins.ml" in
  let tree = Lib.parsetree_from_file file in
  let module State = ModuleState.Make (struct let filename = file end) in
  let module Symtbl = Symtable.Make(State) in
  let module Typecheck = Typecheck.Make(State) in
  let module Closureconv = Closureconv.Make(State) in
  let module TypedtreePrinter = Visualize.TypedtreePrinter(State) (struct let closure = true let fullname = true end) in
  let ast = Symtbl.trans_main (headers, tree) in
  let typedtree = Typecheck.trans_main ast in
  let cconv = Closureconv.conv typedtree in
  TypedtreePrinter.indent ([], snd cconv)

let emit_llvm file =
  let headers = Lib.parsetree_from_file "builtins.ml" in
  let tree = Lib.parsetree_from_file file in
  let module State = ModuleState.Make (struct let filename = file end) in
  let module Symtbl = Symtable.Make(State) in
  let module Typecheck = Typecheck.Make(State) in
  let module Closureconv = Closureconv.Make(State) in
  let module Compile = Translate.Make(State) in
  let ast = Symtbl.trans_main (headers, tree) in
  let typedtree = Typecheck.trans_main ast in
  let cconv = Closureconv.conv typedtree in
    Compile.trans_main cconv


let main () =
    match !source with
    | None -> fprintf stderr "%s" (Arg.usage_string specs usage); exit(1)
    | Some file ->
      match !action with
        Indent_parsetree -> indent_parsetree file
      | Indent_ast -> indent_ast file
      | Indent_typedtree -> indent_typedtree file
      | Indent_cconv -> indent_cconv file
      | Emit_llvm -> emit_llvm file
      | No_option ->
          fprintf stderr "%s" (Arg.usage_string specs usage);
          exit(1)


let _ = main ()



