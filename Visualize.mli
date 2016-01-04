(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

module ParseTreePrinter :
  sig
    val indent : 'a * ParseTree.module_expr -> unit
  end
module AstPrinter :
  functor (ModuleState : ModuleState.S) ->
    sig
      val indent : 'a * Ast.module_expr -> unit
    end
module TypedtreePrinter :
  functor (ModuleState : ModuleState.S) (Options : sig val closure : bool val fullname : bool end) ->
    sig
      val indent : 'a * Typedtree.module_expr -> unit
    end
