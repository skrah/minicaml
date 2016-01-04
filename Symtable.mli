(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

module Make :
  functor (ModuleState : ModuleState.S) ->
    sig
      val trans_main :
        ParseTree.module_expr * ParseTree.module_expr ->
        Ast.module_expr * Ast.module_expr
    end
