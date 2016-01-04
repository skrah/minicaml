(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* File "test17.ml", line 6, characters 20-21:
     Error: Syntax error *)

type tree = { key : int; children : treelist }
let x : int = 0
and treelist = { hd : tree; tl : treelist }



