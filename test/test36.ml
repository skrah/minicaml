(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Excess arguments: type mismatch *)

let _ =
let g : int * string -> int = fun (a, b) -> a
in  g (3, "one", 5)



