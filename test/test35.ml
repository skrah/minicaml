(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Type mismatch *)

let _ =
let g : int * string -> int = fun (a, b) -> a
in  g ("one")



