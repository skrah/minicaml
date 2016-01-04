(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Type mismatch *)

let _ =
let g (a : unit) = a
in  g 2



