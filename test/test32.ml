(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Type mismatch *)

type t = int array
let a = ([" "] ** 10 : t)



