(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Unbound record field f *)

let _ =
let d = 0 in d.f


