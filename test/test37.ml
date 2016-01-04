(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Shadowing a variable *)

let _ =
let a = 0 in
let a = " " in
let a = 10 in
  a



