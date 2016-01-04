(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type int but an expression was
   expected of type 'a array. *)

let _ =
let a = 0 in a.(3)


