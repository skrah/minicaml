(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type string but an expression was
   expected of type int *)

let _ = "abc" = "abc"; 3 > "df"


