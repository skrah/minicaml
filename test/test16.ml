(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Error: The type abbreviation a is cyclic *)

type a = c
and b = a
and c = d
and d = a




