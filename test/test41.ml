(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Multiple definition of the type name a. *)

type a = int
type a = string


