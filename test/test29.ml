(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Structural types: no type mismatch *)

type arrtype1 = int array
type arrtype2 = int array

let arr1 : arrtype1 = ([0] ** 10 : arrtype2)



