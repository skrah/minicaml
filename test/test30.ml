(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Type alias: no type mismatch *)

type a = int array
type b = a

let arr1 : a = ([0] ** 10 : b)



