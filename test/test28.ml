(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type rectype2 but an expression
   was expected of type rectype1. *)

type rectype1 = { name : string; id : int }
type rectype2 = { name1 : string; id1 : int }
let rec1 = { name = "Name"; id1 = 0 }




