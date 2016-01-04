(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type arrtype = int array
   but an expression was expected of type rectype. *)

type arrtype = int array
type rectype = { name : string; id : int }

let r : rectype = { name = "aname"; id = 0 }
let a : arrtype = [0] ** 3


let _ = if r <> a
        then 3
        else 4


