(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type int but an expression was
   expected of type string. *)

type rectype = { name : string; id : int }
let rec1 = { name = "aname"; id = 0 }

let _ = rec1.name <- 3; rec1.id <- ""



