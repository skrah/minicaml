(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)


type rectype = { mutable name : string; mutable age : int }
let rec1 : rectype = { name = "Nobody"; age = 1000 }

let _ = rec1.name <- "Somebody"


