(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* The field nam does not belong to type rectype *)

type rectype = { name : string; id : int }
let rec1 = { name = "Name"; id = 0 }

let _ = rec1.nam <- "asd"



