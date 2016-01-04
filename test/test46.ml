(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

type rectype = {name : string; id : int}
let b = ref (nil : rectype)
let _ = b := nil; b <> nil


