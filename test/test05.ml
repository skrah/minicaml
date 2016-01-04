(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

type intlist = { hd : int; tl : intlist }

type tree = { key : int; children : treelist }
and treelist = { hd1 : tree; tl1 : treelist }

let l : intlist = { hd = 0; tl = nil }



