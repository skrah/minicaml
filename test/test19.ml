(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* File "test19.ml", line 7, characters 16-17:
     Error: Unbound value a *)

let _ =
let rec do_nothing1 (a, b) =
  (do_nothing2 (a + 1); 0)
and do_nothing2 d =
  (do_nothing1 (a, "str"); " ")
in  do_nothing1 (0, "str2")




