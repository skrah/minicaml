(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* File "test18.ml", line 5, characters 3-14:
     Error: Unbound value do_nothing2 *)

let rec do_nothing1 (a, b) =
  (do_nothing2 (a + 1); 0)
let rec x = 0
and do_nothing2 x =
  (do_nothing1 (x, "str"); " ")

let _ = do_nothing1 (0, "str2")




