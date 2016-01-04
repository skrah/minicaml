(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

let rec do_nothing1 : int * string -> int = fun ((a, b) : int * string) ->
  do_nothing2 (a + 1); 0
and do_nothing2 : int -> string = fun (d : int) ->
  do_nothing1 (d, "str"); " "

let _ = do_nothing1 (0, "str2")



