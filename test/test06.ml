(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)


let rec do_nothing1 : int * string -> unit = fun ((a, b) : int * string) ->
  do_nothing2 (a + 1)
and do_nothing2 : int -> unit = fun (d : int) ->
  do_nothing1 (d, "str")

let _ = do_nothing1 (0, "str2")



