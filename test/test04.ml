(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)


let rec nfactor : int -> int = fun (n : int) ->
  if n <= 0 then 1
  else n * nfactor (n - 1)

let _ = print_i64 (nfactor 10)




