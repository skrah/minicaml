(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type int but an expression was
   expected of type unit. *)

let x =
let rec nfactor : int -> unit = fun (n : int) ->
      if n = 0
      then 1
      else n * nfactor (n - 1)
in  nfactor 10

let _ = 10.0 * x



