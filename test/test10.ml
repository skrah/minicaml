(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression should have type unit. *)

let _ =
  while (10 > 5) do
    5 + 6
  done



