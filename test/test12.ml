(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

let _ =
  let a = ref 0
  in  for i = 0 to 100 do
        a := !a + 1; ()
      done



