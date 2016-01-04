(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* Error: Unbound value i *)

let _ =
while 10 > 5 do
  (i + 1;
   ())
done

