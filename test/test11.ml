(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

(* This expression has type string but an expression was
   expected of type int. *)

let x = ref 10

let _ = for i = 10 to " " do
          x := !x + i
        done



