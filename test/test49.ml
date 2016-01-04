(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

let int = 5
let x = ref int
let y = ref 7
let z = "abc"
type xint = int
let f : int * string -> string = fun ((a, b) : int * string) -> b
type xrec = { mutable u : string; mutable v : int }

let _ =
let a = !x * !y in
let b : int = a * a in
let r = {u="AAAAA"; v=200} in
  x;
  y;
  f (a, z);
  x := 5;
  if !x * !y + 22 - 500 = 100
  then (r.u;
        ());
  while (!x * !y < 200) do
    (x := !x - 1;
     x;
     ();
     if 25 = 25 then ())
  done;
  for x = !x * !y to !y do
    (y := x * x;
     if 1 = 1 then ();
     ())
  done


