(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

let z = 4 + 5

let add n =
  let h m = n + m in h

let addFive = add 5

let addSeven = add 7

let twenty = addFive 15

let twentyTwo = addSeven 15

let twice f =
  let g x = f (f x) in g

let addTen = twice addFive

let seventeen = (twice (add 5)) 7

let addTwentyFour = twice (twice (add 6))

let _ = print_i64(addTwentyFour seventeen)
let _ = print_string "\n"



