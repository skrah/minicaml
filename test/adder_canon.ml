(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

let z : int = 4 + 5

let add = fun (n : int) -> 
            let h = fun (m : int) -> 
                      n + m in
            h

let addFive : int -> int = add 5

let addSeven : int -> int = add 7

let twenty : int = addFive 15

let twentyTwo : int = addSeven 15

let twice = fun (f : int -> int) -> 
              let g = fun (x : int) -> 
                        f (f x) in
              g

let addTen : int -> int = twice addFive

let seventeen : int = twice (add 5) 7

let addTwentyFour : int -> int = twice (twice (add 6))

let _ = addTwentyFour seventeen

