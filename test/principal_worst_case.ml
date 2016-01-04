(* Hindley-Milner algorithm takes exponential time here. *)

let _ =
let pair = fun x -> (fun y -> (fun z -> z x y)) in
let x1 = fun y -> pair y y in
let x2 = fun y -> x1 (x1 y) in
let x3 = fun y -> x2 (x2 y) in
let x4 = fun y -> x3 (x3 y) in
let x5 = fun y -> x4 (x4 y) in
  x5 (fun y -> y)


