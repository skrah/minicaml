(* Example from Appel's Modern Compiler Implementation in ML, translated
   to Caml. *)

let ref x = { contents = x }
let (!) x = x.contents
let (:=) (x, y) = x.contents <- y


let pi_float () =
  let lasts = ref 0.0 in
  let t = ref 3.0 in
  let s = ref 3.0 in
  let n = ref 1.0 in
  let na = ref 0.0 in
  let d = ref 0.0 in
  let da = ref 24.0 in

  while !lasts <> !s do
    (lasts := !s;
     n := !n +. !na;
     na := !na +. 8.0;
     d := !d +. !da;
     da := !da +. 32.0;
     let tmp = !t *. !n in
     t := tmp /. !d;
     s := !s +. !t)
  done;

  !lasts


let _ =
  for i = 0 to 999999 do
    let _ = pi_float () in ()
  done

let _ = print_float (pi_float ()); print_string "\n"




