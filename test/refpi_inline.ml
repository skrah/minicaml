(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)


let pi_float () =
  let lasts = { contents = 0.0 } in
  let t = { contents = 3.0 } in
  let s = { contents = 3.0 } in
  let n = { contents = 1.0 } in
  let na = { contents = 0.0 } in
  let d = { contents = 0.0 } in
  let da = { contents = 24.0 } in

  while lasts.contents <> s.contents do
    (lasts.contents <- s.contents;
     n.contents <- n.contents +. na.contents;
     na.contents <- na.contents +. 8.0;
     d.contents <- d.contents +. da.contents;
     da.contents <- da.contents +. 32.0;
     let tmp = t.contents *. n.contents in
     t.contents <- tmp /. d.contents;
     s.contents <- s.contents +. t.contents)
  done;

  lasts.contents


let _ =
  for i = 0 to 999999 do
    let _ = pi_float () in ()
  done

let _ = print_float (pi_float ()); print_string "\n"




