(* Pi benchmark *)

let pi_float () =
  let lasts = 0.0 in
  let t = 3.0 in
  let s = 3.0 in
  let n = 1.0 in
  let na = 0.0 in
  let d = 0.0 in
  let da = 24.0 in

  let rec loop (lasts, t, s, n, na, d, da) =
    if s = lasts then s
    else
      let lasts  = s in
      let n = n +. na in
      let na = na +. 8.0 in
      let d = d +. da in
      let da = da +. 32.0 in
      let tmp = t *. n in
      let t = tmp /. d in
      let s = s +. t in
        loop (lasts, t, s, n, na, d, da)

  in  loop (lasts, t, s, n, na, d, da)


let _ =
  for i = 0 to 9999999 do
    let _ = pi_float () in ()
  done

let _ = print_float (pi_float ()); print_string "\n"




