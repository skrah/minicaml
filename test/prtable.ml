(* Example from Appel's Modern Compiler Implementation in ML, translated
   to Caml. *)

let cprint s c = print_string s; c

type xlist = { head : int; tail : xlist }
let xnil = { head = 0; tail = nil }


let rec put_int i c =
  if i = 0 then c ()
  else let rest = i / 10 in
       let dig = i - rest * 10 in
         put_int rest (fun () -> (cprint (chr (dig + ord "0")) c) ())

let print_table l c =
  let rec do_list l =
    if l == xnil then c ()
                else let do_rest () = do_list l.tail in
                     let i = l.head in
                     let again () = put_int (i+i) (cprint "\n" do_rest)
                     in put_int i (cprint "\n" again)
  in do_list l

let l =
  { head = 1000; tail =
    { head = 2000; tail =
      { head = 3000; tail = xnil }}}

let _ = print_table l (fun () -> print_string "\n")


