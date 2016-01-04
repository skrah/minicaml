(* Example from Appel's Modern Compiler Implementation in ML, translated
   to Caml. *)

type cont = unit -> unit

let cprint = fun (s : string) -> 
               fun (c : cont) -> 
                 print_string s;
                 c

type xlist = {head : int; tail : xlist}

let rec xnil : xlist = {head = 0; tail = xnil}

let rec put_int = fun (i : int) -> 
                    fun (c : cont) -> 
                      if i = 0
                      then c ()
                      else let rest : int = i / 10 in
                           let dig : int = i - rest * 10 in
                           put_int rest (fun () -> 
                                           cprint (chr (dig + ord "0")) c ())

let print_table = fun (l : xlist) -> 
                    fun (c : cont) -> 
                      let rec do_list = fun (l : xlist) -> 
                                          if l == xnil
                                          then c ()
                                          else let do_rest = fun () -> 
                                                               do_list l.tail in
                                               let i : int = l.head in
                                               let again = fun () -> 
                                                             put_int (i + i) (cprint "\n" do_rest) in
                                               put_int i (cprint "\n" again) in
                      do_list l

let l : xlist = {head = 1000; tail = {head = 2000; tail = {head = 3000; tail = xnil}}}

let _ = print_table l (fun () -> 
                         print_string "\n")

