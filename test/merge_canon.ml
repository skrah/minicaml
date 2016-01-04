(* Example from Appel's "Modern Compiler Implementation in ML", translated
   to Caml. *)

type xlist = {first : int; rest : xlist}

type any = {mutable any : bool}

let buffer = ref (getchar ())

let printint = fun (i : int) -> 
                 let rec f = fun (i : int) -> 
                               if i > 0
                               then f (i / 10);
                                    print_string (chr (i - i / 10 * 10 + ord "0")) in
                 if i < 0
                 then (print_string "-";
                       f (-i))
                 else if i > 0
                      then f i
                      else print_string "0"

let readint = fun (any : any) -> 
                let i = ref 0 in
                let isdigit = fun (s : string) -> 
                                ord !buffer >= ord "0" && ord !buffer <= ord "9" in
                let skipto = fun () -> 
                               while (!buffer = " " || !buffer = "\n") do
                                 buffer := getchar ()
                               done in
                skipto ();
                any.any <- isdigit !buffer;
                while (isdigit !buffer) do
                  i := !i * 10 + ord (!buffer) - ord "0";
                  buffer := getchar ()
                done;
                !i

let rec readlist = fun () -> 
                     let any = ({any = false} : any) in
                     let i = readint any in
                     if any.any
                     then ({first = i; rest = readlist ()} : xlist)
                     else nil

let rec merge = fun (a : xlist) -> 
                  fun (b : xlist) -> 
                    if a == nil
                    then b
                    else if b == nil
                         then a
                         else if a.first < b.first
                              then ({first = a.first; rest = merge a.rest b} : xlist)
                              else ({first = b.first; rest = merge a b.rest} : xlist)

let rec printlist = fun (l : xlist) -> 
                      if l == nil
                      then print_string "\n"
                      else printint l.first;
                           print_string " ";
                           printlist l.rest

let list1 = readlist ()

let list2 = buffer := getchar ();
            readlist ()

let _ = printlist (merge list1 list2)

