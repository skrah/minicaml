(* Example from Appel's Modern Compiler Implementation in ML, translated
   to Caml. *)


type tree = { key: string; left: tree; right: tree }
let t = { key="YAY"; left=nil; right=nil }

let prettyprint tree =
  let output = ref "" in

  let write s =
    output := concat (!output, s)
  in

  let rec show n t =
    let indent s =
      for i = 1 to n do
        write (" ")
      done;
      output := concat (!output, s);
      write ("\n")
    in
 
    if t == nil then indent(".")
                else (indent t.key;
                      show (n + 1) t.left;
                      show (n + 1) t.right)
  in show 0 tree;
     print_string (!output)

let _ = prettyprint t



