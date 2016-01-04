(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)


(* Functions in this file are used for running the closure-converted output
   through the OCaml type checker (See: ./compile_cconv.sh). *)


exception Value_error

let undef = Obj.magic 0
let nil = Obj.magic 0

let ( ** ) init n =
  match init with
    [x] -> Array.make n x
  | _ -> raise (Invalid_argument "**")

let print_string =
    object
      val env = ();
      method apply = fun s -> Printf.printf "%s" s
    end

let print_i64 =
   object
     val env = ()
     method apply = fun x -> Printf.printf "%d\n" x
   end

let concat =
   object
     val env = ()
     method apply = fun (a, b) -> a ^ b
   end

let ref =
   object
     val env = ()
     method apply = fun x -> { contents = x }
   end

let ( ! ) =
   object
     val env = ()
     method apply = fun x -> x.contents
   end

let ( := ) =
   object
     val env = ()
     method apply = fun (x, y) -> x.contents <- y
   end

let bang = ( ! )

let isdigit =
   object
     val env = ()
     method apply = function
       "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> true
     | _ -> false
   end


let ord =
  object
    val env = ()
    method apply s =
      if s = "" then -1
      else if String.length s != 1 then raise Value_error
      else Char.code s.[0]
  end

let getchar =
  object
    val env = ()
    method apply () =
      try
        let c = input_char stdin in
        let b = Bytes.create 1 in
        Bytes.set b 0 c;
        Bytes.to_string b
      with End_of_file -> ""
  end

let chr =
  object
    val env = ()
    method apply d =
      let c = Char.chr d in
      let b = Bytes.create 1 in
      Bytes.set b 0 c;
      Bytes.to_string b
  end



