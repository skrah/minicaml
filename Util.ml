(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf
open Shared


(*** Util ***)
exception OverflowError


let post_incr r =
    let x = !r in
    if x = Pervasives.max_int then raise OverflowError
    else incr r; x

let pre_decr r =
    if !r = Pervasives.min_int then raise OverflowError
    else decr r; !r

let safe_incr r =
    if !r = Pervasives.max_int then raise OverflowError
    else incr r

let safe_decr r =
    if !r = Pervasives.min_int then raise OverflowError
    else decr r

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
    [] -> Bytes.unsafe_to_string result
  | c :: l -> Bytes.set result i c; imp (i + 1) l in
  imp 0 l

let startswith ~str ~prefix =
  let len = String.length prefix in
    if len > String.length str then false else
      String.sub str 0 len = prefix

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let find_dup cmp lst =
  let rec uniq = function
      [] -> None
    | [_] -> None
    | a :: (b :: _ as rest) -> match cmp a b with
                               | 0 -> Some b
                               | _ -> uniq rest
  in uniq (List.stable_sort cmp lst)

let tabulate n f =
  try Array.to_list (Array.init n f)
  with Invalid_argument _ -> internal_error "tabulate: invalid argument"

let indexof f lst =
    let rec find = function
      | [], _ -> internal_error "indexof: name does not exist"
      | x :: xs, i -> if f x then i else find (xs, i+1)
    in find (lst, 0)

let map_partial f lst =
    let rec select acc = function
      | [] -> acc
      | x :: xs -> (match f x with
                    | None -> select acc xs
                    | Some v -> select (v :: acc) xs)
    in List.rev (select [] lst)

let sprint_list f lst =
  let rec pr_elts = function
    | [] -> ""
    | [h] -> f h
    | h :: t -> f h ^ ", " ^ pr_elts t
  in  sprintf "[%s]" (pr_elts lst)

let sprint_tuple (f : 'a -> string) lst =
  let rec pr_elts = function
    | [] -> ""
    | [h] -> f h
    | h :: t -> f h ^ ", " ^ pr_elts t
  in  sprintf "%s" (pr_elts lst)

let intersperse ~lst ~insert =
  let rec loop = function
      [] -> []
    | [x] -> [x]
    | x :: xs -> x :: insert :: (loop xs)
  in loop lst

let append_unique lst x =
  let rec loop = function
      [] -> [x]
    | h :: t -> if h = x then internal_error "duplicate symbol"
                else h :: loop t
  in loop lst

let append_set lst x =
  let rec loop = function
      [] -> [x]
    | (h :: t) as l -> if h = x then l
                else h :: loop t
  in loop lst

let find_exn f lst =
  try Some (List.find f lst)
  with Not_found -> None

let rec exists_diff f lst =
  match lst with
    [] | [_] -> false
  | x :: (y :: _ as xs) ->
    if f x <> f y then true
    else exists_diff f xs

let rec last = function
    [] -> raise Not_found
  | [x] -> x
  | _ :: xs -> last xs

let fold_map f ctx lst =
  let g (ctx, acc) x =
    let (ctx', x') = f ctx x in
      (ctx', x' :: acc)
  in
  let (ctx', lst') = List.fold_left g (ctx, []) lst in
    (ctx', List.rev lst')

let mk_alphanum n =
  let alpha = n mod 26 in
  let num = n / 26 in
    if num > 0 then
      sprintf "%c%d" (Char.chr (Char.code 'a' + alpha)) num
    else
      sprintf "%c" (Char.chr (Char.code 'a' + alpha))


module String =
struct
  type t = string
  let equal x y = (x = y)
  let hash x = Hashtbl.hash x
end



