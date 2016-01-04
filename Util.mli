(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

exception OverflowError
val post_incr : int ref -> int
val pre_decr : int ref -> int
val safe_incr : int ref -> unit
val safe_decr : int ref -> unit
val explode : string -> char list
val implode : char list -> string
val startswith : str:string -> prefix:string -> bool
val create_hashtable : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
val find_dup : ('a -> 'a -> int) -> 'a list -> 'a option
val tabulate : int -> (int -> 'a) -> 'a list
val indexof : ('a -> bool) -> 'a list -> int
val map_partial : ('a -> 'b option) -> 'a list -> 'b list
val sprint_list : ('a -> string) -> 'a list -> string
val sprint_tuple : ('a -> string) -> 'a list -> string
val intersperse : lst:'a list -> insert:'a -> 'a list
val append_unique : 'a list -> 'a -> 'a list
val append_set : 'a list -> 'a -> 'a list
val find_exn : ('a -> bool) -> 'a list -> 'a option
val exists_diff : ('a -> 'b) -> 'a list -> bool
val last : 'a list -> 'a
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val mk_alphanum : int -> string
module String :
  sig type t = string val equal : 'a -> 'a -> bool val hash : 'a -> int end
