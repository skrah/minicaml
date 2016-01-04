(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

exception Stack_underflow
val scc_adj : int list array -> int list list
val scc_hash : ('a, 'a list) Hashtbl.t -> 'a list list
