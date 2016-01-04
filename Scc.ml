(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

(** This module implements Tarjan's algorithm for finding
    strongly connected components in a graph. *)

exception Stack_underflow


type 'a vertex =
  { id : 'a;
    mutable adj : 'a vertex list;
    mutable index : int;
    mutable lowlink : int;
    mutable on_stack : bool }

let vertex_init id =
  { id = id;
    adj = [];
    index = -1;
    lowlink = -1;
    on_stack = false }


(** Tarjan's algorithm *)
let scc (graph : 'a vertex array) =
  let output = ref [] in
  let index = ref 0 in
  let stack = ref [] in

  let push v = stack := v :: !stack in
  let pop () = match !stack with
                 [] -> raise Stack_underflow
               | v :: vs -> stack := vs; v
  in

  let rec strongconnect v =
    v.index <- !index;
    v.lowlink <- !index;
    Util.safe_incr index;
    push v;
    v.on_stack <- true;

    let f w =
      if w.index < 0 then begin
        strongconnect w;
        v.lowlink <- min v.lowlink w.lowlink
      end else if w.on_stack then
        v.lowlink <- min v.lowlink w.index
    in

    List.iter f v.adj;

    if v.lowlink = v.index then
      let component = ref [] in
        let rec do_while () =
            let w = pop () in
              w.on_stack <- false;
              component := w :: !component;
              if (w != v) then do_while ()
        in do_while ();
           output := !component :: !output
  in

  Array.iter (fun v ->
                if v.index < 0 then strongconnect v)
             graph;

  List.rev (!output)


(** The input graph is represented as an array of integer adjacency lists.
    Convert to the work representation for scc(). *)
let adj_to_workrepr graph =
  let n = Array.length graph in
  let vertices = Array.init n vertex_init in

  let g i adj =
    let v = vertices.(i) in
      List.iter (fun j ->
                  let w = vertices.(j) in
                    v.adj <- w :: v.adj) adj
  in

  Array.iteri g graph;
  vertices

(** Convert the output of scc() back to the original node representation. *)
let workrepr_to_input vertices =
  List.map (fun lst -> List.map (fun v -> v.id) lst) vertices


(** Find the strongly connected components in a graph that is represented
    as an array of integer adjacency lists. *)
let scc_adj (graph : int list array) =
  let vertices = adj_to_workrepr graph in
  let output = scc vertices in
    workrepr_to_input output


(** Find the strongly connected components in a graph that is represented
    as a hash table of adjacency lists. *)
let scc_hash (graph : ('a, 'a list) Hashtbl.t) =
  (* key -> vertex map *)
  let map = Hashtbl.create (Hashtbl.length graph) in

  let init k _ acc =
    let v = vertex_init k in
      Hashtbl.add map k v;
      v :: acc
  in

  let lst = Hashtbl.fold init graph [] in
  let vertices = Array.of_list lst in

  let init_adj k adj =
    let v = Hashtbl.find map k in
      List.iter (fun k ->
                   let w = Hashtbl.find map k in
                     v.adj <- w :: v.adj) adj
  in

  Hashtbl.iter init_adj graph;

  let output = scc vertices in
    workrepr_to_input output



