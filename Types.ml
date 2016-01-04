(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Shared
open Symbol


(**********************************************************************)
(*                                Types                               *)
(**********************************************************************)

type t
  = Tany
  | Tvar of Tyvar.t
  | Tnil of tmeta ref
  | Tmeta of tmeta ref
  | Tfield of Tyfield.t * mutable_flag * t * t
  | Ttuple of t list
  | Trecord of (Tyfield.t * t) list
  | Tarrow of t * t
  | Tpoly of Tyvar.t list * t
  | Tapp of tfun * t list

and tfun
  = Tfun of Tyvar.t list * t
  | Tlink of Tycon.t
  | Tconstr of Tycon.t

and tmeta
  = Unbound of Tymeta.t * int
  | Link of t


(**********************************************************************)
(*                    Initialize primitive types                      *)
(**********************************************************************)

let tycon_id = ref 1000
let tyvar_id = ref 1000

let mk_tycon_id name =
  let id = Util.post_incr tycon_id in
    if id >= Tycon.start_id then failwith "reserved tycon ids exhausted"
    else Tycon.unsafe_make (name, id)

let mk_tyvar_id name =
  let id = Util.post_incr tyvar_id in
    if id >= Tyvar.start_id then internal_error "reserved tyvar ids exhausted"
    else Tyvar.unsafe_make (name, id)

let prim_unit_id = mk_tycon_id "unit"
let prim_bool_id = mk_tycon_id "bool"
let prim_int_id = mk_tycon_id "int"
let prim_float_id = mk_tycon_id "float"
let prim_string_id = mk_tycon_id "string"
let prim_array_id = mk_tycon_id "array"
let prim_unit = Tfun ([], Tapp (Tconstr prim_unit_id, []))
let prim_bool = Tfun ([], Tapp (Tconstr prim_bool_id, []))
let prim_int = Tfun ([], Tapp (Tconstr prim_int_id, []))
let prim_float = Tfun ([], Tapp (Tconstr prim_float_id, []))
let prim_string = Tfun ([], Tapp (Tconstr prim_string_id, []))
let prim_array =
  let v = mk_tyvar_id "a" in
    Tfun ([v], Tapp (Tconstr prim_array_id, [Tvar v]))

let prim_types = [
  prim_unit_id, prim_unit;
  prim_bool_id, prim_bool;
  prim_int_id, prim_int;
  prim_float_id, prim_float;
  prim_string_id, prim_string;
  prim_array_id, prim_array ]

let unit_t = Tapp (Tconstr prim_unit_id, [])
let bool_t = Tapp (Tconstr prim_bool_id, [])
let int64_t = Tapp (Tconstr prim_int_id, [])
let double_t = Tapp (Tconstr prim_float_id, [])
let string_t = Tapp (Tconstr prim_string_id, [])











(*
let rec repr = function
    { desc = Tnil; _ } -> sprintf "nil"
  | { desc = Tint; _ } -> sprintf "int"
  | { desc = Tstring; _ } -> sprintf "string"
  | { desc = Tunit; _ } -> sprintf "unit"
  | { desc = Tellipsis; _ } -> sprintf "ellipsis"
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  ->
      sprintf "(%s) -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord fields; _ } ->
      sprintf "{%s}" (Util.sprint_tuple repr_field fields)
  | { desc = Tarray typ; _ } -> sprintf "array of %s" (repr typ)

and repr_field = function (sym, typ) ->
    sprintf "%s : %s" (Symbol.name sym) (short_repr typ)

and repr_lst = function
  | [] -> ""
  | [x] -> short_repr x
  | x :: xs -> short_repr x ^ " * " ^ repr_lst xs

and short_repr = function
    { desc = Tnil; _ } -> "nil"
  | { desc = Tint; _ } -> "int"
  | { desc = Tstring; _ } -> "string"
  | { desc = Tunit; _ } -> "unit"
  | { desc = Tellipsis; _ } -> "..."
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  -> sprintf "%s -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord _; name; _ } -> sprintf "%s" (Symbol.name name)

int = Tfun ([], Tconstr ("int", []))

(* type r = { x : int } *)
  | Trecord of t list
record =
  Tfun ([a/x1000; b/x1001],
    Tconstr ("struct",  )

arrow =
  Tfun ([a/x1000; b/x1001],
    Tconstr ("arrow", Tvar(a/x1000), Tvar(b/x1001)))

foo =
  Tfun ([a/x1000; b/x1001],
    Tapp ("tuple",
      [Tapp ("arrow", Tvar(a/x1000), Tvar(b/x1001));
       Tapp ("arrow", Tvar(a/x1000), Tvar(b/x1001))])


let predef = [
  Tfun (['a; 'b], Tapp (Tcon "%arrow", ['a, 'b]))
  Tfun (['a; 'b], Tapp (Tcon "%arrow", ['a, 'b]))
  

type texpr = Tarrow of texpr * texpr

let foo = fun (a, b) -> (Tarrow (a, b), arrow (a, b))

  (foo (int, int) = 


(tfun ('a, 'b) -> ("arrow", 'a, 'b)) (int, int) = ("arrow", 'a, 'b)
(tfun () -> ("int",)) () = ("int",)


let arrow = (fun (a, b) -> (arrow (a, b)))


(*
(* type function declaration *)
and field =
  { field_name: string;
    field_mutable: mutable_flag;
    field_type: core_type;
    field_pos: pos }

type type_declaration =
  { type_name: Typeid.t;
    type_params: core_type list;
    type_expr: core_type option;
    type_pos: pos }
*)


(*

type desc =
    Tnil
  | Tint
  | Tstring
  | Tunit
  | Tellipsis
  | Tlink of Symbol.t
  | Tarrow of t list * t  (* formals, head type *)
  | Trecord of field list (* fields *)
  | Tarray of t           (* member type *)

and field = Symbol.t * t

and t =
  { mutable desc: desc;
    name: Symbol.t;
    mutable id: Guid.t }


let newty sym = { desc = Tlink sym; name = sym; id = Guid.get() }

let mk name desc =
  let ty = newty (Symbol.make name) in
    ty.desc <- desc; ty

let mk_funtype params result =
  let ty = newty (Symbol.make "arrow") in
    ty.desc <- Tarrow (params, result); ty

let prim_nil = mk "nil" Tnil
let prim_int = mk "int" Tint
let prim_string = mk "string" Tstring
let prim_unit = mk "unit" Tunit
let prim_ellipsis = mk "ellipsis" Tellipsis
let builtin_types = [prim_nil; prim_int; prim_string; prim_unit; prim_ellipsis]

let __runtime =
  ["print",     Tarrow ([prim_string], prim_unit);
   "print_i64", Tarrow ([prim_int], prim_unit);
   "flush",     Tarrow ([prim_unit], prim_unit);
   "getChar",   Tarrow ([prim_unit], prim_string);
   "ord",       Tarrow ([prim_string], prim_int);
   "chr",       Tarrow ([prim_int], prim_string);
   "size",      Tarrow ([prim_string], prim_int);
   "substring", Tarrow ([prim_string; prim_int; prim_int], prim_string);
   "concat",    Tarrow ([prim_string; prim_string], prim_string);
   "not",       Tarrow ([prim_int], prim_int);
   "exit",      Tarrow ([prim_int], prim_unit) ]

let builtin_functions = List.map (fun (x, y) -> mk x y) __runtime

let hd_type = function
    { desc = Tarrow (_, h); _ } -> h
  | _ -> internal_error "not a function"

let cl_type = function
    { desc = Tarrow (p :: _, _); _ } -> p
  | { desc = Tarrow _; _ } -> internal_error "expected closure parameter"
  | _ -> internal_error "not a function"

let skip_cl_type = function
    { desc = Tarrow (_ :: rest, h); _ } -> mk_funtype rest h
  | { desc = Tarrow _; _ } -> internal_error "expected closure parameter"
  | _ -> internal_error "not a function"

let parameters = function
    { desc = Tarrow (p, _); _ } -> p
  | _ -> internal_error "not a function"

let add_closure_param c = function
    { desc = Tarrow (p, h); _ } ->
      mk_funtype (c :: p) h
  | _ -> internal_error "not a function"

let is_callable = function
    { desc = Tarrow (_, _); _ } -> true
  | _ -> false

let rec eq x y =
  match x, y with
    { desc = Tint; _ }, { desc = Tint; _ } -> true
  | { desc = Tstring; _ }, { desc = Tstring; _ } -> true
  | { desc = Tunit; _ }, { desc = Tunit; _ } -> true
  | { desc = Tarrow (l1, h1); _ }, {desc = Tarrow (l2, h2); _ } -> x.id = y.id || (lst_eq l1 l2 && eq h1 h2)
  | { desc = Trecord _; _ }, { desc = Trecord _; _ } -> x.id = y.id
  | { desc = Tnil; _ }, { desc = Trecord _; _ } -> true
  | { desc = Trecord _; _ }, { desc = Tnil; _ } -> true
  | { desc = Tarray _; _ }, { desc = Tarray _; _ } -> x.id = y.id
  | _ -> false
  
and lst_eq x y =
  match x, y with
    [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | x :: xs, y :: ys -> if not (eq x y) then false else lst_eq xs ys

let neq x y = not (eq x y)

let unify_oper (oper, left, right) =
  match left, right with
    { desc = Tint; _ }, { desc = Tint; _ } -> true
  | { desc = Tstring; _ }, { desc = Tstring; _ } -> Ast.is_cmpop oper
  | { desc = Trecord _; _ }, _ -> Ast.is_eqop oper && eq left right (* pointer equality *)
  | _, { desc = Trecord _; _ } -> Ast.is_eqop oper && eq left right (* pointer equality *)
  | { desc = Tarray _; _ }, { desc = Tarray _; _ } -> Ast.is_eqop oper && eq left right (* pointer equality *)
  | _ -> false

let rec repr = function
    { desc = Tnil; _ } -> sprintf "nil"
  | { desc = Tint; _ } -> sprintf "int"
  | { desc = Tstring; _ } -> sprintf "string"
  | { desc = Tunit; _ } -> sprintf "unit"
  | { desc = Tellipsis; _ } -> sprintf "ellipsis"
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  ->
      sprintf "(%s) -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord fields; _ } ->
      sprintf "{%s}" (Util.sprint_tuple repr_field fields)
  | { desc = Tarray typ; _ } -> sprintf "array of %s" (repr typ)

and repr_field = function (sym, typ) ->
    sprintf "%s : %s" (Symbol.name sym) (short_repr typ)

and repr_lst = function
  | [] -> ""
  | [x] -> short_repr x
  | x :: xs -> short_repr x ^ " * " ^ repr_lst xs

and short_repr = function
    { desc = Tnil; _ } -> "nil"
  | { desc = Tint; _ } -> "int"
  | { desc = Tstring; _ } -> "string"
  | { desc = Tunit; _ } -> "unit"
  | { desc = Tellipsis; _ } -> "..."
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  -> sprintf "%s -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord _; name; _ } -> sprintf "%s" (Symbol.name name)
  | { desc = Tarray _; name; _ } -> sprintf "%s" (Symbol.name name)



(*
let rec repr = function
    { desc = Tnil; id; _ } -> sprintf "nil/%d" (Guid.as_int id)
  | { desc = Tint; id; _ } -> sprintf "int/%d" (Guid.as_int id)
  | { desc = Tstring; id; _ } -> sprintf "string/%d" (Guid.as_int id)
  | { desc = Tunit; id; _ } -> sprintf "unit/%d" (Guid.as_int id)
  | { desc = Tellipsis; id; _ } -> sprintf "ellipsis/%d" (Guid.as_int id)
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  ->
      sprintf "(%s) -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord fields; name; id } ->
      sprintf "%s/%d {%s}" (Symbol.name name) (Guid.as_int id) (Util.sprint_tuple repr_field fields)
  | { desc = Tarray typ; _ } -> sprintf "array of %s" (repr typ)

and repr_field = function (sym, typ) ->
    sprintf "%s : %s" (Symbol.name sym) (short_repr typ)

and repr_lst = function
  | [] -> ""
  | [x] -> short_repr x
  | x :: xs -> short_repr x ^ " * " ^ repr_lst xs

and short_repr = function
    { desc = Tnil; _ } -> "nil"
  | { desc = Tint; _ } -> "int"
  | { desc = Tstring; _ } -> "string"
  | { desc = Tunit; _ } -> "unit"
  | { desc = Tellipsis; _ } -> "..."
  | { desc = Tlink name; _ } -> (Symbol.name name)
  | { desc = Tarrow (lst, h); _ }  -> sprintf "%s -> %s" (repr_lst lst) (short_repr h)
  | { desc = Trecord _; name; id } -> sprintf "%s/%d" (Symbol.name name) (Guid.as_int id)
  | { desc = Tarray _; name; id } -> sprintf "%s/%d" (Symbol.name name) (Guid.as_int id)
*)
*)


*)
