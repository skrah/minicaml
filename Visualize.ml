(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf


let rec xquote = function
  | [] -> []
  | '\\' :: xs -> '\\' :: '\\' :: xquote xs
  | '\n' :: xs -> '\\' :: 'n' :: xquote xs
  | '\t' :: xs -> '\\' :: 't' :: xquote xs
  | x :: xs    -> x :: xquote xs


(*** Convert ParseTree back to re-indented source ***)
module ParseTreePrinter =
struct
open Shared
open ParseTree

let indent ch d = fprintf ch "%*s" d ""
let newline ch d = fprintf ch "\n%a" indent d

let quote s = "\"" ^ Util.implode (xquote (Util.explode s)) ^ "\""
let extra s = String.length s
let exprextra = function
    { pexp_desc = Pexp_path (Pvar_simple (s, _)); _ } -> extra s
  | _ -> 0


and mutable_flag = function
      Immutable -> ""
    | Mutable -> "mutable "


let rec list_single_comma (d : int) (ch : out_channel) f = function
    [a] -> f d ch a
  | a :: r -> f d ch a; fprintf ch  ", "; list_single_comma d ch f r
  | [] -> ()

let rec list_single_star (d : int) (ch : out_channel) f = function
    [a] -> f d ch a
  | a :: r -> f d ch a; fprintf ch  " * "; list_single_star d ch f r
  | [] -> ()

let rec list_multi_semi d ch f = function
    [a]    -> f d ch a
  | a :: r -> f d ch a; fprintf ch ";\n%a" indent d; list_multi_semi d ch f r
  | []     -> ()


let rec module_expr tree =
   let (_, Pmod_structure items) = tree in
   structure_item_list 0 stdout items;
   fprintf stdout "\n\n"

and structure_item_list d ch = function
    [a] -> structure_item d ch a
  | (Pstr_type [_] as a) :: ((Pstr_type [_]) :: _ as r)
  | (Pstr_primitive _ as a) :: ((Pstr_primitive _) :: _ as r) ->
      structure_item d ch a; newline ch d;
      structure_item_list d ch r
  | a :: r -> structure_item d ch a; newline ch d; newline ch d;
              structure_item_list d ch r
  | []     -> ()

and structure_item d ch = function

    Pstr_type [] -> ()

  | Pstr_type decs ->
      let d = 5 + extra (List.hd decs).ptyp_name + 3 in
        fprintf ch "%a" (typedecs "type" d) decs

  | Pstr_primitive { pvb_desc = { pvd_name; pvd_type=Some c; _ }; pvb_expr = x } ->
      fprintf ch "external %s : %a = %a"
        pvd_name
        (type_expression d) c
        (expression d) x

  | Pstr_primitive _ ->
      internal_error "invalid external definition"

  | Pstr_value (rec_flag, bindings) ->
      fprintf ch "%a\n" (value_bindings rec_flag d) bindings

and typedecs prefix d ch = function
    [a]    -> type_declaration prefix d ch a
  | a :: r -> type_declaration prefix d ch a; newline ch d;
              typedecs "and" d ch r
  | []     -> ()

and type_declaration prefix d ch { ptyp_name; ptyp_params; ptyp_expr; _ } =
      fprintf ch "%s %a%s = %a"
        prefix
        (type_parameter_list d) ptyp_params
        ptyp_name
        (type_expression_opt_nc d) ptyp_expr

and type_parameter_list d ch lst =
    let do_print d ch lst = list_single_comma d ch type_expression lst in
      match lst with
      | [] -> ()
      | [x] -> fprintf ch "%a " (type_expression d) x
      | _ -> fprintf ch "(%a) " (do_print d) lst

and type_expression_opt d ch t =
  match t with
    None -> ()
  | Some c -> fprintf ch " : %a" (type_expression d) c

and type_expression_opt_nc d ch t =
  match t with
    None -> ()
  | Some c -> fprintf ch "%a" (type_expression d) c

and type_expression d ch t =
  match t.core_type_desc with

    Ptyp_any -> fprintf ch "_"

  | Ptyp_var s -> fprintf ch "'%s" s

  | Ptyp_arrow (a, b) ->
      fprintf ch "%a -> %a"
        (type_expression d) a
        (type_expression d) b

  | Ptyp_tuple lst -> fprintf ch "%a" (type_expression_list d) lst

  | Ptyp_record lst -> fprintf ch "{ %a }" (field_declaration_list (d+2)) lst

  | Ptyp_constr (s, lst) ->
      begin match lst with
        [] -> fprintf ch "%s" s
      | _ -> fprintf ch "%a%s"
               (type_parameter_list d) lst
               s
      end

and type_expression_list d ch lst = list_single_star d ch type_expression lst
and field_declaration_list d ch lst = list_multi_semi d ch field_declaration lst

and field_declaration d ch { pfield_name; pfield_mutable; pfield_type; _ } =
      fprintf ch "%s%s : %a"
        (mutable_flag pfield_mutable)
        pfield_name
        (type_expression d) pfield_type

and type_expression_length t =
    let count fd =
      let buf = Bytes.create 1 in
      let rec loop acc =
        match Unix.read fd buf 0 1 with
          0 -> acc
        | 1 -> loop (acc+1)
        | _ -> internal_error "unexpected return value"
      in loop 0
    in

    let (in_fd, out_fd) = Unix.pipe () in
    let ch = Unix.out_channel_of_descr out_fd in
      type_expression 0 ch t;
      flush ch;
      Unix.close out_fd;
      let n = count in_fd in
        Unix.close in_fd; n

and valextra v =
  match v.pvd_type with
    Some c -> extra v.pvd_name + 3 + (type_expression_length c)
  | None -> extra v.pvd_name + 3

and type_prologue _ ch t =
  match t with
    None -> ()
  | _ -> fprintf ch "("

and type_epilogue d ch t =
  match t with
    None -> ()
  | Some c -> fprintf ch " : %a)" (type_expression d) c


and value_bindings rec_flag d ch bindings =
  match rec_flag with
    Recursive -> iter_bindings "let rec" d ch bindings
  | Nonrecursive -> iter_bindings "let" d ch bindings

and iter_bindings prefix d ch = function
    [a] -> value_binding prefix d ch a
  | a :: r -> value_binding prefix d ch a; newline ch d;
              iter_bindings "and" d ch r
  | [] -> internal_error "empty value binding list"

and value_binding prefix d ch { pvb_desc; pvb_expr } =
    let d' = d + 4 + String.length prefix + valextra pvb_desc in
      fprintf ch "%s %a = %a"
        prefix
        (value_description d) pvb_desc
        (expression d') pvb_expr

and value_description d ch { pvd_name; pvd_type; _ } =
    fprintf ch "%s%a" pvd_name (type_expression_opt d) pvd_type

and value_description_list d ch lst = list_single_comma d ch value_description lst


and expression d ch = function

    { pexp_desc = Pexp_nil; pexp_type; _ } ->
       fprintf ch "%anil%a"
         (type_prologue d) pexp_type
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_any; pexp_type; _ } ->
       fprintf ch "%a_%a"
         (type_prologue d) pexp_type
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_bool b; pexp_type; _ } ->
       fprintf ch "%a%b%a"
         (type_prologue d) pexp_type
         b 
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_int s; pexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) pexp_type
         s
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_float s; pexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) pexp_type
         s
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_uminus expr; pexp_type; _ } ->
       fprintf ch "%a-%a%a"
         (type_prologue d) pexp_type
         (expression d) expr
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_string s; pexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) pexp_type
         (quote s)
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_path p; pexp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) pexp_type
         (path d) p
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_assign (p, op, expr); pexp_type; _ } ->
        fprintf ch "%a%a %s %a%a"
          (type_prologue d) pexp_type
          (path d) p
          (assign_op_repr op)
          (expression d) expr
          (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_op (left, operand, right); pexp_type; _ } ->
       fprintf ch "%a%a %s %a%a"
         (type_prologue d) pexp_type
         (expression d) left
         (op_repr operand)
         (expression d) right
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_call (expr, arg); pexp_type; _ } ->
       let d' = d + exprextra expr + 1 in
         fprintf ch "%a%a(%a)%a"
           (type_prologue d) pexp_type
           (fix_deref d) expr
           (expression d') arg
           (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_record assignment_list; pexp_type; _ } ->
       fprintf ch "%a{ %a }%a"
         (type_prologue d) pexp_type
         (field_assignments (d+2)) assignment_list
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_array (size, init); pexp_type; _ } ->
       fprintf ch "%a[%a] ** (%a)%a"
         (type_prologue d) pexp_type
         (expression d) init
         (expression d) size
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_if (test, expr, None); pexp_type; _ } ->
       let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a%a"
           (type_prologue d) pexp_type
           (expression d) test
           indent d
           (expression d') expr
           (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_if (test, then_expr, Some else_expr); pexp_type; _ } ->
      let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a\n%aelse %a%a"
           (type_prologue d) pexp_type
           (expression d) test
           indent d
           (expression d') then_expr
           indent d
           (expression d') else_expr
           (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_while (test, body); pexp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%awhile (%a) do\n%a%a\n%adone%a"
           (type_prologue d) pexp_type
           (expression d) test
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_for ([loop_var], hi, body); pexp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%afor %s = %a to %a do\n%a%a\n%adone%a"
           (type_prologue d) pexp_type
           loop_var.pvb_desc.pvd_name
           (expression d) loop_var.pvb_expr
           (expression d) hi
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_for (_, _, _); _ } ->
      internal_error "invalid for expression"

  | { pexp_desc = Pexp_tuple lst; pexp_type; _ } ->
       fprintf ch "(%a%a%a)"
         (type_prologue d) pexp_type
         (expression_list_comma d) lst
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_sequence []; pexp_type; _ } ->
       fprintf ch "%a()%a"
         (type_prologue d) pexp_type
         (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_sequence lst; pexp_type; _ } ->
       let d, o, c = match lst with
                       _::_::_ -> d+1, "(", ")"
                     | _ -> d, "", "" in
         fprintf ch "%s%a%a%a%s" o
           (type_prologue d) pexp_type
           (expression_list_semi d) lst
           (type_epilogue d) pexp_type c

  | { pexp_desc = Pexp_let (_, [], _); _ } ->
      internal_error "let: empty value binding list"

  | { pexp_desc = Pexp_let (rec_flag, bindings, body); pexp_type; _ } ->
        fprintf ch "%a%a in\n%a%a%a"
          (type_prologue d) pexp_type
          (value_bindings rec_flag d) bindings
          indent d
          (expression d) body
          (type_epilogue d) pexp_type

  | { pexp_desc = Pexp_lambda l; pexp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) pexp_type
         (lambda_expression d) l
         (type_epilogue d) pexp_type

and fix_deref d ch x =
  match x.pexp_desc with
    Pexp_path (Pvar_simple ("(!)", _)) -> fprintf ch "!"
  | _ -> expression d ch x; fprintf ch " "


and path d ch = function

    Pvar_simple (s, _) ->
      fprintf ch "%s" s

  | Pvar_field (var, s, _) ->
      fprintf ch "%a.%s"
        (path d) var
        s

  | Pvar_subscript (var, expr, _) ->
      fprintf ch "%a.(%a)"
        (path d) var
        (expression d) expr

and lambda_expression d ch { pfun_params; pfun_body; _ } =
    let d' = d+2 in
      fprintf ch "fun (%a) -> \n%a%a"
        (value_description_list d) pfun_params
        indent d'
        (expression d') pfun_body

and expression_list_comma d ch lst = list_single_comma d ch expression lst
and expression_list_semi d ch lst = list_multi_semi d ch expression lst

and field_assign d ch (s, expr) =
    fprintf ch "%s = %a"
      s 
      (expression d) expr

and field_assignments d ch lst = list_multi_semi d ch field_assign lst

let indent = module_expr


end


(*** Convert Ast back to re-indented source ***)
module AstPrinter (ModuleState : ModuleState.S)=
struct
open Shared
open Types
open Ast
open ModuleState

module Unify = Unify.Make(ModuleState)

let indent ch d = fprintf ch "%*s" d ""
let newline ch d = fprintf ch "\n%a" indent d

let add_space s = match s with "" -> "" | _ -> s ^ " "
let quote s = "\"" ^ Util.implode (xquote (Util.explode s)) ^ "\""
let extra s = String.length s
let vsym id = ValueID.name id
let id_extra id = String.length (vsym id)
let exprextra = function
    { aexp_desc = Aexp_path (Avar_simple (id, _, _)); _ } -> id_extra id
  | _ -> 0


let rec list_single_comma (d : int) (ch : out_channel) f = function
    [a] -> f d ch a
  | a :: r -> f d ch a; fprintf ch  ", "; list_single_comma d ch f r
  | [] -> ()

let rec list_multi_semi d ch f = function
    [a]    -> f d ch a
  | a :: r -> f d ch a; fprintf ch ";\n%a" indent d; list_multi_semi d ch f r
  | []     -> ()


let rec module_expr tree =
   let (_, Amod_structure decls) = tree in
   structure_item_list 0 stdout decls;
   fprintf stdout "\n\n"

and structure_item_list d ch = function
    [a] -> structure_item d ch a
  | (Astr_type [_] as a) :: ((Astr_type [_]) :: _ as r)
  | (Astr_primitive _ as a) :: ((Astr_primitive _) :: _ as r) ->
      structure_item d ch a; newline ch d;
      structure_item_list d ch r
  | a :: r -> structure_item d ch a; newline ch d; newline ch d;
              structure_item_list d ch r
  | []     -> ()

and structure_item d ch = function

  | Astr_type [] -> ()

  | Astr_type decs ->
      let d = 5 + id_extra (List.hd decs).atyp_id + 3 in
      fprintf ch "%a" (typedecs "type" d) decs

  | Astr_primitive { avb_desc = { avd_id; _ }; avb_expr = x } ->
      let tfun = TyconTable.really_find avd_id in
        fprintf ch "external %s : %s = %a"
          (vsym avd_id)
          (Unify.repr (Tapp (tfun, [])))
          (expression d) x

  | Astr_value (rec_flag, bindings) ->
      fprintf ch "%a\n" (value_bindings rec_flag d) bindings

and typedecs prefix d ch = function
    [a] -> type_declaration prefix d ch a
  | a :: r -> type_declaration prefix d ch a; newline ch d;
              typedecs "and" d ch r
  | [] -> internal_error "empty value binding list"

and type_declaration prefix _ ch { atyp_id; _ } =
      let tfun = TyconTable.really_find atyp_id in
      let params, body = Unify.tfun_repr tfun in
        fprintf ch "%s %s%s = %s"
          prefix (add_space params) (Tycon.name atyp_id) body


and valextra v =
  match v.avd_constraint with
    Some c -> id_extra v.avd_id + 3 + (extra (Unify.repr c))
  | None -> id_extra v.avd_id + 3

and type_expression_opt _ ch t =
  match t with
    None -> ()
  | Some c -> fprintf ch " : %s" (Unify.repr c)

and type_prologue _ ch t =
  match t with
    None -> ()
  | _ -> fprintf ch "("

and type_epilogue _ ch t =
  match t with
    None -> ()
  | Some c -> fprintf ch " : %s" (Unify.repr c)


and expression d ch = function

    { aexp_desc = Aexp_nil; aexp_type; _ } ->
       fprintf ch "%anil%a"
         (type_prologue d) aexp_type
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_any; aexp_type; _ } ->
       fprintf ch "%a_%a"
         (type_prologue d) aexp_type
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_bool b; aexp_type; _ } ->
       fprintf ch "%a%b%a"
         (type_prologue d) aexp_type
         b 
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_int s; aexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) aexp_type
         (Int64.to_string s)
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_float s; aexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) aexp_type
         (string_of_float s)
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_uminus expr; aexp_type; _ } ->
       fprintf ch "%a-%a%a"
         (type_prologue d) aexp_type
         (expression d) expr
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_string s; aexp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) aexp_type
         (quote s)
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_path p; aexp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) aexp_type
         (path d) p
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_assign (p, op, expr); aexp_type; _ } ->
        fprintf ch "%a%a %s %a%a"
          (type_prologue d) aexp_type
          (path d) p
          (assign_op_repr op)
          (expression d) expr
          (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_op (left, operand, right); aexp_type; _ } ->
       fprintf ch "%a%a %s %a%a"
         (type_prologue d) aexp_type
         (expression d) left
         (op_repr operand)
         (expression d) right
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_call (expr, arg); aexp_type; _ } ->
       let d' = d + exprextra expr + 1 in
         fprintf ch "%a%a(%a)%a"
           (type_prologue d) aexp_type
           (fix_deref d) expr
           (expression d') arg
           (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_record assignment_list; aexp_type; _ } ->
       fprintf ch "%a{ %a }%a"
         (type_prologue d) aexp_type
         (field_assignments (d+2)) assignment_list
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_array (size, init); aexp_type; _ } ->
       fprintf ch "%a[%a] ** (%a)%a"
         (type_prologue d) aexp_type
         (expression d) init
         (expression d) size
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_if (test, expr, None); aexp_type; _ } ->
       let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a%a"
           (type_prologue d) aexp_type
           (expression d) test
           indent d
           (expression d') expr
           (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_if (test, then_expr, Some else_expr); aexp_type; _ } ->
      let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a\n%aelse %a%a"
           (type_prologue d) aexp_type
           (expression d) test
           indent d
           (expression d') then_expr
           indent d
           (expression d') else_expr
           (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_while (test, body); aexp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%awhile (%a) do\n%a%a\n%adone%a"
           (type_prologue d) aexp_type
           (expression d) test
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_for ([loop_var], hi, body); aexp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%afor %s = %a to %a do\n%a%a\n%adone%a"
           (type_prologue d) aexp_type
           (vsym loop_var.avb_desc.avd_id)
           (expression d) loop_var.avb_expr
           (expression d) hi
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_for (_, _, _); _ } ->
      internal_error "invalid for expression"

  | { aexp_desc = Aexp_tuple lst; aexp_type; _ } ->
       fprintf ch "%a(%a)%a"
         (type_prologue d) aexp_type
         (expression_list_comma d) lst
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_sequence []; aexp_type; _ } ->
       fprintf ch "%a()%a"
         (type_prologue d) aexp_type
         (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_sequence lst; aexp_type; _ } ->
       let d, o, c = match lst with
                       _::_::_ -> d+1, "(", ")"
                     | _ -> d, "", "" in
         fprintf ch "%s%a%a%a%s" o
           (type_prologue d) aexp_type
           (expression_list_semi d) lst
           (type_epilogue d) aexp_type c

  | { aexp_desc = Aexp_let (_, [], _); _ } ->
      internal_error "let: empty value binding list"

  | { aexp_desc = Aexp_let (rec_flag, bindings, body); aexp_type; _ } ->
        fprintf ch "%a%a in\n%a%a%a"
          (type_prologue d) aexp_type
          (value_bindings rec_flag d) bindings
          indent d
          (expression d) body
          (type_epilogue d) aexp_type

  | { aexp_desc = Aexp_lambda l; aexp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) aexp_type
         (lambda_expression d) l
         (type_epilogue d) aexp_type

and fix_deref d ch x =
  match x.aexp_desc with
    Aexp_path (Avar_simple (id, _, _)) when vsym id = "(!)" ->
      fprintf ch "!"
  | _ -> expression d ch x; fprintf ch " "


and path d ch = function

    Avar_simple (id, _, _) ->
      fprintf ch "%s" (vsym id)

  | Avar_field (p, id, _) ->
      fprintf ch "%a.%s"
        (path d) p
        (Tyfield.name id)

  | Avar_subscript (p, expr, _) ->
      fprintf ch "%a.(%a)"
        (path d) p
        (expression d) expr




and value_bindings rec_flag d ch bindings =
  match rec_flag with
    Recursive -> iter_bindings "let rec" d ch bindings
  | Nonrecursive -> iter_bindings "let" d ch bindings

and iter_bindings prefix d ch = function
    [a] -> value_binding prefix d ch a
  | a :: r -> value_binding prefix d ch a; newline ch d;
              iter_bindings "and" d ch r
  | [] -> internal_error "empty value binding list"

and value_binding prefix d ch { avb_desc; avb_expr } =
    let d' = d + 1 + String.length prefix + valextra avb_desc in
      fprintf ch "%s %a = %a"
        prefix
        (value_description d) avb_desc
        (expression d') avb_expr

and value_description d ch { avd_id=id; avd_constraint=c; _ } =
    fprintf ch "%s%a" (vsym id) (type_expression_opt d) c


and lambda_expression d ch { afun_params; afun_body; _ } =
    let d' = d+2 in
      fprintf ch "fun (%a) -> \n%a%a"
        (value_description_list d) afun_params
        indent d'
        (expression d') afun_body

and expression_list_comma d ch lst = list_single_comma d ch expression lst
and expression_list_semi d ch lst = list_multi_semi d ch expression lst
and value_description_list d ch lst = list_single_comma d ch value_description lst

and field_assign d ch (id, expr) =
    fprintf ch "%s = %a" (Tyfield.name id) (expression d) expr

and field_assignments d ch lst = list_multi_semi d ch field_assign lst

let indent = module_expr

end


(*** Convert Typedtree back to re-indented source ***)
module TypedtreePrinter (ModuleState : ModuleState.S)
                        (Options : sig val closure : bool val fullname : bool end) =
struct
open Shared
open Types
open Typedtree
open ModuleState

module Unify = Unify.Make(ModuleState)

let closure = Options.closure
let fullname = Options.fullname

let indent ch d = fprintf ch "%*s" d ""
let newline ch d = fprintf ch "\n%a" indent d

let add_space s = match s with "" -> "" | _ -> s ^ " "
let quote s = "\"" ^ Util.implode (xquote (Util.explode s)) ^ "\""
let extra s = String.length s
let vsym id =
  match (ValueTable.really_find id).value_kind with
    External -> ValueID.name id
  | _ -> ValueID.short_repr id
let id_extra id = String.length (vsym id)
let pathextra = function
    { tpath_desc = (Tvar_simple (id, _)); _ } -> id_extra id
  | _ -> 0
let exprextra = function
    { texp_desc = Texp_path path; _  } -> pathextra path
  | _ -> 0


let rec list_single_comma (d : int) (ch : out_channel) f = function
    [a] -> f d ch a
  | a :: r -> f d ch a; fprintf ch  ", "; list_single_comma d ch f r
  | [] -> ()

let rec list_multi_semi d ch f = function
    [a]    -> f d ch a
  | a :: r -> f d ch a; fprintf ch ";\n%a" indent d; list_multi_semi d ch f r
  | []     -> ()


let rec module_expr tree =
   let (_, Tmod_structure decls) = tree in
   structure_item_list 0 stdout decls;
   fprintf stdout "\n\n"

and structure_item_list d ch = function
    [a] -> structure_item d ch a
  | (Tstr_type [_] as a) :: ((Tstr_type [_]) :: _ as r)
  | (Tstr_primitive _ as a) :: ((Tstr_primitive _) :: _ as r) ->
      structure_item d ch a; newline ch d;
      structure_item_list d ch r
  | a :: r -> structure_item d ch a; newline ch d; newline ch d;
              structure_item_list d ch r
  | []     -> ()

and structure_item d ch = function

  | Tstr_type [] -> ()

  | Tstr_type decs ->
      let d = 5 + id_extra (List.hd decs).ttyp_id + 3 in
      fprintf ch "%a" (typedecs "type" d) decs

  | Tstr_primitive { tvb_desc = { tvd_id; _ }; tvb_expr = x } ->
      let tfun = TyconTable.really_find tvd_id in
        fprintf ch "external %s : %s = %a"
          (vsym tvd_id)
          (Unify.repr ~closure ~fullname (Tapp (tfun, [])))
          (expression d) x

  | Tstr_value (rec_flag, bindings) ->
      fprintf ch "%a\n" (value_bindings rec_flag d) bindings

  | Tstr_function [] -> ()

  | Tstr_function funcs ->
      fprintf ch "%a" (function_definitions "let rec" d) funcs

and typedecs prefix d ch = function
    [a] -> type_declaration prefix d ch a
  | a :: r -> type_declaration prefix d ch a; newline ch d;
              typedecs "and" d ch r
  | [] -> internal_error "empty value binding list"

and type_declaration prefix _ ch { ttyp_id; _ } =
      let tfun = TyconTable.really_find ttyp_id in
      let params, body = Unify.tfun_repr ~closure ~fullname tfun in
        fprintf ch "%s %s%s = %s"
          prefix (add_space params) (Tycon.name ttyp_id) body

and valextra v = id_extra v.tvd_id + 3 + (extra (Unify.short_repr ~closure ~fullname v.tvd_type))

and type_prologue _ ch _ = fprintf ch "("

and type_epilogue _ ch t = fprintf ch " : %s)" (Unify.short_repr ~closure ~fullname t)


and expression d ch = function

    { texp_desc = Texp_undef; texp_type; _ } ->
       fprintf ch "%aundef%a"
         (type_prologue d) texp_type
         (type_epilogue d) texp_type

  | { texp_desc = Texp_nil; texp_type; _ } ->
       fprintf ch "%anil%a"
         (type_prologue d) texp_type
         (type_epilogue d) texp_type

  | { texp_desc = Texp_any; texp_type; _ } ->
       fprintf ch "%a_%a"
         (type_prologue d) texp_type
         (type_epilogue d) texp_type

  | { texp_desc = Texp_bool b; texp_type; _ } ->
       fprintf ch "%a%b%a"
         (type_prologue d) texp_type
         b 
         (type_epilogue d) texp_type

  | { texp_desc = Texp_int s; texp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) texp_type
         (Int64.to_string s)
         (type_epilogue d) texp_type

  | { texp_desc = Texp_float s; texp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) texp_type
         (string_of_float s)
         (type_epilogue d) texp_type

  | { texp_desc = Texp_uminus expr; texp_type; _ } ->
       fprintf ch "%a-%a%a"
         (type_prologue d) texp_type
         (expression d) expr
         (type_epilogue d) texp_type

  | { texp_desc = Texp_string s; texp_type; _ } ->
       fprintf ch "%a%s%a"
         (type_prologue d) texp_type
         (quote s)
         (type_epilogue d) texp_type

  | { texp_desc = Texp_path p; texp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) texp_type
         (path d) p
         (type_epilogue d) texp_type

  | { texp_desc = Texp_assign (p, op, expr); texp_type; _ } ->
        fprintf ch "%a%a %s %a%a"
          (type_prologue d) texp_type
          (path d) p
          (assign_op_repr op)
          (expression d) expr
          (type_epilogue d) texp_type

  | { texp_desc = Texp_op (left, operand, right); texp_type; _ } ->
       fprintf ch "%a%a %s %a%a"
         (type_prologue d) texp_type
         (expression d) left
         (op_repr operand)
         (expression d) right
         (type_epilogue d) texp_type

  | { texp_desc = Texp_call (expr, arg); texp_type; _ } ->
       let d' = d + exprextra expr + 1 in
         fprintf ch "%a%a(%a)%a"
           (type_prologue d) texp_type
           (fix_deref d) expr
           (expression d') arg
           (type_epilogue d) texp_type

  | { texp_desc = Texp_ccall (p, arg); texp_type; _ } ->
       let d' = d + pathextra p + 1 + extra "apply" in
         fprintf ch "%a%a#apply %a%a"
           (type_prologue d) texp_type
           (path d) p
           (expression d') arg
           (type_epilogue d) texp_type

  | { texp_desc = Texp_record assignment_list; texp_type; _ } ->
       fprintf ch "%a{ %a }%a"
         (type_prologue d) texp_type
         (field_assignments (d+2)) assignment_list
         (type_epilogue d) texp_type

  | { texp_desc = Texp_array (size, init); texp_type; _ } ->
       fprintf ch "%a[%a] ** (%a)%a"
         (type_prologue d) texp_type
         (expression d) init
         (expression d) size
         (type_epilogue d) texp_type

  | { texp_desc = Texp_ifthen (test, expr); texp_type; _ } ->
       let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a%a"
           (type_prologue d) texp_type
           (expression d) test
           indent d
           (expression d') expr
           (type_epilogue d) texp_type

  | { texp_desc = Texp_ifthenelse (test, then_expr, else_expr); texp_type; _ } ->
      let d' = d + extra "then " in
         fprintf ch "%aif %a\n%athen %a\n%aelse %a%a"
           (type_prologue d) texp_type
           (expression d) test
           indent d
           (expression d') then_expr
           indent d
           (expression d') else_expr
           (type_epilogue d) texp_type

  | { texp_desc = Texp_while (test, body); texp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%awhile (%a) do\n%a%a\n%adone%a"
           (type_prologue d) texp_type
           (expression d) test
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) texp_type

  | { texp_desc = Texp_for ([loop_var], hi, body); texp_type; _ } ->
       let d' = d + 2 in
         fprintf ch "%afor %s = %a to %a do\n%a%a\n%adone%a"
           (type_prologue d) texp_type
           (vsym loop_var.tvb_desc.tvd_id)
           (expression d) loop_var.tvb_expr
           (expression d) hi
           indent d'
           (expression d') body
           indent d
           (type_epilogue d) texp_type

  | { texp_desc = Texp_for (_, _, _); _ } ->
      internal_error "invalid for expression"

  | { texp_desc = Texp_tuple lst; texp_type; _ } ->
       fprintf ch "%a(%a)%a"
         (type_prologue d) texp_type
         (expression_list_comma d) lst
         (type_epilogue d) texp_type

  | { texp_desc = Texp_sequence []; texp_type; _ } ->
       fprintf ch "%a()%a"
         (type_prologue d) texp_type
         (type_epilogue d) texp_type

  | { texp_desc = Texp_sequence lst; texp_type; _ } ->
       let d, o, c = match lst with
                       _::_::_ -> d+1, "(", ")"
                     | _ -> d, "", "" in
         fprintf ch "%s%a%a%a%s" o
           (type_prologue d) texp_type
           (expression_list_semi d) lst
           (type_epilogue d) texp_type c

  | { texp_desc = Texp_let (_, [], _); _ } ->
      internal_error "let: empty value binding list"

  | { texp_desc = Texp_let (rec_flag, bindings, body); texp_type; _ } ->
        fprintf ch "%a%a in\n%a%a%a"
          (type_prologue d) texp_type
          (value_bindings rec_flag d) bindings
          indent d
          (expression d) body
          (type_epilogue d) texp_type

  | { texp_desc = Texp_lambda l; texp_type; _ } ->
       fprintf ch "%a%a%a"
         (type_prologue d) texp_type
         (lambda_expression d) l
         (type_epilogue d) texp_type

  | { texp_desc = Texp_closure (env, code); texp_type; _ } ->
       fprintf ch "%a\nobject%a\nend%a"
         (type_prologue d) texp_type
         (closure_fields d) (env, code)
         (type_epilogue d) texp_type

and fix_deref d ch x =
  match x.texp_desc with
    Texp_path { tpath_desc = (Tvar_simple (id, _)); _ } when vsym id = "(!)" ->
      fprintf ch "!"
  | _ -> expression d ch x; fprintf ch " "


and path d ch p = match p.tpath_desc with

    Tvar_simple (id, _) ->
      fprintf ch "%s" (vsym id)

  | Tvar_field (p, id) ->
      fprintf ch "%a.%s"
        (path d) p
        (Tyfield.name id)

  | Tvar_subscript (p, expr) ->
      fprintf ch "%a.(%a)"
        (path d) p
        (expression d) expr


and value_bindings rec_flag d ch bindings =
  match rec_flag with
    Recursive -> iter_bindings "let rec" d ch bindings
  | Nonrecursive -> iter_bindings "let" d ch bindings

and iter_bindings prefix d ch = function
    [a] -> value_binding prefix d ch a
  | a :: r -> value_binding prefix d ch a; newline ch d;
              iter_bindings "and" d ch r
  | [] -> internal_error "empty value binding list"

and value_binding prefix d ch { tvb_desc; tvb_expr } =
    let d' = d + 1 + String.length prefix + valextra tvb_desc in
      fprintf ch "%s %a = %a"
        prefix
        (value_description d) tvb_desc
        (expression d') tvb_expr

and value_description _ ch { tvd_id=id; tvd_type=t; _ } =
    fprintf ch "%s : %s" (vsym id) (Unify.short_repr ~closure ~fullname t)

and xvalue_description _ ch { tvd_id=id; tvd_type=t; _ } =
    fprintf ch "(%s : %s)" (vsym id) (Unify.short_repr ~closure ~fullname t)

and lambda_expression d ch { tfun_body; tfun_params; _ } =
    let d' = d+2 in
      fprintf ch "fun (%a) -> \n%a%a"
        (xvalue_description_list d) tfun_params
        indent d'
        (expression d') tfun_body

and closure_fields d ch (env, code) =
    fprintf ch "\n val env = %a"
      (expression d) env;
    fprintf ch "\n method apply v = %s (env, v)" code

and function_definition prefix d ch f =
    let d' = d + 3 + 2 + extra f.tfunction_label in
      fprintf ch "%s %s (%a) = \n%a%a"
        prefix
        f.tfunction_label
        (xvalue_description_list d) f.tfunction_params
        indent d'
        (expression d') f.tfunction_body

and function_definitions prefix d ch = function
    [a] -> function_definition prefix d ch a
  | a :: r -> function_definition prefix d ch a; newline ch d;
              function_definitions "and" d ch r
  | [] -> internal_error "empty function definition list"


and expression_list_comma d ch lst = list_single_comma d ch expression lst
and expression_list_semi d ch lst = list_multi_semi d ch expression lst
and xvalue_description_list d ch lst = list_single_comma d ch xvalue_description lst

and field_assign d ch (id, expr) =
    fprintf ch "%s = %a" (Tyfield.name id) (expression d) expr

and field_assignments d ch lst = list_multi_semi d ch field_assign lst

let indent = module_expr

end



