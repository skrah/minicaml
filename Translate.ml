(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf
open Shared
open Typedtree


module L = Llvm

let ign f x = ignore (f x)
let map_array f lst = Array.of_list (List.map f lst)


module Make(ModuleState : ModuleState.S) =
struct

open ModuleState

module Unify = Unify.Make(ModuleState)


(* Context *)
let context = Llvm.global_context ()

(* Builder *)
let builder = Llvm.builder context

(* Module initialization *)
let the_module = Llvm.create_module context "tiger_main"
let _ = Llvm.set_target_triple "x86_64-pc-linux-gnu" the_module
let _ = Llvm.set_data_layout "e-m:e-i64:64-f80:128-n8:16:32:64-S128" the_module

(* Module typedefs *)
let module_types = Hashtbl.create 16


(* Module constants and types *)
let i1_t = Llvm.i1_type context
let i8_t = Llvm.i8_type context
let i64_t = Llvm.i64_type context
let double_t = Llvm.double_type context

let i8_ptr_t = Llvm.pointer_type i8_t

let i64_zero = Llvm.const_null i64_t
let i64_one = Llvm.const_int i64_t 1
let i64_undef = Llvm.undef i64_t

let caml_value = Llvm.named_struct_type context "caml_value"
let () = Hashtbl.add module_types "caml_value" caml_value
let () = Llvm.struct_set_body caml_value [|i64_t; i64_t|] false
let caml_value_ptr = Llvm.pointer_type caml_value
let caml_value_size = Llvm.size_of caml_value

let const_undef = Llvm.const_named_struct caml_value [|i64_undef; i64_undef|]
let const_nil = Llvm.const_named_struct caml_value [|i64_undef; i64_zero|]
let const_unit = Llvm.const_named_struct caml_value [|i64_undef; i64_zero|]
let const_true = Llvm.const_named_struct caml_value [|i64_undef; i64_one|]
let const_false = Llvm.const_named_struct caml_value [|i64_undef; i64_zero|]
let const_one = Llvm.const_named_struct caml_value [|i64_undef; i64_one|]

let pack_i64 x =
  let agg = Llvm.const_named_struct caml_value [|i64_undef; i64_undef|] in
    Llvm.build_insertvalue agg x 1 "" builder

let unpack_i64 agg = Llvm.build_extractvalue agg 1 "" builder

let pack_bool b =
  let x = Llvm.build_zext b i64_t "packbool" builder in
    pack_i64 x

let unpack_bool agg =
  let x = unpack_i64 agg in
    Llvm.build_trunc x i1_t "unpackbool" builder

let pack_float f =
  let x = Llvm.build_bitcast f i64_t "packfloat" builder in
    pack_i64 x

let unpack_float agg =
  let x = unpack_i64 agg in
    Llvm.build_bitcast x double_t "unpackfloat" builder

let pack_string s =
  let x = Llvm.build_ptrtoint s i64_t "packstr" builder in
    pack_i64 x

let pack_array a =
  let x = Llvm.build_ptrtoint a i64_t "packarray" builder in
    pack_i64 x

let unpack_array agg =
  let x = unpack_i64 agg in
    Llvm.build_inttoptr x caml_value_ptr "unpackarray" builder


let pack_closure env code =
  let env_i64 = Llvm.build_ptrtoint env i64_t "packenv" builder in
  let code_i64 = Llvm.build_ptrtoint code i64_t "packcode" builder in
  let agg = Llvm.const_named_struct caml_value [|i64_undef; i64_undef|] in
  let agg' = Llvm.build_insertvalue agg env_i64 0 "" builder in
    Llvm.build_insertvalue agg' code_i64 1 "" builder

let unpack_closure agg =
  let env_i64 = Llvm.build_extractvalue agg 0 "extractenv" builder in
  let code_i64 = Llvm.build_extractvalue agg 1 "extractcode" builder in
  let env = Llvm.build_inttoptr env_i64 i8_ptr_t "unpackenv" builder in
  let code = Llvm.build_inttoptr code_i64 i8_ptr_t "unpackcode" builder in
    (env, code)


(* Runtime functions *)
let builtins = [
  "MiniCaml_alloc", i8_ptr_t, [|i64_t; i64_t|];
  "MiniCaml_init_array", caml_value, [|caml_value; caml_value|];
  "abort", Llvm.void_type context, [||]]

let lookup_function name =
  match L.lookup_function name the_module with
    None -> internal_error (sprintf "undeclared function: %s" name)
  | Some f -> f

let declare_function_once (label, result, formals) =
  let ft = L.function_type result formals in
    match L.lookup_function label the_module with
      None -> L.declare_function label ft the_module
    | Some _ -> internal_error "function label collision"

let declare_function_once2 label ft =
  match L.lookup_function label the_module with
    None -> L.declare_function label ft the_module
  | Some _ -> internal_error "function label collision"

let set_argument_names (the_function, params) =
  let set i v =
    let arg = (L.param the_function i) in
    L.set_value_name (ValueID.name v.tvd_id) arg
  in List.iteri set params

let _ = List.iter (ign declare_function_once) builtins

let rec tr_arrow t =
  let module T = Types in
  match t with
    T.Tarrow (f, _) ->
     let formals = match f with
                     T.Ttuple ts -> map_array (fun _ -> caml_value) ts
                   | _ -> [|caml_value|] in
     let formals' = Array.append [|caml_value|] formals in
       L.function_type caml_value formals'
  | T.Tpoly (_, t) -> tr_arrow t
  | _ -> let s = sprintf "tr_arrow: unexpected type: %s" (Unify.short_repr t) in
           internal_error s

let codegen_proto f =
  let label = f.tfunction_label in
  let formals_t = map_array (fun _ -> caml_value) f.tfunction_params in
  let result_t = caml_value in
  let ft = L.function_type result_t formals_t in
  let the_function = declare_function_once2 label ft in
    set_argument_names (the_function, f.tfunction_params)

let indexof lst f =
  let rec loop i = function
      [] -> raise Not_found
    | x :: _ when f x -> i
    | _ :: xs -> loop (i+1) xs
  in loop 0 lst

let rec fieldno t field =
  let module T = Types in
  match Unify.expand t with
    T.Trecord r ->
      indexof r (fun (n, _) -> n = field)
  | T.Tpoly (_, t') -> fieldno t' field
  | _ -> internal_error "fieldno expects a record"

let get_access id =
  let v = ValueTable.really_find id in
    match v.value_access with
      None -> internal_error "missing access"
    | Some a -> a

let set_access v access =
  let x = ValueTable.really_find v.tvd_id in
    match x.value_access with
      None -> x.value_access <- Some access
    | Some _ -> internal_error "setting access twice"

let alloc_local frame v =
  let open Llvm in
  let builder = builder_at context (instr_begin (entry_block frame)) in
  let alloca = build_alloca caml_value (ValueID.name v.tvd_id) builder in
    set_access v alloca; alloca

let alloc_global v init =
  let open Llvm in
  let name = sprintf "MiniCaml_%s" (ValueID.name v.tvd_id) in
    match lookup_global name the_module with
      None -> let alloca = define_global name init the_module in
                set_linkage Llvm.Linkage.Private alloca;
                set_access v alloca;
                alloca
    | _ -> internal_error "defining globale twice"

let alloc_block n =
  let f = lookup_function "MiniCaml_alloc" in
  let nmemb = Llvm.const_int i64_t n in
  let ptr = Llvm.build_call f [|nmemb; caml_value_size|] "" builder in
    Llvm.build_pointercast ptr caml_value_ptr "" builder

let tr_boolop left op right =
  let l = unpack_bool left in
  let r = unpack_bool right in
  let ret = match op with
              Op_and -> Llvm.build_and l r "and" builder
            | Op_or -> Llvm.build_or l r "or" builder
            | _ -> internal_error "tr_op: expected boolean operator"
  in pack_bool ret

let tr_intop left op right =
  let l = unpack_i64 left in
  let r = unpack_i64 right in
  let ret = match op with
              Op_plus -> Llvm.build_add l r "add" builder
            | Op_minus -> Llvm.build_sub l r "sub" builder
            | Op_times -> Llvm.build_mul l r "mul" builder
            | Op_divide -> Llvm.build_sdiv l r "div" builder
            | _ -> internal_error "tr_arithop: expected int operator"
  in pack_i64 ret

let tr_floatop left op right =
  let l = unpack_float left in
  let r = unpack_float right in
  let ret = match op with
              Op_plusdot -> Llvm.build_fadd l r "fadd" builder
            | Op_minusdot -> Llvm.build_fsub l r "fsub" builder
            | Op_timesdot -> Llvm.build_fmul l r "fmul" builder
            | Op_dividedot -> Llvm.build_fdiv l r "fdiv" builder
            | _ -> internal_error "tr_arithop: expected float operator"
  in pack_float ret

let tr_icmp = function
  Op_eq | Op_eqeq -> Llvm.Icmp.Eq
| Op_ne -> Llvm.Icmp.Ne
| Op_lt -> Llvm.Icmp.Slt
| Op_gt -> Llvm.Icmp.Sgt
| Op_le -> Llvm.Icmp.Sle
| Op_ge -> Llvm.Icmp.Sge
| _ -> internal_error "tr_icmp: expected comparison operator"

let tr_fcmp = function
  Op_eq | Op_eqeq -> Llvm.Fcmp.Oeq
| Op_ne -> Llvm.Fcmp.One
| Op_lt -> Llvm.Fcmp.Olt
| Op_gt -> Llvm.Fcmp.Ogt
| Op_le -> Llvm.Fcmp.Ole
| Op_ge -> Llvm.Fcmp.Oge
| _ -> internal_error "tr_fcmp: expected comparison operator"


let rec codegen_expr x =

  let module T = Types in
    match x.texp_desc with

      Texp_undef -> const_undef

    | Texp_any -> const_undef

    | Texp_nil -> const_nil

    | Texp_bool x -> if x then const_true else const_false

    | Texp_int x -> let i64 = Llvm.const_of_int64 i64_t x true in pack_i64 i64

    | Texp_float x -> let f = Llvm.const_float double_t x in pack_float f

    | Texp_uminus expr ->
       let expr' = codegen_expr expr in
       let x = unpack_i64 expr' in
       let y = Llvm.build_sub i64_zero x "" builder in
         pack_i64 y

    | Texp_string s ->
       let ptr = Llvm.build_global_stringptr s "str" builder in
         pack_string ptr

    | Texp_path path -> trans_path path (* builds a load *)

    | Texp_assign (path, _, expr) ->
       let access = tr_path path in
       let expr' = codegen_expr expr in
       ignore (Llvm.build_store expr' access builder);
       const_unit

    | Texp_op (left, op, right) ->
       let l = codegen_expr left in
       let r = codegen_expr right in
       begin match left.texp_type with
       (* boolean *)
         t when t = T.bool_t && is_boolop op ->
           tr_boolop l op r

       (* arithmetic *)
       | t when t = T.double_t && is_floatop op ->
           tr_floatop l op r

       | t when t = T.int64_t && is_intop op ->
           tr_intop l op r

       (* comparisons *)
       | t when t = T.int64_t && is_cmpop op ->
           let icmp = tr_icmp op in
           let l' = unpack_i64 l in
           let r' = unpack_i64 r in
           let b = Llvm.build_icmp icmp l' r' "icmp" builder in
             pack_bool b

       | t when t = T.double_t && is_cmpop op ->
           let fcmp = tr_fcmp op in
           let l' = unpack_float l in
           let r' = unpack_float r in
           let b = Llvm.build_fcmp fcmp l' r' "fcmp" builder in
             pack_bool b

       | t when t = T.string_t ->
           let icmp = tr_icmp op in
           let func = lookup_function "MiniCaml_strcmp" in
           let cmp = Llvm.build_call func [|const_undef; l; r|] "strcmp" builder in
           let b = Llvm.build_icmp icmp (unpack_i64 cmp) i64_zero "icmp" builder in
             pack_bool b

       | _ when is_eqop op ->
           let icmp = tr_icmp op in
           let l' = unpack_array l in
           let r' = unpack_array r in
           let b = Llvm.build_icmp icmp l' r' "pcmp" builder in
             pack_bool b

       | _ -> internal_error "unexpected operation"
       end

    | Texp_call _ -> internal_error "unexpected call"

    | Texp_ccall (path, arg) ->
       let ft = tr_arrow path.tpath_type in (* adds env to formals *)
       let code_ptr_t = Llvm.pointer_type ft in
       let closure = trans_path path in (* builds a load *)
       let env, code = unpack_closure closure in
       let env' = pack_array env in
       let code' = L.build_pointercast code code_ptr_t "castcode" builder in
       let args = match arg.texp_desc with
                    Texp_tuple [] -> [arg]
                  | Texp_tuple ts -> ts
                  | _ -> [arg] in
       let args' = map_array codegen_expr args in
       let args'' = Array.append [|env'|] args' in
         Llvm.build_call code' args'' "" builder

    | Texp_tuple [] -> const_unit

    | Texp_tuple _ -> internal_error "tuple outside of call expression"

    | Texp_record fields ->
       let exprs = List.map (fun f -> codegen_expr (snd f)) fields in
       let ptr = alloc_block (List.length exprs) in

       let rec init i = function
             [] -> ()
           | x :: xs ->
             let index = Llvm.const_int i64_t i in
             let loc = Llvm.build_gep ptr [|index|] "" builder in
             ignore (Llvm.build_store x loc builder);
             init (i+1) xs
       in init 0 exprs; pack_array ptr

    | Texp_array (len, init) ->
       let module T = Types in
       let len' = codegen_expr len in
       let init' = codegen_expr init in
       let f = lookup_function "MiniCaml_init_array" in
         Llvm.build_call f [|len'; init'|] "" builder

    | Texp_ifthen (cond, then_exp) ->
       let cond' = codegen_expr cond in
       let cond' = unpack_bool cond' in

       let start_bb = Llvm.insertion_block builder in
       let the_function = Llvm.block_parent start_bb in

       let then_bb = Llvm.append_block context "then" the_function in
       Llvm.position_at_end then_bb builder;
       ignore (codegen_expr then_exp);
       let new_then_bb = Llvm.insertion_block builder in

       let merge_bb = Llvm.append_block context "ifcont" the_function in
       Llvm.position_at_end merge_bb builder;

       Llvm.position_at_end start_bb builder;
       ignore (Llvm.build_cond_br cond' then_bb merge_bb builder);

       Llvm.position_at_end new_then_bb builder;
       ignore (Llvm.build_br merge_bb builder);

       Llvm.position_at_end merge_bb builder;

       const_unit

    | Texp_ifthenelse (cond, then_exp, else_exp) ->
       let cond_val = codegen_expr cond in
       let cond_val = unpack_bool cond_val in

       let start_bb = Llvm.insertion_block builder in
       let the_function = Llvm.block_parent start_bb in

       let then_bb = Llvm.append_block context "then" the_function in
       Llvm.position_at_end then_bb builder;
       let then_val = codegen_expr then_exp in
       let new_then_bb = Llvm.insertion_block builder in

       let else_bb = Llvm.append_block context "else" the_function in
       Llvm.position_at_end else_bb builder;
       let else_val = codegen_expr else_exp in
       let new_else_bb = Llvm.insertion_block builder in

       let merge_bb = Llvm.append_block context "ifcont" the_function in
       Llvm.position_at_end merge_bb builder;
       let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
       let phi = Llvm.build_phi incoming "iftmp" builder in

       Llvm.position_at_end start_bb builder;
       ignore (Llvm.build_cond_br cond_val then_bb else_bb builder);

       Llvm.position_at_end new_then_bb builder;
       ignore (Llvm.build_br merge_bb builder);
       Llvm.position_at_end new_else_bb builder;
       ignore (Llvm.build_br merge_bb builder);

       Llvm.position_at_end merge_bb builder;

       phi

    | Texp_while (cond, body) ->
       let loop_header = Llvm.insertion_block builder in
       let the_function = Llvm.block_parent loop_header in

       let start_bb = Llvm.append_block context "cond" the_function in
       Llvm.position_at_end loop_header builder;
       ignore (Llvm.build_br start_bb builder);

       let then_bb = Llvm.append_block context "while" the_function in
       Llvm.position_at_end then_bb builder;
       ignore (codegen_expr body);
       let new_then_bb = Llvm.insertion_block builder in

       let merge_bb = Llvm.append_block context "whilecont" the_function in
       Llvm.position_at_end merge_bb builder;

       Llvm.position_at_end start_bb builder;
       let cond' = codegen_expr cond in
       let cond' = unpack_bool cond' in
       ignore (Llvm.build_cond_br cond' then_bb merge_bb builder);

       Llvm.position_at_end new_then_bb builder;
       ignore (Llvm.build_br start_bb builder);

       Llvm.position_at_end merge_bb builder;

       const_unit

    | Texp_for (bindings, hi, body) ->
       let { tvb_desc=loop_var; tvb_expr=init } =
         match bindings with
           [vb] -> vb
         | _ -> internal_error "more than a single binding in for expression"
       in

       let loop_header = Llvm.insertion_block builder in
       let the_function = Llvm.block_parent loop_header in

       let alloca = alloc_local the_function loop_var in
       let start_val = codegen_expr init in
       ignore (Llvm.build_store start_val alloca builder);
       let hi_val = codegen_expr hi in

       let start_bb = Llvm.append_block context "cond" the_function in
       Llvm.position_at_end loop_header builder;
       ignore (Llvm.build_br start_bb builder);

       let then_bb = Llvm.append_block context "for" the_function in
       Llvm.position_at_end then_bb builder;
       ignore (codegen_expr body);
       let cur = Llvm.build_load alloca "" builder in
       let next = tr_intop cur Op_plus const_one in
       ignore (Llvm.build_store next alloca builder);
       let new_then_bb = Llvm.insertion_block builder in

       let merge_bb = Llvm.append_block context "forcont" the_function in
       Llvm.position_at_end merge_bb builder;

       Llvm.position_at_end start_bb builder;
       let cur = Llvm.build_load alloca "" builder in
       let cond = Llvm.build_icmp Llvm.Icmp.Sle (unpack_i64 cur) (unpack_i64 hi_val) "" builder in
       ignore (Llvm.build_cond_br cond then_bb merge_bb builder);

       Llvm.position_at_end new_then_bb builder;
       ignore (Llvm.build_br start_bb builder);

       Llvm.position_at_end merge_bb builder;

       const_unit

    | Texp_sequence lst ->
        Util.last (List.map codegen_expr lst)

    | Texp_let (rec_flag, bindings, body) ->
        ignore (tr_bindings rec_flag bindings);
        codegen_expr body

    | Texp_closure (env, code) ->
        let env' = codegen_expr env in
        let env'' = unpack_array env' in
        let code' = lookup_function code in
          pack_closure env'' code'

    | Texp_lambda _ -> internal_error "unexpected lambda"

  and tr_path x = match x.tpath_desc with

      Tvar_simple (id, _) -> get_access id

    | Tvar_field (p, id) ->
       let base = tr_path p in
       let value = Llvm.build_load base "recordpath" builder in
       let ptr = unpack_array value in
       let i = fieldno p.tpath_type id in
       let index = Llvm.const_int i64_t i in
         Llvm.build_gep ptr [|index|] "recordfield" builder

    | Tvar_subscript (path, expr) ->
       let base = tr_path path in
       let value = Llvm.build_load base "arraypath" builder in
       let ptr = unpack_array value in
       let agg = codegen_expr expr in
       let index = unpack_i64 agg in
         Llvm.build_gep ptr [|index|] "arraymember" builder

  and trans_path v =
    let ptr = tr_path v in
      Llvm.build_load ptr "path" builder

  and structure_item = function
 
      Tstr_type _ -> ()

    | Tstr_primitive { tvb_desc; tvb_expr } ->
        let label = match tvb_expr.texp_desc with
                      Texp_string s -> s
                    | _ -> internal_error "expected label" in
        let ft = tr_arrow tvb_desc.tvd_type in
        let code = declare_function_once2 label ft in
        let closure = pack_closure (Llvm.undef i64_t) code in
        ignore (alloc_global tvb_desc closure)

    | Tstr_value (rec_flag, bindings) ->
        tr_bindings rec_flag bindings

    | Tstr_function funcs ->
        ignore (List.iter codegen_proto funcs);
        List.iter (fun f ->
          let cur_bb = Llvm.insertion_block builder in
          ignore (codegen_frame f);
          Llvm.position_at_end cur_bb builder;
        ) funcs

  and tr_bindings _ bindings =
      let cont_bb = Llvm.insertion_block builder in
      let frame = Llvm.block_parent cont_bb in
      List.iter (fun v ->
        ignore (alloc_local frame v.tvb_desc);
      ) bindings;
      Llvm.position_at_end cont_bb builder;
      List.iter (fun v ->
        let expr = codegen_expr v.tvb_expr in
        let access = get_access v.tvb_desc.tvd_id in
        ignore (Llvm.build_store expr access builder)
      ) bindings


and gen_view_shift frame params =
  let llvm_params = Array.to_list (Llvm.params frame) in
  List.iter2 (fun v arg ->
    let alloca = alloc_local frame v in
    ignore (Llvm.build_store arg alloca builder)
  ) params llvm_params

and codegen_frame f =
  let label = f.tfunction_label in
  let the_function = lookup_function label in

  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;

  gen_view_shift the_function f.tfunction_params;

  let ret = codegen_expr f.tfunction_body in

  begin match f.tfunction_body.texp_type with
    Types.Tapp (Types.Tconstr id, []) when id = Types.prim_unit_id ->
      ignore (Llvm.build_ret const_unit builder); the_function
  | _ -> ignore (Llvm.build_ret ret builder); the_function
  end

let trans_module decls = List.map structure_item decls

let codegen_main_proto () =
    let label = "MiniCaml_main" in
    let the_function = declare_function_once (label, Llvm.void_type context, [||]) in
      the_function

let codegen_main_frame decls =
   let label = "MiniCaml_main" in
   let the_function = lookup_function label in

   let bb = Llvm.append_block context "entry" the_function in
   Llvm.position_at_end bb builder;

   ignore (trans_module decls);
   Llvm.build_ret_void builder


let trans_main (Tmod_structure includes, Tmod_structure decls) =
  ignore (codegen_main_proto ());
  ignore (codegen_main_frame (includes @ decls));
  Llvm_analysis.assert_valid_module the_module;
  Llvm.dump_module the_module;


end




