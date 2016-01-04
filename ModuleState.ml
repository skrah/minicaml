(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

open Printf
open Shared
open ParseTree
open Types


module type S =
sig
  val filename : string
  module ScopeID : Symbol.S with type t = Symbol.ScopeID.t
  module Tycon : Symbol.S with type t = Symbol.Tycon.t
  module Tyvar : Symbol.S with type t = Symbol.Tyvar.t
  module Tyfield : Symbol.S with type t = Symbol.Tyfield.t
  module Tymeta : Symbol.S with type t = Symbol.Tymeta.t
  module ValueID : Symbol.S with type t = Symbol.ValueID.t
  type tycon_env = Symbol.Tycon.t Env.t
  type tyvar_env = Symbol.Tyvar.t Env.t
  type value_env = Symbol.ValueID.t Env.t
  type scope = {
    scope_id : Symbol.ScopeID.t;
    scope_prev : Symbol.ScopeID.t;
    mutable scope_locals : Symbol.ValueID.t list;
    mutable scope_escapes : Symbol.ValueID.t list;
  }
  type value = {
    value_id : Symbol.ValueID.t;
    value_scope : Symbol.ScopeID.t;
    value_level : int;
    mutable value_type : Types.t option;
    mutable value_kind : Shared.value_kind;
    mutable value_escape : bool;
    mutable value_assigned_to : bool;
    mutable value_access : Llvm.llvalue option
  }
  type context = {
    scope : Symbol.ScopeID.t;
    level : int;
    tycon_env : tycon_env;
    tyvar_env : tyvar_env;
    value_env : value_env;
  }
  module ScopeTable :
    sig
      val add : context -> string -> context
      val add_main : string -> Symbol.ScopeID.t
      val find : Symbol.ScopeID.t -> scope
    end
  module TyconTable :
    sig
      val add_env : context -> string -> context * Symbol.Tycon.t
      val add_tbl : context -> string -> Types.tfun -> Symbol.Tycon.t
      val unsafe_add : context -> Symbol.Tycon.t * Types.tfun -> context
      val unsafe_add2 : Symbol.Tycon.t -> Types.tfun -> unit
      val find_env : context -> string -> Location.t -> Symbol.Tycon.t
      val find_tbl : Symbol.Tycon.t -> Location.t -> Types.tfun
      val really_find : Symbol.Tycon.t -> Types.tfun
      val get_tfun_params : Types.tfun -> Symbol.Tyvar.t list
      val destruct : Symbol.Tycon.t -> Symbol.Tyvar.t list * Types.t
      val get_record_fields : Symbol.Tycon.t -> Symbol.Tyfield.t list
      val get_record_fields_with_types : Symbol.Tycon.t -> (Symbol.Tyfield.t * Types.t) list
    end
  module TyvarTable :
    sig
      val add : context -> ParseTree.core_type -> context * Symbol.Tyvar.t
      val add_str : context -> string -> context * Symbol.Tyvar.t
      val find_env : context -> string -> Location.t -> Symbol.Tyvar.t
    end
  module TymetaTable :
    sig
      val find : context -> string -> Types.t
      val clear : unit -> unit
    end
  module FieldTable :
    sig
      val add_id : ParseTree.field -> Symbol.Tyfield.t
      val add_id_str : string -> Symbol.Tyfield.t
      val add_type : Symbol.Tyfield.t -> Types.t -> unit
      val find_id : string -> Location.t -> Symbol.Tyfield.t
      val typeof : Symbol.Tyfield.t -> Types.t
      val flagof : Symbol.Tyfield.t -> Shared.mutable_flag
      val record_abbr : (Symbol.Tyfield.t * Types.t) list -> Types.t
      val validate : (Symbol.Tyfield.t * 'a) list -> Location.t -> Symbol.Tycon.t
    end
  module ValueTable :
    sig
      val add :
        context -> ParseTree.value_description -> context * Symbol.ValueID.t
      val add_name : Symbol.ScopeID.t -> Shared.value_kind -> string -> Symbol.ValueID.t
      val find_and_update : bool -> context -> string -> Location.t -> Symbol.ValueID.t
      val find : Symbol.ValueID.t -> Location.t -> value
      val really_find : Symbol.ValueID.t -> value
      val add_type : Symbol.ValueID.t -> Types.t -> unit
      val typeof : Symbol.ValueID.t -> Types.t
      val is_assigned_to : Symbol.ValueID.t -> bool
      val fold : (Symbol.ValueID.t -> value -> unit -> unit) -> unit
    end
  val mk_main_context : string -> context
end


module type ARGS =
sig
  val filename : string
end


(** Implementation of the module state *)
module Make (A: ARGS)  =
struct
  let filename = A.filename
  include SymbolGenerator.Make (struct end)

  type tycon_env = Symbol.Tycon.t Env.t
  type tyvar_env = Symbol.Tyvar.t Env.t
  type value_env = Symbol.ValueID.t Env.t

  type scope =
    { scope_id : ScopeID.t;
      scope_prev : ScopeID.t;
      mutable scope_locals : ValueID.t list;
      mutable scope_escapes : ValueID.t list }

  type value =
    { value_id : ValueID.t;
      value_scope : ScopeID.t;
      value_level : int;
      mutable value_type : Types.t option;
      mutable value_kind : value_kind;
      mutable value_escape : bool;
      mutable value_assigned_to : bool;
      mutable value_access : Llvm.llvalue option }

  type context =
    { scope : ScopeID.t;
      level : int;
      tycon_env : tycon_env;
      tyvar_env : tyvar_env;
      value_env : value_env }

  module ScopeTable =
    struct
      module Tbl = Hashtbl.Make(ScopeID)
      let tbl : scope Tbl.t = Tbl.create 128

      let add ctx name =
        let id = ScopeID.make name in
        let next = { scope_id = id;
                     scope_prev = ctx.scope;
                     scope_locals = [];
                     scope_escapes = [] }
        in
        let ctx' = { ctx with scope = id } in
          Tbl.add tbl id next; ctx'

      let add_main name =
        let extern = ScopeID.make "external" in
        let id = ScopeID.make name in
        if ScopeID.id id <> 0 then internal_error "invalid main scope id";
        let main = { scope_id = id;
                     scope_prev = extern;
                     scope_locals = [];
                     scope_escapes = [] } in
          Tbl.add tbl id main; id

      let find id =
        if ScopeID.id id < 0 then internal_error "scope id < 0"
        else Tbl.find tbl id
    end

  module TyconTable =
    struct
      module Tbl = Hashtbl.Make(Tycon)
      let tbl : Types.tfun Tbl.t = Tbl.create 128

      let add_env ctx name =
        let id = Tycon.make name in
        let env' = Env.add name id ctx.tycon_env in
        let ctx' = { ctx with tycon_env = env' } in
          (ctx', id)

      let find_env ctx name pos =
        try Env.find name ctx.tycon_env
        with Not_found ->
          Location.error pos ("env: undeclared type constructor: " ^ name)

      let add_tbl ctx name tfun =
        let id = Env.find name ctx.tycon_env in
          Tbl.add tbl id tfun; id

      let unsafe_add ctx (id, tfun) =
        let env' = Env.add (Tycon.name id) id ctx.tycon_env in
        let ctx' = { ctx with tycon_env = env' } in
          Tbl.add tbl id tfun; ctx'

      let unsafe_add2 id tfun = Tbl.add tbl id tfun

      let find_tbl id loc =
        try Tbl.find tbl id
        with Not_found ->
          let name = Tycon.name id in
          Location.error loc ("tbl: undeclared type constructor: " ^ name)

      let really_find id =
        try Tbl.find tbl id
        with Not_found ->
          let name = Tycon.name id in
          internal_error ("tbl: undeclared type constructor: " ^ name)

      let get_tfun_params = function
          Tfun (params, _) -> params
        | Tlink id | Tconstr id ->
           begin match Tbl.find tbl id with
             Tfun (params, _) -> params
           | Tlink _ -> internal_error "unresolved tlink"
           | Tconstr _ -> internal_error "unresolved tconstr"
           end

      let destruct id =
        match really_find id with
          Tfun (params, body) -> (params, body)
        | Tlink _ -> internal_error "unexpected tlink"
        | Tconstr _ -> internal_error "unexpected tconstr"

      let get_record_fields id =
        match really_find id with
          Tfun (_, Trecord fields) -> List.map fst fields
        | _ -> internal_error "expected record type function"

      let get_record_fields_with_types id =
        match really_find id with
          Tfun (_, Trecord fields) -> fields
        | _ -> internal_error "expected record type function"

    end

  module TyvarTable =
    struct
      let add ctx t =
        let name = match t.core_type_desc with
            Ptyp_any -> "_"
          | Ptyp_var s -> s
          | _ -> internal_error "invalid type variable" in
        let id = Tyvar.make name in
        let env' = Env.add name id ctx.tyvar_env in
        let ctx' = { ctx with tyvar_env = env' } in
          (ctx', id)

      let add_str ctx name =
        let id = Tyvar.make name in
        let env' = Env.add name id ctx.tyvar_env in
        let ctx' = { ctx with tyvar_env = env' } in
          (ctx', id)

      let find_env ctx name pos =
        try Env.find name ctx.tyvar_env
        with Not_found ->
          Location.error pos ("undeclared type variable: " ^ name)
    end

  module TymetaTable =
    struct
      module Tbl = Hashtbl.Make(Util.String)
      let types : Types.t Tbl.t = Tbl.create 16

      let add ctx name =
        let t = Tmeta (ref (Unbound (Tymeta.make name, ctx.level))) in
        Tbl.add types name t; t

      let find ctx name =
        try let t = Tbl.find types name in
              Tmeta (ref (Link t))
        with Not_found -> add ctx name

      let clear () = Tbl.clear types
    end

  module FieldTable =
    struct
      module StringTbl = Hashtbl.Make(Util.String)
      module FieldTbl = Hashtbl.Make(Tyfield)
      let fields : Tyfield.t StringTbl.t = StringTbl.create 128
      let types : Types.t FieldTbl.t = FieldTbl.create 128

      let add_id f =
        let id = Tyfield.make f.pfield_name in
          try ignore (StringTbl.find fields f.pfield_name);
              let msg = sprintf "duplicate field %s" f.pfield_name in
              Location.error f.pfield_loc msg
          with Not_found ->
            StringTbl.add fields f.pfield_name id; id

      let add_id_str s =
        let id = Tyfield.make s in
          try ignore (StringTbl.find fields s);
              let msg = sprintf "duplicate field %s" s in
              internal_error msg
          with Not_found ->
            StringTbl.add fields s id; id

      let add_type id t = FieldTbl.add types id t

      let find_id name pos =
        try StringTbl.find fields name
        with Not_found ->
          let msg = sprintf "undeclared field %s" name in
          Location.error pos msg

      let typeof id =
        try FieldTbl.find types id
        with Not_found ->
          let msg = sprintf "missing field table entry: %s" (Tyfield.name id) in
            internal_error msg

      let flagof id =
        match typeof id with
            Tpoly (_, Tfield (_, flag, _, _)) -> flag
        | _ -> internal_error "not a field type"

      let record_abbr = function
          [] -> internal_error "empty record"
        | (f, _) :: _ -> begin match typeof f with
                           Tpoly (_, Tfield (_, _, tr, _)) -> tr
                         | _ -> internal_error "not a field type"
                         end

      (* validate record construction *)
      let validate fields loc =
        let extract id =
          match typeof id with
            Tpoly (_, Tfield (fld, _, Tapp (Tlink record, _), _)) ->
              (fld, record)
          | _ -> internal_error "not a field type"
        in

        let rec doit = function
            [] -> internal_error "empty record"
          | [(f, _)] ->
              let (g, r) = extract f in
                if f <> g then
                  error loc "record construction differs from declared type"
                else r
          | (f1, _) :: ((f2, _) :: _ as tl) ->
              let (g1, r1) = extract f1 in
              let (g2, r2) = extract f2 in
                if f1 <> g1 || f2 <> g2 || r1 <> r2 then
                  error loc "record construction differs from declared type"
                else doit tl

        in doit fields

    end

  module ValueTable =
    struct
      module Tbl = Hashtbl.Make(ValueID)
      let tbl : value Tbl.t = Tbl.create 128

      let add ctx v =
        let id = ValueID.make v.pvd_name in
        let scope = ScopeTable.find ctx.scope in
        let desc = { value_id = id;
                     value_scope = scope.scope_id;
                     value_level = 0;
                     value_type = None;
                     value_kind = v.pvd_kind;
                     value_escape = false;
                     value_assigned_to = false;
                     value_access = None } in
        scope.scope_locals <- scope.scope_locals @ [id];
        let env' = Env.add v.pvd_name id ctx.value_env in
        let ctx' = { ctx with value_env = env' } in
          Tbl.add tbl id desc; (ctx', id)

      let add_name scope kind s =
        let id = ValueID.make s in
        let scope = ScopeTable.find scope in
        let desc = { value_id = id;
                     value_scope = scope.scope_id;
                     value_level = 0;
                     value_type = None;
                     value_kind = kind;
                     value_escape = false;
                     value_assigned_to = false;
                     value_access = None } in
        scope.scope_locals <- scope.scope_locals @ [id];
        Tbl.add tbl id desc; id

      let find id pos =
        try Tbl.find tbl id
        with Not_found ->
          let name = ValueID.name id in
          Location.error pos ("undeclared variable: " ^ name)

      let really_find id =
        try Tbl.find tbl id
        with Not_found ->
          let name = ValueID.name id in
          internal_error ("missing value table entry: " ^ name)

      let add_type id t =
        let v = really_find id in
          match v.value_type with
            _ -> v.value_type  <- Some t
      (* XXX | Some t -> internal_error "setting type twice" *)

      let typeof id =
        match (really_find id).value_type with
          None -> internal_error "type not set"
        | Some t -> t

      let is_assigned_to id = let v = really_find id in v.value_assigned_to

      let find_and_update assign ctx name pos =
        let id = try Env.find name ctx.value_env
                 with Not_found ->
                   Location.error pos ("undeclared variable: " ^ name)
        in
        let scope = ScopeTable.find ctx.scope in
        let () = match really_find id with
                   { value_kind = External; _ } -> ()
                 | v -> if assign then
                          v.value_assigned_to <- true;
                        if v.value_scope <> scope.scope_id then
                          let s = ScopeTable.find v.value_scope in
                          v.value_escape <- true;
                          if not (List.mem id s.scope_escapes) then
                            s.scope_escapes <- s.scope_escapes @ [id]
        in id

      let fold f = Tbl.fold f tbl ()

    end

  let mk_main_context name =
    let main_scope = ScopeTable.add_main name in
    let ctx = { scope = main_scope; 
                level = 0;
                tycon_env = Env.empty;
                tyvar_env = Env.empty;
                value_env = Env.empty } in
      List.fold_left TyconTable.unsafe_add ctx Types.prim_types

end


