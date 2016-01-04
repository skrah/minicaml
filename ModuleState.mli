(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

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
        val add_env :
          context -> string -> context * Symbol.Tycon.t
        val add_tbl :
          context -> string -> Types.tfun -> Symbol.Tycon.t
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
module type ARGS = sig val filename : string end
module Make : functor (A : ARGS) -> S
