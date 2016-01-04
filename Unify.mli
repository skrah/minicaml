(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

exception ValidationError
exception CyclicType
module Make :
  functor (ModuleState : ModuleState.S) ->
    sig
      module Tvar_env :
        sig
          type key = ModuleState.Tyvar.t
          type 'a t = 'a Map.Make(ModuleState.Tyvar).t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val max_binding : 'a t -> key * 'a
          val choose : 'a t -> key * 'a
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      val add : 'a Tvar_env.t -> Tvar_env.key -> 'a -> 'a Tvar_env.t
      val find_env : 'a Tvar_env.t -> Tvar_env.key -> 'a option
      val tmeta_of_var : int -> ModuleState.Tyvar.t -> Types.t
      val validate : bool -> Types.t -> unit
      val subst : Types.t Tvar_env.t -> Types.t -> Types.t
      val subst_with_mapping :
        Tvar_env.key list -> Types.t list -> Types.t -> Types.t
      val rename_params :
        Tvar_env.key list -> Types.t -> Symbol.Tyvar.t list * Types.t
      val subst_with_meta : int -> Tvar_env.key list -> Types.t -> Types.t
      val expand : Types.t -> Types.t
      val expand_links : Types.t -> Types.t
      val occurs :
        Types.tmeta ref -> Symbol.Tymeta.t -> int -> Types.t -> unit
      val unify : Types.t -> Types.t -> unit
      val generalize : int -> Types.t -> Types.t
      val instantiate : int -> Types.t -> Types.t
      val requantify : Types.t -> Symbol.Tyvar.t list * Types.t
      val compress : Types.t -> Types.t
      val repr : ?closure:bool -> ?fullname:bool -> Types.t -> string
      val short_repr : ?closure:bool -> ?fullname:bool -> Types.t -> string
      val tfun_repr : ?closure:bool -> ?fullname:bool -> Types.tfun -> string * string
    end
