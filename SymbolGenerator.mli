(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)

module type ALL =
sig
  module ScopeID : Symbol.S with type t = Symbol.ScopeID.t
  module Tycon : Symbol.S with type t = Symbol.Tycon.t
  module Tyvar : Symbol.S with type t = Symbol.Tyvar.t
  module Tymeta : Symbol.S with type t = Symbol.Tymeta.t
  module Tyfield : Symbol.S with type t = Symbol.Tyfield.t
  module ValueID : Symbol.S with type t = Symbol.ValueID.t
end
module Make : functor (A : sig  end) -> ALL
