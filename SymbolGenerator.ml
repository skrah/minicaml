(*
 * Copyright (c) 2015 Stefan Krah. All rights reserved.
 *
 * This file is distributed under the terms of the Q Public License
 * version 1.0.
 *)



(**********************************************************************)
(*                           Symbol generators                        *)
(**********************************************************************)

module type ALL =
sig
  module ScopeID : Symbol.S with type t = Symbol.ScopeID.t
  module Tycon : Symbol.S with type t = Symbol.Tycon.t
  module Tyvar : Symbol.S with type t = Symbol.Tyvar.t
  module Tymeta : Symbol.S with type t = Symbol.Tymeta.t
  module Tyfield : Symbol.S with type t = Symbol.Tyfield.t
  module ValueID : Symbol.S with type t = Symbol.ValueID.t
end

module MakeGenerator (A : Symbol.S) : Symbol.S with type t = A.t =
struct
  include A

  let next_sym = ref A.start_id
  let make str = let id = Util.post_incr next_sym in A.unsafe_make (str, id)
end

module Make (A: sig end) : ALL =
struct
  module ScopeID = MakeGenerator (Symbol.ScopeID)
  module Tycon = MakeGenerator (Symbol.Tycon)
  module Tyvar = MakeGenerator (Symbol.Tyvar)
  module Tymeta = MakeGenerator (Symbol.Tymeta)
  module Tyfield = MakeGenerator (Symbol.Tyfield)
  module ValueID = MakeGenerator (Symbol.ValueID)
end



