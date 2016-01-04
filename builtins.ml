(* Builtin functions and types *)

type 'a ref = { mutable contents : 'a }

external print_string : string -> unit = "MiniCaml_print_string"
external print_i64 : int -> unit = "MiniCaml_print_i64"
external print_float : float -> unit = "MiniCaml_print_float"
external flush : unit -> unit = "MiniCaml_flush"
external getchar : unit -> string = "MiniCaml_getchar"
external ord : string -> int = "MiniCaml_ord"
external chr : int -> string = "MiniCaml_chr"
external size : string -> int = "MiniCaml_size"
external concat : string * string -> string = "MiniCaml_concat"
external strcmp : string * string -> int = "MiniCaml_strcmp"

external ref : 'a -> 'a ref = "MiniCaml_ref"
external (:=) : 'a ref * 'a -> unit = "MiniCaml_assign_ref"
external (!) : 'a ref -> 'a = "MiniCaml_deref"



