open Core.Std

type t =
  | Cartpole_v0

val to_string : t -> string
val of_string : string -> t Or_error.t
