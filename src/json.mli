open Core.Std

type t

val of_string : string -> t Or_error.t
val to_string : t -> string

val extract_list
  :  t
  -> f:(t -> 'a Or_error.t)
  -> 'a list Or_error.t

val extract_string : t -> string Or_error.t

val extract_float : t -> float Or_error.t

val extract_bool : t -> bool Or_error.t

val extract_assoc
  :  t
  -> f:(t -> 'a Or_error.t)
  -> (string * 'a) list Or_error.t

val find_assoc
  :  t
  -> key:string
  -> t Or_error.t
