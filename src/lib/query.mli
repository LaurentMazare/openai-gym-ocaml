open Core.Std
open Async.Std

module Float_tensor : sig
  type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t
  val to_list_exn : t -> float list
end

type t

val create
  :  host:string
  -> port:int
  -> t

val get_env
  :  t
  -> (Instance_id.t * Env_id.t) list Or_error.t Deferred.t

val new_env
  :  t
  -> Env_id.t
  -> Instance_id.t Or_error.t Deferred.t

val reset
  :  t
  -> Instance_id.t
  -> Float_tensor.t Or_error.t Deferred.t

module Step_result : sig
  type t =
    { observation : Float_tensor.t
    ; reward : float
    ; is_done : bool
    } [@@deriving sexp]
end

val step
  :  t
  -> Instance_id.t
  -> action:Json.t
  -> Step_result.t Or_error.t Deferred.t
