open Core.Std
open Async.Std

type t

val create
  :  host:string
  -> port:int
  -> t

val get_env
  :  t
  -> (Instance_id.t * Env_id.t Or_error.t) list Or_error.t Deferred.t

val new_env
  :  t
  -> Env_id.t
  -> Instance_id.t Or_error.t Deferred.t

val reset
  :  t
  -> Instance_id.t
  -> float list Or_error.t Deferred.t

module Step_result : sig
  type t =
    { observation : float list
    ; reward : float
    ; is_done : bool
    } [@@deriving sexp]
end

val step
  :  t
  -> Instance_id.t
  -> action:int
  -> Step_result.t Or_error.t Deferred.t
