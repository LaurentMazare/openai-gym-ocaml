open Core.Std
open Async.Std

module type S = sig
  type t
  type action
  
  val create : host:string -> port:int -> t Or_error.t Deferred.t
  
  val reset : t -> float list Or_error.t Deferred.t
  
  val step : t -> action:action -> Query.Step_result.t Or_error.t Deferred.t

  val run
    :  t
    -> int (* Number of iterations. *)
    -> init:'a
    -> reset_f:('a -> idx:int -> obs:float list -> 'b * action)
    -> step_f:('b -> obs:float list -> reward:float -> 'b * action)
    -> done_f:('b -> 'a)
    -> 'a Or_error.t Deferred.t
end
