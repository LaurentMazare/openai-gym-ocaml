open Core.Std
open Async.Std

module type Arg_intf = sig
  val env_id : Env_id.t
  type action

  val action_to_json : action -> Json.t
end

module Make(Arg : Arg_intf) : Environment_intf.S with type action = Arg.action = struct
  include Arg

  type t =
    { query : Query.t
    ; instance_id : Instance_id.t
    }

  let create ~host ~port =
    let query = Query.create ~host ~port in
    Query.new_env query env_id
    >>|? fun instance_id ->
    { query; instance_id }
    
  let reset t =
    Query.reset t.query t.instance_id

  let step t ~action =
    Query.step t.query t.instance_id ~action:(action_to_json action)
end
