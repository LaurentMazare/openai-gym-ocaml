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

  let run t iters ~init ~reset_f ~step_f ~done_f =
    let rec loop_step acc_bis action =
      step t ~action
      >>=? fun step_result ->
      if step_result.is_done
      then return (Ok acc_bis)
      else
        let acc_bis, action =
          step_f
            acc_bis
            ~obs:step_result.observation
            ~reward:step_result.reward
        in
        loop_step acc_bis action
    in
    let rec loop acc idx =
      if idx = iters
      then return (Ok acc)
      else
        reset t
        >>=? fun obs ->
        let acc_bis, action = reset_f acc ~idx ~obs in
        loop_step acc_bis action
        >>=? fun acc_bis ->
        let acc = done_f acc_bis in
        loop acc (idx + 1)
    in
    loop init 0
end
