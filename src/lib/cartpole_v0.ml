module Arg = struct
  let env_id = Env_id.of_string "CartPole-v0"
  type action = A0 | A1
  let action_to_json = function
    | A0 -> `Int 0
    | A1 -> `Int 1
end

include Environment.Make(Arg)
