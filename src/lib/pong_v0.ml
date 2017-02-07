module Arg = struct
  let env_id = Env_id.of_string "Pong-v0"

  type action =
    | Noop
    | Fire
    | Right
    | Left
    | Right_fire
    | Left_fire

  let action_to_json = function
    | Noop -> `Int 0
    | Fire -> `Int 1
    | Right -> `Int 2
    | Left -> `Int 3
    | Right_fire -> `Int 4
    | Left_fire -> `Int 5
end

include Environment.Make(Arg)

