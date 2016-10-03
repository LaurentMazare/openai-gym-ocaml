module T = Environment.Make(struct
  let env_id = Env_id.Cartpole_v0
  type action = int
  let action_to_json int = `Int int
end)

include T
