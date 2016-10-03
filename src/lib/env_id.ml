open Core.Std

type t =
  | Cartpole_v0

let to_string = function
  | Cartpole_v0 -> "CartPole-v0"

let of_string = function
  | "CartPole-v0" -> Ok Cartpole_v0
  | otherwise -> Or_error.errorf "Unknown env-id %s" otherwise
