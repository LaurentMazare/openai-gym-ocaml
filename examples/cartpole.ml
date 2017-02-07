open Core.Std
open Async.Std

let obs_size = 4

module Model : sig
  type t
  val random : unit -> t
  val action : t -> obs:float list -> Cartpole_v0.action
  val add_noise : t -> learning_rate:float -> t
end = struct
  type t = float list

  let random () = List.init obs_size ~f:(fun _i -> Random.float 2. -. 1.)

  let sigmoid x =
    1. /. (1. +. exp (-. x))

  let action t ~obs =
    List.fold2_exn t obs ~init:0. ~f:(fun acc x y -> acc +. x *. y)
    |> sigmoid
    |> fun x ->
    if x > 0.5
    then Cartpole_v0.Arg.A1
    else Cartpole_v0.Arg.A0

  let add_noise t ~learning_rate =
    List.map2_exn t (random ()) ~f:(fun x y -> x +. learning_rate *. y)
end

type t =
  { game_count : int
  ; best_reward : float
  ; best_model : Model.t
  }

let run ~host ~port =
  Cartpole_v0.create ~host ~port
  >>=? fun t ->
  let init =
    { game_count = 0
    ; best_reward = 0.
    ; best_model = Model.random ()
    }
  in
  Cartpole_v0.run t 20
    ~init
    ~reset_f:(fun t ~idx:_ ~obs:_ ->
      let model =
        Model.add_noise t.best_model ~learning_rate:(exp (-. 0.01 *. float t.game_count))
      in
      (t, model, 0.), Cartpole_v0.Arg.A1)
    ~step_f:(fun (t, model, acc_reward) ~obs ~reward ->
      let obs = Query.Float_tensor.to_list_exn obs in
      let action = Model.action model ~obs in
      (t, model, acc_reward +. reward), action)
    ~done_f:(fun (t, model, reward) ->
      printf "%6d %12f %12f\n%!" t.game_count reward t.best_reward;
      let best_model, best_reward =
        if t.best_reward < reward
        then model, reward
        else t.best_model, t.best_reward
      in
      { game_count = t.game_count + 1
      ; best_reward
      ; best_model
      })

let () =
  Command.async
    ~summary:"Simple openai-gym client"
    Command.Spec.(
      empty
      +> flag "-host" (optional_with_default "localhost" string)
        ~doc:" Convert to uppercase before echoing back"
      +> flag "-port" (optional_with_default 5000 int)
        ~doc:" Port to connect to (default 5000)"
    )
    (fun host port () ->
      run ~host ~port
      >>| fun x ->
      ignore (ok_exn x : t))
  |> Command.run
