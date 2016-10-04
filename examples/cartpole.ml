open Core.Std
open Async.Std

let run ~host ~port =
  Cartpole_v0.create ~host ~port
  >>=? fun t ->
  Cartpole_v0.run t 5
    ~init:()
    ~reset_f:(fun () ~idx:_ ~obs:_ ->
      0, Cartpole_v0.Arg.A1)
    ~step_f:(fun cnt ~obs ~reward ->
      printf "Obs: %s Reward: %f\n%!"
        ([%sexp_of: float list] obs |> Sexp.to_string)
        reward;
      cnt+1, Cartpole_v0.Arg.A1)
    ~done_f:(fun cnt ->
      printf "Session lasted for %d steps.\n%!" cnt)

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
      >>| ok_exn)
  |> Command.run
