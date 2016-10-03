open Core.Std
open Async.Std

let run ~host ~port =
  let t = Query.create ~host ~port in
  Query.new_env t Cartpole_v0
  >>= fun instance_id ->
  let instance_id = ok_exn instance_id in
  printf "env-id: %s\n%!" (Instance_id.to_string instance_id);
  Query.get_env t
  >>= fun envs ->
  let envs = ok_exn envs in
  List.iter envs ~f:(fun (instance_id, env_id) ->
    let env_id =
      match env_id with
      | Ok env_id -> Env_id.to_string env_id
      | Error err -> Error.to_string_hum err
    in
    printf "%s -> %s\n%!" (Instance_id.to_string instance_id) env_id);
  Query.reset t instance_id
  >>= fun obs ->
  let obs = ok_exn obs in
  List.iter obs ~f:(fun obs ->
    printf "%f\n%!" obs);
  Query.step t instance_id ~action:1
  >>= fun step_result ->
  let step_result = ok_exn step_result in
  printf "%s\n%!" (Query.Step_result.sexp_of_t step_result |> Sexp.to_string);
  Deferred.unit

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
    (fun host port () -> run ~host ~port)
  |> Command.run
