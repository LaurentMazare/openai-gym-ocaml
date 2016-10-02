open Core.Std
open Async.Std

module Query = struct
  let uri ~server ~port ~path =
    let base_uri =
      sprintf "http://%s:%d/v1/%s/" server port path
      |> Uri.of_string
    in
    base_uri

  let cannot_parse body =
    Or_error.errorf "Cannot parse output: %s" body

  let get_env ~server ~port =
    let uri = uri ~server ~port ~path:"envs" in
    Cohttp_async.Client.get uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun body ->
    match Yojson.Safe.from_string body with
    | `Assoc assoc_list ->
      begin
        match List.Assoc.find assoc_list "all_envs" with
        | Some (`Assoc env_ids) ->
          List.map env_ids ~f:(function
            | (env_id, `String env) -> Ok (env_id, env)
            | _ -> cannot_parse body)
          |> Or_error.combine_errors
        | _ -> cannot_parse body
      end
    | _ -> cannot_parse body

  let new_env env_id ~server ~port =
    let uri = uri ~server ~port ~path:"envs" in
    let body =
      Yojson.Safe.to_string
        (`Assoc [ "env_id", `String env_id ])
    in
    let headers = Cohttp.Header.init_with "Content-type" "application/json" in
    Cohttp_async.Client.post uri
      ~headers
      ~body:(`String body)
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun body ->
    match Yojson.Safe.from_string body with
    | `Assoc assoc_list ->
      begin
        match List.Assoc.find assoc_list "instance_id" with
        | Some (`String env_id) -> Ok env_id
        | _ -> cannot_parse body
      end
    | _ -> cannot_parse body
end

let run ~server ~port =
  Query.new_env "CartPole-v0" ~server ~port
  >>= fun env_id ->
  let env_id = ok_exn env_id in
  printf "env-id: %s\n%!" env_id;
  Query.get_env ~server ~port
  >>= fun envs ->
  let envs = ok_exn envs in
  List.iter envs ~f:(fun (env_id, env) ->
    printf "%s -> %s\n%!" env_id env);
  Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-server" (optional_with_default "localhost" string)
        ~doc:" Convert to uppercase before echoing back"
      +> flag "-port" (optional_with_default 5000 int)
        ~doc:" Port to connect to (default 5000)"
    )
    (fun server port () -> run ~server ~port)
  |> Command.run
