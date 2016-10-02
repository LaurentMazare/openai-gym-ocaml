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
        | Some (`Assoc instance_and_env_ids) ->
          List.map instance_and_env_ids ~f:(function
            | (instance_id, `String env_id) -> Ok (instance_id, env_id)
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
        | Some (`String instance_id) -> Ok instance_id
        | _ -> cannot_parse body
      end
    | _ -> cannot_parse body

  let reset instance_id ~server ~port =
    let uri = uri ~server ~port ~path:(sprintf "envs/%s/reset" instance_id) in
    let body =
      Yojson.Safe.to_string
        (`Assoc [ "instance_id", `String instance_id ])
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
        match List.Assoc.find assoc_list "observation" with
        | Some (`List obs) ->
          List.map obs ~f:(function
            | `Float obs -> Ok obs
            | obs -> cannot_parse (Yojson.Safe.to_string obs))
          |> Or_error.combine_errors
        | _ -> cannot_parse body
      end
    | _ -> cannot_parse body
end

let run ~server ~port =
  Query.new_env "CartPole-v0" ~server ~port
  >>= fun instance_id ->
  let instance_id = ok_exn instance_id in
  printf "env-id: %s\n%!" instance_id;
  Query.get_env ~server ~port
  >>= fun envs ->
  let envs = ok_exn envs in
  List.iter envs ~f:(fun (instance_id, env_id) ->
    printf "%s -> %s\n%!" instance_id env_id);
  Query.reset instance_id ~server ~port
  >>| fun obs ->
  let obs = ok_exn obs in
  List.iter obs ~f:(fun obs ->
    printf "%f\n%!" obs)

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