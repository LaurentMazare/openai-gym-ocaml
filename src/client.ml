open Core.Std
open Async.Std

module Env_id = struct
  type t =
    | Cartpole_v0

  let to_string = function
    | Cartpole_v0 -> "CartPole-v0"

  let of_string = function
    | "CartPole-v0" -> Ok Cartpole_v0
    | otherwise -> Or_error.errorf "Unknown env-id %s" otherwise
end

module Instance_id : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let of_string = Fn.id
  let to_string = Fn.id
end

module Query = struct
  let uri ~server ~port ~path =
    let base_uri =
      sprintf "http://%s:%d/v1/%s/" server port path
      |> Uri.of_string
    in
    base_uri

  let get_env ~server ~port =
    let uri = uri ~server ~port ~path:"envs" in
    Cohttp_async.Client.get uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun body ->
    let open Or_error.Monad_infix in
    Json.of_string body
    >>= Json.find_assoc ~key:"all_envs"
    >>= Json.extract_assoc ~f:Json.extract_string
    >>| List.map ~f:(fun (key, value) -> Instance_id.of_string key, Env_id.of_string value)

  let new_env env_id ~server ~port =
    let uri = uri ~server ~port ~path:"envs" in
    let body =
      Yojson.Safe.to_string
        (`Assoc [ "env_id", `String (Env_id.to_string env_id) ])
    in
    let headers = Cohttp.Header.init_with "Content-type" "application/json" in
    Cohttp_async.Client.post uri
      ~headers
      ~body:(`String body)
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun body ->
    let open Or_error.Monad_infix in
    Json.of_string body
    >>= Json.find_assoc ~key:"instance_id"
    >>= Json.extract_string
    >>| Instance_id.of_string

  let reset instance_id ~server ~port =
    let instance_id = Instance_id.to_string instance_id in
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
    let open Or_error.Monad_infix in
    Json.of_string body
    >>= Json.find_assoc ~key:"observation"
    >>= Json.extract_list ~f:Json.extract_float
end

let run ~server ~port =
  Query.new_env Cartpole_v0 ~server ~port
  >>= fun instance_id ->
  let instance_id = ok_exn instance_id in
  printf "env-id: %s\n%!" (Instance_id.to_string instance_id);
  Query.get_env ~server ~port
  >>= fun envs ->
  let envs = ok_exn envs in
  List.iter envs ~f:(fun (instance_id, env_id) ->
    let env_id =
      match env_id with
      | Ok env_id -> Env_id.to_string env_id
      | Error err -> Error.to_string_hum err
    in
    printf "%s -> %s\n%!" (Instance_id.to_string instance_id) env_id);
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
