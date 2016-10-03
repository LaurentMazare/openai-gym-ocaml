open Core.Std
open Async.Std

type t =
  { host : string
  ; port : int
  }

let create ~host ~port =
  { host
  ; port
  }

let uri t ~path =
  let base_uri =
    sprintf "http://%s:%d/v1/%s/" t.host t.port path
    |> Uri.of_string
  in
  base_uri

let get_env t =
  let uri = uri t ~path:"envs" in
  Cohttp_async.Client.get uri
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body
  >>| fun body ->
  let open Or_error.Monad_infix in
  Json.of_string body
  >>= Json.find_assoc ~key:"all_envs"
  >>= Json.extract_assoc ~f:Json.extract_string
  >>| List.map ~f:(fun (key, value) -> Instance_id.of_string key, Env_id.of_string value)

let new_env t env_id =
  let uri = uri t ~path:"envs" in
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

let reset t instance_id =
  let instance_id = Instance_id.to_string instance_id in
  let uri = uri t ~path:(sprintf "envs/%s/reset" instance_id) in
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

module Step_result = struct
  type t =
    { observation : float list
    ; reward : float
    ; is_done : bool
    } [@@deriving sexp]
end

let step t instance_id ~action =
  let instance_id = Instance_id.to_string instance_id in
  let uri = uri t ~path:(sprintf "envs/%s/step" instance_id) in
  let body =
    Yojson.Safe.to_string
      (`Assoc
        [ "instance_id", `String instance_id
        ; "action", `Int action
        ])
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
  >>= fun json ->
  Json.find_assoc json ~key:"observation"
  >>= Json.extract_list ~f:Json.extract_float
  >>= fun observation ->
  Json.find_assoc json ~key:"reward"
  >>= Json.extract_float
  >>= fun reward ->
  Json.find_assoc json ~key:"done"
  >>= Json.extract_bool
  >>| fun is_done ->
  { Step_result.observation; reward; is_done }
