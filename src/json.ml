open Core.Std

type t = Yojson.Safe.json

let of_string str =
  Or_error.try_with (fun () -> Yojson.Safe.from_string str)

let to_string json =
  Yojson.Safe.to_string json

let find_assoc json ~key =
  match json with
  | `Assoc assoc_list ->
    begin
      match List.Assoc.find assoc_list key with
      | Some value -> Ok value
      | None -> Or_error.errorf "Cannot find key %s in %s" key (to_string json)
    end
  | otherwise -> Or_error.errorf "Expected assoc, got %s" (to_string otherwise)

let extract_assoc json ~f =
  match json with
  | `Assoc assoc_list ->
    List.map assoc_list ~f:(fun (key, json) ->
      Or_error.map (f json) ~f:(fun v -> key, v))
    |> Or_error.combine_errors
  | otherwise -> Or_error.errorf "Expected assoc, got %s" (to_string otherwise)

let extract_string = function
  | `String str -> Ok str
  | otherwise -> Or_error.errorf "Expected string, got %s" (to_string otherwise)

let extract_float = function
  | `Float f -> Ok f
  | otherwise -> Or_error.errorf "Expected float, got %s" (to_string otherwise)

let extract_list json ~f =
  match json with
  | `List l ->
    List.map l ~f
    |> Or_error.combine_errors
  | otherwise -> Or_error.errorf "Expected list, got %s" (to_string otherwise)
