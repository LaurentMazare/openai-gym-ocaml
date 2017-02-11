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
  | `Int i -> Ok (float i)
  | `Float f -> Ok f
  | otherwise -> Or_error.errorf "Expected float, got %s" (to_string otherwise)

let extract_bool = function
  | `Bool b -> Ok b
  | otherwise -> Or_error.errorf "Expected bool, got %s" (to_string otherwise)

let extract_list json ~f =
  match json with
  | `List l ->
    List.map l ~f
    |> Or_error.combine_errors
  | otherwise -> Or_error.errorf "Expected list, got %s" (to_string otherwise)

let extract_float_tensor t =
  let rec dims = function
    | `Int _ | `Float _ -> []
    | `List [] -> failwith "Empty dimension"
    | `List (l0 :: _ as l) ->
      List.length l :: dims l0
    | otherwise -> failwithf "Expected list or float, got %s" (to_string otherwise) ()
  in
  Or_error.try_with (fun () ->
    let dims = dims t |> Array.of_list in
    let tensor = Bigarray.Genarray.create Bigarray.float64 C_layout dims in
    let rec walk indexes = function
      | `Int i ->
        Bigarray.Genarray.set tensor (List.rev indexes |> Array.of_list) (float i)
      | `Float f ->
        Bigarray.Genarray.set tensor (List.rev indexes |> Array.of_list) f
      | `List l ->
        List.iteri l ~f:(fun index t -> walk (index :: indexes) t)
      | otherwise -> failwithf "Expected list or float, got %s" (to_string otherwise) ()
    in
    walk [] t;
    tensor)
