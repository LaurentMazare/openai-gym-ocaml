open Core.Std
open Async.Std

let run ~host ~port =
  Cartpole_v0.create ~host ~port
  >>=? fun t ->
  Cartpole_v0.reset t
  >>=? fun obs ->
  List.iter obs ~f:(fun obs ->
    printf "%f\n%!" obs);
  let rec loop n =
    if n = 0
    then return (Ok ())
    else
      Cartpole_v0.step t ~action:1
      >>=? fun step_result ->
      printf "%s\n%!" (Query.Step_result.sexp_of_t step_result |> Sexp.to_string);
      loop (n-1)
  in
  loop 10

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
