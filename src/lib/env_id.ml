open Core.Std

include String_id.Make(struct
  let module_name = "env-id"
end)()
