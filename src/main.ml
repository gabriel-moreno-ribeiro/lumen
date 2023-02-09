(* lumen [file.lm]   runs a file, or starts a REPL when no file is given *)

let run_file path =
  let src = In_channel.with_open_bin path In_channel.input_all in
  match Interp.run src print_string with
  | Ok () -> exit 0
  | Error msg -> prerr_endline msg; exit 70

let repl () =
  print_endline "lumen repl - type statements ending in ';' (ctrl-d to quit)";
  let env = Interp.make_globals print_string in
  let rec loop () =
    print_string "> ";
    flush stdout;
    match In_channel.input_line stdin with
    | None -> print_newline ()
    | Some line ->
      let line = String.trim line in
      if line <> "" then begin
        (* let a bare expression be printed, like a calculator *)
        let src = if String.ends_with ~suffix:";" line || String.ends_with ~suffix:"}" line then line else "print " ^ line ^ ";" in
        match Interp.run ~env src print_string with
        | Ok () -> ()
        | Error msg -> prerr_endline msg
      end;
      loop ()
  in
  loop ()

let () =
  match Sys.argv with
  | [| _ |] -> repl ()
  | [| _; path |] -> run_file path
  | _ -> prerr_endline "usage: lumen [file.lm]"; exit 2
