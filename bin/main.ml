open! Core
open! Async

(* let print_dir_summary dir ~api_key ~model =
  let%bind dir_with_summaries =
    Dirscribe.Llm_utils.generate_directory_summary dir ~model ~api_key
  in
  (match dir_with_summaries.llm_summary with
  | YetToBegin -> printf "%s\n" "NO SUMMARY WAS GENERATED"
  | Finished llm_summary' -> printf "SUMMARY: %s\n" llm_summary');
  return ()

let () =
  let path = "." in
  let abs_path =
    if Filename.is_absolute path then path
    else Filename.concat (Sys_unix.getcwd ()) path
  in
  Dirscribe.Os_utils.validate_path abs_path;
  let envs = Dirscribe.Os_utils.load_env_file abs_path () in
  let api_key = Map.find_exn envs "GEMINI_API_KEY" in
  let model = Dirscribe.Llm_utils.Gemini20 in
  let ignore_paths = Dirscribe.Os_utils.load_gitignore_file abs_path () in
  let root_dir = Dirscribe.Os_utils.collect_dir ~ignore_paths abs_path in
  Dirscribe.Os_utils.print_dir root_dir;
  ignore (print_dir_summary root_dir ~model ~api_key);
  never_returns (Scheduler.go ()) *)

type sync_validated = { ignored_paths : string list }
type async_validated = { env_vars : string Map.M(String).t }

let sync_validate path depth env_path =
  printf "PATH: %s\nDEPTH: %d\nENV PATH: %s\n" path depth env_path;
  let depth_validation depth =
    if depth = 0 then raise_s [%message "The depth cannot be 0"]
    else if depth < 0 then raise_s [%message "The depth cannot be negative"]
  in

  (* validate the root directory path *)
  let open Or_error.Let_syntax in
  let%bind () =
    Or_error.try_with (fun () -> Dirscribe.Os_utils.validate_path_exn path)
  in
  (* validate the depth *)
  let%bind () = Or_error.try_with (fun () -> depth_validation depth) in
  (* validate the paths to ignore *)
  let%bind ignored_paths =
    Or_error.try_with (Dirscribe.Os_utils.load_gitignore_file path)
  in
  Ok { ignored_paths }

let async_validate dot_dirscribe_path =
  match%map
    Async.try_with (fun () ->
        Dirscribe.Os_utils.validate_dot_dirscribe dot_dirscribe_path)
  with
  | Ok () ->
      let env_path = Filename.concat dot_dirscribe_path ".env" in
      let open Or_error.Let_syntax in
      let%bind envs =
        Or_error.try_with (fun () ->
            Dirscribe.Os_utils.load_env_file env_path ())
      in
      Ok { env_vars = envs }
  | Error err -> Or_error.of_exn err

let run path depth dot_dirscribe_path =
  let%map env_vars_err = async_validate dot_dirscribe_path in
  let env_vars = (match env_vars_err with
  | Ok { env_vars } -> env_vars
  | Error err -> eprintf "%s\n" (Error.to_string_hum err) in

  let ignored_paths = sync_validate path depth in
  match ignored_paths with
  | Ok { ignored_paths } -> ()
  | Error err -> eprintf "%s\n" (Error.to_string_hum err) Deferred.never ())

let () =
  Command.async ~summary:"Reads the given path and summarizes the context"
    (let%map_open.Command path = anon ("path" %: string)
     and depth =
       flag "-depth" (required int)
         ~doc:"Depth means the number of times subdirectories to recurse until"
     and env_path =
       flag "-dot-dirscribe-path"
         (optional_with_default "~/.dirscribe" string)
         ~doc:"File specifying the parameters for dirscribe"
     in
     fun () -> run path depth env_path)
  |> Command_unix.run
