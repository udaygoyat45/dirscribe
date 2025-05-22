open! Core
open! Async

let print_dir_summary dir ~api_key ~model =
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
  never_returns (Scheduler.go ())

(* ignore (Dirscribe.Llm_utils.print_directory_with_summaries root_dir ~api_key);
  never_returns (Scheduler.go ()) *)

(* let print_llm_content ~model ~api_key ~prompt =
  let%bind json = Dirscribe.Llm_utils.llm_request ~model ~api_key ~prompt in
  let message = Dirscribe.Llm_utils.extract_llm_response json in
  let stdout_writer = Lazy.force Writer.stdout in
  Writer.write stdout_writer message;
  Writer.flushed stdout_writer

let () =
  let path = "." in
  let abs_path =
    if Filename.is_absolute path then path
    else Filename.concat (Sys_unix.getcwd ()) path
  in
  Stdlib.Printf.printf "%s\n" "Testing LLMs";
  let envs = Dirscribe.Os_utils.load_env_file abs_path () in
  let api_key = Map.find_exn envs "GEMINI_API_KEY" in
  let prompt = "TESTING PURPOSES. REPLY BACK WITH A HELLO" in
  let model = Dirscribe.Llm_utils.Gemini20 in

  ignore (print_llm_content ~model ~prompt ~api_key);
  never_returns (Scheduler.go ()) *)
