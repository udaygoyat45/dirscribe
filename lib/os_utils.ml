open Core
open Async

type content_format = Text of string | NonText [@@deriving sexp]
type llm_summary = YetToBegin | Finished of string [@@deriving sexp]

type file = {
  name : string;
  absolute_path : string;
  content : content_format;
  llm_summary : llm_summary;
}
[@@deriving sexp]

type directory = {
  depth : int;
  absolute_path : string;
  relative_path : string;
  files : fileVisibility list;
  directories : dirVisibility list;
  llm_summary : llm_summary;
}
[@@deriving sexp]

and dirVisibility =
  | IgnoreDir of string
  | DepthLimit of string
  | AvailableDir of directory

and fileVisibility = AvailableFile of file | IgnoreFile of string

let max_depth = 1
let context_window = 1_000_000

let validate_path path =
  match Sys_unix.is_directory path with
  | `Yes -> ()
  | `Unknown ->
      raise_s
        [%message
          "Path could not be evaluated. Please check permissions"
            (path : string)]
  | `No -> raise_s [%message "Path is not a valid directory" (path : string)]

let load_env_file base_path () =
  let parse_line line =
    match String.split ~on:'=' line with
    | [ key; value ] -> Some (key, value)
    | _ -> None
  in
  let env_path = Filename.concat base_path ".env" in
  let dot_env_entries =
    In_channel.with_file env_path ~f:(fun file ->
        List.filter_map ~f:parse_line (In_channel.input_lines file))
  in
  let sys_env_entries = [] in
  let env_entries =
    List.merge dot_env_entries sys_env_entries ~compare:(fun _ _ -> 0)
  in
  match Map.of_alist_or_error (module String) env_entries with
  | Ok m -> m
  | Error e -> raise_s [%message "Ill-formatted env file" (e : Error.t)]

let load_gitignore_file base_path () =
  let parse_line line =
    let path = String.strip line in
    if Filename.is_absolute path then Some path
    else Some (Filename.concat base_path path)
  in
  let gitignore_file = Filename.concat base_path ".gitignore" in
  let ignore_paths =
    In_channel.with_file gitignore_file ~f:(fun file ->
        List.filter_map ~f:parse_line (In_channel.input_lines file))
  in
  let dot_git_path = Filename.concat base_path ".git" in
  match Sys_unix.is_directory dot_git_path with
  | `Yes -> dot_git_path :: ignore_paths
  | _ -> ignore_paths

let get_file_content file_path =
  let rec acquire_file ?(chars_read = 0) file =
    if chars_read > context_window then ""
    else
      match In_channel.input_line file with
      | Some line ->
          let new_len = String.length line + chars_read in
          if new_len > context_window then ""
          else line ^ "\n" ^ acquire_file ~chars_read:new_len file
      | None -> ""
  in
  let raw_file_content =
    In_channel.with_file file_path ~f:(fun file -> acquire_file file)
  in
  let trim_file_content content = String.prefix content context_window in
  let file_content = trim_file_content raw_file_content in
  let is_printable c =
    let code = Char.to_int c in
    (code >= 32 && code <= 126) || code = 9 || code = 10 || code = 13
  in
  let content_size = String.length file_content in
  let printable_size = String.count file_content ~f:is_printable in
  let ratio =
    Float.( / ) (Float.of_int printable_size) (Float.of_int content_size)
  in
  if Float.( > ) ratio 0.9 then Text file_content else NonText

let rec collect_data_aux ~ignore_paths path base_path depth =
  let absolute_path = Filename.concat base_path path in
  if depth > max_depth then DepthLimit (Filename.basename absolute_path)
  else if List.mem ignore_paths absolute_path ~equal:Filename.equal then
    IgnoreDir (Filename.basename absolute_path)
  else
    let dir_list = Sys_unix.ls_dir absolute_path in
    let files =
      List.filter_map
        ~f:(fun name ->
          let item_path = Filename.concat absolute_path name in
          match Sys_unix.is_file_exn item_path with
          | true when List.mem ignore_paths item_path ~equal:Filename.equal ->
              Some (IgnoreFile item_path)
          | true ->
              Some
                (AvailableFile
                   {
                     name;
                     content = get_file_content item_path;
                     llm_summary = YetToBegin;
                     absolute_path = item_path;
                   })
          | false -> None)
        dir_list
    in
    let dir_names =
      List.filter
        ~f:(fun name ->
          let item_path = Filename.concat absolute_path name in
          Sys_unix.is_directory_exn item_path)
        dir_list
    in
    let directories =
      List.map
        ~f:(fun dir_name ->
          let relative_dir_path = Filename.concat path dir_name in
          collect_data_aux ~ignore_paths relative_dir_path base_path (depth + 1))
        dir_names
    in
    AvailableDir
      {
        absolute_path;
        relative_path = path;
        files;
        depth;
        directories;
        llm_summary = YetToBegin;
      }

let collect_dir ~ignore_paths abs_path =
  printf "Root Directory: %s\n" abs_path;
  let root_dir = collect_data_aux ~ignore_paths "." abs_path 0 in
  match root_dir with
  | AvailableDir root_dir -> root_dir
  | IgnoreDir root_name ->
      raise_s
        [%message
          (Printf.sprintf "The root directory (%s) present in ignore path list"
             root_name)
            (ignore_paths : string list)
            (abs_path : string)]
  | DepthLimit root_name ->
      raise_s
        [%message
          "Check the depth limit is greater than 1"
            (root_name : string)
            (abs_path : string)
            (ignore_paths : string list)]

let string_of_file_visibility = function
  | AvailableFile file -> Stdlib.Printf.sprintf "%s" file.name
  | IgnoreFile file_path ->
      Stdlib.Printf.sprintf "%s (IGNORED FILE)" (Filename.basename file_path)

let content_of_opaque_file = function
  | AvailableFile file -> (
      match file.content with Text content -> Some content | NonText -> None)
  | IgnoreFile _ -> None

let llm_summary_of_opaque_file = function
  | AvailableFile file -> (
      match file.llm_summary with
      | YetToBegin -> None
      | Finished response -> Some response)
  | IgnoreFile _ -> None

let name_of_opaque_file = function
  | AvailableFile file -> file.name
  | IgnoreFile file_name -> file_name

let string_of_dir root_dir =
  let rec string_of_dir_aux dir =
    let header =
      Pretty_print.pretty_sprintf_string "%s" dir.relative_path (dir.depth - 1)
    in
    let file_str =
      List.map dir.files ~f:(fun file ->
          Pretty_print.pretty_sprint_string
            (string_of_file_visibility file)
            dir.depth)
      |> String.concat
    in
    let directory_str =
      List.map dir.directories ~f:(fun dir' ->
          match dir' with
          | AvailableDir dir' -> string_of_dir_aux dir'
          | IgnoreDir dir_name ->
              Pretty_print.pretty_sprintf_string "%s (IGNORED DIR)" dir_name
                dir.depth
          | DepthLimit dir_name ->
              Pretty_print.pretty_sprintf_string "%s (DEPTH REACHED)" dir_name
                dir.depth)
      |> String.concat
    in
    Stdlib.Printf.sprintf "%s%s%s" header file_str directory_str
  in
  string_of_dir_aux root_dir

let print_dir root_dir = printf "%s\n" (string_of_dir root_dir)

let is_opaque_file_summarizable = function
  | AvailableFile file -> (
      match file.content with Text _ -> true | NonText -> false)
  | _ -> false
