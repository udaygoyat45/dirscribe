open Core
open Async

type gemini_model = Gemini25 | Gemini25Pro | Gemini20

let generate_gemini_uri ~model ~api_key =
  let model_str =
    match model with
    | Gemini25Pro -> "gemini-2.5-pro"
    | Gemini25 -> "gemini-2.5-flash"
    | Gemini20 -> "gemini-2.0-flash"
  in
  Uri.of_string
    (Printf.sprintf
       "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
       model_str api_key)

let llm_request ~model ~api_key ~prompt =
  let uri = generate_gemini_uri ~model ~api_key in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body_json =
    `Assoc
      [
        ( "contents",
          `List
            [
              `Assoc
                [ ("parts", `List [ `Assoc [ ("text", `String prompt) ] ]) ];
            ] );
      ]
    |> Yojson.Safe.to_string
  in
  let body = Cohttp_async.Body.of_string body_json in
  let%bind resp, body = Cohttp_async.Client.post ~headers ~body uri in
  let status = Cohttp.Response.status resp in
  match status with
  | `Code 200 | `OK ->
      let%map body_str = Cohttp_async.Body.to_string body in
      Yojson.Safe.from_string body_str
  | `Code code_int ->
      raise_s
        [%message
          "Error making llm request"
            (status : Cohttp.Code.status_code)
            (code_int : int)]
  | _ ->
      raise_s
        [%message "Error making llm request" (status : Cohttp.Code.status_code)]

let content_of_gemini_json (llm_summary : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  llm_summary |> member "candidates" |> to_list
  |> List.concat_map ~f:(fun candidate ->
         candidate |> member "content" |> member "parts" |> to_list
         |> List.map ~f:(fun text -> text |> member "text" |> to_string))
  |> String.concat ~sep:""

let generate_file_summarization_prompt content =
  Printf.sprintf
    "This prompt is part of a greater application which is trying to \
     understand the context of all the files in a directory and how they work \
     together. This is just one part of the application, where your job is to \
     summarize the content of this file. Please do so within 4-5 sentences.\n\n\
     %s"
    content

let generate_dir_summarization_prompt (directory : Os_utils.directory) =
  let prompt_header =
    "This prompt is part of a greater application which is trying to \
     understand the context of all the files within a root directory. This \
     prompt is concerning a subdirectory. There will be 3 pieces of \
     information provided to you: (1) The hierarchy of this directory (2) The \
     summaries of the subdirectories within this directory (3) The summaries \
     of files within the current directory.\n"
  in
  let hierarchy = Os_utils.string_of_dir directory in
  let dir_summaries =
    List.map directory.directories ~f:(fun subdir ->
        match subdir with
        | AvailableDir subdir' -> (
            match subdir'.llm_summary with
            | YetToBegin ->
                raise_s
                  [%message
                    "Every subdirectory should have a summary"
                      (subdir' : Os_utils.directory)]
            | Finished llm_summary ->
                Stdlib.Printf.sprintf "SUBDIRECTOR_NAME: %s\n SUMMARY:%s\n"
                  subdir'.relative_path llm_summary)
        | IgnoreDir subdir_name ->
            Stdlib.Printf.sprintf
              "SUBDIRECTORY_NAME: %s\n\
              \ This subdirectory is ignored (no summary provided)\n"
              subdir_name
        | DepthLimit subdir_name ->
            Stdlib.Printf.sprintf
              "SUBDIRECTORY_NAME: %s\n\
              \ This subdirectory is beyond the depth limit (no summary \
               provided)\n"
              subdir_name)
    |> String.concat ~sep:"----------------------------------------\n"
  in
  let file_summaries =
    List.filter_map directory.files ~f:(fun file ->
        let is_text_based = Os_utils.is_opaque_file_summarizable file in
        match file with
        | AvailableFile file' -> (
            match file'.llm_summary with
            | YetToBegin when not is_text_based -> None
            | YetToBegin ->
                raise_s
                  [%message
                    "Every text-based file should have a summary"
                      (file' : Os_utils.file)]
            | Finished llm_summary ->
                Some
                  (Stdlib.Printf.sprintf "FILE_NAME: %s\n SUMMARY:%s\n"
                     file'.name llm_summary))
        | IgnoreFile file_name ->
            Some
              (Stdlib.Printf.sprintf
                 "FILE_NAME: %s\n This file is ignored (no summary provided)\n"
                 file_name))
    |> String.concat ~sep:"----------------------------------------\n"
  in

  String.concat ~sep:"-------------------------------------"
    [ prompt_header; hierarchy; dir_summaries; file_summaries ]

let generate_current_directory_files_summaries (directory : Os_utils.directory)
    ~model ~api_key =
  let extract_valid_files (dir : Os_utils.directory) =
    List.filter_map dir.files ~f:(function
      | Os_utils.AvailableFile file -> (
          match file.content with
          | Os_utils.Text _ -> (
              match file.llm_summary with YetToBegin -> Some file | _ -> None)
          | Os_utils.NonText -> None)
      | Os_utils.IgnoreFile _ -> None)
  in
  let valid_files = extract_valid_files directory in
  let valid_files_paths =
    List.map valid_files ~f:(fun file -> file.absolute_path)
  in
  let file_to_summarization_prompt (file : Os_utils.file) =
    match file.content with
    | Os_utils.Text content -> content
    | Os_utils.NonText ->
        raise_s
          [%message
            "Cannot create a prompt for a NonText file" (file : Os_utils.file)]
  in
  let%bind file_summaries =
    Deferred.all
      (List.map valid_files ~f:(fun file ->
           let prompt = file_to_summarization_prompt file in
           let%bind llm_json = llm_request ~model ~api_key ~prompt in
           let%bind () =
             Pretty_print.inform_user "FILE DONE: %s\n" file.relative_path
           in
           let%bind () = Writer.flushed (Lazy.force Writer.stdout) in
           return (content_of_gemini_json llm_json)))
  in
  let files_summaries_paired = List.zip_exn valid_files_paths file_summaries in
  let path_to_summary =
    Map.of_alist_exn (module String) files_summaries_paired
  in
  let update_directory (dir_node : Os_utils.directory) =
    let files' =
      List.map dir_node.files ~f:(fun (file : Os_utils.fileVisibility) ->
          match file with
          | AvailableFile file' ->
              if Map.mem path_to_summary file'.absolute_path then
                Os_utils.AvailableFile
                  {
                    file' with
                    llm_summary =
                      Finished
                        (Map.find_exn path_to_summary file'.absolute_path);
                  }
              else file
          | IgnoreFile _ -> file)
    in
    { dir_node with files = files' }
  in
  return (update_directory directory)

let generate_current_directory_summary (directory : Os_utils.directory) ~model
    ~api_key =
  let prompt = generate_dir_summarization_prompt directory in
  let%bind llm_json = llm_request ~model ~api_key ~prompt in
  let llm_content = content_of_gemini_json llm_json in
  let%bind () =
    Pretty_print.inform_user "DIR DONE: %s\n" directory.relative_path
  in
  let%bind () = Writer.flushed (Lazy.force Writer.stdout) in
  return { directory with llm_summary = Finished llm_content }

let rec generate_directory_summary (root_directory : Os_utils.directory) ~model
    ~api_key =
  let%bind root_directory' =
    generate_current_directory_files_summaries root_directory ~model ~api_key
  in
  let available_dirs =
    List.filter_map root_directory.directories ~f:(fun subdir ->
        match subdir with AvailableDir subdir' -> Some subdir' | _ -> None)
  in
  let unavailable_opaque_dirs =
    List.filter root_directory.directories ~f:(fun subdir ->
        match subdir with AvailableDir _ -> false | _ -> true)
  in
  let%bind subdirectories =
    Deferred.all
      (List.map available_dirs ~f:(fun subdir ->
           generate_directory_summary subdir ~model ~api_key))
  in
  let available_opaque_dirs =
    List.map subdirectories ~f:(fun subdir -> Os_utils.AvailableDir subdir)
  in
  let opaque_subdirectories = unavailable_opaque_dirs @ available_opaque_dirs in
  let root_directory'' =
    { root_directory' with directories = opaque_subdirectories }
  in
  generate_current_directory_summary root_directory'' ~model ~api_key

let print_directory_with_summaries dir ~api_key =
  let%bind root_dir_with_summaries =
    generate_directory_summary dir ~model:Gemini20 ~api_key
  in
  List.iter root_dir_with_summaries.files ~f:(fun file ->
      let llm_summary = Os_utils.llm_summary_of_opaque_file file in
      let name = Os_utils.name_of_opaque_file file in
      match llm_summary with
      | Some content ->
          Async.printf
            "FILE_NAME: %s\n\nSUMMARY: %s\n\n--------------------------------\n"
            name content
      | None -> ());
  return ()
