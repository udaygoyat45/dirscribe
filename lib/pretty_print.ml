open Async

let indent = 2
let bar = "│"
let branch = "├─"
let block = bar ^ String.make indent ' '
let n_block n = String.concat "" (List.init n (fun _ -> block))

let pretty_print_string str depth =
  match depth with
  | _ when depth < 0 -> printf "%s\n" str
  | _ ->
      printf "%s\n" (n_block depth ^ branch ^ " " ^ str);
      ignore (Writer.flushed (Lazy.force Writer.stdout))

let pretty_printf_string format str depth =
  let str' = Stdlib.Printf.sprintf format str in
  (match depth with
  | _ when depth < 0 -> printf "%s\n" str'
  | _ -> printf "%s\n" (n_block depth ^ branch ^ " " ^ str'));
  ignore (Writer.flushed (Lazy.force Writer.stdout))

let pretty_sprint_string str depth =
  match depth with
  | _ when depth < 0 -> Stdlib.Printf.sprintf "%s\n" str
  | _ -> Stdlib.Printf.sprintf "%s\n" (n_block depth ^ branch ^ " " ^ str)

let pretty_sprintf_string format str depth =
  let str' = Stdlib.Printf.sprintf format str in
  match depth with
  | _ when depth < 0 -> Stdlib.Printf.sprintf "%s\n" str'
  | _ -> Stdlib.Printf.sprintf "%s\n" (n_block depth ^ branch ^ " " ^ str')
