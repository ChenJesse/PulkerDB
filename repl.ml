open Interpreter
open Help

let print_arrow () = ANSITerminal.(print_string [blue] "::> ")

let print_error err = ANSITerminal.(print_string [red] (spacing ^ "ERROR: " ^ err ^ "\n"))

let print_output msg = ANSITerminal.(print_string [magenta] (spacing ^ msg ^ "\n"))

let print_info msg = ANSITerminal.(print_string [green] (spacing ^ msg ^ "\n"))

let rec loop input = (
  match input with 
  | "-exit" -> Persist.write_env Db.environment; exit(0)
  | "-help" -> ANSITerminal.(print_info help_msg)
  | "-gen_doc" -> ANSITerminal.(print_info gen_doc_msg)
  | "-query_doc" -> ANSITerminal.(print_info query_doc_msg)
  | "-update_doc" -> ANSITerminal.(print_info update_doc_msg)
  | "-agg_doc" -> ANSITerminal.(print_info agg_doc_msg)
  | _ -> (
    match parse input with 
    | Success msg -> print_output msg
    | Failure err -> print_error err
  ));
  print_arrow ();
  let new_input = read_line () in
  loop new_input

