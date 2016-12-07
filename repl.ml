open Interpreter
open Help
open Db

let print_arrow () = ANSITerminal.(print_string [blue] "::> ")

let print_error err = 
  ANSITerminal.(print_string [red] (spacing ^ "ERROR: " ^ err ^ "\n"))

let print_output msg = 
  ANSITerminal.(print_string [magenta] (spacing ^ msg ^ "\n"))

let print_info msg = ANSITerminal.(print_string [green] (spacing ^ msg ^ "\n"))

let rec loop input = (
  match input with 
  | "-exit" -> ANSITerminal.(print_info exiting_msg);
              let _ = Db.save_env () in exit(0)
  | "-help" ->       ANSITerminal.(print_info help_msg)
  | "-gen_doc" ->    ANSITerminal.(print_info gen_doc_msg)
  | "-query_doc" ->  ANSITerminal.(print_info query_doc_msg)
  | "-update_doc" -> ANSITerminal.(print_info update_doc_msg)
  | "-agg_doc" ->    ANSITerminal.(print_info agg_doc_msg)
  | "-index_doc" ->  ANSITerminal.(print_info index_doc_msg)
  | "-indkey_doc" -> ANSITerminal.(print_info indkey_doc_msg)
  | "-store" ->      ANSITerminal.(print_info store_msg)
  | "-show" ->       ANSITerminal.(print_info show_msg)
  | "-save" ->       ANSITerminal.(print_info save_msg)
  | "-usdb" ->       ANSITerminal.(print_info usdb_msg)
  | "-usbm" ->       ANSITerminal.(print_info usbm_msg)
  | "-drdb" ->       ANSITerminal.(print_info drdb_msg)
  | "-dbsh" ->       ANSITerminal.(print_info dbsh_msg)
  | "-ccol" ->       ANSITerminal.(print_info ccol_msg)
  | "-drcl" ->       ANSITerminal.(print_info drcl_msg)
  | "-isrt" ->       ANSITerminal.(print_info isrt_msg)
  | "-find" ->       ANSITerminal.(print_info find_msg)
  | "-clsh" ->       ANSITerminal.(print_info clsh_msg)
  | "-repl" ->       ANSITerminal.(print_info repl_msg)
  | "-updt" ->       ANSITerminal.(print_info updt_msg)
  | "-rmve" ->       ANSITerminal.(print_info rmve_msg)
  | "-aggr" ->       ANSITerminal.(print_info aggr_msg)
  | "-cidx" ->       ANSITerminal.(print_info cidx_msg)
  | "-gidx" ->       ANSITerminal.(print_info gidx_msg)
  | _ -> (
    match parse input with 
    | Success msg -> print_output msg
    | Failure err -> print_error err
  ));
  print_arrow ();
  let new_input = read_line () in
  loop new_input

