open Interpreter
open Help

let print_arrow () = ANSITerminal.(print_string [blue] "::> ")

let print_error msg = ANSITerminal.(print_string [red] (spacing ^ "ERROR: " ^ msg ^ "\n"))

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
    | CreateDBResponse (x, msg) ->
      if x then print_output "Successfully created database!"
      else print_error msg
    | CreateColResponse (x, msg) ->
      if x then print_output "Successfully created collection!"
      else print_error msg
    | CreateDocResponse (x, msg) ->
      if x then print_output "Successfully created document!"
      else print_error msg
    | RemoveDocResponse (x, msg) ->
      if x then print_output "Successfully removed document!"
      else print_error msg
    | ReplaceDocResponse (x, msg) ->
      if x then print_output "Successfully replaced document!"
      else print_error msg
    | DropDBResponse (x, msg) ->
      if x then print_output "Successfully dropped database!"
      else print_error msg
    | DropColResponse (x, msg) ->
      if x then print_output "Succesfully dropped collection!"
      else print_error msg
    | QueryResponse (x, output) ->
      if x then print_output output
      else print_error "Query failed."
    | ShowColResponse (x, output) ->
      if x then print_output output
      else print_error "Col does not exist."
    | ShowDBResponse (x, output) ->
      if x then print_output output
      else print_error "DB does not exist."
    | ShowCatalogResponse (x, output) ->
      if x then print_output output
      else print_error "Something went wrong with the environment."
    | UpdateColResponse (x, msg) ->
      if x then print_output "Successfully updated collection!"
      else print_error msg
    | AggregateResponse (x, output) ->
      if x then print_output output
    else print_error "Aggregation failed."
    | ParseErrorResponse(x, output) ->
      print_error ("Parsing failed. " ^ output)
  ));
  print_arrow ();
  let new_input = read_line () in
  loop new_input

