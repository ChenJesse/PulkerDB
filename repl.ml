open Interpreter

let spacing = "    "

let print_arrow () = ANSITerminal.(print_string [blue] "::> ")

let print_error msg = ANSITerminal.(print_string [red] (spacing ^ "ERROR: " ^ msg ^ "\n"))

let print_output msg = ANSITerminal.(print_string [magenta] (spacing ^ msg ^ "\n"))

let help_msg = "
                        REPL COMMANDS
---------------------------------------------------------------
| -exit : Exit the programming gracefully                     |
| -gen_doc : Information on format for ANY_DOC                |
| -query_doc : Information on format for QUERY_DOC            |
| -update_doc : Information on format for UPDATE_DOC          |
| -agg_doc : Information on format for for AGG_DOC            |
---------------------------------------------------------------

                      DATABASE COMMANDS
---------------------------------------------------------------
| use DATABASE_NAME                                           |
| db.dropDatabase()                                           |
| db.createCollection(COLLECTION_NAME)                        |
| db.COLLECTION_NAME.drop()                                   |
| db.COLLECTION_NAME.insert(GEN_DOC)                          |
| db.COLLECTION_NAME.find()                                   |
| db.COLLECTION_NAME.show()                                   |
| db.COLLECTION_NAME.replace(QUERY_DOC | GEN_DOC)             |
| db.COLLECTION_NAME.update(QUERY_DOC | UPDATE_DOC)           |
| db.COLLECTION_NAME.remove(QUERY_DOC)                        |
| db.COLLECTION_NAME.aggregate(AGG_DOC)
--------------------------------------------------------------- \n"

let rec loop input =
  (if input = "-exit" then
    let () = Persist.write_env Db.environment in
    exit(0)
  else if input = "-help" then ANSITerminal.(print_string [green] help_msg)
  else if input = "-gen_doc" then ANSITerminal.(print_string [green] "gendoc")
  else if input = "-query_doc" then ANSITerminal.(print_string [green] "querydoc")
  else if input = "-update_doc" then ANSITerminal.(print_string [green] "updatedoc")
else if input = "-agg_doc" then ANSITerminal.(print_string [green] "aggdoc")
  else
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
  );
  print_arrow ();
  let new_input = read_line () in
  loop new_input

