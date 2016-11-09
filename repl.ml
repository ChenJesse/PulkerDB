open Interpreter

let help_msg = ""

let rec loop input =
  (if input = "exit" then exit(0)
  else if input = "help" then print_endline help_msg
  else match parse input with
    | CreateDBResponse (x, msg) ->
      if x then print_endline "Successfully created database"
      else print_endline "Failed to create database"
    | CreateColResponse (x, msg) ->
      if x then print_endline "Successfully created collection"
      else print_endline "Failed to create collection"
    | CreateDocResponse (x, msg) ->
      if x then print_endline "Successfully created document"
      else print_endline "Failed to create document"
    | RemoveDocResponse (x, msg) ->
      if x then print_endline "Successfully removed document"
      else print_endline "Failed to remove document"
    | DropDBResponse (x, msg) ->
      if x then print_endline "Successfully dropped database"
      else print_endline "Failed to drop database"
    | DropColResponse (x, msg) ->
      if x then print_endline "Succesfully dropped collection"
      else print_endline "Failed to drop collection"
    | QueryResponse (x, output) ->
      if x then print_endline output
      else print_endline "Query failed"
  );
  let new_input = read_line () in
  loop new_input

