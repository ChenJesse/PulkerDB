open Repl
open Interpreter
open Db
open Str

let () =
  ANSITerminal.(print_string [red] 
    "\n\nWelcome to PulkerDB, the poor man's MongoDB.\n");
  let first_input = read_line () in
  loop first_input
