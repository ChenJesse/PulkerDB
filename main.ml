open Repl
open Interpreter
open Db
open Str

let () =
  ANSITerminal.(print_string [red] 
    "\n\n     Welcome to PulkerDB, a NoSQL database.     
              Press -help for a list of commands.\n");
  print_arrow ();
  let first_input = read_line () in
  loop first_input
