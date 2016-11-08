let process input =
  Str.string_match (Str.regexp "use ") filename (0)

let help_msg = ""

let rec loop input =
  match input with
    | "exit" -> exit(0)
    | "help" -> print_endline help_msg
    | x ->
      (if process x then print_endline "Operation succeeded"
      else print_endline "Operation failed");
      let new_input = read_line () in
      loop new_input
