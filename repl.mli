open Interpreter

val print_arrow: unit -> unit

(**
 * REPL interface. Parses for help keywords, and otherwise passes the input
 * to Interpreter.parse for further processing.
 * requires:
 *   - [input] is a string, from shell input
 *)
val loop : string -> unit


