open Db

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
val parse_json: string -> doc

(**
 * Parses the input from the REPL, and calls the appropriate function
 *)
val parse : string -> response  
