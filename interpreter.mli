open Db
open Persist
open Models

type tuple = 
  | Nil 
  | Single of string
  | Pair of string * string
  | Triple of string * string * string
  | Quad of string * string * string * string

(**
 * Given an input, parses it into a tuple of 3 or 4 elements
 * based on certain delimiting characters
 *   - [input] is a sanitized input from parse
 *)
val tuplize_input: string -> tuple

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *   - [json_string] is a string
 *)
val parse_json: string -> doc

(**
 * Parses the input from the REPL, and calls the appropriate function, 
 * returning a response
 *   - [input] is a string
 *)
val parse : string -> response  
