open Yojson.Basic

type db = string

type col = string

(**
 * Taken from Yojson.Basic documentation:
 * type json = [ `Assoc of (string * json) list
 *             | `Bool of bool
 *             | `Float of float
 *             | `Int of int
 *             | `List of json list
 *             | `Null
 *             | `String of string ] 
 *)
type doc = Yojson.Basic.json

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
let parse json_string = from_string json_string
