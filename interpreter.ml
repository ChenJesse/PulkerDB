open Yojson.Basic
open Parser
open Db

(**
 * Parses the input from the REPL, and calls the appropriate function
 *)
let parse input = failwith "Unimplemented"

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_doc json_string = failwith "Unimplemented"

(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_db name = failwith "Unimplemented"

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_col name = failwith "Unimplemented"

(**
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let remove_doc name = failwith "Unimplemented"

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_db name = failwith "Unimplemented"

(**
 * Given a string representing name of col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_col name = failwith "Unimplemented"

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
let parse_json json_string = from_string json_string
