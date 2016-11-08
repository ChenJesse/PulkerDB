open Parser
open Db

(**
 * Parses the input from the REPL, and calls the appropriate function
 *)
val parse : string -> bool  

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_doc : string -> bool

(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_db : string -> bool

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_col : string -> bool

(**
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val remove_doc : string -> bool

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_db : string -> bool

(**
 * Given a string representing name of col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_col : string -> bool

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
val parse_json: string -> doc
