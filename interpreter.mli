open Parser
open Db

type response = | CreateDBResponse of bool 
  | CreateColResponse of bool 
  | CreateDocResponse of bool 
  | RemoveDocResponse of bool 
  | DropDBResponse of bool 
  | DropColResponse of bool 
  | QueryResponse of bool * string

(**
 * Parses the input from the REPL, and calls the appropriate function
 *)
val parse : string -> response  

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_doc : string -> response

(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_db : string -> response

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_col : string -> response

(**
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val remove_doc : string -> response

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_db : string -> response

(**
 * Given a string representing name of col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_col : string -> response

(**
 * Given a string representing a query JSON, looks for matching docs in the environment. 
 * On failure, return false. On success, return true.
 *)
val query : string -> response

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
val parse_json: string -> doc
