open Yojson.Basic

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

type col = (string * doc list) ref

type db = (string * col list) ref

type catalog = (db list) ref

type response = | CreateDBResponse of bool 
  | CreateColResponse of bool 
  | CreateDocResponse of bool 
  | RemoveDocResponse of bool 
  | DropDBResponse of bool 
  | DropColResponse of bool 
  | QueryResponse of bool * string

(**
 * Stores the document in the appropriate collection, in the
 * appropriate database
 *)
let store db col doc = failwith "Unimplemented"

(**
 * Gets the document in the appropriate collection, in the
 * appropriate database
 *)
let get db col doc = failwith "Unimplemented"

(**
 * Checks the document in the appropriate collection, in the
 * appropriate database, for existence
 *)
let check db col doc = failwith "Unimplemented"

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_doc db col doc = failwith "Unimplemented"

(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_db db = failwith "Unimplemented"

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_col db col = failwith "Unimplemented"

(**
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let remove_doc db col doc = failwith "Unimplemented"

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_db db = failwith "Unimplemented"

(**
 * Given a string representing name of col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_col db col = failwith "Unimplemented"

(**
 * Given a string representing a query JSON, looks for matching docs in the environment. 
 * On failure, return false. On success, return true.
 *)
let query json_string = failwith "Unimplemented"
