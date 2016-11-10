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

type response = | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string

val environment : catalog

(**
 * Given a doc, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_doc : string -> string -> doc -> response

(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_db : string -> response

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_col : string -> string -> response

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment. 
 * On failure, return false. On success, return true.
 *)
val remove_doc : string -> string -> doc -> response

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_db : string -> response

(**
 * Given strings representing names of db and col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
val drop_col : string -> string -> response

(**
 * Given a string representing a query JSON, looks for matching docs in the environment. 
 * On failure, return false. On success, return true.
 *)
val query_col : string -> string -> doc -> response
