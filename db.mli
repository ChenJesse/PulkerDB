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
val store : db -> col -> doc -> bool

(**
 * Gets the document in the appropriate collection, in the
 * appropriate database
 *)
val get : db -> col -> doc -> doc

(**
 * Checks the document in the appropriate collection, in the
 * appropriate database, for existence
 *)
val check : db -> col -> doc -> bool

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val create_doc : string -> string -> string -> response

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
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
val remove_doc : string -> string -> string -> response

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
val query : string -> string -> response
