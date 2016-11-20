open Yojson.Basic
open Persist

type response = | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | ReplaceDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string
  | ParseErrorResponse of bool * string

val environment : catalog

val get_db_ref: string -> db

val get_col_ref: string -> db -> col

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
 * Given a string representing name of col, creates a col in the environment, within the specified db.
 * On failure, return false. On success, return true.
 *)
val create_col : string -> string -> response

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
val remove_doc : string -> string -> doc -> response

(**
 * Given a string representing name of db and col, creates removes the documents within the col
 * based on the query criteria in the doc, and then inserts the updated_doc.
 * On failure, return false. On success, return true.
 *)
val replace_col : string -> string -> doc -> doc -> response

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

val check_doc : doc -> doc -> bool
