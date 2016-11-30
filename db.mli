open Yojson.Basic
open Persist
open Tree

type response =
  | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | ReplaceDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string
  | ParseErrorResponse of bool * string
  | ShowColResponse of bool * string
  | UpdateColResponse of bool * string
  | ShowDBResponse of bool * string
  | ShowCatalogResponse of bool * string
  | AggregateResponse of bool * string
  | CreateIndexResponse of bool * string


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
 * Given a string representing name of db and col, creates updates the documents within the col
 * based on the query criteria in the doc.
 * On failure, return false. On success, return true.
 *)
val update_col : string -> string -> doc -> doc -> response

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

val create_index: string -> string -> string -> Yojson.Basic.json -> response
(**
 * Given strings representing names of db and col, prints the contents of the col.
 * On failure, return false. On success, return true.
 *)
val show_col: string -> string -> response

(**
 * Given a string of a database name, show the collections within the db.
 * On failure, return false. On success, return true.
 *)
val show_db: string -> response

(**
 * Show the dabatases in the catalog. On failure, return false. On success, return true.
 *)
val show_catalog: unit -> response

(**
 * Given a string representing a query JSON, looks for matching docs in the environment.
 * On failure, return false. On success, return true.
 *)
val query_col : string -> string -> doc -> response

val check_doc : doc -> doc -> bool

val aggregate: string -> string -> doc -> response

(**
 * Test only method to reset the entire environment
 *)
val clear_env : unit -> unit
