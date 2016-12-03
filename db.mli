open Yojson.Basic
open Persist
open Tree

type response = Success of string | Failure of string

val environment : catalog

(**
 * Given a doc, creates a doc in the environment.
 * Also checks all the existing indices on the collection, to see if they
 * need to be updated, and sets dirty bit of the database.
 * requires:
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [doc] is a doc
 *)
val create_doc : string -> string -> doc -> response

(**
 * Given a response with a query as the string, outputs the
 * doc into Output/x.json
 * requires:
 *   - [response] is a Success response with a stringified doc, or Failure
 *)
val persist_query : response -> response

(**
 * Given a string representing name of db, checks for db with same name in 
 * the environment, then checks if db exists in disk, and if so, loads it 
 * into memory. Otherwise, Creates the db.
 * requires:
 *   - [db_name] is a string
 *)
val create_db : string -> response

(**
 * Given a col_name, creates a collection in the specified db if
 * a collection with the same name does not already exist.
 * requires:
 *   - [db_name] is a string
 *   - [col_name] is a string
 *)
val create_col : string -> string -> response

(**
 * Drops anything in the specified collection that satisfies the query_doc. 
 * Changes should persist if the database is saved.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *)
val remove_doc : string -> string -> doc -> response

(**
 * Given a doc representing criteria to query on, removes all appropriate docs,
 * and then inserts the given doc. 
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *   - [update_doc] is a doc conforming to structure specified in help.ml
 *)
val replace_col : string -> string -> doc -> doc -> response

(**
 * Given a doc representing criteria to query on, updates all appropriate docs.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *   - [update_doc] is a doc conforming to structure specified in help.ml
 *)
val update_col : string -> string -> doc -> doc -> response

(**
 * Given a string representing name of db, drops the db in the environment, 
 * and should drop the db in disk if the database is saved.
 *   - [db_name] is a string
 *)
val drop_db : string -> response

(**
 * Given a string representing name of db and col, drops the col 
 * in the environment, and should drop the col in disk if the database is saved.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *)
val drop_col : string -> string -> response

val create_index: string -> string -> string -> Yojson.Basic.json -> response

val get_values: Yojson.Basic.json -> string -> string -> string -> response

(**
 * Returns a response with the stringified doc of all documents in the
 * specified collection.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *)
val show_col: string -> string -> response

(**
 * Returns a response with all the collections in the specified db.
 *   - [db_name] is a string
 *)
val show_db: string -> response

(** 
 * Prints out all the databases that are loaded into memory so far 
 *)
val show_catalog: unit -> response

(**
 * Given a string representing a query JSON, looks for matching docs in
 * the environment under the db and collection.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc, of structure defined in help.ml
 *)
val query_col : string -> string -> doc -> response

(**
 * Given a query_doc and general doc, checks to see if the doc satisfies all
 * the requirements of the query_doc.
 *   - [doc] is a doc
 *   - [query_doc] is a doc, of structure defined in help.ml
 *)
val check_doc : doc -> doc -> bool

(**
 * Handles the aggregation logic on a collection.
 * Example: 
 * db.mycol.aggregate({_id : "$by_user", num_tutorial : {$sum : "$likes"}})
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [agg_doc] is a doc, conforming to structure specified in help.ml
 *)
val aggregate: string -> string -> doc -> response

(**
 * Test only method to reset the entire environment
 *)
val clear_env : unit -> unit
