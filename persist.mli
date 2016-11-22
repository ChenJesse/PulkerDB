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

type db = (string * col list * bool) ref

type catalog = (db list) ref

exception NotInDisc

(*
 * Writes a collection to a json file.
 * Every document in the collection will be a separate entry in the "documents"
 * field.
 *)
val write_collection: string -> string -> doc list -> unit

(*
 * Writes a database to the hard drive.
 * A directory will be created with the same name as the name of the database.
 * For every collection in the database, a json file with the same name as
 * the collection will be created, holding all the documents in the collection,
 * in the directory.
 *)
val write_db: db -> unit

(*
 * Writes a catalog of databases to the hard drive.
 * Each database will have its own directory
 *)
val write_env: catalog -> unit

(*
 * Given a collection name col, a db name db, and a collection ref,
 * saves the collection from db.col in the ref
 *)
val read_collection: string -> string -> col -> unit

(*
 * Given a db name, reads the db from disc and saves the db
 * to the ref
 *)
val read_db: db -> unit
