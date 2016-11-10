open Db

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