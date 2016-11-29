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
 open Tree


type doc = Yojson.Basic.json

type indexFile = {idName:string; idTable: (Yojson.Basic.json,Yojson.Basic.json) Hashtbl.t; keys: Yojson.Basic.json list Tree.tree ref}

type indexList = indexFile list

type col = doc list * indexList

type db = (string, col) Hashtbl.t * bool

type catalog = (string, db) Hashtbl.t

exception NotInDisc



(**
 *Compares to JSON files and returns -1 0 or 1 depending on the result.
 *)


val remove_db: string -> unit


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
val write_db: string -> db -> unit

(*
 * Writes a catalog of databases to the hard drive.
 * Each database will have its own directory
 *)
val write_env: catalog -> unit

(*
 * Given a collection name col, a db name db, and a collection ref,
 * saves the collection from db.col in the ref
 *)
val read_collection: string -> string -> col

(*
 * Given a db name and ref, reads the db from disc and saves the db
 * to the ref
 *)
val read_db: string -> db -> unit
