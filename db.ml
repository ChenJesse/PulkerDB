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

(**
 * Stores the document in the appropriate collection, in the
 * appropriate database
 *)
let store : db -> col -> doc -> bool = failwith "Unimplemented"

(**
 * Gets the document in the appropriate collection, in the
 * appropriate database
 *)
let get : db -> col -> doc -> doc = failwith "Unimplemented"

(**
 * Checks the document in the appropriate collection, in the
 * appropriate database, for existence
 *)
let check : db -> col -> doc -> bool = failwith "Unimplemented"
