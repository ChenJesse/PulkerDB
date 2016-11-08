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
