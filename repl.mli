open Interpreter

val print_arrow: unit -> unit

(**
 * serves as primary access point for users to
 * interact with the database. Can query, store, delete, and create
 * databases, collections, documents
 *)
val loop : string -> unit


