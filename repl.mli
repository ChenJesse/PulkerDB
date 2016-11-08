open Interpreter
open Db

(**
 * pipelines data to interpreter, depending on the nature of
 * of the command. Handles communicating with the user
 * by printing results in terminal
 *)
val process: string -> bool


(**
 * serves as primary access point for users to
 * interact with the database. Can query, store, delete, and create
 * databases, collections, documents
 *)
val loop : string -> unit


