
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
