open Parser

(**
 * Leverages parser to create a doc, given the JSON in string format
 *)
val create_doc : string -> doc 

val create_db : string -> db

val create_col : string -> col
