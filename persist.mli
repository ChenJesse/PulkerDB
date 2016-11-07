open Db
open Parser

val doc_to_json: doc -> json

val json_to_doc : json -> doc 

val persist : db -> col -> doc -> bool  
