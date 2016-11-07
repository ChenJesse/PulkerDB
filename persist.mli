open YoJson

val doc_to_json: doc -> YoJson.JSON

val json_to_doc : YoJson.JSON -> doc 

val persist : db -> col -> doc -> bool  
