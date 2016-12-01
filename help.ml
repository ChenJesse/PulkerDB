let help_msg = "
                        REPL COMMANDS
---------------------------------------------------------------
| -exit : Exit the programming gracefully                     |
| -gen_doc : Information on format for ANY_DOC                |
| -query_doc : Information on format for QUERY_DOC            |
| -update_doc : Information on format for UPDATE_DOC          |
| -agg_doc : Information on format for for AGG_DOC            |
| -index_doc : Information on format for for AGG_DOC          |
---------------------------------------------------------------

                      DATABASE COMMANDS
---------------------------------------------------------------
| use DATABASE_NAME                                           |
| db.dropDatabase()                                           |
| db.createCollection(COLLECTION_NAME)                        |
| db.COLLECTION_NAME.drop()                                   |
| db.COLLECTION_NAME.insert(GEN_DOC)                          |
| db.COLLECTION_NAME.find()                                   |
| db.COLLECTION_NAME.show()                                   |
| db.COLLECTION_NAME.replace(QUERY_DOC | GEN_DOC)             |
| db.COLLECTION_NAME.update(QUERY_DOC | UPDATE_DOC)           |
| db.COLLECTION_NAME.remove(QUERY_DOC)                        |
| db.COLLECTION_NAME.aggregate(AGG_DOC)                       |
| db.COLLECTION_NAME.createIndex(INDEX_DOC)                   |
--------------------------------------------------------------- \n"

let gen_doc_msg = 
  "A general document. 
    Has the structure of a JSON (allows recursive nesting, arrays, etc.). Example: {a: 1, b: {c: 3}}"

let query_doc_msg = 
  "A query document. 
    Every general JSON is a query JSON. 
    Also supports comparators, which are: $lte, $lt, $gte, $gt, $ne. 
    Example: {a: {$lt: 5}} looks for any documents with a field \"a\" less than 5.
    Example: {a: 5} looks for any documents with a field \"a\" equal to 5."

let update_doc_msg = 
  "An update document. 
    A general JSON limited to one key value pair. Example: {a: 100}"

let agg_doc_msg = 
  "An aggregation document. 
    A general JSON that must have an \"_id\" field designating the attribute to group by, and pairs of this structure: 
    {x: {y: z}}, where x is the field name you desire to generate, where y is the aggregation method ($sum, $max, or $min), 
    and z is the field name of what you want to aggregate on."

let spacing = "    "

