let help_msg = "
                        REPL COMMANDS
---------------------------------------------------------------
| -exit : Exit the programming gracefully                     |
| -gen_doc : Information on format for ANY_DOC                |
| -query_doc : Information on format for QUERY_DOC            |
| -update_doc : Information on format for UPDATE_DOC          |
| -agg_doc : Information on format for AGG_DOC                |
| -index_doc : Information on format for INDEX_DOC            |
| -indkey_doc : Information on format for INDKEY_DOC          |
| -store : Information on the store flag                      |
---------------------------------------------------------------

                      DATABASE COMMANDS
---------------------------------------------------------------
|                     COMMAND                       |   INFO  |
---------------------------------------------------------------
| show()                                            |  -show  |
| save()                                            |  -save  |
| use DATABASE_NAME                                 |  -usdb  |
| use benchmark                                     |  -usbm  |
| db.dropDatabase()                                 |  -drdb  |
| db.show()                                         |  -dbsh  |
| db.createCollection(COLLECTION_NAME)              |  -ccol  |
| db.COLLECTION_NAME.drop()                         |  -drcl  |
| db.COLLECTION_NAME.insert(GEN_DOC)                |  -isrt  |
| db.COLLECTION_NAME.find(QUERY_DOC)                |  -find  |
| db.COLLECTION_NAME.show()                         |  -clsh  |
| db.COLLECTION_NAME.replace(QUERY_DOC | GEN_DOC)   |  -repl  |
| db.COLLECTION_NAME.update(QUERY_DOC | UPDATE_DOC) |  -updt  |
| db.COLLECTION_NAME.remove(QUERY_DOC)              |  -rmve  |
| db.COLLECTION_NAME.aggregate(AGG_DOC)             |  -aggr  |
| db.COLLECTION_NAME.createIndex(INDEX_DOC)         |  -cidx  |
| db.COLLECTION_NAME.getIndex(INDEX_DOC)            |  -gidx  |
--------------------------------------------------------------- \n"

let show_msg =
  "Shows all the existing databases."

let save_msg =
  "Persists the environment to disk."

let usdb_msg =
  "Attempts to create the specified database. Will fail if database already exists with the same name."

let usbm_msg = "Method for evaluating performance on database of 15000 items."

let drdb_msg =
  "Attempts to drop an existing database. Will fail if database does not exist."

let dbsh_msg =
  "Shows all the existing collections in the specified database."

let ccol_msg =
  "Creates a collection in the specified database. Will fail if a collection already exists with the same name in the database."

let drcl_msg =
  "Drops a collection in the specified database. Will fail if collection does not exist."

let isrt_msg =
  "Inserts a document in the specified collection, in the specified database.
    Note that duplicate documents are permitted.
    Enter -gen_doc for more information."

let find_msg =
  "Searches the specified collection for documents conforming to the query_doc's requirements.
    Note that if a document does not contain one of the specified fields in the query_doc, it will be treated as not satisfying the query_doc.
    Note that a document must have all of the specified fields in the query_doc.
    Note that arrays are unfortunately not supported in querying.
    Enter query_doc for more information."

let clsh_msg =
  "Shows all the documents that exist in the collection."

let repl_msg =
  "Chains together a remove operation, and an insert operation. Enter -query_doc or -gen_doc for more information."

let updt_msg =
  "Will update all the documents retrieved from the query_doc, according to the update_doc.
    Note that if the document does not already have a field to be updated, the field will be created.
    Enter -query_doc or -update_doc for more information."

let rmve_msg =
  "Will remove all the documents that satisfy the query_doc. Enter -query_doc for more information."

let aggr_msg =
  "Will aggregate the collection based on the agg_doc, and return the information in document form.
    An aggregation operation consists of a group_by phase, and then aggregation operations on one or more fields.
    IMPORTANT: Aggregations are supported only for integers.
    Note that in the case where _sum has no luck aggregating the desired fields, it will return 0.
    Note that in the case where _max has no luck aggregating the desired fields, it will return -4611686018427387904.
    Note that in the case where _min has no luck aggregating the desired fields, it will return 4611686018427387904.
    Enter -agg_doc for more information."

let cidx_msg =
  "Given a field that exists in the specified collection, will create an ascending index
    on that field. See -index_doc for proper field formatting."

let gidx_msg =
  "Given an index that exists in our collection, and a key that exists for the specified index,
    return the list of JSON's associated with that key."

let gen_doc_msg =
  "A general document.
    Has the structure of a JSON (allows recursive nesting, arrays, etc.). Example: {a: 1, b: {c: 3}}"

let query_doc_msg =
  "A query document.
    Every general document is a query document.
    Also supports comparators, which are: _lte, _lt, _gte, _gt, _ne.
    Note that on entering the comparators, wrap the keys in quotes, as you would a string.
    Example: {a: {_lt: 5}} looks for any documents with a field \"a\" less than 5.
    Example: {a: 5} looks for any documents with a field \"a\" equal to 5."

let update_doc_msg =
  "An update document.
    A general document of the form {_set: {a}}, where \"a\" is the field(s) that you wish to update.
    Example: {_set: {a: 100, b: 200}} will set fields \"a\" to 100 and \"b\" to 200. If either fields
    do not exist, the attribute will be created."

let agg_doc_msg =
  "An aggregation document.
    A general document that must have an \"_id\" field with a string value designating the attribute to group by,
    and pairs of this structure: {x: {y: z}}, where x is the field name you desire to generate, where y is the aggregation method (_sum, _max, or _min),
    and z is the field name of what you want to aggregate on."

let index_doc_msg =
  "An index document.
    A document that consists of one key value pair, in the format {field: 1}.
    The field must exist in the collection you are creating the index on. If not, the creation will fail."

let indkey_doc_msg =
  "An index-key document.
    Is composed of the form {field: key}, where the field is an attribute you have indexed on in this collection,
    and the key is a value of that attribute for which you want to retrieve all associated documents.
    Example: {h: 2} where h is a field in my collection, and 2 is a value that h takes in certain documents."

let store_msg =
  "If you wish to pipe the results of your query to a json file, end your command with the -s flag.
    The appropriate commands to use this with are: find, aggregate, and db.COLLECTION_NAME.show().
    Flagging inappropriate commands will have no effect.
    Example: db.COLLECTION_NAME.find({a: {_lte: 5}}) -s, will store all the results of the query in a json file."

let spacing = "    "

let exiting_msg = "Persisting your changes, existing gracefully..."

