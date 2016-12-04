------------------------------------------------------------------------------------------------------
RUNNING INSTRUCTIONS
------------------------------------------------------------------------------------------------------

1) Navigate to directory PulkerDB
2) Enter in terminal `make pulker`
3) To shut down shell gracefully, enter `-exit`
4) Enter `-help` for more info

------------------------------------------------------------------------------------------------------
FEATURE OVERVIEW
------------------------------------------------------------------------------------------------------

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
| show()                                                      |
| save()                                                      |
| use DATABASE_NAME                                           |
| use benchmark                                               |
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
| db.COLLECTION_NAME.getIndex(INDEX_DOC)                      |
---------------------------------------------------------------"

Most of the commands are heavily based around MongoDB. 
Tutorial's can be found at https://www.tutorialspoint.com/mongodb/.

------------------------------------------------------------------------------------------------------
DOCUMENT STURCTURES
------------------------------------------------------------------------------------------------------
GEN_DOC: A general document.
  Has the structure of a JSON (allows recursive nesting, arrays, etc.). Example: {a: 1, b: {c: 3}}

QUERY_DOC: A query document.
  Every general document is a query document.
  Also supports comparators, which are: $lte, $lt, $gte, $gt, $ne.
  Note that on entering the comparators, wrap the keys in quotes, as you would a string.
  Example: {a: {$lt: 5}} looks for any documents with a field "a" less than 5.
  Example: {a: 5} looks for any documents with a field "a" equal to 5.

UPDATE_DOC: An update document.
  A general document of the form {$set: {a}}, where "a" is the field(s) that you wish to update.
  Example: {$set: {a: 100, b: 200}} will set fields "a" to 100 and "b" to 200. If either fields
  do not exist, the attribute will be created.

AGG_DOC: An aggregation document.
  A general document that must have an "_id" field designating the attribute to group by, and pairs of this structure:
  {x: {y: z}}, where x is the field name you desire to generate, where y is the aggregation method ($sum, $max, or $min),
  and z is the field name of what you want to aggregate on.

INDEX_DOC: An index document.
  A document that consists of one key value pair, in the format {field: 1}.
  The field must exist in the collection you are creating the index on. If not, the creation will fail.

INDKEY_DOC: An index-key document.
  Is composed of the form {field: key}, where the field is an attribute you have indexed on in this collection,
  and the key is a value of that attribute for which you want to retrieve all associated documents.
  Example: {h: 2} where h is a field in my collection, and 2 is a value that h takes in certain documents.

store: If you wish to pipe the results of your query to a json file, end your command with the -s flag.
  The appropriate commands to use this with are: find, aggregate, and db.COLLECTION_NAME.show().
  Flagging inappropriate commands will have no effect.
  Example: db.COLLECTION_NAME.find({a: {$lte: 5}}) -s, will store all the results of the query in a json file."

------------------------------------------------------------------------------------------------------
SAMPLE FLOW WALKTHROUGH
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------
>::
Output:
Explanation:
------------------------------------------------------------------------------------------------------


