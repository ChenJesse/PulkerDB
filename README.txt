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
---------------------------------------------------------------

Most of the commands are heavily based around MongoDB.
Tutorial's can be found at https://www.tutorialspoint.com/mongodb/.

------------------------------------------------------------------------------------------------------
COMMAND SUMMARIES
------------------------------------------------------------------------------------------------------
show: Shows all the existing databases.

save: Persists the environment to disk.

usdb: Attempts to create the specified database. Will fail if database already exists with the same name.

usbm: Method for evaluating performance on database of 15000 items.

drdb: Attempts to drop an existing database. Will fail if database does not exist.

dbsh: Shows all the existing collections in the specified database.

ccol: Creates a collection in the specified database. Will fail if a collection already exists with the same name in the database.

drcl: Drops a collection in the specified database. Will fail if collection does not exist.

isrt: Inserts a document in the specified collection, in the specified database.
    Note that duplicate documents are permitted.
    Enter -gen_doc for more information.

find: Searches the specified collection for documents conforming to the query_doc's requirements.
    Note that if a document does not contain one of the specified fields in the query_doc, it will be treated as not satisfying the query_doc.
    Note that a document must have all of the specified fields in the query_doc.
    Note that arrays are unfortunately not supported in querying.
    Enter query_doc for more information.

clsh: Shows all the documents that exist in the collection.

repl: Chains together a remove operation, and an insert operation. Enter -query_doc or -gen_doc for more information.

updt: Will update all the documents retrieved from the query_doc, according to the update_doc.
    Note that if the document does not already have a field to be updated, the field will be created.
    Enter -query_doc or -update_doc for more information.

rmve: Will remove all the documents that satisfy the query_doc. Enter -query_doc for more information.

aggr: Will aggregate the collection based on the agg_doc, and return the information in document form.
    An aggregation operation consists of a group_by phase, and then aggregation operations on one or more fields.
    IMPORTANT: Aggregations are supported only for integers.
    Note that in the case where $sum has no luck aggregating the desired fields, it will return 0.
    Note that in the case where $max has no luck aggregating the desired fields, it will return -4611686018427387904.
    Note that in the case where $min has no luck aggregating the desired fields, it will return 4611686018427387904.
    Enter -agg_doc for more information.

cidx: Given a field that exists in the specified collection, will create an ascending index
    on that field. See -index_doc for proper field formatting.

gidx: Given an index that exists in our collection, and a key that exists for the specified index,
    return the list of JSON's associated with that key.


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
  Example: db.COLLECTION_NAME.find({a: {$lte: 5}}) -s, will store all the results of the query in a json file.

------------------------------------------------------------------------------------------------------
SAMPLE FLOW WALKTHROUGH
------------------------------------------------------------------------------------------------------
::> use test
Output: Database created successfully!
Explanation: No database exists with the same name
------------------------------------------------------------------------------------------------------
::> use test
Output: ERROR: Database with that name already exists
Explanation: Database exists with the same name
-----------------------------------------------------------------------------------------------------
::> test.createCollection(c)
Output: Collection created successfully!
Explanation: No collection exists with the same name in test
------------------------------------------------------------------------------------------------------
::> test.createCollection(c)
Output:  ERROR: c already exists
Explanation: No duplicate collections
------------------------------------------------------------------------------------------------------
::> test.c.insert({a: 1})
Output: Document created successfully!
Explanation: Valid input
------------------------------------------------------------------------------------------------------
::> test.c.insert({a;1})
Output: ERROR: Invalid document provided
Explanation: Invalid formatting of input.
------------------------------------------------------------------------------------------------------
::> test.c.insert({a: "apple"})
Output: Document created successfully!
Explanation: Valid input
------------------------------------------------------------------------------------------------------
::> test.c.createIndex({a: 1})
Output: Index was successfully made!
Explanation: Valid input
------------------------------------------------------------------------------------------------------
::> test.c.insert({a: 2, b: {c: "asdf", d: {e: 100}}})
Output: Document created successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.getIndex({a: 1})
Output: [ { "a": 1 } ]
Explanation: Only doc with field a = 1
------------------------------------------------------------------------------------------------------
::> test.c.createIndex({b: 1})
Output: Index was successfully made!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.getIndex({b: {c: "asdf", d: {e: 100}}})
Output: [ { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } } ]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.createIndex({c: 1})
Output: ERROR: no docs matched the desired field
Explanation: None of the docs have c
------------------------------------------------------------------------------------------------------
::> test.show()
Output: [c]
Explanation: c is the only collection
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output: [
  { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } },
  { "a": "apple" },
  { "a": 1 }
]
Explanation: Shows everything in collection
------------------------------------------------------------------------------------------------------
::> show()
Output: [test]
Explanation: test is the only database
------------------------------------------------------------------------------------------------------
::> test.c.insert({a: true, x: 123})
Output: Document created successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({a: {"$lte": true}})
Output: [
  { "a": "apple" },
  { "a": 1 },
  { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } },
  { "a": true, "x": 123 }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({a:"banana"})
Output: []
Explanation:

------------------------------------------------------------------------------------------------------
::> test.c.insert({a: 2, b: {c: "asdf", d: {e: 200}}})
Output: Document created successfully
Explanation:
-----------------------------------------------------------------------------------------------------
::> test.c.find({b:{c:"asdf"}})
Output:  [
  { "a": 2, "b": { "c": "asdf", "d": { "e": 200 } } },
  { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } }
]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({b:{c:"asdf", d:{e:200}}})
Output: [ { "a": 2, "b": { "c": "asdf", "d": { "e": 200 } } } ]
Explanation:
------------------------------------------------------------------------------------------------------
::>  test.c.insert({a:5})
Output: Document created successfuly
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({a: {"$lt":5}})
Output: [
  { "a": "apple" },
  { "a": 1 },
  { "a": 2, "b": { "c": "asdf", "d": { "e": 200 } } },
  { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } },
  { "a": 2, "b": { "c": "asdf", "d": { "e": 100 } } }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({a: {"$gt":5}})
Output: [ { "a": true, "x": 123 } ]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.find({a: {"$gte": 5}})
Output: [ { "a": true, "x": 123 }, { "a": 5 } ]

Explanation:
-----------------------------------------------------------------------------------------------------
::> test.c.find({a:{"$ne":2}})
Output: [ { "a": "apple" }, { "a": 1 }, { "a": true, "x": 123 }, { "a": 5 } ]

Explanation:
------------------------------------------------------------------------------------------------------
::>test.c.find({a:{"$ne":2}, x:123})
Output:    [ { "a": true, "x": 123 } ]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.remove({a:{"$exists":true}})
Output: Removed document successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output: []
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:2})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:3})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:4})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:5})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.replace({a:{"$lt":3}}|{a:100})
Output: Collection replaced successfully
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output:[ { "a": 100 }, { "a": 5 }, { "a": 4 }, { "a": 3 } ]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.update({a:{"$lt": 100}}|{"$set":{b:2}})
Output: Collection updated successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output: [ { "a": 100 }, { "b": 2, "a": 5 }, { "b": 2, "a": 4 }, { "b": 2, "a": 3 } ]
Explanation:
------------------------------------------------------------------------------------------------------
::>test.c.update({a:{"$exists":true}}|{"$set":{b:"pulkit"}})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output: [
  { "b": "pulkit", "a": 100 },
  { "b": "pulkit", "a": 5 },
  { "b": "pulkit", "a": 4 },
  { "b": "pulkit", "a": 3 }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.update({a:{"$exists":true}}|{"$set":{b:{c:"pulkit"}}})
Output:  Collection updated successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output:[
  { "b": { "c": "pulkit" }, "a": 100 },
  { "b": { "c": "pulkit" }, "a": 5 },
  { "b": { "c": "pulkit" }, "a": 4 },
  { "b": { "c": "pulkit" }, "a": 3 }
]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.update({a:{"$exists":true}}|{"$set":{b:{c:"pulkerton"}}})
Output: Collection updated successfully!
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.show()
Output: [
  { "b": { "c": "pulkerton" }, "a": 100 },
  { "b": { "c": "pulkerton" }, "a": 5 },
  { "b": { "c": "pulkerton" }, "a": 4 },
  { "b": { "c": "pulkerton" }, "a": 3 }
]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.remove({a:{"$exists", true}})
Output: Collection replaced successfully
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:2, b:3})
Output:
Explanation:
-----------------------------------------------------------------------------------------------------
::>test.c.insert({a:1, b:99})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:93939, b:9292})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:1, b:20})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:2, b:4})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.insert({a:93939, b:9291})
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.aggregate({_id:"a", bsum:{"$sum":"b"}})
Output: [
  { "_id": 1, "bsum": 119 },
  { "_id": 2, "bsum": 7 },
  { "_id": 93939, "bsum": 18583 }
]
Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.aggregate({_id:"a", bsum:{"$sum":1}})
Output: [
  { "_id": 1, "bsum": 2 },
  { "_id": 2, "bsum": 2 },
  { "_id": 93939, "bsum": 2 }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.aggregate({_id:"a", bmax:{"$max":b}})
Output: [
  { "_id": 1, "bmax": 99 },
  { "_id": 2, "bmax": 4 },
  { "_id": 93939, "bmax": 9292 }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> test.c.aggregate({_id:"a", bmin:{"$min":"b"}})
Output: [
  { "_id": 1, "bmin": 20 },
  { "_id": 2, "bmin": 3 },
  { "_id": 93939, "bmin": 9291 }
]

Explanation:
------------------------------------------------------------------------------------------------------
::> use benchmark
Output: The time to run query 1 was: 0.016 the time to run query 2 was: 0.
Explanation: The non index using query took 0.016, the index using query took 0 (too fast)
------------------------------------------------------------------------------------------------------
::>
Output:
Explanation:
------------------------------------------------------------------------------------------------------
::>
Output:
Explanation:
------------------------------------------------------------------------------------------------------



