Instructions:

1) Navigate to directory PulkerDB
2) Enter in terminal `make pulker`
3) To shut down shell gracefully, enter `-exit`
4) Enter `-help` for more info

Feature List
————————————————————————————————————————————————————————————————
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
---------------------------------------------------------------

Sample flow walkthrough:

