open OUnit2
open Interpreter
open Db
open Yojson.Basic
open Models

let interpreter_tests = [
  "dropDB" >:: (fun _ -> assert_equal (Triple("db", "dropDatabase", ""))
    (tuplize_input "db.dropDatabase()"));
  "createCol" >:: (fun _ -> assert_equal (Triple("db", "createCollection", "name"))
    (tuplize_input "db.createCollection(name)"));
  "dropCol" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "drop", ""))
    (tuplize_input "db.COLLECTION_NAME.drop()"));
  "insertDoc" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "drop", ""))
    (tuplize_input "db.COLLECTION_NAME.drop()"));
  "find" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "find", ""))
    (tuplize_input "db.COLLECTION_NAME.find()"));
  "remove" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "remove", "{asdf}"))
    (tuplize_input "db.COLLECTION_NAME.remove({asdf})"));
  "replace" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "replace", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.replace({asdf}|{fdas})"));
  "update" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "update", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.update({asdf}|{fdas})"));
  "index" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "createIndex", "{a:1}"))
    (tuplize_input "db.COLLECTION_NAME.createIndex({a:1})"));
  "dropDB_flagged" >:: (fun _ -> assert_equal (Triple("db", "dropDatabase", ""))
    (tuplize_input "db.dropDatabase() -s"));
  "createCol_flagged" >:: (fun _ ->
     assert_equal (Triple("db", "createCollection", "name"))
    (tuplize_input "db.createCollection(name) -s"));
  "dropCol_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "drop", ""))
    (tuplize_input "db.COLLECTION_NAME.drop() -s"));
  "insertDoc_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "drop", ""))
    (tuplize_input "db.COLLECTION_NAME.drop() -s"));
  "find_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "find", ""))
    (tuplize_input "db.COLLECTION_NAME.find() -s"));
  "remove_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "remove", "{asdf}"))
    (tuplize_input "db.COLLECTION_NAME.remove({asdf}) -s"));
  "replace_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "replace", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.replace({asdf}|{fdas}) -s"));
  "update_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "update", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.update({asdf}|{fdas}) -s"));
  "index_flagged" >:: (fun _ ->
     assert_equal (Quad("db", "COLLECTION_NAME", "createIndex", "{a:1}"))
    (tuplize_input "db.COLLECTION_NAME.createIndex({a:1}) -s"));
]

let db_tests = [
  "test1" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1}")));
  "test2" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 0}")));
  "test3" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: 3}")));
  "test4" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: 2}")));
  "test5" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_lte: 3}}")));
  "test6" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_lt: 3}}")));
  "test7" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_gt: 2}}")));
  "test8" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_gt: 3}}")));
  "test9" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_ne: 2}}")));
  "test10" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}")
    (parse_json "{a: 1, b: 2, c: {_ne: 3}}")));
  "test11" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
    (parse_json "{b: {c: 5}}")));
  "test12" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: 6}}")));
  "test13" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: 5, d: \"asdf\"}}")));
  "test14" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: 5, d: \"asdff\"}}")));
  "test15" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: {_lt: 6}, d: \"asdf\"}}")));
  "test16" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: {_gt: 6}, d: \"asdf\"}}")));
  "test17" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: {_lt: 6}, d: {_gt: \"a\"}}}")));
  "test17" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}")
      (parse_json "{b: {c: {\"_ltt\": 6}, d: {_gt: \"a\"}}}")));
]

let empty_db : db = ((Hashtbl.create 5), true)
let empty_db2 : db = ((Hashtbl.create 5), true)
let empty_db3 : db = ((Hashtbl.create 5), true)

let test_doc = `Assoc( [("key", `String("value"))] )
let test_col : col = (test_doc::[],[])
let test_db : db = ((Hashtbl.create 5), true)
let () = Hashtbl.add (fst test_db) "test_col" test_col
let test_env = Hashtbl.create 5
let () = Hashtbl.add (test_env) "test_db" test_db

let test_db2 : db = ((Hashtbl.create 5), true)
let test_env2 = Hashtbl.create 5
let () = Hashtbl.add test_env2 "test_db2" test_db2

let test_col3 : col = ([],[])
let test_db3 = ((Hashtbl.create 5), true)
let () = Hashtbl.add (fst test_db3) "test_col3" test_col3
let test_env3 = Hashtbl.create 5
let () = Hashtbl.add test_env3 "test_db3" test_db3

let persist_tests = [
  "writes env to disc and reads from it" >:: (fun _ ->
    Persist.write_env test_env;
    assert (Sys.is_directory "Persist");
    assert (Sys.is_directory "Persist/test_db");
    Persist.read_db "test_db" empty_db;
    assert_equal (Hashtbl.find (fst empty_db) "test_col")
                 ([`Assoc([("key", `String("value"))])], []);
    Sys.remove "Persist/test_db/test_col.json";
    Unix.rmdir "Persist/test_db";
  );
  "empty db" >:: (fun _ ->
    Persist.write_env (test_env2);
    assert (Sys.is_directory "Persist");
    assert (Sys.is_directory "Persist/test_db2");
    Persist.read_db "test_db2" empty_db2;
    assert_equal (Hashtbl.length (fst empty_db2)) 0;
    Unix.rmdir "Persist/test_db2";
  );
  "empty col" >:: (fun _ ->
    Persist.write_env (test_env3);
    assert (Sys.is_directory "Persist/test_db3");
    Persist.read_db "test_db3" empty_db3;
    assert_equal (Hashtbl.find (fst empty_db3) "test_col3") ([],[]);
    Sys.remove "Persist/test_db3/test_col3.json";
    Unix.rmdir "Persist/test_db3";
    Unix.rmdir "Persist"
  )
]

let json_printer str = parse_json str |> pretty_to_string

let end_to_end_tests = [
  "test1" >:: (fun _ -> assert_equal (Success "Database created successfully!")
    (clear_env(); parse "use test"));
  "test2" >:: (fun _ -> assert_equal (Failure "Database with same name already exists")
    (clear_env (); parse "use test"; parse "use test"));
  "test3" >:: (fun _ -> assert_equal (Success "Collection created successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)"));
  "test4" >:: (fun _ -> assert_equal (Failure "c already exists.")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.createCollection(c)"));
  "test5" >:: (fun _ -> assert_equal (Success "Dropped database successfully!")
    (clear_env(); parse "use test"; parse "test.dropDatabase()"));
  "test6" >:: (fun _ -> assert_equal (Success "Database created successfully!")
    (clear_env(); parse "use test"; parse "test.dropDatabase()"; parse "use test"));
  "test7" >:: (fun _ -> assert_equal (Success "Dropped collection successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.drop()"));
  "test8" >:: (fun _ -> assert_equal (Success "Collection created successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.drop()"; parse "test.createCollection(c)"));
  "test9" >:: (fun _ -> assert_equal (Success "Document created successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"));
  "test9I" >:: (fun _ -> assert_equal (Success "Index was successfully made!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.createIndex({a:1})"));
  "test9IB" >:: (fun _ -> assert_equal (Failure "No documents matched the desired field.")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.createIndex({c:1})"));
  "test10" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.show()"));
  "test11" >:: (fun _ -> assert_equal (Success "Document created successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"));
  "test11I" >:: (fun _ -> assert_equal (Success "Index was successfully made!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.createIndex({a:1})"));
  "test12" >:: (fun _ ->
    assert_equal (Success "[ { \"a\": 5, \"b\": 6 }, { \"a\": 1, \"b\": 2 } ]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.show()"));
  "test13" >:: (fun _ -> assert_equal (Success "[ { \"a\": 1, \"b\": 2 } ]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"));
  "test13I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.createIndex({a:1})"; parse "test.c.find({b: 2})"));
  "test13I_2" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.createIndex({b:1})"; parse "test.c.find({b: 2})"));
  "test14" >:: (fun _ -> assert_equal (Success "Collection updated successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5} | {_set: {b: 6}})"));
  "test14I" >:: (fun _ -> assert_equal (Success "Collection updated successfully!")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.createIndex({b:1})"; parse "test.c.find({b: 2})";
      parse "test.c.update({a: 5} | {_set: {b: 6}})"));
  "test15" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1, b: 2}, {a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5} | {_set: {b: 2}})";
      parse "test.c.show()"));
  "test16" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1, b: 2}, {a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({b: 2})"));
  "test16I" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1, b: 2}, {a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.createIndex({b:1})"; parse "test.c.find({b: 2})";
      parse "test.c.update({a: 5}|{_set: {b: 2}})"; parse "test.c.find({b: 2})"));
  "test16I_2" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1, b: 2}, {a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";   parse "test.c.createIndex({b:1})";
      parse "test.c.insert({a: 5, b: 6})"; parse "test.c.find({b: 2})";
      parse "test.c.update({a: 5}|{_set: {b: 2}})"; parse "test.c.find({b: 2})"));
  "test17" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({b: {_gt: 2}})"));
  "test17I" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.createIndex({b:1})"; parse "test.c.find({b: {_gt: 2}})"));
  "test18" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1, b: 2}, {a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({b: {_gte: 2}})"));
  "test18I" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.createIndex({b:1})"; parse "test.c.find({b: {_gte: 3}})"));
  "test19I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.createIndex({a:1})"; parse "test.c.find({a: {_gt: 3}})"));
  "test19" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 5, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({a: {_gt: 3}})"));
  "test20" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({a: {_lt: 3}})"));
  "test20I" >:: (fun _ -> assert_equal (Success (json_printer "[ {a: 1, b: 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{_set: {b: 2}})";
      parse "test.c.find({a: {_lt: 3}})"));
  "test21" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: {b: {c: 2}}}, {a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.show()"));
  "test21I" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: {b: {c: 2}}}, {a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.createIndex({a:1})";
      parse "test.c.insert({a: {b: {c: 2}}})";parse "test.c.show()"));
  "test22" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: {b: {c: 2}}}, {a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.show()"));
  "test22I" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: {b: {c: 2}}}, {a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.show()"));
  "test23" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: 2}}})"));
  "test23I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.createIndex({b:1})"; parse "test.c.createIndex({a:1})";
      parse "test.c.find({a: {b: {c: 2}}})"));
  "test23I_2" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.createIndex({a:1})"; parse "test.c.find({a: {b: {c: 2}}})"));
  "test24" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: 3}}})"));
  "test24I" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.createIndex({a:1})"; parse "test.c.find({a: {b: {c: 3}}})"));
  "test25" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: {_lt: 3}}}})"));
  "test25I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.createIndex({a:1})";
      parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: {_lt: 3}}}})"));
  "test26" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: {_lt: 2}}}})"));
  "test26I" >:: (fun _ -> assert_equal (Success "[]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; "test.c.createIndex({a:1})";
      parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: {_lt: 2}}}})"));
  "test27" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.find({a: {b: {c: {_gt: 1}}}})"));
  "test27I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      "test.c.createIndex({a:1})"; parse "test.c.find({a: {b: {c: {_gt: 1}}}})"));
  "test28" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})"; parse "test.c.find({a: 1, b: 2})"));
  "test28I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.createIndex({a:1})"; parse "test.c.insert({a: 1, b: 3})";
      parse "test.c.find({a: 1, b: 2})"));
  "test29" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})";
      parse "test.c.update({a: 1}|{_set: {b: 100}})"; parse "test.c.remove({a: 1})";
      parse "test.c.show()"));
  "test29I" >:: (fun _ -> assert_equal (Success (json_printer "[{a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})";  parse "test.c.createIndex({a:1})";
      parse "test.c.update({a: 1}|{_set: {b: 100}})";
      parse "test.c.remove({a: 1})"; parse "test.c.show()"));
  "test30" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 1234}, {a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})";
      parse "test.c.update({a: 1}|{_set: {b: 100}})";
      parse "test.c.replace({a: 1}| {a: 1234})"; parse "test.c.show()"));
  "test30I" >:: (fun _ ->
    assert_equal  (Success (json_printer "[{a: 1234}, {a: {b: {c: 2}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})";
      parse "test.c.update({a: 1}|{_set: {b: 100}})";
      parse "test.c.replace({a: 1}| {a: 1234})"; parse "test.c.createIndex({b:1})";
      parse "test.c.show()"));
  "test31" >:: (fun _ ->
    assert_equal
    (Success (json_printer "[{_id: 1, asdf: 101}, {_id: 2, asdf: 1000}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}})"));
  "test31I" >:: (fun _ ->
    assert_equal (Success (json_printer "[{_id: 1, asdf: 101}, {_id: 2, asdf: 1000}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})"; parse "test.c.createIndex({a:1})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}})"));
  "test32I" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{_id: 1, asdf: 101, fdsa: 999999}, {_id: 2, asdf: 1000, fdsa: 999}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100, c: 9})";
      parse "test.c.insert({a: 2, b: 1000, c: 999})";
      parse "test.c.insert({a: 1, b: 1, c: 999999})"; parse "test.c.createIndex({a:1})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}, fdsa: {\"_max\": \"c\"}})"));
  "test32" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{_id: 1, asdf: 101, fdsa: 999999}, {_id: 2, asdf: 1000, fdsa: 999}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100, c: 9})";
      parse "test.c.insert({a: 2, b: 1000, c: 999})";
      parse "test.c.insert({a: 1, b: 1, c: 999999})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}, fdsa: {\"_max\": \"c\"}})"));
  "test33I" >:: (fun _ ->
    assert_equal (Success (json_printer "[{_id: 1, asdf: 2}, {_id: 2, asdf: 1}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})"; parse "test.c.createIndex({a:1})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": 1}})"));
  "test33" >:: (fun _ ->
    assert_equal (Success (json_printer "[{_id: 1, asdf: 2}, {_id: 2, asdf: 1}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})";
      parse "test.c.insert({a: 2, b: 1000})"; parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": 1}})"));
  "test34" >:: (fun _ ->
    assert_equal (Success (json_printer "[{c: 100, a: 5, b: 6}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.update({a: 5}|{_set: {c: 100}})";
      parse "test.c.find({b: 6})"));
  "test35" >:: (fun _ ->
    assert_equal (Success (json_printer "[{c: {d: 100}, a: 5, b: 6}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})";
      parse "test.c.update({a: 5}|{_set: {c: {d: 100}}})";
      parse "test.c.find({b: 6})"));
  "test36" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 5, b: {c: {d: 100}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5}}})";
      parse "test.c.update({a: 5}|{_set: {b: {c: {d: 100}}}})";
      parse "test.c.find({a: 5})"));
  "test37" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{e: {f: {g: 100}}, a: 5, b: {c: {d: 5}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5}}})";
      parse "test.c.update({a: 5}|{_set: {e: {f: {g: 100}}}})";
      parse "test.c.find({a: 5})"));
  "test38" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 5, b: {c: {d: 5, e: 120}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5, e: 12}}})";
      parse "test.c.update({a: 5}|{_set:{b: {c: {e: 120}}}})";
      parse "test.c.find({a: 5})"));
  "test39" >:: (fun _ ->
    assert_equal (Success (json_printer "[{a: 5, b: {c: {d: 5, e: {z: \"asdf\"}}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5, e: 12}}})";
      parse "test.c.update({a: 5}|{_set:{b: {c: {e: {z: \"asdf\"}}}}})";
      parse "test.c.find({a: 5})"));
  "test40" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{h: 1234, a: 5, b: {c: {d: 5, e: {z: \"asdf\"}}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5, e: 12}}})";
      parse "test.c.update({a: 5}|{_set:{b: {c: {e: {z: \"asdf\"}}}, h: 1234}})";
      parse "test.c.find({a: 5})"));
  "test40a" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{a: 5, b: \"asdf\"}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5, e: 12}}})";
      parse "test.c.update({a: 5}|{_set:{b: \"asdf\"}})";
      parse "test.c.find({a: 5})"));
  "test41" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{h: 1234, a: 6, b: {c: {d: 5, e: {z: \"asdf\"}}}}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})";
      parse "test.c.insert({a: 5, b: {c: {d: 5, e: 12}}})";
      parse "test.c.update({a: 5}|{_set:{b: {c: {e: {z: \"asdf\"}}}, h: 1234, a: 6}})";
      parse "test.c.find({a: 6})"));
  "test34Remove1" >:: (fun _-> assert_equal
    (Success (json_printer "[{a: 2, b: 1000}]"))
     (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})";
      parse "test.c.remove({a:1})"; parse "test.c.getindex({a:2})"));
  "test34Remove2" >:: (fun _-> assert_equal (Success "[ null ]")
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})"; parse "test.c.remove({a:1})";
      parse "test.c.getindex({a:1})"));
  "test34Remove3" >:: (fun _-> assert_equal (Success (json_printer "[{a: 1, b: 100}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})"; parse "test.c.remove({b:1})";
      parse "test.c.getindex({a:1})"));
 "test34Remove3" >:: (fun _-> assert_equal (Success (json_printer "[{a: 1, b: 100}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})"; parse "test.c.remove({b:1})";
      parse "test.c.getindex({a:1})"));
 "test34Remove4" >:: (fun _-> assert_equal (Success (json_printer "[{a: 1, b: 2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})"; parse "test.c.remove({a:1})";
      parse "test.c.insert({a:1, b:2})"; parse "test.c.getindex({a:1})"));
 "test34Remove4" >:: (fun _-> assert_equal (Success (json_printer "[{a: 2, b: 25}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100})"; parse "test.c.insert({a: 2, b: 1000})";
      parse "test.c.insert({a: 1, b: 1})";
      parse "test.c.createIndex({a:1})"; parse "test.c.remove({a:2})";
      parse "test.c.insert({a:2, b:25})"; parse "test.c.getindex({a:2})"));
 "test35Update1" >::  (fun _ ->
  assert_equal (Success (json_printer "[{a: 2, b: 3}, {a:2, b:2}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})"; parse "test.c.createIndex({a:1})";
      parse "test.c.update({a: 1}|{_set: {a: 2}})";
      parse "test.c.remove({a: 1})"; parse "test.c.getindex({a: 2})"));
 "test36Replace1" >:: (fun _ -> assert_equal (Success (json_printer "[{a: 1234}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: {b: {c: 2}}})";
      parse "test.c.insert({a: 1, b: 3})"; parse "test.c.createIndex({a:1})";
      parse "test.c.update({a: 1}|{_set: {b: 100}})";
      parse "test.c.replace({a: 1}| {a: 1234})";
      parse "test.c.getindex({a:1234})"));
 "test100" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{_id: 1, asdf: 101, fdsa: -4611686018427387904}, 
        {_id: 2, asdf: 1000, fdsa: -4611686018427387904}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100, c: [9]})";
      parse "test.c.insert({a: 2, b: 1000, c: [999]})";
      parse "test.c.insert({a: 1, b: 1, c: [999999]})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}, 
        fdsa: {\"_max\": \"c\"}})"));
 "test101" >:: (fun _ ->
    assert_equal (Success
      (json_printer "[{_id: 1, asdf: 101, fdsa: 0}, 
        {_id: 2, asdf: 1000, fdsa: 0}]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)";
      parse "test.c.insert({a: 1, b: 100, c: [9]})";
      parse "test.c.insert({a: 2, b: 1000, c: [999]})";
      parse "test.c.insert({a: 1, b: 1, c: [999999]})";
      parse "test.c.aggregate({_id: \"a\", asdf: {\"_sum\": \"b\"}, 
        fdsa: {\"_sum\": \"c\"}})"));
]

let suite =
  "PulkerDB Test Suite"
  >::: interpreter_tests@db_tests@persist_tests@end_to_end_tests

let _ = run_test_tt_main suite
