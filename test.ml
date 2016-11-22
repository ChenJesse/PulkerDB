open OUnit2
open Interpreter
open Db

let jsonify res = match res with
  | QueryResponse (_, json) -> print_endline json;parse_json json
  | _ -> failwith ""

(*
use DATABASE_NAME
db.dropDatabase()
db.createCollection(name)
db.COLLECTION_NAME.drop()
db.COLLECTION_NAME.insert(document)
db.COLLECTION_NAME.find()
db.COLLECTION_NAME.update(SELECTION_CRITERIA, UPDATED_DATA)
db.COLLECTION_NAME.remove(DELLETION_CRITERIA)
*)
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
  "replace" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "replace", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.replace({asdf}|{fdas})"));
  "update" >:: (fun _ -> assert_equal (Quad("db", "COLLECTION_NAME", "update", "{asdf}|{fdas}"))
    (tuplize_input "db.COLLECTION_NAME.update({asdf}|{fdas})"));
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
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$lte\": 3}}")));
  "test6" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$lt\": 3}}")));
  "test7" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$gt\": 2}}")));
  "test8" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$gt\": 3}}")));
  "test9" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$ne\": 2}}")));
  "test10" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: 2, c: 3}") (parse_json "{a: 1, b: 2, c: {\"$ne\": 3}}")));
  "test11" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: 5}}")));
  "test12" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: 6}}")));
  "test13" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: 5, d: \"asdf\"}}")));
  "test14" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: 5, d: \"asdff\"}}")));
  "test15" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: {\"$lt\": 6}, d: \"asdf\"}}")));
  "test16" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: {\"$gt\": 6}, d: \"asdf\"}}")));
  "test17" >:: (fun _ -> assert_equal true
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: {\"$lt\": 6}, d: {\"$gt\": \"a\"}}}")));
  "test17" >:: (fun _ -> assert_equal false
    (check_doc (parse_json "{a: 1, b: {c: 5, d: \"asdf\"}}") (parse_json "{b: {c: {\"$ltt\": 6}, d: {\"$gt\": \"a\"}}}")));
]

let empty_db : Persist.db = ref ("test_db", [], false)
let empty_db2 : Persist.db = ref ("test_db2", [], false)
let empty_db3 : Persist.db = ref ("test_db3", [], false)

let test_doc = `Assoc( [("key", `String("value"))] )
let test_col = ref ("test_col", test_doc::[])
let test_db = ref ("test_db", test_col::[], true)
let test_env = ref (test_db::[])

let test_db2 : Persist.db = ref ("test_db2", [], true)
let test_env2 = ref (test_db2::[])

let test_col3 : Persist.col = ref ("test_col3", [])
let test_db3 = ref ("test_db3", test_col3::[], true)
let test_env3 = ref (test_db3::[])

let db_name (name, _, _) = name
let db_cols (_, col_list, _) = col_list
let db_dirty (_, _, dirty) = dirty

let persist_tests = [
  "writes env to disc and reads from it" >:: (fun _ ->
    Persist.write_env test_env;
    assert (Sys.is_directory "test_db");
    Persist.read_db empty_db;
    assert (db_name !empty_db = "test_db");
    assert (db_cols !empty_db <> []);
    assert (db_dirty !empty_db = false);
    let col = !( (db_cols !empty_db) |> List.hd ) in
    assert (fst col = "test_col");
    assert (snd col = [`Assoc([("key", `String("value"))])]);
    Sys.remove "test_db/test_col.json";
    Unix.rmdir "test_db";
    empty_db := "target", ([] : Persist.col list), false
  );
  "empty db" >:: (fun _ ->
    Persist.write_env (test_env2);
    assert (Sys.is_directory "test_db2");
    Persist.read_db empty_db2;
    assert (db_name !empty_db2 = "test_db2");
    assert (db_cols !empty_db2 = []);
    assert (db_dirty !empty_db2 = false);
    Unix.rmdir "test_db2";
    empty_db := "target", ([] : Persist.col list), false
  );
  "empty col" >:: (fun _ ->
    Persist.write_env (test_env3);
    assert (Sys.is_directory "test_db3");
    Persist.read_db empty_db3;
    assert (db_name !empty_db3 = "test_db3");
    assert (db_cols !empty_db3 <> []);
    assert (db_dirty !empty_db3 = false);
    let col = ! ( (db_cols !empty_db3) |> List.hd ) in
    assert (fst col = "test_col3");
    assert (snd col = []);
    Sys.remove "test_db3/test_col3.json";
    Unix.rmdir "test_db3"
  )
]

let end_to_end_tests = [
  "test1" >:: (fun _ -> assert_equal (CreateDBResponse(true, "Success!"))
    (clear_env(); parse "use test"));
  "test2" >:: (fun _ -> assert_equal (CreateDBResponse(false, "Database with same name already exists"))
    (clear_env (); parse "use test"; parse "use test"));
  "test3" >:: (fun _ -> assert_equal (CreateColResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"));
  "test4" >:: (fun _ -> assert_equal (CreateColResponse(false, "c already exists."))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; parse "test.createCollection(c)"));
  "test5" >:: (fun _ -> assert_equal (DropDBResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.dropDatabase()"));
  "test6" >:: (fun _ -> assert_equal (CreateDBResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.dropDatabase()"; parse "use test"));
  "test7" >:: (fun _ -> assert_equal (DropColResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; parse "test.c.drop()"));
  "test8" >:: (fun _ -> assert_equal (CreateColResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; parse "test.c.drop()"; parse "test.createCollection(c)"));
  "test9" >:: (fun _ -> assert_equal (CreateDocResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; parse "test.c.insert({a: 1, b: 2})"));
  "test10" >:: (fun _ -> assert_equal (ShowColResponse(true, "[ { \"a\": 1, \"b\": 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; parse "test.c.insert({a: 1, b: 2})"; parse "test.c.show()"));
  "test11" >:: (fun _ -> assert_equal (CreateDocResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"));
  "test12" >:: (fun _ -> assert_equal (ShowColResponse(true, "[ { \"a\": 5, \"b\": 6 }, { \"a\": 1, \"b\": 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"; parse "test.c.show()"));
  "test13" >:: (fun _ -> assert_equal (QueryResponse(true, "[ { \"a\": 1, \"b\": 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"; 
      parse "test.c.find({b: 2})"));
  "test14" >:: (fun _ -> assert_equal (UpdateColResponse(true, "Success!"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"; 
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5} | {\"$set\": {b: 6}})"));
  "test15" >:: (fun _ -> assert_equal (ShowColResponse(true, "[ { \"a\": 1, \"b\": 2 }, { \"a\": 5, \"b\": 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"; 
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5} | {\"$set\": {b: 2}})"; 
      parse "test.c.show()"));
  "test16" >:: (fun _ -> assert_equal (QueryResponse(true, "[ { \"a\": 1, \"b\": 2 }, { \"a\": 5, \"b\": 2 } ]"))
    (clear_env(); parse "use test"; parse "test.createCollection(c)"; 
      parse "test.c.insert({a: 1, b: 2})"; parse "test.c.insert({a: 5, b: 6})"; 
      parse "test.c.find({b: 2})"; parse "test.c.update({a: 5}|{\"$set\": {b: 2}})"; 
      parse "test.c.find({b: 2})"));
]

let suite =
  "PulkerDB Test Suite"
  >::: interpreter_tests@db_tests@persist_tests@end_to_end_tests

let _ = run_test_tt_main suite
