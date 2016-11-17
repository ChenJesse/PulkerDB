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
(* 
let test_doc = `Assoc([("key", `String("value"))])
let test_col = ref ("test_col", test_doc::[])
let test_db = ref ("test_db", test_col::[])
let test_env = ref (test_db::[])

let test_db2 : Db.db = ref ("test_db2", [])
let test_env2 = ref (test_db2::[])

let test_col3 : Db.col = ref ("test_col3", [])
let test_db3 = ref ("test_db3", test_col3::[])
let test_env3 = ref (test_db3::[])

let persist_tests = [
  "writes env to disc and reads from it" >:: (fun _ ->
    Persist.write_env test_env;
    assert (Sys.is_directory "test_db");
    Persist.read_db "test_db";
    let env = !Db.environment in
    assert (env <> []);
    let db = !(List.hd env) in
    assert (fst db = "test_db");
    let col = !( (snd db) |> List.hd ) in
    assert (fst col = "test_col.txt");
    assert (snd col = [`Assoc([("key", `String("value"))])]);
    Sys.remove "test_db/test_col.txt";
    Unix.rmdir "test_db"
  );
  "empty db" >:: (fun _ ->
    Persist.write_env (test_env2);
    assert (Sys.is_directory "test_db2");
    Persist.read_db "test_db2";
    let env = !Db.environment in
    assert (env <> []);
    let db = !(List.hd env) in
    assert (fst db = "test_db2");
    assert (snd db = []);
    Unix.rmdir "test_db2"
  );
  "empty col" >:: (fun _ ->
    Persist.write_env (test_env3);
    assert (Sys.is_directory "test_db3");
    Persist.read_db "test_db3";
    let env = !Db.environment in
    assert (env <> []);
    let db = !(List.hd env) in
    assert (fst db = "test_db3");
    let col = ! ( (snd db) |> List.hd ) in
    assert (fst col = "test_col3.txt");
    assert (snd col = []);
    Sys.remove "test_db3/test_col3.txt";
    Unix.rmdir "test_db3"
  )
]
 *)

let suite =
  "PulkerDB Test Suite"
  >::: interpreter_tests@db_tests

let _ = run_test_tt_main suite
