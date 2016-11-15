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
  "useDB" >:: (fun _ -> assert_equal (Db.CreateDBResponse (true, "Success!"))
    (parse "use test"));
  "dropDB1" >:: (fun _ -> assert_equal (Db.DropDBResponse (false, "test does not exist."))
    (parse "test.dropDatabase()"));
  "dropDB2" >:: (fun _ -> assert_equal (Db.DropDBResponse (true, "Success!"))
    (parse "use test"; parse "test.dropDatabase()"));
  "createCol1" >:: (fun _ -> assert_equal (Db.CreateColResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"));
  "createCol2" >:: (fun _ -> assert_equal (Db.CreateColResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.createCollection(testCol2)"));
  "createCol3" >:: (fun _ -> assert_equal (Db.CreateColResponse (false, "testcol2 already exists."))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.createCollection(testCol2)"; parse "test.createCollection(testCol2)"));
  "dropCol1" >:: (fun _ -> assert_equal (Db.DropColResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.testCol.drop()"));
  "dropCol2" >:: (fun _ -> assert_equal (Db.DropColResponse (false, "asdf does not exist."))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.asdf.drop()"));
  "createDoc1" >:: (fun _ -> assert_equal (Db.CreateDocResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.testCol.insert({})"));
  "createDoc2" >:: (fun _ -> assert_equal (Db.CreateDocResponse (false, "asdf was not found."))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "asdf.testCol.insert({})"));
  "createDoc3" >:: (fun _ -> assert_equal (Db.CreateDocResponse (false, "fdsa was not found."))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.fdsa.insert({})"));
  "createDoc4" >:: (fun _ -> assert_equal (Db.CreateDocResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"; parse "test.testCol.insert({a: 1, b: 2, c: 3})"));
  "createDoc4" >:: (fun _ -> assert_equal (Db.CreateDocResponse (true, "Success!"))
    (parse "use test"; parse "test.createCollection(testCol)"; 
    parse "test.testCol.insert({a: 1, b: 2, c: 3})"; parse "test.testCol.insert({a: {b: {c: 5}}})"));
(*   "find1" >:: (fun _ -> assert_equal (parse_json "[ { \"a\": 1, \"b\": 2, \"c\": 3 } ]")
    (parse "use test"; parse "test.createCollection(testCol)"; 
    parse "test.testCol.insert({a: 1, b: 2, c: 3})"; 
    parse "test.testCol.insert({a: {b: {c: 3}}, b: 2})"; 
    parse "test.testCol.find({a: 1})" |> jsonify)); *)
]

let db_tests = [

]

let persist_tests = [
  
]


let suite =
  "PulkerDB Test Suite"
  >::: interpreter_tests

let _ = run_test_tt_main suite
