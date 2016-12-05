open Models

exception NotInDisc

(* Query outputs will be stored as (0...n).json *)
let output_name = ref 0

(* Creates a directory called name unless it exists *)
let create_dir name =
  try Unix.mkdir name 0o777 with
  | _ -> ()

(* Given a json representing the output of a query,
 * writes it to the Output folder *)
let write_query_json json =
  create_dir "Output";
  let filename = string_of_int (!output_name) in
  output_name := !output_name + 1;
  let filepath = "Output/" ^ filename ^ ".json" in
  Yojson.Basic.to_file filepath json;
  filepath

(* [col filename] Given a filename, returns true if filename is a
 * collection ie ends with .json file extension
 * requires:
 *   - [filename] is a string
 *)
let col filename =
  let len = String.length filename in
  if len < 5 then
    false
  else
    Str.string_match (Str.regexp ".json$") filename (len-5)

(* Applies fx to every collection in dirname *)
let traverse_dir fx dirname =
  let rec helper dir_handle =
    try
      let file = Unix.readdir dir_handle in
      if col file then fx file;
      helper dir_handle
    with
      | End_of_file -> ()
  in
  helper (Unix.opendir dirname)

(* Given a list of docs, converts it to a `List containing the doc list *)
let rec list_to_doc (doc_list : doc list) (acc:doc list) : doc =
  match doc_list with
    | [] -> `List(acc)
    | h::t -> list_to_doc t (h::acc)

(* Removes a db from disc by name, deleting its folder and json files *)
let remove_db db_name =
  try (
    let rm filename = Unix.unlink ("Persist/" ^ db_name ^ "/" ^ filename) in
    traverse_dir rm ("Persist/" ^ db_name);
    Unix.rmdir ("Persist" ^ db_name))
  with
  | _ -> raise NotInDisc

(* Writes a collection to disc,
 * creating a json file in the folder containing the db *)
let write_collection db_name col_name doc_list =
  let docs = list_to_doc doc_list [] in
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = "Persist/" ^ db_name ^ "/" ^ col_name ^ ".json" in
  Yojson.Basic.to_file filepath docs_json

(* writes a db to disc, creating a directory with the name of the db and
 * creating a json in the directory for every collection *)
let write_db db_name db =
  let (col_hashtbl, dirty) = db in
  let helper col_name col =
    write_collection db_name col_name (fst col)
  in
  Unix.chdir "Persist";
  (try (
    Unix.mkdir db_name 0o777;
    Unix.chdir "..")
  with Unix.Unix_error (Unix.EEXIST, "mkdir", db_name) ->
    Unix.chdir "..";
    Hashtbl.iter helper col_hashtbl);
  Hashtbl.iter helper col_hashtbl

(* Writes every db in the catalog to disc, creating a directory for every db
 * and a json file in the directories corresponding to the dbs' collection *)
let write_env (env : catalog) =
  let helper db_name db =
    let (_, dirty) = db in
    try (
      if dirty then (
        remove_db db_name;
        Unix.rmdir db_name;
        write_db db_name db))
    with
      | NotInDisc -> write_db db_name db
  in
  create_dir "Persist";
  Hashtbl.iter helper env

(* given a json of type `List x, unpacks x into a list of json *)
let get_docs json = match json with
  | `List x ->
    let rec helper json_list acc = match json_list with
      | [] -> acc
      | h::t -> helper t (h::acc)
    in helper x []
  | _ -> failwith "expected a json of `List"

(* given a filename, removes the .json file extension *)
let strip filename =
  let len = String.length filename in
  String.sub filename 0 (len-5)

(* given a db name and a collection name, returns a collection by reading
 * the collection from disc *)
let read_collection db_name col_name =
  let path = "Persist/" ^ db_name ^ "/" ^ col_name ^ ".json" in
  let doc_list = path |> Yojson.Basic.from_file
    |> Yojson.Basic.Util.member "entries"  |> get_docs in
  (doc_list, [])

(* given a db name, returns a db by reading the db and all of its collections
 * from disc *)
let read_db db_name db =
  let (col_hashtbl, dirty) = db in
  try
    let col_from_disc file =
      let col_name = strip file in
      let new_col = read_collection db_name col_name in
      Hashtbl.add col_hashtbl col_name new_col
    in
    traverse_dir col_from_disc ("Persist/" ^ db_name);
  with
    | Unix.Unix_error _ -> raise NotInDisc

let show_persisted () =
  let rec helper dir_handle acc =
    try
      let db = Unix.readdir dir_handle in
      if Sys.is_directory ("Persist/" ^ db) && (db <> ".") && (db <> "..") then
        helper dir_handle (db :: acc)
      else
        helper dir_handle acc
    with
      | End_of_file -> acc
  in
  try (helper (Unix.opendir "Persist") []) with
  | _ -> []

