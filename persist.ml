(**
 * Taken from Yojson.Basic documentation:
 * type json = [ `Assoc of (string * json) list
 *             | `Bool of bool
 *             | `Float of float
 *             | `Int of int
 *             | `List of json list
 *             | `Null
 *             | `String of string ]
 *)
 open Tree


type doc = Yojson.Basic.json

type index_file = {id_name:string; id_table: (Yojson.Basic.json,Yojson.Basic.json) Hashtbl.t; keys: Yojson.Basic.json list Tree.tree ref}

type index_list = index_file list

type col = doc list * index_list

type db = (string, col) Hashtbl.t * bool

type catalog = (string, db) Hashtbl.t



exception NotInDisc

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

let rec list_to_doc (doc_list : doc list) (acc:doc list) : doc =
  match doc_list with
    | [] -> `List(acc)
    | h::t -> list_to_doc t (h::acc)

let remove_db db_name =
  try (
    let rm filename = Unix.unlink (db_name ^ "/" ^ filename) in
    traverse_dir rm db_name);
    Unix.rmdir db_name
  with
  | _ -> raise NotInDisc

let write_collection db_name col_name doc_list =
  let docs = list_to_doc doc_list [] in
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = db_name ^ "/" ^ col_name ^ ".json" in
  Yojson.Basic.to_file filepath docs_json

let write_db db_name db =
  let (col_hashtbl, dirty) = db in
  Unix.mkdir db_name 0o777;
  let helper col_name col =
    write_collection db_name col_name ((fst)col)
  in
  Hashtbl.iter helper col_hashtbl

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
  in Hashtbl.iter helper env

let get_docs json = match json with
  | `List x ->
    let rec helper json_list acc = match json_list with
      | [] -> acc
      | h::t -> helper t (h::acc)
    in helper x []
  | _ -> failwith "expected a json of `List"

let strip filename =
  let len = String.length filename in
  String.sub filename 0 (len-5)

let read_collection db_name col_name =
  let path = db_name ^ "/" ^ col_name ^ ".json" in
  let doc_list = path |> Yojson.Basic.from_file
    |> Yojson.Basic.Util.member "entries"  |> get_docs in
  (doc_list, [])

let read_db db_name db =
  let (col_hashtbl, dirty) = db in
  try
    let col_from_disc file =
      let col_name = strip file in
      let new_col = read_collection db_name col_name in
      Hashtbl.add col_hashtbl col_name new_col
    in
    traverse_dir col_from_disc db_name
  with
    | Unix.Unix_error _ -> raise NotInDisc