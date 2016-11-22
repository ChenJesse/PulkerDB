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
type doc = Yojson.Basic.json

type col = (string * doc list) ref

type db = (string * col list * bool) ref

type catalog = (db list) ref

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

let write_collection db_name col_name doc_list =
  let docs = list_to_doc doc_list [] in
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = db_name ^ "/" ^ col_name ^ ".json" in
  Yojson.Basic.to_file filepath docs_json

let write_db db_ref =
  let (db_name, col_refs, dirty) = !db_ref in
  Unix.mkdir db_name 0o777;
  let rec helper col_list = match col_list with
    | [] -> ()
    | h::t ->
      let (col_name, docs) = !h in
      write_collection db_name col_name docs;
      helper t
  in helper col_refs

let write_env (env_ref : catalog) =
  let rec helper env = match env with
    | [] -> ()
    | db :: t ->
      let (db_name, _, dirty) = !db in (
      try (
        let rm filename = Unix.unlink (db_name ^ "/" ^ filename) in
        if dirty then (
          traverse_dir (rm) db_name;
          Unix.rmdir db_name;
          write_db db))
      with
        | Unix.Unix_error(Unix.ENOENT, "opendir", db_name) -> write_db db
      );
      helper t
  in helper !env_ref

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

let read_collection db_name col_name col_ref =
  let path = db_name ^ "/" ^ col_name in
  let doc_list = path |> Yojson.Basic.from_file
    |> Yojson.Basic.Util.member "entries"  |> get_docs in
  col_ref := ( (fst !col_ref), doc_list)

let read_db db_ref =
  let (db_name, col_list, dirty) = !db_ref in
  try
    let col_from_disc file =
      let new_col = ref (strip file, []) in
      read_collection db_name file new_col;
      db_ref := (db_name, new_col::col_list, dirty)

    in
    traverse_dir col_from_disc db_name
  with
    | Unix.Unix_error _ -> raise NotInDisc
