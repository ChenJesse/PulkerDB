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

let rec list_to_doc (doc_list : doc list) (acc:doc list) : doc =
  match doc_list with
    | [] -> `List(acc)
    | h::t -> list_to_doc t (h::acc)

let write_collection db_name col_name doc_list =
  let docs = list_to_doc doc_list [] in
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = db_name ^ "/" ^ col_name ^ ".txt" in
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
      let (_, _, dirty) = !db in
      if dirty then write_db db;
      helper t
  in helper !env_ref

let get_docs json = match json with
  | `List x ->
    let rec helper json_list acc = match json_list with
      | [] -> acc
      | h::t -> helper t (h::acc)
    in helper x []
  | _ -> failwith "expected a json of `List"

(* [txt filename] Given a filename, returns true if filename has a
 * .txt extension.
 * requires:
 *   - [filename] is a string
 *)
let txt filename =
  let len = String.length filename in
  if len < 4 then
    false
  else
    Str.string_match (Str.regexp "\\.txt$") filename (len-4)

let read_collection db_name col_name col_ref =
  let path = db_name ^ "/" ^ col_name in
  let doc_list = path |> Yojson.Basic.from_file
    |> Yojson.Basic.Util.member "entries"  |> get_docs in
  col_ref := ( (fst !col_ref), doc_list)

let read_db db_ref =
  let (db_name, col_list, dirty) = !db_ref in
  try
    let dir = Unix.opendir db_name in
    let rec helper dir_handle =
      try
        let file = Unix.readdir dir_handle in
        if txt file then (
          let new_col = ref (file, []) in
          read_collection db_name file new_col;
          db_ref := (db_name, new_col::col_list, dirty);
          helper dir
        )
        else
          helper dir
      with
        | End_of_file -> ()
    in helper dir
  with
    | Unix.Unix_error _ -> raise NotInDisc
