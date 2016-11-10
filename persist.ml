let write_collection db_name col_name docs =
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = db_name ^ "/" ^ col_name in
  Yojson.Basic.to_file filepath docs_json

let write_db db_ref =
  let (db_name, col_refs) = !db_ref in
  Unix.mkdir db_name 0o777;
  let rec helper col_list = match col_list with
    | [] -> ()
    | h::t ->
      let (col_name, docs) = !h in
      write_collection db_name col_name docs;
      helper t
  in helper col_refs

let rec write_env env = match env with
  | [] -> ()
  | h::t -> write_db h; write_env t