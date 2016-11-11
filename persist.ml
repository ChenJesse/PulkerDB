let write_collection db_name col_name docs =
  let docs_json = `Assoc([("entries", docs)]) in
  let filepath = db_name ^ "/" ^ col_name ^ ".txt" in
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

let get_docs json = match json with
  | `List x ->
    let rec helper json_list acc = match json_list with
      | [] -> acc
      | h::t -> helper t (h::acc)
    in helper x []
  | _ -> failwith "expected a json of `List"

let read_collection db_name db_ref col_name =
  match create_col db_name col_name with
    | CreateColResponse(true, _) ->
      let path = db_name ^ "/" ^ col_name ^ ".txt" in
      let doc_list = path |> Yojson.Basic.from_file
        |> Yojson.Basic.Util.member "entries"  |> get_docs in
      let old_col = (get_col_ref col_name db_ref) in
      old_col := ( (fst !old_col), doc_list)
    | CreateColResponse(false, x) -> failwith x
    | _ -> failwith "unexpected response"

let read_db db_name =
  match create_db db_name with
    | CreateDBResponse(true, _) ->
      let dir = Unix.opendir db_name in
      let rec helper dir_handle =
        try
          let file = Unix.readdir dir_handle in
          read_collection db_name (get_db_ref db_name) file;
          helper dir
        with
          | End_of_file -> ()
      in helper dir
    | CreateDBResponse(false, x) -> failwith x
    | _ -> failwith "unexpected response"