open Yojson.Basic
open Persist
open Tree
open Models

type response = Success of string | Failure of string

exception DropException
exception LocateDBException
exception LocateColException
exception InvalidUpdateDocException
exception InvalidAggDocException
exception InvalidQueryDocException
exception KeyNotFoundException

let unexpected_error = "Unexpected error"
let db_find_error db_name = db_name ^ " was not found."
let col_find_error col_name = col_name ^ " was not found."

type converter = ToInt of (doc -> int) | ToString of (doc -> string)
  | ToBool of (doc -> bool) | ToFloat of (doc -> float)

type opWrapper = Less | LessEq | Greater | GreaterEq | NotEq | Eq | Exists

let environment : catalog = Hashtbl.create 20

(* ------------------------------HELPERS------------------------------ *)

let add_db_env db_name db = Hashtbl.add environment db_name db

(**
 * Returns database with db_name in the environment. On failure,
 * goes to disk and tries to load a persisted db under the same name,
 * and then return it. On failure, raises exception.
 * requires:
 *   - [db_name] is a string
 *)
let get_db db_name =
  try (
    try (Hashtbl.find environment db_name) with
      | _ ->
        let (empty_db : db) = ((Hashtbl.create 100), false) in
        read_db db_name empty_db;
        empty_db
  ) with
  | _ -> raise LocateDBException

(**
 * Retrieves the collection object with name col, given the db.
 * requires:
 *   - [col_name] is a string
 *   - [db] is a db
 *)
let get_col col_name db =
  try (
    let cols = db |> fst in
    Hashtbl.find cols col_name
  )
  with
  | _ -> raise LocateColException

(**
 * To be called when a database is modified, to flag that db as
 * dirty, so that on persist, we know to re-save that db.
 * requires:
 *   - [db_name] is a string
 *)
let set_dirty db_name =
  let (db, _) = get_db db_name in
  Hashtbl.replace environment db_name (db, true)

(**
 * Given a list, will print a string based on the contents of the
 * list, with each element wrapped in brackets.
 * requires:
 *   - [lst] is a list of strings
 *)
let stringify_list lst =
  List.fold_left (fun acc ele -> acc ^ " [" ^ ele ^ "]") "" lst

(**
 * Given two docs, a and b will print 0, 1, or -1 depending on whether A
 * is = > or < to B
 * requires:
 *   - [a] is of type doc with the structure specified in help.ml
 *   - [b]] is of type doc with the structure specified in help.ml
 *)
let compare_docs a b =
  if a > b then 1
  else if a < b then -1
  else 0

(**
 * Given an array of docs, sorts in ascending order
 * requires:
 *   - [arr] is an array of docs
 *)
let key_sort arr = Array.sort compare_docs arr

let persist_query response = match response with
  | Success json_string ->
    let file = json_string |> from_string |> write_query_json in
    Success ("Output stored at " ^ file ^ ".\n" ^ json_string)
  | Failure x -> Failure x

let save_env () = write_env environment; Success "Saved successfully!"

(* -------------------------------CREATION------------------------------- *)

<<<<<<< HEAD
(**
 * Given a string representing name of db, checks for db with same name in
 * the environment, then checks if db exists in disk, and if so, loads it
 * into memory. Otherwise, Creates the db.
 * requires:
 *   - [db_name] is a string
 *)
=======
let new_dbs = ref []
>>>>>>> a797be0a101675d17a20f20915d7acc5cf292324

let create_db db_name =
  match (Hashtbl.mem environment db_name) with
  | true -> Failure "Database with same name already exists"
  | false -> try (
      let (empty_db:db) = (Hashtbl.create 100, false) in
      read_db db_name empty_db;
      add_db_env db_name empty_db;
      Failure "Database with same name already exists"
    ) with
    | NotInDisc ->
      add_db_env db_name (Hashtbl.create 100, true);
      new_dbs := db_name::(!new_dbs);
      Success "Database created successfully!"
    | _ -> Failure unexpected_error

(**
 * Adds og_doc to a index if one is found. Otherwise return nothing
 * requires:
 *    - [og_doc] is a doc
 *    - [doc] is a list of `Assoc
 *    - [col] is of type collection
 *)
let rec index_modifier og_doc doc col = match doc with
  | [] -> ()
  | (k,v)::tl ->
    let rec helper indices = match indices with
    | [] -> index_modifier og_doc tl col
    | index::t ->
      if index.id_name = k then
        let tree = index.keys in
        if Tree.member v !tree then
          if ((Tree.find v !tree) = [`Null] && v <>`Null ||
             ((Tree.find v !tree) = [`Int 0] && v <> `Int 0))
          then tree:= Tree.insert v og_doc (!tree) true
          else tree:= Tree.insert v og_doc (!tree) false
        else tree := Tree.insert v og_doc (!tree) false
    in
    helper (snd col)

(**
 * Given the doc, update the index
 * if that doc's attribute matches a declared index field.
 * requires:
 *      - [ogDoc] is of type doc
 *      - [doc] is a list of `Assoc
 *      - [col] is of type collection
 *)
let rec index_updater ogDoc doc col = match doc with
  |`Assoc a -> (
    match a with
    | ((b : string),(c : Tree.key))::tl -> index_modifier ogDoc a col
    | _ -> ()
  )
  |_ -> ()

(**
 * Generate json with ascending values based on input len.
 * requires:
 *     - [len] is a int of value >=0
 *     - [lst] is a list of docs
 *)
let benchmark_json_gen len lst =
  let rec helper lent lst_p ctr =
  if ((List.length lst_p) >= lent) then lst_p
  else
    let new_doc = `Assoc[("a", `Int ctr); ("b", `Int (ctr * 2))] in
    helper lent (new_doc::lst_p) (ctr + 1)
  in helper len lst 0


(**
 * Returns the tree associated with the specified index in the index desired.
 * Returns empty if no index can be found.
 * requires:
 *      - [index_name] is of type string and is a valid index
 *      - [index] is of type index_file list
 *)
let rec get_index index_name index =
  match index with
  | [] -> Tree.empty
  | { id_name = a; id_table =_; keys= c }::tl ->
    if a = index_name then !c else get_index index_name tl

(**
 * Run through the tree you were provided and return
 * value list for the key that matches value.
 * requires:
 *       - [tree_list] is a list of keys and values associated in
 *          the index tree.
 *       - [value] is of the same value and type as a key in the tree list.
 *)
let rec tree_helper tree_list value =
  match tree_list with
  | [] -> Failure (pretty_to_string (`List []))
  | (k,v)::tl ->
    if value = k then Success (pretty_to_string (`List v))
    else tree_helper tl value

let get_values value index_name col_name db_name =
  let index_list = snd ((get_db db_name) |> get_col col_name) in
  let index_tree  = get_index index_name index_list in
  let tree_as_list = Tree.to_list index_tree in
  tree_helper tree_as_list value


let create_doc db_name col_name doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let col_list = fst col in
    let new_col = doc::col_list in
    index_updater doc doc col;
    let new_col_index = (new_col, (snd) col) in
    Hashtbl.replace (fst db) col_name new_col_index;
    set_dirty db_name;
    Success "Document created successfully!"
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure unexpected_error

let create_col db_name col_name =
  try (
    let db = get_db db_name in
    match (Hashtbl.mem (fst db) col_name) with
    | true -> Failure (col_name ^ " already exists.")
    | false ->
        Hashtbl.add (fst db) col_name ([],[]);
        Success "Collection created successfully!"
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | _ -> Failure unexpected_error

(* -------------------------------QUERYING-------------------------------- *)

(* Returns true if the doc is a nested json*)
let nested_json doc =
  match doc with
  | `Assoc _ -> true
  | _ -> false

(* Returns true if the doc is a comparator json ex. "{key: {'$lte', 5}}" *)
let comparator_json doc =
  match doc with
  | `Assoc lst -> let k = List.hd lst |> fst in (String.get k 0) = '$'
  | _ -> false

(**
 * Given a doc (json), retrieves the function that
 * extracts the value into OCaml primitive
 *   - [doc1] is a doc
 *   - [doc2] is a doc
 *)
let rec get_converter doc1 doc2 =
  match doc1, doc2 with
  | (`Bool _, `Bool _) -> ToBool(Util.to_bool)
  | (`Float x, `Float y) -> ToFloat(Util.to_float)
  | (`Int x, `Int y) -> ToInt(Util.to_int)
  | (`String x, `String y) -> ToString(Util.to_string)
  | (_,_) -> failwith "Types don't match"

let unwrap_op op =
  match op with
  | Less -> (<)
  | LessEq -> (<=)
  | Greater -> (>)
  | GreaterEq -> (>=)
  | NotEq -> (<>)
  | Eq -> (=)
  | _ -> failwith "Shouldn't be here"

let compare_int op (doc1 : doc) (doc2 : doc) converter =
  match converter with
  | ToInt x -> (unwrap_op op) (x doc1) (x doc2)
  | _ -> failwith "Incorrect converter"

let compare_bool op (doc1 : doc) (doc2 : doc) converter =
  match converter with
  | ToBool x -> (unwrap_op op) (x doc1) (x doc2)
  | _ -> failwith "Incorrect converter"

let compare_float op (doc1 : doc) (doc2 : doc) converter =
  match converter with
  | ToFloat x -> (unwrap_op op) (x doc1) (x doc2)
  | _ -> failwith "Incorrect converter"

let compare_string op (doc1 : doc) (doc2 : doc) converter =
  match converter with
  | ToString x -> (unwrap_op op) (x doc1) (x doc2)
  | _ -> failwith "Incorrect converter"

(**
 * Compares doc1 and doc2, assuming they are the same primitive
 * type (int vs. int, string vs. string, etc.)
 *   - [doc1] is a doc
 *   - [doc2] is a doc
 *)
let compare op doc1 doc2 =
  match (get_converter doc1 doc2) with
  | ToInt x -> compare_int op doc1 doc2 (ToInt(x))
  | ToBool x -> compare_bool op doc1 doc2 (ToBool(x))
  | ToString x -> compare_string op doc1 doc2 (ToString(x))
  | ToFloat x -> compare_float op doc1 doc2 (ToFloat(x))

let parse_op str = match str with
  | "$lt" -> Some Less
  | "$lte" -> Some LessEq
  | "$gt" -> Some Greater
  | "$gte" -> Some GreaterEq
  | "$ne" -> Some NotEq
  | "$exists" -> Some Exists
  | _ -> None

(**
 * query_doc is guaranteed to have the field this index tree is built on.
 * collectionTree is the index for the attribute specified.
 * This needs to parse the query and figure out what type of query it is.
 * Depending on what type of query it is,
 * it will then call traverse with certain bounds on the tree.
 * requires:
 *      - [col_tree] is a index tree
 *      - [query_doc] is a list of `Assoc
 *)
let index_query_builder col_tree query_doc =
  let rec helper col_tree query_doc = match query_doc with
    | h::t -> (
      let comparator = match (fst h) with
      | "$lt" -> Some Less
      | "$lte" -> Some LessEq
      | "$gt" -> Some Greater
      | "$gte" -> Some GreaterEq
      | "$ne" -> Some NotEq
      | _ -> None
      in
      let max_temp = `List[`Int max_int] in
      match comparator with
      | Some Less -> get_range col_tree (snd h) `Null
      | Some Greater -> get_range col_tree max_temp (snd h)
      | Some LessEq ->
        (get_range col_tree (snd h) `Null) @ (find (snd h) col_tree)
      | Some GreaterEq ->
        (get_range col_tree max_temp (snd h)) @ (find (snd h) col_tree)
      | Some NotEq ->
        (get_range col_tree (snd h) `Null) @ (get_range col_tree max_temp (snd h))
      | Some Exists -> get_range col_tree max_temp `Null
      | Some Eq -> failwith unexpected_error
      | None -> match (nested_json (snd h)) with
        | true -> (
          (* We have a doc as the value, need to recurse *)
          (* Represents the nested doc in the query_doc *)
          let nested = match (snd h) with
            | `Assoc lst -> lst
            | _ -> failwith unexpected_error in
          (* If it's a comparator JSON, we only recurse a level in on doc (nested) *)
          if h |> snd |> comparator_json then helper col_tree nested else []
        )
        (* We have an equality check *)
        | false -> find (snd h) col_tree
      )
    | _ -> failwith unexpected_error
  in
  match query_doc with
  | `Assoc lst -> helper col_tree lst
  | _ -> failwith "Invalid query JSON"

let check_doc doc query_doc =
  let rec helper doc query_doc p_key acc = match acc, query_doc with
  | (false, _) -> false
  | (_, []) -> acc
  | (_, h::t) ->
    let comparator = parse_op (fst h)
    in
    match comparator with
    | Some Exists ->
      let doc1 = Util.member p_key doc in
      let doc2 = snd h in
      (doc1 <> `Null && doc2 = `Bool true) || (doc1 = `Null && doc2 = `Bool false)
    | Some op ->
      let item1 = Util.member p_key doc in
      let item2 = snd h in
      (try (compare op item1 item2)
      with | _ -> false)
    | None -> (match (nested_json (snd h)) with
      | true ->
        let nested = match (snd h) with
          | `Assoc lst -> lst
          | _ -> failwith "Can't be here" in
        if (comparator_json (snd h)) then helper doc nested (fst h) true
          |> helper doc t p_key
        else (try (helper (Util.member (fst h) doc) nested (fst h) true
              |> helper doc t p_key) with | _ -> false)
      | false ->
        let item1 = Util.member (fst h) doc in
        let item2 = snd h in
        (try (compare Eq item1 item2 |> helper doc t p_key)
        with | _ -> false)
      )
  in
  match query_doc with
  | `Assoc lst -> helper doc lst "" true
  | _ -> raise InvalidQueryDocException

(**
* Checks if there are any indices that match the current queries field.
* IF there are, we want to get teh docs associated with this query from the index
* And return those after converting to a normal doc list.
* Otherwise, continue and ultimately just
* return whatever the collection's list of docs are if no index can be matched.
* requires:
*     - [col] is of type collection
*     - [query_list] is a list of type doc
*)
let index_checker col query_list =
  let docs = ref [] in
  let rec helper query_list =
    match query_list with
    | [] -> docs := fst col
    | h::t ->
      let index = get_index (fst h) (snd col) in
      if index <> Tree.empty then docs := index_query_builder index (`Assoc [h])
      else helper t
  in
  helper query_list;
  if !docs = [] then docs := (fst col);
  !docs

let query_col db_name col_name query_doc =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let lst_queries = (match query_doc with |`Assoc lst -> lst |_ -> [] ) in
    let documents = index_checker col lst_queries in
    let query_result = List.filter (fun d -> check_doc d query_doc) documents in
    let query_string = `List(query_result) |> pretty_to_string in
    Success query_string
  ) with
  | InvalidQueryDocException ->
    Failure "Invalid query doc. Refer to -query_doc for more information."
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure unexpected_error

(**
 * Extract all the keys associated with this List, removing duplicates as we go.
 * requires:
 *     - [list_tbl] is a list representation of a hashtable
 *     - [key_list] is a list of type doc
 *)
let rec extract_keys list_tbl key_list =
  match list_tbl with
  | [] -> key_list
  | (k,v)::t ->
    if List.mem k key_list then extract_keys t key_list
    else extract_keys t (k::key_list)

(**
 * Return the keySet for my hashtable, tbl. That is, a set with only unique keys.
 * requires:
 *     - tbl is a hashtable.
 *)
let key_set tbl =
  let list_tbl = Hashtbl.fold (fun k v acc-> (k,v)::acc) tbl [] in
  let final_list = extract_keys list_tbl [] in
  Array.of_list final_list


let create_index db_name col_name index_name query_doc =
  let db = db_name |> get_db in
  let col = db |> get_col col_name in
  let query_result = List.filter (fun d -> check_doc d query_doc) (fst col) in
  match List.length query_result with
  | 0 -> Failure "no docs matched the desired field"
  | _ ->
    let table = Hashtbl.create 5 in
    let keys_array =
      List.iter (fun res -> Hashtbl.add table (Util.member index_name res) res) query_result;
      key_set table in
    let tree = ref Tree.empty in
    let update = {id_name = index_name; id_table = table; keys = tree} in
    key_sort keys_array;
    keys_array |> Array.to_list |> List.iter (fun key ->
      let table_list = Hashtbl.find_all table key in
      List.iter (fun ele -> tree := Tree.insert key ele !tree false) table_list
    );
    (fst col, update::(snd col)) |> Hashtbl.replace (fst db) col_name;
    Success "Index was successfully made!"

(**
 * Given a remove operation, updates the index tree to reflect new state.
 * requires:
 *      - [db_name] is the string
 *      - [col_name] is the string
 *      - [index_list] is a list of index_file
 *      - [new_list] is a list of index_file
 *)
let rec recreate_index db_name col_name index_list new_list=
  match index_list with
  |[]-> new_list
  |{id_name= name; id_table= idtable; keys= tree}::tl->
      let query_doc = `Assoc [(name, `Assoc[("$exists", `Bool true)])] in
      let _ = create_index db_name col_name name query_doc in
      let new_index_list = (snd) (db_name |> get_db |> get_col col_name) in
      let newTree = get_index name new_index_list in
      {id_name = name; id_table = idtable; keys = ref(newTree)}::new_list

let show_col db_name col_name =
  try (
    let col = db_name |> get_db |> get_col col_name in
    let contents = `List(fst col) |> pretty_to_string in
    Success contents
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure unexpected_error

let show_db db_name =
  try (
    let db_hashtbl = db_name |> get_db |> fst in
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) db_hashtbl [] in
    let contents = stringify_list contents_list in
    Success contents
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | _ -> Failure unexpected_error

let show_catalog () =
  try (
    let persisted_dbs = Hashtbl.fold (fun k _ init -> k::init) environment [] in (* TODO: Replace with function instead of environment *)
    let new_dbs = !new_dbs in 
    let contents = new_dbs@persisted_dbs |> stringify_list in
    Success contents
  ) with
  | _ -> Failure unexpected_error

(* ------------------------------AGGREGATION-------------------------------- *)

(**
 * Returns a hashtable, with all the documents in the specified db.col
 * in buckets, based off of the given attribute. If a doc does not
 * have the specified attribute, then don't add it to the hashtable.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [attr] is a string
 *)
let bucketize db_name col_name attr =
  let col = (db_name |> get_db |> get_col col_name) in
  let buckets = Hashtbl.create 20 in
  List.iter (fun doc ->
    try (
      let value = Util.member attr doc in
      if Hashtbl.mem buckets value then
        let bucket = Hashtbl.find buckets value in
        Hashtbl.replace buckets value (doc::bucket)
      else
        Hashtbl.add buckets value [doc]
      ) with | _ -> ()
  ) (fst col);
  buckets

(**
 * Iterates through the buckets, and based on the operation
 * specified, aggregates the values associated with t_field.
 * If the value associated with t_field is not an int, ignore it.
 *   - [bucket] is a list of docs
 *   - [op] is a string
 *   - [t_field] is a string
 *)
let aggregator_attr bucket op t_field =
  match op with
  | "$sum" ->
    `Int (List.fold_left (fun acc doc ->
      try (
        let v = Util.(member t_field doc |> to_int) in acc + v
      ) with | _ -> acc) 0 bucket)
  | "$max" ->
    `Int (List.fold_left (fun max doc ->
      try (
        let v = Util.(member t_field doc |> to_int) in
        if v > max then v else max
      ) with | _ -> max) min_int bucket)
  | "$min" -> `Int (List.fold_left (fun min doc ->
      try (
        let v = Util.(member t_field doc |> to_int) in
        if v < min then v else min
      ) with | _ -> min) max_int bucket)
  | _ -> failwith "Aggregator failed"

(**
 * For summing up with constants for a count of buckets.
 *   - [bucket] is a list of docs
 *   - [op] is a string
 *   - [t_field] is a string
 *)
let aggregator_const bucket op t_field =
  match op with
  | "$sum" -> `Int (List.fold_left (fun acc doc -> acc + t_field) 0 bucket)
  | _ -> failwith "Aggregator failed"

(**
 * Handles the aggregation logic for all buckets.
 * Iterates through all buckets, and in each iteration,
 * iterates through each field we have to create, specified in agg_lst.
 *   - [acc] is a list of docs
 *   - [agg_lst] is a list of pairs
 *   - [t_field] is a string
 *)
let aggregation_helper acc agg_lst buckets =
  Hashtbl.iter (fun key bucket ->
    let result_doc = `Assoc (
      ("_id", key)::
      List.map (fun (field_name, field) ->
        match field with
        | `Assoc lst -> (
            let (op, t_field) = List.hd lst in
            try (
              (field_name, aggregator_attr bucket op (t_field |> Util.to_string))
            ) with
            | _ -> (field_name, aggregator_const bucket op (t_field |> Util.to_int))
          )
        | _ -> raise InvalidAggDocException
      ) agg_lst
    ) in
    acc := result_doc::(!acc)
  ) buckets

(**
 * Handles the aggregation logic on a collection.
 * Example:
 * db.mycol.aggregate({_id : "$by_user", num_tutorial : {$sum : "$likes"}})
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [agg_doc] is a doc, conforming to structure specified in help.ml
 *)
let aggregate db_name col_name agg_doc =
  try (
    let bucket_attr = Util.(member "_id" agg_doc |> to_string) in
    let buckets = bucketize db_name col_name bucket_attr in
    match agg_doc with
    | `Assoc lst ->
      let acc = ref [] in
      let filtered = List.filter (fun pair -> (fst pair) <> "_id") lst in
      aggregation_helper acc filtered buckets;
      let agg_string = `List(!acc) |> pretty_to_string in
      Success agg_string
    | _ -> Failure "Error with aggregating response.
                    Refer to -agg_doc for more information."
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with aggregating response.
                  Refer to -agg_doc for more information."

(* -------------------------------REMOVING--------------------------------- *)

(**
 * Given a string representing name of db, drops the db in the environment,
 * and should drop the db in disk if the database is saved.
 *   - [db_name] is a string
 *)
let drop_db db_name =
  try (
    let db_exists = Hashtbl.mem environment db_name in
    if db_exists then (
      Hashtbl.remove environment db_name;
      remove_db db_name;
      Success "Dropped database successfully!"
    ) else (
      raise DropException
    )
  ) with
  | NotInDisc -> Success "Dropped database successfully!"
  | DropException -> Failure (db_find_error db_name)
  | _ -> Failure unexpected_error

(**
 * Given a string representing name of db and col, drops the col
 * in the environment, and should drop the col in disk if the database is saved.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *)

let drop_col db_name col_name =
  try (
    let (db_hashtbl, _) = get_db db_name in
    let col_exists = Hashtbl.mem db_hashtbl col_name in
    if col_exists then (
      Hashtbl.remove db_hashtbl col_name;
      set_dirty db_name;
      Success "Dropped collection successfully!"
    ) else
      Failure (col_find_error col_name)
  ) with
  | _ -> Failure unexpected_error

let batch_replace doc_list tree index_val =
  let zeroth_doc = List.nth doc_list 0 in
  let non_zero_docs = List.filter (fun f -> f <> zeroth_doc) doc_list in
  tree := Tree.insert index_val zeroth_doc !tree true;
  List.iter (fun ele ->
    tree := Tree.insert index_val ele !tree false) non_zero_docs

(**
 * Replaces the value list associated with the key of this doc.
 * requires:
 *     - [index_list] is a list of type index_file
 *     - [doc] is a doc conforming to structure specified in help.ml
 *)
let rec replace_tree index_list doc =
  match index_list with
  | [] -> ()
  | { id_name = name; id_table = table; keys = tree }::tl ->
    let index_val = Util.member name doc in
    if index_val <> `Null then
      let doc_list = Tree.find index_val !tree |> List.filter (fun f -> f <> doc) in
      match List.length doc_list with
      | 0 -> tree := Tree.insert index_val `Null !tree true
      | 1 -> tree := Tree.insert index_val (List.nth doc_list 0) !tree true
      | _ -> batch_replace doc_list tree index_val
    else
      if Tree.member index_val !tree then
        let doc_list = List.filter (fun f -> f <> doc) (Tree.find index_val !tree) in
        if List.length doc_list = 0 then
          tree := Tree.insert index_val (`Int 0) !tree true
        else batch_replace doc_list tree index_val
      else replace_tree tl doc

(**
 * Drops anything in the specified collection that satisfies the query_doc.
 * Changes should persist if the database is saved.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *)
let remove_doc db_name col_name query_doc =
  try (
    let db = db_name |> get_db in
    let col = get_col col_name db in
    let index_list = (snd) col in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) (fst col) in
    Hashtbl.replace (fst db) col_name (new_col, []);
    let lost_docs = List.filter (fun d->  (check_doc d query_doc)) (fst col) in
    List.iter (replace_tree index_list) lost_docs;
    Hashtbl.replace (fst db) col_name (new_col, snd col);
    set_dirty db_name;
    Success "Removed document successfully!"
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with removing documents.
                  Refer to -query_doc for more information."

(* ----------------------------UPDATING/REPLACING---------------------------- *)

(**
 * Drops anything in the specified collection that satisfies the query_doc.
 * Changes should persist if the database is saved. Then returns all removed docs in list.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *)
let remove_and_get_doc db_name col_name query_doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let index_list = snd col in
    let query = List.filter (fun d -> check_doc d query_doc) (fst col) in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) (fst col) in
    Hashtbl.replace (fst db) col_name (new_col, []);
    List.iter (replace_tree index_list) query;
    Hashtbl.replace (fst db) col_name (new_col, snd col);
    query
  ) with | _ -> []

(**
 * Responsible for updating a single document, given an update document
 *   - [doc] is a general doc, the document to be modified
 *   - [update_doc] is a doc conforming to structure specified in help.ml
 *)
let rec modify_doc doc update_doc =
  let helper doc (u_key, u_value) =
    let lst = match doc with
      | `Assoc lst -> lst
      | _ -> failwith "Should not be here"
    in
    (* Constructing the updated doc *)
    if Util.member u_key doc <> `Null then
      `Assoc (
        (List.map (fun pair -> match (fst pair) = u_key with
          | true -> (match snd pair with
            | `Assoc _ -> (fst pair, modify_doc (snd pair) u_value)
            | _ -> (fst pair, u_value))
          | false -> pair) lst)
      )
    else `Assoc ((u_key, u_value)::lst)
  in
  match update_doc with
  | `Assoc pairs -> List.fold_left (fun acc pair -> helper acc pair) doc pairs
  | _ -> raise InvalidUpdateDocException

(**
 * Given a doc representing criteria to query on, removes all appropriate docs,
 * and then inserts the given doc.
 *   - [db_name] is a string
 *   - [col_name] is a string
 *   - [query_doc] is a doc conforming to structure specified in help.ml
 *   - [update_doc] is a doc conforming to structure specified in help.ml
 *)
let replace_col db_name col_name query_doc update_doc =
  try (
    let db = get_db db_name in
    let _ = remove_doc db_name col_name query_doc in
    let col = get_col col_name db in
    let new_col = update_doc::((fst)col) in
    Hashtbl.replace (fst db) col_name (new_col,[]);
    let new_index_list = recreate_index db_name col_name ((snd) col) [] in
    Hashtbl.replace (fst db) col_name (new_col, new_index_list);
    set_dirty db_name;
    Success "Collection replaced successfully!"
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with replacing doc. Ensure that the query document
    is in the correct format. Refer to -query_doc for more information."

let update_col db_name col_name query_doc update_doc =
  try (
    let db = get_db db_name in
    let u_doc = match Util.member "$set" update_doc with
      | `Assoc json -> `Assoc json
      | _ -> raise InvalidUpdateDocException in
    let query = remove_and_get_doc db_name col_name query_doc in
    let col = get_col col_name db in
    let new_col = (fst col)@(List.map (fun json -> (modify_doc json u_doc)) query) in
    Hashtbl.replace (fst db) col_name (new_col,(snd) col);
    let new_index_list = recreate_index db_name col_name (snd col) [] in
    Hashtbl.replace (fst db) col_name (new_col, new_index_list);
    Success "Collection updated successfully!"
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with updating collection.
    Refer to -query_doc and -update_doc for more information."

let clear_env () = Hashtbl.reset environment

let benchmarker ()=
  let _ = create_db "benchmark_db" in
  let _ = create_col "benchmark_db" "col1" in
  let _= create_col "benchmark_db" "col2" in
  let json_list_1  = benchmark_json_gen 20000 [] in
  let json_list_2 = benchmark_json_gen 20000 [] in
  let index_doc = `Assoc[ ("a", `Assoc[("$exists", `Bool true)])] in
  let query_doc = `Assoc[ ("a", `Int 5); ("b", `Int 10)] in
  List.map(fun f-> create_doc "benchmark_db" "col1" f) json_list_1;
  List.map(fun f-> create_doc "benchmark_db" "col2" f) json_list_2;
  let _ = create_index "benchmark_db" "col2" "a" index_doc in
  let t = Sys.time() in
  let _ = query_col "benchmark_db" "col1" query_doc in
  let time_query_1  = Sys.time() -. t in
  let t_query_2 = Sys.time() in
  let _ = query_col "benchmark_db" "col2" query_doc in
  let time_query_2 = Sys.time() -. t_query_2 in
  Success ("The time to run query 1 was: " ^ (string_of_float time_query_1) ^
           " the time to run query 2 was: " ^ (string_of_float time_query_2))

