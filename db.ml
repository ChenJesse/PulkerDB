open Yojson.Basic
open Persist
open Tree

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

let get_db db_name : db =
  try (
    try (Hashtbl.find environment db_name) with
      | _ ->
        let (empty_db : db) = ((Hashtbl.create 100), false) in
        read_db db_name empty_db;
        empty_db
  ) with
  | _ -> raise LocateDBException

let get_col (col:string) (db:db) : col =
  try (
    let cols = db |> fst in
    Hashtbl.find cols col
  )
  with
  | _ -> raise LocateColException

let set_dirty db_name =
  let (db, _) = get_db db_name in
  Hashtbl.replace environment db_name (db, true)

let stringify_list lst =
  List.fold_left (fun acc ele -> acc ^ " [" ^ ele ^ "]") "" lst

let trpl_fst t = match t with
  | (a, _, _) -> a

let trpl_snd t = match t with
  | (_, b, _) -> b

let compareJSON a b =
  if a > b then 1
  else if a < b then -1
  else 0

(**
 * Function for sorting keys of my index in increasing order
 *)
let key_sort arr = Array.sort compareJSON arr
(* -------------------------------CREATION------------------------------- *)
(**
 * Given a string representing name of db, creates a db in the environment.
 * On failure, return false. On success, return true.
 *)
let create_db db_name =
  match (Hashtbl.mem environment db_name) with
  | true -> Failure "Database with same name already exists"
  | false -> try (
      let (empty_db:db) = (Hashtbl.create 100, false) in
      read_db db_name empty_db;
      add_db_env db_name empty_db;
      Success "Database loaded into memory!"
    ) with
    | NotInDisc ->
      add_db_env db_name (Hashtbl.create 100, true);
      Success "Database created successfully!"
    | _ -> Failure unexpected_error

(**
* Adds ogDoc to a index if one is found. Otherwise return nothing
*)
let rec index_changer ogDoc doc col = match doc with
  | [] -> ()
  | (k,v)::tl ->
    let loop_condition = true in
    let ctr = ref 0 in
    let id_list = snd col in
      while (!ctr < (List.length id_list) && loop_condition)
        do (
          let cur_index = List.nth id_list !ctr in
          if (cur_index.id_name) = k then (
            let tree = cur_index.keys in
            if(Tree.member v !tree)
            then
              if (((Tree.find v !tree) = [`Null] && v <>`Null) ||
                 ((Tree.find v !tree) = [`Int 0] && v <> `Int 0) )
              then tree:= Tree.insert v ogDoc (!tree) true
              else tree:= Tree.insert v ogDoc (!tree) false
            else tree := Tree.insert v ogDoc (!tree) false;
            loop_condition = false;
            ctr := !ctr + 1
          ) else ctr := !ctr + 1
        ) done;
    index_changer ogDoc tl col

(**
 * Given the doc, update the index
 * if that doc's attribute matches a declared index field.
 *)
let rec index_updater ogDoc doc col = match doc with
  |`Assoc a -> (
    match a with
    | ((b : string),(c : Tree.key))::tl -> index_changer ogDoc a col
    | _ -> ()
  )
  |_ -> ()

(**
 * Returns the tree associated with the specified index in the index desired.
 * Returns empty if no index can be found.
 *)
let rec get_index index_name index =
  match index with
  | [] -> Tree.empty
  | { id_name = a; id_table =_; keys= c }::tl ->
    if a = index_name then !c else get_index index_name tl

(**
 * Returns the values associated with the specified key in the desired index
 * Returns empty list if nothing can be found.
 *)
let  get_values value index_tree =
let tree_as_list = Tree.to_list index_tree in
let rec helper tree_list value =
  match tree_list with
  |[] -> []
  |(k,v)::tl -> if(value = k) then v else helper tl value
in helper tree_as_list value

(**
 * Given a string representation of JSON, creates a doc in the environment.
 * On failure, return false. On success, return true.
 *)
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


(**
 * Given a string representing name of col, creates a col in the environment.
 * On failure, return false. On success, return true.
 *)
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
 * Given a doc (json), extracts the value into OCaml primitive
 *)
let rec get_converter (doc1 : doc) (doc2 : doc) =
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

let compare op item1 item2 =
  match (get_converter item1 item2) with
  | ToInt x -> compare_int op item1 item2 (ToInt(x))
  | ToBool x -> compare_bool op item1 item2 (ToBool(x))
  | ToString x -> compare_string op item1 item2 (ToString(x))
  | ToFloat x -> compare_float op item1 item2 (ToFloat(x))

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
 * Depending on what type of query it is, it will then call traverse with certain bounds on the tree *)
let index_query_builder col_tree query_doc =
  let rec helper col_tree query_doc = match query_doc with
    | h::t ->
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
      | None -> match (nested_json (snd h)) with
        | true -> (
          (* We have a doc as the value, need to recurse *)
          (* Represents the nested doc in the query_doc *)
          let nested = match (snd h) with
            | `Assoc lst -> lst
            | _ -> failwith "Can't be here" in
          (* If it's a comparator JSON, we only recurse a level in on doc (nested) *)
          if h |> snd |> comparator_json then helper col_tree nested else []
        )
        (* We have an equality check *)
        | false -> find (snd h) col_tree
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
      | true -> (* We have a doc as the value, need to recurse *)
        (* Represents the nested doc in the query_doc *)
        let nested = match (snd h) with
          | `Assoc lst -> lst
          | _ -> failwith "Can't be here" in
        (* If it's a comparator JSON, we only recurse a level in on doc (nested) *)
        if (comparator_json (snd h)) then helper doc nested (fst h) true
        |> helper doc t p_key
        (* If it's a normal JSON, we only recurse a level in on doc and query_doc *)
        else (try (helper (Util.member (fst h) doc) nested (fst h) true
              |> helper doc t p_key) with | _ -> false)
      | false -> (* We have a simple equality check *)
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
* return whatever the collection's list of docs are if no index can be matched
*)
let index_checker col query_list =
  (* match query_list with
  | [] -> fst col
  | h::t ->
    let index_list = snd col in
    let index = get_index (fst h) index_list in
    if index <> Tree.empty then
      index_query_builder index (`Assoc [h])
    else
      index_checker col t *)
  let ctr = ref(0) in
  let index_list = (snd) col in
  let docs = ref([]) in
  let break_condition = ref(false) in
  while (!break_condition = false & !ctr < List.length query_list)
  do (
    let index = get_index (fst (List.nth query_list !ctr)) index_list in
    if index <> Tree.empty then (
      docs := index_query_builder index (`Assoc [List.nth query_list !ctr]);
      break_condition := true;
    ) else ctr := !ctr + 1
  ) done;
  if !docs = [] then (docs := (fst col); !docs) else !docs

(**
 * Given a string representing a query JSON, looks for matching docs in
 * the environment.
 * On failure, return false. On success, return true.
 *)
let query_col db_name col_name query_doc =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let lst_queries = (match query_doc with |`Assoc lst -> lst |_ -> [] ) in
    let documents = index_checker col lst_queries in
    let query_result = List.filter (fun d -> check_doc d query_doc) documents in
    let query_string = `List(query_result) |> pretty_to_string in
    Success query_string
  ) with
  | InvalidQueryDocException -> Failure "Invalid query doc. Refer to -query_doc for more information."
  | LocateDBException -> Failure(db_find_error db_name)
  | LocateColException -> Failure(col_find_error col_name)
  | _ -> Failure unexpected_error

(**
 * Extract all the keys associated with this List, removing duplicates as we go.
 *)
let rec extract_keys list_tbl key_list =
  match list_tbl with
  | [] -> key_list
  | (k,v)::t ->
    if List.mem k key_list then extract_keys t key_list
    else extract_keys t (k::key_list)

(**
 * Return the keySet for my hashtable, tbl. That is, a set with only unique keys.
 *)
let key_set tbl =
  let list_tbl = Hashtbl.fold (fun k v acc-> (k,v)::acc) tbl [] in
  let final_list = extract_keys list_tbl [] in
  Array.of_list final_list

(**
 * Given the database, collection, desired index_name and querydoc, creates a index
 *)
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
 *)
let rec recreate_index db_name col_name index_list new_list=
  match index_list with
  |[]-> new_list
  |{id_name= name; id_table= idtable; keys= tree}::tl->
      let query_doc = `Assoc [(name, `Assoc[("$exists", `Bool true)])] in
      create_index db_name col_name name query_doc;
      let new_index_list = (snd) (db_name |> get_db |> get_col col_name) in
      let newTree = get_index name new_index_list in
      {id_name = name; id_table = idtable; keys = ref(newTree)}::new_list
(**
 * Given a string representing name of col, shows a col in the environment.
 * On failure, return false. On success, return true.
 *)
let show_col db_name col_name =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let contents = `List((fst)col) |> pretty_to_string in
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
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) environment [] in
    let contents = stringify_list contents_list in
    Success contents
  ) with
  | _ -> Failure unexpected_error

(* -------------------------------AGGREGATION--------------------------------- *)

let bucketize db_name col_name attr =
  let col = (db_name |> get_db |> get_col col_name) in
  let buckets = Hashtbl.create 20 in
  List.iter (fun doc ->
    let value = Util.member attr doc in
    if Hashtbl.mem buckets value then
      let bucket = Hashtbl.find buckets value in
      Hashtbl.replace buckets value (doc::bucket)
    else
      Hashtbl.add buckets value [doc]
  ) (fst col);
  buckets

let aggregator_attr bucket op t_field =
  match op with
  | "$sum" -> `Int (List.fold_left (fun acc doc -> let v = Util.(member t_field doc |> to_int) in
                              acc + v) 0 bucket)
  | "$max" -> `Int (List.fold_left (fun max doc -> let v = Util.(member t_field doc |> to_int) in
                              if v > max then v else max) min_int bucket)
  | "$min" -> `Int (List.fold_left (fun min doc -> let v = Util.(member t_field doc |> to_int) in
                              if v < min then v else min) max_int bucket)
  | _ -> failwith "Aggregator failed"

(**
 * For summing up with constants for a count of buckets
 *)
let aggregator_const bucket op t_field =
  match op with
  | "$sum" -> `Int (List.fold_left (fun acc doc -> acc + t_field) 0 bucket)
  | _ -> failwith "Aggregator failed"

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

(*db.mycol.aggregate({_id : "$by_user", num_tutorial : {$sum : "$likes"}})*)
let aggregate db_name col_name agg_doc =
  try (
    let bucket_attr = Util.(member "_id" agg_doc |> to_string) in
    let buckets = bucketize db_name col_name bucket_attr in
    (* Each iteration of the helper will go through a bucket and create the aggregated json *)
    match agg_doc with
    | `Assoc lst ->
      let acc = ref [] in
      let filtered = List.filter (fun pair -> (fst pair) <> "_id") lst in
      aggregation_helper acc filtered buckets;
      let agg_string = `List(!acc) |> pretty_to_string in
      Success agg_string
    | _ -> Failure "Error with aggregating response. Refer to -agg_doc for more information."
  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with aggregating response. Refer to -agg_doc for more information."

(* -------------------------------REMOVING--------------------------------- *)

(**
 * Given a string representing name of db, drops a db in the environment.
 * On failure, return false. On success, return true.
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
 * Given a string representing name of col, drops a col in the environment.
 * On failure, return false. On success, return true.
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

let rec replace_tree  index_list doc =
match index_list with
|[]-> ()
|{id_name=name; id_table=table; keys = tree}::tl->
  let index_val = Util.member name doc in
  if(index_val <> `Null) then
    let doc_list = Tree.find index_val !tree in
    let new_doc_list = List.filter (fun f-> f <> doc) doc_list in
    if(List.length new_doc_list = 0)
    then tree:= Tree.replace index_val `Null !tree true
    else if (List.length new_doc_list = 1)
    then tree:= Tree.replace index_val (List.nth new_doc_list 0) !tree true
    else
      let zerothdoc = List.nth new_doc_list 0 in
      let nonzerodocs = List.filter (fun f-> f <> zerothdoc) new_doc_list in
      tree:= Tree.replace index_val zerothdoc !tree true;
      List.iter (fun ele -> tree := Tree.insert index_val ele !tree false) nonzerodocs
  else
    if(Tree.member index_val !tree)
    then (
      let new_doc_list = List.filter (fun f-> f<> doc ) (Tree.find index_val !tree) in
      if(List.length new_doc_list = 0)
      then tree:= Tree.replace index_val (`Int 0) !tree true
      else
      (
        let zerothdoc = List.nth new_doc_list 0 in
        let nonzerodocs = List.filter (fun f-> f <> zerothdoc) new_doc_list in
        tree:= Tree.replace index_val zerothdoc !tree true;
        List.iter (fun ele -> tree := Tree.insert index_val ele !tree false) nonzerodocs
      )
         )
     else replace_tree tl doc

(**
 * Given a doc representing criteria to query on, removes all
 * appropriate docs in the environment. On failure, return false.
 * On success, return true.
 *)


let remove_doc db_name col_name query_doc =
  try (
    let db = db_name |> get_db in
    let col = get_col col_name db in
    let index_list = (snd) col in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) ((fst)col) in
    Hashtbl.replace (fst db) col_name (new_col,[]);
    let lost_docs = List.filter (fun d->  (check_doc d query_doc)) ((fst)col) in
    List.iter (replace_tree index_list) lost_docs;
    Hashtbl.replace (fst db) col_name (new_col, (snd) col);
    set_dirty db_name;
    Success "Removed document successfully!"



  ) with
  | LocateDBException -> Failure (db_find_error db_name)
  | LocateColException -> Failure (col_find_error col_name)
  | _ -> Failure "Error with removing documents. Refer to -query_doc for more information."

(* -------------------------------UPDATING/REPLACING--------------------------------- *)

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let remove_and_get_doc db_name col_name query_doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let index_list = (snd) col in
    let query = List.filter (fun d -> check_doc d query_doc) (fst col) in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) (fst col) in
    Hashtbl.replace (fst db) col_name (new_col,[]);
    List.iter (replace_tree index_list) query;
    Hashtbl.replace (fst db) col_name (new_col, (snd) col);
    query
       ) with
  | _ -> []

(**
 * Responsible for updating a document, given an update document
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
          | true -> (
            match snd pair with
            | `Assoc _ -> (fst pair, modify_doc (snd pair) u_value)
            | _ -> (fst pair, u_value)
          )
          | false -> pair) lst)
      )
    else `Assoc ((u_key, u_value)::lst)
  in
  match update_doc with
  | `Assoc pairs -> List.fold_left (fun acc pair -> helper acc pair) doc pairs
  | _ -> raise InvalidUpdateDocException

(**
 * Given a doc representing criteria to query on, removes all appropriate docs,
 * and then inserts the given doc. On failure, return false. On success, return true.
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

(**
 * Given a doc representing criteria to query on, updates all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
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
