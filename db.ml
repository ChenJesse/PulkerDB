open Yojson.Basic
open Persist
open Tree
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

type response =
  | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | ReplaceDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string
  | ParseErrorResponse of bool * string
  | ShowColResponse of bool * string
  | UpdateColResponse of bool * string
  | ShowDBResponse of bool * string
  | ShowCatalogResponse of bool * string
  | AggregateResponse of bool * string
  | CreateIndexResponse of bool * string


exception DropException
exception LocateDBException
exception LocateColException
exception InvalidUpdateDocException
exception InvalidAggDocException
exception InvalidQueryDocException

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
    | true -> CreateDBResponse(false, "Database with same name already exists")
    | false -> try (
        let (empty_db:db) = (Hashtbl.create 100, false) in
        read_db db_name empty_db;
        add_db_env db_name empty_db;
        CreateDBResponse(true, "Success!")
      ) with
      | NotInDisc ->
        add_db_env db_name (Hashtbl.create 100, false);
        CreateDBResponse(true, "Success!")
      | _ -> CreateDBResponse(false, unexpected_error)

 (**
  * Adds ogDoc to a index if one is found. Otherwise return nothing
  *)
 let rec index_changer ogDoc doc col = match doc with
    |[] -> ()
    |(k,v)::tl->  let loop_condition = true in
                  let ctr = ref(0) in
                  let id_list = (snd) col in
                    while(!ctr < (List.length id_list)
                        && loop_condition = true) do (

                        let cur_index = List.nth id_list !ctr in
                        if((cur_index.id_name) = k) then (

                          print_endline "found a match and changing it";
                          let tree = cur_index.keys in
                          print_endline (string_of_int (Tree.size !tree));
                          tree:= (Tree.insert (v) ([ogDoc]) (!tree) );
                          print_endline (string_of_int (Tree.size !tree));
                          loop_condition = false;
                          ctr:=!ctr+1;
                          ()
                       )
                        else (
                            ctr:= !ctr+1
                        )
                      ) done; index_changer ogDoc tl col

(**
 * Given the doc, update the index
 * if that doc's attribute matches a declared index field.
 *)
  let rec index_updater ogDoc doc col = match doc with
    |`Assoc a->( match a with |((b:string),(c:Tree.key))::tl -> index_changer ogDoc a col)
    |_ -> ()




(**
* Returns the tree associated with the specified index in the index desired. Returns empty if no index can be found.
*)

 let rec get_index index_name index =match index with
 | []->Tree.empty
 | {id_name = a; id_table =_; keys= c}::tl->
        if(a = index_name) then( !c )else get_index index_name tl

(**
 * Given a string representation of JSON, creates a doc in the environment.
 * On failure, return false. On success, return true.
 *)
let create_doc db_name col_name doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let col_list = (fst)col in
    let new_col = doc::col_list in
    index_updater doc doc col;
    let new_colIndex = (new_col, (snd) col) in
    Hashtbl.replace (fst db) col_name new_colIndex;
    set_dirty (db_name);
    CreateDocResponse(true, "Success!")
  ) with
  | LocateDBException -> CreateDocResponse(false, db_find_error db_name)
  | LocateColException -> CreateDocResponse(false, col_find_error col_name)
  | _ -> CreateDocResponse(false, unexpected_error)

(**
 * Given a string representing name of col, creates a col in the environment.
 * On failure, return false. On success, return true.
 *)
let create_col db_name col_name =
  try (
    let db = get_db db_name in
    match (Hashtbl.mem (fst db) col_name) with
    | true -> CreateColResponse(false, (col_name ^ " already exists."))
    | false ->
        Hashtbl.add (fst db) col_name ([],[]);
        CreateColResponse(true, "Success!")
  ) with
  | LocateDBException -> ShowColResponse(false, db_find_error db_name)
  | _ -> CreateColResponse(false, unexpected_error)

(* -------------------------------QUERYING-------------------------------- *)
(* Returns true if the doc is a nested json*)
let nested_json doc = match doc with
| `Assoc _ -> true
| _ -> false

(* Returns true if the doc is a comparator json ex. "{key: {'$lte', 5}}" *)
let comparator_json doc = match doc with
| `Assoc lst -> let k = List.hd lst |> fst in (String.get k 0) = '$'
| _ -> false

(**
 * Given a doc (json), extracts the value into OCaml primitive
 *)
let rec get_converter (doc1 : doc) (doc2 : doc) = match doc1, doc2 with
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

(** query_doc is guaranteed to have the field this index tree is built on.
 * collectionTree is the index for the attribute specified.
 *This needs to parse the query and figure out what type of query it is.
 *Depending on what type of query it is, it will then call traverse with certain bounds on the tree *)
let index_query_builder collectionTree query_doc = (
   let rec helper collectionTree query_doc = match query_doc with
    |h::t ->
      let comparator = match (fst h) with
      | "$lt" -> Some Less
      | "$lte" -> Some LessEq
      | "$gt" -> Some Greater
      | "$gte" -> Some GreaterEq
      | "$ne" -> Some NotEq
      | _ -> None
    in
    let max_temp = `List[`Int 99999999999999] in
    match comparator with
    | Some Less ->    let doc2 = snd h in (find_docs collectionTree doc2 `Null)
    | Some Greater -> let doc2 = snd h in (find_docs collectionTree (max_temp) doc2)
    | Some LessEq -> (let doc2  = snd h in
                      let real_doc2 = (match doc2 with
                                | `Null -> `Null
                                | `Int a -> let b = (a+1) in (`Int b)
                                | `Float a -> let b = a +.1.0 in (`Float b)
                                | `String a -> (let d = String.get a ((String.length a)-1) in
                                               let code = (Char.code d)+1 in
                                               let charNew = Char.chr code in
                                               let new_string = String.concat ""
                                               [(String.sub a 0 ((String.length a)-1));
                                               (String.make 1 charNew)] in
                                              `String new_string)
                                | `Bool a ->  `Bool a

                              )
                              in
                      find_docs collectionTree real_doc2 `Null)
    | Some GreaterEq -> (let doc2  = snd h in
                         let real_doc2 = (match doc2 with
                                | `Null -> `Null
                                | `Int a -> let b = (a-1) in (`Int b)
                                | `Float a -> let b = a -.1.0 in (`Float b)
                                | `String a -> (let d = String.get a ((String.length a)-1) in
                                              let code = (Char.code d)-1 in
                                              let charNew = Char.chr code in
                                              let newString =
                                              String.concat "" [(String.sub a 0 ((String.length a)-1)); (String.make 1 charNew)] in
                                              `String newString)
                                | `Bool a ->  `Bool a

                              )
                              in
                    find_docs collectionTree (max_temp) real_doc2)
    | Some NotEq->  let doc2 = snd h in let first_doc_list =(find_docs collectionTree doc2 `Null) in
                 let sec_doc_list =  (find_docs collectionTree (max_temp) doc2) in
                 first_doc_list@sec_doc_list

    | None -> ( match (nested_json (snd h)) with
      | true -> ((* We have a doc as the value, need to recurse *)
        (* Represents the nested doc in the query_doc *)
        let nested = match (snd h) with
          | `Assoc lst -> lst
          | _ -> failwith "Can't be here" in
        (* If it's a comparator JSON, we only recurse a level in on doc (nested) *)
        if (comparator_json (snd h)) then (helper collectionTree nested)
      else  [])
            (*WE have an equality check *)
      | false -> (let doc2 = (snd) h in
                let real_doc2_low= (match doc2 with
                                |`Null -> `Null
                                |`Int a -> let b = (a-1) in (`Int b)
                                |`Float a -> let b = a -.1.0 in (`Float b)
                                |`String a -> (let d = String.get a ((String.length a)-1) in
                                              let code = (Char.code d)-1 in
                                              let charNew = Char.chr code in
                                              let newString =
                                              String.concat "" [(String.sub a 0 ((String.length a)-1)); (String.make 1 charNew)] in
                                              `String newString)
                                |`Bool a ->  `Bool a

                              ) in
                let real_doc2_high = (match doc2 with
                                |`Null -> `Null
                                |`Int a -> let b = (a+1) in (`Int b)
                                |`Float a -> let b = a +.1.0 in (`Float b)
                                |`String a -> (let d = String.get a ((String.length a)-1) in
                                              let code = (Char.code d)+1 in
                                              let charNew = Char.chr code in
                                              let newString =
                                              String.concat "" [(String.sub a 0 ((String.length a)-1)); (String.make 1 charNew)] in
                                              `String newString)
                                |`Bool a ->  `Bool a

                              ) in
                find_docs collectionTree real_doc2_high real_doc2_low ) )

   in
  match query_doc with
  | `Assoc lst -> helper collectionTree lst
  | _ -> failwith "Invalid query JSON"
)

let check_doc doc query_doc =
  let rec helper doc query_doc p_key acc = match acc, query_doc with
  | (false, _) -> false
  | (_, []) -> acc
  | (_, h::t) ->
    let comparator = match (fst h) with
      | "$lt" -> Some Less
      | "$lte" -> Some LessEq
      | "$gt" -> Some Greater
      | "$gte" -> Some GreaterEq
      | "$ne" -> Some NotEq
      | "$exists" -> Some Exists
      | _ -> None
    in
    match comparator with
    | Some Exists -> print_endline "came into this"; let doc1= Util.member p_key doc in let doc2 = (snd) h in
    if((doc1 <> `Null && doc2 =`Bool true) || (doc1 = `Null && doc2 = `Bool false)) then true else false
    | Some c ->
      let doc1 = Util.member p_key doc in
      let doc2 = snd h in
      (try ((match (get_converter doc1 doc2) with
        | ToInt x -> compare_int c doc1 doc2 (ToInt(x))
        | ToBool x -> compare_bool c doc1 doc2 (ToBool(x))
        | ToString x -> compare_string c doc1 doc2 (ToString(x))
        | ToFloat x -> compare_float c doc1 doc2 (ToFloat(x))
      )) with | _ -> false)
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
        let doc1 = Util.member (fst h) doc in
        let doc2 = snd h in
        (try (
          let outcome = (match (get_converter doc1 doc2) with
            | ToInt x -> compare_int Eq doc1 doc2 (ToInt(x))
            | ToBool x -> compare_bool Eq doc1 doc2 (ToBool(x))
            | ToString x -> compare_string Eq doc1 doc2 (ToString(x))
            | ToFloat x -> compare_float Eq doc1 doc2 (ToFloat(x)))
          in
          helper doc t p_key outcome
        ) with | _ -> false)
      )
  in
  match query_doc with
  | `Assoc lst -> helper doc lst "" true
  | _ -> raise InvalidQueryDocException

(**
 * Converts the doc list returned by index checker into a normal list of docs rather than a nested list.
 *)
 let demistify lst =
   let final_result = ref([]) in
   let ctr = ref(0) in
   while (!ctr < List.length lst) do (final_result:= (List.nth (List.nth lst !ctr) 0)::!final_result; ctr:=!ctr+1) done;
   !final_result

 (**
  * Checks if there are any indices that match the current queries field.
  * IF there are, we want to get teh docs associated with this query from the index
  * And return those after converting to a normal doc list.
  * Otherwise, continue and ultimately just
  * return whatever the collection's list of docs are if no index can be matched
  *)
 let index_checker (col:Persist.col) queryList =
    let ctr = ref(0) in
    let index_list = (snd) col in
    let docs = ref([]) in
    let break_condition = ref(false) in
    while(!break_condition = false & !ctr < List.length queryList)
    do (
         let index = (get_index ((fst) (List.nth queryList !ctr)) index_list) in
         if(index<>Tree.empty)
          then (
             docs := demistify (index_query_builder index (`Assoc [List.nth queryList !ctr]));
            break_condition := true;
               )
       else (
              ctr:=!ctr+1
            )

    ) done;
    (if(!docs = [])
    then  (docs:= ((fst) col); !docs)
    else (!docs))


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
    QueryResponse(true, query_string)
  ) with
  | InvalidQueryDocException -> ShowColResponse(false, "Invalid query doc. Refer to -query_doc for more information.")
  | LocateDBException -> ShowColResponse(false, db_find_error db_name)
  | LocateColException -> ShowColResponse(false, col_find_error col_name)
  | _ -> QueryResponse(false, unexpected_error)

(**
 * Extract all the keys associated with this List, removing duplicates as we go.
 *)
let rec extract_keys listTbl keyList =
  match listTbl with
  |[]-> keyList
  |(k,v)::tl -> if(List.mem k keyList)
                then (extract_keys tl keyList)
                else (extract_keys tl (k::keyList))

(**
* Return the keySet for my hashtable, tbl. That is, a set with only unique keys.
*)
let key_set tbl =
  let list_tbl = Hashtbl.fold (fun k v acc-> (k,v)::acc) tbl [] in
  let final_list = extract_keys list_tbl [] in
  let arr_new = Array.make (List.length final_list) `Null in
  let ctr = ref(0) in
  while(!ctr<List.length final_list)
  do (
       Array.set arr_new !ctr (List.nth final_list !ctr);
       ctr:= !ctr +1
      ) done;
  arr_new

(**
* Given the database, collection, desired index_name and querydoc, creates a index
*
*)
let create_index db col_name index_name querydoc=
    let col = (db |> get_db |> get_col col_name) in
    let query_result = List.filter (fun d-> check_doc d querydoc) ((fst)(col)) in
    if(List.length query_result = 0)
      then (
          CreateIndexResponse(false, "no docs matched the desired field")
            )
    else ( (*(doublecheck if this is right) Get all the tuples with the attribute *)
      let table = Hashtbl.create 5 in(* Create a hashtable for loading *)
      let ctr = ref(0) in
      let len = List.length query_result in
      let pam = print_string index_name in
      while(!ctr < len)
      do (
        let current_doc = List.nth (query_result) (!ctr) in
        let t = Util.member index_name current_doc in(*Load them all into the hashtable *)
        Hashtbl.add table t current_doc;
        ctr:= !ctr+1;
        ) done;
      let keys_tb =  (key_set table) in
      key_sort (keys_tb);
      let ctr2 = ref(0) in
      let len = Array.length keys_tb in
      let tree = ref(Tree.empty) in
      while(!ctr2 < len)
      do (
          let ctr3 = ref(0) in
          let tbl_list = Hashtbl.find_all table (Array.get keys_tb !ctr2)  in
          while (!ctr3 < List.length tbl_list)
          do (

                tree:=(Tree.insert (Array.get keys_tb !ctr2)  ([List.nth tbl_list !ctr3] ) !tree);
                ctr3 := !ctr3+1

            ) done;
          ctr2:= !ctr2+1
        ) done;

      let t = {id_name=index_name; id_table = table; keys = tree} in
      let id_list = t::((snd) col) in
      let new_col = ((fst)col, id_list) in
      let old_db = db |> get_db in
      Hashtbl.replace ((fst)old_db) col_name new_col;
      !tree;
      CreateIndexResponse(true, "Index was successfully made!")
    )
      (* remove this print later. *)


(**
 * Given a string representing name of col, shows a col in the environment.
 * On failure, return false. On success, return true.
 *)
let show_col db_name col_name =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let contents = `List((fst)col) |> pretty_to_string in
    ShowColResponse(true, contents)
  ) with
  | LocateDBException -> ShowColResponse(false, db_find_error db_name)
  | LocateColException -> ShowColResponse(false, col_find_error col_name)
  | _ -> ShowColResponse(false, unexpected_error)

let show_db db_name =
  try (
    let db_hashtbl = db_name |> get_db |> fst in
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) db_hashtbl [] in
    let contents = stringify_list contents_list in
    ShowDBResponse(true, contents)
  ) with
  | LocateDBException -> ShowDBResponse(false, db_find_error db_name)
  | _ -> ShowDBResponse(false, unexpected_error)

let show_catalog () =
  try (
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) environment [] in
    let contents = stringify_list contents_list in
    ShowCatalogResponse(true, contents)
  ) with
  | _ -> ShowCatalogResponse(false, unexpected_error)

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
  ) ((fst)col);
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
      AggregateResponse(true, agg_string)
    | _ -> AggregateResponse(false, "Error with aggregating response. Refer to -agg_doc for more information.")
  ) with
  | LocateDBException -> AggregateResponse(false, db_find_error db_name)
  | LocateColException -> AggregateResponse(false, col_find_error col_name)
  | _ -> AggregateResponse(false, "Error with aggregating response. Refer to -agg_doc for more information.")

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
      DropDBResponse(true, "Success!")
    ) else (
      raise DropException
    )
  ) with
  | NotInDisc -> DropDBResponse(true, "Success!")
  | DropException -> DropDBResponse(false, db_find_error db_name)
  | _ -> DropDBResponse(false, unexpected_error)

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
      DropColResponse(true, "Success!")
    ) else
      DropColResponse(false, col_find_error col_name)
  ) with
  | _ -> DropColResponse(false, unexpected_error)

(**
 * Given a doc representing criteria to query on, removes all
 * appropriate docs in the environment. On failure, return false.
 * On success, return true.
 *)
let remove_doc db_name col_name query_doc =
  try (
    let db = db_name |> get_db in
    let col = get_col col_name db in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) ((fst)col) in
    Hashtbl.replace (fst db) col_name (new_col,[]);
    set_dirty db_name;
    RemoveDocResponse(true, "Success!")
  ) with
  | LocateDBException -> RemoveDocResponse(false, db_find_error db_name)
  | LocateColException -> RemoveDocResponse(false, col_find_error col_name)
  | _ -> RemoveDocResponse(false, "Error with removing documents. Refer to -query_doc for more information.")

(* -------------------------------UPDATING/REPLACING--------------------------------- *)

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let remove_and_get_doc db_name col_name query_doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let query = List.filter (fun d -> check_doc d query_doc) ((fst)col) in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) ((fst)col) in (* Keep docs that don't satisfy query_doc *)
    Hashtbl.replace (fst db) col_name (new_col,[]);
    query
  ) with
  | _ -> []

(**
 * Responsible for updating a document, given an update document
 *)
let rec modify_doc doc update_doc =
  let helper doc u_doc =
    let (u_key, u_value) = match u_doc with
      | `Assoc lst -> List.hd lst
      | _ -> raise InvalidUpdateDocException
    in
    let lst = match doc with
      | `Assoc lst -> lst
      | _ -> failwith "Should not be here"
    in
    (* Constructing the updated doc *)
    `Assoc (
      List.map (fun pair -> match (fst pair) = u_key with
        | true -> (
              match snd pair with
              | `Assoc _ -> (fst pair, modify_doc (snd pair) u_value)
              | _ -> (fst pair, u_value)
            )
        | false -> pair) lst
    )
  in
  match update_doc with
  | `Assoc _ -> helper doc update_doc (* Should only have one assoc pair *)
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
    set_dirty db_name;
    ReplaceDocResponse(true, "Success!")
  ) with
  | LocateDBException -> RemoveDocResponse(false, db_find_error db_name)
  | LocateColException -> RemoveDocResponse(false, col_find_error col_name)
  | _ -> ReplaceDocResponse(false, "Error with replacing doc. Ensure that the query document
    is in the correct format. Refer to -query_doc for more information.")

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
    let new_col = ((fst)col)@(List.map (fun json -> (modify_doc json u_doc)) query) in
    Hashtbl.replace (fst db) col_name (new_col,[]);
    UpdateColResponse(true, "Success!")
  ) with
  | LocateDBException -> RemoveDocResponse(false, db_find_error db_name)
  | LocateColException -> RemoveDocResponse(false, col_find_error col_name)
  | _ -> UpdateColResponse(false, "Error with updating collection.
    Refer to -query_doc and -update_doc for more information.")

let clear_env () = Hashtbl.reset environment
