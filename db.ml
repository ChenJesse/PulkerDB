open Yojson.Basic
open Persist

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
type dataEntry = (json * doc list) (*replace with a hashtable*)
type indexFile = {idName:string; idTable: (json,json) Hashtbl.t}
type indexList = indexFile list
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
  | CreateIndexResponse of bool * string

exception DropException
exception LocateDBException
exception LocateColException
exception InvalidUpdateDocException

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
*Function for comparing JSON Values with increasing order of priority (i.e. Lists are the largest)
*
*)
let compareJSON (val1:json) (val2:json) =
  match val1,val2 with
  |(`Null, `Null)-> 0
  |(`Null, _) -> -1
  |(_, `Null) -> 1
  |(`Int a , `Int b)-> if(a > b) then (1) else (if(a<b) then -1 else 0)
  |(`Int a, _)-> -1
  |(_, `Int b) -> 1
  |(`Float a, `Float b) -> if(a > b) then (1) else (if(a<b) then -1 else 0)
  |(`Float a, _)-> -1
  |(_, `Float b) -> 1
  |(`String a, `String b) -> if( a > b) then (1) else (if(a<b) then -1 else 0)
  |(`String a, _)-> -1
  |(_, `String b) -> 1
  |(`Bool a, `Bool b) -> if( a > b) then (1) else (if ( a < b ) then -1 else 0)
  |(`Bool a, _)-> -1
  |(_, `Bool b) -> 1
  |(`List a, `List b) -> if( a > b) then (1) else (if(a<b) then -1 else 0) (*The logic with this one might not be 100% right..*)


(**
* Function for sorting keys of my index in increasing order
*)
let keysort arr = Array.sort compareJSON arr
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
      | _ -> CreateDBResponse(false, "Problem with storing database")

(**
 * Given a string representation of JSON, creates a doc in the environment.
 * On failure, return false. On success, return true.
 *)
let create_doc db_name col_name doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let new_col = doc::col in
    Hashtbl.replace (fst db) col_name new_col;
    set_dirty (db_name);
    CreateDocResponse(true, "Success!")
  ) with
  | LocateDBException -> CreateDocResponse(false, (db_name ^ " was not found."))
  | LocateColException -> CreateDocResponse(false, (col_name ^ " was not found."))
  | _ -> CreateDocResponse(false, "Something went wrong with storing the document.")

(**
 * Given a string representing name of col, creates a col in the environment.
 * On failure, return false. On success, return true.
 *)
let create_col db_name col_name =
  let db = get_db db_name in
  match (Hashtbl.mem (fst db) col_name) with
  | true -> CreateColResponse(false, (col_name ^ " already exists."))
  | false -> try (
      Hashtbl.add (fst db) col_name [];
      CreateColResponse(true, "Success!")
    ) with
    | _ -> CreateColResponse(false, "Something went wrong with storing the collection.")

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
    |Some Exists -> let doc1= Util.member p_key doc in let doc2 = (snd) h in
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
  | _ -> failwith "Invalid query JSON"

(**
 * Given a string representing a query JSON, looks for matching docs in
 * the environment.
 * On failure, return false. On success, return true.
 *)
let query_col db_name col_name query_doc =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let query_result = List.filter (fun d -> check_doc d query_doc) col in
    let query_string = `List(query_result) |> pretty_to_string in
    QueryResponse(true, query_string)
  ) with
  | _ -> QueryResponse(false, "Query failed for some reason")

let createIndex db col index_name querydoc=
let col = (db |> get_db |> get_col col) in
    let query_result = List.filter (fun d-> check_doc d querydoc) (col) in (*(doublecheck if this is right) Get all the tuples with the attribute *)
     let table = Hashtbl.create 5 in(* Create a hashtable for loading *)
    let ctr = ref(0) in
    let len = List.length query_result in
    while(!ctr < len)
    do (
    let currentDoc = List.nth (query_result) (!ctr) in
    let t = Util.member index_name currentDoc in(*Load them all into the hashtable *)
    Hashtbl.add table t currentDoc;
    ctr:= !ctr+1;
  ) done;
    let t = {idName=index_name; idTable = table} in t
    (*The final index, table tuple*)
(**
 * Given a string representing name of col, shows a col in the environment.
 * On failure, return false. On success, return true.
 *)
let show_col db_name col_name =
  try (
    let col = (db_name |> get_db |> get_col col_name) in
    let contents = `List(col) |> pretty_to_string in
    ShowColResponse(true, contents)
  ) with
  | _ ->
    ShowColResponse(false, "Something went wrong with showing a collection")

let show_db db_name =
  try (
    let db_hashtbl = db_name |> get_db |> fst in
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) db_hashtbl [] in
    let contents = stringify_list contents_list in
    ShowDBResponse(true, contents)
  ) with
  | _ ->
    ShowDBResponse(false, "Something went wrong with dropping a collection")

let show_catalog () =
  try (
    let contents_list = Hashtbl.fold (fun k _ init -> k::init) environment [] in
    let contents = stringify_list contents_list in
    ShowCatalogResponse(true, contents)
  ) with
  | _ ->
    ShowCatalogResponse(false, "Something went wrong with dropping a collection")

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
  | DropException -> DropDBResponse(false, (db_name ^ " does not exist."))
  | _ -> DropDBResponse(false, "Something went wrong with dropping a db")

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
      DropColResponse(false, (col_name ^ " does not exist."))
  ) with
  | _ -> DropColResponse(false, "Something went wrong with dropping a collection")

(**
 * Given a doc representing criteria to query on, removes all
 * appropriate docs in the environment. On failure, return false.
 * On success, return true.
 *)
let remove_doc db_name col_name query_doc =
  try (
    let db = db_name |> get_db in
    let col = get_col col_name db in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) col in
    Hashtbl.replace (fst db) col_name new_col;
    set_dirty db_name;
    RemoveDocResponse(true, "Success!")
  ) with
  | _ -> RemoveDocResponse(false, "Something went wrong with removing documents")

(* -------------------------------UPDATING/REPLACING--------------------------------- *)

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let remove_and_get_doc db_name col_name query_doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let query = List.filter (fun d -> check_doc d query_doc) col in
    let new_col = List.filter (fun d -> not (check_doc d query_doc)) col in (* Keep docs that don't satisfy query_doc *)
    Hashtbl.replace (fst db) col_name new_col;
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
    let col = get_col col_name db in
    let _ = remove_doc db_name col_name query_doc in
    let new_col = update_doc::col in
    Hashtbl.replace (fst db) col_name new_col;
    set_dirty db_name;
    ReplaceDocResponse(true, "Success!")
  ) with
  | _ -> ReplaceDocResponse(false, "Something went wrong with replacing documents")

(**
 * Given a doc representing criteria to query on, updates all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let update_col db_name col_name query_doc update_doc =
  try (
    let db = get_db db_name in
    let col = get_col col_name db in
    let u_doc = match Util.member "$set" update_doc with
      | `Assoc json -> `Assoc json
      | _ -> raise InvalidUpdateDocException in
    let query = remove_and_get_doc db_name col_name query_doc in
    let new_col = col@(List.map (fun json -> (modify_doc json u_doc)) query) in
    Hashtbl.replace (fst db) col_name new_col;
    UpdateColResponse(true, "Success!")
  ) with
    | _ -> UpdateColResponse(false, "Invalid update document provided")

let clear_env () = Hashtbl.reset environment
