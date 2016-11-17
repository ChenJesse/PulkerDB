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
type doc = Yojson.Basic.json

type col = (string * doc list) ref

type db = (string * col list) ref

type catalog = (db list) ref

type response = CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string
  | ParseErrorResponse of bool * string

exception DropException
exception LocateDBException
exception LocateColException

type converter = ToInt of (doc -> int) | ToString of (doc -> string)
  | ToBool of (doc -> bool) | ToFloat of (doc -> float)

type opWrapper = Less | LessEq | Greater | GreaterEq | NotEq | Eq

let environment : catalog = ref []

let get_db_ref db =
  try (
    try (List.filter (fun x -> (fst !x) = db) !environment |> List.hd) with
      | _ -> read_db db; (List.filter (fun x -> (fst !x) = db) !environment |> List.hd)
  ) with | _ -> raise LocateDBException

let get_col_ref (col:string) (db:db) =
  try (!db |> snd |> List.filter (fun x -> (fst !x) = col) |> List.hd) with
    | _ -> raise LocateColException

(**
 * Given a string representation of JSON, creates a doc in the environment.
 * On failure, return false. On success, return true.
 *)
let create_doc db col doc =
  try (
    let col_ref = db |> get_db_ref |> get_col_ref col in
    col_ref := (fst !col_ref, doc::(snd !col_ref));
    CreateDocResponse(true, "Success!")
  ) with
  | LocateDBException -> CreateDocResponse(false, (db ^ " was not found."))
  | LocateColException -> CreateDocResponse(false, (col ^ " was not found."))
  | _ -> CreateDocResponse(false, "Something went wrong with storing the document.")


(**
 * Given a string representing name of db, creates a db in the environment.
 * On failure, return false. On success, return true.
 *)
let create_db db =
  match (List.exists (fun x -> (fst !x) = db) !environment) with
    | true -> CreateDBResponse(false, "Database with same name already exists")
    | false -> try (
        environment := (ref (db, []))::(!environment);
        CreateDBResponse(true, "Success!")
      ) with
      | _ -> CreateDBResponse(false, "Problem with storing database")

(**
 * Given a string representing name of col, creates a col in the environment.
 * On failure, return false. On success, return true.
 *)
let create_col db col =
  let db = get_db_ref db in
  match (snd !db |> List.exists (fun x -> (fst !x) = col)) with
    | true -> CreateColResponse(false, (col ^ " already exists."))
    | false -> try (
        db := (fst !db, (ref (col, []))::(snd !db));
        CreateColResponse(true, "Success!")
      ) with
      | _ -> CreateColResponse(false, "Something went wrong with storing the collection.")

(* Returns true if the doc is a nested json*)
let nested_json doc = match doc with
  | `Assoc _ -> true
  | _ -> false

(* Returns true if the doc is a comparator json ex. "{key: {'$lte', 5}}" *)
let comparator_json doc = match doc with
  | `Assoc lst -> let k = List.hd lst |> fst in (String.get k 0) = '$'
  | _ -> false

(**
 * Given a doc (json), extracts the value into OCaml primitive (or list of primitives)
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
        | _ -> None
      in
      match comparator with
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
            let nested = match (snd h) with | `Assoc lst -> lst | _ -> failwith "Can't be here" in
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
 * Given a string representing name of db, drops a db in the environment.
 * On failure, return false. On success, return true.
 *)
let drop_db db =
  try (
    let env = !environment in
    let db_exists = List.exists (fun d -> fst !d = db) env in
    if db_exists then (
      environment := List.filter (fun d -> fst !d <> db) env;
      DropDBResponse(true, "Success!")
    ) else (
      raise DropException
    )
  ) with
    | DropException -> DropDBResponse(false, (db ^ " does not exist."))
    | _ -> DropDBResponse(false, "Something went wrong with dropping a db")

(**
 * Given a string representing name of col, drops a col in the environment.
 * On failure, return false. On success, return true.
 *)
let drop_col db col =
  try (
    let db_ref = get_db_ref db in
    let db = !db_ref in
    let col_exists = List.exists (fun c -> (fst !c = col)) (snd db) in
    if col_exists then (
      db_ref := (fst db, List.filter (fun c -> (fst !c <> col)) (snd db));
      DropColResponse(true, "Success!")
    ) else
      DropColResponse(false, (col ^ " does not exist."))
  ) with
    | _ -> DropColResponse(false, "Something went wrong with dropping a collection")

(**
 * Given a doc representing criteria to query on, updates all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let update_col db col query_doc =
  try (
    let col_ref = db |> get_db_ref |> get_col_ref col in
    let col = !col_ref in
    col_ref := (fst col, List.filter (fun d -> not (check_doc d query_doc)) (snd col)); (* Keep docs that don't satisfy query_doc *)
    RemoveDocResponse(true, "Success!")
  ) with
    | _ -> RemoveDocResponse(false, "Something went wrong with removing documents")

(**
 * Given a doc representing criteria to query on, removes all appropriate docs in the environment.
 * On failure, return false. On success, return true.
 *)
let remove_doc db col query_doc =
  try (
    let col_ref = db |> get_db_ref |> get_col_ref col in
    let col = !col_ref in
    col_ref := (fst col, List.filter (fun d -> not (check_doc d query_doc)) (snd col)); (* Keep docs that don't satisfy query_doc *)
    RemoveDocResponse(true, "Success!")
  ) with
    | _ -> RemoveDocResponse(false, "Something went wrong with removing documents")

(**
 * Given a string representing a query JSON, looks for matching docs in the environment.
 * On failure, return false. On success, return true.
 *)
let query_col db col query_doc =
  try (
    let col = !(db |> get_db_ref |> get_col_ref col) in
    let query_result = List.filter (fun d -> check_doc d query_doc) (snd col) in
    let query_string = `List(query_result) |> pretty_to_string in
    QueryResponse(true, query_string)
  ) with
    | _ -> QueryResponse(false, "Query failed for some reason")
