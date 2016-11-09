open Yojson.Basic

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

type response = | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string

let environment : catalog = ref []

let get_db db = 
  try (List.filter (fun x -> (fst !x) = db) !environment |> List.hd) with 
    | _ -> failwith "Could not locate database"

let get_col col db = 
  try (!db |> snd |> List.filter (fun x -> (fst !x) = col) |> List.hd) with 
    | _ -> failwith "Could not locate collection"

(**
 * Given a string representation of JSON, creates a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_doc db col doc = 
  let col_ref = db |> get_db |> get_col col in 
  try (
    col_ref := (fst !col_ref, doc::(snd !col_ref));
    CreateDocResponse(true, "Success")
  ) with 
  | _ -> CreateDocResponse(false, "Problem with storing document")


(**
 * Given a string representing name of db, creates a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_db db = 
  match (List.exists (fun x -> (fst !x) = db) !environment) with 
    | true -> CreateDBResponse(false, "Database with same name already exists") 
    | false -> try (
        environment := (ref (db, []))::!environment ;
        CreateDBResponse(true, "Success")
      ) with 
      | _ -> CreateDBResponse(false, "Problem with storing database")

(**
 * Given a string representing name of col, creates a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let create_col db col = 
  let db = get_db db in 
  match (snd !db |> List.exists (fun x -> (fst !x) = col)) with 
    | true -> CreateColResponse(false, "Collection with same name already exists")
    | false -> try (
        db := (fst !db, (ref (col, []))::(snd !db));
        CreateColResponse(true, "Success")
      ) with 
      | _ -> CreateColResponse(false, "Problem with storing collection")

(**
 * Given a string representation of JSON, removes a doc in the environment. 
 * On failure, return false. On success, return true.
 *)
let remove_doc db col doc = failwith "Unimplemented"

(**
 * Given a string representing name of db, drops a db in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_db db = failwith "Unimplemented"

(**
 * Given a string representing name of col, drops a col in the environment. 
 * On failure, return false. On success, return true.
 *)
let drop_col db col = failwith "Unimplemented"

(**
 * Given a string representing a query JSON, looks for matching docs in the environment. 
 * On failure, return false. On success, return true.
 *)
let query_col db col query_doc = failwith "Unimplemented"
