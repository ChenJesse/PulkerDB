(* Each subcategory of code lists which file will have to be changed/integrated into*)

(*db.ml *)
type dataEntry = (json * doc list) (*replace with a hashtable*)
type indexFile = string * (dataEntry List)
type indexList = indexFile list
type response =
  | CreateDBResponse of bool * string
  | CreateColResponse of bool * string
  | CreateIndexResponse of bool * string
  | CreateDocResponse of bool * string
  | RemoveDocResponse of bool * string
  | ReplaceDocResponse of bool * string
  | DropDBResponse of bool * string
  | DropColResponse of bool * string
  | QueryResponse of bool * string
  | ParseErrorResponse of bool * string
  | ShowColResponse of bool * string
  | UpdateColResponse of bool * string

let compareDocs = ();

(* Need to redefine this to be with arrays instead.*)

let loadFromIndex lowIndex HighIndex indexFile indexDesired=
match indexFile with
|[]-> []
|(x,y)::t -> if (x<>indexDesired) then ( loadFromIndex lowKey HighKey t indexDesired )
            else (let ctr = lowKey in let docList = ref([]) in while(ctr<HighIndex+1) do (let retTuple = Array.get ctr y in
              docList:= (snd retTuple)::docList) done;

(*Index creation (sans tree for now) *)
let createIndex db_name col_name index_name querydoc=
try (let col = !(db |> get_db_ref |> get_col_ref col) in
    let query_result = List.filter (fun d-> check_doc d query_doc) (snd col) in (*(doublecheck if this is right) Get all the tuples with the attribute *)
    let sorted_results = docSort querydoc query_result in (* Sort them all *)
    let table = Hashtbl.create 5 in(* Create a hashtable for loading *)
    let ctr = ref(0) in
    let len = List.length sorted_results in
    while(ctr < len)
    do (
    let currentDoc = List.nth ctr sorted_results in
    let t = Util.member index_name currentDoc in(*Load them all into the hashtable *)
    Hashtbl.add table t currentDoc;
    ctr:= !ctr+1;
  ) done;
    (index_name, table); (*The final index, table tuple*)



let query_col db col query_doc =
  try (
    let col = !(db |> get_db_ref |> get_col_ref col) in
    let query_result = List.filter (fun d -> check_doc d query_doc) (snd col) in
    let query_string = `List(query_result) |> pretty_to_string in
    QueryResponse(true, query_string)
  ) with
  | _ -> QueryResponse(false, "Query failed for some reason")
(* For Interpreter.ml*)

  | Quad (a, b, c, d) -> (match c with
        | "createIndex" -> parse_json d |> create_index a b
        | "drop" -> if d = "" then drop_col a b else raise ParseError
        | "show" -> if d = "" then show_col a b else raise ParseError
        | "insert" -> parse_json d |> create_doc a b
        | "find" ->  parse_json d |> query_col a b
        | "remove" -> parse_json d |> remove_doc a b
        | "replace" ->
          let pair = tuplize_parameters d in
          replace_col a b (pair |> fst |> parse_json) (pair |> snd |> parse_json)
        | "update" ->
          let pair = tuplize_parameters d in
          update_col a b (pair |> fst |> parse_json) (pair |> snd |> parse_json)

        | _ -> raise ParseError



(*For persist.ml *)
let write_index db_name col_name indexFile =
let dataEntryList = (snd) indexFile in
let filepath = db_name ^ "/" ^ col_name ^ (fst) indexFile ^ ".json" in
let ctr = ref(0) in
let doc_list = ref([]) in
while(ctr < List.length dataEntryList)
do(let tempList = (snd) List.nth ctr dataEntryList) in
  doc_list:= !doc_list ::tempList::`Assoc[("name", "placeholder")]) done;
let docs_json = `ASsoc(["entries", doc_list)]) in
Yojson.Basic.to_file filepath docs_json


let rebuildIndexFile db_name col_name indexFile =
  let path = db_name ^ "/" ^ col_name ^ (fst) indexFile in
  let doc_list = path |> Yojson.Basic.from_file
  |> Yojson.Basic.Util.member "entries" |> get_docs in
  indexBuilder indexFile doc_list

  let read_db db_ref =
  let (db_name, col_list, dirty) = !db_ref in
  try
    let col_from_disc file =
      let new_col = ref (strip file, []) in
      read_collection db_name file new_col;
      db_ref := (db_name, new_col::col_list, dirty)
    in
    let index_from_disc file indexName
       let
    traverse_dir col_from_disc db_name
  with
    | Unix.Unix_error _ -> raise NotInDisc
  (*Need to decide whether I want to both reloading data entries or if should just rebuild the tree itself or both*)



(*For repl.ml *)
 REPL COMMANDS
---------------------------------------------------------------
| -exit : Exit the programming gracefully                     |
| -about : Learn more about this project                      |
---------------------------------------------------------------

                      DATABASE COMMANDS
---------------------------------------------------------------
| use DATABASE_NAME                                           |
| db.dropDatabase()                                           |
| db.createCollection(name)                                   |
| db.COLLECTION_NAME.drop()                                   |
| db.COLLECTION_NAME.insert(document)                         |
| db.COLLECTION_NAME.find()                                   |
| db.COLLECTION_NAME.show()                                   |
| db.COLLECTION_NAME.replace(SELECTION_CRITERIA|UPDATED_DATA) |
| db.COLLECTION_NAME.update(SELECTION_CRITERIA|UPDATED_DATA)  |
| db.COLLECTION_NAME.remove(DELLETION_CRITERIA)               |
  db.COLLECTION_NAME.createIndex(FIELD)           (* New*)    |
---------------------------------------------------------------

(*db.ml changes *)
