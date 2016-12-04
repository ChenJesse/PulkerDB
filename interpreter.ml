open Yojson.Basic
open Db
open Persist
open Models

type tuple =
| Nil
| Single of string
| Pair of string * string
| Triple of string * string * string
| Quad of string * string * string * string

type response = Db.response

exception ParseError
exception ImproperNameError
exception ParseDocError

let parse_json json_string = try (from_string json_string) with
  | _ -> raise ParseDocError

(* Trims white space, convert to lowercase *)
let sanitize_input input = String.(trim input |> lowercase_ascii)

(** Check that the collection or database name is valid *)
let validate_name input =
  not (String.(contains input ' ' || contains input '(' || contains input ')'
    || contains input '|'))

(* Returns a string containing everything before the first instance of c in input *)
let prefix c input = String.sub input 0 (String.index input c)

(* Returns a string containing everything before the last instance of c in input *)
let rprefix c input = String.sub input 0 (String.rindex input c)

(* Returns a string containing everything after the first instance of c in input *)
let suffix c input = ((String.length input) - ((String.index input c) + 1))
    |> String.sub input ((String.index input c) + 1)

(**
 * Handles the logic for a 'use db' command.
 *   - [input] is a sanitized input from parse
 *)
let handle_use_db input =
  let command = prefix ' ' input in
  let database = suffix ' ' input in
  match command with
    | "use" -> if(database ="benchmark") then benchmarker () else (
                  if (validate_name database) then create_db database
                  else raise ImproperNameError)
    | _ -> raise ParseError

let tuplize_input input =
  let rec helper acc i =
    match i with
    | "" -> ( match acc with
      | Pair(_, _) | Triple(_, _, _) | Quad(_, _, _, _) -> acc
      | _ -> raise ParseError)
    | _ ->
      let (add, remainder) =
        if (String.get i 0) = '(' then ((rprefix ')' i |> suffix '('), "")
        else if (String.contains i '.') then ((prefix '.' i), (suffix '.' i))
        else if (String.contains i '(') then
          ((prefix '(' i), ("(" ^ (suffix '(' i)))
        else (i, "")
      in
      match acc with
      | Nil -> helper (Single(add)) remainder
      | Single a -> helper (Pair(a, add)) remainder
      | Pair (a, b) -> helper (Triple(a, b, add)) remainder
      | Triple (a, b, c) -> helper (Quad(a, b, c, add)) remainder
      | Quad (_, _, _, _) -> failwith "Too many elements"
  in
  helper Nil input

(**
 * Will split parameters into a tuple, looking for the | char
 *   - [input] is a string
 *)
let tuplize_parameters input = (prefix '|' input, suffix '|' input)

(**
 * Handles the logic for a 'createIndex' command.
 *   - [a, b, c, d] are the components of a quad
 *)
let create_index_helper a b c d =
  match parse_json d with
  | `Assoc lst ->
    let (f, g) = List.hd lst in
    let query_doc = `Assoc [(f, `Assoc[("$exists", `Bool true)])] in
    create_index a b f query_doc
  | _ -> raise ParseError

(**
 * Handles the logic for a 'getIndex' command.
 *   - [a, b, c, d] are the components of a quad
 *)
let get_index_helper a b c d =
  match parse_json d with
  | `Assoc lst ->
    let (f, g) = List.hd lst in
    get_values g f b a
  | _ -> raise ParseError

(**
 * Checks for the -s flag, that signals that the output must be
 * stored in a json
 *   - [i] is the sanitized input from parse
 *   - [response] is the normal response from parse
 *)
let store i response =
  let flagged = (String.contains i '-') && (suffix '-' i) = "s" in
  if flagged then response |> persist_query else response

(**
 * Handles all commands that are tuplized into 3 parts.
 *   - [a, b, c] are the components of a triple
 *)
let handle_triple a b c =
  match b with
  | "show" -> if c = "" then show_db a else raise ParseError
  | "dropdatabase" -> if c = "" then drop_db a else raise ParseError
  | "createcollection" ->
    if (validate_name c) then create_col a c
    else raise ImproperNameError
  | _ -> raise ParseError

(**
 * Handles all commands that are tuplized into 4 parts.
 *   - [a, b, c, d] are the components of a quad
 *   - [i] is the sanitized input from parse
 *)
let handle_quad a b c d i =
  match c with
  | "createindex" -> create_index_helper a b c d
  | "drop" -> if d = "" then drop_col a b else raise ParseError
  | "show" -> if d = "" then show_col a b |> store i else raise ParseError
  | "insert" -> parse_json d |> create_doc a b
  | "find" ->  parse_json d |> query_col a b |> store i
  | "aggregate" -> parse_json d |> aggregate a b |> store i
  | "remove" -> parse_json d |> remove_doc a b
  | "replace" ->
    (let pair = tuplize_parameters d in
    replace_col a b (pair |> fst |> parse_json) (pair |> snd |> parse_json))
  | "update" ->
    (let pair = tuplize_parameters d in
    update_col a b (pair |> fst |> parse_json) (pair |> snd |> parse_json))
  | "getindex" -> get_index_helper a b c d
  | _ -> raise ParseError

let parse input =
  try (
    let i = sanitize_input input in
    match (Str.string_match (Str.regexp "use") i 0) with
    | true -> handle_use_db i
    | false ->
      if input = "show()" then show_catalog ()
      else if input = "save()" then save_env ()
      else match (tuplize_input i) with
      | Triple (a, b, c) -> handle_triple a b c
      | Quad (a, b, c, d) -> handle_quad a b c d i
    | _ -> failwith "Improper tuple"
  ) with
  | ParseDocError ->
    Failure "Invalid document provided.
            Refer to documentation in -help for more information."
  | ParseError -> Failure "Invalid command input."
  | ImproperNameError -> Failure "Invalid database or collection name."
  | _ -> Failure "Something unexpected happened."
