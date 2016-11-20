open Yojson.Basic
open Db
open Persist

type tuple = 
| Nil 
| Single of string
| Pair of string * string
| Triple of string * string * string
| Quad of string * string * string * string

type response = Db.response

exception ParseError
exception ImproperNameError

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
let parse_json json_string = try (from_string json_string) with | _ -> raise ParseError

(**
 * Trims white space, convert to lowercase
 *)
let sanitize_input input = String.(trim input |> lowercase_ascii)

(** Check that the collection or database name is valid *)
let validate_name input = 
  not (String.(contains input ' ' || contains input '(' || contains input ')'))

(**
 * Returns a string containing everything before
 * the first instance of c in input
 *)
let prefix c input = String.sub input 0 (String.index input c)

(**
 * Returns a string containing everything before
 * the last instance of c in input
 *)
let rprefix c input = String.sub input 0 (String.rindex input c)

(**
 * Returns a string containing everything after 
 * the first instance of c in input
 *)
let suffix c input = ((String.length input) - ((String.index input c) + 1)) 
    |> String.sub input ((String.index input c) + 1)

(**
 * Returns response, given a sanitized input
 *)
let handle_use_db input = 
  let command = prefix ' ' input in 
  let database = suffix ' ' input in 
  match command with 
    | "use" -> if (validate_name database) then create_db database 
               else raise ImproperNameError
    | _ -> raise ParseError

(**
 * Given an input, parses it into a tuple of 3 or 4 
 * elements
 *)
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
        else if (String.contains i '(') then ((prefix '(' i), ("(" ^ (suffix '(' i)))
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
 * Will split two parameters into a tuple
 *)
let tuplize_parameters input = (prefix '|' input, suffix '|' input)

(**
 * Parses the input from the REPL, and calls the appropriate function
 *)
let parse input = 
  try (
    let i = sanitize_input input in 
    match (Str.string_match (Str.regexp "use") i 0) with 
    | true -> handle_use_db i
    | false -> 
      if input = "exit" then failwith "Unimplemented" (* Should persist all the changed collections *)
      else match (tuplize_input i) with 
      | Triple (a, b, c) -> ( match b with 
        | "dropdatabase" -> if c = "" then drop_db a else raise ParseError
        | "createcollection" -> if (validate_name c) then create_col a c
                                else raise ImproperNameError
        | _ -> raise ParseError
      )
      | Quad (a, b, c, d) -> (match c with 
        | "drop" -> if d = "" then drop_col a b else raise ParseError
        | "show" -> if d = "" then show_col a b else raise ParseError
        | "insert" -> parse_json d |> create_doc a b
        | "find" ->  parse_json d |> query_col a b 
        | "remove" -> parse_json d |> remove_doc a b
        | "replace" -> 
          let pair = tuplize_parameters d in 
          replace_col a b (pair |> fst |> parse_json) (pair |> snd |> parse_json)
        | _ -> raise ParseError
      ) 
    | _ -> failwith "Improper tuple"
  ) with 
  | ParseError -> ParseErrorResponse(false, "Invalid command")
  | ImproperNameError -> ParseErrorResponse(false, "Invalid database or collection name")
  | _ -> ParseErrorResponse(false, "Something weird happened")
