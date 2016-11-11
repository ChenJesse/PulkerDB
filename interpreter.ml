open Yojson.Basic
open Db

type tuple = 
  | Nil 
  | Single of string
  | Pair of string * string
  | Triple of string * string * string
  | Quad of string * string * string * string

type response = Db.response

exception ParseError

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
let parse_json json_string = from_string json_string

(**
 * Trims white space, convert to lowercase
 *)
let sanitize_input input = String.(trim input |> lowercase_ascii)

(**
 * Returns a string containing everything before
 * the first instance of c in input
 *)
let prefix c input = String.sub input 0 (String.index input c)

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
    | "use" -> create_db database (* Should we allow spaces in db name? *)
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
        | _ -> failwith "Not proper tuple")
      | _ -> 
        let (add, remainder) = 
          if (String.get i 0) = '(' then ((prefix ')' i |> suffix '('), "")
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
 * Parses the input from the REPL, and calls the appropriate function
 *)
let parse input = 
  let i = sanitize_input input in 
  match (String.contains i ' ') with 
    | true -> handle_use_db i
    | false -> match (tuplize_input i) with 
      | Triple (a, b, c) -> ( match b with 
        | "dropdatabase" -> drop_db a
        | "createcollection" -> create_col a c
        | _ -> raise ParseError
      )
      | Quad (a, b, c, d) -> (match c with 
        | "drop" -> drop_col a b
        | "insert" -> parse_json d |> create_doc a b
        | "find" -> parse_json d |> query_col a b
        | "update" -> failwith "Unimplemented" (* TODO: Not sure how to handle multiple parameters *)
        | "remove" -> parse_json d |> remove_doc a b
        | _ -> raise ParseError
      ) 
      | _ -> failwith "Improper tuple"