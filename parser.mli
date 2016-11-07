
type doc = 
  | Pair of (string * doc) * doc 
  | Pair of (string * doc list) 
  | Pair of (string * int) 
  | Pair of (string * int list) 
  | Pair of (string * bool) 
  | Pair of (string * bool list) 
  | Pair of (string * string) 
  | Pair of (string * string list)

type db = string

type col = string

(**
 * given a valid string of a JSON, will output
 * corresponding doc with the appropriate structure
 *)
val parse: string -> doc
