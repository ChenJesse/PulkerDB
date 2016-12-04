open Tree

type doc = Yojson.Basic.json

type index_file = {
  id_name: string;
  id_table: (Yojson.Basic.json, Yojson.Basic.json) Hashtbl.t;
  keys: Yojson.Basic.json tree ref
}

type index_list = index_file list

type col = doc list * index_list

type db = (string, col) Hashtbl.t * bool

type catalog = (string, db) Hashtbl.t
