exception Unimplemented

module type Comparable =
  sig
    type t
    val compare : t -> t -> [ `EQ | `GT | `LT ]
    val format : Format.formatter -> t -> unit
  end

module type Dictionary =
  sig
    module Key : Comparable
    type key = Key.t
    type 'value t
    val rep_ok : 'value t  -> 'value t
    val empty : 'value t
    val is_empty : 'value t -> bool
    val size : 'value t -> int
    val insert : key -> 'value -> 'value t -> 'value t
    val member : key -> 'value t -> bool
    val find : key -> 'value t -> 'value option
    val remove : key -> 'value t -> 'value t
    val choose : 'value t -> (key * 'value) option
    val fold : (key -> 'value -> 'acc -> 'acc) -> 'acc -> 'value t -> 'acc
    val to_list : 'value t -> (key * 'value) list
    val format : (Format.formatter -> 'value -> unit)
                  -> Format.formatter -> 'value t -> unit
  end

module type DictionaryMaker =
  functor (C : Comparable) -> Dictionary with type Key.t = C.t



module MakeTreeDictionary (C : Comparable) = struct
  module Key = C
  type key = C.t

  (* AF: The recursive tree
       *      (k1,v1)
   *         /       \
   *      (k2,v2)    (k3,v3)
   *      /  \     /   \
   *   ...  ...   ...  (kn, vn)
   * represents a dictionary of [(k1, v1)..(kn, vn)]. A leaf represents an
   * empty dict.
   * RI: Keys must be unique. There should be no duplicate keys in the dict. *)
  type 'value tree =
    | Leaf
    | TwoNode of (key * 'value) * 'value tree * 'value tree
    | ThreeNode of (key * 'value) * (key * 'value) *
          'value tree * 'value tree * 'value tree

  type 'value t = 'value tree

  (* AF: treeStatus is used as a wrapper around a tree. The 'Kicked' variant
   * represents an invalid configuration in the 2-3 tree that needs to be
   * resolved further up the tree.
   * The 'Done' variant signals that the subtree has a
   * valid configuration. Used in insertion into the 2-3 tree.
   * RI: None. *)
  type 'value treeStatus =
    | Kicked of (key * 'value) * 'value tree * 'value tree
    | Done of 'value tree


  (* Hole represents the gap left by a removed node. None
   * indicates the result of deleting a terminal node.
   * Some (k, v) indicates that we are deleting a terminal node that
   * will replace the non-terminal node that we are deleting. *)
  type 'value hole =
    | Hole of (key * 'value) option * 'value tree
    | Absorbed of (key * 'value) option * 'value tree

  (* Documents the position of key value in either a two node or three node *)
  type direction =
    | Left2
    | Right2
    | Left3
    | Mid3
    | Right3

  (* rep_ok is currently disabled *)
  let rep_ok d = d
    (* let rec helper n =
      match n with
        | Leaf -> 0
        | TwoNode (keyval, l, r) ->
          let left_depth = helper l in
          let right_depth = helper r in
          if (left_depth = right_depth) then 1 + left_depth
          else failwith "RI"
        | ThreeNode (leftKeyVal, rightKeyVal, l, m, r) ->
          let left_depth = helper l in
          let middle_depth = helper m in
          let right_depth = helper r in
          if (left_depth = right_depth && middle_depth = left_depth) then
            1 + left_depth
          else failwith "RI"
    in
    if ((helper d) > -1) then d else failwith "RI" *)

  let empty = Leaf

  let is_empty d = match d with
    | Leaf -> true
    | _ -> false

  let rec size d = match d with
    | Leaf -> 0
    | TwoNode (_, l, r) -> 1 + size l + size r
    | ThreeNode (_, _, l, m, r) -> 2 + size l + size m + size r

  (* [keyval d] Given a TwoNode, returns the key value pair
   * requires:
   *   - [d] is a valid TwoNode
   *)
  let keyval d = match d with
    | TwoNode(kv,_,_) -> kv
    | _ -> failwith "Expected a TwoNode"

  (* [keyval d] Given a TwoNode, returns the left child
   * requires:
   *   - [d] is a valid TwoNode
   *)
  let left d = match d with
    | TwoNode(_,l,_) -> l
    | _ -> failwith "Expected a TwoNode"

  (* [keyval d] Given a TwoNode, returns the right child
   * requires:
   *   - [d] is a valid TwoNode
   *)
  let right d = match d with
    | TwoNode(_,_,r) -> r
    | _ -> failwith "Expected a TwoNode"

  (* [insert_up node parent] Given a node and a parent, based on whether
   * the node needs to be kicked up and the parent type, will
   * either resolve the invalid configuration at this level, perhaps
   * to push it up to the level on top for handling.
   * requires:
   *   - [node] is a valid treeStatus
   *   - [parent] is a valid tree
   *)
  let insert_up node parent = match node with
    | Done x -> Done(parent)
    | Kicked(kv, l, r) ->
      let child = TwoNode(kv, l, r) in
      match parent with
        | Leaf -> Done(child)
        | TwoNode((k,v), l, r) ->
          if child = l then
            Done(ThreeNode((keyval child), (k,v),
              (left child), (right child), r))
          else
            Done(ThreeNode((k,v), (keyval child), l,
              (left child), (right child)))
        | ThreeNode((k1,v1), (k2,v2), l, m, r) ->
          if child = l then
            Kicked(
                (k1,v1),
                child,
                TwoNode((k2,v2), m, r)
              )
          else if child = m then
            Kicked(
                (keyval child),
                TwoNode((k1,v1), l, (left child)),
                TwoNode((k2,v2), (right child), r)
              )
          else
            Kicked(
                (k2,v2),
                TwoNode((k1,v1), l, m),
                child
              )

  (* [insert_down key value d] Given a (key, value) of
   * what you want to insert, inserts an appropriate node in the appropriate
   * spot, returning a type treeStatus.
   * requires:
   *   - [key] is a key of type key
   *   - [value] is a value of type 'value
   *   - [d] is a valid tree
   *)
  let rec insert_down key value d = match d with
    | Leaf -> insert_up (Kicked((key, value), Leaf, Leaf)) d
    | TwoNode((k,v), l, r) ->
      if key < k then
        let new_l = (insert_down key value l) in
        match new_l with
          | Done x -> Done(TwoNode((k,v), x, r))
          | Kicked(_,_,_) -> insert_up new_l d
      else if key > k then
        let new_r = (insert_down key value r) in
        match new_r with
          | Done x -> Done(TwoNode((k,v), l, x))
          | Kicked(_,_,_) -> insert_up new_r d
      else
        Done(TwoNode((k,value), l, r))
    | ThreeNode((k1,v1), (k2,v2), l, m, r) ->
      if key < k1 then
        let new_l = (insert_down key value l) in
        match new_l with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), x, m, r))
          | Kicked(_,_,_) -> insert_up new_l d
      else if key < k2 then
        let new_m = (insert_down key value m) in
        match new_m with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, x, r))
          | Kicked(_,_,_) -> insert_up new_m d
      else if key > k2 then
        let new_r = (insert_down key value r) in
        match new_r with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, m, x))
          | Kicked(_,_,_) -> insert_up new_r d
      else if key = k1 then
        Done(ThreeNode((k1,value), (k2,v2), l, m, r))
      else
        Done(ThreeNode((k1,v1), (k2,value), l, m, r))

  let insert key value d =
    match insert_down key value d with
      | Done x -> x
      | _ -> failwith "tree is not done???"

  let rec member key d =
    match d with
      | Leaf -> false
      | TwoNode ((k, v), l, r) ->
        if k = key then true
        else
          if key < k then
            member key l
          else
          member key r

      | ThreeNode ((k1, v1), (k2, v2), l, m, r) ->
        if k1 = key || k2 = key then true
        else
          if key < k1 then
            member key l
          else if key < k2 then
            member key m
          else
            member key r

  let rec find key d =
    match d with
      | Leaf -> None
      | TwoNode ((k, v), l, r) ->
        if k = key then (Some v)
        else
          if key < k then
            find key l
          else
            find key r
      | ThreeNode ((k1, v1), (k2, v2), l, m, r) ->
        if k1 = key then Some v1
        else if k2 = key then Some v2
        else
          if key < k1 then
            find key l
          else if key < k2 then
            find key m
          else
            find key r

  (* [remove_up node parent direction]
   * Pushes the given node into the parent, returning either
   * a new hole if the result does not satisfy the invariant, or an absorbed
   * otherwise.
   * requires:
   *   - [node] is of type hole
   *   - [parent] is of type tree
   *   - [direction] is of type direction.
   *)
  let remove_up node parent direction =
    match node with
      | Absorbed(_,_) -> failwith "not a hole"
      | Hole (kv, hole_child) ->
        let sibling = match parent, direction with
          | Leaf, _ -> Leaf
          | TwoNode(_, _, r), Left2 -> r
          | TwoNode(_,l,_), Right2 -> l
          | ThreeNode(_,_,_,m,_), Left3 -> m
          | ThreeNode(_,_,l,_,_), Mid3 -> l
          | ThreeNode(_,_,_,m,_), Right3 -> m
          | _ -> failwith "invalid input"
        in
        match parent, sibling, direction with
          | Leaf, Leaf, _ -> Absorbed(None, hole_child)
          | TwoNode((kp, vp), lp, rp), TwoNode(kvs, ls, rs), Left2 ->
              Hole(kv, ThreeNode((kp,vp), kvs, hole_child, ls, rs))
          | TwoNode((kp, vp), lp, rp), TwoNode(kvs, ls, rs), Right2 ->
              Hole(kv, ThreeNode((kp,vp), kvs, ls, rs, hole_child))
          | TwoNode((kp, vp), lp, rp),
                ThreeNode((ks1, vs1), (ks2, vs2), ls, ms, rs), Left2 ->
              let new_l = TwoNode((kp, vp), hole_child, ls) in
              let new_r = TwoNode((ks2, vs2), ms, rs) in
              Absorbed(kv, TwoNode((ks1, vs1), new_l, new_r))
          | TwoNode((kp, vp), lp, rp),
                ThreeNode((ks1, vs1), (ks2, vs2), ls, ms, rs), Right2 ->
              let new_l = TwoNode((ks1, vs1), ls, ms) in
              let new_r = TwoNode((kp, vp), rs, hole_child) in
              Absorbed(kv, TwoNode((ks2, vs2), new_l, new_r))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                TwoNode((ks,vs), ls, rs), Left3 ->
              let new_l = ThreeNode((kp1,vp1), (ks,vs), hole_child, ls, rs) in
              Absorbed(kv, TwoNode((kp2, vp2), new_l, rp))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                TwoNode((ks,vs), ls, rs), Right3 ->
              let new_r = ThreeNode((ks,vs), (kp2,vp2), ls, rs, hole_child) in
              Absorbed(kv, TwoNode((kp1, vp1), lp, new_r))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                TwoNode((ks,vs), ls, rs), Mid3 ->
              (* middle follows left-hand algo*)
              let new_l = ThreeNode((ks,vs), (kp1,vp1), ls, rs, hole_child) in
              Absorbed(kv, TwoNode((kp2,vp2), new_l, rp))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                ThreeNode((ks1,vs1), (ks2,vs2), ls, ms, rs), Left3 ->
              let new_l = TwoNode((kp1,vp1), hole_child, ls) in
              let new_m = TwoNode((ks2, vs2), ms, rs) in
              Absorbed(kv, ThreeNode((ks1,vs1), (kp2,vp2), new_l, new_m, rp))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                ThreeNode((ks1,vs1), (ks2,vs2), ls, ms, rs), Right3 ->
              let new_m = TwoNode((ks1,vs1), ls, ms) in
              let new_r = TwoNode((kp2,vp2), rs, hole_child) in
              Absorbed(kv, ThreeNode((kp1,vp1), (ks2,vs2), lp, new_m, new_r))
          | ThreeNode((kp1,vp1), (kp2,vp2), lp, _, rp),
                ThreeNode((ks1,vs1), (ks2,vs2), ls, ms, rs), Mid3 ->
              (* middle follow left-hand algo *)
              let new_l = TwoNode((ks1,vs1), ls, ms) in
              let new_m = TwoNode((kp1,vp1), rs, hole_child) in
              Absorbed(kv, ThreeNode((ks2,vs2), (kp2,vp2), new_l, new_m, rp))
          | _ -> failwith "Parent and sibling are not valid nodes"

(* -------------------DOES NOT COMPILE, JUST ATTEMPTED DELETE----------------*)
(*   let rec delete_smallest d = match d with
    | Leaf -> failwith "Trying to delete from Leaf"
    | TwoNode(kv, Leaf, _) -> Hole(Some kv, Leaf)
    | ThreeNode(kv1, kv2, Leaf, Leaf, Leaf) ->
        Absorbed(Some kv1, TwoNode(kv2, Leaf, Leaf))
    | TwoNode(kv, l, r) ->
        let temp = delete_smallest l in
        match temp with
          | Hole(_, Leaf) -> remove_up temp d Left2
          | Absorbed(opt, child) -> Absorbed(opt, TwoNode(kv, child, r))
    | ThreeNode(kv1, kv2, l, m, r) ->
        let temp = delete_smallest l in
        match temp with
          | Hole(_, Leaf) -> remove_up temp d Left3
          | Absorbed(opt, child) ->
              Absorbed(opt, ThreeNode(kv1, kv2, child, m, r)) *)

  (* let remove_down key d = match d with
    | Leaf -> Absorbed(Leaf)
    | ThreeNode((k1,v1), (k2,v2), Leaf, _, _) ->
      if key = k1 then
        Absorbed(TwoNode((k2,v2), Leaf, Leaf))
      else if key = k2 then
        Absorbed(TwoNode((k1,v1), Leaf, Leaf))
      else (Absorbed(d))
    | TwoNode((k,v), Leaf, Leaf) ->
      if key = k then
        Hole((k,v), Leaf)
      else
        Absorbed(d)
    | TwoNode((k,v), l, r) ->
      if key = k then
        let
        '' *)

  let remove key d = d

  let choose d = match d with
    | Leaf -> None
    | TwoNode (keyval, _, _) -> Some keyval
    | ThreeNode (keyval, _, _, _, _) -> Some keyval

  let fold f init d =
     let rec helper d acc = match d with
      | Leaf -> acc
      | TwoNode (kv, l, r) ->
        helper l acc |> f (fst kv) (snd kv) |> helper r
      | ThreeNode (kv1, kv2, l, m, r) ->
        helper l acc |> f (fst kv1) (snd kv1) |> helper m
        |> f (fst kv2) (snd kv2) |> helper r
    in
    helper d init

  let to_list d =
    fold (fun k v acc -> acc@[(k, v)]) [] d

  let format format_val fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module type Set =
  sig
    module Elt : Comparable
    type elt = Elt.t
    type t
    val rep_ok : t  -> t
    val empty : t
    val is_empty : t -> bool
    val size : t -> int
    val insert : elt -> t -> t
    val member : elt -> t -> bool
    val remove : elt -> t -> t
    val union : t -> t -> t
    val intersect : t -> t -> t
    val difference : t -> t -> t
    val choose : t -> elt option
    val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
    val to_list : t -> elt list
    val format : Format.formatter -> t -> unit
  end

module type SetMaker =
  functor (C : Comparable) -> Set with type Elt.t = C.t

(* HINT:  To build a set out of a dictionary, consider this:
   a dictionary is much like a **set** of (key,value) pairs. *)
module MakeSetOfDictionary (D:Dictionary) = struct
  module Elt = D.Key
  type elt = Elt.t

  (* AF: dict (k1, v1)..(kn, vn) represents a set k1..kn.
   * Note that k and v have the same type, and thus v can
   * essentially be ignored.
   * An empty dictionary represents an empty set.
   * RI: Keys must be unique, because there are no duplicates in the set *)
  type t = elt D.t

  let rep_ok s = s
  (*     let no_dups s = D.to_list s
      |> List.map (fun el -> D.remove (fst el) s |> D.find (fst el))
      |> List.for_all (fun el -> el = None)
    in
    if (no_dups s) then s else failwith "RI" *)

  let empty = D.empty

  let is_empty s = ((rep_ok s) = D.empty)

  let size s = rep_ok s |> D.size

  let insert x s =
    if rep_ok s |> D.member x then s
    else D.insert x x s

  let member x s = rep_ok s |> D.member x

  let remove x s = D.remove x s

  let choose s = match (rep_ok s |> D.choose) with
    | Some x -> Some (fst x)
    | None -> None

  let rec fold f init s =
    let wrapper k v acc = f k acc in
    D.fold wrapper init s

  let to_list s = rep_ok s |> D.to_list |> List.map (fun el -> fst el)

  let union s1 s2 =
    let rec helper l acc = match l with
      | [] -> acc
      | h::t -> if member h acc then helper t acc else helper t (insert h acc)
    in
    let list1 = to_list s1 in
    let list2 = to_list s2 in
    D.empty |> helper list1 |> helper list2

  let intersect s1 s2 =
    let rec helper l1 l2 acc = match l1 with
      | [] -> acc
      | h::t -> if member h l2 then helper t l2 (insert h acc)
                else helper t l2 acc
    in
    let s1_list = to_list s1 in
    helper s1_list s2 D.empty

  let difference s1 s2 =
    let rec helper l1 l2 acc = match l1 with
      | [] -> acc
      | h::t -> if member h l2 then helper t l2 acc
        else helper t l2 (insert h acc)
    in
    let s1_list = to_list s1 in
    helper s1_list s2 D.empty

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end


