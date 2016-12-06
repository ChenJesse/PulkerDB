open Yojson.Basic
type key = Yojson.Basic.json

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
  | TwoNode of (key * 'value list) * 'value tree * 'value tree
  | ThreeNode of (key * 'value list) * (key * 'value list) *
        'value tree * 'value tree * 'value tree

(* AF: treeStatus is used as a wrapper around a tree. The 'Kicked' variant
 * represents an invalid configuration in the 2-3 tree that needs to be
 * resolved further up the tree.
 * The 'Done' variant signals that the subtree has a
 * valid configuration. Used in insertion into the 2-3 tree.
 * RI: None. *)
type 'value treeStatus =
  | Kicked of (key * 'value list) * 'value tree * 'value tree
  | Done of 'value tree

(* Hole represents the gap left by a removed node. None
 * indicates the result of deleting a terminal node.
 * Some (k, v) indicates that we are deleting a terminal node that
 * will replace the non-terminal node that we are deleting. *)
type 'value hole =
  | Hole of (key * 'value list) option * 'value tree
  | Absorbed of (key * 'value list) option * 'value tree

(* Documents the position of key value in either a two node or three node *)
type direction =
  | Left2
  | Right2
  | Left3
  | Mid3
  | Right3

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
          TwoNode((k2,v2), m, r))
      else if child = m then
        Kicked(
          (keyval child),
          TwoNode((k1,v1), l, (left child)),
          TwoNode((k2,v2), (right child), r))
      else
        Kicked(
          (k2,v2),
          TwoNode((k1,v1), l, m),
          child)

(* [insert_down key value d] Given a (key, value) of
 * what you want to insert, inserts an appropriate node in the appropriate
 * spot, returning a type treeStatus.
 * requires:
 *   - [key] is a key of type key
 *   - [value] is a value of type 'value
 *   - [d] is a valid tree
 *)
let rec insert_down key value d replace = match d with
  | Leaf -> insert_up (Kicked((key, [value]), Leaf, Leaf)) d
  | TwoNode((k,v), l, r) ->
    if key < k then
      let new_l = (insert_down key value l replace) in
      match new_l with
      | Done x -> Done(TwoNode((k, v), x, r))
      | Kicked(_,_,_) -> insert_up new_l d
    else if key > k then
      let new_r = (insert_down key value r replace) in
      match new_r with
      | Done x -> Done(TwoNode((k, v), l, x))
      | Kicked(_,_,_) -> insert_up new_r d
    else
      if (replace)
      then Done(TwoNode((k, [value]@[]), l, r))
      else Done(TwoNode((k, [value]@v), l, r))
  | ThreeNode((k1,v1), (k2,v2), l, m, r) ->
    if key < k1 then
      let new_l = (insert_down key value l replace) in
      match new_l with
      | Done x -> Done(ThreeNode((k1,v1), (k2,v2), x, m, r))
      | Kicked(_,_,_) -> insert_up new_l d
    else if key < k2 then
      let new_m = (insert_down key value m replace) in
      match new_m with
      | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, x, r))
      | Kicked(_,_,_) -> insert_up new_m d
    else if key > k2 then
      let new_r = (insert_down key value r replace) in
      match new_r with
      | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, m, x))
      | Kicked(_,_,_) -> insert_up new_r d
    else if key = k1 then
     if ( replace )
    then Done(ThreeNode((k1,[value]@[]), (k2,v2), l, m, r))
    else  Done(ThreeNode((k1,[value]@v1), (k2,v2), l, m, r))
    else
      if (replace)
      then Done(ThreeNode((k1,v1), (k2,[value]@[]), l, m, r))
      else Done(ThreeNode((k1,v1), (k2,[value]@v2), l, m, r))

let insert key value d r_check = match insert_down key value d r_check with
  | Done x -> x
  | _ -> failwith "Something went horribly wrong while inserting"

let rec member key d = match d with
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

let rec find key d = match d with
  | Leaf -> []
  | TwoNode ((k, v), l, r) ->
    if k = key then v
    else
      if key < k then
        find key l
      else
        find key r
  | ThreeNode ((k1, v1), (k2, v2), l, m, r) ->
    if k1 = key then v1
    else if k2 = key then v2
    else
      if key < k1 then
        find key l
      else if key < k2 then
        find key m
      else
        find key r

(*
 * gets all keys between highval and lowval (exclusive) and
 * concatenates their values
 *)
let rec get_range d highval lowval = match d with
  | Leaf -> []
  | TwoNode ((k, v), l, r) ->
    if k < highval && k > lowval then
      if ((v = [`Null] && k <> `Null) || (v= [`Int 0] && k <> `Int 0))
        then (get_range l highval lowval) @ (get_range r highval lowval)
      else v @ (get_range l highval lowval) @ (get_range r highval lowval)
    else if k >= highval then
      get_range l highval lowval
    else
      get_range r highval lowval
  | ThreeNode ((k1, v1), (k2, v2), l, m, r) ->
     if (lowval < k1) then
        if(lowval < k2 && k2 < highval && k1 < highval) then
          if ( (v2 = [`Null] && k2 <> `Null) || (v2 = [`Int 0] && k2 <> `Int 0) )
          then (get_range l  highval lowval)@(get_range m  highval lowval)@(get_range r  highval lowval)
          else v2@(get_range l  highval lowval)@(get_range m  highval lowval)@(get_range r  highval lowval)
        else if (k1 < highval && k2 >= highval ) then
          if ( (v1 = [`Null] && k1 <> `Null) || (v1 = [`Int 0] && k1 <> `Int 0) )
          then (get_range l  highval lowval)@(get_range m  highval lowval)
          else v1@(get_range l  highval lowval)@(get_range m  highval lowval)
        else if (k1 = highval) then get_range l highval lowval
        else []
      else if (lowval = k1) then
        if(k2 = highval) then  get_range m highval lowval
        else if (k2 < highval) then  (get_range m highval lowval)@(get_range r highval lowval)
        else []
      else []

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

let remove key d = d

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


