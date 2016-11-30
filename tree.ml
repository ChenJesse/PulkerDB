  type key = Yojson.Basic.json

  (**
   * Given two jsons, val1, val2 compares them and returns -1, 0, or 1 depending
   * on whether val1< val2; val1=val2; val1>val2 respectively.
   *)
  let compareJSON (val1:Yojson.Basic.json) (val2:Yojson.Basic.json) =
  match val1,val2 with
  |(`Null, `Null)-> 0
  |(`Null, _) -> -1
  |(_, `Null) -> 1
  |(`Int a , `Int b)-> if(a > b) then (1) else (if(a<b) then -1 else 0)
  |(`Int a, _)-> -1
  |(_, `Int b) -> 1
  |(`Float a, `Float b) -> if(a > b) then (1) else (if(a<b) then -1 else 0)
  |(`Float a, _)-> -1
  |(_, `Float b) -> 1
  |(`String a, `String b) -> if( a > b) then (1) else (if(a<b) then -1 else 0)
  |(`String a, _)-> -1
  |(_, `String b) -> 1
  |(`Bool a, `Bool b) -> if( a > b) then (1) else (if ( a < b ) then -1 else 0)
  |(`Bool a, _)-> -1
  |(_, `Bool b) -> 1
  |(`List a, `List b) -> if( a > b) then (1) else (if(a<b) then -1 else 0) (*The logic with this one might not be 100% right..*)
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
    | Leaf -> insert_up (Kicked((key, [value]), Leaf, Leaf)) d
    | TwoNode((k,v), l, r) ->
      if ((compareJSON key k) = -1) then
        let new_l = (insert_down key value l) in
        match new_l with
          | Done x -> Done(TwoNode((k, v), x, r))
          | Kicked(_,_,_) -> insert_up new_l d
      else if key > k then
        let new_r = (insert_down key value r) in
        match new_r with
          | Done x -> Done(TwoNode((k, v), l, x))
          | Kicked(_,_,_) -> insert_up new_r d
      else
        Done(TwoNode((k, [value]@v), l, r))
    | ThreeNode((k1,v1), (k2,v2), l, m, r) ->
      if((compareJSON key k1) = -1) then
        let new_l = (insert_down key value l) in
        match new_l with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), x, m, r))
          | Kicked(_,_,_) -> insert_up new_l d
      else if ((compareJSON key k2) = -1) then
        let new_m = (insert_down key value m) in
        match new_m with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, x, r))
          | Kicked(_,_,_) -> insert_up new_m d
      else if ((compareJSON key k2) = 1) then
        let new_r = (insert_down key value r) in
        match new_r with
          | Done x -> Done(ThreeNode((k1,v1), (k2,v2), l, m, x))
          | Kicked(_,_,_) -> insert_up new_r d
      else if ((compareJSON key k1) = 0) then
        Done(ThreeNode((k1,[value]@v1), (k2,v2), l, m, r))
      else
        Done(ThreeNode((k1,v1), (k2,[value]@v2), l, m, r))

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

     (*
      * Highval has to be > than the actual bound we want by like at least 1, lowval has to be less than the actual lowval we want by at least 1
      *)
    let rec findDocs  d  highval lowval =
    match d with
      | Leaf -> []
      | TwoNode ((k, v), l, r) -> let highCom = compareJSON k highval in
        let lowCom = compareJSON k lowval in
        if ((highCom = -1) && (lowCom = 1)) then
          v @(findDocs  l  highval lowval)@(findDocs  r  highval lowval)
        else
          if ((highCom =1) || (highCom = 0)) then
            findDocs  l  highval lowval
          else
            findDocs r  highval lowval
      | ThreeNode ((k1, v1), (k2, v2), l, m, r) ->
         let highComk1 = compareJSON k1 highval in
         let highComk2 = compareJSON k2 highval in
         let lowComk1  = compareJSON k1 lowval in
         let lowComk2 = compareJSON k2 lowval in
         match lowComk1, highComk1, lowComk2, highComk2 with
         |0,_,1, -1-> v2@(findDocs l  highval lowval)@(findDocs m  highval lowval)@(findDocs r  highval lowval)
         |0,_,_, 0 -> findDocs m  highval lowval
         |0,0,_,_->[]
         |1, -1, 1, -1 -> v1@v1@(findDocs l  highval lowval)@(findDocs m  highval lowval)@(findDocs r  highval lowval)
         |1,0, _, _-> findDocs l  highval lowval
         |1,-1,1,0-> v1@(findDocs l  highval lowval)@(findDocs m  highval lowval)
         |_,_,0,_-> (findDocs r  highval lowval)
         |_ -> v1@v2@[]
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


