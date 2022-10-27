(*
   Copyright 2012-2022 Codinuum Software Lab <https://codinuum.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* UIDmapping.ml *)



module UID = Otreediff.UID
module GI = Otreediff.GIndex
module Comp = Compression


let fprintf = Printf.fprintf
let sprintf = Printf.sprintf

type node_t = Spec.node_t


let in_subtree_mutually tree nd1 nd2 =
    let gi1, gi2 = nd1#gindex, nd2#gindex in
    let lgi1 = (tree#initial_leftmost nd1)#gindex in
    let lgi2 = (tree#initial_leftmost nd2)#gindex in
    (lgi1 <= gi2 && gi2 < gi1) || (lgi2 <= gi1 && gi1 < gi2)


let is_crossing nd1 nd2 n1 n2 =
  (nd1#gindex - n1#gindex) * (nd2#gindex - n2#gindex) < 0



let _is_incompatible tree1 tree2 nd11 nd12 nd21 nd22 =
  let in_subtree_mutually1 = in_subtree_mutually tree1 nd11 nd21 in
  let in_subtree_mutually2 = in_subtree_mutually tree2 nd12 nd22 in
   (in_subtree_mutually1 && not in_subtree_mutually2) ||
   (not in_subtree_mutually1 && in_subtree_mutually2)


let is_incompatible tree1 tree2 nd11 nd12 nd21 nd22 =
  let crossing = is_crossing nd11 nd12 nd21 nd22 in
  (_is_incompatible tree1 tree2 nd11 nd12 nd21 nd22) && (not crossing)

let is_crossing_or_incompatible tree1 tree2 nd11 nd12 nd21 nd22 =
  is_crossing nd11 nd12 nd21 nd22 || _is_incompatible tree1 tree2 nd11 nd12 nd21 nd22


let select_p_pairs p tree1 tree2 pair_weight_list = (* returns p pairs and not p pairs *)

  let pair_weight_list =
    List.fast_sort
      (fun (n1, _, _) (n2, _, _) -> Stdlib.compare n1#gindex n2#gindex)
      pair_weight_list
  in

  DEBUG_MSG "select_p_pairs: [%s]"
    (Xlist.to_string
       (fun (n1, n2, w) -> sprintf "(%a-%a,%d)" UID.ps n1#uid UID.ps n2#uid w)
       ";" pair_weight_list);

  let len = List.length pair_weight_list in

  if len > 1 then begin
    let a = Array.of_list pair_weight_list in
    let mat = Array.make_matrix len len false in
    for i = 0 to len - 2 do
      let nd11, nd12, _ = a.(i) in
      for j = i + 1 to len - 1 do
	let nd21, nd22, _ = a.(j) in
	let b =
	  if (nd11 == nd21 || nd12 == nd22) then
	    false
	  else
	    p tree1 tree2 nd11 nd12 nd21 nd22
	in
	mat.(i).(j) <- b;
	mat.(j).(i) <- b
      done
    done;

    let nfriends_a = Array.make len 0 in
    for i = 0 to len - 1 do
      let c = ref 0 in
      for j = 0 to len - 1 do
	if mat.(i).(j) then
	  incr c
      done;
      nfriends_a.(i) <- !c
    done;

    let sorted_idxs =
      List.fast_sort
	(fun i j ->
	  let _, _, wi = a.(i) in
	  let _, _, wj = a.(j) in
	  let x = Stdlib.compare wj wi in
	  if x = 0 then begin
	    let fi = nfriends_a.(i) in
	    let fj = nfriends_a.(j) in
	    Stdlib.compare fj fi
	  end
	  else
	    x
	) (Xlist.range len)
    in

    let compat_idxs =
      List.fold_left
	(fun l idx ->
	  if List.for_all (fun i -> mat.(i).(idx)) l then
	    idx :: l
	  else
	    l
	) [] sorted_idxs
    in

    let incompat_idxs =
      List.filter (fun i -> not (List.mem i compat_idxs)) (Xlist.range len)
    in

    let compat = List.map (fun i -> a.(i)) compat_idxs in
    let incompat = List.map (fun i -> a.(i)) incompat_idxs in


    DEBUG_MSG "select_p_pairs: p pairs: [%s]"
      (Xlist.to_string
	 (fun (n1, n2, _) -> sprintf "(%a-%a)" UID.ps n1#uid UID.ps n2#uid)
	 ";" compat);
    DEBUG_MSG "select_p_pairs: not p pairs: [%s]"
      (Xlist.to_string
	 (fun (n1, n2, _) -> sprintf "(%a-%a)" UID.ps n1#uid UID.ps n2#uid)
	 ";" incompat);

    compat, incompat
  end
  else
    pair_weight_list, []


let select_compatible_pairs tree1 tree2 pair_weight_list = (* returns compat. pairs and incompat. pairs *)
  select_p_pairs
    (fun t1 t2 n11 n12 n21 n22 ->
      let b = not (is_incompatible t1 t2 n11 n12 n21 n22) in

      DEBUG_MSG "%a-%a - %a-%a --> compatible:%B"
	UID.ps n11#uid UID.ps n12#uid UID.ps n21#uid UID.ps n22#uid b;

      b
    ) tree1 tree2 pair_weight_list


let select_compatible_and_not_crossing_pairs tree1 tree2 pair_weight_list =
  select_p_pairs
    (fun t1 t2 n11 n12 n21 n22 ->
      let b =
	not (is_incompatible t1 t2 n11 n12 n21 n22) && not (is_crossing n11 n12 n21 n22)
      in

      DEBUG_MSG "%a-%a - %a-%a --> compatible_and_not_crossing:%B"
	UID.ps n11#uid UID.ps n12#uid UID.ps n21#uid UID.ps n22#uid b;

      b
    ) tree1 tree2 pair_weight_list


let add map1 map1rev map2 map2rev uid1 uid2 =
  let conflict1 = ref None in
  let conflict2 = ref None in
  begin
    try
      let uid1' = Hashtbl.find map1 uid1 in
      if uid1' <> uid2 then begin

        Hashtbl.replace map1 uid1 uid2;
        Hashtbl.remove map1rev uid1';
	Hashtbl.remove map2rev uid1';

        conflict2 := Some uid1';

        BEGIN_DEBUG
	  DEBUG_MSG "conflict: %a-%a" UID.ps uid1 UID.ps uid1';
	  DEBUG_MSG "overridden by: %a-%a" UID.ps uid1 UID.ps uid2
        END_DEBUG

      end
    with
      Not_found ->
        try
          let uid1' = Hashtbl.find map2 uid1 in
	  if uid1' <> uid2 then begin

	    Hashtbl.replace map2 uid1 uid2;
	    Hashtbl.remove map2rev uid1';
	    Hashtbl.remove map1rev uid1';

            conflict2 := Some uid1';

            BEGIN_DEBUG
	      DEBUG_MSG "conflict: %a-%a" UID.ps uid1 UID.ps uid1';
	      DEBUG_MSG "overridden by: %a-%a" UID.ps uid1 UID.ps uid2
            END_DEBUG

	  end
    with
      Not_found -> Hashtbl.add map1 uid1 uid2
  end;
  begin
    try
      let uid2' = Hashtbl.find map1rev uid2 in
      if uid2' <> uid1 then begin

	Hashtbl.replace map1rev uid2 uid1;
	Hashtbl.remove map1 uid2';
	Hashtbl.remove map2 uid2';

        conflict1 := Some uid2';

	BEGIN_DEBUG
	  DEBUG_MSG "conflict: %a-%a" UID.ps uid2' UID.ps uid2;
	  DEBUG_MSG "overridden by: %a-%a" UID.ps uid1 UID.ps uid2
	END_DEBUG

      end
    with
      Not_found ->
        try
          let uid2' = Hashtbl.find map2rev uid2 in
	  if uid2' <> uid1 then begin

	    Hashtbl.replace map2rev uid2 uid1;
	    Hashtbl.remove map2 uid2';
	    Hashtbl.remove map1 uid2';

            conflict1 := Some uid2';

	    BEGIN_DEBUG
	      DEBUG_MSG "conflict: %a-%a" UID.ps uid2' UID.ps uid2;
	      DEBUG_MSG "overridden by: %a-%a" UID.ps uid1 UID.ps uid2
	    END_DEBUG

	  end
    with
      Not_found -> Hashtbl.add map1rev uid2 uid1
  end;
  !conflict1, !conflict2

let tbl_remove tbl k =
  (*while Hashtbl.mem tbl k do*)
    Hashtbl.remove tbl k
  (*done*)


module Json = struct

  let find_nearest_unordered_ancestor_node =
    Sourcecode.find_nearest_p_ancestor_node (fun x -> x#data#is_order_insensitive)

  let _fprintf ch fmt =
    Printf.ksprintf (fun s -> ignore (ch#output_ s 0 (String.length s))) fmt

  let get_gid (nd : node_t) =
    let gid = nd#data#gid in
    if gid > 0 then
      gid
    else
      nd#gindex

  let get_loc nd =
    let loc = nd#data#src_loc in
    let sl = loc.Loc.start_line in
    let el = loc.Loc.end_line in
    let sc = loc.Loc.start_char in
    let ec = loc.Loc.end_char in
    sprintf "{\"start_line\":%d,\"start_char\":%d,\"end_line\":%d,\"end_char\":%d}" sl sc el ec

  let get_cat nd = "\""^nd#data#get_category^"\""

  let get_name nd =
    if nd#data#is_named && nd#data#is_named_orig && not nd#data#has_value then
      nd#data#get_orig_name
    else
      ""

  let get_info1 (nd : node_t) =
    let named_nameless = nd#data#is_named && not nd#data#is_named_orig in
    let name = get_name nd in
    let phantom = nd#data#is_phantom in
    let unordered = nd#data#is_order_insensitive in
    let l = ref [] in
    l := (sprintf "\"loc\":%s" (get_loc nd)) :: !l;
    l := (sprintf "\"cat\":%s" (get_cat nd)) :: !l;
    if name <> "" then
      l := (sprintf "\"name\":\"%s\"" name) :: !l;
    if named_nameless then
      l := "\"named_nameless\":true" :: !l;
    if phantom then
      l := "\"phantom\":true" :: !l;
    if unordered then
      l := "\"unordered\":true" :: !l;
    "{"^(String.concat "," !l)^"}"

  let get_info mapped_node_tbl (nd1 : node_t) (nd2 : node_t) =
    let named_nameless =
      nd1#data#is_named && not nd1#data#is_named_orig &&
      nd2#data#is_named && not nd2#data#is_named_orig
    in
    let name1 = get_name nd1 in
    let name2 = get_name nd2 in
    let phantom = nd1#data#is_phantom || nd2#data#is_phantom in
    let unordered =
      nd2#data#is_order_insensitive ||
      try
        let un2 = find_nearest_unordered_ancestor_node nd2 in
        let un1 = Hashtbl.find mapped_node_tbl un2 in
        let ug1, ug2 = un1#gindex, un2#gindex in
        let gi1, gi2 = nd1#gindex, nd2#gindex in
        (ug1 - gi1) * (ug2 - gi2) > 0
      with _ -> false
    in
    let l = ref [] in
    l := (sprintf "\"from_loc\":%s,\"to_loc\":%s" (get_loc nd1) (get_loc nd2)) :: !l;
    l := (sprintf "\"from_cat\":%s,\"to_cat\":%s" (get_cat nd1) (get_cat nd2)) :: !l;
    if name1 <> "" then
      l := (sprintf "\"from_name\":\"%s\"" name1) :: !l;
    if name2 <> "" then
      l := (sprintf "\"to_name\":\"%s\"" name2) :: !l;
    if named_nameless then
      l := "\"named_nameless\":true" :: !l;
    if phantom then
      l := "\"phantom\":true" :: !l;
    if unordered then
      l := "\"unordered\":true" :: !l;
    "{"^(String.concat "," !l)^"}"

end


class ['node_t] c cenv = object (self : 'self)

  val mutable use_crossing_or_incompatible_matches_count_cache = false

  method use_crossing_or_incompatible_matches_count_cache =
    use_crossing_or_incompatible_matches_count_cache

  val crossing_or_incompatible_matches_count_cache = Hashtbl.create 0

  val mutable size_of_crossing_or_incompatible_matches_count_cache = 0
  method size_of_crossing_or_incompatible_matches_count_cache =
    size_of_crossing_or_incompatible_matches_count_cache +
      (Hashtbl.length crossing_or_incompatible_matches_count_cache)

  method clear_crossing_or_incompatible_matches_count_cache =
    if use_crossing_or_incompatible_matches_count_cache then begin
      size_of_crossing_or_incompatible_matches_count_cache <-
	size_of_crossing_or_incompatible_matches_count_cache +
	  (Hashtbl.length crossing_or_incompatible_matches_count_cache);
      Hashtbl.clear crossing_or_incompatible_matches_count_cache;

      DEBUG_MSG "crossing_or_incompatible_matches_count_cache cleared"
    end

  val mutable crossing_or_incompatible_matches_count_cache_hit_count = 0
  method crossing_or_incompatible_matches_count_cache_hit_count =
    crossing_or_incompatible_matches_count_cache_hit_count

  val mutable cenv = cenv
(*
  val tree1 = cenv#tree1
  val tree2 = cenv#tree2
*)
  method search_node_by_uid1 u = cenv#tree1#search_node_by_uid u
  method search_node_by_uid2 u = cenv#tree2#search_node_by_uid u

  val mutable starting_uid_pairs_for_glueing = ([] : (UID.t * UID.t) list)
  method clear_starting_uid_pairs_for_glueing = starting_uid_pairs_for_glueing <- []

  method set_starting_uid_pairs_for_glueing l =
    starting_uid_pairs_for_glueing <- l

  method add_starting_uid_pairs_for_glueing l =
    starting_uid_pairs_for_glueing <- l @ starting_uid_pairs_for_glueing

  method add_starting_uid_pair_for_glueing p =
    starting_uid_pairs_for_glueing <- p :: starting_uid_pairs_for_glueing

  method starting_uid_pairs_for_glueing = starting_uid_pairs_for_glueing

  val mutable locked_uids = (Hashtbl.create 0 : (UID.t, Key.t) Hashtbl.t)

  method is_locked_uid u = Hashtbl.mem locked_uids u

  method lock_uid ?(key=Key.any_key) u =
    DEBUG_MSG "locking %a with %s" UID.ps u (Key.to_string key);
    Hashtbl.add locked_uids u key

  method unlock_uid u =
    DEBUG_MSG "unlocking %a" UID.ps u;
    Hashtbl.remove locked_uids u

  method key_of_locked_uid u = Hashtbl.find locked_uids u


  val mutable locked_mappings = (Xset.create 0 : (UID.t * UID.t) Xset.t)

  method lock_mapping u1 u2 = Xset.add locked_mappings (u1, u2)
  method is_locked_mapping u1 u2 = Xset.mem locked_mappings (u1, u2)


  val mutable stable_pairs = (Hashtbl.create 0: (UID.t, UID.t) Hashtbl.t)
  method stable_pairs = stable_pairs
  method set_stable_pairs ps = stable_pairs <- ps
  method is_stable_pair u1 u2 =
    try
      let us = Hashtbl.find_all stable_pairs u1 in
      List.memq u2 us
    with
      Not_found -> false

  method add_stable_pair u1 u2 =
    try
      let u1' = Hashtbl.find stable_pairs u1 in
      if u1' <> u2 then
	Hashtbl.add stable_pairs u1 u2
    with
      Not_found -> Hashtbl.add stable_pairs u1 u2

  method find_stable_pair u1 =
    try
      Hashtbl.find_all stable_pairs u1
    with
      Not_found -> []

  method iter_stable_pairs f = Hashtbl.iter f stable_pairs

(*
  val weak_dom = (Hashtbl.create 0: (UID.t, bool) Hashtbl.t)
  val weak_cod = (Hashtbl.create 0: (UID.t, bool) Hashtbl.t)
  method add_weak_pair (u1, u2) =
    Hashtbl.replace weak_dom u1 true;
    Hashtbl.replace weak_cod u2 true;
  method mem_dom_weak u = Hashtbl.mem weak_dom u
  method mem_cod_weak u = Hashtbl.mem weak_cod u
*)

  val map = (Hashtbl.create 0 : (UID.t, UID.t) Hashtbl.t)

  (* for settled nodes *)
  val s_map = (Hashtbl.create 0 : (UID.t, UID.t) Hashtbl.t)

  val mutable settled_roots = (Xset.create 0 : (UID.t * UID.t) Xset.t)

  val rev_map = (Hashtbl.create 0 : (UID.t, UID.t) Hashtbl.t)
  val rev_s_map = (Hashtbl.create 0 : (UID.t, UID.t) Hashtbl.t)

  val mutable blacklist1 = (Xset.create 0 : UID.t Xset.t)
  val mutable blacklist2 = (Xset.create 0 : UID.t Xset.t)

  method set_blacklist1 h = blacklist1 <- h
  method set_blacklist2 h = blacklist2 <- h

  method size = (Hashtbl.length map) + (Hashtbl.length s_map)

  method iter_unsettled f = Hashtbl.iter f map
  method iter_settled f = Hashtbl.iter f s_map

  method iter (f : UID.t -> UID.t -> unit) =
    begin
      Hashtbl.iter f map;
      Hashtbl.iter f s_map
    end

  method private _iter_sorted cmp dom f =
    let sorted_dom = List.fast_sort cmp dom in
    List.iter
      (fun u1 ->
	try
	  let u2 = self#find u1 in
	  f u1 u2
	with
	  Not_found -> assert false
      ) sorted_dom

  method iter_sorted cmp f =
    self#_iter_sorted cmp self#dom_unsettled f;
    self#_iter_sorted cmp self#dom_settled f

  method iter_unsettled_sorted cmp f =
    self#_iter_sorted cmp self#dom_unsettled f

  method iter_settled_sorted cmp f =
    self#_iter_sorted cmp self#dom_settled f


  method iter_rev f = begin Hashtbl.iter f rev_map; Hashtbl.iter f rev_s_map end


  method find_unsettled uid =
    Hashtbl.find map uid

  method find_settled uid =
    Hashtbl.find s_map uid

  method find uid =
    try
      Hashtbl.find map uid
    with
      Not_found ->
	Hashtbl.find s_map uid

  method inv_find uid =
    try
      Hashtbl.find rev_map uid
    with
      Not_found ->
	Hashtbl.find rev_s_map uid


  method add_unsettled uid1 uid2 =
    DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2;
    let conflict = add map rev_map s_map rev_s_map uid1 uid2 in
    self#clear_crossing_or_incompatible_matches_count_cache;
    conflict


  method add_settled ?(stable=false) uid1 uid2 =

    DEBUG_MSG "[stable=%B]: %a-%a" stable UID.ps uid1 UID.ps uid2;

    let conflict =
      if Xset.mem blacklist1 uid1 || Xset.mem blacklist2 uid2 then
        self#add_unsettled uid1 uid2

      else begin
        let c = add s_map rev_s_map map rev_map uid1 uid2 in

        if stable then
	  self#add_stable_pair uid1 uid2; (* MODIFIED-> ADOPTED *)

        c
      end
    in
    self#clear_crossing_or_incompatible_matches_count_cache;
    conflict


  method add_settled_roots uid1 uid2 =
    if Xset.mem blacklist1 uid1 || Xset.mem blacklist2 uid2 then
      ()
    else
      Xset.add settled_roots (uid1, uid2)

  method is_settled_root_pair uid1 uid2 =
    Xset.mem settled_roots (uid1, uid2)

  method iter_settled_roots f =
    Xset.iter (fun (uid1, uid2) -> f uid1 uid2) settled_roots

  method iter_settled_roots_sorted cmp f =
    let sorted =
      List.fast_sort (fun (u1, _) (u2, _) -> cmp u1 u2) (Xset.to_list settled_roots)
    in
    List.iter (fun (uid1, uid2) -> f uid1 uid2) sorted

  method merge_no_override (m : 'self) =
    m#iter_settled
      (fun u1 u2 ->
	if not (self#mem_dom u1 || self#mem_cod u2) then
	  ignore (self#add_settled ~stable:false u1 u2)
      );
    m#iter_unsettled
      (fun u1 u2 ->
	if not (self#mem_dom u1 || self#mem_cod u2) then
	  ignore (self#add_unsettled u1 u2)
      );
    m#iter_settled_roots self#add_settled_roots

  method merge (m : 'self) =
    let conflicts = ref [] in
    m#iter_settled
      (fun u1 u2 ->
        let c = self#add_settled ~stable:false u1 u2 in
        conflicts := c :: !conflicts
      );
    m#iter_unsettled
      (fun u1 u2 ->
        let c = self#add_unsettled u1 u2 in
        conflicts := c :: !conflicts
      );
    m#iter_settled_roots self#add_settled_roots;
    !conflicts

  method check_for_ref (ref_upairs : Spec.upairs_t) (m : 'self) =
    let check u1 u2 =
      (*DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;*)

      if not (ref_upairs#mem u1 u2) then begin
        DEBUG_MSG "adding %a-%a" UID.ps u1 UID.ps u2;
        ref_upairs#add u1 u2
      end

    in
    m#iter_unsettled check;
    m#iter_settled check

  method merge_checked (m : 'self) = (* only applicable to pre_uidmapping *)
    let invalidated_settled_root_tbl = Hashtbl.create 0 in

    let check adder u1 u2 =

      DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;

      let mem1 = self#mem_dom u1 in
      let mem2 = self#mem_cod u2 in
      if mem1 || mem2 then begin
	let u1' = try self#find u1 with _ -> u2 in
	let u2' = try self#inv_find u2 with _ -> u1 in

	if u1' <> u2 || u2' <> u1 then begin (* conflict *)
	  let n1 = self#search_node_by_uid1 u1 in
	  let n2 = self#search_node_by_uid2 u2 in
	  let score = cenv#get_adjacency_score n1 n2 in

	  let cond1 =
	    if mem1 then begin

	      DEBUG_MSG "conflict with %a-%a" UID.ps u1 UID.ps u1';

	      let n1' = self#search_node_by_uid2 u1' in
	      let score' = cenv#get_adjacency_score n1 n1' in
	      score > score'
	    end
	    else
	      true
	  in
          let cond2 =
            if mem2 then begin

              DEBUG_MSG "conflict with %a-%a" UID.ps u2' UID.ps u2;

              let n2' = self#search_node_by_uid1 u2' in
              let score' = cenv#get_adjacency_score n2' n2 in
              score > score'
            end
            else
              true
          in
          if cond1 && cond2 then
            adder u1 u2

          else
            let c1 = Array.to_list (Array.map (fun n -> n#uid) n1#initial_children) in
            let c2 = Array.to_list (Array.map (fun n -> n#uid) n2#initial_children) in

            DEBUG_MSG "not added: %a-%a" UID.ps u1 UID.ps u2;

            assert (List.length c1 = List.length c2);
            Hashtbl.add invalidated_settled_root_tbl (u1, u2) (List.combine c1 c2)

        end
        else
          DEBUG_MSG "already have %a-%a" UID.ps u1 UID.ps u2

      end
      else
	adder u1 u2
    in

    m#iter_settled
      (check (fun u1 u2 -> ignore (self#add_settled ~stable:false u1 u2)));
    m#iter_unsettled (check (fun u1 u2 -> ignore (self#add_unsettled u1 u2)));

    let rec get_settled_roots (uid1, uid2) =
      try
	let pairs = Hashtbl.find invalidated_settled_root_tbl (uid1, uid2) in
	List.flatten (List.map get_settled_roots pairs)
      with
	Not_found -> [(uid1, uid2)]
    in

    m#iter_settled_roots
      (fun uid1 uid2 ->
	let pairs = get_settled_roots (uid1, uid2) in
	match pairs with
	| [] -> self#add_settled_roots uid1 uid2
	| _ ->
	    List.iter
	      (fun (u1, u2) ->
		self#add_settled_roots u1 u2
	      ) pairs
      )


  method mem uid = Hashtbl.mem map uid || Hashtbl.mem s_map uid

  method mem_unsettled uid = Hashtbl.mem map uid

  method mem_settled uid = Hashtbl.mem s_map uid

  method has_unsettled_mapping uid1 uid2 =
    try
      let uid1' = self#find_unsettled uid1 in
      uid1' = uid2
    with
      Not_found -> false


  method has_settled_mapping uid1 uid2 =
    try
      let uid1' = self#find_settled uid1 in
      uid1' = uid2
    with
      Not_found -> false

  method has_mapping uid1 uid2 =
    self#has_unsettled_mapping uid1 uid2 ||
    self#has_settled_mapping uid1 uid2


  method remove uid1 uid2 =
    DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2;
    try
      if self#find uid1 = uid2 then begin
        if self#mem_dom_settled uid1 || self#mem_cod_settled uid2 then begin
          self#remove_settled uid1 uid2;

          DEBUG_MSG "settled map %a-%a removed" UID.ps uid1 UID.ps uid2
        end;
        if self#mem_dom_unsettled uid1 || self#mem_cod_unsettled uid2 then begin
          (*Hashtbl.remove*)tbl_remove map uid1;
          (*Hashtbl.remove*)tbl_remove rev_map uid2;

          DEBUG_MSG "unsettled map %a-%a removed" UID.ps uid1 UID.ps uid2
        end;
        self#clear_crossing_or_incompatible_matches_count_cache;
        true
      end
      else
        false
    with
      Not_found -> false

  method remove_settled uid1 uid2 =
    DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2;
    try
      if self#find uid1 = uid2 then begin

        let tree1 = cenv#tree1 in
        let tree2 = cenv#tree2 in

        let nd1 = self#search_node_by_uid1 uid1 in
        let nd2 = self#search_node_by_uid2 uid2 in

        if not (self#is_settled_root_pair uid1 uid2) then begin
          let gi1 = nd1#gindex in
          let gi2 = nd2#gindex in
          self#iter_settled_roots
	    (fun u1 u2 ->
	      let n1 = self#search_node_by_uid1 u1 in
	      let n2 = self#search_node_by_uid2 u2 in
	      if
                (tree1#initial_leftmost n1)#gindex <= gi1 && gi1 < n1#gindex ||
                (tree2#initial_leftmost n2)#gindex <= gi2 && gi2 < n2#gindex
              then
	        Xset.remove settled_roots (u1, u2)
	    )
        end;
        (*Hashtbl.remove*)tbl_remove s_map uid1;
        (*Hashtbl.remove*)tbl_remove rev_s_map uid2;
        Xset.remove settled_roots (uid1, uid2);
        let ca1 = nd1#initial_children in
        Array.iteri
          (fun i c ->
            try
              let c' = self#search_node_by_uid2 (self#find c#uid) in
              if c'#initial_parent == nd2 then
	        Xset.add settled_roots (c#uid, c'#uid)
            with
              _ -> ()
          ) ca1;
        self#clear_crossing_or_incompatible_matches_count_cache
      end
    with
      Not_found -> ()



  method filter f =
    Hashtbl.iter
      (fun u1 u2 ->
	if not (f u1 u2) then begin
	  Hashtbl.remove map u1;
	  Hashtbl.remove rev_map u2;
	  Hashtbl.remove rev_s_map u2
	end
      ) map;
    Hashtbl.iter
      (fun u1 u2 ->
	if not (f u1 u2) then begin
	  Hashtbl.remove s_map u1;
	  Hashtbl.remove rev_s_map u2;
	  Hashtbl.remove rev_map u2;
	  Xset.remove settled_roots (u1, u2);
	end
      ) s_map

  method promote uid1 uid2 =
    if self#has_unsettled_mapping uid1 uid2 then begin
      let _ = self#remove uid1 uid2 in
      self#add_settled ~stable:false uid1 uid2
    end
    else
      None, None

  method demote uid1 uid2 =
    if self#has_settled_mapping uid1 uid2 then begin
      let _ = self#remove uid1 uid2 in
      self#add_unsettled uid1 uid2
    end
    else
      None, None


  method private _to_list m =
    let l = ref [] in
    Hashtbl.iter (fun u1 u2 -> l := (u1, u2) :: !l) m;
    !l

  method private _dom m =
    let l = ref [] in
    Hashtbl.iter (fun u1 u2 -> l := u1 :: !l) m;
    !l

  method private _cod m =
    let l = ref [] in
    Hashtbl.iter (fun u1 u2 -> l := u2 :: !l) m;
    !l


  method to_list_unsettled = self#_to_list map
  method to_list_settled = self#_to_list s_map
  method to_list = self#to_list_unsettled @ self#to_list_settled

  method cod_unsettled = self#_cod map
  method dom_unsettled = self#_dom map

  method cod_settled = self#_cod s_map
  method dom_settled = self#_dom s_map

  method cod = self#cod_unsettled @ self#cod_settled
  method dom = self#dom_unsettled @ self#dom_settled


  method to_string_gid =
    let gi tree u = (tree#search_node_by_uid u)#gindex in
    let buf = Buffer.create 0 in
    let gi_pairs = ref [] in
    let cmp (gi1, _) (gi2, _) = Stdlib.compare gi1 gi2 in

    let add uid1 uid2 =
(*
      let n1 = tree1#search_node_by_uid uid1 in
      let n2 = tree2#search_node_by_uid uid2 in
*)
      let tree1, tree2 = cenv#tree1, cenv#tree2 in
      let gi1 = gi tree1 uid1 in
      let gi2 = gi tree2 uid2 in
(*
      DEBUG_MSG "%a-%a -> %a-%a (%s-%s)" UID.ps uid1 UID.ps uid2 GI.ps gi1 GI.ps gi2 n1#to_string n2#to_string;
*)
      gi_pairs := (gi1, gi2) :: !gi_pairs
    in
    let pr mark =
    List.iter
      (fun (gi1, gi2) ->
	Buffer.add_string buf
	  (sprintf "UIDmapping#to_string_gid: %s: %a-%a\n" mark GI.ps gi1 GI.ps gi2)
      ) (List.fast_sort cmp !gi_pairs)
    in
    self#iter_unsettled add;
    pr "U";
    gi_pairs := [];
    self#iter_settled add;
    pr "S";
    Buffer.contents buf

  method to_string =
    let l1 = ref [] in
    let l1s = ref [] in
    let l2 = ref [] in

    self#iter_unsettled
      (fun uid1 uid2 -> l1 := (uid1, uid2)::!l1);
    self#iter_settled
      (fun uid1 uid2 -> l1s := (uid1, uid2)::!l1s);
    self#iter_rev
      (fun uid1 uid2 -> l2 := (uid1, uid2)::!l2);

    let sort =
      List.fast_sort (fun (u, _) (u', _) -> UID.compare u u')
    in

    let unsettled = sort !l1 in
    let settled = sort !l1s in
    let rev = sort !l2 in

    let reduce list =
      let (r, reduced) =
	List.fold_left
	  (fun (range, l) (u1, u2) ->
	    match range with
	    | [] -> [(u1, u2)], l

	    | [(u1', u2')] ->
		if u1 = UID.succ u1' && u2 = UID.succ u2' then
		  [(u1', u2'); (u1, u2)], l
		else
		  [(u1, u2)], [(u1', u2')]::l

	    | [(u1', u2'); (u1'', u2'')] ->
		if u1 = UID.succ u1'' && u2 = UID.succ u2'' then
		  [(u1', u2'); (u1, u2)], l
		else
		  [(u1, u2)], [(u1', u2'); (u1'', u2'')]::l

	    | _ -> assert false
	  ) ([], []) list
      in
      List.rev
	(match r with
	| [] -> reduced
	| [p] -> r::reduced
	| [p1; p2] -> r::reduced
	| _ -> assert false
	)
    in

    let range_to_string = function
      | [u1, u2] ->
	  sprintf "%a-%a" UID.ps u1 UID.ps u2
      | [u1, u2; u1', u2'] ->
	  sprintf "[%a:%a]-[%a:%a]"
	    UID.ps u1 UID.ps u1' UID.ps u2 UID.ps u2'
      | _ -> assert false
    in
    let sz = List.length !l1 in
    let szs = List.length !l1s in
    let szr = List.length !l2 in
    sprintf "map size: %d (rev map size: %d)" (sz+szs) szr ^
    sprintf "\nUNSETTLED (size=%d):\n" sz ^
    (Xlist.to_string range_to_string ";" (reduce unsettled)) ^
    sprintf "\nSETTLED (size=%d):\n" szs ^
    (Xlist.to_string range_to_string ";" (reduce settled)) ^
    sprintf "\nREV_MAP (size=%d):\n" szr ^
    (Xlist.to_string range_to_string ";" (reduce rev))

  method dump fname =
    let list =
      List.fast_sort
	(fun (u1, u2) (u3, u4) -> compare u1 u3) self#to_list
    in
    try
      let ch = open_out fname in
      List.iter
	(fun (uid1, uid2) ->
	  fprintf ch "%19a -- %a\n" UID.r uid1 UID.r uid2
	) list;
      close_out ch
    with
      Sys_error s -> WARN_MSG s

  method dump_json ?(comp=Comp.none) fname =
    let tree1 = cenv#tree1 in
    let tree2 = cenv#tree2 in
    let _fprintf ch fmt =
      Printf.ksprintf (fun s -> ignore (ch#output_ s 0 (String.length s))) fmt
    in
    let dump_node ch nd =
      let loc = nd#data#src_loc in
      let so = loc.Loc.start_offset in
      let eo = loc.Loc.end_offset in
      let sl = loc.Loc.start_line in
      let el = loc.Loc.end_line in
      let lab = nd#data#get_category in
      _fprintf ch "{";
      _fprintf ch "\"label\":\"%s\"" lab;
      _fprintf ch ",\"start_offset\":%d,\"end_offset\":%d,\"start_line\":%d,\"end_line\":%d" so eo sl el;
      _fprintf ch "}";
    in
    let dump_map ?(comma=false) ch map =
      let comma_flag = ref comma in
      Hashtbl.iter
        (fun u1 u2 ->
          let n1 = tree1#search_node_by_uid u1 in
          let n2 = tree2#search_node_by_uid u2 in

          if !comma_flag then
            _fprintf ch ",";
          _fprintf ch "[";
          dump_node ch n1;
          _fprintf ch ",";
          dump_node ch n2;
          _fprintf ch "]";
          comma_flag := true;

        ) map;
      !comma_flag
    in
    try
      let ch = new Xchannel.out_channel ~comp (Xchannel.Destination.of_file fname) in
      _fprintf ch "[";
      let comma = dump_map ch map in
      let _ = dump_map ~comma ch s_map in
      _fprintf ch "]";
      ch#close
    with
    | Xchannel.Error s -> WARN_MSG s


  method dump_with_info ?(comp=Comp.none) fname =
    let _fprintf ch fmt =
      Printf.ksprintf (fun s -> ignore (ch#output_ s 0 (String.length s))) fmt
    in

    let list_unsettled =
      List.fast_sort
	(fun (u1, u2) (u3, u4) -> compare u1 u3) self#to_list_unsettled
    in
    let list_settled =
      List.fast_sort
	(fun (u1, u2) (u3, u4) -> compare u1 u3) self#to_list_settled
    in
    let get_gid nd =
      let gid = nd#data#gid in
      if gid > 0 then gid else nd#gindex
    in
    try
      let ch = new Xchannel.out_channel ~comp (Xchannel.Destination.of_file fname) in
      let f =
	fun (uid1, uid2) ->
	  let nd1 = self#search_node_by_uid1 uid1 in
	  let nd2 = self#search_node_by_uid2 uid2 in
	  let k = if nd1#data#eq nd2#data then "E" else "R" in
	  _fprintf ch "%s[%a:%a]%s -- [%a:%a]%s\n" k
	    UID.ps uid1 GI.ps (get_gid nd1) nd1#data#to_string
	    UID.ps uid2 GI.ps (get_gid nd2) nd2#data#to_string
      in
      _fprintf ch
	"%d unsettled entries and %d settled entries\n"
	(List.length list_unsettled) (List.length list_settled);
      _fprintf ch "*** Unsettled ***\n";
      List.iter f list_unsettled;
      _fprintf ch "*** Settled ***\n";
      List.iter f list_settled;
      ch#close
    with
    | Xchannel.Error s -> WARN_MSG s


  method dump_gid_json ?(comp=Comp.none) is_mov fname =
    let mapped_node_tbl = Hashtbl.create 0 in
    let _spl, _mvl =
      List.partition_map
        (fun (uid1, uid2) ->
          let nd1 = self#search_node_by_uid1 uid1 in
          let nd2 = self#search_node_by_uid2 uid2 in
          Hashtbl.add mapped_node_tbl nd2 nd1;
          if is_mov uid1 uid2 then
            Right (nd1, nd2)
          else
            Left (nd1, nd2)
        ) self#to_list
    in
    let spl = List.fast_sort (fun (n1, n2) (n3, n4) -> compare n1#gindex n3#gindex) _spl in
    let mvl = List.fast_sort (fun (n1, n2) (n3, n4) -> compare n1#gindex n3#gindex) _mvl in

    let _fprintf = Json._fprintf in
    let get_gid = Json.get_gid in
    let get_info = Json.get_info mapped_node_tbl in

    try
      let d = Filename.dirname fname in
      if not (Xfile.dir_exists d) then
        Xfile.mkdir d;
      let ch = new Xchannel.out_channel ~comp (Xchannel.Destination.of_file fname) in
      let dump ch l =
        let comma_flag = ref false in
        List.iter
          (fun (nd1, nd2) ->
            if !comma_flag then
              _fprintf ch ",";
            let info = get_info nd1 nd2 in
            _fprintf ch "[%a,%a,%s]" GI.rs (get_gid nd1) GI.rs (get_gid nd2) info;
            comma_flag := true
          ) l
      in
      _fprintf ch "{\"SPM\":[";
      dump ch spl;
      _fprintf ch "],\"MM\":[";
      dump ch mvl;
      _fprintf ch "]}";
      ch#close
    with
    | Xchannel.Error s -> WARN_MSG s


  method print_status =
    DEBUG_MSG "num of unsettled bindings: %d" (Hashtbl.length map);
    DEBUG_MSG "num of settled bindings: %d" (Hashtbl.length s_map)

  method setup_rev_map =
    DEBUG_MSG "setup_rev_map: deprecated!"

(*
  method setup_rev_map =
  Hashtbl.clear rev_map;
  Hashtbl.clear rev_s_map;
  self#iter_unsettled (fun u1 u2 -> Hashtbl.add rev_map u2 u1);
  self#iter_settled (fun u1 u2 -> Hashtbl.add rev_s_map u2 u1)
 *)

  method mem_dom u = self#mem u
  method mem_cod u = Hashtbl.mem rev_map u || Hashtbl.mem rev_s_map u

  method mem_dom_settled u = self#mem_settled u
  method mem_cod_settled u = Hashtbl.mem rev_s_map u

  method mem_dom_unsettled u = self#mem_unsettled u
  method mem_cod_unsettled u = Hashtbl.mem rev_map u

  method cleanup_ghost =
    let is_ghost_node nd = nd#data#src_loc = Loc.ghost in
    let is_ghost_uid tree uid =
      let nd =
	try
	  tree#search_node_by_uid uid
	with
	  Not_found -> assert false
      in
      is_ghost_node nd
    in
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    self#iter
      (fun uid1 uid2 ->
	if is_ghost_uid tree1 uid1 || is_ghost_uid tree2 uid2 then begin
	  DEBUG_MSG "cleanup_ghost: %a-%a" UID.ps uid1 UID.ps uid2;
	  ignore (self#remove uid1 uid2)
	end
      )


  method count_p_mapping
      (p : 'node_t -> 'node_t -> 'node_t -> 'node_t -> bool)
      (nd1 : 'node_t)
      (nd2 : 'node_t)
      =
    let count = ref 0 in
    let f ?(settled=false) u1 u2 =
      try
        let tree1, tree2 = cenv#tree1, cenv#tree2 in
	let n1 = self#search_node_by_uid1 u1 in
	let n2 = self#search_node_by_uid2 u2 in
	if p nd1 nd2 n1 n2 then begin
(*
          DEBUG_MSG " crossing: %a-%a -> %a-%a"
	      UID.ps nd1#uid UID.ps nd2#uid UID.ps u1 UID.ps u2;
*)
	  if settled then
	    let step = n1#gindex - (tree1#initial_leftmost n1)#gindex + 1 in
	    count := !count + step
	  else
	    incr count
	end
      with
	Not_found ->
	  WARN_MSG "node not found: %a-%a" UID.ps u1 UID.ps u2
    in
    self#iter_unsettled f;
    self#iter_settled_roots (f ~settled:true);
    !count

  method count_crossing_mapping nd1 nd2 =
    self#count_p_mapping is_crossing nd1 nd2

  method count_crossing_matches nd1 nd2 =
    self#count_p_mapping
      (fun nd1 nd2 n1 n2 ->
	is_crossing nd1 nd2 n1 n2 && n1#data#eq n2#data
      )
      nd1 nd2

  method count_crossing_or_incompatible_matches nd1 nd2 =
    try
      if not self#use_crossing_or_incompatible_matches_count_cache then
	raise Not_found;

      let count =
	Hashtbl.find crossing_or_incompatible_matches_count_cache (nd1, nd2)
      in

      DEBUG_MSG "cache hit!";

      crossing_or_incompatible_matches_count_cache_hit_count <-
	crossing_or_incompatible_matches_count_cache_hit_count + 1;

      count
    with
      Not_found ->
        let tree1, tree2 = cenv#tree1, cenv#tree2 in
	let count =
	  self#count_p_mapping
	    (fun nd1 nd2 n1 n2 ->
	      (is_crossing nd1 nd2 n1 n2 || is_incompatible tree1 tree2 nd1 nd2 n1 n2) &&
	      n1#data#eq n2#data
	    )
	    nd1 nd2
	in
	if self#use_crossing_or_incompatible_matches_count_cache then
	  Hashtbl.add crossing_or_incompatible_matches_count_cache (nd1, nd2) count;

	count

  method count_compatible_noncrossing_matches nd1 nd2 =
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    let count =
      self#count_p_mapping
	(fun nd1 nd2 n1 n2 ->
	  (not (is_crossing nd1 nd2 n1 n2) && not (is_incompatible tree1 tree2 nd1 nd2 n1 n2)) &&
	  n1#data#eq n2#data
	)
	nd1 nd2
    in
    count



  method iter_p_mapping
      (p : 'node_t -> 'node_t -> 'node_t -> 'node_t -> bool)
      (nd1 : 'node_t)
      (nd2 : 'node_t)
      (f : UID.t -> UID.t -> unit)
      =
    self#iter
      (fun u1 u2 ->
	try
	  let n1 = self#search_node_by_uid1 u1 in
	  let n2 = self#search_node_by_uid2 u2 in
	  if p nd1 nd2 n1 n2 then
	    f u1 u2
	with
	  Not_found ->
	    WARN_MSG "node not found: %a-%a" UID.ps u1 UID.ps u2
      )

  method iter_crossing_mapping nd1 nd2 f =
    self#iter_p_mapping is_crossing nd1 nd2 f

  method iter_incompatible_mapping nd1 nd2 f =
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    self#iter_p_mapping (is_incompatible tree1 tree2) nd1 nd2 f

  method iter_crossing_or_incompatible_mapping nd1 nd2 f =
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    self#iter_p_mapping
      (fun nd1 nd2 n1 n2 ->
	is_crossing nd1 nd2 n1 n2 ||
	is_incompatible tree1 tree2 nd1 nd2 n1 n2
      )
      nd1 nd2 f



  val mutable partition_a1 = [||]
  val mutable partition_a2 = [||]

  method setup_partitions =
    let nds1, nds2 = ref [], ref [] in
    self#iter_settled_roots
      (fun u1 u2 ->
	let n1 = self#search_node_by_uid1 u1 in
	let n2 = self#search_node_by_uid2 u2 in
	if n1#data#is_partition && n1#data#_digest <> None then begin
	  nds1 := n1 :: !nds1;
	  nds2 := n2 :: !nds2;
	end
    );
    let a1 = Array.of_list !nds1 in
    let a2 = Array.of_list !nds2 in
    let cmp n1 n2 = Stdlib.compare n1#gindex n2#gindex in
    Array.fast_sort cmp a1;
    Array.fast_sort cmp a2;
    let al1 = Array.map (fun n -> n#data#_label) a1 in
    let al2 = Array.map (fun n -> n#data#_label) a2 in
    let m, _, _, _ = Adiff.adiff al1 al2 in
    let p1, p2 = List.split (List.map (fun (i, j) -> a1.(i), a2.(j)) m) in
    let pa1 = Array.of_list p1 in
    let pa2 = Array.of_list p2 in
    Array.fast_sort cmp pa1;
    Array.fast_sort cmp pa2;

    let get_to_be_filtered tree pa =
      let l = ref [] in
      Array.iteri
	(fun i n ->
	  let lmn = tree#initial_leftmost n in
	  try
	    if lmn#gindex - 1 = pa.(i-1)#gindex then
	      l := n#uid :: !l
	  with
	    _ -> ()
	) pa;
      !l
    in
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    let to_be_filtered1 = get_to_be_filtered tree1 pa1 in
    let to_be_filtered2 = get_to_be_filtered tree2 pa2 in

    let fp1, fp2 = ref [], ref [] in
    for i = (Array.length pa1) - 1 downto 0 do
      if
	not (List.memq pa1.(i)#uid to_be_filtered1 &&
	     List.memq pa2.(i)#uid to_be_filtered2)
      then begin
	fp1 := pa1.(i)#gindex :: !fp1;
	fp2 := pa2.(i)#gindex :: !fp2
      end
    done;

    partition_a1 <- Array.of_list !fp1;
    partition_a2 <- Array.of_list !fp2;

    BEGIN_DEBUG
      Array.iteri
	(fun i gi1 ->
	  let n1 = tree1#search_node_by_gindex gi1 in
	  let n2 = tree2#search_node_by_gindex (partition_a2.(i)) in
	  DEBUG_MSG "partition[%d]: %a-%a %s" i
	    UID.ps n1#uid UID.ps n2#uid n1#data#label
	) partition_a1
    END_DEBUG


  method private _partition_nodes partition_a (nds : 'node_t list) =
    let plen = Array.length partition_a in

    if plen = 0 then
      raise (Invalid_argument "UIDmapping.c#_partition_nodes");

    let a = Array.make (plen + 1) (ref []) in
    Array.iteri (fun i _ -> a.(i) <- ref []) a;
    List.iter
      (fun n ->
	let g = n#gindex in

	if g < partition_a.(0) then
	  a.(0) := n :: !(a.(0));

	if plen > 1 then
	  for i = 1 to plen - 1 do
	    if partition_a.(i - 1) < g && g < partition_a.(i) then
	      a.(i) := n :: !(a.(i))
	  done;

	if g > partition_a.(plen - 1) then
	  a.(plen) := n :: !(a.(plen))

      ) nds;

    BEGIN_DEBUG
      Array.iteri
	(fun i ndsr ->
	  if !ndsr <> [] then
	    DEBUG_MSG "partition[%d] = [%s]" i
	      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" !ndsr)
	) a
    END_DEBUG;

    Array.map (fun lr -> !lr) a


  method partition_nodes1 nds = self#_partition_nodes partition_a1 nds
  method partition_nodes2 nds = self#_partition_nodes partition_a2 nds


  method get_subtree_match_score (nd1 : 'node_t) (nd2 : 'node_t) =
    let score = ref 0 in
    let tree1, tree2 = cenv#tree1, cenv#tree2 in
    tree1#fast_scan_whole_initial_subtree nd1
      (fun n1 ->
	let us2 = self#find_stable_pair n1#uid in
	List.iter
	  (fun u2 ->
	    let n2 = tree2#search_node_by_uid u2 in
	    if tree2#initial_subtree_mem nd2 n2 then
	      incr score
	  ) us2;
(*
	begin
	  try
	    let u2 = self#find_settled n1#uid in
	    let n2 = tree2#search_node_by_uid u2 in
	    if tree2#initial_subtree_mem nd2 n2 then
	      incr score
	  with
	    Not_found -> ()
	end;
	begin
	  try
	    let u2 = self#find_unsettled n1#uid in
	    let n2 = tree2#search_node_by_uid u2 in
	    if tree2#initial_subtree_mem nd2 n2 then
	      incr score
	  with
	    Not_found -> ()
	end
*)
      );
    !score


  method get_proximity
      ?(extra=Proximity.null_uid_tbl) (nd1 : 'node_t) (nd2 : 'node_t)
      =
  let ancs1 = Array.of_list nd1#initial_ancestor_nodes in
  let ancs2 = Array.of_list nd2#initial_ancestor_nodes in
  let lai1 = (Array.length ancs1) - 1 in
  let lai2 = (Array.length ancs2) - 1 in

  let prox = new Proximity.node_proximity in

  let _cands = ref [] in

  for i = lai1 downto 0 do
    let ui = ancs1.(i)#uid in
    try
      let u, high_conf =
	try
	  let x = self#find ui in
	  x, true
	with
	  Not_found ->
	    let x = Hashtbl.find extra ui in
	    DEBUG_MSG "(%a,%a): pivot derived from extra map"
	      UID.ps nd1#uid UID.ps nd2#uid;
	    x, false
      in
      for j = lai2 downto 0 do
	if u = ancs2.(j)#uid then
	  let ni = self#search_node_by_uid1 ui in
	  let n = self#search_node_by_uid2 u in
	  _cands := (high_conf, ni, n, i + j) :: !_cands
      done
    with
      Not_found -> ()
  done;

  let cands =
    List.fast_sort
      (fun (_, _, _, p0) (_, _, _, p1) -> Stdlib.compare p1 p0)
      !_cands
  in
  begin
    match cands with
    | [] -> ()
    | (high, n1, n2, p)::rest ->

	DEBUG_MSG "(%a,%a): prox=%d pivot=(%a,%a) (confidence=%s)"
	  UID.ps nd1#uid UID.ps nd2#uid p UID.ps n1#uid UID.ps n2#uid
	  (if high then "high" else "low");

	prox#set_primary_prox p;
	prox#set_primary_pivot (n1, n2);
	if not high then
	  prox#lower_confidence;

	let rest' =
	  List.filter (fun (h, _, _, _) -> h = not high) rest
	in
	match rest' with
	| [] -> ()
	| (_, n1, n2, p)::_ ->
	    prox#set_secondary_prox p;
	    prox#set_secondary_pivot (n1, n2)
  end;
  prox
  (* end of method get_proximity *)

end (* of class UIDmapping.c *)
