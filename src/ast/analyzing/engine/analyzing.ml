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
(* analyzing.ml *)



module UID = Otreediff.UID
module GI = Otreediff.GIndex

let matched_sibling_ratio_thresh = 0.9

let sprintf = Printf.sprintf
let printf = Printf.printf

type node_t = Spec.node_t
type tree_t = Spec.tree_t

let ups = Misc.ups
let gps = Misc.gps
let nps = Misc.nps
let nups = Misc.nups
let ngps = Misc.ngps
let nsps = Misc.nsps
let locps = Misc.locps
let labps = Misc.labps
let ndps = Misc.ndps

(*
exception No_differences_found
*)
exception Found


module F (Label : Spec.LABEL_T) = struct

  module Postprocessing = Postprocessing.F (Label)

  let mkinfo = Info.make

  (* generates movement info from isomorphic subtrees *)
  let classify_isomorphic (tree1 : tree_t) (tree2 : tree_t) mapping iso =
    List.fold_left
      (fun r i ->
	let j = Otreediff.Mapping.find i mapping in
	let nd1, nd2 = tree1#get i, tree2#get j in
	let uid1, uid2 = nd1#uid, nd2#uid in
	let gi1, gi2 = nd1#gindex, nd2#gindex in
	let lgi1 = (tree1#initial_leftmost nd1)#gindex in (* dangerous access to global data from subtree *)
	let lgi2 = (tree2#initial_leftmost nd2)#gindex in (* dangerous access to global data from subtree *)
	let lgi_gi1, lgi_gi2 = (lgi1, gi1), (lgi2, gi2) in
	let info1, info2 = mkinfo nd1, mkinfo nd2 in


	let has_initial_parent1 = nd1#has_initial_parent in
	let has_initial_parent2 = nd2#has_initial_parent in

	if  has_initial_parent1 && has_initial_parent2 then
	  let p1, p2 = nd1#initial_parent, nd2#initial_parent in
	  let pi, pj = p1#index, p2#index in
	  begin
	    try
	      if pj = (Otreediff.Mapping.find pi mapping) then
		(Pruned.make_isomorphic uid1 lgi_gi1 uid2 lgi_gi2 info1 info2)::r
	      else
		raise Not_found

	    with Not_found ->
	      (Pruned.make_migratory uid1 lgi_gi1 uid2 lgi_gi2 info1 info2)::r
	  end

	else if has_initial_parent1 || has_initial_parent2 then
	  (Pruned.make_migratory uid1 lgi_gi1 uid2 lgi_gi2 info1 info2)::r

	else
	  (Pruned.make_isomorphic uid1 lgi_gi1 uid2 lgi_gi2 info1 info2)::r

      ) [] iso

  let not_redundant mem tree i =
    let nd = tree#get i in
    let b = not (mem tree nd) in

    DEBUG_MSG "%d -> %B" i b;

    b


  (* to avoid pruning too much *)
  let shrink_iso tree1 tree2 mapping iso1 =

    let iso2 =
      try
	List.map (fun i -> Otreediff.Mapping.find i mapping) iso1
      with Not_found -> assert false
    in

    let tbl1 = Hashtbl.create 0 in
    let tbl2 = Hashtbl.create 0 in

    let tadd tbl k v =
      try
	Hashtbl.replace tbl k (v::(Hashtbl.find tbl k))
      with Not_found ->
	Hashtbl.add tbl k [v]
    in

    let ison1 = List.map tree1#get iso1 in
    let ison2 = List.map tree2#get iso2 in

    DEBUG_MSG "[%a]" nsps ison1;

    List.iter2
      (fun nd1 nd2 ->
	tadd tbl1 nd1#parent nd1;
	tadd tbl2 nd2#parent nd2
      ) ison1 ison2;

    let cands1 = ref [] in
    let cands2 = ref [] in

    Hashtbl.iter
      (fun pnd nds ->
	if
	  List.for_all
	    (fun n -> List.memq n nds)
	    (Array.to_list pnd#children)
	then begin

	  DEBUG_MSG "all children are to be pruned: %a" nups pnd;

	  cands1 := (List.hd nds)::!cands1
	end
      ) tbl1;
    Hashtbl.iter
      (fun pnd nds ->
	if
	  List.for_all
	    (fun n -> List.memq n nds)
	    (Array.to_list pnd#children)
	then begin

	  DEBUG_MSG "all children are to be pruned: %a" nups pnd;

	  cands2 := (List.hd nds)::!cands2
	end
      ) tbl2;

    let iso1' = ref [] in

    List.iter2
      (fun nd1 nd2 ->
	if List.memq nd1 !cands1 || List.memq nd2 !cands2 then begin
	  iso1' := nd1#children_indexes @ !iso1'
	end
	else begin
	  iso1' := nd1#index::!iso1'
	end
      ) ison1 ison2;

    DEBUG_MSG "[%a]" nsps (List.rev_map tree1#get (!iso1'));

    !iso1'
  (* end of func shrink_iso *)


  (*
   * generates Edit.seq, determines nodes to be expanded, and
   * accumulates nodes to be pruned
   *)
  let analyze0
      options
      (tree1 : tree_t)
      (tree2 : tree_t)
      eds     (* edit sequence by the base algorithm *)
      mapping (* mapping by the base algorithm *)
      iso     (* isomorphic subtrees by the base algorithm *)
      pruned  (* subtrees to be pruned (global) *)
      =
    let expnd1, expnd2 = ref [], ref [] in

    let moderate_nchildren =
      Misc.moderate_nchildren ~threshold:options#moderate_nchildren_threshold
    in

    let proc_one = function
      | Otreediff.Edit.Relabel(i1, i2) ->
	  let nd1, nd2 = tree1#get i1, tree2#get i2 in

	  if nd1#is_collapsed && (moderate_nchildren nd1) then
	    if nd1#collapse_not_locked then
	      expnd1 := i1::!expnd1;
(*
	    else
	      if nd1#data#eq nd2#data then begin
		expnd1 := i1::!expnd1;
		nd1#unlock_collapse;
		Array.iter (fun n -> n#lock_collapse) nd1#initial_children
	      end;
*)

	  if nd2#is_collapsed && (moderate_nchildren nd2) then
	    if nd2#collapse_not_locked then
	      expnd2 := i2::!expnd2;
(*
	    else
	      if nd2#data#eq nd1#data then begin
		expnd2 := i2::!expnd2;
		nd2#unlock_collapse;
		Array.iter (fun n -> n#lock_collapse) nd2#initial_children
	      end;
*)
          BEGIN_DEBUG
            let uid1, uid2 = nd1#uid, nd2#uid in
	    DEBUG_MSG "[RELABELED]: \"(%a)%s\" --> \"(%a)%s\""
	      ups uid1 nd1#to_qualified_string ups uid2 nd2#to_qualified_string;
          END_DEBUG;

      | Otreediff.Edit.Delete i ->
	  let nd = tree1#get i in
	  if nd#is_collapsed && nd#collapse_not_locked && (moderate_nchildren nd) then
	    expnd1 := i::!expnd1

      | Otreediff.Edit.Insert(j, children_ref) ->
	  let nd = tree2#get j in
	  if nd#is_collapsed && nd#collapse_not_locked && (moderate_nchildren nd) then
	    expnd2 := j::!expnd2

    in (* end of func proc_one *)

    Otreediff.Edit.seq_iter proc_one eds;

    pruned#add_pruned_nodes (classify_isomorphic tree1 tree2 mapping iso);

    BEGIN_DEBUG
      DEBUG_MSG "Mapping:\n";
      Otreediff.Mapping.iter
	(fun i j -> DEBUG_MSG "%a -- %a\n" nups (tree1#get i) nups (tree2#get j))
	mapping
    END_DEBUG;

    !expnd1, !expnd2
  (* end of func analyze0 *)


  let do_compare options cenv count cid tree1 tree2 pruned =

    let sz1, sz2 = tree1#size, tree2#size in

    DEBUG_MSG "*** %d: ANALYZING SUBTREE PAIR [%s] ***\n" count cid;
    DEBUG_MSG "[%s]: sizes of (sub) trees: old=%d new=%d" cid sz1 sz2;


    DEBUG_MSG "T1:\n%s\nT2:\n%s\n" tree1#to_string tree2#to_string;


    let eds, mapping, iso =
      if Misc.check_hard_tree_size_limit options sz1 sz2 then begin

	Xprint.warning "exceeds HARD (sub)tree size LIMIT! |T1|=%d |T2|=%d limit=%d"
	  sz1 sz2 options#hard_tree_size_limit;

	if tree1#is_flat && tree2#is_flat (* && not options#ignore_huge_arrays_flag *) then begin
	  Xprint.warning "trying to do array diff...";
	  Flattreediff.find tree1 tree2
	end
	else
	  Otreediff.Lib.find_trivial tree1 tree2
      end
      else
	if tree1#is_flat && tree2#is_flat then begin
	  DEBUG_MSG "using flattreediff...";
	  Flattreediff.find tree1 tree2
	end
	else begin
	  DEBUG_MSG "using otreediff...";
	  Treediff.sfind tree1 tree2
	end
    in

(*    let iso = shrink_iso tree1 tree2 mapping iso in *)


    (* exclude iso-subtree of size 1 *)
    let iso =
      List.filter
	(fun i ->
	  let b = (tree1#leftmost i) <> i in

	  if not b then
	    DEBUG_MSG "isomorphic subtree of size 1: %a (not treated as iso)" nups (tree1#get i);
	  b
	) iso
    in

    BEGIN_DEBUG
      let to_s1 i = UID.to_string (tree1#get i)#uid in
      let to_s2 j = UID.to_string (tree2#get j)#uid in
      DEBUG_MSG "edit sequence:\n%s\nmapping:\n%s"
	(Otreediff.Edit._seq_to_string to_s1 to_s2 eds)
	(Otreediff.Mapping._to_string to_s1 to_s2 mapping);
    END_DEBUG;

    let iso_pairs =
      List.map (fun i -> i, Otreediff.Mapping.find i mapping) iso
    in

    let para_iso_mems = ref [] in
    let para_iso = ref [] in
    let non_iso = ref [] in

    let iso_members =
      List.fold_left
	(fun l (i, j) ->
	  let mems = tree1#fast_subtree_members i in
	  let nmems1 = List.length mems in

	  if nmems1 < options#prune_threshold then begin
	    non_iso := i::!non_iso;
	    l
	  end
	  else
	    let nmems2 = List.length (tree2#fast_subtree_members j) in
	    let nimems1 = tree1#whole_initial_subtree_size (tree1#get i) in
	    let nimems2 = tree2#whole_initial_subtree_size (tree2#get j) in

	    if nmems1 < nimems1 || nmems2 < nimems2 then begin
	      para_iso_mems := mems @ !para_iso_mems;
	      para_iso := i::!para_iso
	    end
	    else
	      if nmems1 > nimems1 || nmems2 > nimems2 then begin
		FATAL_MSG "inconsistent iso: %d" i;
		exit 1
	      end;

	    l @ mems

	) [] iso_pairs
    in
    let iso_pairs =
      List.filter (fun (i, _) -> not (List.mem i !non_iso)) iso_pairs
    in
    let iso, _ = List.split iso_pairs in

    DEBUG_MSG "iso_members: [%s]"
      (Xlist.to_string string_of_int ";" iso_members);

    let expnd1, expnd2 (* simply analyzed edit sequence *) =
      analyze0 options tree1 tree2 eds mapping iso pruned
    in

    let uidmapping = new UIDmapping.c cenv in

    Otreediff.Mapping.iter
      (fun i j ->
	let nd1, nd2 = tree1#get i, tree2#get j in
	let uid1, uid2 = nd1#uid, nd2#uid in
	if not (nd1#is_collapsed || nd2#is_collapsed) || (nd1#data#equals nd2#data) then
	  if List.mem i iso_members && not (List.mem i !para_iso_mems) then begin
	    ignore (uidmapping#add_settled ~stable:false uid1 uid2);

	    if List.mem i iso then
	      uidmapping#add_settled_roots uid1 uid2
	  end
	  else
	    ignore (uidmapping#add_unsettled uid1 uid2)

      ) mapping;

    eds, mapping, iso_pairs, expnd1, expnd2, uidmapping,
    iso_members, !para_iso_mems, !para_iso
  (* end of func do_compare *)



  let prune_and_expand
      cid
      (tree1 : tree_t)
      (tree2 : tree_t)
      (iso1, iso2, expnd1, expnd2)
      (g_uidmapping : node_t UIDmapping.c)
      =
    BEGIN_DEBUG
      DEBUG_MSG "[%s]: prune: T1[%s] T2:[%s]"
	cid
	(Xlist.to_string UID.to_string "," iso1)
	(Xlist.to_string UID.to_string "," iso2);
      DEBUG_MSG "[%s]: expand: T1[%s] T2:[%s]"
	cid
	(Xlist.to_string UID.to_string "," expnd1)
	(Xlist.to_string UID.to_string "," expnd2)
    END_DEBUG;

    DEBUG_MSG "pruning...";

    let rt1, rt2 = tree1#root, tree2#root in
    let rtu1, rtu2 = rt1#uid, rt2#uid in

    if iso1 = [rtu1] then rt1#unhide_parent;
    if iso2 = [rtu2] then rt2#unhide_parent;

    (* change also 'pruned#add_pruned*' somewhere, if you change this *)
    tree1#prune_uids iso1;
    tree2#prune_uids iso2;

    DEBUG_MSG "prune_and_expand: done.";

    DEBUG_MSG "prune_and_expand: expanding...";

    tree1#expand_uids expnd1;
    tree2#expand_uids expnd2;

    DEBUG_MSG "prune_and_expand: done.";

    tree1#init; tree2#init
  (* end of func prune_and_expand *)


  let matching_cond options tree1 tree2 nd1 nd2 = (* for Analyzing.F.find_matching_subtrees *)
(*
  (nd1#data#is_named && nd2#data#is_named &&
  nd1#data#eq nd2#data) ||

  (nd1#is_collapsed && nd2#is_collapsed &&
  nd1#data#eq nd2#data) ||
 *)
(*
  (tree1#is_flat && tree2#is_flat)
 *)

    let cc1 = Misc.get_collapsed_children nd1 in
    let cc2 = Misc.get_collapsed_children nd2 in

    let _moderate_nchildren =
      Misc._moderate_nchildren ~threshold:options#moderate_nchildren_threshold
    in


    if (_moderate_nchildren cc1 nd1) || (_moderate_nchildren cc2 nd2) then
      tree1#is_flat && tree2#is_flat
    else begin

      DEBUG_MSG "have huge amount of children:\n%d <-- %s\nand\n%d <-- %s"
	(List.length cc1) nd1#to_string
	(List.length cc2) nd2#to_string;

      (Misc._to_be_flat cc1 nd1) && (Misc._to_be_flat cc2 nd2)
    end

  let map_cond tree1 tree2 =
    try
      let rt1, rt2 = tree1#root, tree2#root in
      rt1#data#is_named && rt2#data#is_named &&
      rt1#data#eq rt2#data
    with Otreediff.Otree.Empty -> false


  let find_matching_subtrees options (tree1 : tree_t) (tree2 : tree_t) eds =

    let addtree subs nd1 nd2 =
      DEBUG_MSG "addtree: %a %a" nups nd1 nups nd2;
      if nd1#data#eq nd2#data then
	if
	  nd1#is_collapsed && nd2#is_collapsed &&
	  not (nd1#data#equals nd2#data)
	then
	  let tree1' = tree1#make_subtree_from_node nd1 in
	  let tree2' = tree2#make_subtree_from_node nd2 in

	  nd1#hide_parent;
	  nd2#hide_parent;

	  tree1'#expand_root; (* tree1'#init; *)
	  tree2'#expand_root; (* tree2'#init; *)

	  (tree1', tree2')::subs
	else subs
      else subs
    in (* end of func addtree *)

    let rels =
      List.filter
	(function Otreediff.Edit.Relabel _ -> true | _ -> false)
	eds
    in

    BEGIN_DEBUG
      let to_s1 i = UID.to_string (tree1#get i)#uid in
      let to_s2 j = UID.to_string (tree2#get j)#uid in
      DEBUG_MSG "rels: [\n%s\n]"
	(Xlist.to_string (Otreediff.Edit._to_string to_s1 to_s2) "\n" rels)
    END_DEBUG;

    match rels with
(*
  [Otreediff.Edit.Relabel(i1, i2)] ->
  let nd1, nd2 = tree1#get i1, tree2#get i2 in
  addtree [] nd1 nd2
 *)
    | _ ->
	let subs =
	  List.fold_left
	    (fun subs ed ->
	      match ed with
		Otreediff.Edit.Relabel(i1, i2) ->
		  let nd1, nd2 = tree1#get i1, tree2#get i2 in
		  if matching_cond options tree1 tree2 nd1 nd2 then
		    addtree subs nd1 nd2
		  else
		    subs
	      | _ -> subs
	    ) [] rels
	in
	BEGIN_DEBUG
          let len = (List.length subs) in
	  DEBUG_MSG "%d subtree(s)\n" len;
	  List.iter
	    (fun (t1, t2) ->
	      DEBUG_MSG "T1:\n%s\nT2:\n%s\n"
		t1#to_string t2#to_string
	    ) subs
	END_DEBUG;

	subs
  (* end of func find_matching_subtrees *)


  (* compare subtrees *)
  let rec compare_subtree
      options
      counter
      cenv
      dirname
      (g_uidmapping : node_t UIDmapping.c)
      pruned
      ((tree1 : tree_t), (tree2 : tree_t))
      =
    let ouid = UID.to_string tree1#root#uid in
    let nuid = UID.to_string tree2#root#uid in
    let cid = sprintf "%s-%s" ouid nuid in

    (* repeats until no nodes are expanded *)
    let rec loop() =

      counter#incr;
      let c = counter#value in

      if options#dots_flag then begin
	tree1#save_dot "Old" [] (Filename.concat dirname (sprintf "%d.old%s.dot" c ouid));
	tree2#save_dot "New" [] (Filename.concat dirname (sprintf "%d.new%s.dot" c nuid))
      end;

      let eds, mapping, iso_pairs, expnd1, expnd2, uidmapping,
	iso_mems, para_iso_mems, para_iso
	  =
	do_compare options cenv c cid tree1 tree2 pruned
      in

      let iso, iso2 = List.split iso_pairs in

      let get_nds t = List.map t#get in
      let para_iso2 =
	List.map (fun i -> Otreediff.Mapping.find i mapping) para_iso
      in
      pruned#add_para_iso1 (get_nds tree1 para_iso);
      pruned#add_para_iso2 (get_nds tree2 para_iso2);


      if options#dots_flag then
	Otreediff.Lib.to_dot (Filename.concat dirname (sprintf "%d.%s.dot" c cid))
	  tree1 tree2 eds mapping iso_pairs;


      let to_be_pruned1 = iso_mems in

      List.iter (* add nodes to be pruned to global uid mapping *)
	(fun i ->
	  let is_para_iso = List.mem i para_iso_mems in
	  let j = Otreediff.Mapping.find i mapping in
	  let nd1, nd2 = tree1#get i, tree2#get j in
	  let uid1, uid2 = nd1#uid, nd2#uid in

	  DEBUG_MSG "to_be_pruned: %a-%a" ups uid1 ups uid2;

	  if nd1#is_collapsed && nd2#is_collapsed then begin
	    let l1, l2 = ref [], ref [] in

	    tree1#fast_scan_whole_initial_subtree nd1 (fun nd -> l1 := nd::!l1);
	    tree2#fast_scan_whole_initial_subtree nd2 (fun nd -> l2 := nd::!l2);
	    let sz1, sz2 = List.length !l1, List.length !l2 in
	    if sz1 <> sz2 then begin
	      FATAL_MSG	"pruned subtree size mismatch: %a vs %a: %d != %d"
		ups uid1 ups uid2 sz1 sz2;
	      exit 1
	    end;
	    List.iter2
	      (fun nd1 nd2 ->
		ignore (g_uidmapping#add_settled ~stable:false nd1#uid nd2#uid)
	      ) !l1 !l2
	  end
	  else
            (* both are not collapsed *)
	    if not (nd1#is_collapsed || nd2#is_collapsed) then
	      if is_para_iso then
		ignore (g_uidmapping#add_unsettled uid1 uid2)
	      else
		ignore (g_uidmapping#add_settled ~stable:false uid1 uid2)

	    else begin
	      DEBUG_MSG "nd1#is_collapsed=%B nd1#data#digest=%s"
		nd1#is_collapsed nd1#data#digest_string;
	      DEBUG_MSG "nd2#is_collapsed=%B nd2#data#digest=%s"
		nd2#is_collapsed nd2#data#digest_string;
	      assert false
	    end;

	  if not is_para_iso && List.mem i iso then
	    g_uidmapping#add_settled_roots uid1 uid2

	) to_be_pruned1;


      BEGIN_DEBUG
	DEBUG_MSG "* %d: iso=[%s]" c
	  (Xlist.to_string
	     (fun i -> UID.to_string (tree1#get i)#uid) "," iso);
	DEBUG_MSG "* %d: to_be_pruned1=[%s]" c
	  (Xlist.to_string
	     (fun i -> UID.to_string (tree1#get i)#uid) "," to_be_pruned1)
      END_DEBUG;


      let clusters = (* by only using nodes not to be pruned *)
	Otreediff.Clustering.exact_cluster tree1 tree2
	  (List.filter
	     (fun (i, j) -> not (List.mem i to_be_pruned1)) mapping)
	  eds
      in

      DEBUG_MSG "* %d: contracting..." c;

      let deferred_cluster, pruned_clusters =
	Misc.contract tree1 tree2 clusters
      in

      DEBUG_MSG "* %d: done." c;

      List.iter (* adding nodes in pruned clusters to global uid mapping *)
	(fun clu ->
	  List.iter
	    (fun (u1, u2) ->
	      ignore (g_uidmapping#add_unsettled u1 u2)
	    ) clu
	) pruned_clusters;

      let subs = find_matching_subtrees options tree1 tree2 eds in

      List.iter (fun (t1, t2) -> t1#init; t2#init) subs;

      let subuidmapping_list, deferred_clusters =
	if counter#value = 1 then begin
(*
	    BEGIN_INFO
	      Xprint.verbose options#verbose_flag "found %d subtree pair(s)" (List.length subs);
	      printf "comparing.%!";
	    END_INFO;
*)
	  let a =
	    List.map
	      (fun sub ->
		let b =
		  compare_subtree options counter cenv dirname g_uidmapping pruned sub
		in
(*
		BEGIN_INFO
		  printf ".%!";
		END_INFO;
*)
		b
	      ) subs
	  in
(*
	  if not (options#viewer_flag) then
	    BEGIN_INFO
	      printf "done.\n%!";
	    END_INFO;
*)
	  List.split a
	end
	else
	  List.split
	    (List.map
	       (compare_subtree options counter cenv dirname g_uidmapping pruned)
	       subs)
      in

      let subs_data = List.combine subs subuidmapping_list in

      let get_uids t = List.map (fun i -> (t#get i)#uid) in
      let iso1_uid = get_uids tree1 iso in
      let iso2_uid = get_uids tree2 iso2 in
      let expnd1_uid = get_uids tree1 expnd1 in
      let expnd2_uid = get_uids tree2 expnd2 in

      DEBUG_MSG "* %d: calling prune_and_expand" c;

      prune_and_expand cid tree1 tree2
	(iso1_uid, iso2_uid, expnd1_uid, expnd2_uid) g_uidmapping;


      (* contract deferred clusters *)

      List.iter
	(fun (t1, t2) ->
	  begin
	    try
	      t1#root#unhide_parent;
	    with Otreediff.Otree.Empty -> ()
	  end;
	  begin
	    try
	      t2#root#unhide_parent
	    with Otreediff.Otree.Empty -> ()
	  end
	) subs;

      tree1#init; tree2#init;

      DEBUG_MSG "* %d: contracting deferred clusters..." c;

      if options#dots_flag then begin
	tree1#save_dot "Old" [] (Filename.concat dirname (sprintf "%d.old+.dot" c));
	tree2#save_dot "New" [] (Filename.concat dirname (sprintf "%d.new+.dot" c))
      end;

      List.iter
	(function
	    Some clu ->

	      DEBUG_MSG "cluster: %s"
		(Xlist.to_string
		   (fun (u1, u2) ->
		     sprintf "%a-%a" ups u1 ups u2) "," clu);

	      if (List.length clu) <= 1 then ()
	      else
		let deferred, pruned =
		  Misc.contract tree1 tree2
		    [List.map
		       (fun (u1, u2) ->
			 (tree1#search_node_by_uid u1)#index,
			 (tree2#search_node_by_uid u2)#index
		       ) clu]
		in
		if deferred = None && (List.length pruned = 1) then
		  List.iter
		    (fun clu ->
		      List.iter
			(fun (u1, u2) ->
			  ignore (g_uidmapping#add_unsettled u1 u2)
			) clu
		    ) pruned
		else begin
		  FATAL_MSG "deferred clusters contraction";
		  exit 1
		end

	  | None -> ()
	) deferred_clusters;


      DEBUG_MSG "* done.";

      tree1#init; tree2#init;


      (*
       * if the sizes of two trees exceed the limit, subtrees are pruned,
       * and their edit sequences are incorporated into the global report
       *)

      let sz1, sz2 = tree1#size, tree2#size in

      let r2s tree =
	try
	  UID.to_string tree#root#uid
	with
          Otreediff.Otree.Empty -> "-"
      in
      let _ = r2s in
      DEBUG_MSG "* %d: |T1(root:%s)|=%d |T2(root:%s)|=%d" c
	(r2s tree1) sz1 (r2s tree2) sz2;

      match sz1, sz2 with
	0, 0 -> uidmapping, None
      | 0, n | n, 0 -> uidmapping, deferred_cluster
      | _ -> begin
	  Misc.set_tree_size_limit options sz1 sz2;

	  DEBUG_MSG "* %d: tree size limit: %d %d" c
	    options#tree_size_limit1 options#tree_size_limit2;

	  DEBUG_MSG "* %d: checking tree size limit (1)" c;

	  let too_large = Misc.check_tree_size_limit options sz1 sz2 in

	  if too_large then
	    DEBUG_MSG "* %d: tree size TOO LARGE!" c;

	  (* may be in path of some nodes *)
	  List.iter
	    (fun ((t1, t2), subuidmapping) ->

	      DEBUG_MSG "* %d: checking subtree pair %s - %s"
		c (r2s t1) (r2s t2);

	      if too_large || (map_cond t1 t2) then begin
		begin
		  try
		    let nd1 = t1#root in
		    if nd1#pos < 0 then
		      DEBUG_MSG "already (to be) pruned: %a" nups nd1
		    else begin
		      nd1#prune;
		      if nd1#in_path then begin

			DEBUG_MSG "aborted1: %a -> [%a]" nups nd1 nsps nd1#get_substances;

			pruned#add_abortedl1 nd1#get_substances
		      end
		      else begin
			DEBUG_MSG "aborted1: %a" nups nd1;
			pruned#add_aborted1 nd1
		      end
		    end
		  with Otreediff.Otree.Empty -> ()
		end;

		begin
		  try
		    let nd2 = t2#root in
		    if nd2#pos < 0 then
		      DEBUG_MSG "already (to be) pruned: %a" nups nd2
		    else begin
		      nd2#prune;
		      if nd2#in_path then begin

			DEBUG_MSG "aborted2: %a -> [%a]" nups nd2 nsps nd2#get_substances;

			pruned#add_abortedl2 nd2#get_substances
		      end
		      else begin
			DEBUG_MSG "aborted2: %a" nups nd2;
			pruned#add_aborted2 nd2
		      end
		    end
		  with Otreediff.Otree.Empty -> ()
		end;


		BEGIN_DEBUG
		  DEBUG_MSG "g_uidmapping:\n%s" g_uidmapping#to_string;
		  DEBUG_MSG "subuidmapping:\n%s" subuidmapping#to_string;
		  DEBUG_MSG "g_uidmapping#merge_no_override subuidmapping";
		END_DEBUG;

		g_uidmapping#merge_no_override subuidmapping

	      end;

	    ) subs_data;
	  tree1#init; tree2#init;


	  let sz1, sz2 = tree1#size, tree2#size in

	  DEBUG_MSG "* %d: |T1(root:%a)|=%d |T2(root:%a)|=%d" c nups tree1#root sz1 nups tree2#root sz2;

	  DEBUG_MSG "* %d: tree size limit: %d %d" c
	    options#tree_size_limit1 options#tree_size_limit2;

	  DEBUG_MSG "* %d: checking tree size limit (2)" c;

	  if Misc.check_tree_size_limit options sz1 sz2 then begin

	    DEBUG_MSG "* %d: STILL tree size TOO LARGE!" c;

	    uidmapping, deferred_cluster
	  end
	  else
	    if Misc.check_hard_tree_size_limit options sz1 sz2 then begin

	      DEBUG_MSG "* %d: exceeds HARD tree size LIMIT!" c;

	      uidmapping, deferred_cluster
	    end
	    else
	      if expnd1 = [] && expnd2 = [] then
		uidmapping, deferred_cluster
	      else
		try
                  let um, dc = loop() in
                  let _ = um#check_for_ref cenv#ref_upairs uidmapping in
                  um, dc
		with Otreediff.Lib.Distance_too_far ->
		  DEBUG_MSG "* %d: distance too far, aborting" c;
		  uidmapping, deferred_cluster
      end
    in (* end of func loop *)

    let result = loop() in

    result
  (* end of func compare_subtree *)


  let dump_sources file src1 src2 =
    let dumper ch =
      Printf.fprintf ch "%s\n%s\n" src1 src2
    in
    Xfile.dump file dumper


  let get_diff_status
      options
      lang
      cenv
      (orig_edits : Edit.seq)
      (edits : Edit.seq)
      (uidmapping : node_t UIDmapping.c)
      (tree1 : tree_t)
      (tree2 : tree_t)
      =
    let cache_path = cenv#cache_path in
    let diff      = Filename.concat cache_path Stat.diff_file_name in
    let diff_json = Filename.concat cache_path Stat.diff_file_name^".json" in
    let gdiff_json = Filename.concat cache_path "g"^Stat.diff_file_name^".json" in
    let dinfo     = Filename.concat cache_path Stat.info_file_name in
    let dsummary  = Filename.concat cache_path Stat.summary_file_name in
    let dstat     = Filename.concat cache_path Stat.stat_file_name in
    let dstat_json = Filename.concat cache_path Stat.stat_file_name^".json" in
    let dmap      = Filename.concat cache_path Stat.map_file_name in
    let dgmap     = Filename.concat cache_path "g"^Stat.map_file_name in
    let dmapfact  = Filename.concat cache_path Stat.map_file_name^".nt" in
    let dsrc      = Filename.concat cache_path Stat.sources_file_name in
    let dparser   = Filename.concat cache_path Stat.parser_file_name in
    (*let dchange   = Filename.concat cache_path Stat.changes_file_name in*)
    let ddot1     = Filename.concat cache_path Stat.dot_file_name1 in
    let ddot2     = Filename.concat cache_path Stat.dot_file_name2 in
    (*let delta     = Filename.concat cache_path Delta_base.delta_file_name^".xml" in*)

    let is_modified = not edits#is_empty in

    if is_modified then begin

      let getlab n = (Obj.obj n#data#_label : Label.t) in
      let is_anon n =
        try
          let _ = Label.get_value (getlab n) in
          false
        with
          Not_found -> not n#data#is_named_orig
      in
      edits#finalize uidmapping is_anon;

      begin
      BEGIN_DEBUG
	DEBUG_MSG "checking SPSM...";
	let u2g1 u = (tree1#search_node_by_uid u)#gindex in
	let u2g2 u = (tree2#search_node_by_uid u)#gindex in
	let spsm = ref [] in
	let moved_nodes = edits#get_moved_nodes tree1 in

        DEBUG_MSG "|moved_nodes|=%d" (Xset.length moved_nodes);

	let moved_uids = Xset.map (fun n -> n#uid) moved_nodes in

        let map_count = ref 0 in
        let mov_count = ref 0 in
        let rel_count = ref 0 in
        let movrel_count = ref 0 in

	uidmapping#iter
	  (fun u1 u2 ->
            incr map_count;

            let is_mov = Xset.mem moved_uids u1 in
            let is_rel = edits#mem_rel12 u1 u2 in

            if is_mov then
              incr mov_count;

            if is_rel then
              incr rel_count;

            if is_mov && is_rel then
              incr movrel_count;

	    if not is_mov && not is_rel then
	      spsm := (u2g1 u1, u2g2 u2)::!spsm
	  );
	DEBUG_MSG "SPSM: size=%d" (List.length !spsm);
	List.iter
	  (fun (g1, g2) ->
	    DEBUG_MSG "SPSM: %a-%a" gps g1 gps g2
	  ) (List.fast_sort Stdlib.compare !spsm);
        DEBUG_MSG "map_count: %d" !map_count;
        DEBUG_MSG "mov_count: %d" !mov_count;
        DEBUG_MSG "rel_count: %d" !rel_count;
        DEBUG_MSG "movrel_count: %d" !movrel_count;
	DEBUG_MSG "done."
      END_DEBUG;
      end;

      if options#dump_dot_flag then begin
	edits#dump_dot1 ~final:true ddot1 tree1 tree2 uidmapping;
	edits#dump_dot2 ~final:true ddot2 tree2 tree1 uidmapping;
      end;


      let edits_copy = edits#copy in
      edits_copy#cleanup_ghost tree1 tree2;

      let line_align = edits_copy#get_line_align tree1 tree2 uidmapping in
      edits_copy#dump_diff_simple ~line_align tree1 tree2 diff;
      edits_copy#dump_diff_json ~line_align tree1 tree2 diff_json;
      edits_copy#dump_gdiff_json ~comp:Compression.gzip tree1 tree2 (gdiff_json^".gz");

      edits#dump_diff_info dinfo tree1 tree2;
      edits#dump_diff_summary dsummary tree1 tree2 uidmapping;

      let diff_stat = edits#get_diff_stat tree1 tree2 uidmapping in
(*
  edits#dump_diff_stat dstat tree1 tree2 uidmapping;
 *)
      Stat.File.dump_diff_stat dstat diff_stat;
      Stat.File.dump_diff_stat_json dstat_json diff_stat;

      uidmapping#dump_with_info ~comp:Compression.gzip (dmap^".gz");
      uidmapping#dump_json ~comp:Compression.gzip (dmap^".json.gz");

      let moved_nodes = edits#get_moved_nodes tree1 in
      let is_mov u1 u2 =
        try
          let n1 = tree1#search_node_by_uid u1 in
          if Xset.mem moved_nodes n1 then
            let n2 = tree2#search_node_by_uid u2 in
            tree2#search_node_by_uid (uidmapping#find u1) = n2
          else
            false
        with _ -> false
      in
      uidmapping#dump_gid_json ~comp:Compression.gzip is_mov (dgmap^".json.gz");

      if options#fact_for_mapping_flag then
	Lang.extract_mapping_fact options lang uidmapping dmapfact tree1 tree2;

      Stat.dump_parser_name dparser tree1#parser_name;
      dump_sources dsrc tree1#source_path tree2#source_path;

      DEBUG_MSG "\nEdits:\n%s\n" (edits#to_string);

      if options#verbose_flag then
	edits#show_diff_stat ~short:true tree1 tree2 uidmapping;

      (*let edits_copy = edits#copy in
      edits_copy#ungroup tree1 tree2;
      edits_copy#cleanup_ghost tree1 tree2;

      Edit.dump_changes options lang tree1 tree2 uidmapping edits_copy edits dchange;

      if options#dump_delta_flag then begin
        edits#dump_delta tree1 tree2 uidmapping edits_copy delta
      end;*)

      if options#dump_ccs_flag then begin (* dump common code structure *)

	if options#check_flag then
	  Xprint.warning "result check and ccs dump are mutually exclusive";

(*	  let deleted1, deleted2 = edits#remove_unmapped tree1 tree2 in *)
	ignore (edits#remove_unmapped tree1 tree2);
	let mold = Filename.concat cache_path "mapped_old"^Astml.ccs_ext in
	let mnew = Filename.concat cache_path "mapped_new"^Astml.ccs_ext in
	let mold_nodes = Filename.concat cache_path "mapped_old.gids" in
	let mnew_nodes = Filename.concat cache_path "mapped_new.gids" in
	let get_gids tree =
	  let l = ref [] in
	  tree#scan_all
	    (fun nd ->
	      let gid =
		let g = nd#data#gid in
		if g > 0 then g else nd#gindex
	      in
	      l := gid::!l
	    );
	  List.rev !l
	in
(*
  Xfile.dump mold tree1#dump_xml_ch;
  Xfile.dump mnew tree2#dump_xml_ch;
 *)
	tree1#dump_astml ~comp:options#ast_compression mold;
	tree2#dump_astml ~comp:options#ast_compression mnew;

	Xfile.dump mold_nodes (fun ch -> output_string ch (GI.list_to_string (get_gids tree1)));
	Xfile.dump mnew_nodes (fun ch -> output_string ch (GI.list_to_string (get_gids tree2)));

(*
            let dtrees1 = List.map (fun n -> tree1#make_subtree_from_node n) deleted1 in
            let dtrees2 = List.map (fun n -> tree2#make_subtree_from_node n) deleted2 in
            let gen_fn1, gen_fn2 =
              let c1, c2 = ref 0, ref 0 in
              let gen1() =
                let n = Filename.concat cache_path (sprintf "deleted%d%s" !c1 Astml.ccs_ext) in
                incr c1;
                n
              in
              let gen2() =
                let n = Filename.concat cache_path (sprintf "inserted%d%s" !c2 Astml.ccs_ext) in
                incr c2;
                n
              in
              gen1, gen2
            in
            List.iter (fun t -> Xfile.dump (gen_fn1()) t#dump_xml_ch) dtrees1;
            List.iter (fun t -> Xfile.dump (gen_fn2()) t#dump_xml_ch) dtrees2;
*)
      end
      else begin (* dump_ccs_flag = false *)

	if options#check_flag then
	  if orig_edits#check tree1 tree2 uidmapping then begin
	    Xprint.message "result check: PASSED!"
	  end
	  else begin
	    Xprint.message "result check: FAILED!";
	    let f = open_out (Filename.concat cache_path "INCORRECT") in
	    close_out f
	  end

      end;

      diff_stat

    end (* if is_modified *)
    else begin
      let diff_stat = edits#get_diff_stat tree1 tree2 uidmapping in

      let is_mov u1 u2 = false in
      uidmapping#dump_gid_json ~comp:Compression.gzip is_mov (dgmap^".json.gz");

      if options#viewer_flag then
	printf "%c%!" Const.viewer_mode_status_SAME

      else begin
        Stat.File.dump_diff_stat dstat diff_stat;
        Stat.File.dump_diff_stat_json dstat_json diff_stat;
(*
  edits#dump_diff_stat dstat tree1 tree2 uidmapping;
 *)
	if options#fact_for_mapping_flag then begin
	  if tree1#source_digest <> tree2#source_digest then
	    Lang.extract_mapping_fact options lang uidmapping dmapfact tree1 tree2
	end;
        (*Xprint.warning "no differences found: %s -- %s" tree1#source_path tree2#source_path*)
(*
  raise No_differences_found
 *)
      end;

      diff_stat

    end

  let find_boundary = Sourcecode.find_nearest_p_ancestor_node (fun x -> x#data#is_boundary)

  let has_mapped_boundary uidmapping n1 n2 =
    let b =
      try
        let bn1 = find_boundary n1 in
        let bn2 = find_boundary n2 in
        try
          uidmapping#find bn1#uid = bn2#uid
        with _ -> false
      with _ -> false
    in
    DEBUG_MSG "%a %a: %B" nups n1 nups n2 b;
    b

  (* top level comarison *)
  let compare_tree
      options
      lang
      (cenv : (node_t, tree_t) Comparison.c)
      pre_uidmapping
      may_be_unsettled1
      may_be_unsettled2
      ref_uidmapping
      tree1 tree2
      =
    let cache_path = cenv#cache_path in

    Cache.prepare_cache_dir options cache_path;

    begin
(*
    try
*)

      BEGIN_DEBUG
	DEBUG_MSG "Old:\n%s" tree1#to_string;
	DEBUG_MSG "New:\n%s" tree2#to_string;
        DEBUG_MSG "size of tree: old:%d new:%d" tree1#size tree2#size;
      END_DEBUG;

      if options#viewer_flag then
	printf "%c%!" Const.viewer_mode_status_OUTLINE_COMP
      else
	DEBUG_MSG "comparing outlines...";

      let g_uidmapping = new UIDmapping.c cenv in

      g_uidmapping#set_blacklist1 may_be_unsettled1;
      g_uidmapping#set_blacklist2 may_be_unsettled2;

      let pruned = new Pruned.nodes in

      let counter = new Misc.counter in (* count calls of do_compare *)

      let uidmapping, _ =
	compare_subtree options counter cenv cache_path g_uidmapping pruned (tree1, tree2)
      in


      BEGIN_DEBUG
	DEBUG_MSG "uidmapping:\n%s" uidmapping#to_string;
	DEBUG_MSG "g_uidmapping:\n%s" g_uidmapping#to_string;
        DEBUG_MSG "merging mappings...";
      END_DEBUG;

      ignore (uidmapping#merge g_uidmapping);

      DEBUG_MSG "mapping merged.";


      if options#check_flag then
	pruned#iter
	  (fun (kind, uid1, _, uid2, _, _, _) ->
	    DEBUG_MSG "checking nodes to be pruned: %s %a %a"
	      (Pruned.kind_to_string !kind) ups uid1 ups uid2;
	    try
	      let uid1' = uidmapping#find uid1 in
	      if uid1' <> uid2 then begin
		FATAL_MSG
		  "nodes to be pruned (%a-%a) does not contained in the mapping: %a is mapped to %a"
		  ups uid1 ups uid2 ups uid1 ups uid1';
		exit 1
	      end
	    with
	      Not_found ->
		FATAL_MSG "pruned %a: not found" ups uid1;
		exit 1
	  );


      (*** postprocessing ***)

      if options#viewer_flag then
	printf "%c%!" Const.viewer_mode_status_POSTPROCESS
      else
	Xprint.verbose options#verbose_flag "postprocessing...";

      BEGIN_DEBUG
	DEBUG_MSG "aborted1: [%a]" nsps pruned#aborted1;
	DEBUG_MSG "para_iso1: [%a]" nsps pruned#para_iso1;
	DEBUG_MSG "aborted2: [%a]" nsps pruned#aborted2;
	DEBUG_MSG "para_iso2: [%a]" nsps pruned#para_iso2;
	DEBUG_MSG "T1:\n%s" tree1#to_string;
	DEBUG_MSG "T2:\n%s" tree2#to_string;
	DEBUG_MSG "uidmapping BEFORE POSTPROCESSING: %s" uidmapping#to_string;
        DEBUG_MSG "ref_uidmapping BEFORE POSTPROCESSING:\n%s" ref_uidmapping#to_string;
        (*DEBUG_MSG "ref_uidmapping BEFORE POSTPROCESSING:\n%s" ref_uidmapping#to_string_gid;*)
      END_DEBUG;

      if options#preprune_flag then begin
	DEBUG_MSG "MERGING PRE-UIDMAPPING";
	DEBUG_MSG "|uidmapping|=%d |pre_uidmapping|=%d" uidmapping#size pre_uidmapping#size;
	uidmapping#merge_checked pre_uidmapping;
	DEBUG_MSG "PRE-UIDMAPPING MERGED.\n"
      end;


      if options#prematch_flag (* || options#prematch_named_flag *) then begin
	DEBUG_MSG "MERGING REF-UIDMAPPING...";
	let count = ref 0 in
	ref_uidmapping#iter
	  (fun uid1 uid2 ->
	    incr count;
	    DEBUG_MSG "merging %a-%a" ups uid1 ups uid2;
            ignore (uidmapping#add_unsettled uid1 uid2)
	  );
	uidmapping#set_stable_pairs ref_uidmapping#stable_pairs;
	DEBUG_MSG "%d pairs from REF-UIDMAPPING MERGED.\n" !count
      end;

      DEBUG_MSG "|uidmapping|=%d\n%s" uidmapping#size uidmapping#to_string;

      let _ = cenv#elaborate_uidmapping uidmapping in

      DEBUG_MSG "|uidmapping|=%d" uidmapping#size;

      Postprocessing.postprocess options cenv tree1 tree2 uidmapping pruned ref_uidmapping;

      Xprint.verbose options#verbose_flag "postprocessing completed.";

      let initial_starting_uid_pairs_for_glueing =
        let find_anc_stmt =
          let moveon x = not x#data#is_sequence in
          Misc.get_p_ancestor ~moveon (fun x -> x#data#is_statement)
        in
        let count_occurences stmt n =
          let pred x = x#data#eq n#data in
          let moveon x = not x#data#is_sequence in
          let l = Misc.get_p_descendants ~moveon pred stmt in
          let c = List.length l in
          DEBUG_MSG "%a %a -> %d" nups stmt nups n c;
          c
        in
        let is_mapped n1 n2 =
          try
            uidmapping#find n1#uid = n2#uid
          with
            Not_found -> false
        in
        let has_mapped_stmt n1 n2 =
          let b =
            try
              let s1 = find_anc_stmt n1 in
              let s2 = find_anc_stmt n2 in
              DEBUG_MSG "stmt: %a %a -> %a %a" nups n1 nups n2 nups s1 nups s2;

              List.for_all
                (fun (n, s) ->
                  Array.exists (fun n -> n#data#is_sequence) s#initial_children
                ) [n1, s1; n2, s2] &&
              if
                s1#data#relabel_allowed s2#data
              then begin
                is_mapped s1 s2 ||

                (uidmapping#mem_dom s1#uid &&
                 let s1' = tree2#search_node_by_uid (uidmapping#find s1#uid) in
                 not n1#data#is_named_orig || count_occurences s1 n1 < 2 && count_occurences s1' n1 = 0) ||

                (uidmapping#mem_cod s2#uid &&
                 let s2' = tree1#search_node_by_uid (uidmapping#inv_find s2#uid) in
                 not n2#data#is_named_orig || count_occurences s2 n2 < 2 && count_occurences s2' n2 = 0)

              end
              else
                is_mapped s1 s2 && count_occurences s1 n1 < 2 && count_occurences s2 n2 < 2
            with
              _ -> false
          in
          DEBUG_MSG "%a %a -> %B" nups n1 nups n2 b;
          b
        in
        let pre_mapped_uids = Xset.create 0 in
        let _ =
          pre_uidmapping#iter_settled_roots
            (fun u1 u2 ->
              try
                let n1 = tree1#search_node_by_uid u1 in
                (*let n2 = tree2#search_node_by_uid u2 in*)
                if
                  (*tree1#whole_initial_subtree_size n1 > 2 &&*)
                  n1#initial_nchildren > 0 &&
                  n1#data#is_named
                then begin
                  Xset.add pre_mapped_uids u1;
                  Xset.add pre_mapped_uids u2
                end
              with _ -> ()
            )
        in
        let is_matched_subtree x =
          let b =
            match x#data#_digest with
            | None -> false
            | Some d ->
                try
                  match cenv#multiple_subtree_matches#find d with
                  | [], _, _ | _, [], _ -> false
                  | [_], [_], _ -> false
                  | _::_, _::_, _ -> true
                with
                  Not_found -> false
          in
          (*DEBUG_MSG "%a -> %B" nups x b;*)
          b
        in
        let has_matched_subtree ?(moveon=fun _ -> true) n =
          let b =
            let pred x =
              Xset.mem pre_mapped_uids x#uid ||
              is_matched_subtree x
            in
            Misc.has_p_descendant ~moveon pred n
          in
          DEBUG_MSG "%a -> %B" nups n b;
          b
        in
        let contained_in_stmt_having_matched_subtree n =
          let moveon x = not x#data#is_sequence in
          let b =
            try
              let s = find_anc_stmt n in
              is_matched_subtree s ||
              has_matched_subtree ~moveon s
            with
              Not_found -> false
          in
          DEBUG_MSG "%a -> %B" nups n b;
          b
        in
        let has_other_names n1 n2 =
          try
            let name = n1#data#get_orig_name in
            let moveon x = not x#data#is_sequence in
            let is_named_orig n x =
              x#data#is_named_orig && x#data#get_orig_name <> name &&
              try n#initial_parent != x && x#initial_parent != n with _ -> true
            in
            let s1 = find_anc_stmt n1 in
            let s2 = find_anc_stmt n2 in
            let dl1 = Misc.get_p_descendants ~moveon (is_named_orig n1) s1 in
            let dl2 = Misc.get_p_descendants ~moveon (is_named_orig n2) s2 in
            let nl1 = List.map (fun x -> x#data#get_orig_name) dl1 in
            let nl2 = List.map (fun x -> x#data#get_orig_name) dl2 in
            let nl = Xlist.intersection nl1 nl2 in
            DEBUG_MSG "other names found: [%s]" (String.concat "; " nl);
            let b = nl <> [] in
            DEBUG_MSG "%a %a -> %B" nups n1 nups n2 b;
            b
          with
            _ -> true
        in
        let contained_in_stmt_having_boundary n =
          let b =
            try
              let s = find_anc_stmt n in
              Misc.has_p_descendant (fun x -> x#data#is_boundary) s
            with
              Not_found -> false
          in
          if b then
            DEBUG_MSG "%a -> %B" nups n b;
          b
        in
        DEBUG_MSG "cenv#ref_upairs compatible with uidmapping:";
        let upairs = ref [] in
        cenv#ref_upairs#iter
          (fun uid1 uid2 ->
            try
              let nd1 = tree1#search_node_by_uid uid1 in
              let nd2 = tree2#search_node_by_uid uid2 in
              (*DEBUG_MSG "checking %a-%a" ups uid1 ups uid2;*)
              if
                nd1#data#eq nd2#data &&
                not (is_mapped nd1 nd2) &&
                (not nd1#data#is_common) &&
                (not nd1#data#is_op) &&
                (not nd1#data#is_statement) &&
                (not nd1#data#is_sequence) &&
                (not nd1#data#has_value || nd1#data#has_non_trivial_value) &&
                (not nd1#data#is_named || nd1#data#is_named_orig) &&
                not (is_matched_subtree nd1) && not (is_matched_subtree nd2) &&
                has_mapped_boundary uidmapping nd1 nd2 &&
                has_mapped_stmt nd1 nd2 &&
                not (has_matched_subtree nd1) && not (has_matched_subtree nd2) &&
                not (contained_in_stmt_having_matched_subtree nd1) &&
                not (contained_in_stmt_having_matched_subtree nd2) &&
                has_other_names nd1 nd2 &&
                not (contained_in_stmt_having_boundary nd1) &&
                not (contained_in_stmt_having_boundary nd2)
              then begin
                DEBUG_MSG "!!!!! %a-%a: %a [%a]-[%a]" ups uid1 ups uid2 labps nd1 locps nd1 locps nd2;
                upairs := (uid1, uid2) :: !upairs
              end
            with _ -> ()
          );
        !upairs
      in
      if initial_starting_uid_pairs_for_glueing <> [] then
        uidmapping#add_starting_uid_pairs_for_glueing initial_starting_uid_pairs_for_glueing;

if not options#no_glue_flag then begin
      if lang#has_elaborate_edits then begin
	let _, added_pairs = cenv#elaborate_uidmapping uidmapping in
	uidmapping#add_starting_uid_pairs_for_glueing added_pairs;

	let _, added_pairs, conflicted_pairs =
	  Postprocessing.glue_deletes_and_inserts options cenv tree1 tree2
            ~no_moves:options#no_moves_flag uidmapping (new UIDmapping.c cenv)
	in
	uidmapping#add_starting_uid_pairs_for_glueing added_pairs
      end
      else begin
	let _, added_pairs = cenv#elaborate_uidmapping uidmapping in
	uidmapping#add_starting_uid_pairs_for_glueing added_pairs;

	let _ =
	  Postprocessing.glue_deletes_and_inserts options cenv tree1 tree2
	    ~override:true ~no_moves:options#no_moves_flag uidmapping (new UIDmapping.c cenv)
	in
	uidmapping#clear_starting_uid_pairs_for_glueing;
      end;

      ignore (cenv#elaborate_uidmapping uidmapping);
end;

      DEBUG_MSG "uidmapping BEFORE EDIT SEQ GENERATION: %s" uidmapping#to_string;

      if options#recover_orig_ast_flag then begin
        tree1#recover_true_children ~initial_only:true ();
        tree2#recover_true_children ~initial_only:true ()
      end;


      if options#viewer_flag then
	printf "%c%!" Const.viewer_mode_status_EDITSEQ_GEN
      else
	Xprint.verbose options#verbose_flag "generating edit sequence...";

      let edits = new Edit.seq options in

      Postprocessing.generate_edits options lang cenv pruned edits uidmapping;

      DEBUG_MSG "generated edits:\n %s" edits#to_string;

      Xprint.verbose options#verbose_flag "fixing up edit sequences...";
      Postprocessing.fixup_edits options lang cenv tree1 tree2 pruned edits uidmapping pre_uidmapping;

      let dchange = Filename.concat cache_path Stat.changes_file_name in
      let edits_copy = edits#copy in
      edits_copy#ungroup tree1 tree2;
      edits_copy#cleanup_ghost tree1 tree2;
      Edit.dump_changes options lang tree1 tree2 uidmapping edits_copy edits dchange;

      if options#dump_delta_flag then begin
        let delta =
          if options#dump_delta_out <> "" then
            options#dump_delta_out
          else
            Filename.concat cache_path Delta_base.delta_file_name^".xml"
        in
        let info_file_path = Filename.concat cache_path "delta_info.json" in
        edits#dump_delta ~info_file_path tree1 tree2 uidmapping edits_copy delta
      end;

      let orig_edits =
        if options#ignore_non_orig_relabel_flag || options#ignore_move_of_unordered_flag then
          edits#copy
        else
          edits
      in

      if options#ignore_non_orig_relabel_flag then begin
        DEBUG_MSG "filtering relabels...";
        edits#filter_relabels
          (function
            | Relabel(movrel, (uid1, info1, exc1), (uid2, info2, exc2)) as rel -> begin
                let _ = rel in
                let nd1 = Info.get_node info1 in
                let nd2 = Info.get_node info2 in
                (*let is_order_insensitive n = n#data#is_order_insensitive in
                let has_order_insensitive n = Array.exists is_order_insensitive n#initial_children in
                not (has_order_insensitive nd1 || has_order_insensitive nd2) &&*)
                match nd1#data#orig_lab_opt, nd2#data#orig_lab_opt with
                | Some o1, Some o2 when o1 = o2 -> begin
                    DEBUG_MSG "filtered: %s" (Edit.to_string rel);
                    false
                end
                | _ when begin
                    (not nd1#data#is_named_orig) && (not nd1#data#has_value) &&
                    (not nd2#data#is_named_orig) && (not nd2#data#has_value) &&
                    nd1#data#anonymized_label = nd2#data#anonymized_label &&
                    nd1#data#elem_name_for_delta = nd2#data#elem_name_for_delta
                end -> begin
                  DEBUG_MSG "filtered: %s" (Edit.to_string rel);
                  false
                end
                | _ when nd1#data#orig_to_elem_data_for_eq = nd2#data#orig_to_elem_data_for_eq -> begin
                    DEBUG_MSG "filtered: %s" (Edit.to_string rel);
                    false
                end

                | _ -> true
            end
            | _ -> true
          )
      end;
      if options#ignore_move_of_unordered_flag(* && not options#recover_orig_ast_flag*) then begin

        DEBUG_MSG "filtering moves...";

        let check_mov_weak nd1 nd2 = function
          | Editop.Move(mid, kind, (uid1, info1, exc1), (uid2, info2, exc2)) as mov -> begin
              let b =
                nd1#data#is_order_insensitive && nd2#data#is_order_insensitive &&
                let pnd1 = nd1#initial_parent in
                let pnd2 = nd2#initial_parent in
                (try (uidmapping#find pnd1#uid) = pnd2#uid with _ -> false) &&
                match edits#find12 pnd1#uid pnd2#uid with
                | [] -> true
                | [Editop.Relabel _] -> pnd1#data#elem_name_for_delta = pnd2#data#elem_name_for_delta
                | _ -> false
              in
              let _ = mov in
              DEBUG_MSG "%s -> %B" (Edit.to_string mov) b;
              b
          end
          | _ -> assert false
        in
        let check_mov_for_delta nd1 nd2 = function
          | Editop.Move(mid, kind, (uid1, info1, exc1), (uid2, info2, exc2)) as mov -> begin
              let b =
                nd1#data#is_order_insensitive && nd2#data#is_order_insensitive &&
                !exc1 = [] && !exc2 = [] &&
                let pnd1 = nd1#initial_parent in
                let pnd2 = nd2#initial_parent in
                (try (uidmapping#find pnd1#uid) = pnd2#uid with _ -> false) &&
                (try
                  edits#iter_moves
                    (function
                      | Editop.Move(_, _, (_, _, e1), (_, _, _)) -> begin
                          List.iter
                            (fun inf ->
                              if Info.get_node inf == nd1 then
                                raise Exit
                            ) !e1
                      end
                      | _ -> assert false
                    );
                  true
                with
                  Exit -> false) &&
                match edits#find12 pnd1#uid pnd2#uid with
                | [] -> true
                | [Editop.Relabel _] -> pnd1#data#elem_name_for_delta = pnd2#data#elem_name_for_delta
                | _ -> false
              in
              let _ = mov in
              DEBUG_MSG "%s -> %B" (Edit.to_string mov) b;
              b
          end
          | _ -> assert false
        in
        let check_mov =
          if options#dump_delta_flag then
            check_mov_for_delta
          else
            check_mov_weak
        in

        edits#filter_moves
          (function
            | Editop.Move(mid, kind, (uid1, info1, exc1), (uid2, info2, exc2)) as mov -> begin
                let _ = mov in
                let nd1 = Info.get_node info1 in
                let nd2 = Info.get_node info2 in
                if
                  check_mov nd1 nd2 mov
                then begin
                  DEBUG_MSG "filtered: %s" (Edit.to_string mov);
                  begin
                    edits#iter_relabels
                      (function
                        | Relabel(movrel, (u1, i1, _), (u2, i2, _)) -> begin
                            let n1 = Info.get_node i1 in
                            let n2 = Info.get_node i2 in
                            if tree1#is_initial_ancestor nd1 n1 && tree2#is_initial_ancestor nd2 n2 then
                              movrel := false
                        end
                        | _ -> ()
                      )
                  end;
                  false
                end
                else
                  true
            end
            | _ -> true
          )
      end;
      Xprint.verbose options#verbose_flag "done.";

      BEGIN_DEBUG
        let moved_nodes = edits#get_moved_nodes tree1 in
        DEBUG_MSG "|moved_nodes|=%d" (Xset.length moved_nodes);
      END_DEBUG;

      let diff_status = get_diff_status options lang cenv orig_edits edits uidmapping tree1 tree2 in

      if cenv#use_adjacency_cache then
	DEBUG_MSG "size of adjacency cache: %d (cache hit: %d)"
	  cenv#size_of_adjacency_cache cenv#adjacency_cache_hit_count;

      if cenv#use_similarity_cache then
	DEBUG_MSG "size of similarity cache: %d (cache hit: %d)"
	  cenv#size_of_similarity_cache cenv#similarity_cache_hit_count;

      if cenv#use_mapping_comparison_cache then
	DEBUG_MSG "size of mapping comparison cache: %d (cache hit: %d)"
	  cenv#size_of_mapping_comparison_cache cenv#mapping_comparison_cache_hit_count;

      if uidmapping#use_crossing_or_incompatible_matches_count_cache then
	DEBUG_MSG "size of crossing or incompatible matches count cache: %d (cache hit: %d)"
	  uidmapping#size_of_crossing_or_incompatible_matches_count_cache
	  uidmapping#crossing_or_incompatible_matches_count_cache_hit_count;

      diff_status

(*
    with
      Sys_error msg -> Xprint.error "%s" msg; exit 1
*)
    end
(*
    tree1#show_whole_initial_subtree_size_hist;
    tree1#show_whole_initial_subtree_scan_hist;
    tree2#show_whole_initial_subtree_size_hist;
    tree2#show_whole_initial_subtree_scan_hist;
*)

  (* end of compare_tree *)


  class tree_comparator lang options ?(cache_path="") file1 file2 = object
    inherit Lang.tree_comparator

    val mutable extra_source_files1 = []
    val mutable extra_source_files2 = []

    method extra_source_files1 = extra_source_files1
    method extra_source_files2 = extra_source_files2


    method compare =

      let cache_path =
        if cache_path = "" then
          options#get_cache_path_for_file2 file1 file2
        else
	  cache_path
      in

      let multiple_subtree_matches = new Comparison.multiple_subtree_matches options in
      let multiple_node_matches =
        new Comparison.multiple_node_matches (fun l -> Label.to_string (Obj.obj l : Label.t))
      in

(*
    try
*)

      if options#viewer_flag then
	printf "%c%!" Const.viewer_mode_status_PARSE;

      let tree1 =
        let tree_builder1 = lang#make_tree_builder options in
        let t = tree_builder1#build_tree file1 in
        extra_source_files1 <- tree_builder1#extra_source_files;
        t
      in
      let tree2 =
        let tree_builder2 = lang#make_tree_builder options in
        let t = tree_builder2#build_tree file2 in
        extra_source_files2 <- tree_builder2#extra_source_files;
        t
      in

      let has_elaborate_edits = lang#has_elaborate_edits in

      begin
        let gain = 1. in
        let a = 32 in
        let t = 50000 in
        let sz = min tree1#initial_size tree2#initial_size in
        let x = (float a) /. (1. +. (exp (gain *. float(t - sz)))) in
        let mt = truncate x in
        options#set_subtree_match_threshold mt;
        Xprint.verbose options#verbose_flag "subtree_match_threshold set to %d" mt;
        if mt = a then begin
          options#set_simple_glue_flag;
          Xprint.verbose options#verbose_flag "simple_glue_flag set";
        end
      end;

      let cenv = new Comparison.c options ~has_elaborate_edits tree1 tree2 in
      cenv#set_cache_path cache_path;
      cenv#set_multiple_subtree_matches multiple_subtree_matches;
      cenv#set_multiple_node_matches multiple_node_matches;

      let cache_path1 = options#get_cache_path_for_file1 file1 in
      let cache_path2 = options#get_cache_path_for_file1 file2 in
      Cache.prepare_cache_dir options cache_path1;
      Cache.prepare_cache_dir options cache_path2;

      begin
	try
	  let k, v = options#fact_versions.(0) in
	  tree1#set_vkind k;
	  tree1#set_version v
	with
	  Invalid_argument _ -> ()
      end;
      begin
	try
	  let k, v = options#fact_versions.(1) in
	  tree2#set_vkind k;
	  tree2#set_version v
	with
	  Invalid_argument _ -> ()
      end;

      Stat.dump_source options file1 tree1;
      Stat.dump_source options file2 tree2;

      Stat.dump_parser options file1 tree1;
      Stat.dump_parser options file2 tree2;

      Stat.dump_file_info options file1 tree1;
      Stat.dump_file_info options file2 tree2;

      if options#fact_flag then begin
	let extract_fact1 = lang#extract_fact options cache_path1 in
	let extract_fact2 = lang#extract_fact options cache_path2 in

	begin
	  try
	    let r = options#fact_proj_roots.(0) in
	    tree1#set_proj_root r
	with
	  Invalid_argument _ -> ()
	end;
	begin
	  try
	    let r = options#fact_proj_roots.(1) in
	    tree2#set_proj_root r
	with
	  Invalid_argument _ -> ()
	end;

	extract_fact1 tree1;
	extract_fact2 tree2;

      end;

(*
      Xprint.verbose options#verbose_flag "line terminator of T1: %s"
	tree1#line_terminator_name;
      Xprint.verbose options#verbose_flag "line terminator of T2: %s"
	tree2#line_terminator_name;
*)

      Xprint.verbose options#verbose_flag "|T1|=%d |T2|=%d" tree1#initial_size tree2#initial_size;


      let digest1 = tree1#digest in
      let digest2 = tree2#digest in

      Xprint.verbose options#verbose_flag "digest of T1: %s" (Xhash.to_hex digest1);
      Xprint.verbose options#verbose_flag "digest of T2: %s" (Xhash.to_hex digest2);

      if digest1 = digest2 then begin
        DEBUG_MSG "creating trivial mapping...";
        let uidmapping = new UIDmapping.c cenv in
        tree1#fast_scan_whole_initial
          (fun nd1 ->
            let nd2 = tree2#search_node_by_gindex nd1#gindex in
            ignore (uidmapping#add_settled nd1#uid nd2#uid)
          );
        let edits = new Edit.seq options in
        get_diff_status options lang cenv edits edits uidmapping tree1 tree2
      end
      else begin

        (* pre-pruning and pre-matching *)

        let pre_uidmapping = new UIDmapping.c cenv in
        let ref_uidmapping = new UIDmapping.c cenv in

        let may_be_unsettled1 = Xset.create 0 in
        let may_be_unsettled2 = Xset.create 0 in

        let add tbl x nd =
	  try
	    let nds = Hashtbl.find tbl x in
	    if not (List.memq nd nds) then
	      Hashtbl.replace tbl x (nd::nds)
	  with
	    Not_found -> Hashtbl.add tbl x [nd]
        in

	let ltbl1 = Hashtbl.create 0 in
	let ltbl2 = Hashtbl.create 0 in

        let getlab n =
          (*match n#data#orig_lab_opt with
            | Some o -> o
            | None -> *)n#data#_label
        in

        if options#preprune_flag || options#prematch_flag then begin

	  tree1#fast_scan_whole_initial (fun nd -> add ltbl1 (getlab nd) nd);
	  tree2#fast_scan_whole_initial (fun nd -> add ltbl2 (getlab nd) nd);

	  BEGIN_DEBUG
	    List.iter
	      (fun (tag, tbl) ->
	        Hashtbl.iter
                  (fun l nds ->
                    DEBUG_MSG "%s: [%s]%s --> %d times (%s)"
                      tag
                      (Label.to_string (Obj.obj l))
                      (if (List.hd nds)#initial_nchildren = 0 then "[LEAF]" else "")
                      (List.length nds)
                      (String.concat ";" (List.map (fun n -> UID.to_string n#uid) nds))
                  ) tbl;
                DEBUG_MSG "%d entries in %s" (Hashtbl.length tbl) tag
              ) [("ltbl1", ltbl1); ("ltbl2", ltbl2)]
          END_DEBUG;

          let visited1 = Xset.create 0 in
          let visited2 = Xset.create 0 in

          let rec check_ancestors nd1 nd2 =
            DEBUG_MSG "%a-%a" nps nd1 nps nd2;
            try
              let pnd1 = nd1#initial_parent in
              let pnd2 = nd2#initial_parent in

              if
                pnd1#data#is_boundary || pnd2#data#is_boundary ||
                Xset.mem visited1 pnd1 || Xset.mem visited2 pnd2
              then
                raise Exit;

              if
                pnd1#data#anonymized_label = pnd2#data#anonymized_label &&
                nd1#data#eq nd2#data && pnd1#data#is_sequence
              then begin
                cenv#add_permutation_hub_cand pnd1 pnd2 nd1#data#label
              end
              else begin
                Xset.add visited1 pnd1;
                Xset.add visited2 pnd2;
              end;

              if pnd1#data#anonymized_label = pnd2#data#anonymized_label then
                check_ancestors pnd1 pnd2

            with _ -> ()
          in

	  Hashtbl.iter
	    (fun _lab nds1 ->
	      let lab = Obj.obj _lab in
	      try
	        let nds2 = Hashtbl.find ltbl2 _lab in
	        if Label.is_named lab || Label.has_value lab then begin
                  match nds1, nds2 with
                  | [nd1], [nd2] -> check_ancestors nd1 nd2
                  | _ -> ()
                end
              with
                Not_found -> ()
            ) ltbl1;

          cenv#finalize_permutation_hub_tbl();

        end;

        let sort_nds =
	  List.fast_sort (fun nd1 nd2 -> Stdlib.compare nd2#gindex nd1#gindex)
        in

        let pruned_list1 = ref [] in
        let pruned_list2 = ref [] in

        let pruned1 = Xset.create 0 in
        let pruned2 = Xset.create 0 in

        let prune1 nd =
          DEBUG_MSG "%a to be pruned" nups nd;
          Xset.add pruned1 nd
        in
        let prune2 nd =
          DEBUG_MSG "%a to be pruned" nups nd;
          Xset.add pruned2 nd
        in

        let part pruned_nodes =
          List.partition
            (fun n ->
              let b =
                try
                  let ba = find_boundary n in
                  not (Xset.mem pruned_nodes ba)
                with _ -> true
              in
              if not b then
                DEBUG_MSG "filtered: %a %s" nups n n#data#to_string;
              b
            )
        in

        let locked1 = Xset.create 0 in
        let locked2 = Xset.create 0 in

        let lock1 nd =
          DEBUG_MSG "%a to be locked" nups nd;
          Xset.add locked1 nd
        in
        let lock2 nd =
          DEBUG_MSG "%a to be locked" nups nd;
          Xset.add locked2 nd
        in

        (* pre-pruning *)

        if options#preprune_flag then begin

	  DEBUG_MSG "prepruning (and locking collapsed nodes)...";

	  let tbl1 = Hashtbl.create 0 in
	  let tbl2 = Hashtbl.create 0 in

	  tree1#fast_scan_whole_initial
	    (fun nd ->
	      match nd#data#digest with
              | None when begin
                  nd#data#has_non_trivial_value && nd#initial_nchildren = 0 &&
                  cenv#under_permutation_hub1 nd
              end -> begin
                let v = nd#data#get_value in
                DEBUG_MSG "value(tree1): %a(%a) -> %s" nups nd ngps nd v;
                add tbl1 v nd
              end
	      | None -> ()
	      | Some d ->
		  DEBUG_MSG "digest(tree1): %a(%a) -> %s" nups nd ngps nd nd#data#digest_string;

		  add tbl1 d nd
	    );
	  tree2#fast_scan_whole_initial
	    (fun nd ->
	      match nd#data#digest with
              | None when begin
                  nd#data#has_non_trivial_value && nd#initial_nchildren = 0 &&
                  cenv#under_permutation_hub2 nd
              end -> begin
                let v = nd#data#get_value in
                DEBUG_MSG "value(tree1): %a(%a) -> %s" nups nd ngps nd v;
                add tbl2 v nd
              end
	      | None -> ()
	      | Some d ->
		  DEBUG_MSG "digest(tree2): %a(%a) -> %s" nups nd ngps nd nd#data#digest_string;

		  add tbl2 d nd
	    );

	  BEGIN_DEBUG
	    DEBUG_MSG "size of digest table1: %d" (Hashtbl.length tbl1);
	    DEBUG_MSG "size of digest table2: %d" (Hashtbl.length tbl2)
          END_DEBUG;

	  let add_may_be_unsettled may_be_unsettled nd =
	    try
	      let p = nd#initial_parent#uid in
	      Xset.add may_be_unsettled p
	    with
	      Otreediff.Otree.Parent_not_found _ -> ()
	  in (* end of func add_may_be_unsettled *)

	  let pre_map_add nd1 nd2 =
	    let us1, us2 = ref [], ref [] in
	    tree1#fast_scan_whole_initial_subtree nd1 (fun n -> us1 := n#uid::!us1);
	    tree2#fast_scan_whole_initial_subtree nd2 (fun n -> us2 := n#uid::!us2);

	    BEGIN_DEBUG
	      List.iter2
	        (fun u1 u2 ->
		  DEBUG_MSG "adding %a - %a" ups u1 ups u2
	        ) !us1 !us2
            END_DEBUG;

	    List.iter2
              (fun u1 u2 ->
                ignore (pre_uidmapping#add_settled ~stable:false u1 u2)
              ) !us1 !us2;

	    pre_uidmapping#add_settled_roots nd1#uid nd2#uid;

	    List.iter2
	      (fun u1 u2 -> pre_uidmapping#add_stable_pair u1 u2)
	      !us1 !us2;

            (* regard the parents as possible unsettled mapping sources *)
            add_may_be_unsettled may_be_unsettled1 nd1;
            add_may_be_unsettled may_be_unsettled2 nd2;
	  in (* end of func pre_map_add *)

	  Hashtbl.iter
	    (fun d nds ->
	      Hashtbl.replace tbl2 d (sort_nds nds)
	    ) tbl2;

          let _multi_match_list = ref [] in

          let lock_cond nd =
            nd#data#is_statement && nd#initial_nchildren > 0 &&
            Array.for_all
              (fun n -> not n#data#has_value || n#data#has_non_trivial_value )
              nd#initial_children
          in

          let parent_matches = Xset.create 0 in

          let add_subtree_match ?(add_parent_match_only=false) ?(add_parent_match=true) ?(mes="") nd1 nd2 =

            let sz = tree1#whole_initial_subtree_size nd1 in

            DEBUG_MSG "%sdigest (or value) match: %a[%a] <--> %a[%a] <%a> (size=%d)" mes
              nups nd1 locps nd1 nups nd2 locps nd2 labps nd1 sz;

            cenv#add_subtree_match (nd1, nd2, sz);

            if not add_parent_match_only then begin
              pre_map_add nd1 nd2;
              if
                nd1#data#is_boundary ||
                nd1#data#has_non_trivial_value && nd1#initial_nchildren = 0
              then begin
                prune1 nd1;
                prune2 nd2;
              end
              else begin
                lock1 nd1;
                lock2 nd2;
              end
            end;

            if add_parent_match && lock_cond nd1 then begin
              try
                let pu1 = nd1#initial_parent#uid in
                let pu2 = nd2#initial_parent#uid in
                DEBUG_MSG "adding parent match: (%a,%a)" ups pu1 ups pu2;
                Xset.add parent_matches (pu1, pu2)
              with _ -> ()
            end
          in

          let matched_digests = Xset.create 0 in

	  Hashtbl.iter
	    (fun d nds ->
              let nds1 = sort_nds nds in
	      Hashtbl.replace tbl1 d nds1;
              try
	        let nds2 = Hashtbl.find tbl2 d in

	        let nds1 = List.filter (fun n -> tree1#root != n) nds1 in
	        let nds2 = List.filter (fun n -> tree2#root != n) nds2 in

                match nds1, nds2 with
                | [], _ | _, [] -> ()
                | [nd1], [nd2] ->
                    (*Xset.add matched_digests d;*)
                    add_subtree_match ~add_parent_match:false nd1 nd2

                | _ ->
                    Xset.add matched_digests d;
                    _multi_match_list := (d, nds1, nds2) :: !_multi_match_list
              with
                _ -> ()
	    ) tbl1;

          let multi_match_list =
            List.fast_sort
              (fun (_, nds10, nds20) (_, nds11, nds21) ->
                match nds10, nds20, nds11, nds21 with
                | nd10::_, nd20::_, nd11::_, nd21::_ ->
                    compare (nd10#gindex, nd20#gindex) (nd11#gindex, nd21#gindex)
                | _ -> 0
              ) !_multi_match_list
          in

	  let getmems tree nd =
	    let m = ref [] in
	    tree#fast_scan_whole_initial_subtree nd (fun n -> m := n::!m);
	    !m
	  in

          let lock_cand_tbl = Hashtbl.create 0 in

          let count = ref 0 in

          let is_matched_subtree x =
            let b =
              match x#data#_digest with
              | None -> false
              | Some d -> Xset.mem matched_digests d
            in
            DEBUG_MSG "%a -> %B" nups x b;
            b
          in
          let get_matched_sibling_ratio nd =
            let count = ref 0 in
            let matched = ref 0 in
            try
              Array.iter
                (fun c ->
                  incr count;
                  if c != nd && is_matched_subtree c then
                    incr matched
                ) nd#initial_parent#initial_children;
              if !count > 0 then begin
                let m = !matched + 1 in
                let c = !count in
                let r = (float m) /. (float c) in
                DEBUG_MSG "%d/%d=%.3f" m c r;
                r
              end
              else
                0.0
            with _ -> 0.0
          in
          let find_anc_stmt =
            Sourcecode.find_nearest_p_ancestor_node (fun x -> x#data#is_statement)
          in
          let has_no_anc_stmt x =
            try
              let _ = find_anc_stmt x in
              false
            with
              Not_found -> true
          in

          List.iter
            (fun (d, nds1, nds2) ->

              DEBUG_MSG "[%a] <--> [%a]" nsps nds1 nsps nds2;

              match nds1, nds2 with
              | [], _ | _, [] -> ()
              | [nd1], [nd2] -> begin
                  DEBUG_MSG "!!!!!";
                  add_subtree_match ~mes:"2ND " nd1 nd2
              end
              | _ -> begin

                  let nds1_, filtered_nds1 = part pruned1 nds1 in
                  let nds2_, filtered_nds2 = part pruned2 nds2 in
                  let n_filtered_nds1 = List.length filtered_nds1 in
                  let n_filtered_nds2 = List.length filtered_nds2 in
                  assert (n_filtered_nds1 = n_filtered_nds2);
                  (*count := !count + n_filtered_nds1;*)

                  let nds1_, nds2_ =
                    match nds1_, nds2_ with
                    | [nd1], [nd2] -> begin
                        DEBUG_MSG "%a-%a %a [%a]-[%a]" nps nd1 nps nd2 labps nd1 locps nd1 locps nd2;

                        let matched_sibling_ratio1 = get_matched_sibling_ratio nd1 in
                        let matched_sibling_ratio2 = get_matched_sibling_ratio nd2 in

                        DEBUG_MSG "matched_sibling_ratio=%.3f (%a)" matched_sibling_ratio1 nps nd1;
                        DEBUG_MSG "matched_sibling_ratio=%.3f (%a)" matched_sibling_ratio2 nps nd2;

                        let rec in_matched_subtree x =
                          try
                            let p = x#initial_parent in
                            let b =
                              match p#data#_digest with
                              | None -> false
                              | Some d ->
                                  Xset.mem matched_digests d ||
                                  try
                                    match cenv#multiple_subtree_matches#find d with
                                    | [], _, _ | _, [], _ -> false
                                    | _ -> true
                                  with
                                    Not_found -> false
                            in
                            if b then begin
                              DEBUG_MSG "%a -> %B" nups x b;
                              b
                            end
                            else
                              in_matched_subtree p
                          with
                            _ -> false
                        in

                        if
                          nd1#data#is_statement &&
                          has_no_anc_stmt nd1 && has_no_anc_stmt nd2 &&
                          matched_sibling_ratio1 > matched_sibling_ratio_thresh &&
                          matched_sibling_ratio2 > matched_sibling_ratio_thresh
                        then begin
                          DEBUG_MSG "!!!!!";
                          add_subtree_match ~add_parent_match_only:true ~mes:"3RD " nd1 nd2;
                          count := !count + n_filtered_nds1;
                          nds1_, nds2_
                        end
                        else if
                          nd1#data#is_op &&
                          try
                            not (in_matched_subtree nd1)
                          with _ -> false
                        then begin
                          DEBUG_MSG "!!!!! %a [%a]-[%a]" labps nd1 locps nd1 locps nd2;
                          count := !count + n_filtered_nds1;
                          nds1_, nds2_
                        end
                        else
                          nds1, nds2

                    end
                    | [], _ | _, [] -> begin
                        count := !count + n_filtered_nds1;
                        nds1_, nds2_
                    end
                    | [nd1], _ -> begin
                        (*DEBUG_MSG "!!!!! nd1: %a:%s" nps nd1 nd1#data#to_string;*)
                        try
                          let pu1 = nd1#initial_parent#uid in
                          let nd2_cands =
                            List.fold_left
                              (fun l nd2 ->
                                let pu2 = nd2#initial_parent#uid in
                                if Xset.mem parent_matches (pu1, pu2) then begin
                                  DEBUG_MSG "nd2=%a" nups nd2;
                                  (pu2, nd2) :: l
                                end
                                else
                                  l
                              ) [] nds2
                          in
                          match nd2_cands with
                          | [pu2, nd2] -> begin
                              DEBUG_MSG "!!!!!";
                              let mes = sprintf "PARENT(%a,%a) " ups pu1 ups pu2 in
                              add_subtree_match ~add_parent_match:false ~mes nd1 nd2;
                              count := !count + n_filtered_nds1;
                              [], []
                          end
                          | _ -> nds1, nds2
                        with
                          _ -> nds1, nds2
                    end
                    | _, [nd2] -> begin
                        (*DEBUG_MSG "!!!!! nd2: %a:%s" nps nd2 nd2#data#to_string;*)
                        try
                          let pu2 = nd2#initial_parent#uid in
                          let nd1_cands =
                            List.fold_left
                              (fun l nd1 ->
                                let pu1 = nd1#initial_parent#uid in
                                if Xset.mem parent_matches (pu1, pu2) then begin
                                  DEBUG_MSG "nd1=%a" nups nd1;
                                  (pu1, nd1) :: l
                                end
                                else
                                  l
                              ) [] nds1
                          in
                          match nd1_cands with
                          | [pu1, nd1] -> begin
                              DEBUG_MSG "!!!!!";
                              let mes = sprintf "PARENT(%a,%a) " ups pu1 ups pu2 in
                              add_subtree_match ~add_parent_match:false ~mes nd1 nd2;
                              count := !count + n_filtered_nds1;
                              [], []
                          end
                          | _ -> nds1, nds2
                        with
                          _ -> nds1, nds2
                    end
                    | _ -> nds1, nds2
                  in
                  let nds1, nds2 = nds1_, nds2_ in

                  match nds1, nds2 with
                  | [], _ | _, [] -> ()
                  (*| [_], [_] -> ()*)
                  | _ -> begin
                      match nds1, nds2 with
                      | [], _ | _, [] -> ()
                      | _ -> begin

                          let ndmems1 = List.map (fun nd -> nd, getmems tree1 nd) nds1 in
                          let ndmems2 = List.map (fun nd -> nd, getmems tree2 nd) nds2 in

                          BEGIN_DEBUG
                            DEBUG_MSG "multiple digest match: %a" labps (List.hd nds1);
                            DEBUG_MSG "[%s] <--> [%s]"
                              (Xlist.to_string (fun (n, _) -> UID.to_string n#uid) ";" ndmems1)
                              (Xlist.to_string (fun (n, _) -> UID.to_string n#uid) ";" ndmems2);
                            let to_s nds =
                              (Xlist.to_string GI.to_string ";"
                                 (List.fast_sort Stdlib.compare
                                    (List.map (fun n -> n#gindex) nds)))
                            in
                            DEBUG_MSG "multiple digest match (gindex): [%s] <--> [%s]" (to_s nds1) (to_s nds2)
                          END_DEBUG;

                          begin
                            let add_lock_cand nd cand =
                              try
                                let pu = nd#initial_parent#uid in
                                try
                                  let l = Hashtbl.find lock_cand_tbl pu in
                                  Hashtbl.replace lock_cand_tbl pu (cand::l)
                                with Not_found ->
                                  Hashtbl.add lock_cand_tbl pu [cand]
                              with _ -> ()
                            in
                            (*let is_pre_mapped n1 n2 =
                              let b =
                                try
                                  pre_uidmapping#find n1#uid = n2#uid
                                with
                                  Not_found -> false
                              in
                              DEBUG_MSG "%a %a -> %B" nups n1 nups n2 b;
                              b
                            in*)
                            (*let is_cross_boundary n1 n2 =
                              let b =
                                try
                                  let bn1 = find_boundary n1 in
                                  let bn2 = find_boundary n2 in
                                  let bn_mapped =
                                    try
                                      pre_uidmapping#find bn1#uid = bn2#uid
                                    with Not_found -> false
                                  in
                                  not bn_mapped &&
                                  pre_uidmapping#mem_dom bn1#uid && pre_uidmapping#mem_cod bn2#uid &&
                                  Misc.is_cross_boundary pre_uidmapping n1 n2
                                with
                                  _ -> false
                              in
                              DEBUG_MSG "%a %a -> %B" nups n1 nups n2 b;
                              b
                            in*)
                            match nds1, nds2 with
                            (*| [nd1], [nd2] when begin
                                DEBUG_MSG "!!!!! operator to be locked? %a-%a" nups nd1 nups nd2;
                                nd1#data#is_op && not (is_pre_mapped nd1 nd2) &&
                                not (is_cross_boundary nd1 nd2) &&
                                Array.for_all
                                  (fun c ->
                                    not c#data#is_common &&
                                    (not c#data#has_value || c#data#has_non_trivial_value)
                                  ) nd1#initial_children &&
                                try
                                  let sn1 = find_anc_stmt nd1 in
                                  let sn2 = find_anc_stmt nd2 in
                                  let pred n =
                                    n#data#is_sequence ||
                                    Misc.has_p_descendant (fun x -> x#data#is_sequence) n
                                  in
                                  Array.exists pred sn1#initial_children &&
                                  Array.exists pred sn2#initial_children
                                with
                                  _ -> true
                            end -> begin
                              DEBUG_MSG "!!!!! locking operators: %a [%a]-[%a]" labps nd1 locps nd1 locps nd2;
                              lock1 nd1;
                              lock2 nd2;
                            end*)
                            | [_], [_] -> ()
                            | [nd1], nd2::_ when lock_cond nd1 -> add_lock_cand nd1 (nd1#initial_pos, nds1, nds2)
                            | nd1::_, [nd2] when lock_cond nd2 -> add_lock_cand nd2 (nd2#initial_pos, nds1, nds2)
                            | _ -> ()
                          end;

                          multiple_subtree_matches#add d (ndmems1, ndmems2)
                      end
                  end
                end
            ) multi_match_list;

          DEBUG_MSG "%d node pairs deleted from multi_match_list" !count;

          begin
            let context_thresh = 5 in

            DEBUG_MSG "context_thresh=%d" context_thresh;

            let lock_cand (_, nds1, nds2) =
              BEGIN_DEBUG
                let n_to_str n = (UID.to_string n#uid)^":"^n#data#to_string in
                DEBUG_MSG "[%s]" (String.concat "; " (List.map n_to_str nds1));
                DEBUG_MSG "-> [%s]" (String.concat "; " (List.map n_to_str nds2));
              END_DEBUG;
              List.iter lock1 nds1;
              List.iter lock2 nds2;
            in
            Hashtbl.iter
              (fun puid _cand_list ->

                let cand_list =
                  List.fast_sort (fun (pos0, _, _) (pos1, _, _) -> compare pos0 pos1) _cand_list
                in
                BEGIN_DEBUG
                  DEBUG_MSG "puid=%a" ups puid;
                  List.iter
                    (fun (pos, nds1, nds2) ->
                      DEBUG_MSG "%d [%a] [%a]" pos nsps nds1 nsps nds2
                    ) cand_list
                END_DEBUG;

                let a = Array.of_list cand_list in
                let alen = Array.length a in

                if alen >= context_thresh then begin

                  DEBUG_MSG "* puid=%a" ups puid;

                  let _, ns10, ns20 = a.(0) in
                  let len1 = List.length ns10 in
                  let len2 = List.length ns20 in
                  DEBUG_MSG "len1=%d len2=%d" len1 len2;
                  let sel x1 x2 = if len1 = 1 then x2 else x1 in
                  let mat = Array.make_matrix (sel len1 len2) alen GI.dummy in
                  let ok =
                    try
                      Array.iteri
                        (fun i (_, ns1, ns2) ->
                          if List.length ns1 = len1 && List.length ns2 = len2 then begin
                            List.iteri
                              (fun j n ->
                                mat.(j).(i) <- n#gindex
                              ) (sort_nds (sel ns1 ns2))
                          end
                          else
                            raise Exit
                        ) a;
                      Array.iteri
                        (fun j matj ->
                          DEBUG_MSG "j=%d" j;
                          Array.iteri
                            (fun i g ->
                              DEBUG_MSG "%d: %a" i gps g;
                              if try matj.(i-1) > g with _ -> false then
                                raise Exit
                            ) matj
                        ) mat;
                      true
                    with
                      Exit -> false
                  in
                  DEBUG_MSG "ok=%B" ok;

                  if ok then begin
                    Array.iteri
                      (fun i (pos, _nds1, _nds2) ->
                        let nds1, nds2 =
                          match _nds1, _nds2 with
                          | [nd1], _ -> [nd1], []
                          | _, [nd2] -> [], [nd2]
                          | _ -> assert false
                        in
                        a.(i) <- (pos, nds1, nds2)
                      ) a;
                    let head_pos = ref (-1) in
                    let head_i = ref 0 in
                    let delta = ref 1 in
                    try
                      Array.iteri
                        (fun i (pos, nds1, nds2) ->
                          DEBUG_MSG "[i=%d pos=%d] head_pos=%d head_i=%d delta=%d"
                            i pos !head_pos !head_i !delta;

                          if !head_pos < 0 then begin
                            DEBUG_MSG "  head_pos->%d head_i->%d" pos i;
                            head_pos := pos;
                            head_i := i
                          end
                          else if pos = !head_pos + !delta then begin
                            incr delta;
                            DEBUG_MSG "  delta->%d" !delta;
                          end
                          else begin
                            if false then begin
                              if !delta >= context_thresh then begin
                                let _i = i - 1 in
                                DEBUG_MSG "  !!!!! locking from %d-th to %d-th" !head_i _i;
                                for c = !head_i to _i do
                                  lock_cand a.(c)
                                done;
                              end;
                              DEBUG_MSG "  head_pos->%d head_i->%d delta->1" pos i;
                              head_pos := pos;
                              head_i := i;
                              delta := 1
                            end
                            else
                              raise Exit
                          end
                        ) a;
                      let _i = Array.length a - 1 in
                      if !head_i + !delta = _i + 1 then begin
                        if !delta >= context_thresh then begin
                          DEBUG_MSG "!!!!! locking from %d-th to %d-th" !head_i _i;
                          for c = !head_i to _i do
                            lock_cand a.(c)
                          done;
                        end;
                      end
                    with
                      Exit -> ()
                  end
                end
              ) lock_cand_tbl
          end;

          pruned_list1 := Xset.to_list pruned1;
          pruned_list2 := Xset.to_list pruned2;

          DEBUG_MSG "@";

	  let _ = (tree1#expand_all : UID.t -> bool) in
	  let _ = (tree2#expand_all : UID.t -> bool) in

	  tree1#prune_nodes !pruned_list1;
	  tree2#prune_nodes !pruned_list2;

	  List.iter
	    (fun (tree, set) ->
	      Xset.iter
	        (fun n ->
		  n#lock_collapse;

		  DEBUG_MSG "collapsed node locked: %a(%a) (subtree size: %d)" nups n ngps n
		    (tree#whole_initial_subtree_size n)
	        ) set
            ) [(tree1, locked1); (tree2, locked2)];

	  BEGIN_DEBUG
	    DEBUG_MSG "%d node (subtree) pairs pruned" (List.length !pruned_list1);
            DEBUG_MSG "%d collapsed node pairs locked" (Xset.length locked1)
          END_DEBUG;

	  if not options#no_collapse_flag then begin
	    DEBUG_MSG "collapsing nodes...";
	    tree1#collapse;
	    tree2#collapse
	  end;

	  tree1#collapse_nodes (Xset.mem locked1);
	  tree2#collapse_nodes (Xset.mem locked2);

	  DEBUG_MSG "prepruning (and locking collapsed) completed."
        end;
        (* end of pre-pruning *)


        (* pre-matching *)

        if options#prematch_flag (* || options#prematch_named_flag *) then begin
	  DEBUG_MSG "prematching...";


	  let check_pruned prnd tree nd =
(*	  let ndgi = nd#gindex in *)
	    try
	      List.iter
	        (fun n ->
		  if tree#initial_subtree_mem n nd then
		    raise Found
(*
		if (tree#initial_leftmost n)#gindex <= ndgi && ndgi <= n#gindex then
		  raise Found
*)
	        ) prnd;
	      false
	    with
              Found -> true
	  in

	  let check_locked lckd tree nd =
(*	  let ndgi = nd#gindex in *)
	    try
	      Xset.iter
	        (fun n ->
		  if tree#initial_subtree_mem n nd then
		    raise Found
(*
		if (tree#initial_leftmost n)#gindex <= ndgi && ndgi <= n#gindex then
		  raise Found
*)
	        ) lckd;
	      false
	    with
              Found -> true
	  in

	  let rt1, rt2 = tree1#root, tree2#root in


	  let prematch_ok1 nd =
	    not (check_pruned !pruned_list1 tree1 nd || check_locked locked1 tree1 nd) &&
	    nd != rt1
	  in

	  let prematch_ok2 nd =
	    not (check_pruned !pruned_list2 tree2 nd || check_locked locked2 tree2 nd) &&
	    nd != rt2
	  in


	  let reg nd1 nd2 =

	    DEBUG_MSG "node match: %a[%a] <--> %a[%a] <%a>" nups nd1 locps nd1 nups nd2 locps nd2 labps nd1;

	    let uid1, uid2 = nd1#uid, nd2#uid in
	    ignore (ref_uidmapping#add_unsettled uid1 uid2);
	    if
	      (nd1#initial_nchildren = 0 && nd2#initial_nchildren = 0 && nd1#data#is_named) (* ||
              (nd1#data#is_boundary && nd2#data#is_boundary) *) (* cf. airo.c 2164L-2673L *)
	    then begin
	      DEBUG_MSG "STABLE";
	      ref_uidmapping#add_stable_pair uid1 uid2
	    end
	  in

	  let register_matches _nds1 _nds2 =
            DEBUG_MSG "_nds1=[%a] _nds2=[%a]" nsps _nds1 nsps _nds2;
	    let len1 = List.length _nds1 in
	    let len2 = List.length _nds2 in
	    let thresh = options#prematch_cands_threshold in

	    if len1 > thresh || len2 > thresh then
	      ()
	    else
	      let nds1 = List.filter prematch_ok1 _nds1 in
	      let nds2 = List.filter prematch_ok2 _nds2 in

              DEBUG_MSG "nds1=[%a] nds2=[%a]" nsps nds1 nsps nds2;

	      match nds1, nds2 with
	      | [nd1], [nd2] -> begin
                  (*if
                    try
                      nd1#initial_parent#data#anonymized_label = nd2#initial_parent#data#anonymized_label
                    with _ -> true
                  then*)
                    reg nd1 nd2
              end
	      | _ ->
		  if options#prematch_early_resolve_flag then
		    let a1 = Array.of_list nds1 in
		    let a2 = Array.of_list nds2 in
		    let selected =
		      let cmpr = new SMP.ComparatorFloat.c cenv#get_adjacency_score a1 a2 in
		      SMP.get_stable_matches cmpr a1 a2
		    in
		    List.iter (fun (nd1, nd2) -> reg nd1 nd2) selected
	  in (* end of func register_matches *)

          (*let get_one nds =
            let tbl = Hashtbl.create 0 in
            List.iter
              (fun x ->
                let a = find_boundary x in
                try
                  let l = Hashtbl.find tbl a in
                  if l <> [] then
                    Hashtbl.replace tbl a []
                with
                  Not_found ->
                    Hashtbl.add tbl a [x]
              ) nds;
            let res = ref None in
            begin
              try
                Hashtbl.iter
                  (fun a l ->
                    match l with
                    | [x] -> begin
                        match !res with
                        | Some _ -> res := None; raise Exit
                        | None -> res := Some x
                    end
                    | _ -> ()
                  ) tbl
              with
                Exit -> ()
            end;
            !res
          in*)
	  Hashtbl.iter
	    (fun _lab nds1 ->
	      let lab = Obj.obj _lab in
	      try
	        let nds2 = Hashtbl.find ltbl2 _lab in
                (*let nds1, nds2 =
                  if List.length nds1 > 2 && List.length nds2 > 2 then
                    match get_one nds1, get_one nds2 with
                    | Some n1, Some n2 -> begin
                        reg n1 n2;
                        Xlist.subtract nds1 [n1], Xlist.subtract nds2 [n2]
                    end
                    | _ -> nds1, nds2
                  else
                    nds1, nds2
                in*)
(*
                let b0 = Label.has_non_trivial_value lab in
                let b1 = Label.is_string_literal lab || Label.is_int_literal lab in
                if b0 <> b1 then begin
                  DEBUG_MSG "!!!!! %s: non_trivial_value=%B (string||int)_literal=%B"
                    (Label.to_string lab) b0 b1;

                  DEBUG_MSG "[%a]-[%a]" nsps nds1 nsps nds2;
                  match nds1, nds2 with
                  | [nd1], [nd2] -> DEBUG_MSG "[%a]-[%a]" locps nd1 locps nd2
                  | _ -> ()
                end;
*)
                if
                  Label.is_named lab ||
                  (*Label.has_non_trivial_value lab*)
                  Label.is_string_literal lab || Label.is_int_literal lab
                then begin
                  register_matches nds1 nds2;
                end;
	        if options#multi_node_match_flag then
		  multiple_node_matches#add _lab (nds1, nds2)
	      with
	        Not_found -> ()

	    ) ltbl1;

	  DEBUG_MSG "prematching completed."
        end;
        (* end of pre-matching *)

        compare_tree
	  options
	  lang
	  cenv
	  pre_uidmapping
	  may_be_unsettled1
	  may_be_unsettled2
	  ref_uidmapping
	  tree1 tree2

      end
(*
    with
    | Sys_error msg -> Xprint.error "%s" msg; exit 1
    | Xfile.No_extension f -> Xprint.error "have no file extension: \"%s\"" f; exit 1
*)
  (* end of method compare *)


  end (* of class comparator *)




end (* of functor Analyzing.F *)

(*
let compare options ?(cache_path="") file1 file2 =
  let ext1 = file1#get_extension in
  let ext2 = file2#get_extension in

  if ext1 <> ext2 then begin
    ERROR_MSG "different extensions: %s and %s" ext1 ext2;
    exit 1
  end;

  let lang = Lang.search options ext1 in

  let comp = lang.Lang.compare in

  comp options lang ~cache_path file1 file2
*)

let get_comparator options ?(cache_path="") file1 file2 =
  let ext1 = file1#get_extension in
  let ext2 = file2#get_extension in

  if ext1 <> ext2 && not options#parser_designated then begin
    Xprint.error "different extensions: %s and %s" ext1 ext2;
    exit 1
  end;

  let lang = Lang.search options ext1 in

  lang#make_tree_comparator options ~cache_path file1 file2
