(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

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
module UID = Otreediff.UID

open Printf

let cost tree1 tree2 i j =
  let nd1 = tree1#get i in
  let nd2 = tree2#get j in
  if i = 0 && j = 0 then
    Stdlib.max_int
  else if i = 0 then (* insert *)
    let f =
      if nd2#is_collapsed then
        nd2#data#weight
      else
        1
    in
    4 * f
  else if j = 0 then (* delete *)
    let f =
      if nd1#is_collapsed then
        nd1#data#weight
      else
        1
    in
    4 * f
  else
    if nd1#data#eq nd2#data then begin
      if nd1#path#equals nd2#path then
	if nd1#data#digest = nd2#data#digest then
	  if nd1#collapse_locked && nd2#collapse_locked then
	    0
	  else if nd1#is_collapsed && nd2#is_collapsed then
	    1
	  else
	    2
	else
	  2
      else
	3
    end
    else (* relabel *)
      5

let semantic_cost tree1 tree2 i j =
  let nd1 = tree1#get i in
  let nd2 = tree2#get j in
  if i = 0 && j = 0 then
    Stdlib.max_int
  else if i = 0 then (* insert *)
    let f =
      if nd2#is_collapsed then
        nd2#data#weight
      else
        1
    in
    4 * f
  else if j = 0 then (* delete *)
    let f =
      if nd1#is_collapsed then
        nd1#data#weight
      else
        1
    in
    4 * f
  else
    if nd1#data#eq nd2#data then begin
      if nd1#path#equals nd2#path then
	if nd1#data#digest = nd2#data#digest then
	  if nd1#collapse_locked && nd2#collapse_locked then
	    0
	  else if nd1#is_collapsed && nd2#is_collapsed then
	    1
	  else
	    2
	else
	  2
      else
	3
    end
    else (* relabel *)
      if nd1#data#relabel_allowed nd2#data then
        if nd1#data#quasi_eq nd2#data then
          4
        else if nd1#data#anonymized_label = nd2#data#anonymized_label then
          5
        else if nd1#data#anonymized2_label = nd2#data#anonymized2_label then
          6
        else
          7
      else
          100(*Stdlib.max_int*)

let _find cost tree1 tree2 = Otreediff.ZS.Int.find cost tree1 tree2
let find tree1 tree2 = _find cost tree1 tree2
let sfind tree1 tree2 = _find semantic_cost tree1 tree2



let find_glue_cands ?(simple=false) tree1 tree2 nodes1 nodes2 matches_tbl =

  DEBUG_MSG "\ndeleted:[%s]\ninserted:[%s]"
    (Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" nodes1)
    (Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" nodes2);

  let final_cands =
    if nodes1 = [] || nodes2 = [] then
      []

    else
      let cands = ref [] in

      let add tbl x nd =
	try
	  let nds = Hashtbl.find tbl x in
	  Hashtbl.replace tbl x (nd::nds)
	with
	  Not_found -> Hashtbl.add tbl x [nd]
      in

      (* stage 1: simple match *)
      let ltbl1 = Hashtbl.create 0 in
      let ltbl2 = Hashtbl.create 0 in
      List.iter (fun nd -> add ltbl1 nd#data#_label nd) nodes1;
      List.iter (fun nd -> add ltbl2 nd#data#_label nd) nodes2;

      let add_cand (nd1, nd2) =
        if not (try List.assq nd1 !cands == nd2 with Not_found -> false) then
          cands := (nd1, nd2) :: !cands
      in

      Hashtbl.iter 
	(fun lab nds1 ->
	  try
	    let nds2 = Hashtbl.find ltbl2 lab in
	    match nds1, nds2 with
	    | [nd1], [nd2] -> add_cand (nd1, nd2)
	    | nd1::_, nd2::_ ->
		DEBUG_MSG "[%s] vs [%s] --> abort" 
		  (Xlist.to_string (fun n -> UID.to_string n#uid) ";" nds1)
		  (Xlist.to_string (fun n -> UID.to_string n#uid) ";" nds2)
	    | _ -> assert false
	  with
	    Not_found -> ()
	) ltbl1;

      if not simple then begin
        (* stage 2: name match *)
        let cands1, cands2 = List.split !cands in
        let nodes1' = List.filter (fun n -> not (List.memq n cands1)) nodes1 in
        let nodes2' = List.filter (fun n -> not (List.memq n cands2)) nodes2 in
        if nodes1' <> [] && nodes2' <> [] then begin
	  let nltbl1 = Hashtbl.create 0 in
	  let nltbl2 = Hashtbl.create 0 in

	  let f tbl nd =
	    try
	      add tbl (nd#data#_anonymized2_label, nd#data#get_name) nd
	    with
	      Not_found -> ()
	  in
	  List.iter (f nltbl1) nodes1;
	  List.iter (f nltbl2) nodes2;
	  Hashtbl.iter 
	    (fun nl nds1 ->
	      try
	        let nds2 = Hashtbl.find nltbl2 nl in
	        match nds1, nds2 with
	        | [nd1], [nd2] -> add_cand (nd1, nd2)
	        | nd1::_, nd2::_ ->
		    DEBUG_MSG "[%s] vs [%s] --> abort" 
		      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" nds1)
		      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" nds2)
	        | _ -> assert false
	      with
	        Not_found -> ()
	    ) nltbl1;
        end
      end;

      !cands
  in    

  DEBUG_MSG "cands: [%s]" 
    (Xlist.to_string 
       (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" final_cands);

  final_cands
(* end of func find_glue_cands *)


let fast_match_trees tree1 tree2 ref_uidmapping = (* fast but inaccurate *)

  BEGIN_DEBUG
    DEBUG_MSG "|T1(root=%a)|=%d |T2(root=%a)|=%d" 
    UID.ps tree1#root#uid tree1#size UID.ps tree2#root#uid tree2#size;
    DEBUG_MSG "T1:\n%s\n" tree1#to_string;
    DEBUG_MSG "T2:\n%s\n" tree2#to_string
  END_DEBUG;

  let ref_matches = ref [] in
  ref_uidmapping#iter
    (fun u1 u2 ->
      try
	let n1 = tree1#search_node_by_uid u1 in
	let n2 = tree2#search_node_by_uid u2 in
	ref_matches := (n1, n2) :: !ref_matches
      with
	Not_found -> ()
(*
  DEBUG_MSG "not found: %a-%a" UID.ps u1 UID.ps u2
 *)
    );

  BEGIN_DEBUG
    DEBUG_MSG "ref_matches:";
    List.iter (fun (n1, n2) -> DEBUG_MSG "%s - %s" n1#to_string n2#to_string) !ref_matches;
(*
  List.iter 
  (fun (n1, n2) -> DEBUG_MSG "%a-%a" GI.ps n1#gindex GI.ps n2#gindex) 
  (List.fast_sort (fun (n1, _) (n2, _) -> Stdlib.compare n1#gindex n2#gindex) !ref_matches)
 *)
  END_DEBUG;

  let matches, relabels = ref [], ref [] in

  if !ref_matches = [] then begin
    let rec scan (nd1, nd2) =

      if nd1#data#equals nd2#data then
	matches := (nd1, nd2)::!matches 
      else
	if nd1#data#relabel_allowed nd2#data then
	  relabels := (nd1, nd2)::!relabels;

      let c1, c2 = nd1#children, nd2#children in

      let mk nd = nd#data#_label, nd#data#digest in

      let cdat1 = Array.map mk c1 in
      let cdat2 = Array.map mk c2 in

      let mat, rel, _, _ = Adiff.adiff cdat1 cdat2 in

      List.iter 
	scan 
	(List.map 
	   (fun (i1, i2) -> 
	     try
	       c1.(i1), c2.(i2)
	     with Invalid_argument _ -> assert false
	   ) (mat @ rel))
    in
    scan (tree1#root, tree2#root)
  end
  else begin
    let matches' = ref [] in

    let ref_matches1, ref_matches2 = List.split !ref_matches in

    let rec check_parents nd1 nd2 =
      try
        let pnd1 = nd1#parent in
        let pnd2 = nd2#parent in
        
        if not (List.memq pnd1 ref_matches1 || List.memq pnd2 ref_matches2) then
          if pnd1#data#eq pnd2#data then begin
            matches' := (pnd1, pnd2) :: !matches';
            check_parents pnd1 pnd2
          end
          else 
            if pnd1#data#relabel_allowed pnd2#data then begin
	      relabels := (pnd1, pnd2) :: !relabels;
	      check_parents pnd1 pnd2
            end

      with
        Otreediff.Otree.Parent_not_found _ -> ()
    in
    List.iter
      (fun (nd1, nd2) ->
	check_parents nd1 nd2
      ) !ref_matches;

(*
  let relabels' = ref [] in

  let matches_tbl = Hashtbl.create 0 in
  List.iter
  (fun (nd1, nd2) ->
  let a1 = Array.of_list (tree1#ancestor_nodes nd1) in
  let a2 = Array.of_list (tree2#ancestor_nodes nd2) in
  let mat, rel, _, _ = Adiff.adiff a1 a2 in
  List.iter
  (fun (i, j) ->
  Hashtbl.add matches_tbl (a1.(i), a2.(j)) true
  ) mat;
  List.iter
  (fun (i, j) ->
  let an1, an2 = a1.(i), a2.(j) in
  if an1#data#relabel_allowed an2#data then
  relabels' := (an1, an2) :: !relabels'
  ) rel
  ) !ref_matches;

  Hashtbl.iter
  (fun (nd1, nd2) _ ->
  matches' := (nd1, nd2) :: !matches'
  ) matches_tbl;

  let rtbl1 = Hashtbl.create 0 in
  let rtbl2 = Hashtbl.create 0 in
  List.iter
  (fun (nd1, nd2) ->
  begin
  try
  let freq = Hashtbl.find rtbl1 nd1 in
  Hashtbl.replace rtbl1 nd1 (freq + 1)
  with
  Not_found -> Hashtbl.replace rtbl1 nd1 1
  end;
  begin
  try
  let freq = Hashtbl.find rtbl2 nd2 in
  Hashtbl.replace rtbl2 nd2 (freq + 1)
  with
  Not_found -> Hashtbl.replace rtbl2 nd2 1
  end
  ) !relabels';
  List.iter
  (fun (nd1, nd2) ->
  if Hashtbl.find rtbl1 nd1 = 1 && Hashtbl.find rtbl2 nd2 = 1 then
  relabels := (nd1, nd2) :: !relabels
  ) !relabels';
 *)

    BEGIN_DEBUG
      DEBUG_MSG "matches':";
      List.iter (fun (n1, n2) -> DEBUG_MSG "%s - %s" n1#to_string n2#to_string) !matches';
(*
  List.iter 
  (fun (n1, n2) -> DEBUG_MSG "%a-%a" GI.ps n1#gindex GI.ps n2#gindex) 
  (List.fast_sort (fun (n1, _) (n2, _) -> Stdlib.compare n1#gindex n2#gindex) !matches')
 *)
    END_DEBUG;

    matches := !ref_matches @ !matches'
				
  end;


  let deletes, inserts = ref [], ref [] in

  let matches1, matches2 = List.split !matches in
  let relabels1, relabels2 = List.split !relabels in
  tree1#scan_all
    (fun nd ->
      if not (List.memq nd matches1 || List.memq nd relabels1) then
	deletes := nd :: !deletes
    );
  tree2#scan_all
    (fun nd ->
      if not (List.memq nd matches2 || List.memq nd relabels2) then
	inserts := nd :: !inserts
    );

  (* we want more matches! *)
  let matches_tbl = Hashtbl.create 0 in
  List.iter (fun (n1, n2) -> Hashtbl.add matches_tbl n1#uid n2#uid) !matches;
  let extra_matches =
    find_glue_cands tree1 tree2 !deletes !inserts matches_tbl
  in
(*    
      BEGIN_DEBUG
      List.iter 
      (fun (n1, n2) -> 
      DEBUG_MSG "%a-%a" GI.ps n1#gindex GI.ps n2#gindex
      ) !matches
      END_DEBUG;
 *)
  !matches, extra_matches, !relabels
(* end of func fast_match_trees *)



let match_trees 
    cenv
    tree1
    tree2
    ?(root_check=true)
    ?(semantic=false)
    uidmapping
    ref_uidmapping
    =

  BEGIN_DEBUG
    DEBUG_MSG "|T1(root:%a)|=%d |T2(root:%a)|=%d"
      UID.ps tree1#root#uid tree1#size UID.ps tree2#root#uid tree2#size;
    DEBUG_MSG "T1:\n%s\n" tree1#to_string;
    DEBUG_MSG "T2:\n%s\n" tree2#to_string
  END_DEBUG;

  let eds, mapping, _ = (if semantic then sfind else find) tree1 tree2 in
  let deletes, inserts, relabels = Otreediff.Edit.seq_split eds in

  BEGIN_DEBUG
    (*DEBUG_MSG "eds (raw):\n%s" (Otreediff.Edit.seq_to_string eds);*)
    let to_s1 i = UID.to_string (tree1#get i)#uid in
    let to_s2 j = UID.to_string (tree2#get j)#uid in
    DEBUG_MSG "eds:\n%s" (Otreediff.Edit._seq_to_string to_s1 to_s2 eds);
    DEBUG_MSG "mapping:\n%s" (Otreediff.Mapping._to_string to_s1 to_s2 mapping)
  END_DEBUG;

    let contain_root = 
      Otreediff.Mapping.mem_elem tree1#root#index tree2#root#index mapping
    in

    let res =

      if contain_root || not root_check then begin

	let matches = 
	  Otreediff.Mapping.filter 
	    (fun i j -> 
	      not (List.mem (i, j) relabels)
	    ) mapping 
	in

	let matches = 
	  List.map (fun (i, j) -> tree1#get i, tree2#get j) matches
	in
	let matches_tbl = Hashtbl.create 0 in
	List.iter (fun (n1, n2) -> Hashtbl.add matches_tbl n1#uid n2#uid) matches;

	let relabels = 
	  List.map (fun (i, j) -> tree1#get i, tree2#get j) relabels
	in
	let deletes = List.map tree1#get deletes in
	let inserts = List.map tree2#get inserts in

	(* relabled nodes are regarded as deleted and then inserted nodes *)
	let rel1, rel2 = List.split relabels in

	(* get more matches! *)
	let extra_matches = 
	  find_glue_cands tree1 tree2 (deletes @ rel1) (inserts @ rel2) matches_tbl
	in

	DEBUG_MSG "extra_matches: [%s]"
	  (Xlist.to_string 
	     (fun (nd1, nd2) -> 
	       sprintf "%a-%a" UID.ps nd1#uid UID.ps nd2#uid) ";" extra_matches);


	(* check conflicts between relabels and extra_matches *)
	let bad_relabels = ref [] in
	let inv_relabels = List.map (fun (nd1, nd2) -> nd2, nd1) relabels in

	let context_tbl = Hashtbl.create 0 in (* (node * node) -> (node * node) list *)

	let get_adjacency_score nd1 nd2 = 
	  let s, ref_pairs = cenv#_get_adjacency_score nd1 nd2 in

	  if ref_pairs <> [] then
	    Hashtbl.add context_tbl (nd1, nd2) ref_pairs;

	  let ext = ref 0 in
	  tree1#fast_scan_whole_initial_subtree nd1
	    (fun nd -> 
	      try
		let u' = uidmapping#find_settled nd#uid in
		let n' = tree2#search_node_by_uid u' in
		if tree2#initial_subtree_mem nd2 n' then
		  incr ext
	      with 
		Not_found -> ()
	    );
	  let sz1 = tree1#whole_initial_subtree_size nd1 in
	  let sz2 = tree2#whole_initial_subtree_size nd2 in
	  s +. (float (!ext * 2)) /. (float (sz1 + sz2))
	in

	let ref_extra_matches = ref [] in

	let promoted_extra_matches = ref [] in
	
	let extra_matches_ =
	  List.filter
	    (fun (nd1, nd2) ->

	      let cands2, bad_rel2 = 
		try
		  let n2 = List.assq nd1 relabels in
		  if n2 == nd2 then
		    [|nd2|], []
		  else
		    [|n2; nd2|], [(nd1, n2)]
		with 
		  Not_found -> [|nd2|], []
	      in
	      let cands1, bad_rel1 = 
		try
		  let n1 = List.assq nd2 inv_relabels in
		  if n1 == nd1 then
		    [|nd1|], []
		  else
		    [|n1; nd1|], [(n1, nd2)]
		with 
		  Not_found -> [|nd1|], []
	      in

	      let sz1 = Array.length cands1 in
	      let sz2 = Array.length cands2 in

	      if sz1 = 1 && sz2 = 1 then (* no conflict *)
		true

	      else begin (* conflict with relabel(s) *)

		BEGIN_DEBUG
		let conflicts =
		  if sz1 = 1 && sz2 > 1 then
		    [nd1, cands2.(0)]
		  else if sz1 > 1 && sz2 = 1 then
		    [cands1.(0), nd2]
		  else 
		    [(nd1, cands2.(0)); (cands1.(0), nd2)]
		in
		DEBUG_MSG "extra_match %a-%a conflicts with relabel(s) %s"
		  UID.ps nd1#uid UID.ps nd2#uid 
		  (String.concat ", " 
		     (List.map 
			(fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) 
			conflicts))
		  END_DEBUG;

		let selected =
(*
  let w n1 n2 = get_adjacency_score n1 n2, cenv#eval_label_match n1 n2 in
  let cmpr = new SMP.ComparatorFloatInt.c w cands1 cands2 in
 *)
		  let cmpr = new SMP.ComparatorFloat.c get_adjacency_score cands1 cands2 in

		  SMP.get_stable_matches cmpr cands1 cands2
		in

		DEBUG_MSG "selected: %s"
		  (String.concat ", " 
		     (List.map 
			(fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) 
			selected));

		List.fold_left
		  (fun b (n1, n2) ->
		    if b then
		      true
		    else
		      if nd1 == n1 && nd2 == n2 then begin
			bad_relabels := bad_rel1 @ bad_rel2;
			begin
			  try
			    ref_extra_matches := 
			      (Hashtbl.find context_tbl (n1, n2)) @ !ref_extra_matches
			  with
			    Not_found -> ()
			end;
			promoted_extra_matches := (n1, n2) :: !promoted_extra_matches;
			false
		      end
		      else
			false

		  ) false selected
	      end

	    ) extra_matches
	in (* extra_matches_ *)


	let relabels_ = 
	  List.filter 
	    (fun (nd1, nd2) -> not (List.mem (nd1, nd2) !bad_relabels)) 
	    relabels
	in
	
	(* checking confliction of ref_extra_matches *)
	let more_extra_matches = ref [] in
	let inv_ref_extra_matches = 
	  List.map (fun (n1, n2) -> n2, n1) !ref_extra_matches 
	in
	let filt (nd1, nd2) =
	  let score = ref (-1.0) in
	  (try
	    let nd1' = List.assq nd1 !ref_extra_matches in
	    if nd1' == nd2 then
	      true
	    else begin
	      score := cenv#get_adjacency_score nd1 nd2;
	      let score' = cenv#get_adjacency_score nd1 nd1' in
	      let b = !score >= score' in
	      if not b then begin
		more_extra_matches := (nd1, nd1') :: !more_extra_matches;

		DEBUG_MSG "%a-%a vs %a-%a: score=%f < score'=%f" 
		  UID.ps nd1#uid UID.ps nd2#uid UID.ps nd1#uid UID.ps nd1'#uid !score score'

	      end;
	      b
	    end
	  with 
	    Not_found -> true) &&
	  (try
	    let nd2' = List.assq nd2 inv_ref_extra_matches in
	    if nd2' == nd1 then
	      true
	    else begin
	      if !score < 0.0 then
		score := cenv#get_adjacency_score nd1 nd2;
	      let score' = cenv#get_adjacency_score nd2' nd2 in
	      let b = !score >= score' in
	      if not b then begin
		more_extra_matches := (nd2', nd2) :: !more_extra_matches;

		DEBUG_MSG "%a-%a vs %a-%a: score=%f < score'=%f" 
		  UID.ps nd1#uid UID.ps nd2#uid UID.ps nd2'#uid UID.ps nd2#uid !score score'
	      end;
	      b
	    end
	  with 
	    Not_found -> true)
	in

	let _final_matches = List.filter filt matches in
	let final_extra_matches = List.filter filt extra_matches_ in
	let _final_relabels = List.filter filt relabels_ in


	let ex_mat, ex_rel = 
	  List.partition 
	    (fun (n1, n2) -> n1#data#eq n2#data) 
	    (!promoted_extra_matches @ !more_extra_matches)
	in

	let final_matches = _final_matches @ ex_mat in
	let final_relabels = _final_relabels @ ex_rel in

	BEGIN_DEBUG
	let to_str m =
	  Xlist.to_string 
	    (fun (n1, n2) -> 
	      sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid
	    ) ";" m
	in
	DEBUG_MSG "final matches: [%s]" (to_str final_matches);
	DEBUG_MSG "final extra matches: [%s]" (to_str final_extra_matches);
	DEBUG_MSG "final relabels: [%s]" (to_str final_relabels)
	  END_DEBUG;

	final_matches, final_extra_matches, final_relabels

      end
      else begin
	DEBUG_MSG "mapping does not contain roots!";
	DEBUG_MSG "using fast_match_trees...";
	fast_match_trees tree1 tree2 ref_uidmapping
      end
    in
    res
(* end of func match_trees *)
