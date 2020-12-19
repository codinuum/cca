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
(* postprocessing.ml *)

module UID = Otreediff.UID
module MID = Moveid
module GI  = Otreediff.GIndex
module B   = Binding
module BID = B.ID

let node_to_uid_string nd =
  Printf.sprintf "%a(%a)" UID.ps nd#uid GI.ps nd#gindex

let nodes_to_uids_string nds =
  String.concat ";" (List.map node_to_uid_string nds)

let nps () = node_to_uid_string
let nsps () = nodes_to_uids_string


module F (Label : Spec.LABEL_T) = struct

  exception Break
  exception Found

  type node_t = Spec.node_t
  type tree_t = Spec.tree_t

  let is_ghost_node = Triple.is_ghost_ast_node

  let getlab nd = (Obj.obj nd#data#_label : Label.t)

  let can_be_keyroot tree nd =
    let initial_ancestors = tree#initial_ancestor_nodes nd in

    DEBUG_MSG "initial ancestors of %a: [%s]" UID.ps nd#uid 
      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" initial_ancestors);

    let d = List.length initial_ancestors in
    let dcond = d >= Label.keyroot_depth_min in
    let prohibited = Label.cannot_be_keyroot nd in
    let b = dcond && not prohibited in

    DEBUG_MSG "%a -> %B (depth=%d,prohibited=%B)" 
      UID.ps nd#uid b d prohibited;
    b


  let printf = Printf.printf
  let sprintf = Printf.sprintf

  let mkinfo = Info.make


  let get_p_ancestor ?(moveon=fun _ -> true) pred nd =
    try
      let cur = ref nd#initial_parent in
      while moveon !cur && not (pred !cur) do
        cur := (!cur)#initial_parent
      done;

      if not (moveon !cur) then
        raise Not_found;

      !cur
    with
      Otreediff.Otree.Parent_not_found _ -> raise Not_found

  let pmap_add pmap u v = 
    try 
      let vs = Hashtbl.find pmap u in
      if not (List.mem v vs) then 
	Hashtbl.replace pmap u (v::vs)
    with Not_found -> Hashtbl.add pmap u [v]


  let left_to_right tree node1 node2 =
    Stdlib.compare node1#gindex node2#gindex


  let estimate_cost_of_move = Comparison.estimate_cost_of_move


  let detect_permutation mid_gen tree1 tree2 pruned edits pmap =
    let select_compatible_pairs tree1 tree2 pair_nth_weight_list =
      let pair_weight_list =
        List.map (fun (n1, n2, _, sz) -> (n1, n2, sz)) pair_nth_weight_list
      in
      let compat, incompat = UIDmapping.select_compatible_pairs tree1 tree2 pair_weight_list in

      List.iter
	(fun (n1, n2, _) ->
	  let u1, u2 = n1#uid, n2#uid in
	  pruned#set_kind u1 u2 Pruned.Migratory;
	  let info1, info2 = mkinfo n1, mkinfo n2 in
	  try
	    match edits#find_mov12 u1 u2 with
	    | Edit.Move (m, k, _, _) -> 

		if !k <> Edit.Mpermutation then begin

		  DEBUG_MSG "kind changed: %a: %s -> %s (select_compatible_pairs)"
		    MID.ps !m (Edit.move_kind_to_string !k) (Edit.move_kind_to_string Edit.Mpermutation);

		  k := Edit.Mpermutation
		end

	    | _ -> assert false
	  with
	    Not_found ->
	      edits#add_edit
		(Edit.make_move_permutation
		   (mid_gen#gen) (u1, info1) (u2, info2));

              DEBUG_MSG "added permutation %a -> %a" UID.ps u1 UID.ps u2
	) incompat;

      List.map (fun (n1, n2, sz) -> (n1, n2, ref 0, sz)) compat
    in

    Hashtbl.iter 
      (fun uid vs ->

	let vs = select_compatible_pairs tree1 tree2 vs in

	let len = List.length vs in
	if len > 1 then begin

	  let sorted1 =
	    List.stable_sort 
	      (fun (n1, _, _, _) (n2, _, _, _) -> 
		left_to_right tree1 n1 n2
	      ) vs
	  in
	  ignore (List.fold_left (fun c (_, _, nth, _) -> nth := c; c + 1) 0 sorted1);
	  let a1 = Array.make len 0 in
	  Array.iteri (fun i _ -> a1.(i) <- i) a1;

	  let sorted2 =
	    List.stable_sort 
	      (fun (_, n1, _, _) (_, n2, _, _) -> 
		left_to_right tree2 n1 n2) vs
	  in
	  let a2 = Array.make len 0 in
	  ignore (List.fold_left (fun c (_, _, nth, _) -> a2.(c) <- !nth; c + 1) 0 sorted2);


	  BEGIN_DEBUG
	    DEBUG_MSG "%a %s" UID.ps uid
	      (Xlist.to_string 
		 (fun (nd1, nd2, nth, sz) ->
		   sprintf "<%a,%a(%dth)(sz:%d)>" UID.ps nd1#uid UID.ps nd2#uid !nth sz)
		 "" (List.rev vs));
	    DEBUG_MSG "a1 = [%s]" 
	      (Xarray.to_string string_of_int " " a1);
	    DEBUG_MSG "a2 = [%s]" 
	      (Xarray.to_string string_of_int " " a2);
	  END_DEBUG;


	  let moved =
	    if a1 = a2 then 
	      []
	    else
	      let weight_list = List.map (fun (_, _, _, sz) -> sz) sorted1 in
(*	      
	      let get_weight _ j _ = 
		try 
		  List.nth weight_list a2.(j)
		with _ -> assert false 
	      in
	      let del =
		let res = HCS.Int.compute get_weight a1 a2 in
		res.HCS.Int.del
	      in
*)
	      let get_weight i j _ = 
		try 
		  (List.nth weight_list a2.(j), -i)
		with _ -> assert false 
	      in
	      let del =
		let res = HIS.IntInt.compute get_weight a1 a2 in
		let r1, _ = List.split res in
		List.filter (fun i -> not (List.mem i r1)) (Xlist.range len)
	      in
(*
	      if not (Xlist.subtract del del' = [] && Xlist.subtract del' del = []) then begin
		Xprint.message "a1=[%s]" (Xarray.to_string string_of_int "," a1);
		Xprint.message "a2=[%s]" (Xarray.to_string string_of_int "," a2);
		Xprint.message "cost: [%s]" (Xlist.to_string string_of_int "," weight_list);
		Xprint.message "del=[%s]" (Xlist.to_string string_of_int "," del);
		Xprint.message "del'=[%s]" (Xlist.to_string string_of_int "," del');
	      end;
*)
	      del

(*
	      let _, rel, del, _ = Adiff.adiff a1 a2 in
	      let rel1, _ = List.split rel in
	      rel1 @ del
*)
	  in

	  DEBUG_MSG "moved (cost considered): [%s]" (Xlist.to_string string_of_int ";" moved);

	  List.iter 
	    (fun (nd1, nd2, nth, sz) -> 
	      if List.mem !nth moved then begin
		let uid1, uid2 = nd1#uid, nd2#uid in
		let ok =
		  let eds = edits#find12 uid1 uid2 in
		  match eds with
		  | [] -> true
		  | [Edit.Move(m, kind, (u1, _, _), (u2, _, _)) as ed]
		  | [Edit.Move(m, kind, (u1, _, _), (u2, _, _)) as ed; 
		     Edit.Relabel _]
		  | [Edit.Relabel _;
		     Edit.Move(m, kind, (u1, _, _), (u2, _, _)) as ed] ->
		       assert (u1 = uid1 && u2 = uid2);
		       if !kind = Edit.Mpermutation then begin

			 DEBUG_MSG "already have %s" (Edit.to_string ed);

			 false
		       end
		       else if !kind = Edit.Modd then begin

			 DEBUG_MSG "already have %s" (Edit.to_string ed);

			 pruned#set_kind uid1 uid2 Pruned.Migratory;

			 DEBUG_MSG "kind changed: %a: %s -> %s (detect_permutation)"
			   MID.ps !m (Edit.move_kind_to_string !kind) (Edit.move_kind_to_string Edit.Mpermutation);

			 kind := Edit.Mpermutation;
			 false
		       end
		       else begin 
			 edits#remove_edit ed; 
			 true 
		       end
		  | [_] -> true
		  | _ -> assert false
		in
		if ok then begin
		  pruned#set_kind uid1 uid2 Pruned.Migratory;
		  let info1, info2 = mkinfo nd1, mkinfo nd2 in
		  edits#add_edit
		    (Edit.make_move_permutation
		       (mid_gen#gen) (uid1, info1) (uid2, info2));

		  DEBUG_MSG "added permutation %a -> %a" UID.ps uid1 UID.ps uid2
		end
	      end
	    ) vs
	end
      ) pmap
  (* end of func detect_permutation *)



  let is_pseudo_match nd1 nd2 =
    try
      nd1#data#_anonymized2_label = nd2#data#_anonymized2_label && 
      nd1#data#get_name = nd2#data#get_name
    with
      Not_found -> false


  let check_relabel options ?(exact=false) tree1 tree2 nd1 nd2 uidmapping =
    let parent_cond = 
      let p1 = nd1#has_initial_parent in
      let p2 = nd2#has_initial_parent in
      if p1 && p2 then
	let pnd1 = nd1#initial_parent in
	let pnd2 = nd2#initial_parent in
	let puid1 = pnd1#uid in
	let puid2 = pnd2#uid in

	try
	  let puid1' = uidmapping#find puid1 in
	  if puid1' = puid2 then
	    pnd1#data#eq pnd2#data || pnd1#data#relabel_allowed pnd2#data
	  else 
	    false
	with 
	  Not_found -> false
      else 
	if (not p1) && (not p2) then 
	  true
	else 
	  false
    in

    let children1 = 
      Array.to_list (Array.map (fun n -> n#uid) nd1#initial_children) 
    in
    let children2 = 
      Array.to_list (Array.map (fun n -> n#uid) nd2#initial_children)
    in
(*
    let pseudo_match = is_pseudo_match nd1 nd2 in

    let to_be_exact = exact && not pseudo_match in
*)
    let children_cond =
      let nchildren1 = List.length children1 in
      let nchildren2 = List.length children2 in

      if nchildren1 = 0 || nchildren2 = 0 then
	true

      else begin
	let sum_nchildren = nchildren1 + nchildren2 in
	if sum_nchildren = 0 then
(*	to_be_exact *) true

	else
	  let nmapped =
	    List.fold_left
	      (fun n u ->
		try
		  let u' = uidmapping#find u in
		  if List.memq u' children2 then
		    let nd1 = 
		      try
			tree1#search_node_by_uid u 
		      with Not_found -> assert false
		    in
		    let nd2 = 
		      try
			tree2#search_node_by_uid u' 
		      with Not_found -> assert false
		    in
		    if (* nd1#data#eq nd2#data *) nd1#data#_anonymized2_label = nd2#data#_anonymized2_label then 
		      n + 1
		    else 
		      n
		  else n 
		with Not_found -> n
	      ) 0 children1
	  in
	  (float (nmapped * 2)) /. (float sum_nchildren) >= options#pp_relabel_criteria
      end
    in

    let result = 
      if (* to_be_exact *) exact then
	parent_cond && children_cond 
      else
	parent_cond || children_cond 
    in

(*
    DEBUG_MSG "[exact=%B] %a-%a: pseudo_match:%B parent_cond:%B children_cond:%B --> %B"
      exact UID.ps nd1#uid UID.ps nd2#uid pseudo_match parent_cond children_cond result;
*)

    DEBUG_MSG "[exact=%B] %a-%a: parent_cond:%B children_cond:%B --> %B"
      exact UID.ps nd1#uid UID.ps nd2#uid parent_cond children_cond result;

    result
  (* end of func check_relabel *)


  let generate_moves options tree1 tree2 pruned edits uidmapping =

    let mid_gen = options#moveid_generator in

    BEGIN_DEBUG
      uidmapping#print_status;
    END_DEBUG;

    let extra_edits = new Edit.seq options in
    let pmap = Hashtbl.create 0 in

    let is_mapped1 = uidmapping#mem_dom in
    let is_mapped2 = uidmapping#mem_cod in


    let check au1 au2 =
      let an1 = tree1#search_node_by_uid au1 in
      let an2 = tree2#search_node_by_uid au2 in
(*
      let moderate_nchildren_threshold = options#moderate_nchildren_threshold in
      let moderate_nchildren = Misc.moderate_nchildren ~threshold:moderate_nchildren_threshold in

      if (moderate_nchildren an1) && (moderate_nchildren an2) then begin (* we ignore too many children *)
*)
	let c1 = 
	  Sourcecode.find_nearest_mapped_descendant_nodes is_mapped1 an1 
	in
	let c2 = 
	  Sourcecode.find_nearest_mapped_descendant_nodes is_mapped2 an2 
	in

	let c2' = ref [] in

	DEBUG_MSG "checking %a{%s} - %a{%s}"
	  UID.ps au1 (Xlist.to_string (fun n -> UID.to_string n#uid) ";" c1)
	  UID.ps au2 (Xlist.to_string (fun n -> UID.to_string n#uid) ";" c2);

	let maps =
	  Xlist.union 
	    (List.map (fun n1 -> n1, tree2#search_node_by_uid (uidmapping#find n1#uid)) c1)
	    (List.map (fun n2 -> tree1#search_node_by_uid (uidmapping#inv_find n2#uid), n2) c2)
	in
	let is_odd_map n1 n2 asz =

	  DEBUG_MSG "%a-%a: maps: [%s]" UID.ps n1#uid UID.ps n2#uid
	    (Xlist.to_string (fun (n1, n2) -> (UID.to_string n1#uid)^"-"^(UID.to_string n2#uid)) ";" maps);

	  let incompat = 
	    List.filter
	      (fun (n1', n2') ->
(*
		(UIDmapping._is_incompatible tree1 tree2 n1 n2 n1' n2') &&
*)
		(UIDmapping.is_crossing_or_incompatible tree1 tree2 n1 n2 n1' n2') &&

		n1' != n1 && n2' != n2
	      ) maps
	  in

	  DEBUG_MSG "incompatible maps: [%s]" 
	    (Xlist.to_string (fun (n1, n2) -> (UID.to_string n1#uid)^"-"^(UID.to_string n2#uid)) ";" incompat);

	  if incompat = [] then
	    false

	  else
	    let cost =
	      let incompat' = 
		List.map (fun (n1, n2) -> (n1, n2, estimate_cost_of_move tree1 tree2 uidmapping n1 n2)) incompat
	      in
	      let incompat'', _ = UIDmapping.select_compatible_pairs tree1 tree2 incompat' in
	      List.fold_left (fun sum (n1, n2, sz) -> sum + sz) 0 incompat''
	    in
	    
	    let b = asz <= cost in

	    BEGIN_DEBUG
	      DEBUG_MSG "%a-%a --> %B (asz=%d, cost=%d)" 
	        UID.ps n1#uid UID.ps n2#uid b asz cost;
	    END_DEBUG;

	    b
	in (* is_odd_map *)

	let chk u u' =
	  match extra_edits#find12 u u' with
	  | [] -> true
	  | [Edit.Move(_, s, _, _) as ed]
	  | [Edit.Relabel _;Edit.Move(_, s, _, _) as ed]
	  | [Edit.Move(_, s, _, _) as ed;Edit.Relabel _] -> 

	      DEBUG_MSG "already exists: %s" (Edit.to_string ed);
	      
	      false
	  | [_] -> true
	  | _ -> assert false
	in
        let add_move ?(odd=false) u u' nd nd' =
          let mkmv =
            if odd then
              Edit.make_move_odd
            else
              Edit.make_move
          in
          if chk u u' then
            let ed = mkmv (mid_gen#gen) (u, mkinfo nd) (u', mkinfo nd') in
            DEBUG_MSG "adding %s" (Editop.to_string ed);
            extra_edits#add_edit ed
        in
	List.iter 
	  (fun nd ->
	    let u = nd#uid in

	    DEBUG_MSG "nearest mapped descendant of %a ---> %a" UID.ps au1 UID.ps u;

	    let u' = uidmapping#find u in
	    let nd' = tree2#search_node_by_uid u' in


	    if List.memq nd' c2 then begin (* may be a permutation *)

	      let asz = estimate_cost_of_move tree1 tree2 uidmapping nd nd' in

	      if is_odd_map nd nd' asz (* EXPERIMENTAL!!! *) then begin (* odd move *)
                add_move ~odd:true u u' nd nd'
	      end;

	      c2' := nd' :: !c2';

	      DEBUG_MSG "may be a permutation: (%a) %a-%a"
		UID.ps au1 UID.ps u UID.ps u';
		
	      pmap_add pmap au1 (nd, nd', ref 0, asz);
	      DEBUG_MSG "map added"

	    end
	    else begin (* normal move *)
              add_move u u' nd nd'
	    end
	  ) c1;

	List.iter
	  (fun nd' ->
	    let u' = nd'#uid in
	    if not (List.memq nd' !c2') then begin

	      DEBUG_MSG "descendant of %a ---> %a" UID.ps au2 UID.ps u';

	      let u = uidmapping#inv_find u' in
	      let nd = tree1#search_node_by_uid u in

              add_move u u' nd nd'
	    end
	  ) c2

(*      end (* we ignore huge arrays *) *)

    in (* end of func check *)
    
(*    uidmapping#iter_unsettled check; *)
    uidmapping#iter check;

    let c = extra_edits#get_nedits in

    BEGIN_DEBUG
      DEBUG_MSG "%d edits generated" c;
      extra_edits#iter
        (fun ed -> DEBUG_MSG "%s" (Editop.to_string ed));
      DEBUG_MSG "detecting permutations...";
    END_DEBUG;

    detect_permutation mid_gen tree1 tree2 pruned extra_edits pmap;

    DEBUG_MSG "%d edits generated (permutatioin)" (extra_edits#get_nedits - c);

    (*if options#no_moves_flag then begin
      extra_edits#iter_moves
        (function
          | Edit.Move(_, _, (u1, inf1, ex1), (u2, inf2, ex2)) as mov -> begin
	      let del = Edit.Delete(false, u1, inf1, ex1) in
              let ins = Edit.Insert(false, u2, inf2, ex2) in
              uidmapping#remove u1 u2;
              List.iter
                (fun ed ->
                  DEBUG_MSG "adding %s" (Editop.to_string ed);
                  edits#add_edit ed
                ) [del; ins];
	      try
                match edits#find_rel12 u1 u2 with
      	        | Edit.Relabel _ as rel -> edits#remove_edit rel;
                | _ -> assert false
              with
                Not_found -> ()
          end
          | _ -> assert false
        )
    end
    else*)
      edits#add_edits extra_edits
  (* end of func generate_moves *)


  let make_move_region_tbl tree1 tree2 edits =
    let mtbl = Hashtbl.create 0 in (* mid -> (leftmost gi * top gi * leftmost gi * top gi) *)
    edits#iter_moves
      (function 
	| Edit.Move(mid, _, (_, info1, _), (_, info2, _)) -> begin
	    let nd1 = Info.get_node info1 in
	    let nd2 = Info.get_node info2 in
	    let gi1 = nd1#gindex in
	    let gi2 = nd2#gindex in
	    let lgi1 = (tree1#initial_leftmost nd1)#gindex in
	    let lgi2 = (tree2#initial_leftmost nd2)#gindex in
	    try
	      let lg1, g1, lg2, g2 = Hashtbl.find mtbl !mid in
	      if gi1 > g1 && gi2 > g2 then
		Hashtbl.replace mtbl !mid (lgi1, gi1, lgi2, gi2)
	    with
	      Not_found -> 
		Hashtbl.add mtbl !mid (lgi1, gi1, lgi2, gi2)
	end
	| _ -> assert false
      );
    BEGIN_DEBUG
      Hashtbl.iter
	(fun m (lmg1, g1, lmg2, g2) ->
          let n1 = tree1#search_node_by_gindex g1 in
          let n2 = tree2#search_node_by_gindex g2 in
	  DEBUG_MSG "move region: %a -> (%a,%a)-(%a,%a) (%a-%a)" MID.ps m
	    GI.ps lmg1 GI.ps g1 GI.ps lmg2 GI.ps g2
            UID.ps n1#uid UID.ps n2#uid
	) mtbl;
    END_DEBUG;

    mtbl

  let make_parent_move_tbl move_region_tbl edits =
    let parent_move_tbl = Hashtbl.create 0 in
    edits#iter_moves
      (function 
	| Edit.Move(mid, _, (_, info1, _), (_, info2, _)) ->
	    let nd1 = Info.get_node info1 in
	    let nd2 = Info.get_node info2 in
	    let gi1 = nd1#gindex in
	    let gi2 = nd2#gindex in
	    Hashtbl.iter
	      (fun m (lmg1, g1, lmg2, g2) ->
		if 
		  lmg1 <= gi1 && gi1 < g1 && 
		  lmg2 <= gi2 && gi2 < g2 &&
		  !mid <> m
		then begin
		  DEBUG_MSG "parent move of %a --> %a" MID.ps !mid MID.ps m;
		  begin
		    try
		      let (pm, pg1, pg2) = Hashtbl.find parent_move_tbl !mid in
		      if pm <> m && g1 <= pg1 (* && g2 <= pg2 *) then
			Hashtbl.replace parent_move_tbl !mid (m, g1, g2)
		    with
		      Not_found -> Hashtbl.add parent_move_tbl !mid (m, g1, g2)
		  end
		end
	      ) move_region_tbl

	| _ -> assert false
      );
    parent_move_tbl

  let make_child_move_tbl parent_move_tbl =
    let tbl = Hashtbl.create 0 in
    Hashtbl.iter
      (fun mid (parent_mid, _, _) ->
	try
	  let ms = Hashtbl.find tbl parent_mid in
	  if not (List.mem mid ms) then
	    Hashtbl.replace tbl parent_mid (mid::ms)
	with
	  Not_found -> Hashtbl.add tbl parent_mid [mid]
      ) parent_move_tbl;
    tbl

  let scan_ancestors ?(moveon=fun x -> true) nd f =
    try
      let cur = ref nd in
      while (moveon !cur) do
        cur := (!cur)#initial_parent;
        f !cur
      done
    with
      Otreediff.Otree.Parent_not_found _ -> ()

  (* grouping moves *)
  let group_moves options tree1 tree2 edits uidmapping =

    DEBUG_MSG "* GROUPING MOVES:";

    let mid_gen() =
      let mid = options#moveid_generator#gen in
      DEBUG_MSG "%a" MID.ps mid;
      mid
    in

    let mid_chg_tbl = Hashtbl.create 0 in

    let get_mid n =
      try
        match edits#find_mov1 n#uid with
        | Edit.Move(id, _, _, _) -> Some !id
        | _ -> None
      with
        Not_found -> None
    in

    let rec gen_moves movid kind nd =
      let uid = nd#uid in

      DEBUG_MSG "(%a, %s): scanning (%a)%s..."
	MID.ps movid (Edit.move_kind_to_string kind) UID.ps uid nd#data#to_string;

      let movid, kind =
        let moveon x =
          match get_mid x with
          | Some id -> id = movid
          | _ -> true
        in
        try
          scan_ancestors ~moveon nd
            (fun a ->
              DEBUG_MSG "a=%a" UID.ps a#uid;
              match get_mid a with
              | Some id when id = movid ->
                  DEBUG_MSG "found: %a" MID.ps id;
                  raise Exit
              | _ -> ()
            );
          let nm = edits#get_nmoves_of_move_id movid in
          DEBUG_MSG "num moved nodes of %a: %d" MID.ps movid nm;
          if nm > 0 then begin
            let movid' = mid_gen() in
            DEBUG_MSG "%a -> %a" MID.ps movid MID.ps movid';
            movid', Edit.Mnormal
          end
          else
            movid, kind
        with
          Exit -> movid, kind
      in

      let moveon = ref true in

      let chk eds =
	match eds#find1 uid with
	| [] -> 
	    DEBUG_MSG "edit not found: %a" UID.ps uid;
	    true

	| [Edit.Move(id, k, _, _)]
	| [Edit.Move(id, k, _, _); Edit.Relabel _] 
	| [Edit.Relabel _;Edit.Move(id, k, _, _)] -> 
	    moveon := !id = movid;

	    if not !moveon then begin
              DEBUG_MSG "found: another move (%a)" MID.ps !id;
	      if !k = Edit.Modd then begin

		DEBUG_MSG "%a --> %a" MID.ps !id MID.ps movid;

		Hashtbl.replace mid_chg_tbl !id movid;

		moveon := true
	      end
            end;

	    false

	| [Edit.Relabel _] -> 
	    DEBUG_MSG "found: relabel";
	    true

	| [ed] ->
	    DEBUG_MSG "found: another edit: %s" (Edit.to_string ed);
	    false

	| eds -> 
	    DEBUG_MSG "%d conflicting edits found:\n%s"
              (List.length eds) (Xlist.to_string Edit.to_string "\n" eds);
	    assert false

      in (* end of func chk *)

      let addmov nd1 =
	let uid1 = nd1#uid in
	try
	  let uid2 = uidmapping#find uid1 in
	  let nd2 = tree2#search_node_by_uid uid2 in

          DEBUG_MSG "adding: (%a)%s - (%a)%s"
	    UID.ps uid1 nd1#data#to_string UID.ps uid2 nd2#data#to_string;

	  try
	    match edits#find_mov12 uid1 uid2 with
	    | Edit.Move(id, _, (u1, _, _), (u2, _, _)) ->
		if uid1 = u1 && uid2 = u2 then begin
		  DEBUG_MSG "%a --> %a" MID.ps !id MID.ps movid;
		  id := movid
		end
		else
		  assert false
	    | _ -> assert false
	  with
	    Not_found -> 
	      let info1, info2 = mkinfo nd1, mkinfo nd2 in
	      edits#add_edit
		(Edit._make_move movid kind (uid1, info1) (uid2, info2))
	with 
	  Not_found ->
            DEBUG_MSG "not mapped";
            edits#add_edit (Edit.make_delete nd1)
      in

      if chk edits then begin (* mapped (have no edit other than relabel) *)
	  addmov nd
      end;

      if !moveon then begin
	let is_frontier =
          try
            let uid' = uidmapping#find nd#uid in
            let nd' = tree2#search_node_by_uid uid' in

	    let cus = nd#initial_children_uids in
	    let cus' = nd'#initial_children_uids in

	    List.exists 
	      (fun u -> 
	        try
                  let u' = uidmapping#find u in
		  let b = not (List.memq u' cus') in
                  DEBUG_MSG "%a -> %a (%B)" UID.ps u UID.ps u' b;
                  b
	        with 
                  Not_found -> false
	      ) cus
	  || 
	    List.exists 
	      (fun u' ->
	        try
                  let u = uidmapping#inv_find u' in
		  let b = not (List.memq u cus) in
                  DEBUG_MSG "%a <- %a (%B)" UID.ps u UID.ps u' b;
                  b
	        with 
                  Not_found -> false
	      ) cus'
          with
            Not_found -> false
        in
        DEBUG_MSG "is_frontier --> %B" is_frontier;
        let genmov =
          if is_frontier then (* cf. regression:java/Tar.java: 384L-397L --> 459L-472L *)
            fun n -> gen_moves (mid_gen()) Edit.Mnormal n
          else
            gen_moves movid kind
        in
        Array.iter genmov nd#initial_children

      end

    in (* end of func gen_moves *)

    edits#iter_moves_bottomup
      (function 
	| Edit.Move(mid, kind, (_, info1, _), _) as ed ->

	    DEBUG_MSG "checking move: %s" (Edit.to_string ed);

	    let nd1 = Info.get_node info1 in

	    Array.iter (gen_moves !mid !kind) nd1#initial_children

	| _ -> assert false
      );

    mid_chg_tbl

  (* end of func group_moves *)


  let find_keyroots 
      options
      ?(relax=false)
      ?(ignore_sequence=false)
      ?(filt=fun u1 u2 -> true)
      (tree1 : tree_t) 
      (tree2 : tree_t) 
      (uidmapping : node_t UIDmapping.c) 
      =
    let cands = ref [] in

    uidmapping#iter_unsettled
      (fun uid1 uid2 ->

        if filt uid1 uid2 then begin

	  DEBUG_MSG "checking %a-%a" UID.ps uid1 UID.ps uid2;

	  let nd1 = tree1#search_node_by_uid uid1 in
	  let nd2 = tree2#search_node_by_uid uid2 in


          let moveon =
            (if ignore_sequence then
              not nd1#data#is_sequence
            else
              true
            ) 
              &&
            if relax then
              nd1#data#_anonymized_label = nd2#data#_anonymized_label
            else
              nd1#data#eq nd2#data
          in

	  if moveon then begin

	    try
	      if can_be_keyroot tree1 nd1 && can_be_keyroot tree2 nd2 then begin

	        let chn1 = nd1#initial_children_uids in
	        let chn2 = nd2#initial_children_uids in

                DEBUG_MSG "chn1: [%s]" (Xlist.to_string UID.to_string ";" chn1);
                DEBUG_MSG "chn2: [%s]" (Xlist.to_string UID.to_string ";" chn2);

	        let cond =
		  List.exists 
		    (fun u -> 
		      try
                        let u' = uidmapping#find u in
                        DEBUG_MSG "%a -> %a" UID.ps u UID.ps u';
		        not (List.memq u' chn2) 
		      with 
                        Not_found -> true
		    ) chn1
	        || 
		  List.exists 
		    (fun u ->
		      try
                        let u' = uidmapping#inv_find u in
                        DEBUG_MSG "%a <- %a" UID.ps u' UID.ps u;
		        not (List.memq u' chn1)
		      with 
                        Not_found -> true
		    ) chn2
	        in
	        if cond then begin
		  DEBUG_MSG " -> candidate";
		  cands := (nd1, nd2)::!cands;
	        end
              end
	    with 
              Not_found -> ()
	  end
        end
      );

    BEGIN_DEBUG
      List.iter 
	(fun (n1, n2) -> 
	  DEBUG_MSG "cands: %a(size=%d) - %a(size=%d)"
	    UID.ps n1#uid (tree1#whole_initial_subtree_size n1)
	    UID.ps n2#uid (tree2#whole_initial_subtree_size n2)
	) !cands;
    END_DEBUG;

    let cands_large, cands_moderate = 
      List.partition 
	(fun (n1, n2) -> 
	  tree1#whole_initial_subtree_size n1 > options#match_algo_threshold ||
	  tree2#whole_initial_subtree_size n2 > options#match_algo_threshold
         ) !cands
    in

    let finalize cnds =
      List.filter
	(fun (nd1, nd2) ->
	  let b =
	    not
	      (
               List.exists
                 (fun (n1, n2) ->
                   let x =
                     tree1#is_initial_ancestor n1 nd1 && tree2#is_initial_ancestor n2 nd2
                   in
                   if x then
		     DEBUG_MSG "%a-%a is contained in %a-%a" 
		       UID.ps nd1#uid UID.ps nd2#uid
		       UID.ps n1#uid UID.ps n2#uid;
                   x
                 ) cnds 
	      )
	  in
	  b
	) cnds
    in

    let fin_cands_large = finalize !cands (* cands_large *) in
    let fin_cands_moderate = finalize cands_moderate in

    BEGIN_DEBUG
      List.iter 
	(fun (n1, n2) -> 
	  DEBUG_MSG "large: %a(size=%d) - %a(size=%d)" 
	    UID.ps n1#uid (tree1#whole_initial_subtree_size n1)
	    UID.ps n2#uid (tree2#whole_initial_subtree_size n2);
	) fin_cands_large;
      List.iter 
	(fun (n1, n2) -> 
	  DEBUG_MSG "moderate: %a(size=%d) - %a(size=%d)"
	    UID.ps n1#uid (tree1#whole_initial_subtree_size n1)
	    UID.ps n2#uid (tree2#whole_initial_subtree_size n2)
	) fin_cands_moderate;
    END_DEBUG;

    fin_cands_large, fin_cands_moderate
  (* end of func find_keyroots *)


  let is_odd_relabel ?(exact=false)
      (tree1 : tree_t) 
      (tree2 : tree_t) 
      (uidmapping : node_t UIDmapping.c)
      nd1 nd2
      =
    
    (* check ancestors *)
    let is_odd_anc =
      try
        let an1 = Sourcecode.find_nearest_mapped_ancestor_node uidmapping#mem_dom nd1 in
        let an2 = Sourcecode.find_nearest_mapped_ancestor_node uidmapping#mem_cod nd2 in
        let au1, au2 = an1#uid, an2#uid in
        let au1' = uidmapping#find au1 in
        let au2' = uidmapping#inv_find au2 in
        au1' <> au2 || au2' <> au1
      with
        _ -> false
    in


    (* check descendants *)
    let ds1 = Sourcecode.find_nearest_mapped_descendant_nodes uidmapping#mem_dom nd1 in
    let ds2 = Sourcecode.find_nearest_mapped_descendant_nodes uidmapping#mem_cod nd2 in

    let exists_in_subtree nd tree rt =
      try
        tree#fast_scan_whole_initial_subtree rt
          (fun n -> 
            if nd#data#eq n#data then
              raise Exit
          );
        false
      with
        Exit -> true
    in

    let uid1, uid2 = nd1#uid, nd2#uid in

    let is_odd_desc =
      (List.exists
	 (fun dn ->
	   let du = dn#uid in
	   let du' = uidmapping#find du in

	   DEBUG_MSG "\tmapped descendant of %a: %a (-> %a)" UID.ps uid1 UID.ps du UID.ps du';

	   let dn' = tree2#search_node_by_uid du' in
           let dn_exists_in_nd2tree = exists_in_subtree dn tree2 nd2 in

	   if (not (tree2#initial_subtree_mem nd2 dn')) && dn#data#eq dn'#data && not dn_exists_in_nd2tree then begin

	     DEBUG_MSG "\todd mapping: %a-%a" UID.ps uid1 UID.ps uid2;

	     true
	   end
	   else
	     false
	 ) ds1) 
    ||
      (List.exists
	 (fun dn -> 
	   let du = dn#uid in
	   let du' = uidmapping#inv_find du in

	   DEBUG_MSG "\tmapped descendant of %a: %a (-> %a)" UID.ps uid2 UID.ps du UID.ps du';

	   let dn' = tree1#search_node_by_uid du' in
           let dn_exists_in_nd1tree = exists_in_subtree dn tree1 nd1 in

	   if (not (tree1#initial_subtree_mem nd1 dn')) && dn#data#eq dn'#data && not dn_exists_in_nd1tree then begin

	     DEBUG_MSG "\todd mapping: %a-%a" UID.ps uid1 UID.ps uid2;

	     true
	   end
	   else
	     false
	 ) ds2) 
    in

    let allowed = nd1#data#relabel_allowed nd2#data in

    DEBUG_MSG "allowed:%B" allowed;

(*
    let not_absurd = 
      allowed && (check_relabel options ~exact tree1 tree2 nd1 nd2 uidmapping)
    in

    DEBUG_MSG "not_absurd=%B" not_absurd;

    let is_odd1 = not not_absurd in
*)
(*    is_odd_desc || is_odd1 *)
    (not allowed) || is_odd_desc || is_odd_anc

  (* end of func is_odd_relabel *)


(*
  let estimate_subtree_similarity tree1 tree2 nd1 nd2 =
    let getsz t n = t#whole_initial_subtree_size n in
    let t1 = new Tree.c nd1 false in
    let t2 = new Tree.c nd2 false in
    let m, em, r = Treediff.fast_match_trees t1 t2 in
    let nmapped = 
      List.fold_left
	(fun s (n1, n2) ->
	  s +
	  (if n1#is_leaf then getsz tree1 n1 else 1) +
	  (if n2#is_leaf then getsz tree2 n2 else 1)
	) 0 (m @ em @ r)
    in
    (float (nmapped + nmapped)) /. (float ((getsz tree1 nd1) + (getsz tree2 nd2)))
*)

  let eliminate_enclaves
      options
      (cenv : (node_t, tree_t) Comparison.c)
      keyroots
      (tree1 : tree_t)
      (tree2 : tree_t)
      (uidmapping : node_t UIDmapping.c)
      (ref_uidmapping : node_t UIDmapping.c)
      =
    DEBUG_MSG "** ELIMINATING ENCLAVES...\n";

    let new_pairs1 = Hashtbl.create 0 in
    let new_pairs2 = Hashtbl.create 0 in

    let new_pairs_add (uid1, uid2) =
      DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2;
      try
	let u2 = Hashtbl.find new_pairs1 uid1 in
	if u2 = uid2 then 
	  ()
	else
          let nd1 = tree1#search_node_by_uid uid1 in
          let nd2 = tree2#search_node_by_uid uid2 in
          let n2 = tree2#search_node_by_uid u2 in
          if nd1#data#eq nd2#data && not (nd1#data#eq n2#data) then begin
            Hashtbl.remove new_pairs1 uid1;
            raise Not_found
          end
          else
	    DEBUG_MSG "! conflict with %a-%a, not added!" UID.ps uid1 UID.ps u2;
      with 
	Not_found ->
	  try
	    let u1 = Hashtbl.find new_pairs2 uid2 in
	    if u1 = uid1 then 
	      ()
	    else
              let nd1 = tree1#search_node_by_uid uid1 in
              let nd2 = tree2#search_node_by_uid uid2 in
              let n1 = tree1#search_node_by_uid u1 in
              if nd1#data#eq nd2#data && not (nd2#data#eq n1#data) then begin
                Hashtbl.remove new_pairs2 uid2;
                raise Not_found
              end
              else
	      DEBUG_MSG "! conflict with %a-%a, not added!" UID.ps u1 UID.ps uid2;
	  with 
	    Not_found ->
	      Hashtbl.add new_pairs1 uid1 uid2;
	      Hashtbl.add new_pairs2 uid2 uid1
    in
(*
    let new_pairs_remove (uid1, uid2) =
      DEBUG_MSG "removing %a-%a" UID.ps uid1 UID.ps uid2;
      Hashtbl.remove new_pairs1 uid1;
      Hashtbl.remove new_pairs2 uid2;
    in
*)

    let new_pairs_mem1 uid1 = Hashtbl.mem new_pairs1 uid1 in
    let new_pairs_mem2 uid2 = Hashtbl.mem new_pairs2 uid2 in

    let add_enclave uid1 uid2 =
      DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2;

      if not (uidmapping#is_locked_uid uid1 || uidmapping#is_locked_uid uid2) then
	if 
	  uidmapping#mem_settled uid1 || 
	  uidmapping#mem_cod_settled uid2 
	then (* impossible? *)
	  DEBUG_MSG "settled mapping exists, aborted"
	else 
	  new_pairs_add (uid1, uid2)
      else
	DEBUG_MSG "locked pair: %a-%a" UID.ps uid1 UID.ps uid2
    in


    let find_enclaves nd1 nd2 = 
      BEGIN_DEBUG
        let uid1, uid2 = nd1#uid, nd2#uid in
        DEBUG_MSG "finding enclaves of %a-%a (%a-%a)" 
	  UID.ps uid1 UID.ps uid2 GI.ps nd1#gindex GI.ps nd2#gindex;
      END_DEBUG;

      let otree = tree1#make_subtree_from_node nd1 in
      let ntree = tree2#make_subtree_from_node nd2 in
      nd1#hide_parent; 
      nd2#hide_parent;

(*
      BEGIN_DEBUG
	DEBUG_MSG "|T1(root:%a)|=%d |T2(root:%a)|=%d" 
	  UID.ps uid1 otree#size UID.ps uid2 ntree#size;

	DEBUG_MSG "T1:\n%s" otree#to_string;
	DEBUG_MSG "T2:\n%s" ntree#to_string;
      END_DEBUG;
*)
      if otree#size > 1 &&  ntree#size > 1 then begin

	let matches, extra_matches, relabels =
	  if 
	    otree#size > options#match_algo_threshold ||
	    ntree#size > options#match_algo_threshold
	  then 
	    Treediff.fast_match_trees otree ntree ref_uidmapping
	  else 
	    Treediff.match_trees cenv otree ntree uidmapping ref_uidmapping
	in

	let matches_ = matches in
(*
  let matches_ = (* does not override mapping because matches are not so reliable, especially in case of permutation *)
  List.filter
  (fun (nd1, nd2) ->
  let u1, u2 = nd1#uid, nd2#uid in
  not (
  (uidmapping#mem_dom u1 && not (uidmapping#mem_dom_weak u1)) || 
  (uidmapping#mem_cod u2 && not (uidmapping#mem_cod_weak u2)))
  ) matches
  in
 *)

	BEGIN_DEBUG
	  let cmp (n1, _) (n2, _) = Stdlib.compare n1#gindex n2#gindex in
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "match (filtered): %a-%a (%a-%a)" 
		UID.ps n1#uid UID.ps n2#uid GI.ps n1#gindex GI.ps n2#gindex
	    ) matches_;
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "match (filtered:gindex): %a-%a" 
		GI.ps n1#gindex GI.ps n2#gindex
	    ) (List.fast_sort cmp matches_);
	END_DEBUG;

	let extra_matches_ = (* does not override _strict_ mapping because extra matches are not so reliable *)
	  List.filter
	    (fun (nd1, nd2) ->
	      let conflict1 =
		try
		  let u2 = uidmapping#find nd1#uid in
		  let n2 = tree2#search_node_by_uid u2 in
		  nd1#data#eq n2#data
		with 
                  Not_found -> false
	      in
	      let conflict2 =
		try
		  let u1 = uidmapping#inv_find nd2#uid in
		  let n1 = tree1#search_node_by_uid u1 in
		  nd2#data#eq n1#data
		with 
                  Not_found -> false
	      in
	      not (conflict1 || conflict2)
	    ) extra_matches
	in

	BEGIN_DEBUG
	  let cmp (n1, _) (n2, _) = Stdlib.compare n1#gindex n2#gindex in
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "extra_match (filtered): %a-%a (%a-%a)" 
		UID.ps n1#uid UID.ps n2#uid GI.ps n1#gindex GI.ps n2#gindex
	    ) extra_matches_;
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "extra_match (filtered:gindex): %a-%a" GI.ps n1#gindex GI.ps n2#gindex
	    ) (List.fast_sort cmp extra_matches_);
	END_DEBUG;

	let temp_uidmapping = new UIDmapping.c cenv in
	ignore (temp_uidmapping#merge uidmapping);

	let to_be_removed = ref [] in

	let matches_and_extra_matches = matches_ @ extra_matches_ in

	List.iter
	  (fun (nd1, nd2) ->
	    let uid1, uid2 = nd1#uid, nd2#uid in
	    let score = ref (-1.0) in
	    let cond1 =
	      try
		let uid1' = temp_uidmapping#find uid1 in
		if uid1' <> uid2 then
		  let nd1' = tree2#search_node_by_uid uid1' in
		  score := cenv#get_adjacency_score nd1 nd2;
		  let score' = cenv#get_adjacency_score nd1 nd1' in
		  if !score >= score' then begin
		    to_be_removed := (nd1, nd1') :: !to_be_removed;
		    true
		  end
		  else begin
		    to_be_removed := (nd1, nd2) :: !to_be_removed;
		    false
		  end
		else
		  false
	      with
		Not_found -> true
	    in
	    let cond2 =
	      try
		let uid2' = temp_uidmapping#inv_find uid2 in
		if uid2' <> uid1 then
		  let nd2' = tree1#search_node_by_uid uid2' in
		  if !score < 0.0 then
		    score := cenv#get_adjacency_score nd1 nd2;
		  let score' = cenv#get_adjacency_score nd2' nd2 in
		  if !score >= score' then begin
		    to_be_removed := (nd2', nd2) :: !to_be_removed;
		    true
		  end
		  else begin
		    to_be_removed := (nd1, nd2) :: !to_be_removed;
		    false
		  end
		else
		  false
	      with
		Not_found -> true
	    in

	    if cond1 && cond2 then
	      ignore (temp_uidmapping#add_unsettled nd1#uid nd2#uid)

	  ) matches_and_extra_matches;


	let matches_and_extra_matches_ =
	  List.filter (fun (n1, n2) -> not (List.mem (n1, n2) !to_be_removed)) matches_and_extra_matches
	in

	BEGIN_DEBUG
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "matches_and_extra_matches_: %a-%a (%a-%a)" 
		UID.ps n1#uid UID.ps n2#uid GI.ps n1#gindex GI.ps n2#gindex
	    ) matches_and_extra_matches_;
	END_DEBUG;

	let relabels_checked = 
	  List.filter 
	    (fun (nd1, nd2) -> not (is_odd_relabel ~exact:true tree1 tree2 temp_uidmapping nd1 nd2))
	    relabels 
	in

	let relabels_ = (* does not override mapping because relabels are not so reliable *)
	  List.filter
	    (fun (nd1, nd2) ->
	      DEBUG_MSG "filtering relabel: %a-%a" UID.ps nd1#uid UID.ps nd2#uid;

              (*(nd1#data#is_named_orig && nd2#data#is_named_orig && nd1#data#get_name = nd2#data#get_name) ||*)

	      let uid1, uid2 = nd1#uid, nd2#uid in
	      let conflict1 = uidmapping#mem_dom uid1 && uidmapping#find uid1 <> uid2 in
	      let conflict2 = uidmapping#mem_cod uid2 && uidmapping#inv_find uid2 <> uid1 in

	      DEBUG_MSG "conflict1:%B conflict2:%B" conflict1 conflict2;

	      let ncross = ref (-1) in
	      let adj = ref (-1.0) in

              let cond0 =
                if conflict1 || conflict2 then

                  let ok1, dnc1, remover1 =
                    if conflict1 then
                      try
                        let u1 = nd1#uid in
                        let u1' = uidmapping#find u1 in

                        DEBUG_MSG "relabel: %a-%a conflicts with %a-%a"
                          UID.ps u1 UID.ps nd2#uid UID.ps u1 UID.ps u1';

                        if uidmapping#is_stable_pair u1 u1' then begin
                          DEBUG_MSG "relabel: %a-%a: stable" UID.ps u1 UID.ps u1';
                          false, None, (fun () -> ())
                        end
                        else
                          let nd1' = tree2#search_node_by_uid u1' in
                          let b = ref false in
                          let dnc = ref None in
                          cenv#compare_mappings uidmapping
                            nd1 nd1' (fun d -> dnc := d)
                            nd1 nd2 ~ncrossing_new:ncross ~adjacency_new:adj
                            (fun d -> b := true; dnc := d);
                          !b, !dnc, (fun () -> uidmapping#remove u1 u1')
                      with
                        Not_found -> assert false
                    else
                      true, None, (fun () -> ())
                  in
                  let ok2, dnc2, remover2 =
                    if conflict2 then
                      try
                        let u2 = nd2#uid in
                        let u2' = uidmapping#inv_find u2 in
                        DEBUG_MSG "relabel: %a-%a conflicts with %a-%a"
                          UID.ps nd1#uid UID.ps u2 UID.ps u2' UID.ps u2;
	                if uidmapping#is_stable_pair u2' u2 then begin
                          DEBUG_MSG "relabel: %a-%a: stable" UID.ps u2' UID.ps u2;
                          false, None, (fun () -> ())
                        end
                        else
                          let nd2' = tree1#search_node_by_uid u2' in
                          let b = ref false in
                          let dnc = ref None in
                          cenv#compare_mappings uidmapping
                            nd2' nd2 (fun d -> dnc := d)
                            nd1 nd2 ~ncrossing_new:ncross ~adjacency_new:adj
                            (fun d -> b := true; dnc := d);
                          !b, !dnc, (fun () -> uidmapping#remove u2' u2)
                      with
                        Not_found -> assert false
                    else
                      true, None, (fun () -> ())
                  in
                  let b =
                    (ok1 && ok2) (* ||
                                    (ok1 && not ok2 && match dnc1, dnc2 with Some d1, Some d2 -> d1 > d2 | _ -> false) ||
                                    (not ok1 && ok2 && match dnc1, dnc2 with Some d1, Some d2 -> d1 < d2 | _ -> false) *)
                  in
                  if b then begin
                    remover1();
                    remover2()
                  end;
                  b

                else
                  true
              in (* cond0 *)
              DEBUG_MSG "cond0=%B" cond0;

              cond0 &&
              try
                let pnd1 = nd1#initial_parent in
                let pnd2 = nd2#initial_parent in
                try
                  let puid1' = uidmapping#find pnd1#uid in

                  DEBUG_MSG "parent uidmapping: %a -> %a" UID.ps pnd1#uid UID.ps puid1';

                  puid1' = pnd2#uid

                with Not_found ->
                  let parent_cond = (List.assq pnd1 (relabels_checked @ matches_and_extra_matches_)) == pnd2 in
                  DEBUG_MSG "parent_cond=%B" parent_cond;

(*
		    let n_nodes_of_same_cat nodes nd =
		      let alab = nd#data#_anonymized_label in
		      List.fold_left 
			(fun count al -> 
			  if alab = al then
			    count + 1
			  else
			    count
			) 0 (List.map (fun n -> n#data#_anonymized_label) nodes)
		    in
		    let n_nodes_of_same_cat1 = 
		      n_nodes_of_same_cat (Array.to_list pnd1#initial_children) nd1
		    in
		    let n_nodes_of_same_cat2 =
		      n_nodes_of_same_cat (Array.to_list pnd2#initial_children) nd2
		    in
		    let cat_cond =
		      n_nodes_of_same_cat1 = 1 && n_nodes_of_same_cat2 = 1
		    in

		    BEGIN_DEBUG
		      DEBUG_MSG "relabel: %a-%a (parent: %a-%a, num of children of same category not matched: %d-%d)" 
			UID.ps nd1#uid UID.ps nd2#uid
			UID.ps pnd1#uid UID.ps pnd2#uid 
			n_nodes_of_same_cat1 n_nodes_of_same_cat2;
		      DEBUG_MSG "parent_cond:%B cat_cond:%B --> filtered:%B" parent_cond cat_cond (not cond);
		    END_DEBUG;

*)
                    let n_nodes_of_same_cat_not_matched =
                      let cs1 = Array.to_list pnd1#initial_children in
                      let cs2 = Array.to_list pnd2#initial_children in
                      let alab1 = nd1#data#_anonymized_label in
                      let alab2 = nd2#data#_anonymized_label in
                      let filt alab n = n#data#_anonymized_label = alab in
                      let a1 = Array.of_list (List.map (fun n -> n#data#_label) (List.filter (filt alab1) cs1)) in
                      let a2 = Array.of_list (List.map (fun n -> n#data#_label) (List.filter (filt alab2) cs2)) in
                      let mat, _, _, _ = Adiff.adiff a1 a2 in
                      (Array.length a1) + (Array.length a2) - ((List.length mat) * 2)
                    in
                    DEBUG_MSG "n_nodes_of_same_cat_not_matched=%d" n_nodes_of_same_cat_not_matched;

                    let cat_cond = n_nodes_of_same_cat_not_matched = 2 in

                    let cond = parent_cond && cat_cond in

                    BEGIN_DEBUG
                      DEBUG_MSG "relabel: %a-%a (parent: %a-%a, num of children of same category not matched: %d)"
                        UID.ps nd1#uid UID.ps nd2#uid
                        UID.ps pnd1#uid UID.ps pnd2#uid
                        n_nodes_of_same_cat_not_matched;
                      DEBUG_MSG "parent_cond:%B cat_cond:%B --> filtered:%B" parent_cond cat_cond (not cond);
                    END_DEBUG;

                    cond
              with
              | Otreediff.Otree.Parent_not_found _
              | Not_found -> false

	    ) (List.fast_sort (fun (n0, _) (n1, _) -> Stdlib.compare n1#gindex n0#gindex) relabels)
	in

	BEGIN_DEBUG
	  List.iter 
	    (fun (n1, n2) -> 
	      DEBUG_MSG "relabels (filtered): %a-%a" UID.ps n1#uid UID.ps n2#uid
	    ) relabels_;
	END_DEBUG;

	let get_digest tree n =
	  let d = n#data#_digest in
	  let res =
	    if d = None then
	      if n#initial_nchildren > 0 then
		let subtree = tree#make_subtree_from_node n in
		Some subtree#digest
	      else
		None
	    else
	      d
	  in
	  res
	in

	List.iter (* for isos and moves *)
	  (fun (nd1, nd2) -> 
	    let uid1, uid2 = nd1#uid, nd2#uid in

	    DEBUG_MSG "checking <%a-%a>" UID.ps uid1 UID.ps uid2;

	    let adj = cenv#get_adjacency_score nd1 nd2 in

	    DEBUG_MSG " adjacency=%f" adj;

	    if adj > 0.0 then begin

	      let nprox = uidmapping#get_proximity ~extra:new_pairs1 nd1 nd2 in

	      let prox = nprox#primary_prox in

	      DEBUG_MSG " proximity=%d" prox;

	      let passed1 =
		try
		  let u2 = uidmapping#find uid1 in

		  if u2 = uid2 then 
		    raise Found;

		  let n2 = tree2#search_node_by_uid u2 in

		  if not (nd1#data#eq n2#data) && nd1#data#eq nd2#data then
		    raise Not_found;

		  let nprox_before1 = uidmapping#get_proximity nd1 n2 in

		  let prox_before1 = nprox_before1#primary_prox in

		  DEBUG_MSG " proximity (before:%a->%a)=%d" UID.ps uid1 UID.ps u2 prox_before1;

		  let cond =
		    if prox > prox_before1 then
		      if nprox#low_confidence then
			let pn1, pn2 = nprox#primary_pivot in
			let pn_b1, pn_b2 = nprox_before1#primary_pivot in
			let a = cenv#get_adjacency_score pn1 pn2 in
			let a_b = cenv#get_adjacency_score pn_b1 pn_b2 in
			if a >= a_b then
			  true
			else
			  let prox2 = nprox#secondary_prox in

			  DEBUG_MSG " secondary_prox(%a,%a)=%d" UID.ps uid1 UID.ps uid2 prox2;

			  if prox2 > prox_before1 then 
			    true
			  else if prox2 = prox_before1 then begin
			    let adj_before1 = cenv#get_adjacency_score nd1 n2 in

			    DEBUG_MSG " adjacency(before:%a->%a)=%f" UID.ps uid1 UID.ps u2 adj_before1;

			    adj > adj_before1
			  end
			  else
			    false
		      else
			true
		    else if prox = prox_before1 then begin
		      let adj_before1 = cenv#get_adjacency_score nd1 n2 in

		      DEBUG_MSG " adjacency(before:%a->%a)=%f" UID.ps uid1 UID.ps u2 adj_before1;

		      adj > adj_before1
		    end
		    else
		      false
		  in

		  cond && u2 <> uid2

		with Not_found -> true | Found -> false
	      in
	      let passed2 =
		try
		  let u1 = uidmapping#inv_find uid2 in

		  if u1 = uid1 then
		    raise Found;

		  let n1 = tree1#search_node_by_uid u1 in

		  if not (nd2#data#eq n1#data) && nd1#data#eq nd2#data then
		    raise Not_found;

		  let nprox_before2 = uidmapping#get_proximity n1 nd2 in
		  let prox_before2 = nprox_before2#primary_prox in

		  DEBUG_MSG " proximity (before:%a<-%a)=%d" UID.ps u1 UID.ps uid2 prox_before2;

		  let cond =
		    if prox > prox_before2 then
		      if nprox#low_confidence then
			let pn1, pn2 = nprox#primary_pivot in
			let pn_b1, pn_b2 = nprox_before2#primary_pivot in
			let a = cenv#get_adjacency_score pn1 pn2 in
			let a_b = cenv#get_adjacency_score pn_b1 pn_b2 in
			if a >= a_b then
			  true
			else
			  let prox2 = nprox#secondary_prox in

			  DEBUG_MSG " secondary_prox (%a,%a)=%d" UID.ps uid1 UID.ps uid2 prox2;

			  if prox2 > prox_before2 then 
			    true
			  else if prox2 = prox_before2 then begin
			    let adj_before2 = cenv#get_adjacency_score n1 nd2 in

			    DEBUG_MSG " adjacency (before:%a<-%a)=%f" UID.ps u1 UID.ps uid2 adj_before2;

			    adj > adj_before2
			  end
			  else
			    false
		      else
			true
		    else if prox = prox_before2 then begin
		      let adj_before2 = cenv#get_adjacency_score n1 nd2 in

		      DEBUG_MSG " adjacency (before:%a<-%a)=%f" UID.ps u1 UID.ps uid2 adj_before2;

		      adj > adj_before2
		    end
		    else
		      false
		  in

		  cond && u1 <> uid1

		with Not_found -> true | Found -> false
	      in
	      if passed1 && passed2 then begin
		add_enclave uid1 uid2;
		let d1 = get_digest tree1 nd1 in
		let d2 = get_digest tree2 nd2 in
		if d1 = d2 && d1 <> None then begin
		  let l1 = ref [] in
		  let l2 = ref [] in
		  tree1#fast_scan_whole_initial_subtree nd1 (fun n -> l1 := n#uid :: !l1);
		  tree2#fast_scan_whole_initial_subtree nd2 (fun n -> l2 := n#uid :: !l2);
		  List.iter2 
		    (fun u1 u2 -> 
		      if not (uidmapping#is_locked_uid u1 || uidmapping#is_locked_uid u2) then
			new_pairs_add (u1, u2)
		      else
			DEBUG_MSG "locked pair: %a-%a" UID.ps u1 UID.ps u2
		    ) !l1 !l2
		end
	      end

	    end
	  ) (matches_and_extra_matches_ @ relabels_ );
      end; (* if otree#size > 1 && ntree#size > 1 *)

      nd1#unhide_parent;
      nd2#unhide_parent;

    in (* end of func find_enclaves *)

    let sorted_keyroots = 
      List.fast_sort 
	(fun (n1, _) (n2, _) -> Stdlib.compare n1#gindex n2#gindex) 
	keyroots 
    in

    List.iter 
      (fun (nd1, nd2) -> find_enclaves nd1 nd2) sorted_keyroots;

    BEGIN_DEBUG
      let new_pairs_list() =
        Hashtbl.fold (fun u1 u2 l -> (u1, u2)::l) new_pairs1 []
      in
      DEBUG_MSG "enclaves: [\n%s\n]\n"
        (Xlist.to_string 
	   (fun (u1, u2) -> 
	     sprintf "%a-%a" UID.ps u1 UID.ps u2) "\n" (new_pairs_list()));
    END_DEBUG;

    if options#trust_tree_matcher_flag then begin

      DEBUG_MSG "removing old mappings...";

      uidmapping#filter (* removing old mappings *)
	(fun uid1 uid2 ->
	  let b = not (new_pairs_mem1 uid1 || new_pairs_mem2 uid2) in
	  if not b then
	    DEBUG_MSG "removing: %a-%a" UID.ps uid1 UID.ps uid2;
	  b
	);

      DEBUG_MSG "done."; (* removing old mappings *)

      DEBUG_MSG "adding new mappings...";

      Hashtbl.iter
	(fun u1 u2 ->
	  ignore (uidmapping#add_unsettled u1 u2);

	  DEBUG_MSG " %a-%a ---> added" UID.ps u1 UID.ps u2

	) new_pairs1;

      DEBUG_MSG "done."

    end;

    DEBUG_MSG "finished."
  (* end of func eliminate_enclaves *)




  let eliminate_relabels 
      (tree1 : tree_t) 
      (tree2 : tree_t) 
      (uidmapping : node_t UIDmapping.c)
      =
    DEBUG_MSG "* ELIMINATING ODD RELABELS...";

    uidmapping#filter
      (fun uid1 uid2 ->

	DEBUG_MSG "checking %a-%a" UID.ps uid1 UID.ps uid2;

	let nd1 = 
	  try tree1#search_node_by_uid uid1 with 
	    Not_found -> assert false
	in
	let nd2 = 
	  try tree2#search_node_by_uid uid2 with 
	    Not_found -> assert false
	in
	if not (nd1#data#eq nd2#data) then begin

	  DEBUG_MSG " -> relabel";

	  let is_odd = is_odd_relabel tree1 tree2 uidmapping nd1 nd2 in

	  if is_odd then
	    DEBUG_MSG "odd relabel: %a:%s -- %a:%s" 
	      UID.ps uid1 nd1#data#to_string 
	      UID.ps uid2 nd2#data#to_string;
	  
	  not is_odd
	end
	else
	  true
      );

    DEBUG_MSG "ODD RELABELS ELIMINATED."
  (* end of func eliminate_relabels *)



 (*
  * glueing deletes and inserts
  *)
  let glue_deletes_and_inserts
      ?(first=false)
      ?(record_conflicted_pairs=false)
      options
      (cenv : (node_t, tree_t) Comparison.c)
      (tree1 : tree_t) 
      (tree2 : tree_t)
      ?(override=false)
      ?(no_mapping_override=false)
      ?(no_moves=false)
      ?(is_move=(fun n1 n2 -> false))
      ?(downward=false)
      ?(glue_filt=(fun _ _ -> true : UID.t -> UID.t -> bool))
      (uidmapping : node_t UIDmapping.c) 
      (ref_uidmapping : node_t UIDmapping.c) 
      =
    Xprint.verbose options#verbose_flag "glueing deletes and inserts...";
(*
    let is_possible_rename n1 n2 =
      let b = cenv#is_possible_rename n1 n2 in
      DEBUG_MSG "%a-%a --> %B" UID.ps n1#uid UID.ps n2#uid b;
      b
    in
*)
    let relabel_allowed n1 n2 =
      (n1#data#relabel_allowed n2#data) (* && is_possible_rename n1 n2 !!!*)
    in

    let bad_pairs = cenv#bad_pairs in
    BEGIN_DEBUG
      DEBUG_MSG "START";
      if not (Xset.is_empty bad_pairs) then begin
	Xset.iter
	  (fun (u1, u2) -> 
	    DEBUG_MSG "bad_pair: <%a-%a>" UID.ps u1 UID.ps u2
	  ) bad_pairs
      end;
      Hashtbl.iter 
	(fun u1 u2 -> 
	  DEBUG_MSG "stable_pair: %a-%a" UID.ps u1 UID.ps u2
	) uidmapping#stable_pairs;
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
      (*DEBUG_MSG "uidmapping (gindex):\n%s\n" uidmapping#to_string_gid;*)
    END_DEBUG;

    let is_bad_pair u1 u2 = 
      let b = Xset.mem bad_pairs (u1, u2) in
      DEBUG_MSG "%a-%a --> %B" UID.ps u1 UID.ps u2 b;
      b
    in

    let is_mapped_pair u1 u2 =
      let b = uidmapping#mem_dom u1 || uidmapping#mem_cod u2 in
      DEBUG_MSG "%a-%a --> %B" UID.ps u1 UID.ps u2 b;
      b
    in
    let get_mapping u1 u2 =
      let u1', u2' =
        if uidmapping#mem_dom u1 then
          u1, uidmapping#find u1
        else if uidmapping#mem_cod u2 then
          uidmapping#inv_find u2, u2
        else
          raise Not_found
      in
      DEBUG_MSG "%a-%a --> %a-%a" UID.ps u1 UID.ps u2 UID.ps u1' UID.ps u2';
      u1', u2'
    in

    let conflicted_pairs = Xset.create 0 in

    let calc_bonus nd1 nd2 =
      if nd1#data#is_anonymous || nd2#data#is_anonymous then
	if nd1#data#subtree_equals nd2#data then
	  2
	else
	  if nd1#data#eq nd2#data then
	    1
	  else
	    0
      else 
	if nd1#data#eq nd2#data then
	  2
	else
	  0
    in

    let default_score_up = 2 in
    let default_score_down = 1 in

    let get_scoring ?(base=(default_score_up,default_score_down)) u1 u2 =
      let s_up, s_down = base in
      let s = 
	if uidmapping#is_stable_pair u1 u2 then
	  s_up + 3, s_down + 3
	else
	  let n1 = tree1#search_node_by_uid u1 in
	  let n2 = tree2#search_node_by_uid u2 in

	  if n1#data#eq n2#data then
	    if n1#path#equals n2#path then
	      s_up + 2, s_down + 2
	    else
	      s_up + 1, s_down + 1

	  else if n1#data#_anonymized_label = n2#data#_anonymized_label then
	    if n1#path#equals n2#path then
	      s_up + 2, s_down + 2
	    else
	      s_up + 1, s_down + 1

	  else if relabel_allowed n1 n2 then
	    if n1#path#equals n2#path then
	      s_up + 2, s_down + 2
	    else
	      s_up + 1, s_down + 1

	  else
	    s_up, s_down
      in
      DEBUG_MSG "%a-%a -> (%d,%d)" UID.ps u1 UID.ps u2 (fst s) (snd s);
      s
    in


    tree1#init; tree2#init;

    let cands = ref ([] : ((UID.t * UID.t) * int ref) list) in

    let add_cand context nd1 nd2 u1 u2 score = 

      if is_bad_pair u1 u2 then
	DEBUG_MSG "bad pair: %a-%a" UID.ps u1 UID.ps u2
(*
      else if not ((nd1#data#eq nd2#data) || is_possible_rename nd1 nd2) then
	DEBUG_MSG "not possible rename: %a-%a" UID.ps u1 UID.ps u2
*)
      else if no_mapping_override && is_mapped_pair u1 u2 then
	DEBUG_MSG "mapped pair: %a-%a" UID.ps u1 UID.ps u2

      else if no_moves && (is_move nd1 nd2) then
	DEBUG_MSG "move: %a-%a" UID.ps u1 UID.ps u2

      else if not (glue_filt u1 u2) then
        DEBUG_MSG "filtered: %a-%a" UID.ps u1 UID.ps u2

      else begin

	BEGIN_DEBUG
	  let pr u =
	    try
	      let key = uidmapping#key_of_locked_uid u in
	      DEBUG_MSG "%a is locked (key=%s)" UID.ps u
	        (Key.to_string ~opr:(fun o -> Label.to_string (Obj.obj o)) key)
	    with
	      Not_found -> ()
	  in
	  pr u1;
	  pr u2;
	END_DEBUG;

	let check u =
	  try
	    let key = uidmapping#key_of_locked_uid u in
	    (key = Key.make_pair_key nd1 nd2), Some key
	  with
	    Not_found -> true, None
	in

	let u1_ok, key1_opt = check u1 in
	let u2_ok, key2_opt = check u2 in

	let can_add =
	  u1_ok && u2_ok &&
	  (match key1_opt, key2_opt with
	  | Some key1, Some key2 -> key1 = key2
	  | _ -> true)
	in

	if can_add then begin

	  BEGIN_DEBUG
	    DEBUG_MSG "[%s]: adding %a -> %a (score=%d)" context UID.ps u1 UID.ps u2 score;
	    DEBUG_MSG "[%s]:        %a    %a" context GI.ps nd1#gindex GI.ps nd2#gindex;
	  END_DEBUG;
	  
	  try
	    let s = List.assoc (u1, u2) !cands in
	    s := max !s score
	  with 
	    Not_found -> cands := ((u1, u2), ref score)::!cands
	end

      end

    in (* add_cand *)

    let is_cand u1 u2 =
      List.mem_assoc (u1, u2) !cands
    in

    let scanned_up   = Hashtbl.create 0 in
    let scanned_down = Hashtbl.create 0 in
    let scanned_up_add (uid1, uid2)   = Hashtbl.add scanned_up uid1 uid2 in
    let scanned_down_add (uid1, uid2) = Hashtbl.add scanned_down uid1 uid2 in
    let scanned_up_mem (uid1, uid2) =
      try
	let uids = Hashtbl.find_all scanned_up uid1 in
	List.memq uid2 uids
      with 
	Not_found -> false
    in
    let scanned_down_mem (uid1, uid2) =
      try
	let uids = Hashtbl.find_all scanned_down uid1 in
	List.memq uid2 uids
      with 
	Not_found -> false
    in

    let rec scan_up ?(reflex=false) (score_up, score_down) uid1 uid2 =

      if is_bad_pair uid1 uid2 then
	DEBUG_MSG "bad pair: %a-%a" UID.ps uid1 UID.ps uid2

      else if scanned_up_mem (uid1, uid2) then begin
	DEBUG_MSG "already scanned up: %a-%a" UID.ps uid1 UID.ps uid2

      end
      else begin
	scanned_up_add (uid1, uid2);

	DEBUG_MSG "[override=%B] (%d,%d): %a-%a" 
	  override score_up score_down UID.ps uid1 UID.ps uid2;

	let nd1 = tree1#search_node_by_uid uid1 in
	let nd2 = tree2#search_node_by_uid uid2 in

	if nd1#has_initial_parent && nd2#has_initial_parent then begin

	  let puid1 = nd1#initial_parent#uid in
	  let puid2 = nd2#initial_parent#uid in

	  DEBUG_MSG "\tparents: %a-%a" UID.ps puid1 UID.ps puid2;

	  if is_bad_pair puid1 puid2 then
	    DEBUG_MSG "bad pair: %a-%a" UID.ps puid1 UID.ps puid2

	  else begin

	    let pnd1 = tree1#search_node_by_uid puid1 in
	    let pnd2 = tree2#search_node_by_uid puid2 in

	    let already_mapped = 
	      if (uidmapping#mem_dom puid1) || (uidmapping#mem_cod puid2) then
		try
		  let puid1' = uidmapping#find puid1 in
		  let pnd1' = tree2#search_node_by_uid puid1' in
		  Some (pnd1, pnd1')
		with
		  Not_found ->
		    let puid2' = uidmapping#inv_find puid2 in
		    let pnd2' = tree1#search_node_by_uid puid2' in
		    Some (pnd2', pnd2)
	      else
		None
	    in

	    BEGIN_DEBUG
	    match already_mapped with
	    | None -> DEBUG_MSG "\tnot mapped yet"
	    | Some (n1, n2) ->
		let nmapped1 = nmapped_of_subtree1 n1 in
		let nnodes1 = tree1#whole_initial_subtree_size n1 in
		let nmapped2 = nmapped_of_subtree2 n2 in
		let nnodes2 = tree2#whole_initial_subtree_size n2 in
		(*
		  DEBUG_MSG "\tuidmapping: %a(%d/%d) -> %a(%d/%d) (subtree sim.: %f)" 
		  UID.ps n1#uid nmapped1 nnodes1
		  UID.ps n2#uid nmapped2 nnodes2
		  (estimate_subtree_similarity tree1 tree2 n1 n2);
		 *)
		DEBUG_MSG "\tuidmapping: %a(%d/%d) -> %a(%d/%d)" 
		  UID.ps n1#uid nmapped1 nnodes1
		  UID.ps n2#uid nmapped2 nnodes2;
		END_DEBUG;


		let label_match_score = cenv#eval_label_match pnd1 pnd2 in

		let anc_sim = cenv#get_ancestors_similarity pnd1 pnd2 in

		let continue, is_cand =
		  match already_mapped with
		  | None -> true, true
		  | Some (n1, n2) ->
		      let lms = cenv#eval_label_match n1 n2 in
		      let asim = cenv#get_ancestors_similarity n1 n2 in

		      DEBUG_MSG "label match score: %d --> %d" lms label_match_score;
		      DEBUG_MSG "ancestors similarity: %f --> %f" asim anc_sim;

		      let lcond = 
			if override then
			  lms <= label_match_score
			else
			  lms < label_match_score
		      in
		      let acond =
			if override then
			  asim <= anc_sim
			else
			  asim < anc_sim
		      in
		      lcond || acond, lcond
		in (* let continue, is_cand *)
		
		if continue then begin

		  DEBUG_MSG "\tglue cand (scan_up): %a-%a" UID.ps puid1 UID.ps puid2;

		  let bonus, is_ok = 
		    if pnd1#data#equals pnd2#data then 
		      calc_bonus pnd1 pnd2, true
		    else
		      label_match_score, relabel_allowed pnd1 pnd2
		  in
		  if is_ok then begin

		    if is_cand || reflex then begin
		      DEBUG_MSG "base score: %d, bonus: %d" score_up bonus;

		      let score = score_up + bonus in
		      add_cand "scan_up" pnd1 pnd2 puid1 puid2 score
		    end;

		    scan_up (score_up, score_down) puid1 puid2;

		    if not options#simple_glue_flag && is_cand then
(*	      let scoring = get_scoring uid1 uid2 in *)
		      scan_down (* scoring *) (score_up, score_down) puid1 puid2 (* cf. ieee1394_transactions.c 481L packet -> 481L packet *)
		  end
		  else 
(*	      check_upper_boundary (score_up, score_down) puid1 puid2 *)
                    if not options#simple_glue_flag then
		    check_until_upper_boundary (score_up, score_down) pnd1 pnd2

		end (* of if continue *)

	  end (* of if not (is_bad_pair puid1 puid2) *)

	end (* of if (nd1#has_initial_parent && nd2#has_initial_parent) *)

      end (* of if scanned_up_mem (uid1, uid2) *)
    (* end of func scan_up *)


    and check_until_upper_boundary (score_up, score_down) nd1 nd2 =

      BEGIN_DEBUG
        let uid1, uid2 = nd1#uid, nd2#uid in
        DEBUG_MSG "checking %a-%a" UID.ps uid1 UID.ps uid2;
      END_DEBUG;

      let boundary = ref None in
      begin
	try
	  nd1#iter_initial_ancestor_nodes
	    (fun n1 ->
	      try
		let n1' = tree2#search_node_by_uid (uidmapping#find n1#uid) in
		if tree2#is_initial_ancestor n1' nd2 then begin
		  boundary := Some (n1, n1');
		  raise Break
		end
	      with
		Not_found -> ()
	    )
	with
	  Break -> ()
      end;
      begin
	match !boundary with 
	| None -> DEBUG_MSG "no boundary"
	| Some (bn1, bn2) ->
	    DEBUG_MSG "boundary: %a-%a" UID.ps bn1#uid UID.ps bn2#uid;

	    let alab1 = nd1#data#_anonymized2_label in
	    let alab2 = nd2#data#_anonymized2_label in
	    let cand1 = ref None in
	    let cand2 = ref None in
	    begin 
	      try
		nd2#iter_initial_ancestor_nodes
		  (fun n2 ->
		    if n2 == bn2 then
		      raise Break
		    else
		      if not (uidmapping#mem_cod n2#uid) then
			if n2#data#_anonymized2_label = alab1 then begin
			  cand1 := Some (nd1, n2);
			  raise Break
			end
		  )
	      with
		Break -> ()
	    end;
	    begin 
	      try
		nd1#iter_initial_ancestor_nodes
		  (fun n1 ->
		    if n1 == bn1 then
		      raise Break
		    else
		      if not (uidmapping#mem n1#uid) then
			if n1#data#_anonymized2_label = alab2 then begin
			  cand2 := Some (n1, nd2);
			  raise Break
			end
		  )
	      with
		Break -> ()
	    end;
	    
	    let continue n1 n2 =
	      let u1, u2 = n1#uid, n2#uid in

	      if is_bad_pair u1 u2 then
		DEBUG_MSG "bad pair: %a-%a" UID.ps u1 UID.ps u2

	      else
		let bonus, is_ok =
		  if n1#data#equals n2#data then 
		    calc_bonus n1 n2, true
		  else 
		    cenv#eval_label_match n1 n2, relabel_allowed n1 n2
		in
		if is_ok then begin

		  DEBUG_MSG "base score: %d, bonus: %d" score_up bonus;

		  let score = score_up + bonus in
		  add_cand "check_until_upper_boundary" n1 n2 u1 u2 score;

(*	          let scoring = get_scoring n1 n2 in *)
		  scan_up (* scoring *) (score_up, score_down) u1 u2
		end
	    in (* func continue *)

	    BEGIN_DEBUG
	      let f = function
		| None -> "none" 
		| Some (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid
	      in
	      DEBUG_MSG "cand1: %s" (f !cand1);
	      DEBUG_MSG "cand2: %s" (f !cand2);
	    END_DEBUG;

	    match !cand1, !cand2 with
	    | None, None -> ()
	    | Some (n1, n2), None | None, Some (n1, n2) -> continue n1 n2
	    | Some (n11, n12), Some (n21, n22) ->
		if n11 == n21 && n12 == n22 then
		  continue n11 n12

		else
		  let score1 = cenv#get_adjacency_score n11 n12 in
		  let score2 = cenv#get_adjacency_score n21 n22 in
		  if score1 >= score2 then
		    continue n11 n12

		  else
		    continue n21 n22
      end
    (* end of check_until_upper_boundary *)


    and nmapped_of_subtree ?(exclude_root=false) tree mem nd =
      let c = ref 0 in
      let f = 
	if exclude_root then
	  (fun n -> if n != nd && mem n#uid then incr c)
	else
	  (fun n -> if mem n#uid then incr c)
      in
      tree#fast_scan_whole_initial_subtree nd f;
      !c

    and nmapped_of_subtree1 ?(exclude_root=false) nd =
      nmapped_of_subtree ~exclude_root tree1 uidmapping#mem_dom nd

    and nmapped_of_subtree2 ?(exclude_root=false) nd =
      nmapped_of_subtree ~exclude_root tree2 uidmapping#mem_cod nd

    and scan_down ?(hardoverride=false) ?(force_treediff=false) (score_up, score_down) uid1 uid2 =

      let nd1 = tree1#search_node_by_uid uid1 in
      let nd2 = tree2#search_node_by_uid uid2 in

      if not (nd1#is_valid && nd2#is_valid) then
	DEBUG_MSG "invalid node: %a-%a" UID.ps uid1 UID.ps uid2

      else if is_bad_pair uid1 uid2 then
	DEBUG_MSG "bad pair: %a-%a" UID.ps uid1 UID.ps uid2

      else if scanned_down_mem (uid1, uid2) then begin
	DEBUG_MSG "already scanned down: %a-%a"
	  UID.ps uid1 UID.ps uid2
      end
      else begin
	scanned_down_add (uid1, uid2);

	BEGIN_DEBUG
	  DEBUG_MSG "[hardoverride=%B:override=%B] (%d,%d): %a[%s] -" 
	  hardoverride override score_up score_down
	  UID.ps uid1 
	  (Xarray.to_string 
	     (fun n -> UID.to_string n#uid) ";" nd1#initial_children);
	  DEBUG_MSG "[hardoverride=%B:override=%B] (%d,%d): %a[%s]" 
	  hardoverride override score_up score_down
	  UID.ps uid2 
	  (Xarray.to_string 
	     (fun n -> UID.to_string n#uid) ";" nd2#initial_children);
	END_DEBUG;

	let single_children = nd1#initial_nchildren = 1 && nd2#initial_nchildren = 1 in

	(* mapped subtree members (but sub-root) *)
	let nmapped1 = nmapped_of_subtree1 ~exclude_root:true nd1 in
	let nmapped2 = nmapped_of_subtree2 ~exclude_root:true nd2 in

	let nmembers1 = tree1#whole_initial_subtree_size nd1 in
	let nmembers2 = tree2#whole_initial_subtree_size nd2 in

	BEGIN_DEBUG
	  let mapped_ratio1 = (float nmapped1) /. (float nmembers1) in
	  let mapped_ratio2 = (float nmapped2) /. (float nmembers2) in
	  DEBUG_MSG "%a: mapped subtree members (%d/%d=%f)" 
	    UID.ps uid1 nmapped1 nmembers1 mapped_ratio1;
	  DEBUG_MSG "%a: mapped subtree members (%d/%d=%f)" 
	    UID.ps uid2 nmapped2 nmembers2 mapped_ratio2;
	END_DEBUG;

	let all_subtree_members_not_mapped = nmapped1 = 0 && nmapped2 = 0 in

	if all_subtree_members_not_mapped then
	  DEBUG_MSG "all subtree members not mapped";
	
	let size_cond = 
	  (nmembers1 > 1 || nmembers2 > 1) &&
	  (nmembers1 < options#match_algo_threshold) && (nmembers2 < options#match_algo_threshold) 
	in

        let go_down_cond =
          match nd1#initial_children, nd2#initial_children with
          | [|cnd1|], [|cnd2|] -> nd1#data#eq nd2#data
          | _ -> false
        in

	let ca1, ca2 = nd1#initial_children, nd2#initial_children in

        let use_treediff_cond =
          size_cond && (all_subtree_members_not_mapped || force_treediff) && (not go_down_cond)
        in

        if use_treediff_cond then begin (* use tree diff *)

          if force_treediff then
            DEBUG_MSG "force_treediff=true";

          let subtree1 = tree1#make_subtree_from_node nd1 in
          let subtree2 = tree2#make_subtree_from_node nd2 in

          nd1#hide_parent;
          nd2#hide_parent;

          let subroot1 = subtree1#root in
          let subroot2 = subtree2#root in
	  
          let matches, extra_matches, relabels =
            Treediff.match_trees cenv ~root_check:false subtree1 subtree2 uidmapping ref_uidmapping
          in

          let pseudo_matches = List.filter (fun (n1, n2) -> is_pseudo_match n1 n2) relabels in

          let nrelabels      = List.length relabels in

          BEGIN_DEBUG
            let nmatches       = List.length matches in
            let nextra_matches = List.length extra_matches in
            DEBUG_MSG "(by tree diff): match:%d extra_match:%d relabels:%d"
              nmatches nextra_matches nrelabels;
          END_DEBUG;

          let conv = Misc.conv_subtree_node_pairs tree1 tree2 in

          let amatches, aextra_matches, arelabels =
            if nrelabels > 0 then
              let uids_left_named1, uids_left_named2 =
                List.split
                  (List.map
                     (fun (n1, n2) -> n1#uid, n2#uid)
                     (matches @ extra_matches @ pseudo_matches)
                  )
              in
              try
                let atree1 =
                  subtree1#make_anonymized2_subtree_copy ~uids_left_named:uids_left_named1 subroot1
                in
                let atree2 =
                  subtree2#make_anonymized2_subtree_copy ~uids_left_named:uids_left_named2 subroot2
                in

                DEBUG_MSG "anonymized trees: |T1(root:%a)|=%d |T2(root:%a)|=%d"
                  UID.ps uid1 atree1#size UID.ps uid2 atree2#size;

                let acenv = new Comparison.c options atree1 atree2 in

                let m, em, r =
                  Treediff.match_trees acenv ~root_check:false atree1 atree2
                    (new UIDmapping.c acenv) (new UIDmapping.c acenv)
                in

                conv m, conv em, conv r

              with
                Invalid_argument msg ->
                  Xprint.error "%s" msg;
                  assert false

            else (* nrelabels = 0 *)
              matches, extra_matches, []
          in

          let arelabels = (* ??? *)
            List.filter
              (fun (n1, n2) ->
                check_relabel options ~exact:true tree1 tree2 n1 n2 uidmapping
              ) arelabels
          in

          let no_cands_found = ref true in

          List.iter
            (fun (nd1, nd2) ->
              let u1, u2 = nd1#uid, nd2#uid in

              let bonus, is_ok =
                if nd1#data#equals nd2#data then
                  calc_bonus nd1 nd2, true
                else
                  cenv#eval_label_match nd1 nd2, relabel_allowed nd1 nd2
              in
              if is_ok then begin
                no_cands_found := false;

                DEBUG_MSG "! glue cand (scan_down: by tree diff): %a-%a" UID.ps u1 UID.ps u2;

                let base =
                  if single_children (* is_unique_pair nd1 nd2 *) then
                    score_up
                  else
                    score_down
                in

                DEBUG_MSG "base score: %d, bonus: %d" base bonus;

                let score = base + bonus in
                add_cand "tree diff on subtrees" nd1 nd2 u1 u2 score
              end

            ) (amatches @ aextra_matches @ arelabels);

          DEBUG_MSG "no_cands_found=%B" !no_cands_found;

          if !no_cands_found then begin
            let matches, extra_matches, relabels =
              Treediff.match_trees cenv ~root_check:false ~semantic:true subtree1 subtree2 uidmapping ref_uidmapping
            in
            let relabels =
              List.filter
                (fun (n1, n2) ->
                  check_relabel options ~exact:false tree1 tree2 n1 n2 uidmapping
                ) relabels
            in
            List.iter
              (fun (nd1, nd2) ->
                let u1, u2 = nd1#uid, nd2#uid in
                let bonus, is_ok = cenv#eval_label_match nd1 nd2, relabel_allowed nd1 nd2 in
                if is_ok then begin
                  DEBUG_MSG "! glue cand (scan_down: by tree diff): %a-%a" UID.ps u1 UID.ps u2;
                  let base =
                    if single_children (* is_unique_pair nd1 nd2 *) then
                      score_up
                    else
                      score_down
                  in
                  DEBUG_MSG "base score: %d, bonus: %d" base bonus;
                  let score = base + bonus in
                  add_cand "weak tree diff on subtrees" nd1 nd2 u1 u2 score
                end
              ) relabels
          end;

          nd1#unhide_parent;
          nd2#unhide_parent;

        end (* of if the sizes of subtrees are moderate... *)
        else begin (* tree diff not used or no cands found *)

	  let cld1, cld2 =
	    let idxs = ref [] in
	    let cond =
	      if (Array.length ca1) = (Array.length ca2) then begin
		try
		  Array.iteri 
		    (fun i n1 ->
		      let n2 = ca2.(i) in
		      if n1#data#_anonymized_label = n2#data#_anonymized_label then begin
			let not_mapped = 
			  (not (uidmapping#mem_dom n1#uid)) && (not (uidmapping#mem_cod n2#uid))
			in
			if n1#data#eq n2#data || not_mapped then
			  idxs := i :: !idxs
		      end
		      else
			raise Break
		    ) ca1;
		  true
		with
		  Break -> false
	      end
	      else
		false
	    in
	    if cond && (not (override || hardoverride)) then begin
	      List.fold_left
		(fun (l1, l2) i ->
		  ca1.(i)::l1, ca2.(i)::l2
		) ([], []) !idxs
	    end
	    else
	      let _cld1 =
		if override || hardoverride then
		  Array.to_list ca1
		else
		  List.filter
		    (fun n1 -> 
		      try
			let n2 = tree2#search_node_by_uid (uidmapping#find n1#uid) in
			not (n1#data#eq n2#data)
		      with 
			Not_found -> true
		    ) (Array.to_list ca1)
	      in
	      let _cld2 =
		if override || hardoverride then
		  Array.to_list ca2
		else
		  List.filter 
		    (fun n2 -> 
		      try
			let n1 = tree1#search_node_by_uid (uidmapping#inv_find n2#uid) in
			not (n1#data#eq n2#data)
		      with 
			Not_found -> true
		    ) (Array.to_list ca2)
	      in
	      _cld1, _cld2
	  in

	  let cld1, cld2 = 
	    let f l = 
	      List.filter 
		(fun n -> 
		  let u = n#uid in
		  let locked = uidmapping#is_locked_uid u in
		  if locked then
		    DEBUG_MSG "%a is locked" UID.ps u;
		  not locked
		) l 
	    in
	    (f cld1, f cld2)
	  in

	  BEGIN_DEBUG
	    let ltos tree nmapped_of_subtree l = 
	      Xlist.to_string 
	        (fun n -> 
		  sprintf "%a(%d/%d)" UID.ps n#uid (nmapped_of_subtree n) (tree#whole_initial_subtree_size n)
	        ) ";" l 
	    in
	    let ltos1 = ltos tree1 nmapped_of_subtree1 in
	    let ltos2 = ltos tree2 nmapped_of_subtree2 in
	    DEBUG_MSG "filtered: %a[%s] -" UID.ps uid1 (ltos1 cld1);
	    DEBUG_MSG "filtered: %a[%s]" UID.ps uid2 (ltos2 cld2);
	    let ltos l = Xlist.to_string (fun n -> n#data#label) ";" l in
	    DEBUG_MSG "filtered (label): %a[%s] -" UID.ps uid1 (ltos cld1);
	    DEBUG_MSG "filtered (label): %a[%s]" UID.ps uid2 (ltos cld2);
	  END_DEBUG;
	  
	  let cld1a = Array.of_list cld1 in
	  let cld2a = Array.of_list cld2 in

	  let len1 = Array.length cld1a in
	  let len2 = Array.length cld2a in


	  let occur_tbl1 = Hashtbl.create 0 in
	  let occur_tbl2 = Hashtbl.create 0 in
	  let occur_tbl_add tbl k v =
	    try
	      let l = Hashtbl.find tbl k in
	      Hashtbl.replace tbl k (v::l)
	    with
	      Not_found -> Hashtbl.add tbl k [v]
	  in
	  let occur_tbl_create tbl clda =
	    let prev, count =
	      Array.fold_left 
		(fun (prev, count) (alab, alab_str) ->
		  match prev with
		  | None -> (Some (alab, alab_str), 1)
		  | Some (p, pstr) -> 
		      if p = alab then
			(prev, count + 1)

		      else begin
			occur_tbl_add tbl p count;

			DEBUG_MSG "  alab count: %s -> %d" pstr count;

			(Some (alab, alab_str), 1)
		      end
			  
		) (None, 0) (Array.map (fun n -> n#data#_anonymized_label, n#data#anonymized_label) clda)
	    in
	    match prev with
	    | Some (p, pstr) ->
		occur_tbl_add tbl p count;
		DEBUG_MSG "  alab count: %s -> %d" pstr count;
	    | _ -> ()
	  in
	  occur_tbl_create occur_tbl1 cld1a;
	  DEBUG_MSG "  ---";
	  occur_tbl_create occur_tbl2 cld2a;

	  let has_different_repetition_pattern =
	    try
	      Hashtbl.iter
		(fun alab2 counts2 ->
		  try
		    let counts1 = Hashtbl.find occur_tbl1 alab2 in
		    if counts1 <> counts2 then
		      raise Found
		  with
		    Not_found -> ()
	      	) occur_tbl2;
	      false
	    with
	      Found -> true
	  in

	  DEBUG_MSG "has_different_repetition_pattern: %B" has_different_repetition_pattern;



	  let _cands =
	    match cld1a, cld2a with
	    | [||], _ | _, [||] -> []
	    | _ ->
		let use_similarity = has_different_repetition_pattern && len1 <> len2 in

		DEBUG_MSG "use_similarity: %B" use_similarity;

		if use_similarity then
		  let f0 = Array.map (fun n -> n#data#_label, n#data#_digest) in
		  let cldd1 = f0 cld1a in
		  let cldd2 = f0 cld2a in

		  let mat, _, _, _ = Adiff.adiff cldd1 cldd2 in

		  let mat1, mat2 = List.split mat in
		  let f1 m cldd = 
		    Array.iteri (fun i (l, d) -> if not (List.mem i m) then cldd.(i) <- (l, None)) cldd 
		  in
		  f1 mat1 cldd1;
		  f1 mat2 cldd2;

		  let get_weight i j _ =
		    cenv#get_similarity_score cld1a.(i) cld2a.(j)
		  in

		  let mat' = HIS.Float.compute get_weight cldd1 cldd2 in

		  let mat1', mat2' = List.split mat' in
		  let f2 m m' clda cldd = 
		    Array.iteri 
		      (fun i (l, d) -> 
			if not (List.mem i m || List.mem i m') then 
			  cldd.(i) <- ((clda.(i))#data#_anonymized2_label, None)
		      ) cldd
		  in
		  f2 mat1 mat1' cld1a cldd1;
		  f2 mat2 mat2' cld2a cldd2;

		  let mat'' = HIS.Float.compute get_weight cldd1 cldd2 in

		  List.map (fun (i, j) -> cld1a.(i), cld2a.(j)) mat''

		else (* similarity not used *)

                  let get_extra_matches _del _ins m =
                    let m_tbl = Hashtbl.create 0 in
                    List.iter (fun (i, j) -> Hashtbl.add m_tbl cld1a.(i)#uid cld2a.(j)#uid) m;
                    let del = List.map (fun i -> cld1a.(i)) _del in
                    let ins = List.map (fun j -> cld2a.(j)) _ins in
                    Treediff.find_glue_cands ~simple:true tree1 tree2 del ins m_tbl
                  in

		  let f0 = Array.map (fun n -> n#data#_label, n#data#_digest) in
		  let cldd1 = f0 cld1a in
		  let cldd2 = f0 cld2a in

                  let digest_to_str = function
                    | None -> ""
                    | Some d -> Digest.to_hex d
                  in

                  BEGIN_DEBUG
                    Array.iteri
                      (fun i (l, d) ->
                        DEBUG_MSG "cldd1: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                      ) cldd1;
                    Array.iteri
                      (fun i (l, d) ->
                        DEBUG_MSG "cldd2: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                      ) cldd2;
                  END_DEBUG;

		  let mat, _, del, ins = Adiff.adiff cldd1 cldd2 in

                  let exm = get_extra_matches del ins mat in

		  let mat1, mat2 = List.split mat in
		  let f1 m cldd = 
		    Array.iteri (fun i (l, d) -> if not (List.mem i m) then cldd.(i) <- (l, None)) cldd 
		  in
		  f1 mat1 cldd1;
		  f1 mat2 cldd2;

                  BEGIN_DEBUG
                    DEBUG_MSG "mat: [%s]" (Xlist.to_string (fun (i, j) -> sprintf "(%d,%d)" i j) ";" mat);
                    Array.iteri
                      (fun i (l, d) ->
                        DEBUG_MSG "cldd1: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                      ) cldd1;
                    Array.iteri
                    (fun i (l, d) ->
                      DEBUG_MSG "cldd2: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                    ) cldd2;
                  END_DEBUG;

		  let mat', _, _, _ = Adiff.adiff cldd1 cldd2 in

		  let mat1', mat2' = List.split mat' in
                  let a3_flag = false(*not (is_move nd1 nd2) && no_moves*) in
		  let f2 m m' cldd clda =
		    Array.iteri 
		      (fun i (l, d) -> 
			if not (List.mem i m || List.mem i m') then 
                          let a =
                            if a3_flag then
                              (clda.(i))#data#_anonymized3_label
                            else
                              (clda.(i))#data#_anonymized2_label
                          in
			  cldd.(i) <- (a, None)
		      ) cldd
		  in
		  f2 mat1 mat1' cldd1 cld1a;
		  f2 mat2 mat2' cldd2 cld2a;

                  BEGIN_DEBUG
                    DEBUG_MSG "mat': [%s]" (Xlist.to_string (fun (i, j) -> sprintf "(%d,%d)" i j) ";" mat');
                    Array.iteri
                      (fun i (l, d) ->
                        DEBUG_MSG "cldd1: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                      ) cldd1;
                    Array.iteri
                      (fun i (l, d) ->
                        DEBUG_MSG "cldd2: %s(%s)" (Label.to_string (Obj.obj l)) (digest_to_str d)
                      ) cldd2;
                  END_DEBUG;

		  let mat'', _, _, _ = Adiff.adiff cldd1 cldd2 in
                  let nps'' = List.rev_map (fun (i, j) -> cld1a.(i), cld2a.(j)) mat'' in
		  List.rev_append nps'' (List.filter (fun x -> not (List.mem x nps'')) exm)

	  in (* _cands *)


	  BEGIN_DEBUG
	    DEBUG_MSG "_cands:";
	  List.iter
	    (fun (n1, n2) ->
	      DEBUG_MSG "%a[%s] - %a[%s]" 
		UID.ps n1#uid n1#data#label UID.ps n2#uid n2#data#label
	    ) _cands;
	  END_DEBUG;

	  let match_ratio = 
	    let n = Xlist.min [List.length cld1; List.length cld2] in
	    if n = 0 then
	      1.0
	    else
	      (float (List.length _cands)) /. (float n)
	  in

	  DEBUG_MSG "match_ratio=%f" match_ratio;

	  let cands =
	    let clda1 = Array.map (fun n -> n#data#_anonymized2_label) cld1a in
	    let clda2 = Array.map (fun n -> n#data#_anonymized2_label) cld2a in
	    let repetition_free a =
	      try
		Array.iteri 
		  (fun i lab -> 
		    try
		      if lab = a.(i+1) then
			raise Found
		    with 
		      Invalid_argument _ -> ()
		  ) a;
		true
	      with 
		Found -> false
	    in
	    let is_reliable =
	      (repetition_free clda1) && (repetition_free clda2)
	    in

	    if is_reliable && match_ratio < options#pp_anonymized_match_threshold then begin
	      DEBUG_MSG "trying to match anonymized labels... (match ratio: %f, thresh=%f)" 
		match_ratio options#pp_anonymized_match_threshold;

	      let mat, rel, _, _ = Adiff.adiff clda1 clda2 in
	      List.map (fun (i, j) -> cld1a.(i), cld2a.(j)) (mat @ rel)
	    end
	    else
	      _cands
	  in (* cands *)

	  let to_be_focused = cands = [] && match_ratio = 0.0 in

	  let cands =
	    if to_be_focused then begin

	      DEBUG_MSG "  focusing on %a-%a" UID.ps uid1 UID.ps uid2;

	      let gi1, gi2 = nd1#gindex, nd2#gindex in
	      let lgi1 = (tree1#initial_leftmost nd1)#gindex in
	      let lgi2 = (tree2#initial_leftmost nd2)#gindex in
	      let found_st = ref [] in
	      begin
		let filt lgi gi (n, _) =
		  let g = n#gindex in
		  lgi <= g && g <= gi
		in
		try
		  let multiple_subtree_matches = cenv#multiple_subtree_matches in
		  multiple_subtree_matches#iter
		    (fun (d, ndmems1, ndmems2, sz) ->
		      let ndmems1' = List.filter (filt lgi1 gi1) ndmems1 in
		      let ndmems2' = List.filter (filt lgi2 gi2) ndmems2 in
		      match ndmems1', ndmems2' with
		      | [n1, mems1], [n2, mems2] ->
			  let found1, found2 = List.split !found_st in
			  let sub1 = List.exists (fun fd1 -> tree1#initial_subtree_mem fd1 n1) found1 in
			  let sub2 = List.exists (fun fd2 -> tree2#initial_subtree_mem fd2 n2) found2 in
			  if not sub1 && not sub2 then begin
			    DEBUG_MSG "  subtree pair of size %d found: %a-%a" sz UID.ps n1#uid UID.ps n2#uid;
			    found_st := (n1, n2) :: !found_st
			  end
		      | _ -> ()
		    )
		with
		  Not_found -> ()
	      end;
	      let found_st1, found_st2 = List.split !found_st in
	      let issub1 n1 = List.exists (fun fd1 -> tree1#initial_subtree_mem fd1 n1) found_st1 in
	      let issub2 n2 = List.exists (fun fd2 -> tree2#initial_subtree_mem fd2 n2) found_st2 in
	      let found = ref [] in
	      begin
		let filt lgi gi n =
		  let g = n#gindex in
		  lgi <= g && g <= gi
		in
		try
		  let multiple_node_matches = cenv#multiple_node_matches in
		  multiple_node_matches#iter
		    (fun (_lab, ns1, ns2) ->
		      let ns1' = List.filter (filt lgi1 gi1) ns1 in
		      let ns2' = List.filter (filt lgi2 gi2) ns2 in
		      match ns1', ns2' with
		      | [n1], [n2] ->
			  if not (List.mem (n1, n2) !found) && not (issub1 n1) && not (issub2 n2) then begin
			    DEBUG_MSG "  node pair found: %a-%a" UID.ps n1#uid UID.ps n2#uid;
			    found := (n1, n2) :: !found
			  end
		      | _ -> ()
		    )
		with
		  Not_found -> ()
	      end;
	      !found_st @ !found
	    end
	    else 
	      cands
	  in (* cands *)


	  (* checking conflicts *)

	  DEBUG_MSG "checking conflicts...";

          let already_mapped_pairs = ref [] in

	  let cands =
	    Xlist.filter_map
	      (fun (n1, n2) ->
		let u1, u2 = n1#uid, n2#uid in

		if hardoverride then begin
		  DEBUG_MSG "hardoverride";
		  Some (n1, n2, true)
		end
		else begin
		  let score = ref (-1.0) in
		  let ncross = ref (-1) in

		  let defeated1 = ref false in
		  let defeated2 = ref false in

                  let already_mapped1 = ref false in
                  let already_mapped2 = ref false in

		  let cond1, dnc1(*, padj1*) =
		    try
		      let u1' = uidmapping#find u1 in

		      if u1' <> u2 then begin

			DEBUG_MSG "%a-%a conflicts with %a-%a"
			  UID.ps u1 UID.ps u2 UID.ps u1 UID.ps u1';

			let n1' = tree2#search_node_by_uid u1' in

			if 
			  n1#data#is_named_orig && 
			  n1'#data#is_named_orig && 
			  n1#data#eq n1'#data && 
			  not (n1#data#eq n2#data) 
			then
			  false, None(*, None*)
			else
			  let b = ref false in
			  let dnc = ref None in
			  cenv#compare_mappings uidmapping ~override ~bonus_self:true
			    n1 n1' (fun d -> dnc := d)
			    n1 n2 ~ncrossing_new:ncross ~adjacency_new:score
			    (fun d -> 
                              b := true; 
                              dnc := d; 
                              defeated1 := true;
                            );
			  !b, !dnc(*,
                          let p1 = n1#initial_parent in
                          let b =
                            cenv#get_adjacency_score p1 n1'#initial_parent <
                            cenv#get_adjacency_score p1 n2#initial_parent
                          in
                          DEBUG_MSG "padj1=%B" b;
                          Some b*)
		      end
		      else begin
                        already_mapped1 := true;
			false, None(*, None*)
                      end
		    with
		      Not_found -> true, None(*, None*)
		  in
		  let cond2, dnc2(*, padj2*) =
		    try
		      let u2' = uidmapping#inv_find u2 in

		      if u2' <> u1 then begin

			DEBUG_MSG "%a-%a conflicts with %a-%a"
			  UID.ps u1 UID.ps u2 UID.ps u2' UID.ps u2;

			let n2' = tree1#search_node_by_uid u2' in
			let b = ref false in
			let dnc = ref None in
			cenv#compare_mappings uidmapping ~override ~bonus_self:true
			  n2' n2 (fun d -> dnc := d)
			  n1 n2 ~ncrossing_new:ncross ~adjacency_new:score
			  (fun d -> 
                            b := true; 
                            dnc := d; 
                            defeated2 := true;
                          );
			!b, !dnc(*,
                        let p2 = n2#initial_parent in
                        let b =
                          cenv#get_adjacency_score n2'#initial_parent p2 <
                          cenv#get_adjacency_score n1#initial_parent p2
                        in
                        DEBUG_MSG "padj2=%B" b;
                        Some b*)
		      end
		      else begin
                        already_mapped2 := true;
			false, None(*, None*)
                      end
		    with
		      Not_found -> true, None(*, None*)
		  in
                  if !already_mapped1 && !already_mapped2 then
                    already_mapped_pairs := (n1, n2) :: !already_mapped_pairs;

                  let cond_p =
                    try
                      let pn1 = n1#initial_parent in
                      let pn2 = n2#initial_parent in
                      (is_cand pn1#uid pn2#uid) &&
                      pn1#data#get_name = n1#data#get_name &&
                      pn2#data#get_name = n2#data#get_name
                    with
                    | Not_found 
                    | Otreediff.Otree.Parent_not_found _ -> false
                  in

		  let c0 = cond1 && cond2 in
		  let c1 =
                    n1#data#_digest = n2#data#_digest(* ||
                    (not n1#data#is_named_orig && not n2#data#is_named_orig &&
                     n1#data#anonymized_label = n2#data#anonymized_label)*)
                  in
		  let c2 =
                    cond1 && not cond2 &&
                    match dnc1, dnc2 with
                    | Some d1, Some d2 -> d1 > d2
                    (*| _ when not n1#data#is_named_orig && not n2#data#is_named_orig ->
                        padj2 = Some true*)
                    | _ -> false
                  in
		  let c3 =
                    not cond1 && cond2 &&
                    match dnc1, dnc2 with
                    | Some d1, Some d2 -> d1 < d2
                    (*| _ when not n1#data#is_named_orig && not n2#data#is_named_orig ->
                        padj1 = Some true*)
                    | _ -> false
                  in

		  let cond = cond_p || c0 || (c1 && (c2 || c3)) in

                  BEGIN_DEBUG
		    let dnc_to_str = function
		      | Some d -> string_of_int d
		      | None -> "-"
		    in
		    DEBUG_MSG "%a-%a -> cond_p:%B cond1:%B cond2:%B c0:%B c1:%B c2:%B c3:%B dnc1:%s dnc2:%s --> cond:%b"
		      UID.ps u1 UID.ps u2 cond_p cond1 cond2 c0 c1 c2 c3 (dnc_to_str dnc1) (dnc_to_str dnc2) cond;
                  END_DEBUG;

                  if cond then
		    Some (n1, n2, !defeated1 && !defeated2)
		  else
		    None

		end (* of if hardoverride else *)

	      ) cands
	  in (* cands *)

	  DEBUG_MSG "cands: %s" 
	    (String.concat "" 
	       (List.map 
		  (fun (n1, n2, defeated) -> 
		    sprintf "<%a-%a%s>" UID.ps n1#uid UID.ps n2#uid (if defeated then "(defeated)" else "")
		  ) cands));

	  DEBUG_MSG "already mapped pairs (no conflict): %s" 
	    (String.concat "" 
	       (List.map 
		  (fun (n1, n2) -> 
		    sprintf "<%a-%a>" UID.ps n1#uid UID.ps n2#uid
		  ) !already_mapped_pairs));

	  if cands = [] then begin
	    let cld1_all_not_mapped = List.filter (fun n -> (nmapped_of_subtree1 n) = 0) cld1 in
	    let cld2_all_not_mapped = List.filter (fun n -> (nmapped_of_subtree2 n) = 0) cld2 in
	    match cld1_all_not_mapped, cld2_all_not_mapped with
	    | [n1], [n2] -> scan_down (score_up, score_down) n1#uid n2#uid
	    | _ -> (* () *) 
                List.iter (* experimental *)
                  (fun (n1, n2) -> 
                    let cond = 
                      (not (n1#initial_children = [||]) || not (n2#initial_children = [||])) &&
(*                      (n1#data#eq n2#data) && *)
                      (try
                        let _ns1, _ns2 = cenv#multiple_node_matches#find n1#data#_label in
                        let ns1 = List.filter (fun n -> not (uidmapping#mem_dom n#uid)) _ns1 in
                        let ns2 = List.filter (fun n -> not (uidmapping#mem_cod n#uid)) _ns2 in
                        DEBUG_MSG "ns1: [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" ns1);
                        DEBUG_MSG "ns2: [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" ns2);
                        match ns1, ns2 with
                        | [], [] | [_], [_] -> true
                        | _ -> false
                      with
                        Not_found -> true
                      )
                    in
                    if cond then
                      scan_down (score_up, score_down) n1#uid n2#uid
                  ) !already_mapped_pairs
	  end
	  else begin
	    List.iter
	      (fun (cnd1, cnd2, defeated) ->
		let u1, u2 = cnd1#uid, cnd2#uid in

		DEBUG_MSG "glue cand (scan_down): %a-%a" UID.ps u1 UID.ps u2;

		let bonus, is_ok =
		  if cnd1#data#equals cnd2#data then 
		    calc_bonus cnd1 cnd2, true
		  else 
		    cenv#eval_label_match cnd1 cnd2, relabel_allowed cnd1 cnd2
		in
		if is_ok then begin
		  let base = 
		    if single_children (* is_unique_pair cnd1 cnd2 *) then 
		      score_up 
		    else 
		      score_down 
		  in

		  DEBUG_MSG "base score: %d, bonus: %d" base bonus;

		  let score = base + bonus in

		  if cnd1#data#subtree_equals cnd2#data then begin
		    let nds1 = ref [] in
		    let nds2 = ref [] in
		    tree1#fast_scan_whole_initial_subtree cnd1 (fun n -> nds1 := n::!nds1);
		    tree2#fast_scan_whole_initial_subtree cnd2 (fun n -> nds2 := n::!nds2);
		    List.iter2 
		      (fun n1 n2 -> 
			let u1, u2 = n1#uid, n2#uid in
			add_cand "no tree diff" n1 n2 u1 u2 score
		      ) !nds1 !nds2;
		    if cnd1#initial_nchildren > 0 then
    		      scan_up ~reflex:true (score_up, score_down)
                        cnd1#initial_children.(0)#uid cnd2#initial_children.(0)#uid
		  end
		  else begin
		    add_cand "no tree diff" cnd1 cnd2 u1 u2 score;
		    if 
		      (cnd1#initial_nchildren = 0 && cnd2#initial_nchildren = 0) (* ||
										    to_be_focused *)
		    then
		      scan_up ~reflex:true (score_up, score_down) cnd1#uid cnd2#uid
		    else
		      scan_down ~hardoverride:defeated (score_up, score_down) u1 u2
		  end
		end
	      ) cands;

	    if (* override *) true then
	      let cands1, cands2, _ = Xlist.split3 cands in
	      match Xlist.subtractq cld1 cands1, Xlist.subtractq cld2 cands2 with
	      | [n1], [n2] ->
		  if nmapped_of_subtree1 n1 = 0 || nmapped_of_subtree2 n2 = 0 then
		    scan_down ~force_treediff:true (score_up, score_down) n1#uid n2#uid
	      | _ -> ()
	  end

	end (* of tree diff not used *)

      end (* not scanned yet *)

    in (* end of func scan_down *)

    uidmapping#print_status;


(*    cenv#compare_mappings_cache_begin; *)

    let starting_uid_pairs =
      List.fold_left
       (fun l (u1, u2) ->
         if is_bad_pair u1 u2 then
           l
         else
           (u1, u2)::l
       ) [] uidmapping#starting_uid_pairs_for_glueing
    in

    let upward_only =
      if options#simple_glue_flag then
        true
      else
        false
    in
    (*let starting_uid_pairs =
      if not options#simple_glue_flag then
        starting_uid_pairs
      else if starting_uid_pairs = [] then begin
        let s_up = Xset.create 0 in
        let s_down = Xset.create 0 in
        uidmapping#iter_unsettled
	  (fun u1 u2 ->
            let n1 = tree1#search_node_by_uid u1 in
            let n2 = tree2#search_node_by_uid u2 in
            (*if
              not (n1#data#is_named_orig && n1#data#anonymized2_label = n2#data#anonymized2_label)
            then
              let _ = () in*)
              Xset.add s_up n1;
            (*let f = ref (fun () -> ()) in
            begin
              try
                Xset.iter
                  (fun x ->
                    if tree1#is_initial_ancestor x n1 then begin
                      f := (fun () -> Xset.remove s_up x; Xset.add s_up n1);
                      raise Exit
                    end
                    else if tree1#is_initial_ancestor n1 x then
                      raise Exit
                  ) s_up;
                Xset.add s_up n1
              with
                Exit -> (!f)()
            end;*)

            if downward then
              Xset.add s_down n1
              (*let f = ref (fun () -> ()) in
              begin
                try
                  Xset.iter
                    (fun x ->
                      if tree1#is_initial_ancestor n1 x then begin
                        f := (fun () -> Xset.remove s_down x; Xset.add s_down n1);
                        raise Exit
                      end
                      else if tree1#is_initial_ancestor x n1 then
                        raise Exit
                    ) s_down;
                  Xset.add s_down n1
                with
                  Exit -> (!f)()
              end;*)
          );
        Xprint.verbose options#verbose_flag "starting pairs for upward glueing: %d" (Xset.length s_up);
        Xprint.verbose options#verbose_flag "starting pairs for downward glueing: %d" (Xset.length s_down);
        (*Xprint.verbose options#verbose_flag "for down: %d" (Xset.length s_down);*)
        let l = ref [] in
        uidmapping#iter_unsettled
	  (fun u1 u2 ->
            let n1 = tree1#search_node_by_uid u1 in
            if Xset.mem s_up n1 || (downward && Xset.mem s_down n1) then
              l := (u1, u2) :: !l
          );
        !l
      end
      else
        starting_uid_pairs
    in*)
    Xprint.verbose options#verbose_flag
      "%d starting_uid_pairs, upward_only=%B" (List.length starting_uid_pairs) upward_only;

(*
    let starting_uid_pairs =
      List.fast_sort
        (fun (u10, u20) (u11, u21) ->
          let n10 = tree1#search_node_by_uid u10 in
          let n11 = tree1#search_node_by_uid u11 in
          let gi10, gi11 = n10#gindex, n11#gindex in
          let c = Stdlib.compare gi10 gi11 in
          if c = 0 then
	    let n20 = tree2#search_node_by_uid u20 in
	    let n21 = tree2#search_node_by_uid u21 in
            let gi20, gi21 = n20#gindex, n21#gindex in
            Stdlib.compare gi20 gi21
          else
            c
        ) starting_uid_pairs
    in
*)
    uidmapping#set_starting_uid_pairs_for_glueing starting_uid_pairs;

    if starting_uid_pairs = [] then begin
      uidmapping#iter_unsettled (*_sorted Stdlib.compare *)
	(fun u1 u2 ->
	  let s = get_scoring u1 u2 in
	  scan_up s u1 u2;
          if first || not upward_only then
	    scan_down s u1 u2
        );

      uidmapping#iter_settled_roots (*_sorted Stdlib.compare *)
	(fun u1 u2 -> 
	  let s = get_scoring u1 u2 in
	  scan_up s u1 u2)
    end
    else begin

      BEGIN_DEBUG
	List.iter
	  (fun (u1, u2) -> DEBUG_MSG "starting_uid_pair: %a-%a" UID.ps u1 UID.ps u2)
	  starting_uid_pairs;

	let starting_node_pairs = 
	  List.fast_sort (fun (n1, _) (n2, _) -> Stdlib.compare n1#gindex n2#gindex)
	    (List.map
	       (fun (u1, u2) ->
		 let n1 = tree1#search_node_by_uid u1 in
		 let n2 = tree2#search_node_by_uid u2 in
		 n1, n2
	       ) starting_uid_pairs
	    )
	in
	List.iter
	  (fun (n1, n2) -> 
	    DEBUG_MSG "starting_uid_pair (gindex): %a-%a" GI.ps n1#gindex GI.ps n2#gindex
	  ) starting_node_pairs;
      END_DEBUG;

      List.iter
	(fun (u1, u2) ->
	  let s = get_scoring u1 u2 in
          if not downward then
	    scan_up s u1 u2;
          if not upward_only || downward then
	    scan_down s u1 u2
	) starting_uid_pairs

    end;

    let sorted_cands =
      List.fast_sort (fun (_, s1) (_, s2) -> compare !s2 !s1) !cands
    in

    BEGIN_DEBUG
      DEBUG_MSG "%d CANDIDATES (sorted by score):" (List.length sorted_cands);
      List.iter 
	(fun ((uid1, uid2), score) -> 
	  DEBUG_MSG "%a-%a (score=%d)" UID.ps uid1 UID.ps uid2 !score
	) sorted_cands;
    END_DEBUG;


    (* solving conflict *)

    let score_tbl = Hashtbl.create 0 in
    let tbl1 = Hashtbl.create 0 in
    let tbl2 = Hashtbl.create 0 in

    let tbl_add t u =
      try
	let c = Hashtbl.find t u in
	Hashtbl.replace t u (c + 1)
      with Not_found ->
	Hashtbl.add t u 1
    in
    let get_count t u =
      try
	Hashtbl.find t u
      with
	Not_found -> 0
    in
    
    List.iter
      (fun ((u1, u2) as up, x) ->
	tbl_add tbl1 u1;
	tbl_add tbl2 u2;
        Hashtbl.add score_tbl up x
      ) sorted_cands;

    let get_parent_pair_adj_gi (u1, u2) =
      let n1 = tree1#search_node_by_uid u1 in
      let n2 = tree2#search_node_by_uid u2 in
      let p1 = n1#initial_parent in
      let p2 = n2#initial_parent in
      let adj = cenv#get_adjacency_score p1 p2 in
      (p1#uid, p2#uid), adj, (p1#gindex, p2#gindex)
    in
    let get_score up = Hashtbl.find score_tbl up in

    let conflicting_cands, good_cands = 
      List.fold_left
	(fun (conflicting, good) ((u1, u2), s) ->
	  if (get_count tbl1 u1) > 1 || (get_count tbl2 u2) > 1 then
	    ((u1, u2), !s)::conflicting, good
	  else
	    conflicting, ((u1, u2), !s)::good;

	) ([], []) sorted_cands
    in

    BEGIN_DEBUG
      DEBUG_MSG "%d GOOD CANDIDATES (no conflicts):" (List.length good_cands);
      List.iter 
	(fun ((uid1, uid2), s) -> 
	  DEBUG_MSG "%a-%a (score=%d)" UID.ps uid1 UID.ps uid2 s
	) good_cands;
    END_DEBUG;

    let rec cmp_score ?(top=true) (up1, (s1, adj1, gip1)) (up2, (s2, adj2, gip2)) =
      let c1 = Stdlib.compare s2 s1 in
      if c1 = 0 then
	let c2 = Stdlib.compare adj2 adj1 in
	if c2 = 0 then
          try
            if not top then raise Exit;
            let pup1, adj1, pgi1 = get_parent_pair_adj_gi up1 in
            let pup2, adj2, pgi2 = get_parent_pair_adj_gi up2 in
            let ps1 = get_score pup1 in
            let ps2 = get_score pup2 in
            cmp_score ~top:false (pup1, (!ps1, adj1, pgi1)) (pup2, (!ps2, adj2, pgi2))
          with
	    _ -> Stdlib.compare gip2 gip1
	else
	  c2
      else
	c1
    in

    let sorted_conflicting_cands =
      let l =
	List.map
	  (fun ((u1, u2), s) ->
            if record_conflicted_pairs then
              Xset.add conflicted_pairs (u1, u2);
	    let n1 = tree1#search_node_by_uid u1 in
	    let n2 = tree2#search_node_by_uid u2 in
	    let adj = cenv#get_adjacency_score n1 n2 in
	    ((u1, u2), (s, adj, (n1#gindex, n2#gindex)))
	  ) conflicting_cands
      in
      List.fast_sort cmp_score l
    in

    BEGIN_DEBUG
      DEBUG_MSG "%d CONFLICTING CANDIDATES (sorted by score, adjacency, and gindexes):" 
	(List.length sorted_conflicting_cands);
      List.iter 
      (fun ((uid1, uid2), (s, adj, (gi1, gi2))) ->
        DEBUG_MSG "%a-%a (%d, %f, (%a-%a))" UID.ps uid1 UID.ps uid2 s adj GI.ps gi1 GI.ps gi2
      ) sorted_conflicting_cands;
    END_DEBUG;

    Hashtbl.clear tbl1;
    Hashtbl.clear tbl2;

    let _umap = Hashtbl.create 0 in
    let _is_mapped u1 u2 =
      try
        Hashtbl.find _umap u1 = u2
      with
        _ -> false
    in
    let _set1 = Xset.create 0 in
    let _set2 = Xset.create 0 in
    let _are_named_orig u1 u2 =
      let n1 = tree1#search_node_by_uid u1 in
      let n2 = tree2#search_node_by_uid u2 in
      n1#data#is_named_orig && n2#data#is_named_orig
    in

    let resolved_cands, unresolved_cands =
      List.partition
        (fun ((u1, u2), (s, adj, gip)) ->
          if (get_count tbl1 u1) = 0 && (get_count tbl2 u2) = 0 then begin
            if
              not (Xset.mem _set1 u1) && not (Xset.mem _set2 u2) ||
              not options#dump_delta_flag && _are_named_orig u1 u2 ||
              _is_mapped u1 u2
            then begin
              tbl_add tbl1 u1;
              tbl_add tbl2 u2;
              Hashtbl.add _umap u1 u2;
              begin
                try
                  let n1 = tree1#search_node_by_uid u1 in
	          let n2 = tree2#search_node_by_uid u2 in
                  let p1 = n1#initial_parent in
                  let p2 = n2#initial_parent in
                  Xset.add _set1 p1#uid;
                  Xset.add _set2 p2#uid;
                  Hashtbl.add _umap p1#uid p2#uid;
                with
                  _ -> ()
              end;
              true
            end
            else
              false
          end
          else
            false
        ) sorted_conflicting_cands
    in
    let additionally_resolved_cands =
      List.filter
        (fun ((u1, u2), (s, adj, gip)) ->
          if (get_count tbl1 u1) = 0 && (get_count tbl2 u2) = 0 then begin
            let n1 = tree1#search_node_by_uid u1 in
	    let n2 = tree2#search_node_by_uid u2 in
            try
              _is_mapped n1#uid n2#uid &&
              _is_mapped n1#initial_parent#uid n2#initial_parent#uid
            with
              _ -> false
          end
          else
            false
        ) unresolved_cands
    in

    BEGIN_DEBUG
      DEBUG_MSG "%d CANDIDATES (resolved):" (List.length resolved_cands);
      List.iter
      (fun ((uid1, uid2), (s, adj, (gi1, gi2))) ->
          DEBUG_MSG "%a-%a (%d, %f, (%a-%a))" UID.ps uid1 UID.ps uid2 s adj GI.ps gi1 GI.ps gi2
        ) resolved_cands;
      DEBUG_MSG "%d CANDIDATES (additionally resolved):" (List.length additionally_resolved_cands);
      List.iter
	(fun ((uid1, uid2), (s, adj, (gi1, gi2))) ->
	  DEBUG_MSG "%a-%a (%d, %f, (%a-%a))" UID.ps uid1 UID.ps uid2 s adj GI.ps gi1 GI.ps gi2
	) additionally_resolved_cands;
    END_DEBUG;

    let resolved_cands = resolved_cands @ additionally_resolved_cands in

    let good_cands = List.map (fun (p, _) -> p) good_cands in
    let resolved_cands = List.map (fun (p, _) -> p) resolved_cands in

    let final_cands = good_cands @ resolved_cands in

    BEGIN_DEBUG
      DEBUG_MSG "%d CANDIDATES (final):" (List.length final_cands);
      List.iter 
	(fun (uid1, uid2) -> 
	  DEBUG_MSG "%a-%a" UID.ps uid1 UID.ps uid2) final_cands;
    END_DEBUG;


    let removed_pairs = ref [] in
    let added_pairs = ref [] in

    let is_crossing_with_added n1 n2 =
      let b =
        try
          List.iter
            (fun (u1', u2') ->
              let n1' = tree1#search_node_by_uid u1' in
              let n2' = tree2#search_node_by_uid u2' in
              if UIDmapping.is_crossing_or_incompatible tree1 tree2 n1 n2 n1' n2' then
                raise Exit
            ) !added_pairs;
          false
        with
          Exit -> true
      in
      DEBUG_MSG "%a %a -> %B" nps n1 nps n2 b;
      b
    in

    List.iter (* add mappings *)
      (fun (uid1, uid2) ->

	DEBUG_MSG "adding %a-%a" UID.ps uid1 UID.ps uid2;

	if uidmapping#has_mapping uid1 uid2 then begin
	  DEBUG_MSG "already in mapping: %a-%a" UID.ps uid1 UID.ps uid2
	end
	else
	  let nd1 = tree1#search_node_by_uid uid1 in
	  let nd2 = tree2#search_node_by_uid uid2 in

	  let to_be_removed = ref [] in
	  let ncross = ref (-1) in
	  let adj = ref (-1.0) in

	  let can_add1, dnc1, padj1 =
	    try
	      let u2 = uidmapping#find uid1 in
	      if u2 <> uid2 then begin

		DEBUG_MSG "conflict: %a->%a" UID.ps uid1 UID.ps u2;

		if uidmapping#is_stable_pair uid1 u2 then begin
		  DEBUG_MSG "         --> stable";
		  false, None, None
		end
		else begin
		  to_be_removed := (uid1, u2) :: !to_be_removed;
		  let n2 = tree2#search_node_by_uid u2 in
		  let b = ref false in
		  let dnc = ref None in
		  cenv#compare_mappings uidmapping ~override
		    nd1 n2 (fun d -> dnc := d)
		    nd1 nd2 ~ncrossing_new:ncross ~adjacency_new:adj
		    (fun d -> 
		      b := true;
		      dnc := d
		    );
		  !b, !dnc,
                  let p1 = nd1#initial_parent in
                  let b =
                    cenv#get_adjacency_score p1 n2#initial_parent <
                    cenv#get_adjacency_score p1 nd2#initial_parent
                  in
                  DEBUG_MSG "padj1=%B" b;
                  Some b
		end
	      end
	      else (* u2 = uid2 *)
		false, None, None
	    with 
	      Not_found -> not no_moves || not (is_crossing_with_added nd1 nd2), None, None
	  in
	  let can_add2, dnc2, padj2 =
	    try
	      let u1 = uidmapping#inv_find uid2 in
	      if u1 <> uid1 then begin

		DEBUG_MSG "conflict: %a<-%a" UID.ps u1 UID.ps uid2;

		if uidmapping#is_stable_pair u1 uid2 then begin
		  DEBUG_MSG "         --> stable";
		  false, None, None
		end
		else begin
		  to_be_removed := (u1, uid2) :: !to_be_removed;
		  let n1 = tree1#search_node_by_uid u1 in
		  let b = ref false in
		  let dnc = ref None in
		  cenv#compare_mappings uidmapping ~override
		    n1 nd2 (fun d -> dnc := d)
		    nd1 nd2 ~ncrossing_new:ncross ~adjacency_new:adj
		    (fun d -> 
		      b := true;
		      dnc := d
		    );
		  !b, !dnc,
                  let p2 = nd2#initial_parent in
                  let b =
                    cenv#get_adjacency_score n1#initial_parent p2 <
                    cenv#get_adjacency_score nd1#initial_parent p2
                  in
                  DEBUG_MSG "padj2=%B" b;
                  Some b
		end
	      end
	      else (* u1 = uid1 *)
		false, None, None
	    with 
	      Not_found -> not no_moves || not (is_crossing_with_added nd1 nd2), None, None
	  in
          BEGIN_DEBUG
            let dnc_to_str = function
	      | Some d -> string_of_int d
	      | None -> "-"
	    in
            DEBUG_MSG "can_add1=%B can_add2=%B dnc1:%s dnc2:%s"
              can_add1 can_add2 (dnc_to_str dnc1) (dnc_to_str dnc2);
          END_DEBUG;
	  if 
	    (can_add1 && can_add2) ||
	    ((nd1#data#_digest = nd2#data#_digest ||
            (not nd1#data#is_named_orig && not nd2#data#is_named_orig &&
             nd1#data#anonymized_label = nd2#data#anonymized_label)) &&
	     ((can_add1 && not can_add2 &&
               match dnc1, dnc2 with
               | Some d1, Some d2 -> d1 > d2
               | None, Some _ -> padj2 = Some true
               | _ when not nd1#data#is_named_orig && not nd2#data#is_named_orig ->
                   padj2 = Some true
               | _ -> false
              ) ||
	     (not can_add1 && can_add2 &&
              match dnc1, dnc2 with
              | Some d1, Some d2 -> d1 < d2
              | Some _, None -> padj1 = Some true
              | _ when not nd1#data#is_named_orig && not nd2#data#is_named_orig ->
                  padj1 = Some true
              | _ -> false
             )))
	  then begin
            List.iter
              (fun (u1, u2) ->
                if u1 <> uid1 then begin
                  uidmapping#remove u1 u2;
                  DEBUG_MSG "removed %a-%a" UID.ps u1 UID.ps u2
                end
              ) !to_be_removed;
            ignore (uidmapping#add_unsettled uid1 uid2);

	    removed_pairs := !to_be_removed @ !removed_pairs;
	    added_pairs := (uid1, uid2) :: !added_pairs;

	    DEBUG_MSG "added %a-%a (%a-%a)" 
	      UID.ps uid1 UID.ps uid2 GI.ps nd1#gindex GI.ps nd2#gindex;
	  end;

      ) final_cands;

(*    cenv#compare_mappings_cache_end; *)

    BEGIN_DEBUG
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
      List.iter
        (fun (u1, u2) -> 
          let n1 = tree1#search_node_by_uid u1 in
          let n2 = tree2#search_node_by_uid u2 in
          DEBUG_MSG "added_pair: %a-%a (%a-%a)" UID.ps u1 UID.ps u2 GI.ps n1#gindex GI.ps n2#gindex
        ) !added_pairs;
    END_DEBUG;

    Xprint.verbose options#verbose_flag "glueing completed.";

    !removed_pairs, !added_pairs, conflicted_pairs
  (* end of func glue_deletes_and_inserts *)


  let sync_edits
      options
      ?(is_mov=fun n1 n2 -> false, None)
      ?(check_conflicts=false)
      tree1 tree2 edits removed_pairs added_pairs
      =
    let mid_gen = options#moveid_generator in

    DEBUG_MSG "* REMOVING EDITS...";

    List.iter
      (fun (u1, u2) ->
        DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;
	List.iter edits#remove_edit (edits#find12 u1 u2);
	let n1 = tree1#search_node_by_uid u1 in
	let n2 = tree2#search_node_by_uid u2 in
        let del = Edit.make_delete n1 in
        DEBUG_MSG "adding %s" (Edit.to_string del);
	edits#add_edit del;
        let ins = Edit.make_insert n2 in
        DEBUG_MSG "adding %s" (Edit.to_string ins);
	edits#add_edit ins
      ) removed_pairs;

    DEBUG_MSG "* ADDING EDITS...";

    let pending_pairs = ref [] in

    List.iter
      (fun (u1, u2) ->
        DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;
        let n1 = tree1#search_node_by_uid u1 in
        let n2 = tree2#search_node_by_uid u2 in
        begin
          try
            let del = edits#find_del u1 in
            DEBUG_MSG "removing %s" (Edit.to_string del);
            edits#remove_edit del
          with
            Not_found -> ()
        end;
        begin
          try
            let ins = edits#find_ins u2 in
            DEBUG_MSG "removing %s" (Edit.to_string ins);
            edits#remove_edit ins;
          with
            Not_found -> ()
        end;
        if not (n1#data#eq n2#data) then
          edits#add_edit (Edit.make_relabel n1 n2);

        let b, mid_opt = is_mov n1 n2 in
        DEBUG_MSG "is_mov: %a-%a --> %B" UID.ps n1#uid UID.ps n2#uid b;

        if b then begin
          let info1, info2 = mkinfo n1, mkinfo n2 in
          let mid =
            match mid_opt with
            | Some mid -> mid
            | _ -> mid_gen#gen
          in
          edits#add_edit (Edit.make_move_permutation mid (u1, info1) (u2, info2));
        end
        else if check_conflicts then
          pending_pairs := (n1, n2) :: !pending_pairs

      ) added_pairs;

    if check_conflicts then begin
      let a = Array.of_list !pending_pairs in
      let len = Array.length a in
      let ca = Array.make len 0 in
      let li = len - 1 in
      let conflicts = ref [] in
      for i = 0 to li do
        let (n1, n2) as p = a.(i) in
        for j = i + 1 to li do
          let (n1', n2') as p' = a.(j) in
          let b = UIDmapping.is_crossing_or_incompatible tree1 tree2 n1 n2 n1' n2' in
          DEBUG_MSG "conflicts: (%a,%a) vs (%a,%a) --> %B"
            UID.ps n1#uid UID.ps n2#uid UID.ps n1'#uid UID.ps n2'#uid b;
          if b then begin
            conflicts := (p, p') :: !conflicts;
            ca.(i) <- ca.(i) + 1;
            ca.(j) <- ca.(j) + 1;
          end
        done
      done;
      let cpl = List.combine (Array.to_list ca) !pending_pairs in
      let cpl = List.fast_sort (fun (c0, _) (c1, _) -> compare c1 c0) cpl in
      DEBUG_MSG "cpl=[%s]"
        (String.concat ""
           (List.map
              (fun (c,(n1,n2)) -> sprintf "<%d,(%a,%a)>" c UID.ps n1#uid UID.ps n2#uid) cpl));
      let movs = ref [] in
      if !conflicts <> [] then begin
        try
          List.iter
            (fun (_, p) ->

              conflicts :=
                List.fold_left
                  (fun l (p0, p1) ->
                    if p == p0 || p == p1 then
                      l
                    else
                      (p0, p1)::l
                  ) [] !conflicts;

              movs := p :: !movs;

              if !conflicts = [] then
                raise Exit

            ) cpl
        with
          Exit -> ()
      end;
      DEBUG_MSG "movs=[%s]"
        (String.concat ", "
           (List.map
              (fun (n1,n2) -> sprintf "(%a,%a)" UID.ps n1#uid UID.ps n2#uid) !movs));

      List.iter
        (fun (n1, n2) ->
          let info1, info2 = mkinfo n1, mkinfo n2 in
          let mid = mid_gen#gen in
          edits#add_edit (Edit.make_move_permutation mid (n1#uid, info1) (n2#uid, info2));
        ) !movs
    end
  (* end of func sync_edits *)


  let generate_edits ?(simple=false)
      options
      lang 
      (cenv : (node_t, tree_t) Comparison.c)
      pruned 
      edits 
      (uidmapping : node_t UIDmapping.c)
      =
    let tree1 = cenv#tree1 in
    let tree2 = cenv#tree2 in

    DEBUG_MSG "simple=%B" simple;

    DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
    (*DEBUG_MSG "uidmapping (gindex):\n%s\n" uidmapping#to_string_gid;*)


    (* generate deletes and relabels *)
    tree1#fast_scan_whole_initial
      (fun nd ->
	let uid = nd#uid in

	DEBUG_MSG "scanning %a" UID.ps uid;
	try
	  let uid' = uidmapping#find uid in
	  let nd' = tree2#search_node_by_uid uid' in
	  if not (nd#data#eq nd'#data) then begin
            let rel = Edit.make_relabel nd nd' in
            DEBUG_MSG "%s" (Edit.to_string rel);
	    edits#add_edit rel
          end
	with 
	  Not_found ->
            let del = Edit.make_delete nd in
            DEBUG_MSG "%s" (Edit.to_string del);
            edits#add_edit del
      );
    (* generate inserts *)
    tree2#fast_scan_whole_initial
      (fun nd ->
	let uid = nd#uid in

	DEBUG_MSG "scanning %a" UID.ps uid;

	if not (uidmapping#mem_cod uid) then begin
          let ins = Edit.make_insert nd in
          DEBUG_MSG "%s" (Edit.to_string ins);
	  edits#add_edit ins
        end
	else
	  DEBUG_MSG "%a <- %a" UID.ps (uidmapping#inv_find uid) UID.ps uid
      );

    DEBUG_MSG "[0] edits: del:%d ins:%d rel:%d" 
      edits#get_ndeletes edits#get_ninserts edits#get_nrelabels;

    let sync_edits = sync_edits options tree1 tree2 edits in

    if not simple then begin
      begin
	match lang#elaborate_edits with
	| Some f ->


            DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
            (*DEBUG_MSG "uidmapping (gindex):\n%s\n" uidmapping#to_string_gid;*)


	    f options cenv uidmapping edits;
(*
  DEBUG_MSG "[1] edits: del:%d ins:%d rel:%d" 
  edits#get_ndeletes edits#get_ninserts edits#get_nrelabels;
 *)

	    DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;

	    let removed_pairs, added_pairs = cenv#elaborate_uidmapping uidmapping in

	    uidmapping#add_starting_uid_pairs_for_glueing added_pairs;

	    sync_edits removed_pairs added_pairs;
(*
  DEBUG_MSG "[2] edits: del:%d ins:%d rel:%d" 
  edits#get_ndeletes edits#get_ninserts edits#get_nrelabels;
 *)
if not options#no_glue_flag then begin
	    let removed_pairs, added_pairs, conflicted_pairs =
	      glue_deletes_and_inserts options cenv tree1 tree2 
                ~override:true ~no_moves:options#no_moves_flag uidmapping (new UIDmapping.c cenv)
	    in
	    sync_edits removed_pairs added_pairs;
end;
(*
  DEBUG_MSG "[3] edits: del:%d ins:%d rel:%d" 
  edits#get_ndeletes edits#get_ninserts edits#get_nrelabels;
 *)


	| None -> ()
      end;

      let removed_pairs, added_pairs =
	cenv#elaborate_uidmapping ~multi:true ~multi_node:true uidmapping
      in
      uidmapping#set_starting_uid_pairs_for_glueing added_pairs;

      sync_edits removed_pairs added_pairs;

if not options#no_glue_flag then begin
      let removed_pairs, added_pairs, conflicted_pairs =
	glue_deletes_and_inserts options cenv tree1 tree2
          ~no_moves:options#no_moves_flag uidmapping (new UIDmapping.c cenv)
      in
      sync_edits removed_pairs added_pairs;
end;

    end;

    generate_moves options tree1 tree2 pruned edits uidmapping
  (* end of func generate_edits *)



  let postprocess 
      options
      (cenv : (node_t, tree_t) Comparison.c)
      tree1
      tree2
      (uidmapping : node_t UIDmapping.c)
      pruned 
      ref_uidmapping 
      =
    let tree1 = cenv#tree1 in
    let tree2 = cenv#tree2 in

    let aborted1, aborted2 = pruned#aborted1, pruned#aborted2 in
    let para_iso1, para_iso2 = pruned#para_iso1, pruned#para_iso2 in

    (* recover pruned nodes other than settled isos 
     * and expands all collapsed nodes 
     *)

    let filter_para_iso tree aborted para_iso =

      DEBUG_MSG "size of para_iso: %d" 
	(List.length para_iso);

      List.filter
	(fun nd -> 
	  let lm = tree#initial_leftmost nd in
	  let contain_aborted =
	    List.exists 
	      (fun n -> 
		let ngi = n#gindex in
		lm#gindex <= ngi && ngi <= nd#gindex
	      ) aborted
	  in

	  if contain_aborted then
	    DEBUG_MSG "(%a): contains aborted subtree" 
	      UID.ps nd#uid;

	  contain_aborted
	) para_iso
    in (* end of func filter_para_iso *)
    
    let expand_substances para_iso =
      List.flatten
	(List.map 
	   (fun nd ->
	     if nd#in_path then begin
	       let s = nd#get_substances in
	       DEBUG_MSG "in_path: %a -> [%s]" 
		 UID.ps nd#uid
		 (Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" s);
	       s
	     end
	     else [nd]
	   ) para_iso)
    in

    let para_iso1 = 
      expand_substances
	(filter_para_iso tree1 aborted1 para_iso1)
    in
    let para_iso2 = 
      expand_substances
	(filter_para_iso tree2 aborted2 para_iso2)
    in

    BEGIN_DEBUG
      DEBUG_MSG "para_iso1: [%s]" 
	(Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" para_iso1);
      DEBUG_MSG "aborted1: [%s]" 
	(Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" aborted1);
      DEBUG_MSG "para_iso2: [%s]" 
	(Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" para_iso2);
      DEBUG_MSG "aborted2: [%s]" 
	(Xlist.to_string (fun nd -> UID.to_string nd#uid) ";" aborted2);
    END_DEBUG;

    let filt1 nd = List.memq nd aborted1 || List.memq nd para_iso1 in
    let filt2 nd = List.memq nd aborted2 || List.memq nd para_iso2 in
    tree1#recover_filtered filt1;
    tree2#recover_filtered filt2;

    let (* is_expanded1a *) _ = tree1#expand_all in
    let (* is_expanded2a *) _ = tree2#expand_all in

    BEGIN_DEBUG
      DEBUG_MSG "* BEFORE POSTPROCESSING(EE,RE,GLUE) *\n";
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
      DEBUG_MSG "T1:\n%s\n" tree1#to_string;
      DEBUG_MSG "T2:\n%s\n" tree2#to_string;
    END_DEBUG;


    (* 
     * eliminate enclaves
     *)
    if not options#no_enclave_elim_flag then begin
      Xprint.verbose options#verbose_flag "  eliminating enclaves...";

      let _ = cenv#elaborate_uidmapping uidmapping in

(*      check_toplevel tree1 tree2 uidmapping; *)

      DEBUG_MSG "uidmapping:\n%s" uidmapping#to_string;
      (*DEBUG_MSG "uidmapping (gindex):\n%s" uidmapping#to_string_gid*);


      let keyroots_large, keyroots_moderate = 
	find_keyroots options tree1 tree2 uidmapping 
      in
      let keyroots = 
        List.fast_sort (fun (n1, _) (n2, _) -> Stdlib.compare n2#gindex n1#gindex)
	  (if options#lower_ee_flag then 
	    keyroots_moderate
	  else 
	    keyroots_large)
      in

      BEGIN_DEBUG
	List.iter 
	  (fun (n1, n2) -> 
	    DEBUG_MSG "keyroot pair: %a(size=%d) - %a(size=%d)"
	      UID.ps n1#uid (tree1#whole_initial_subtree_size n1)
	      UID.ps n2#uid (tree2#whole_initial_subtree_size n2)
	  ) keyroots;
      END_DEBUG;

      eliminate_enclaves options cenv keyroots tree1 tree2 uidmapping ref_uidmapping;

      Xprint.verbose options#verbose_flag "  enclaves eliminated."

    end;

    let (* is_expanded1b *) _ = tree1#recover_and_expand in
    let (* is_expanded2b *) _ = tree2#recover_and_expand in

(*
    let is_expanded1 uid = is_expanded1a uid || is_expanded1b uid in
    let is_expanded2 uid = is_expanded2a uid || is_expanded2b uid in
*)

    (* 
     * eliminate relabels
     *)
    if not options#no_relabel_elim_flag then begin
      Xprint.verbose options#verbose_flag "  eliminating odd relabels...";
      eliminate_relabels tree1 tree2 uidmapping;
      Xprint.verbose options#verbose_flag "  odd relabels eliminated."
    end;

    BEGIN_DEBUG
      DEBUG_MSG "* BEFORE GLUEING *";
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
      (*DEBUG_MSG "uidmapping (gindex):%s" uidmapping#to_string_gid;*)
      DEBUG_MSG "T1:\n%s" tree1#to_string; 
      DEBUG_MSG "\nT2:\n%s" tree2#to_string;
    END_DEBUG;

    let _ = cenv#elaborate_uidmapping(* ~multi:true ~multi_node:true*) uidmapping in

(*
      DEBUG_MSG "uidmapping (gindex):\n%s" (uidmapping#to_string_gid tree1 tree2);
*)

    (*
     * glue deleted nodes and inserted nodes
     *)
    if not options#no_glue_flag then begin
      let _, added_pairs, conflicted_pairs = 
	glue_deletes_and_inserts
          ~first:true ~record_conflicted_pairs:true options cenv tree1 tree2 uidmapping ref_uidmapping
      in
      DEBUG_MSG "|conflicted_pairs|=%d" (Xset.length conflicted_pairs);
      let starting_pairs =
        List.iter (Xset.add conflicted_pairs) added_pairs;
        Xset.to_list conflicted_pairs
      in

      let starting_pairs =
        List.fast_sort
          (fun (u10, u20) (u11, u21) ->
            let n10 = tree1#search_node_by_uid u10 in
            let n11 = tree1#search_node_by_uid u11 in
            let gi10, gi11 = n10#gindex, n11#gindex in
            let c = Stdlib.compare gi10 gi11 in
            if c = 0 then
	      let n20 = tree2#search_node_by_uid u20 in
	      let n21 = tree2#search_node_by_uid u21 in
              let gi20, gi21 = n20#gindex, n21#gindex in
              Stdlib.compare gi20 gi21
            else
              c
          ) starting_pairs
      in

      uidmapping#add_starting_uid_pairs_for_glueing starting_pairs;
    end 
  (* end of func postprocess *)
	

  type edtag = Edel | Eins | Erel | Emov
  let edtag_to_string = function
    | Edel -> "del" | Eins -> "ins" | Erel -> "rel" | Emov -> "mov"


  let eliminate_odd_relabels options tree1 tree2 edits uidmapping =
    DEBUG_MSG "ELIMINATING ODD RELABELS AGAIN...";

    let permutation_pairs = 
      let l = ref [] in
      edits#iter_moves
	(function 
	  | Edit.Move(mid, kind, (uid1, info1, _), (uid2, info2, _)) ->
	      if !kind = Edit.Mpermutation then
		let nd1 = Info.get_node info1 in
		let nd2 = Info.get_node info2 in
		l := (nd1, nd2)::!l

	  | _ -> assert false
	);
      !l
    in

    let get_crossing_pairs (nd1, nd2) =
      List.filter 
	(fun (n1, n2) -> 
(*	  n1#data#eq n2#data && *)
	  (nd1#gindex - n1#gindex) * (nd2#gindex - n2#gindex) < 0
	) permutation_pairs
    in

    let odd_pairs = ref [] in
    let add_odd (n1, n2) =
      odd_pairs := (n1, n2) :: !odd_pairs
    in

    edits#iter_relabels
      (function
	| Edit.Relabel(_, (u1, info1, _), (u2, info2, _)) -> begin

	    DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;
	    
	    if (uidmapping#is_locked_uid u1 && uidmapping#is_locked_uid u2) then
	      DEBUG_MSG " -> locked"
		
	    else
	      try
		match edits#find_mov12 u1 u2 with
		| Edit.Move(_, kind, _, _) ->
		    let n1 = Info.get_node info1 in
		    let n2 = Info.get_node info2 in

		    let permu_cond =
		      if !kind = Edit.Mpermutation then
			let crossing_pairs = get_crossing_pairs (n1, n2) in
(*
			  DEBUG_MSG "crossing_pairs: [%s]" 
			    (String.concat ";" 
			       (List.map 
				  (fun (_n1, _n2) -> 
				    sprintf "<%a-%a>" UID.ps _n1#uid UID.ps _n2#uid) crossing_pairs));
*)

			let lconv l = (Obj.obj l : Label.t) in
			let alab1 = n1#data#_anonymized_label in
			let alab2 = n2#data#_anonymized_label in
			let alabp = lconv alab1, lconv alab2 in
			(List.exists 
			   (fun (_n1, _n2) -> not (_n1#data#eq _n2#data)) 
			   crossing_pairs
			) &&
			((* alab1 <> alab2 *) not (Label.relabel_allowed alabp) ||
			List.exists 
			  (fun (_n1, _n2) -> 
			    _n1#data#_anonymized_label = alab1 || _n2#data#_anonymized_label = alab2) 
			  crossing_pairs)
		      else
			false
		    in
		    DEBUG_MSG "permu_cond:%B" permu_cond;

		    if permu_cond && not (check_relabel options tree1 tree2 n1 n2 uidmapping) then begin

		      DEBUG_MSG "odd relabel: %a-%a" UID.ps u1 UID.ps u2;

		      add_odd (n1, n2)
		    end

		| _ -> assert false
	      with
		Not_found -> ()

	end
	| _ -> assert false
      );
    
    let odd_pairs_tbl = Hashtbl.create 0 in
    List.iter 
      (fun (n1, n2) -> Hashtbl.add odd_pairs_tbl (n1#uid, n2#uid) true)
      !odd_pairs;

    let odd_movs_exist = ref false in

    edits#filter
      (function
	| Edit.Relabel(_, (u1, _, _), (u2, _, _)) ->
	    not (Hashtbl.mem odd_pairs_tbl (u1, u2))

	| Edit.Move(mid, _, (u1, _, _), (u2, _, _)) -> begin
	    let is_odd = Hashtbl.mem odd_pairs_tbl (u1, u2) in
	    if is_odd then begin
	      odd_movs_exist := true;

	      DEBUG_MSG "odd move: %a %a-%a" MID.ps !mid UID.ps u1 UID.ps u2
	    end;
	    not is_odd
	end

	| _ -> true
      );

    List.iter
      (fun (nd1, nd2) ->
	edits#add_edit (Edit.make_delete nd1);
	edits#add_edit (Edit.make_insert nd2)
      ) !odd_pairs;
    
    uidmapping#filter
      (fun u1 u2 ->
	not (Hashtbl.mem odd_pairs_tbl (u1, u2))
      );

    DEBUG_MSG "* ODD RELABELS ELIMINATED *\n";

    !odd_movs_exist
  (* end of func eliminate_odd_relabels *)


  class mid_data (mid : Editop.move_id) (w : float) (nd : Spec.node_t) = object
    inherit Otreediff.Otree.data
    val mutable weight = w
    method mid = mid
    method node = nd
    method weight = weight
    method set_weight w = weight <- w

    method equals mdat = mid = mdat#mid
    method to_rep = MID.to_raw mid
    method to_string = sprintf "%a(%f)" MID.ps mid weight
    method to_elem_data = "", [], ""
  end
      


  (* handle movrels *)
  let handle_movrels options cenv tree1 tree2 edits uidmapping parent_move_tbl child_move_tbl =

    BEGIN_DEBUG
      Hashtbl.iter
        (fun m ms ->
          DEBUG_MSG "child_move_tbl: %a -> [%s]" MID.ps m (Xlist.to_string MID.to_string ";" ms)
        ) child_move_tbl;
    END_DEBUG;

    DEBUG_MSG "EDITS BEFORE handle_movrels:\n%s" edits#to_string_gid;
    DEBUG_MSG "uidmapping:\n%s" uidmapping#to_string;
    (*DEBUG_MSG "uidmapping (gindex):\n%s" uidmapping#to_string_gid;*)

    DEBUG_MSG "handling movrels...";


    let suggested_pairs = Xset.create 0 in

    if options#no_movrel_flag then begin (* eliminate ALL movrels *)
      DEBUG_MSG "removing ALL movrels...";
      edits#iter_moves
	(function
	  | Edit.Move(mid, _, (uid1, info1, ex1), (uid2, info2, ex2)) as mov -> begin
	      try
		match edits#find_rel12 uid1 uid2 with
		| Edit.Relabel _ as rel ->
		    uidmapping#remove uid1 uid2;
		    edits#remove_edit mov;
		    edits#remove_edit rel;
		    edits#add_edit (Edit.Delete(false, uid1, info1, ex1));
		    edits#add_edit (Edit.Insert(false, uid2, info2, ex2));
		    cenv#add_bad_pair uid1 uid2
		| _ -> assert false
	      with
		Not_found -> ()
	  end
	  | _ -> assert false
	)
    end
    else begin
      let count_mapped tree mem nd =
	let c = ref 0 in
	tree#fast_scan_whole_initial_subtree nd
	  (fun n -> if mem n#uid then incr c);
	!c
      in
      let mtbl = Hashtbl.create 0 in (* mid -> (top node * top node) set *)
      edits#iter_moves
	(function
	  | Edit.Move(mid, _, (uid1, info1, _), (uid2, info2, _)) -> begin
	      let nd1, nd2 = Info.get_node info1, Info.get_node info2 in
	      try
		let _top_pairs = Hashtbl.find mtbl !mid in
		let anc_flag = ref false in
		Xset.iter
		  (function (n1, n2) as p ->
		    if
                      tree1#is_initial_ancestor nd1 n1 &&
                      tree2#is_initial_ancestor nd2 n2
                    then begin
		      anc_flag := true;
		      Xset.remove _top_pairs p
		    end
		    else if
                      tree1#is_initial_ancestor n1 nd1 &&
                      tree2#is_initial_ancestor n2 nd2
                    then begin
		      ()
		    end
		    else
		      anc_flag := true
		  ) _top_pairs;

		if !anc_flag then 
		  Xset.add _top_pairs (nd1, nd2);

	      with
		Not_found -> 
		  let s = Xset.create 0 in
		  Xset.add s (nd1, nd2);
		  Hashtbl.add mtbl !mid s
	  end
	  | _ -> assert false
	);


      BEGIN_DEBUG
	DEBUG_MSG "mtbl:";
	let l = ref [] in
	Hashtbl.iter 
	  (fun mid s ->  
	    Xset.iter 
	      (fun (n1, n2) -> 
		l := (mid, n1, n2) :: !l
	      ) s
	  ) mtbl;
	List.iter
	  (fun (mid, n1, n2) -> 
	    DEBUG_MSG "%a: %a-%a (%a-%a)" MID.ps mid 
	      GI.ps n1#gindex GI.ps n2#gindex UID.ps n1#uid UID.ps n2#uid
	  )
	  (List.fast_sort 
	     (fun (_, n1, _) (_, n2, _) -> 
	       Stdlib.compare n1#gindex n2#gindex) 
	     !l);
      END_DEBUG;

(* !!!
      let is_possible_rename node1 node2 bid1 bid2 =
        let parent_cond =
          try
            let pnd1 = node1#initial_parent in
            let pnd2 = node2#initial_parent in

            let mcond =
              try
                uidmapping#find pnd1#uid = pnd2#uid
              with
                Not_found -> false
            in

            let pbi1_opt = Edit.get_bid_opt pnd1 in
            let pbi2_opt = Edit.get_bid_opt pnd2 in
            match pbi1_opt, pbi2_opt with 
            | Some pbi1, Some pbi2 -> mcond && bid1 = pbi1 && bid2 = pbi2
            | Some _, None | None, Some _ -> false
            | None, None -> mcond
          with
            Otreediff.Otree.Parent_not_found _ -> true
        in
        DEBUG_MSG "%a-%a (%a-%a): parent_cond -> %B" 
          UID.ps node1#uid UID.ps node2#uid Binding.ID.ps bid1 Binding.ID.ps bid2 parent_cond;

        parent_cond && node1#data#relabel_allowed node2#data
      in
      let _ = Edit.collect_use_renames edits is_possible_rename in 
*)

      let stbl = Hashtbl.create 0 in (* mid -> (stability * movrel ratio * nmoved * nmapped1 * nmapped2) *)

      let bad_value = (0.0, 0.0, 0, 0, 0) in
      let set_to_bad mid =
        Hashtbl.add stbl mid bad_value
      in
      let is_bad_mid mid =
        try
          Hashtbl.find stbl mid = bad_value
        with
          Not_found -> false
      in

      Hashtbl.iter 
	(fun mid s ->
	  Xset.iter 
	    (fun (nd1, nd2) ->
              let is_bad =
                try
                  let pnd1 = nd1#initial_parent in
                  let pnd2 = nd2#initial_parent in

                  if pnd1#data#is_sequence && pnd2#data#is_sequence then
                    false
                  else
                    let puid1 = pnd1#uid in
                    let puid2 = pnd2#uid in

                    let puid1' = uidmapping#find puid1 in

                    if puid1' = puid2 then
                      false
                    else begin
                      match edits#find12 puid1 puid1' with
                      | [] -> true
                      | _ -> begin
                          let puid2' = uidmapping#inv_find puid2 in
                          if puid2' = puid1 then
                            false
                          else
                            match edits#find12 puid2' puid2 with
                            | [] -> true
                            | _ -> false
                      end
                    end
                with
	        | Otreediff.Otree.Parent_not_found _
	        | Not_found -> false
              in
              if is_bad then begin
                DEBUG_MSG "%a --> BAD" MID.ps mid;
                set_to_bad mid
              end
	    ) s
	) mtbl;

      
      (* !!! *)
      edits#iter_moves_bottomup 
	(function
	  | Edit.Move(mid, kind, (uid1, info1, ex1), (uid2, info2, ex2)) -> begin

              if not (is_bad_mid !mid) then
                try
                  let _ = edits#find_rel12 uid1 uid2 in

	          let nd1 = Info.get_node info1 in
	          let nd2 = Info.get_node info2 in

                  let bid1 = Edit.get_bid nd1 in
                  let bid2 = Edit.get_bid nd2 in
                  DEBUG_MSG "%a -> %a" BID.ps bid1 BID.ps bid2;
                  let _use1 = cenv#get_use1 bid1 in
                  let _use2 = cenv#get_use2 bid2 in
                  let alab1 = nd1#data#_anonymized2_label in
                  let alab2 = nd2#data#_anonymized2_label in
                  let filt alab nd n =
                    n#data#_anonymized2_label = alab && n != nd
                  in
                  let use1 = List.filter (filt alab1 nd1) _use1 in
                  let use2 = List.filter (filt alab2 nd2) _use2 in

                  BEGIN_DEBUG
                    let nl_to_s = Xlist.to_string (fun n -> UID.to_string n#uid) ";" in
	            DEBUG_MSG "use1=[%s]" (nl_to_s use1);
	            DEBUG_MSG "use2=[%s]" (nl_to_s use2);
                  END_DEBUG;

                  let is_mov1 = edits#is_crossing_with_untouched ?mask:None uidmapping in
                  let is_mov2 n1 n2 = is_mov1 n2 n1 in

                  let find is_mov nd use =
                    let rec doit = function
                      | [] -> raise Not_found
                      | n::rest ->
                          if is_mov nd n then
                            doit rest
                          else
                            n
                    in
                    doit use
                  in
                  let is_bad1 =
                    try
                      let nd1' = find is_mov1 nd1 use2 in
                      let uid1' = nd1'#uid in
                      DEBUG_MSG "non move pair found: %a-%a" UID.ps uid1 UID.ps uid1';
                      Xset.add suggested_pairs (uid1, uid1');
                      try
                        let uid1'' = uidmapping#inv_find uid1' in
                        let b = edits#mem_mov12 uid1'' uid1' in
                        if b then begin
                          let m = edits#find_mid12 uid1'' uid1' in
                          if not (is_bad_mid m) then begin
                            DEBUG_MSG "%a --> BAD" MID.ps m;
                            set_to_bad m
                          end
                        end;
                        BEGIN_DEBUG
                          let k = if b then "movrel:" else "relabel:" in
                          DEBUG_MSG "conflicts with %s%a-%a" k UID.ps uid1'' UID.ps uid1';
                        END_DEBUG;
                        b
                      with
                        Not_found -> false
                    with
                      Not_found -> false
                  in
                  let is_bad2 =
                    try
                      let nd2' = find is_mov2 nd2 use1 in
                      let uid2' = nd2'#uid in
                      DEBUG_MSG "non move pair found: %a-%a" UID.ps uid2' UID.ps uid2;
                      Xset.add suggested_pairs (uid2', uid2);
                      try
                        let uid2'' = uidmapping#find uid2' in
                        let b = edits#mem_mov12 uid2' uid2'' in
                        if b then begin
                          let m = edits#find_mid12 uid2' uid2'' in
                          if not (is_bad_mid m) then begin
                            DEBUG_MSG "%a --> BAD" MID.ps m;
                            set_to_bad m
                          end
                        end;
                        BEGIN_DEBUG
                          let k = if b then "movrel:" else "relabel:" in
                          DEBUG_MSG "conflicts with %s%a-%a" k UID.ps uid2' UID.ps uid2'';
                        END_DEBUG;
                        b
                      with
                        Not_found -> false
                    with
                      Not_found -> false
                  in
                  if is_bad1 || is_bad2 then begin
                    DEBUG_MSG "%a --> BAD" MID.ps !mid;
                    set_to_bad !mid
                  end

                with
                  Not_found -> ()
          end
          | _ -> assert false
        );


      edits#iter_moves_bottomup
	(function
	  | Edit.Move(mid, kind, (uid1, info1, ex1), (uid2, info2, ex2)) as mov -> begin

	      let nd1 = Info.get_node info1 in
	      let nd2 = Info.get_node info2 in
(*
	      let same_kind = nd1#data#_anonymized_label = nd2#data#_anonymized_label in
*)
	      DEBUG_MSG "checking: %s" (Editop.to_string mov);

	      try

		if (uidmapping#is_locked_uid uid1) && (uidmapping#is_locked_uid uid2) then begin
		  DEBUG_MSG "locked mapping: %a-%a" UID.ps uid1 UID.ps uid2;

                  if is_bad_mid !mid then begin
                    DEBUG_MSG "bad mid: %a" MID.ps !mid;
                    uidmapping#unlock_uid uid1;
                    uidmapping#unlock_uid uid2
                  end
                  else
		    raise Not_found
		end;

                let rel = edits#find_rel12 uid1 uid2 in

		let stability, ratio, nmoved, nmapped1, nmapped2 = 
                  (*try
                    Hashtbl.find stbl !mid
                  with
                    Not_found ->*)
		      let parent_moves = 
			try
			  let (pm, _, _) = Hashtbl.find parent_move_tbl !mid in
			  [pm]
			with
			  Not_found -> []
		      in
		      let sibling_moves =
			List.fold_left
			  (fun l pm ->
			    try
			      let cms = Hashtbl.find child_move_tbl pm in
			      Xlist.union l (Xlist.subtract cms [!mid])
			    with
			      Not_found -> l
			  ) [] parent_moves
		      in
		      let child_moves =
			try
			  Hashtbl.find child_move_tbl !mid
			with
			  Not_found -> []
		      in

		      BEGIN_DEBUG
			DEBUG_MSG "parent moves of %a --> [%s]" MID.ps !mid
			  (Xlist.to_string MID.to_string ";" parent_moves);
			DEBUG_MSG "sibling moves of %a --> [%s]" MID.ps !mid
			  (Xlist.to_string MID.to_string ";" sibling_moves);
			DEBUG_MSG "child moves of %a --> [%s]" MID.ps !mid
			  (Xlist.to_string MID.to_string ";" child_moves);
		      END_DEBUG;

		      let strip s =
			Xstring.strip  ~strs:["\""] s
		      in
		      let get_lcss_len str1 str2 = (* length of longest common sub-string *)
			let s1 = strip str1 in
			let s2 = strip str2 in
			let m, _, _, _ = 
			  Adiff.adiff (Xstring.to_char_array s1) (Xstring.to_char_array s2)
			in
			let len = List.length m in
			DEBUG_MSG "\"%s\"(%d) vs \"%s\"(%d) -> %d" s1 (String.length s1) s2 (String.length s2) len;
			len
		      in
		      let is_string n =	Label.is_string_literal (getlab n) in
		      let get_value n =	Label.get_value (getlab n) in

                      let surrounding_moves = parent_moves @ sibling_moves @ child_moves in

                      let stable =
                        if surrounding_moves = [] then
                          try
                            let pu1 = nd1#initial_parent#uid in
                            let pu2 = nd2#initial_parent#uid in
                            (uidmapping#find pu1 = pu2) &&
                            (not (edits#mem_mov12 pu1 pu2) && !kind <> Edit.Mpermutation(* ||
                            try edits#find_mid12 pu1 pu2 = !mid with _ -> false*))
                          with
                            _ -> false
                        else
                          false
                      in

                      if stable then begin
                        DEBUG_MSG "stable relabel";
                        1.0, 1.0, 1, 2, 2
                      end
                      else begin
			let t0 = ref 0 in
			let c, t =
			  List.fold_left
			    (fun (c, t) m ->

			      let c'_ex = ref 0 in
			      let c' =
				edits#get_nmoves_of_move_id_filt 
				  (fun n1 n2 ->
				    if is_string n1 && is_string n2 then begin
				      c'_ex := !c'_ex + 2 * (get_lcss_len (get_value n1) (get_value n2));
				      false
				    end
				    else
				      n1#data#eq n2#data
				  ) m 
			      in

			      let t'_ex = ref 0 in
			      let t' = 
				edits#get_nmoves_of_move_id_filt
				  (fun n1 n2 ->
				    incr t0;
				    if is_string n1 && is_string n2 then begin
				      t'_ex := !t'_ex + (String.length (get_value n1)) + (String.length (get_value n2));
				      false
				    end
				    else
				      true
				  ) m 
			      in
			      (c + c' + !c'_ex, t + t' + !t'_ex)
			    ) (0, 0) (!mid :: surrounding_moves)
			in

			let stability = (float c) /. (float t) in
			let n_mapped1, n_mapped2 = 
			  try
			    let topl = Xset.to_list (Hashtbl.find mtbl !mid) in

			    DEBUG_MSG "top nodes of %a %a-%a: %s" MID.ps !mid UID.ps uid1 UID.ps uid2
			      (Xlist.to_string (fun (n1, n2) -> sprintf "(%a,%a)" UID.ps n1#uid UID.ps n2#uid) ";" topl);

			    List.fold_left
			      (fun (c1, c2) (n1, n2) -> 
				let c1' = count_mapped tree1 uidmapping#mem_dom n1 in
				let c2' = count_mapped tree2 uidmapping#mem_cod n2 in
				c1 + c1', c2 + c2'
			      ) (0, 0) topl
			  with 
			    Not_found -> assert false
			in

			let ratio = (float (!t0 * 2)) /. (float (n_mapped1 + n_mapped2)) in

			DEBUG_MSG "stability: (num of exact matches)/(num of moved): %a --> %d/%d=%f" MID.ps !mid c t stability;
			DEBUG_MSG "num of mapped nodes: %d - %d" n_mapped1 n_mapped2;
			DEBUG_MSG "movrel ratio: (num of moved)/(num of mapped): %f" ratio;

			Hashtbl.add stbl !mid (stability, ratio, t, n_mapped1, n_mapped2);
			stability, ratio, t, n_mapped1, n_mapped2
                      end
		in (* end of let stability, ratio, nmoved, nmapped1, nmapped2 *)

		let bad_movrel = 
		  stability <= options#movrel_stability_threshold && (* || *) (* && *)
		  (ratio <= options#movrel_ratio_threshold || nmoved = 1 (* || (nmapped1 = 1 || nmapped2 = 1) *))
		in

                DEBUG_MSG "stability:%f <= %f --> %B" 
                  stability options#movrel_stability_threshold (stability <= options#movrel_stability_threshold);
                DEBUG_MSG "ratio:%f <= %f --> %B" 
                  ratio options#movrel_ratio_threshold (ratio <= options#movrel_ratio_threshold);

		if bad_movrel then begin

		  DEBUG_MSG "removing unstable movrel: %s" (Editop.to_string rel);

		  uidmapping#remove uid1 uid2;
		  edits#remove_edit mov;
		  edits#remove_edit rel;
		  edits#add_edit (Edit.Delete(false, uid1, info1, ex1));
		  edits#add_edit (Edit.Insert(false, uid2, info2, ex2));
		  cenv#add_bad_pair uid1 uid2
		end

	      with
		Not_found -> ()
	  end
	  | _ -> assert false
	)
    end;
    DEBUG_MSG "movrel handling finished";
    suggested_pairs
  (* end of handle_movrels *)

  (* eliminate small move of unnamed entities *)
  let decompose_moves is_xxx_pair options edits uidmapping size_limit =
    (*let is_stable1 n =
      match edits#find1 n#uid with
      | []
      | [Edit.Relabel _] -> true
      | _ -> false
    in
    let is_stable2 n =
      match edits#find2 n#uid with
      | []
      | [Edit.Relabel _] -> true
      | _ -> false
    in
    let check_parent n1 n2 =
      try
        let pu1 = n1#initial_parent#uid in
        let pu2 = uidmapping#find pu1 in
        n2#initial_parent#uid <> pu2 ||
        match edits#find12 pu1 pu2 with
        | [] | [Edit.Relabel _] -> false
        | _ -> true
      with
        Not_found ->
          try
            let a1 = get_p_ancestor is_stable1 n1 in
            let a2 = get_p_ancestor is_stable2 n2 in
            DEBUG_MSG "n1=%a, n2=%a: a1=%a, a2=%a"
              UID.ps n1#uid UID.ps n2#uid UID.ps a1#uid UID.ps a2#uid;
            (uidmapping#find a1#uid) <> a2#uid
          with
            Not_found -> true
    in*)
    let sz_tbl = Hashtbl.create 0 in
    edits#iter_moves
      (function
        | Edit.Move(mid, _, _, _) as mov -> begin
            let sz, ml =
              try
                let _sz, _ml = (Hashtbl.find sz_tbl !mid) in
                _sz + 1, mov::_ml
              with
                Not_found -> 1, [mov]
            in
            Hashtbl.replace sz_tbl !mid (sz, ml)
        end
        | _ -> assert false
      );
    let dels = Xset.create 0 in
    let inss = Xset.create 0 in
    Hashtbl.iter
      (fun mid (sz, movl) ->
        DEBUG_MSG "%a: sz=%d" MID.ps mid sz;
        if size_limit = 0 || sz <= size_limit then begin
          let cond =
            List.for_all
              (function
                | Edit.Move(_, _, (_, info1, _), (_, info2, _)) -> begin
                    let nd1 = Info.get_node info1 in
                    let nd2 = Info.get_node info2 in
                    (*check_parent nd1 nd2 && *)is_xxx_pair nd1 nd2
                end
                | _ -> assert false
              ) movl
          in
          if cond then begin
            DEBUG_MSG "decomposing %a..." MID.ps mid;
            List.iter
              (fun mov ->
                DEBUG_MSG "decomposing %s" (Edit.to_string mov);
                match mov with
                | Edit.Move(_, kind, (uid1, info1, ex1), (uid2, info2, ex2)) -> begin
                    let nd1 = Info.get_node info1 in
                    let nd2 = Info.get_node info2 in
                    uidmapping#remove uid1 uid2;
                    edits#remove_edit mov;
                    begin
                      try
                        match edits#find_rel12 uid1 uid2 with
                        | Edit.Relabel _ as rel ->
                            edits#remove_edit rel
                        | _ -> assert false
                      with
                        Not_found -> ()
                    end;
                    edits#add_edit (Edit.Delete(false, uid1, info1, ex1));
                    edits#add_edit (Edit.Insert(false, uid2, info2, ex2));
                    Xset.add dels nd1;
                    Xset.add inss nd2;
                end
                | _ -> assert false
              ) movl
          end
        end
      ) sz_tbl;
    dels, inss

  (* *)
  let elaborate_edits_for_delta options ?(sim_thresh=0.1) tree1 tree2 uidmapping edits =
    let nmap n = tree2#search_node_by_uid (uidmapping#find n#uid) in
    let get_subtree_similarity nd1 nd2 =
      let sz1 = tree1#whole_initial_subtree_size nd1 in
      let sz2 = tree2#whole_initial_subtree_size nd2 in
      let mapped_node_count = ref 0 in
      tree1#scan_whole_initial_subtree nd1
        (fun n1 ->
          try
            let n2 = nmap n1 in
            if tree2#initial_subtree_mem nd2 n2 && n1#data#equals n2#data then
              incr mapped_node_count
          with
            Not_found -> ()
        );
      let sim = 2.0 *. (float !mapped_node_count) /. (float (sz1 + sz2)) in
      sim
    in
    let visited = Xset.create 0 in
    let is_ancestor_of_visited n =
      try
        Xset.iter
          (fun x ->
            if tree1#is_initial_ancestor n x then
              raise Exit
          ) visited;
        false
      with
        Exit -> true
    in
    let eliminate_edits rt1 rt2 =
      DEBUG_MSG "rt1=%a rt2=%a" UID.ps rt1#uid UID.ps rt2#uid;
      edits#iter_moves
        (function
          | Edit.Move(_, kind, (uid1, info1, ex1), (uid2, info2, ex2)) as mov -> begin
              let nd1 = Info.get_node info1 in
              let nd2 = Info.get_node info2 in
              if
                tree1#initial_subtree_mem rt1 nd1 ||
                tree2#initial_subtree_mem rt2 nd2
              then begin
                uidmapping#remove uid1 uid2;
                edits#remove_edit mov;
                begin
                  try
                    match edits#find_rel12 uid1 uid2 with
                    | Edit.Relabel _ as rel ->
                        edits#remove_edit rel
                    | _ -> assert false
                  with
                    Not_found -> ()
                end;
                edits#add_edit (Edit.Delete(false, uid1, info1, ex1));
                edits#add_edit (Edit.Insert(false, uid2, info2, ex2));
              end
          end
          | _ -> assert false
        )
    in
    tree1#scan_whole_initial
      (fun n1 ->
        if n1#data#is_boundary then begin
          if not (is_ancestor_of_visited n1) then begin
            Xset.add visited n1;
            try
              let n2 = nmap n1 in
              let sim = get_subtree_similarity n1 n2 in
              (*DEBUG_MSG "n1=%s" n1#initial_to_string;
              DEBUG_MSG "n2=%s" n2#initial_to_string;
              DEBUG_MSG "similarity=%f" sim;*)
              if sim < sim_thresh then begin
                Xprint.verbose options#verbose_flag "elaborating edits on %s -- %s (similarity=%f)"
                  n1#initial_to_string n2#initial_to_string sim;
                eliminate_edits n1 n2
              end
            with
              Not_found -> ()
          end
        end
      )

  (* eliminate false moves *)
  let eliminate_false_moves tree1 tree2 edits uidmapping =
    DEBUG_MSG "* ELIMINATING FALSE MOVES...\n";

    DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
    (*DEBUG_MSG "uidmapping (gindex):\n%s" uidmapping#to_string_gid;*)

    let crossing_with_untouched = Xset.create 0 in (* mid set *)
    let crossing_checked = Xset.create 0 in (* mid set *)
    let move_size_tbl = Hashtbl.create 0 in (* mid -> size *)
    let move_top_tbl = Hashtbl.create 0 in (* mid -> node * node *)
    let move_mem_tbl = Hashtbl.create 0 in (* mid -> (node * node) list *)

    edits#iter_moves
      (function
	| Edit.Move(mid, kind, (uid1, info1, _), (uid2, info2, _)) -> 
	    let sz1 = Info.get_size info1 in
	    let sz2 = Info.get_size info2 in
	    assert (sz1 = sz2);

	    let nd1 = Info.get_node info1 in
	    let nd2 = Info.get_node info2 in

	    DEBUG_MSG "checking %a %a-%a (%a-%a)" MID.ps !mid UID.ps uid1 UID.ps uid2 GI.ps nd1#gindex GI.ps nd2#gindex;

	    begin
	      try
		let ndpairs = Hashtbl.find move_mem_tbl !mid in
		Hashtbl.replace move_mem_tbl !mid ((nd1, nd2)::ndpairs)
	      with
		Not_found -> Hashtbl.add move_mem_tbl !mid [nd1, nd2]
	    end;
	    begin
	      try
		let rt1, rt2 = Hashtbl.find move_top_tbl !mid in
		if nd1#gindex > rt1#gindex then
		  Hashtbl.replace move_top_tbl !mid (nd1, nd2)
	      with
		Not_found -> Hashtbl.add move_top_tbl !mid (nd1, nd2)
	    end;

	    if not (Xset.mem crossing_checked !mid) then begin
	      Xset.add crossing_checked !mid;
	      try
		uidmapping#iter_crossing_or_incompatible_mapping nd1 nd2
		  (fun u1 u2 ->

		    let n1 = tree1#search_node_by_uid u1 in
		    let n2 = tree2#search_node_by_uid u2 in
(*
		    DEBUG_MSG "!!!  crossing: %a-%a (%a-%a)" UID.ps u1 UID.ps u2 GI.ps n1#gindex GI.ps n2#gindex;
*)
		    match edits#find12 u1 u2 with
		    | [] | [Edit.Relabel _] ->
			if (not (is_ghost_node n1)) && (not (is_ghost_node n2)) then begin
			  DEBUG_MSG "-->  crossing_with_untouched: %a-%a" UID.ps u1 UID.ps u2;
			  Xset.add crossing_with_untouched !mid;
			  raise Break
			end
		    | eds -> ()
(*			List.iter (fun e -> DEBUG_MSG "!!!    %s" (Edit.to_string e)) eds *)
		  )
	      with
		Break -> ()
	    end

	| _ -> assert false

      ); (* edits#iter_moves *)

    Hashtbl.iter
      (fun mid ndpairs ->
	let sz =
	  List.fold_left 
	    (fun s (n1, n2) -> 
	      if n1#data#eq n2#data then 
		s +. 1.0
              else if n1#data#_anonymized_label = n2#data#_anonymized_label then
                s +. 0.5
	      else 
		s
	    ) 0.0 ndpairs
	in
(*
	let sz = if sz = 0 then 1 else sz in (* EXPERIMENTAL!!! *)
*)
	Hashtbl.add move_size_tbl mid sz
      ) move_mem_tbl;

    BEGIN_DEBUG
      let crossing_with_untouched_l = Xset.to_list crossing_with_untouched in
      DEBUG_MSG "moves crossing with untouched: [%s]"
        (Xlist.to_string MID.to_string ";" 
	   (List.fast_sort Stdlib.compare crossing_with_untouched_l));
    END_DEBUG;

    let top_mid_tbl1 = Hashtbl.create 0 in (* top node -> mid *)

    let move_tops = 
      Hashtbl.fold 
	(fun m (nd1, nd2) l -> 
	  if not (Xset.mem crossing_with_untouched m) then begin
	    Hashtbl.add top_mid_tbl1 nd1 m;
	    (m, nd1, nd2)::l
	  end
	  else
	    l
	) move_top_tbl [] 
    in

    let sorted_move_tops =
      (List.fast_sort
	 (fun (mid0, nd0, _) (mid1, nd1, _) -> Stdlib.compare nd0#gindex nd1#gindex)
	 move_tops)
    in

    BEGIN_DEBUG
      DEBUG_MSG "move tops:";
      List.iter 
	(fun (mid, nd1, nd2) ->
	  DEBUG_MSG "%a  %a -> %a (size:%f)"
	    MID.ps mid UID.ps nd1#uid UID.ps nd2#uid (try Hashtbl.find move_size_tbl mid with Not_found -> 0.0);
	  let ndpairs = Hashtbl.find move_mem_tbl mid in
          let nds1, nds2 = List.split ndpairs in
          let nl_to_s = Xlist.to_string (fun n -> UID.to_string n#uid) ";" in
	  DEBUG_MSG "      [%s]" (nl_to_s nds1);
	  DEBUG_MSG "   -> [%s]" (nl_to_s nds2)
	)
      (List.fast_sort
	 (fun (mid0, nd0, _) (mid1, nd1, _) -> Stdlib.compare nd0#gindex nd1#gindex)
	 move_tops);
      DEBUG_MSG "move tops (gindex):";
      List.iter
	(fun (mid, nd1, nd2) -> 
	  DEBUG_MSG "%a  %a -> %a (size:%f)"
	    MID.ps mid GI.ps nd1#gindex GI.ps nd2#gindex (try Hashtbl.find move_size_tbl mid with Not_found -> 0.0);
	  let ndpairs = Hashtbl.find move_mem_tbl mid in
          let nds1, nds2 = List.split ndpairs in
	  DEBUG_MSG "      [%s]" 
	    (Xlist.to_string (fun n -> GI.to_string n#gindex) ";" nds1);
	  DEBUG_MSG "   -> [%s]"
	    (Xlist.to_string (fun n -> GI.to_string n#gindex) ";" nds2)
	) sorted_move_tops
    END_DEBUG;


    let extra_move_elements = Xset.create 0 in
    List.iter
      (fun (mid, nd1, nd2) ->
        DEBUG_MSG "mid=%a nd1=%a nd2=%a" MID.ps mid UID.ps nd1#uid UID.ps nd2#uid;
        DEBUG_MSG "%a-%a" GI.ps nd1#gindex GI.ps nd2#gindex;
	nd1#iter_initial_ancestor_nodes
	  (fun n1 ->
	    try
	      let m = Hashtbl.find top_mid_tbl1 n1 in
	      let n1' = tree2#search_node_by_uid (uidmapping#find n1#uid) in

	      if tree2#is_initial_ancestor n1' nd2 then

		let ndpairs = Hashtbl.find move_mem_tbl m in
		try
		  List.iter
		    (fun (mn1, mn2) ->
                      DEBUG_MSG "    mn1=%a mn2=%a" UID.ps mn1#uid UID.ps mn2#uid;
                      DEBUG_MSG "    %a-%a" GI.ps mn1#gindex GI.ps mn2#gindex;
		      if
			UIDmapping.is_crossing_or_incompatible tree1 tree2 nd1 nd2 mn1 mn2
		      then begin
                        DEBUG_MSG "crossing_or_incompatible: (%a-%a) (%a, %a-%a)"
                          UID.ps nd1#uid UID.ps nd2#uid MID.ps m UID.ps mn1#uid UID.ps mn2#uid;
			Xset.add extra_move_elements (m, mn1, mn2);
			raise Break
		      end       
		    ) 
                    (List.fast_sort 
                       (fun (n0, _) (n1, _) -> Stdlib.compare n1#gindex n0#gindex)
                       ndpairs)
		with
		  Break -> ()
	    with
	      Not_found -> ()
	  )
      ) sorted_move_tops;

    let extra_move_element_list = Xset.to_list extra_move_elements in

    BEGIN_DEBUG
      DEBUG_MSG "extra_move_elements: %s"
        (Xlist.to_string 
	   (fun (m, n1, n2) -> sprintf "(%a,%a-%a)" MID.ps m UID.ps n1#uid UID.ps n2#uid) 
	   ";"       
           (List.fast_sort
	      (fun (mid0, nd0, _) (mid1, nd1, _) -> Stdlib.compare nd0#gindex nd1#gindex)
	      extra_move_element_list)
        );
      DEBUG_MSG "extra_move_elements (gindex): %s"
        (Xlist.to_string 
	   (fun (m, n1, n2) -> sprintf "(%a,%a-%a)" MID.ps m GI.ps n1#gindex GI.ps n2#gindex) 
	   ";"       
           (List.fast_sort
	      (fun (mid0, nd0, _) (mid1, nd1, _) -> Stdlib.compare nd0#gindex nd1#gindex)
	      extra_move_element_list)
        )
    END_DEBUG;

    let get_mid (mid, _, _) = mid in
    let get_nd1 (_, nd, _) = nd in
    let get_nd2 (_, _, nd) = nd in

    let next_l mdat tree nd l =
      match l with
      | [] -> [Otreediff.Otree.create_leaf mdat]
      | _ ->
	  let gi = nd#gindex in
	  let lgi = (tree#initial_leftmost nd)#gindex in
	  let unchanged, children =
	    List.partition 
	      (fun mnd ->
		let mgi = mnd#data#node#gindex in
		let b = lgi <= mgi && mgi <= gi in
		not b
	      ) l
	  in
	  unchanged @ [Otreediff.Otree.create_node mdat (Array.of_list children)]
    in

    let get_children get_nd tree sorted_tops =
      Array.of_list
	(List.fold_left
	   (fun l x ->
	     let mid = get_mid x in
	     let nd = get_nd x in
	     let w = try Hashtbl.find move_size_tbl mid with Not_found -> 0.0 in
	     let mdat = new mid_data mid w nd in

	     next_l mdat tree nd l
	       
	   ) [] sorted_tops
	)
    in

    let move_elems = sorted_move_tops @ extra_move_element_list in

    let children1 = 
      get_children get_nd1 tree1
	(List.fast_sort
	   (fun (_, nd0, _) (_, nd1, _) -> Stdlib.compare nd0#gindex nd1#gindex)
	   move_elems)
    in

    let children2 = 
      get_children get_nd2 tree2
	(List.fast_sort
	   (fun (_, _, nd0) (_, _, nd1) -> Stdlib.compare nd0#gindex nd1#gindex)
	   move_elems)
    in

    let mroot_mdat1 = new mid_data MID.unknown 1.0 tree1#root in
    let mroot_mdat2 = new mid_data MID.unknown 1.0 tree2#root in

    let mroot1 = Otreediff.Otree.create_node mroot_mdat1 children1 in
    let mroot2 = Otreediff.Otree.create_node mroot_mdat2 children2 in

    let mtree1 = new Otreediff.Otree.otree mroot1 in
    let mtree2 = new Otreediff.Otree.otree mroot2 in

(* EXPERIMENTAL!!! *)
    let mid_count_tbl = Hashtbl.create 0 in
    List.iter 
      (fun (mid, _, _) -> 
	try
	  let c = Hashtbl.find mid_count_tbl mid in
	  Hashtbl.replace mid_count_tbl mid (c+1)
	with
	  Not_found -> Hashtbl.add mid_count_tbl mid 2
      ) extra_move_element_list;

    let modify_weight n =
      try
	let c = Hashtbl.find mid_count_tbl n#data#mid in
	n#data#set_weight (n#data#weight /. (float c))
      with
	Not_found -> ()
    in
    mtree1#fast_scan_all modify_weight;
    mtree2#fast_scan_all modify_weight;

    BEGIN_DEBUG
      DEBUG_MSG "* mid tree 1:\n%s" mtree1#to_string;
      DEBUG_MSG "* mid tree 2:\n%s" mtree2#to_string;
    END_DEBUG;

    let cost t1 t2 i j = 
      let nd1 = t1#get i in
      let nd2 = t2#get j in
      let w1 = nd1#data#weight in
      let w2 = nd2#data#weight in

      if j = 0 then (* delete *)
	w1
      else if i = 0 then (* insert *)
	w2
      else
	if nd1#data#equals nd2#data then
	  0.0
	else (* relabel *)
	  w1 +. w2
    in

    let eds, map, _ = Otreediff.ZS.Float.find cost mtree1 mtree2 in

    BEGIN_DEBUG
      DEBUG_MSG "eds:\n%s" 
	(Otreediff.Edit.seq_to_string eds);
      DEBUG_MSG "mapping:\n%s" 
	(Otreediff.Mapping.to_string map)
    END_DEBUG;

    let true_moves = 
      List.fold_left
	(fun l ed -> 
	  if Otreediff.Edit.isdel ed then
	    let i = Otreediff.Edit.get_del_idx ed in
	    let m = (mtree1#get i)#data#mid in
	    if not (List.mem m l) then
	      m :: l
	    else
	      l

	  else if Otreediff.Edit.isrel ed then
	    let i = Otreediff.Edit.get_rel_idx1 ed in
	    let m = (mtree1#get i)#data#mid in
	    if not (List.mem m l) then
	      m :: l
	    else
	      l

	  else
	    l
	) [] eds
    in
            
    DEBUG_MSG "true moves: [%s]"
      (Xlist.to_string MID.to_string ";" 
	 (List.fast_sort Stdlib.compare true_moves));

    let is_crossing_with_untouched = edits#is_crossing_with_untouched ?mask:None uidmapping in

    edits#iter_moves
      (function
	| Edit.Move(mid, kind, (_, info1, _), (_, info2, _)) as mov ->
	    let crossing = Xset.mem crossing_with_untouched !mid in
	    if crossing || List.mem !mid true_moves then begin

              if List.mem !mid true_moves then
                if
                  not (is_crossing_with_untouched
                         (Info.get_node info1) (Info.get_node info2))
                then
                  DEBUG_MSG "is this a move? %s" (Edit.to_string mov);

              if
		crossing && !kind <> Edit.Mpermutation &&
		!kind <> Edit.Modd
	      then begin
		DEBUG_MSG "kind changed: %a: %s -> %s (crossing with untouched)"
		  MID.ps !mid
                  (Edit.move_kind_to_string !kind) (Edit.move_kind_to_string Edit.Mpermutation);

		kind := Edit.Mpermutation
	      end

	    end
	    else begin
              DEBUG_MSG "removing %s" (Edit.to_string mov);
	      edits#remove_edit mov
            end

	| _ -> assert false
      );

    DEBUG_MSG "* END OF FALSE MOVE ELIMINATION\n"
   (* end of eliminate_false_moves *)


  (* examine moves are normal or permutation *)
  let examine_moves edits =
    let permutations = ref [] in (* mid list *)
    edits#iter_moves
      (function
	| Edit.Move(mid, kind, (_, info1, _), (_, info2, _)) -> 
	    if !kind = Edit.Mpermutation then begin
	      if not (List.mem !mid !permutations) then
		permutations := !mid :: !permutations
	    end

	| _ -> assert false
      );
    edits#iter_moves
      (function
	| Edit.Move(mid, kind, _, _) -> 
	    if !kind <> Edit.Mpermutation && List.mem !mid !permutations then begin

	      DEBUG_MSG "kind changed: %a: %s -> %s (same group)"
		MID.ps !mid (Edit.move_kind_to_string !kind) (Edit.move_kind_to_string Edit.Mpermutation);

	      kind := Edit.Mpermutation
	    end
	| _ -> assert false
      )
  (* end of examine_moves *)



  (********** fix up edit operations **********)
  let fixup_edits options lang cenv tree1 tree2 pruned edits uidmapping pre_uidmapping =

    BEGIN_DEBUG
      DEBUG_MSG "*** fixing up edits ***";
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
    END_DEBUG;

    let mid_chg_tbl = group_moves options tree1 tree2 edits uidmapping in

    BEGIN_DEBUG
      DEBUG_MSG "* AFTER GROUPING MOVES *";

      Hashtbl.iter
      (fun mid0 mid1 -> DEBUG_MSG "%a -> %a" MID.ps mid0 MID.ps mid1)
      mid_chg_tbl;

      let tbl = Hashtbl.create 0 in
      edits#iter_moves
	(function
	  | Edit.Move(mid, _, (uid1, info1, ex1), (uid2, info2, ex2)) -> begin
	      let nd1 = Info.get_node info1 in
	      let gi1 = nd1#gindex in
	      try
		let c, gi, n = Hashtbl.find tbl !mid in
		Hashtbl.replace tbl !mid (if gi < gi1 then c+1, gi1, nd1 else c+1, gi, n)
	      with 
		Not_found -> Hashtbl.add tbl !mid (1, gi1, nd1)
	  end
	  | _ -> assert false
	);
      let keys = Hashtbl.fold (fun k _ l -> k::l) tbl [] in
      let sorted_keys = List.fast_sort Stdlib.compare keys in
      List.iter
	(fun k ->
	  let c, gi, n = Hashtbl.find tbl k in
	  DEBUG_MSG " %a root:%a(%a) size=%d" MID.ps k GI.ps gi UID.ps n#uid c
	) sorted_keys;

      DEBUG_MSG "edits:\n%s\n" edits#to_string;
    END_DEBUG;
   

    eliminate_false_moves tree1 tree2 edits uidmapping;


    BEGIN_DEBUG
      DEBUG_MSG "* AFTER FALSE MOVE ELIMINATION *";
      DEBUG_MSG "edits:\n%s\n" edits#to_string;
    END_DEBUG;

    examine_moves edits;

    (* eliminating odd relabels again *)
    let (* odd_movs_exist *) _ = eliminate_odd_relabels options tree1 tree2 edits uidmapping in

    let move_region_tbl = make_move_region_tbl tree1 tree2 edits in
    let parent_move_tbl = make_parent_move_tbl move_region_tbl edits in
    let child_move_tbl  = make_child_move_tbl parent_move_tbl in

    let suggested_pairs =
      handle_movrels options cenv tree1 tree2 edits uidmapping parent_move_tbl child_move_tbl
    in

    (* shrink moves in order to increase SPSM *)
    if options#shrink_moves_flag then begin
      let move_region_tbl = make_move_region_tbl tree1 tree2 edits in
      edits#shrink_moves_rp tree1 tree2 uidmapping move_region_tbl;
      let move_region_tbl = make_move_region_tbl tree1 tree2 edits in
      edits#shrink_moves tree1 tree2 uidmapping move_region_tbl
    end;

    let move_region_tbl = make_move_region_tbl tree1 tree2 edits in
    let parent_move_tbl = make_parent_move_tbl move_region_tbl edits in
    let child_move_tbl  = make_child_move_tbl parent_move_tbl in

    let suggested_pairs0 =
      handle_movrels options cenv tree1 tree2 edits uidmapping parent_move_tbl child_move_tbl
    in

    Xset.add_set suggested_pairs suggested_pairs0;
    BEGIN_DEBUG
      if Xset.length suggested_pairs > 0 then begin
        DEBUG_MSG "suggested_pairs:";
        Xset.iter 
          (fun (u1, u2) -> 
            DEBUG_MSG "%a-%a" UID.ps u1 UID.ps u2
          ) suggested_pairs
      end
    END_DEBUG;

    (* check moves *)
    DEBUG_MSG "checking moves...";

    let is_crossing_with_untouched ?(mask=[]) =
      edits#is_crossing_with_untouched ?mask:(Some mask) uidmapping
    in

    edits#iter_moves_topdown
      (function
	| Edit.Move(mid, kind, (_, info1, _), (_, info2, _)) as mov ->
            let nd1 = Info.get_node info1 in
            let nd2 = Info.get_node info2 in

            DEBUG_MSG "checking: %a %a-%a" MID.ps !mid UID.ps nd1#uid UID.ps nd2#uid;

            if not (is_crossing_with_untouched nd1 nd2) then begin
              DEBUG_MSG "not a move: %s" (Edit.to_string mov);
              edits#remove_edit mov
            end
        | _ -> assert false
      );

    DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;

    (* extra postprocessing *)
    if true then begin
      DEBUG_MSG "* STARTING EXTRA POSTPROCESSING";

      let _, keyroots_moderate = 
        let filt u1 u2 =
          not (edits#mem_mov12 u1 u2)
        in
        find_keyroots options ~relax:true ~ignore_sequence:true ~filt tree1 tree2 uidmapping 
      in
      let keyroots = keyroots_moderate in

      BEGIN_DEBUG
        List.iter 
        (fun (n1, n2) -> 
	  DEBUG_MSG "keyroot pair: %a(size=%d) - %a(size=%d)  %s[%s] - %s[%s]"
	    UID.ps n1#uid (tree1#whole_initial_subtree_size n1)
	    UID.ps n2#uid (tree2#whole_initial_subtree_size n2)
            n1#data#label (Loc.to_string n1#data#src_loc) 
            n2#data#label (Loc.to_string n2#data#src_loc)
        ) keyroots;
      END_DEBUG;

      if keyroots <> [] || Xset.length suggested_pairs > 0 then begin

        let starting_pairs = 
          Xlist.union 
            (List.map (fun (n1, n2) -> n1#uid, n2#uid) keyroots)
            (Xlist.filter_map
               (fun (u1, u2) ->
                 try
                   let n1 = tree1#search_node_by_uid u1 in
                   let n2 = tree2#search_node_by_uid u2 in
                   let p1 = n1#initial_parent in
                   let p2 = n2#initial_parent in
                   Some (p1#uid, p2#uid)
                 with
                 | Otreediff.Otree.Parent_not_found _
	         | Not_found -> None
               ) (Xset.to_list suggested_pairs))
        in

        uidmapping#set_starting_uid_pairs_for_glueing starting_pairs;
        let removed_pairs, added_pairs =
if not options#no_glue_flag then begin
          let removed_pairs, added_pairs, _ =
            let is_move n1 n2 = 
              edits#mem_mov12 n1#uid n2#uid || is_crossing_with_untouched n1 n2
            in
            glue_deletes_and_inserts options cenv tree1 tree2 
              ~override:true ~is_move ~downward:true ~no_moves:true
              uidmapping (new UIDmapping.c cenv)
          in
          removed_pairs, added_pairs
end
else
  [], []
        in
        BEGIN_DEBUG
          DEBUG_MSG "removed_pairs:";
        List.iter
          (fun (u1, u2) -> DEBUG_MSG "  %a-%a" UID.ps u1 UID.ps u2) removed_pairs;
        DEBUG_MSG "added_pairs:";
        List.iter
          (fun (u1, u2) -> DEBUG_MSG "  %a-%a" UID.ps u1 UID.ps u2) added_pairs;
        END_DEBUG;

        let is_mov nd1 nd2 =
          DEBUG_MSG "%a-%a" UID.ps nd1#uid UID.ps nd2#uid;
          let mid_opt = ref None in
          try
            List.iter
              (fun (rn1, rn2) ->
                if tree1#is_initial_ancestor rn1 nd1 && tree2#is_initial_ancestor rn2 nd2 then begin
                  DEBUG_MSG "keyroot pair: %a-%a" UID.ps rn1#uid UID.ps rn2#uid;
                  let movs = ref [] in

                  edits#iter_moves
                    (function
	              | Edit.Move(mid, _, (_, info1, _), (_, info2, _)) -> begin
                          let n1 = Info.get_node info1 in
                          let n2 = Info.get_node info2 in
                          if tree1#is_initial_ancestor rn1 n1 && tree2#is_initial_ancestor rn2 n2 then begin
                            movs := (mid, n1, n2) :: !movs
                          end
                      end
                      | _ -> assert false
                    );
                  
                  BEGIN_DEBUG
                    List.iter
                    (fun (mid, n1, n2) ->
                      DEBUG_MSG "  %a" MID.ps !mid
                    ) !movs
                  END_DEBUG;

                  tree1#scan_whole_initial_subtree rn1
                    (fun n1 ->
                      let u1 = n1#uid in
                      try
                        let u2 = uidmapping#find u1 in
                        if not (edits#mem_mov12 u1 u2) then
                          let n2 = tree2#search_node_by_uid u2 in
                          if 
                            UIDmapping.is_crossing_or_incompatible tree1 tree2 nd1 nd2 n1 n2
                          then begin
                            List.iter
                              (fun (m, mn1, mn2) ->
                                let crossing_or_incompat =
                                  UIDmapping.is_crossing_or_incompatible tree1 tree2 nd1 nd2 mn1 mn2
                                in
                                if not crossing_or_incompat then begin
                                  mid_opt := Some !m;
                                  raise Exit
                                end
                              ) !movs;
                            raise Exit
                          end
                      with
                        Not_found -> ()
                    )
                end
              ) keyroots;

            if is_crossing_with_untouched ~mask:added_pairs nd1 nd2 then
              true, None
            else
              false, None
          with
            Exit -> true, !mid_opt
        in (* is_mov *)

        sync_edits options ~is_mov ~check_conflicts:true tree1 tree2 edits removed_pairs added_pairs;

      end

    end; (* extra postprocessing *)


    if Hashtbl.length mid_chg_tbl > 0 then
      edits#iter_moves
	(function 
	  | Edit.Move(mid, _, _, _) -> begin
	      try
		let id = Hashtbl.find mid_chg_tbl !mid in
                DEBUG_MSG "!!! changing move id: %a --> %a" MID.ps !mid MID.ps id;
		mid := id
	      with
		Not_found -> ()
	  end
	  | _ -> assert false
	);



    (* identify relative permutations *)

    DEBUG_MSG "IDENTIFYING RELATIVE PERMUTATIONS...";

    let move_region_tbl = make_move_region_tbl tree1 tree2 edits in
    let parent_move_tbl = make_parent_move_tbl move_region_tbl edits in
    let child_move_tbl  = make_child_move_tbl parent_move_tbl in

    let _mid_fusion_tbl = Hashtbl.create 0 in

    Hashtbl.iter
      (fun mid _cmids ->
        if _cmids <> [] then begin
	  DEBUG_MSG "child moves of %a --> [%s]" MID.ps mid
	    (Xlist.to_string (fun m -> MID.to_string m) ";" _cmids);

          let cmids =
            List.filter
              (fun cmid ->
                let (_, g1, _, g2) = Hashtbl.find move_region_tbl cmid in
                let n1 = tree1#search_node_by_gindex g1 in
                let n2 = tree2#search_node_by_gindex g2 in
                DEBUG_MSG "root pair of %a --> %a-%a" MID.ps cmid UID.ps n1#uid UID.ps n2#uid;
                let is_crossing_or_incompatible = 
                  UIDmapping.is_crossing_or_incompatible tree1 tree2 n1 n2 
                in
                try
                  edits#iter_moves
                    (function
	              | Edit.Move(m, _, (_, inf1, _), (_, inf2, _)) ->
	                  if !m = mid then
                            let n1' = Info.get_node inf1 in
                            let n2' = Info.get_node inf2 in
                            let b = is_crossing_or_incompatible n1' n2' in
                            DEBUG_MSG "crossing_or_incompatible %a-%a --> %B" UID.ps n1'#uid UID.ps n2'#uid b;
                            if b then
                              raise Exit
	              | _ -> assert false
                    );
                  true
                with
                  Exit -> false
              ) _cmids
          in

	  DEBUG_MSG "child moves of %a (filtered) --> [%s]" MID.ps mid
	    (Xlist.to_string (fun m -> MID.to_string m) ";" cmids);


          let pair_weight_list =
            List.map
              (fun cmid ->
                try
                  let (_, g1, _, g2) = Hashtbl.find move_region_tbl cmid in
                  let n1 = tree1#search_node_by_gindex g1 in
                  let n2 = tree2#search_node_by_gindex g2 in
                  let sz = estimate_cost_of_move tree1 tree2 uidmapping n1 n2 in
                  (n1, n2, sz)
                with
                  Not_found -> assert false
              ) cmids
          in
          let compat, _ = 
            UIDmapping.select_compatible_and_not_crossing_pairs tree1 tree2 pair_weight_list 
          in

          List.iter
            (fun (n1, n2, _) ->
              try
                let m = edits#find_mid12 n1#uid n2#uid in
                DEBUG_MSG "compatible move: %a -> %a" MID.ps m MID.ps mid;
                Hashtbl.add _mid_fusion_tbl m mid
              with
                Not_found -> assert false
            ) compat

        end
      ) child_move_tbl;

    (* calculate fixpoint *)
    let mid_fusion_tbl = Hashtbl.create 0 in
    Hashtbl.iter
      (fun m0 m1 ->
        let rec find m =
          try
            let m' = Hashtbl.find _mid_fusion_tbl m in
            find m'
          with
            Not_found -> m
        in
        let m' = find m1 in
        DEBUG_MSG "changing move id: %a --> %a" MID.ps m0 MID.ps m';
        Hashtbl.add mid_fusion_tbl m0 m'
      ) _mid_fusion_tbl;

    if Hashtbl.length mid_fusion_tbl > 0 then begin
      edits#iter_moves
	(function 
	  | Edit.Move(mid, _, _, _) -> begin
	      try
		let id = Hashtbl.find mid_fusion_tbl !mid in
		mid := id
	      with
		Not_found -> ()
	  end
	  | _ -> assert false
	)
    end;

    (* eliminate small move of unnamed entities *)
    if options#no_unnamed_node_move_flag then begin
      let is_unnamed_pair n1 n2 =
        (not n1#data#is_named_orig || not n2#data#is_named_orig) &&
        (not n1#data#has_non_trivial_value || not n2#data#has_non_trivial_value) ||
        n1#data#move_disallowed || n2#data#move_disallowed
      in
      ignore (decompose_moves is_unnamed_pair options edits uidmapping 1)
    end;

    (* *)
    if options#dump_delta_flag then begin
      elaborate_edits_for_delta options tree1 tree2 uidmapping edits;

      let dels, inss =
        if options#no_moves_flag then begin
          decompose_moves (fun _ _ -> true) options edits uidmapping 0
        end
        else begin
          (*let is_common_pair n1 n2 =
            n1#data#is_common || n2#data#is_common
          in
          ignore (decompose_moves is_common_pair options edits uidmapping 1);*)

          let is_unnamed_or_changed_pair n1 n2 =
            let b =
            (not
               (n1#data#eq n2#data &&
                (n1#data#is_named_orig && n2#data#is_named_orig ||
                n1#data#has_non_trivial_value && n2#data#has_non_trivial_value)
               )) ||
            n1#data#move_disallowed || n2#data#move_disallowed ||
            n1#data#is_common || n2#data#is_common
            in
            DEBUG_MSG "%s %s -> %B" n1#data#label n2#data#label b;
            b
          in
          decompose_moves is_unnamed_or_changed_pair options edits uidmapping 16
        end
      in
      let glue_filt u1 u2 =
        let n1 = tree1#search_node_by_uid u1 in
        let n2 = tree2#search_node_by_uid u2 in
        not n1#data#is_named_orig && not n2#data#is_named_orig &&
        n1#data#anonymized_label = n2#data#anonymized_label
      in
      let glue_filt =
        if options#no_moves_flag then begin
          fun u1 u2 ->
            let n1 = tree1#search_node_by_uid u1 in
            let n2 = tree2#search_node_by_uid u2 in
            n1#data#anonymized_label = n2#data#anonymized_label
        end
        else
          glue_filt
      in
      let rec find_mapped_anc umap is_mov add tree' n =
        let u = n#uid in
        try
          let u' = umap u in
          if is_mov u u' then
            find_mapped_anc umap is_mov add tree' n#initial_parent
          else
            let n' = tree'#search_node_by_uid u' in
            add n n' u u'
        with
          Not_found -> find_mapped_anc umap is_mov add tree' n#initial_parent
      in
      let starting_pairs = Xset.create 0 in
      let add12 n n' u u' = Xset.add starting_pairs (n#gindex, u, u') in
      let add21 n' n u' u = Xset.add starting_pairs (n#gindex, u, u') in
      Xset.iter (find_mapped_anc uidmapping#find edits#mem_mov12 add12 tree2) dels;
      Xset.iter (find_mapped_anc uidmapping#inv_find edits#mem_mov21 add21 tree1) inss;
      let starting_pair_list =
        List.map (fun (_, u1, u2) -> u1, u2)
          (List.fast_sort
             (fun (i, _, _) (j, _, _) -> Stdlib.compare j i)
             (Xset.to_list starting_pairs))
      in
      uidmapping#set_starting_uid_pairs_for_glueing starting_pair_list;
      let is_move n1 n2 =
        edits#mem_mov12 n1#uid n2#uid || is_crossing_with_untouched n1 n2(* ||
        let ca1 = n1#initial_parent#initial_children in
        let ca2 = n2#initial_parent#initial_children in
        let nc1 = Array.length ca1 in
        nc1 > 1 && (Array.length ca2) > 1 &&
        let b =
          try
            for i = n1#initial_pos + 1 to nc1 - 1 do
              let rn = ca1.(i) in
              if not rn#data#is_named_orig then
                for j = 0 to n2#initial_pos - 1 do
                  let ln = ca2.(j) in
                  if
                    not ln#data#is_named_orig &&
                    rn#data#anonymized_label = ln#data#anonymized_label
                  then
                    raise Exit
                done
            done;
            false
          with
            Exit -> true
        in
        b*)
      in
if not options#no_glue_flag then begin
      let rps, aps, _ =
        glue_deletes_and_inserts options cenv tree1 tree2
          ~no_mapping_override:true ~no_moves:true ~is_move ~glue_filt
          uidmapping pre_uidmapping
      in
      sync_edits options tree1 tree2 edits rps aps;
end;
      if options#no_moves_flag then begin
        ignore (decompose_moves (fun _ _ -> true) options edits uidmapping 0);
      end;

    end;

    (*DEBUG_MSG "pre_uidmapping:\n%s\n" pre_uidmapping#to_string;*)
    let nmap n = tree2#search_node_by_uid (uidmapping#find n#uid) in
    let nmap' n = tree1#search_node_by_uid (uidmapping#inv_find n#uid) in
    let changed_flag = ref true in
    let mid_change_tbl = Hashtbl.create 0 in
    while !changed_flag do
      changed_flag := false;

    edits#iter_deletes
      (function
	| Edit.Delete(whole, uid, info, excludes) as del -> begin
            try
	      let nd = tree1#search_node_by_uid uid in

              begin
                try
                  let nd_opt' = ref None in
                  let cs' =
                    Array.map
                      (fun c ->
                        let cu = c#uid in
                        let cu' = uidmapping#find cu in
                        match edits#find12 cu cu' with
                        | [] | [Edit.Relabel _] -> begin
                            let c' = tree2#search_node_by_uid cu' in
                            let n' = c'#initial_parent in
                            match !nd_opt' with
                            | None -> begin
                                nd_opt' := Some n';
                                c'
                            end
                            | Some x when x == n' -> c'
                            | _ -> raise Not_found
                        end
                        | _ -> raise Not_found
                      ) nd#initial_children
                  in
                  match !nd_opt' with
                  | Some nd' when nd'#initial_children = cs' -> begin
                      if not options#no_moves_flag || not (is_crossing_with_untouched nd nd') then begin
                        let uid' = nd'#uid in
                        let ins = edits#find_ins uid' in
	                if not (nd#data#eq nd'#data) then begin
                          if nd#data#relabel_allowed nd'#data then
	                    edits#add_edit (Edit.make_relabel nd nd')
                          else
                            raise Not_found
                        end;
                        DEBUG_MSG "del-ins pair found: %s-%s"
                          (Edit.to_string del) (Edit.to_string ins);
                        edits#remove_edit del;
                        edits#remove_edit ins;
                        ignore (uidmapping#add_settled uid uid');
                        changed_flag := true;
                        raise Exit
                      end
                  end
                  | _ -> ()
                with
                  Not_found -> ()
              end;

              let pnd = nd#initial_parent in
              let puid = pnd#uid in

              let uid', nd', puid' =
                try
                  let uid' = pre_uidmapping#find uid in
	          let nd' = tree2#search_node_by_uid uid' in
                  let puid' = nd'#initial_parent#uid in
                  DEBUG_MSG "pre_uidmapping: %a -> %a" UID.ps uid UID.ps uid';
                  uid', nd', puid'
                with
                  Not_found ->
                    let puid' = uidmapping#find puid in
                    let pnd' = tree2#search_node_by_uid puid' in

                    let cl' =
                      Array.fold_left
                        (fun l c ->
                          if c != nd then begin
                            let c' = nmap c in
                            if c'#initial_parent == pnd' then
                              c' :: l
                            else
                              raise Not_found
                          end
                          else
                            nd :: l
                        ) [] pnd#initial_children
                    in
                    let l, cl =
                      Array.fold_left
                        (fun (l, cl) c' ->
                          if uidmapping#mem_cod c'#uid then begin
                            let c = nmap' c' in
                            if c#initial_parent == pnd then
                              l, c' :: cl
                            else
                              raise Not_found
                          end
                          else
                            c' :: l, nd :: cl
                        ) ([], []) pnd'#initial_children
                    in
                    if cl <> cl' then
                      raise Not_found;

                    match l with
                    | [nd'] -> nd'#uid, nd', puid'
                    | _ -> raise Not_found
              in

              let base_cond =
                let ds =
                  Sourcecode.find_nearest_mapped_descendant_nodes uidmapping#mem_dom nd
                in
                let ds' =
                  Sourcecode.find_nearest_mapped_descendant_nodes uidmapping#mem_cod nd'
                in
                List.map nmap ds = ds'
              in

              if base_cond then

              let ins = edits#find_ins uid' in

              let cond0 =
                try
                  uidmapping#find puid = puid' &&
                  match edits#find12 puid puid' with
                  | [] | [Edit.Relabel _] -> true
                  | _ -> false
                with
                  Not_found -> false
              in
              let cond1 =
                let ok = ref false in
                Array.for_all
                  (fun c ->
                    try
                      let cu = c#uid  in
                      let cu' = uidmapping#find cu in
	              let c' = tree2#search_node_by_uid cu' in
                      c'#initial_parent == nd' &&
                      match edits#find12 cu cu' with
                      | [] | [Edit.Relabel _] -> ok := true; true
                      | _ -> false
                    with
                      Not_found -> true
                  ) nd#initial_children &&
                Array.for_all
                  (fun c' ->
                    try
                      let cu' = c'#uid  in
                      let cu = uidmapping#inv_find cu' in
	              let c = tree1#search_node_by_uid cu in
                      c#initial_parent == nd &&
                      match edits#find12 cu cu' with
                      | [] | [Edit.Relabel _] -> true
                      | _ -> false
                    with
                      Not_found -> true
                  ) nd'#initial_children && !ok
              in
              if (cond0 || cond1) && (not options#no_moves_flag || not (is_crossing_with_untouched nd nd')) then begin
	        if not (nd#data#eq nd'#data) then begin
                  if nd#data#relabel_allowed nd'#data then
	            edits#add_edit (Edit.make_relabel nd nd')
                  else
                    raise Not_found
                end;
                begin
                  match ins with
                  | Edit.Insert(_, uid', info', ex') -> begin
                      (*let nd' = tree2#search_node_by_uid uid' in*)
                      if is_crossing_with_untouched nd nd' then
                        let mid = options#moveid_generator#gen in
                        begin
                          try
                            let ml = ref [] in
                            Array.iteri
                              (fun i c ->
                                match edits#find_mov1 c#uid with
                                | Edit.Move(m, _, (_, info1, _), (_, info2, _)) -> begin
                                    let c' = Info.get_node info2 in
                                    DEBUG_MSG "%a -> %a" UID.ps c#uid UID.ps c'#uid;
                                    if nd'#initial_children.(i) == c' then
                                      ml := m :: !ml
                                    else
                                      raise Exit
                                end
                                | _ -> assert false
                              ) nd#initial_children;
                            List.iter
                              (fun m ->
                                DEBUG_MSG "%a -> %a" MID.ps !m MID.ps mid;
                                Hashtbl.replace mid_change_tbl !m mid
                              ) !ml
                          with
                            _ -> ()
                        end;
                        let mov = Edit.make_move_permutation mid (uid, info) (uid', info') in
                        edits#add_edit mov;
                  end
                  | _ -> assert false
                end;
                DEBUG_MSG "del-ins pair found: %s-%s" (Edit.to_string del) (Edit.to_string ins);
                edits#remove_edit del;
                edits#remove_edit ins;
                ignore (uidmapping#add_settled uid uid');
                (*if not base_cond then begin
                  match ins with
                  | Edit.Insert(_, uid', info', _) ->
                      let mid = options#moveid_generator#gen in
                      let mov = Edit._make_move mid Edit.Modd (uid, info) (uid', info') in
                      edits#add_edit mov
                  | _ -> assert false
                end;*)
                changed_flag := true
              end
            with
            | Not_found -> ()
            | Exit -> ()
        end
	| _ -> ()
      )
    done;
    if Hashtbl.length mid_change_tbl > 0 then begin
      edits#iter_moves
	(function
	  | Edit.Move(mid, _, _, _) -> begin
	      try
		let id = Hashtbl.find mid_change_tbl !mid in
		mid := id
	      with
		Not_found -> ()
	  end
	  | _ -> assert false
	)
    end;

    (* final fixup *)

    edits#iter_relabels
      (function
	| Edit.Relabel(movrel, (u1, _, _), (u2, _, _)) ->
	    if edits#mem_mov12 u1 u2 then
	      movrel := true
	| _ -> assert false
      );

    (* count inserts and deletes *)
    edits#iter_deletes_and_inserts 
      (function
	| Edit.Delete(whole, uid, info, excludes) ->
	    if whole then 
	      assert false
	    else begin
	      Info.set_size info 1;
	    end

	| Edit.Insert(whole, uid, info, excludes) -> 
	    if whole then 
	      assert false
	    else begin
	      Info.set_size info 1;
	    end
	| _ -> assert false
      );


    let delete_exclude_map = Hashtbl.create 0 in
    let insert_exclude_map = Hashtbl.create 0 in
    let relabel_exclude_map1 = Hashtbl.create 0 in
    let relabel_exclude_map2 = Hashtbl.create 0 in
    let move_exclude_map1 = Hashtbl.create 0 in
    let move_exclude_map2 = Hashtbl.create 0 in
    let insert_link_map = Hashtbl.create 0 in
    let delete_link_map = Hashtbl.create 0 in
    let relabel_link_map1 = Hashtbl.create 0 in
    let relabel_link_map2 = Hashtbl.create 0 in
    let move_link_map1 = Hashtbl.create 0 in
    let move_link_map2 = Hashtbl.create 0 in

    let rec trace_link map uid =
      try
	let uids = Hashtbl.find map uid in
	uid::(List.flatten(List.map (trace_link map) uids))
      with Not_found -> [uid]
    in

    let get_excludes exclude_map link_map uid =
      let uids = trace_link link_map uid in

      DEBUG_MSG "(%a): members: [%s]" UID.ps uid
	(Xlist.to_string UID.to_string "," uids);

      let excludes =
	List.fold_left 
	  (fun l uid -> 
	    let e = 
	      try 
		Hashtbl.find exclude_map uid 
	      with 
		Not_found -> [] 
	    in

	    DEBUG_MSG "(%a): excludes: [%s]" UID.ps uid 
	      (Xlist.to_string 
		 (fun i -> UID.to_string(Info.get_uid i)) ";" e);

	    Xlist.union l e
	  ) [] uids
      in

      DEBUG_MSG "(%a): [%s]" UID.ps uid 
	(Xlist.to_string 
	   (fun i -> UID.to_string(Info.get_uid i)) ";" excludes);

      excludes
    in (* end of func get_excludes *)

    let mkexcl = List.map mkinfo in
    let map_add m k v = 

      DEBUG_MSG "(link) map_add: %a -> %a" UID.ps k UID.ps v;

      try 
	let vs = Hashtbl.find m k in
	if not (List.mem v vs) then Hashtbl.replace m k (v::vs)
      with Not_found -> Hashtbl.add m k [v]
    in (* end of func map_add *)



    DEBUG_MSG "GROUPING EDITS...";

    let split_hunk_flag = options#split_hunk_flag in(*!!!!!*)

    let check_hunk_boundary =(*!!!!!*)
      let check pnd cnd =
        Label.is_hunk_boundary (getlab pnd) (getlab cnd)
      in
      if split_hunk_flag then
        check
      else
        fun _ _ -> false
    in

    let handle_excludes =(*!!!!!*)
      let normal (*tree*) w eref nd children filt =
        if w then
          !eref
        else
          mkexcl (List.filter filt children)
      in
      let split (*tree*) w eref nd children filt =
        (*let whole =
          try
            tree#scan_whole_initial_subtree nd
              (fun n -> if filt n then raise Exit);
            true
          with
            Exit -> false
        in
        DEBUG_MSG "whole=%B" whole;
        if whole then*)
          mkexcl
            (List.filter
               (fun n ->
                 filt n || check_hunk_boundary nd n
               ) children)
        (*else
          normal tree w eref nd children filt*)
      in
      if split_hunk_flag then
        split
      else
        normal
    in

    edits#iter (* group edits *)
      (fun ed ->
	DEBUG_MSG "[grouping] %s" (Edit.to_string ed);
	
	match ed with
	| Edit.Delete(whole, uid, info, excludes) ->
	    let nd = Info.get_node info in
	    let ichildren = Array.to_list nd#initial_children in
	    let excl =
              handle_excludes (*tree1*) whole excludes nd ichildren
                (fun n -> uidmapping#mem n#uid) (* not deleted *)(*!!!!!*)
              (*if whole then
                !excludes
              else
                mkexcl (List.filter (fun n -> uidmapping#mem n#uid) ichildren)*)
	    in

	    BEGIN_DEBUG
	      DEBUG_MSG "[grouping] Delete: children of\t <%a>: [%s]"
		UID.ps uid (Xlist.to_string UID.to_string "," 
		       (List.map (fun nd -> nd#uid) ichildren));
	      DEBUG_MSG "[grouping] Delete: excl: [%s]"
              (Xlist.to_string Info.to_string ", " excl);
	    END_DEBUG;

	    if nd#has_initial_parent then begin
              let pnd = nd#initial_parent in
	      let puid = pnd#uid in
              (*let w =
                try
                  tree1#scan_whole_initial_subtree pnd
                    (fun n -> if uidmapping#mem n#uid then raise Exit);
                  true
                with
                  Exit -> false
              in!!!!!*)
	      if
                not (uidmapping#mem puid) (* puid is deleted *) &&
                not ((*w &&*) check_hunk_boundary pnd nd)(*!!!!!*)
              then
		map_add delete_link_map puid uid
	    end;

	    Hashtbl.add delete_exclude_map uid excl;

	    DEBUG_MSG "[grouping] delete_exclude_map: added: %a -> [%s]" 
	      UID.ps uid (Xlist.to_string Info.to_string ", " excl)

	| Edit.Insert(whole, uid, info, excludes) -> 
	    let nd = Info.get_node info in
	    let ichildren = Array.to_list nd#initial_children in

	    DEBUG_MSG "[grouping] Insert: whole=%B children of %a: [%s]"
	      whole UID.ps uid (Xlist.to_string UID.to_string "," 
				  (List.map (fun nd -> nd#uid) ichildren));

	    let excl =
              handle_excludes (*tree2*) whole excludes nd ichildren
                (fun n -> uidmapping#mem_cod n#uid) (* not inserted *)(*!!!!!*)
              (*if whole then
                !excludes
              else
                mkexcl (List.filter (fun n -> uidmapping#mem_cod n#uid) ichildren)*)
	    in

	    DEBUG_MSG "[grouping] Insert: excl: [%s]"
	      (Xlist.to_string Info.to_string ", " excl);

	    if nd#has_initial_parent then begin
              let pnd = nd#initial_parent in
              let puid = pnd#uid in
              (*let w =
                try
                  tree1#scan_whole_initial_subtree pnd
                    (fun n -> if uidmapping#mem_cod n#uid then raise Exit);
                  true
                with
                  Exit -> false
              in!!!!!*)
	      if
                not (uidmapping#mem_cod puid) (* puid is inserted *) &&
                not ((*w &&*) check_hunk_boundary pnd nd)(*!!!!!*)
              then
		map_add insert_link_map puid uid
	    end;

	    Hashtbl.add insert_exclude_map uid excl;

	    DEBUG_MSG "[grouping] insert_exclude_map: added: %a -> [%s]" 
	      UID.ps uid (Xlist.to_string Info.to_string ", " excl)

	| Edit.Relabel(_, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	    if options#group_relabels_flag then
	      let nd1 = Info.get_node info1 in
	      let nd2 = Info.get_node info2 in

	      DEBUG_MSG "[grouping] Relabel: %a collapsed=%B" 
		UID.ps uid1 nd1#is_collapsed;
	      DEBUG_MSG "[grouping] Relabel: %a collapsed=%B" 
		UID.ps uid2 nd2#is_collapsed;

	      let filt1 nd =
		let uid = nd#uid in

		match edits#find1 uid with
		| [] -> true
		| [Edit.Relabel(_, (u1, _, _), (u2, i2, _))] ->
		    let n2 = Info.get_node i2 in
		    if n2#has_initial_parent then
		      not (n2#initial_parent#uid = uid2)
		    else true 
		| [Edit.Relabel(_, (u1, _, _), (u2, i2, _));Edit.Move(_, s, _, _)]
		| [Edit.Move(_, s, _, _);Edit.Relabel(_, (u1, _, _), (u2, i2, _))] 
		  ->
		    let n2 = Info.get_node i2 in
		    if n2#has_initial_parent then
		      not (!s = Edit.Mnormal && n2#initial_parent#uid = uid2)
		    else true 
		| [_] -> true
		| eds -> 
		    Xprint.message "[grouping] edits for %a:\n%s" UID.ps nd#uid
		      (Xlist.to_string Edit.to_string "\n" eds);
		    assert false
	      in
	      let filt2 nd =
		match edits#find2 nd#uid with
		| [] -> true
		| [Edit.Relabel(_, (u1, i1, _), (u2, _, _))] ->
		    let n1 = Info.get_node i1 in
		    if n1#has_initial_parent then
		      not (n1#initial_parent#uid = uid1)
		    else true
		| [Edit.Relabel(_, (u1, i1, _), (u2, _, _));Edit.Move(_, s, _, _)]
		| [Edit.Move(_, s, _, _);Edit.Relabel(_, (u1, i1, _), (u2, _, _))] 
		  ->
		    let n1 = Info.get_node i1 in
		    if n1#has_initial_parent then
		      not (!s = Edit.Mnormal && n1#initial_parent#uid = uid1)
		    else true
		| [_] -> true
		| eds ->
		    Xprint.message "[grouping] edits for %a:\n%s" UID.ps nd#uid
		      (Xlist.to_string Edit.to_string "\n" eds);
		    assert false
	      in

	      let children1 = Array.to_list nd1#initial_children in
	      let children2 = Array.to_list nd2#initial_children in

	      let excl1 = mkexcl (List.filter filt1 children1) in
	      let excl2 = mkexcl (List.filter filt2 children2) in

	      DEBUG_MSG ("\n[grouping] Relabel: children1 of\t <%a>: [%s]\n" ^^
			 "[grouping] Relabel: excl1: [%s]\n" ^^
			 "[grouping] Relabel children2 of\t <%a>: [%s]\n" ^^
			 "[grouping] Relabel: excl2: [%s]\n" ^^
			 "[grouping] relabel_exclude_map1: added: %a -> [%s]\n" ^^
			 "[grouping] relabel_exclude_map2: added: %a -> [%s]")
		UID.ps uid1 (Xlist.to_string UID.to_string "," 
			       (List.map (fun nd -> nd#uid) children1))
		(Xlist.to_string Info.to_string ", " excl1)
		UID.ps uid2 (Xlist.to_string UID.to_string "," 
			       (List.map (fun nd -> nd#uid) children2))
		(Xlist.to_string Info.to_string ", " excl2)
		UID.ps uid1 (Xlist.to_string Info.to_string ", " excl1)
		UID.ps uid2 (Xlist.to_string Info.to_string ", " excl2);


	      Hashtbl.add relabel_exclude_map1 uid1 excl1;
	      Hashtbl.add relabel_exclude_map2 uid2 excl2;
	      
	      begin
		try
		  let pnd1, pnd2 = nd1#initial_parent, nd2#initial_parent in
		  let puid1, puid2 = pnd1#uid, pnd2#uid in

		  DEBUG_MSG "[grouping] parent uids: %a, %a" UID.ps puid1 UID.ps puid2;

		  let is_permu_top =
		    match edits#find1 uid1 with
		    | [Edit.Relabel _] -> false
		    | [Edit.Move(_, s, _, _);Edit.Relabel _]
		    | [Edit.Relabel _;Edit.Move(_, s, _, _)] ->
			!s = Edit.Mpermutation
		    | _ -> assert false
		  in
		  
		  if not is_permu_top then
		    match edits#find1 puid1 with
		    | [] -> ()
		    | [Edit.Relabel(_, (u1, _, _), (u2, _, _))]
		    | [Edit.Relabel(_, (u1, _, _), (u2, _, _));Edit.Move _]
		    | [Edit.Move _;Edit.Relabel(_, (u1, _, _), (u2, _, _))] ->
			if u2 = puid2 then begin
			  map_add relabel_link_map1 puid1 uid1;
			  map_add relabel_link_map2 puid2 uid2
			end
		    | [_] -> ()
		    | _ -> assert false
			  
		with Otreediff.Otree.Parent_not_found _ -> ()
	      end
		
	| Edit.Move(mid, kind, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	    let nd1 = Info.get_node info1 in
	    let nd2 = Info.get_node info2 in

	    let filt1 is_mov nd =
	      let uid = nd#uid in

	      match edits#find1 uid with
	      | [] -> true
	      | [Edit.Move(id, k, (u1, _, _), (u2, i2, _));Edit.Relabel _]
	      | [Edit.Relabel _;Edit.Move(id, k, (u1, _, _), (u2, i2, _))]
	      | [Edit.Move(id, k, (u1, _, _), (u2, i2, _))] ->

		  DEBUG_MSG "[grouping] filt1(%a:%a): mov: mid=%a"
                    UID.ps uid GI.ps nd#gindex MID.ps !id;

		  if !mid = !id then
		    if is_mov uid then
		      false 
		    else 
		      true

		  else
(*
		    let n2 = Info.get_node i2 in
		    if n2#has_initial_parent then begin
		      not (!k = Edit.Mnormal && n2#initial_parent#uid = uid2)
                    end
		    else 
*)
		      true

	      | [Edit.Relabel(_, (u1, _, _), (u2, i2, _))] ->

		  DEBUG_MSG "[grouping] filt1(%a:%a): rel" UID.ps uid GI.ps nd#gindex;
(*
		  let n2 = Info.get_node i2 in
		  if n2#has_initial_parent then
		    n2#initial_parent#uid <> uid2
		  else 
*)
		    true

	      | [_] -> true
	      | eds -> 
		  DEBUG_MSG "[grouping] incompatible edits found:\n%s" 
		    (Xlist.to_string Edit.to_string "\n" eds);
		  assert false
	    in
	    let filt2 is_mov nd =
	      let uid = nd#uid in
	      match edits#find2 nd#uid with
	      | [] -> true
	      | [Edit.Move(id, k, (u1, i1, _), (u2, _, _));Edit.Relabel _]
	      | [Edit.Relabel _;Edit.Move(id, k, (u1, i1, _), (u2, _, _))]
	      | [Edit.Move(id, k, (u1, i1, _), (u2, _, _))] -> 

		  DEBUG_MSG "[grouping] filt2(%a:%a): mov: mid=%a" UID.ps uid GI.ps nd#gindex MID.ps !id;

		  if !mid = !id then 
		    if is_mov uid then 
		      false 
		    else 
		      true

		  else
(*
		    let n1 = Info.get_node i1 in
		    if n1#has_initial_parent then begin
		      not (!k = Edit.Mnormal && n1#initial_parent#uid = uid1)
                    end
		    else 
*)
		      true

	      | [Edit.Relabel(_, (u1, i1, _), (u2, _, _))] ->

		  DEBUG_MSG "[grouping] filt2(%a:%a): rel" UID.ps uid GI.ps nd#gindex;
(*
		  let n1 = Info.get_node i1 in
		  if n1#has_initial_parent then
		    n1#initial_parent#uid <> uid1
		  else 
*)
		    true

	      | [_] -> true
	      | eds -> 
		  DEBUG_MSG "[grouping] edits found:\n%s" 
		    (Xlist.to_string Edit.to_string "\n" eds);
		  assert false
	    in

	    let children1 = Array.to_list nd1#initial_children in
	    let children2 = Array.to_list nd2#initial_children in
	    let children_uids1 = List.map (fun nd -> nd#uid) children1 in
	    let children_uids2 = List.map (fun nd -> nd#uid) children2 in

	    let is_mov1 u =
	      let b =
		try
		  let v = uidmapping#find u in
		  List.memq v children_uids2
		with 
		  Not_found -> false
	      in
	      DEBUG_MSG "[grouping] is_mov1: %a --> %B" UID.ps u b;
	      b
	    in
	    let is_mov2 u = 
	      let b = 
		try
		  let v = uidmapping#inv_find u in
		  List.memq v children_uids1
		with 
		  Not_found -> false
	      in
	      DEBUG_MSG "[grouping] is_mov2: %a --> %B" UID.ps u b;
	      b
	    in

            let filt1' n = filt1 is_mov1 n || check_hunk_boundary nd1 n(*!!!!!*) in
            let filt2' n = filt2 is_mov2 n || check_hunk_boundary nd2 n(*!!!!!*) in

	    let excl1 = mkexcl (List.filter filt1' children1) in
	    let excl2 = mkexcl (List.filter filt2' children2) in

	    DEBUG_MSG ("\n[grouping] Move: children1 of\t <%a>: [%s]\n" ^^
		       "[grouping] Move: excl1: [%s]\n" ^^
		       "[grouping] Move: children2 of\t <%a>: [%s]\n" ^^
		       "[grouping] Move: excl2: [%s]")
	      UID.ps uid1 (Xlist.to_string UID.to_string "," 
			     (List.map (fun nd -> nd#uid) children1))
	      (Xlist.to_string Info.to_string ", " excl1)
	      UID.ps uid2 (Xlist.to_string UID.to_string "," 
			     (List.map (fun nd -> nd#uid) children2))
	      (Xlist.to_string Info.to_string ", " excl2);

	    begin
	      try
		let pnd1, pnd2 = nd1#initial_parent, nd2#initial_parent in
		let puid1, puid2 = pnd1#uid, pnd2#uid in

		DEBUG_MSG "[grouping] parent uids: %a, %a" UID.ps puid1 UID.ps puid2;

(*		let parent_move_not_found = ref true in *)

		List.iter 
		  (function
		    | Edit.Move(id, k, (u1, i1, _), (u2, i2, _)) -> 
			if u2 = puid2 then begin
(*			  parent_move_not_found := false; *)

			  if
                            !id = !mid &&
                            not (check_hunk_boundary pnd1 nd1)(*!!!!!*)
                          then begin
			    map_add move_link_map1 puid1 uid1;
			    map_add move_link_map2 puid2 uid2
			  end

			end
		    | _ -> ()
		  ) (edits#find1 puid1);

(*
		if !parent_move_not_found && !kind = Edit.Modd then begin

		    DEBUG_MSG "kind changed: %d: %s -> %s (group edits)"
		      !mid (Edit.move_kind_to_string !kind) (Edit.move_kind_to_string Edit.Mnormal);

		  kind := Edit.Mnormal
		end
*)
	      with 
		Otreediff.Otree.Parent_not_found _ -> ()
	    end;

	    BEGIN_DEBUG
	      DEBUG_MSG "[grouping] move_exclude_map1: added: %a -> [%s]"
		UID.ps uid1 (Xlist.to_string Info.to_string ", " excl1);
	      DEBUG_MSG "[grouping] move_exclude_map2: added: %a -> [%s]"
		UID.ps uid2 (Xlist.to_string Info.to_string ", " excl2)
	    END_DEBUG;

	    Hashtbl.add move_exclude_map1 uid1 excl1;
	    Hashtbl.add move_exclude_map2 uid2 excl2
      ); (* end of group edits *)
    


    let to_be_filtered1 = Xset.create 0 in
    let to_be_filtered2 = Xset.create 0 in

    let add_to_be_filtered to_be_filtered edtag uid uids =

      DEBUG_MSG "%a -> [%s]" UID.ps uid
	(Xlist.to_string UID.to_string ";" uids);

      List.iter 
	(fun u ->
	  if u = uid then 
	    () 
	  else 
	    Xset.add to_be_filtered (edtag, u)
	) uids
    in

    let add_to_be_filtered1 = add_to_be_filtered to_be_filtered1 in
    let add_to_be_filtered2 = add_to_be_filtered to_be_filtered2 in

(*
    let remove_to_be_filtered to_be_filtered edtag uid =

      DEBUG_MSG "%a" UID.ps uid;

      Xset.remove to_be_filtered (edtag, uid)
    in

    let remove_to_be_filtered1 = remove_to_be_filtered to_be_filtered1 in
    let remove_to_be_filtered2 = remove_to_be_filtered to_be_filtered2 in
*)
    
    let mem_to_be_filtered to_be_filtered (edtag, u) =
      Xset.mem to_be_filtered (edtag, u)
    in
    let mem_to_be_filtered1 = mem_to_be_filtered to_be_filtered1 in
    let mem_to_be_filtered2 = mem_to_be_filtered to_be_filtered2 in
    

    (* filtering out redundant edits *)

    let prev_rel2_lm2 = ref GI.dummy in
    let prev_rel2 = ref GI.dummy in

    let prev_mov2_lm2 = ref GI.dummy in
    let prev_mov2 = ref GI.dummy in

    edits#iter_topdown
      (fun ed ->

	DEBUG_MSG "filter: %s" (Editop.to_string ed);

	match ed with
	| Edit.Delete(whole, uid, _, excludes) ->
	    if not whole || split_hunk_flag(*!!!!!*) then begin
	      if mem_to_be_filtered1 (Edel, uid) then
		()
	      else begin
		excludes := get_excludes delete_exclude_map delete_link_map uid;
		add_to_be_filtered1 Edel uid (trace_link delete_link_map uid)
	      end
	    end

	| Edit.Insert(whole, uid, _, excludes) ->
	    if not whole || split_hunk_flag(*!!!!!*) then begin
	      if mem_to_be_filtered2 (Eins, uid) then 
		()
	      else begin
		excludes := get_excludes insert_exclude_map insert_link_map uid;
		add_to_be_filtered2 Eins uid (trace_link insert_link_map uid)
	      end
	    end

	| Edit.Relabel(_, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	    if options#group_relabels_flag then begin
	      let nd2 = Info.get_node info2 in
	      let gid2 = nd2#gindex in

	      let no_swap = 
		if !prev_rel2_lm2 = GI.dummy && !prev_rel2 = GI.dummy then
		  false
		else
		  !prev_rel2_lm2 <= gid2 && gid2 < !prev_rel2
	      in

	      BEGIN_DEBUG
                let nd1 = Info.get_node info1 in
                let gid1 = nd1#gindex in
		DEBUG_MSG "filter: relabel: %a-%a" GI.ps gid1 GI.ps gid2;
		DEBUG_MSG "prev_rel2_lm2=%a prev_rel2=%a" GI.ps !prev_rel2_lm2 GI.ps !prev_rel2;
		DEBUG_MSG "filter: no_swap=%B" no_swap;
	      END_DEBUG;

	      if no_swap && mem_to_be_filtered1 (Erel, uid1) then 
		()
	      else begin
		excludes1 := 
		  get_excludes relabel_exclude_map1 relabel_link_map1 uid1;
		add_to_be_filtered1 Erel uid1 (trace_link relabel_link_map1 uid1)
	      end;
	      if no_swap && mem_to_be_filtered2 (Erel, uid2) then 
		()
	      else begin
		excludes2 :=
		  get_excludes relabel_exclude_map2 relabel_link_map2 uid2;
		add_to_be_filtered2 Erel uid2 (trace_link relabel_link_map2 uid2)
	      end;

	      prev_rel2_lm2 := (tree2#initial_leftmost nd2)#gindex;
	      prev_rel2 := gid2

	    end
	    else begin
	      let nd1 = Info.get_node info1 in
	      let nd2 = Info.get_node info2 in
	      excludes1 := List.map Info.make (Array.to_list nd1#initial_children);
	      excludes2 := List.map Info.make (Array.to_list nd2#initial_children);
	    end

	| Edit.Move(mid, _, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	    let nd2 = Info.get_node info2 in
	    let gid2 = nd2#gindex in

	    let no_swap = 
	      if !prev_mov2_lm2 = GI.dummy && !prev_mov2 = GI.dummy then
		false
	      else
		!prev_mov2_lm2 <= gid2 && gid2 < !prev_mov2
	    in

	    BEGIN_DEBUG
              let nd1 = Info.get_node info1 in
              let gid1 = nd1#gindex in
	      DEBUG_MSG "filter: move: %a-%a" GI.ps gid1 GI.ps gid2;
	      DEBUG_MSG "prev_mov2_lm2=%a prev_mov2=%a" GI.ps !prev_mov2_lm2 GI.ps !prev_mov2;
	      DEBUG_MSG "filter: no_swap=%B" no_swap;
	    END_DEBUG;

	    if no_swap && mem_to_be_filtered1 (Emov, uid1) then 
	      ()
	    else begin
	      excludes1 := get_excludes move_exclude_map1 move_link_map1 uid1;
	      let uids1 = trace_link move_link_map1 uid1 in
	      List.iter 
		(fun u -> (tree1#search_node_by_uid u)#data#set_mid !mid) uids1;

	      add_to_be_filtered1 Emov uid1 uids1

	    end;
	    if no_swap && mem_to_be_filtered2 (Emov, uid2) then 
	      ()
	    else begin
	      excludes2 := get_excludes move_exclude_map2 move_link_map2 uid2;
	      let uids2 = trace_link move_link_map2 uid2 in
	      List.iter 
		(fun u -> (tree2#search_node_by_uid u)#data#set_mid !mid) uids2;

	      add_to_be_filtered2 Emov uid2 uids2

	    end;
	    prev_mov2_lm2 := (tree2#initial_leftmost nd2)#gindex;
	    prev_mov2 := gid2
      );


    BEGIN_DEBUG
      DEBUG_MSG "EDITS BEFORE FILTERING:\n%s\n" edits#to_string;
      DEBUG_MSG ("to_be_filtered1: [%s]\n" ^^
		    "to_be_filtered2: [%s]")
	(Xlist.to_string 
	   (fun (tag, u) -> Printf.sprintf "%a(%s)" UID.ps u (edtag_to_string tag))
	   ";" (Xset.to_list to_be_filtered1))
	(Xlist.to_string 
	   (fun (tag, u) -> Printf.sprintf "%a(%s)" UID.ps u (edtag_to_string tag))
	   ";" (Xset.to_list to_be_filtered2));
    END_DEBUG;
    
    edits#filter
      (function 
	| Edit.Delete(_, uid, _, _) -> not (mem_to_be_filtered1 (Edel, uid))
	| Edit.Insert(_, uid, _, _) -> not (mem_to_be_filtered2 (Eins, uid))
	| Edit.Relabel(_, (uid1, _, _), (uid2, _, _)) ->
	    if options#group_relabels_flag then
	      not (mem_to_be_filtered1 (Erel, uid1)) ||
	      not (mem_to_be_filtered2 (Erel, uid2))
	    else true

	| Edit.Move(_, _, (uid1, _, _), (uid2, _, _)) ->
	    not (mem_to_be_filtered1 (Emov, uid1)) ||
	    not (mem_to_be_filtered2 (Emov, uid2))
      );

    
    DEBUG_MSG "EDITS AFTER FILTERING:\n%s\n" edits#to_string;


    let sort_excludes =
      List.fast_sort 
	(fun i1 i2 -> compare (Info.get_gindex i1) (Info.get_gindex i2))
    in

    let mids = Hashtbl.create 0 in

    edits#iter_topdown
      (function
	| Edit.Delete(_, uid, info, excludes) as del -> 

            DEBUG_MSG "%s" (Editop.to_string del);

	    let uids = List.map Info.get_uid !excludes in
	    let size = tree1#size_of_initial_cluster_u (uid, uids) in
	    Info.set_size info size;
	    excludes := sort_excludes !excludes;

	| Edit.Insert(_, uid, info, excludes) as ins -> 

            DEBUG_MSG "%s" (Editop.to_string ins);

	    let uids = List.map Info.get_uid !excludes in
	    let size = tree2#size_of_initial_cluster_u (uid, uids) in
	    Info.set_size info size;
	    excludes := sort_excludes !excludes;

	| Edit.Relabel(_, (uid1, info1, excludes1), (uid2, info2, excludes2)) as rel ->

            DEBUG_MSG "%s" (Editop.to_string rel);

	    let uids1 = List.map Info.get_uid !excludes1 in
	    let size1 = tree1#size_of_initial_cluster_u (uid1, uids1) in
	    let uids2 = List.map Info.get_uid !excludes2 in
	    let size2 = tree2#size_of_initial_cluster_u (uid2, uids2) in

	    assert (size1 = size2);

	    Info.set_size info1 size1;
	    Info.set_size info2 size2;
	    excludes1 := sort_excludes !excludes1;
	    excludes2 := sort_excludes !excludes2;

	| Edit.Move(mid, _, (uid1, info1, excludes1), (uid2, info2, excludes2)) as mov ->

            DEBUG_MSG "%s" (Editop.to_string mov);

	    let uids1 = List.map Info.get_uid !excludes1 in
	    let size1 = tree1#size_of_initial_cluster_u (uid1, uids1) in

	    let uids2 = List.map Info.get_uid !excludes2 in
	    let size2 = tree2#size_of_initial_cluster_u (uid2, uids2) in

            DEBUG_MSG "size1=%d size2=%d" size1 size2;
	    assert (size1 = size2);

	    Info.set_size info1 size1;
	    Info.set_size info2 size2;

	    if Hashtbl.mem mids !mid then
	      Hashtbl.replace mids !mid (size1 + (Hashtbl.find mids !mid))
	    else begin
	      Hashtbl.add mids !mid size1
	    end;

	    excludes1 := sort_excludes !excludes1;
	    excludes2 := sort_excludes !excludes2;
      );

    BEGIN_DEBUG
      let total = ref 0 in
      DEBUG_MSG "* size of moves:";
      let list =
	List.fast_sort (fun (m1, _) (m2, _) -> Stdlib.compare m1 m2)
	  (Hashtbl.fold (fun mid sz l -> (mid, sz)::l) mids [])
      in
      List.iter
	(fun (mid, sz) ->
          total := !total + sz;
	  DEBUG_MSG "%a --> %d node%s" MID.ps mid sz (if sz = 1 then "" else "s")
	) list;
      DEBUG_MSG "total=%d" !total;
    END_DEBUG
    (* end of func fixup_edits *)


      
end (* of module Postprocessing.F *)
