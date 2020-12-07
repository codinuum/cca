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
(* edit.ml *)



module B = Binding
module BID = B.ID

type node_t = Spec.node_t
type tree_t = Spec.tree_t

include Edit_base

class formatters 
    ~delete ~insert ~relabel ~move ~align 
    ~ignored1 ~ignored2 ~misparsed1 ~misparsed2
    = 
  object
    inherit formatters_base ~delete ~insert ~relabel ~move ~align 
    method delete  = (delete : int -> int -> segment list -> string)
    method insert  = (insert : int -> int -> segment list -> string)
    method relabel = (relabel : bool -> int -> int -> segment list -> int -> int -> segment list -> string)
    method move    = (move : int -> int -> segment list -> int -> int -> segment list -> string)
    method align   = (align : line_match -> string)
    method ignored1   = (ignored1 : segment -> string)
    method ignored2   = (ignored2 : segment -> string)
    method misparsed1 = (misparsed1 : segment -> string)
    method misparsed2 = (misparsed1 : segment -> string)
  end

class seq options = object (self)
  inherit [node_t, tree_t] seq_base options as super

  method dump_diff_simple_ch ?(line_align=[]) (tree1 : 'tree_t) (tree2 : 'tree_t) =
    let formatters = 
      new formatters
        ~delete:(fun st ed segs -> sprintf "DELETE %s\n" (segments_to_string segs))
        ~insert:(fun st ed segs -> sprintf "INSERT %s\n" (segments_to_string segs))
        ~relabel:
        (fun movrel st1 ed1 segs1 st2 ed2 segs2 ->
	  let marker =
	    if movrel then
	      "MOVREL"
	    else
	      "RELABEL"
	  in
          sprintf "%s %s >> %s\n" marker (segments_to_string segs1) (segments_to_string segs2)
        )
        ~move:
        (fun st1 ed1 segs1 st2 ed2 segs2 -> 
          sprintf "MOVE %s >> %s\n" (segments_to_string segs1) (segments_to_string segs2)
        )
        ~align:
        (function
          | LM_single (l1, l2) -> 
	      sprintf "ALIGN %d >> %d\n" l1 l2
          | LM_multi(l1, l2, ext) -> 
	      sprintf "ALIGN %d:%d >> %d:%d\n" l1 (l1+ext) l2 (l2+ext)
        )
        ~ignored1:(fun (s, e) -> sprintf "IGNORED1 %d %d\n" s e)
        ~ignored2:(fun (s, e) -> sprintf "IGNORED2 %d %d\n" s e)
        ~misparsed1:(fun (s, e) -> sprintf "MISPARSED1 %d %d\n" s e)
        ~misparsed2:(fun (s, e) -> sprintf "MISPARSED2 %d %d\n" s e)
    in
    self#_dump_diff_ch ~header:"" ~footer:"" ~formatters ~line_align tree1 tree2

  method dump_diff_json_ch ?(line_align=[]) (tree1 : 'tree_t) (tree2 : 'tree_t) =

    let segs_to_json idx ?(st=(-1)) ?(ed=(-1)) _segs = 
      let segs = List.filter (fun (s, e) -> s <= e) _segs in
      let seg_to_json (s, e) = sprintf "{\"start\":%d,\"end\":%d}" s e in
      let extra =
        if st >= 0 && ed >= 0 && st <= ed then
          sprintf "\"start%d\":%d,\"end%d\":%d" idx st idx ed
        else
          ""
      in
      let segs_str = 
        if segs = [] then
          ""
        else
          sprintf "\"segments%d\":[%s]" idx (Xlist.to_string seg_to_json "," segs)
      in
      String.concat "," (List.filter (fun x -> x <> "") [extra; segs_str])
(*
      match segs with
      | [] -> ""
      | seg::_ ->
          let last = ref seg in
          let seg_to_json ((s, e) as sg) = last := sg; sprintf "{\"start\":%d,\"end\":%d}" s e in
          let sstr = Xlist.to_string seg_to_json "," segs in
          sprintf "\"start%d\":%d,\"end%d\":%d,\"segments%d\":[%s]" 
            idx (fst seg)
            idx (snd !last)
            idx sstr
*)
    in
    let mktag s = sprintf "\"tag\":\"%s\"" s in
    let mksegs1 = segs_to_json 1 in
    let mksegs2 = segs_to_json 2 in
    let mkalign ?(ext=0) l1 l2 =
      sprintf "{%s,\"line1\":%d,\"line2\":%d%s}," 
        (mktag "ALIGN") l1 l2 (if ext = 0 then "" else sprintf ",\"ext\":%d" ext)
    in

    let mkfmt0 m idx seg =
      let seg_str = segs_to_json idx [seg] in
      if seg_str = "" then
        ""
      else
        sprintf "{%s,%s}," (mktag m) seg_str
    in
    let mkfmt m idx st ed segs =
      if st <= ed then
        let seg_str = segs_to_json idx ~st ~ed segs in
        if seg_str = "" then
          ""
        else
          sprintf "{%s,%s}," (mktag m) seg_str
      else
        ""
    in
    let mkfmt2 m st1 ed1 segs1 st2 ed2 segs2 =
      if st1 <= ed1 && st2 <= ed2 then
        let seg_str1 = mksegs1 ~st:st1 ~ed:ed1 segs1 in
        let seg_str2 = mksegs2 ~st:st2 ~ed:ed2 segs2 in
        if seg_str1 = "" || seg_str2 = "" then
          ""
        else
          sprintf "{%s,%s,%s}," (mktag m) seg_str1 seg_str2
      else
        ""
    in
    
    let formatters = 
      new formatters
        ~delete:(mkfmt "DELETE" 1)
        ~insert:(mkfmt "INSERT" 2)
        ~relabel:
        (fun movrel ->
	  let marker =
	    if movrel then
	      "MOVREL"
	    else
	      "RELABEL"
	  in
          mkfmt2 marker
        )
	~move:(mkfmt2 "MOVE")
        ~ignored1:(mkfmt0 "IGNORED1" 1)
        ~ignored2:(mkfmt0 "IGNORED2" 2)
        ~misparsed1:(mkfmt0 "MISPARSED1" 1)
        ~misparsed2:(mkfmt0 "MISPARSED2" 2)
        ~align:
        (function
          | LM_single(l1, l2) -> mkalign l1 l2
          | LM_multi(l1, l2, ext) -> mkalign ~ext l1 l2
        )
    in
    self#_dump_diff_ch ~header:"[" ~footer:"null]" ~formatters ~line_align tree1 tree2

  method private _dump_diff_ch
      ~header ~footer ~formatters
      ?(line_align=[])
      (tree1 : 'tree_t) (tree2 : 'tree_t)
      ch
      =
    let ignored1 = tree1#ignored_regions in
    let ignored2 = tree2#ignored_regions in

    DEBUG_MSG "* DUMPING DIFF DATA (%d edit(s))\n" self#get_nedits;

    let mkpath tree =
      if Storage.kind_is_fs tree#source_kind then
        tree#source_fullpath
      else
        tree#source_path
    in

    let iginfos1 = List.map (Info.of_region ~fname:(mkpath tree1)) ignored1 in
    let iginfos2 = List.map (Info.of_region ~fname:(mkpath tree2)) ignored2 in

    BEGIN_DEBUG
      DEBUG_MSG "ignored regions1: %s" (segments_to_string ignored1);
      DEBUG_MSG "ignored regions2: %s" (segments_to_string ignored2)
    END_DEBUG;

    let filter iginfos info infos =
      List.filter 
	(fun inf -> 
	  Info.is_included inf info && 
	  List.for_all (fun i -> not (Info.is_included inf i)) infos
	) iginfos
    in
  
    let get_segments iginfos info excludes =

      BEGIN_DEBUG
	DEBUG_MSG "info=%s" (Info.to_string info);
	DEBUG_MSG "excludes:";
	List.iter (fun i -> DEBUG_MSG "%s" (Info.to_string i)) !excludes
      END_DEBUG;

      let excludes' = (filter iginfos info !excludes) @ !excludes in
      let segs = Info.segment (info, (Info.sort_infos excludes')) in

      DEBUG_MSG "result=%s" (segments_to_string segs);

      sort_segments segs
    in
  
    let get_mov_segs_pair = function
      | Move(mid, _, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	  let segs1 = get_segments iginfos1 info1 excludes1 in
	  let segs2 = get_segments iginfos2 info2 excludes2 in
	  segs1, segs2
      | _ -> assert false
    in
    
    Printf.fprintf ch "%s" header;

    let segment_count = ref 0 in

    self#iter_topdown
      (fun ed ->

	DEBUG_MSG "scanning %s" (to_string ed);

	try
	  match ed with
	  | Delete(_, uid, info, excludes) ->
              let loc = Info.get_loc info in
              let st, ed = loc.Loc.start_offset, loc.Loc.end_offset in
	      let segs = get_segments iginfos1 info excludes in
              output_string ch (formatters#delete st ed segs)

	  | Insert(_, uid, info, excludes) ->
              let loc = Info.get_loc info in
              let st, ed = loc.Loc.start_offset, loc.Loc.end_offset in
	      let segs = get_segments iginfos2 info excludes in
	      output_string ch (formatters#insert st ed segs)
		
	  | Relabel(movrel, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
              let loc1 = Info.get_loc info1 in
              let loc2 = Info.get_loc info2 in
              let st1, ed1 = loc1.Loc.start_offset, loc1.Loc.end_offset in
              let st2, ed2 = loc2.Loc.start_offset, loc2.Loc.end_offset in
	      let segs1 = get_segments iginfos1 info1 excludes1 in
	      let segs2 = get_segments iginfos2 info2 excludes2 in
	      output_string ch (formatters#relabel !movrel st1 ed1 segs1 st2 ed2 segs2)

	  | Move(mid, _, (_, info1, _), (_, info2, _)) as mov ->
	      let skip =
		try
		  let movs = self#find_mov_gr_mems (self#find_mov_gr !mid) in
		  (Info.get_node info1)#gindex <
		  (Xlist.max
		     (List.map 
			(function 
			  | Move(_, _, (_, i1, _), _) ->
			      (Info.get_node i1)#gindex
			  | _ -> assert false
			) movs))
		with 
		  Not_found -> false
	      in

	      if not skip then begin
                let loc1 = Info.get_loc info1 in
                let loc2 = Info.get_loc info2 in
                let st1, ed1 = loc1.Loc.start_offset, loc1.Loc.end_offset in
                let st2, ed2 = loc2.Loc.start_offset, loc2.Loc.end_offset in

		let segs1, segs2 =
		  try
		    let movs = self#find_mov_gr_mems (self#find_mov_gr !mid) in
		    
		    DEBUG_MSG "* move group %a:\n\t%s" MID.ps !mid
		      (Xlist.to_string to_string "\n\t" movs);

		    let segs_list1, segs_list2 = List.split (List.map get_mov_segs_pair movs) in
		    merge_segments segs_list1, merge_segments segs_list2
		  with 
		    Not_found -> get_mov_segs_pair mov
		in
                output_string ch (formatters#move st1 ed1 segs1 st2 ed2 segs2)
	      end
	      else
		DEBUG_MSG " -> skipped"

	with 
	  Info.Segment -> 
	    incr segment_count; 
	    DEBUG_MSG "Info.Segment exception raised!"
      );
    List.iter (fun seg -> output_string ch (formatters#ignored1 seg)) ignored1;
    List.iter (fun seg -> output_string ch (formatters#ignored2 seg)) ignored2;
    List.iter (fun seg -> output_string ch (formatters#misparsed1 seg)) tree1#misparsed_regions;
    List.iter (fun seg -> output_string ch (formatters#misparsed2 seg)) tree2#misparsed_regions;

    BEGIN_DEBUG
      DEBUG_MSG "delete:%d insert:%d relabel:%d move:%d (total:%d)\n" 
	self#get_ndeletes self#get_ninserts self#get_nrelabels self#get_nmoves self#get_nedits;
      DEBUG_MSG "%d Segment exception(s) raised" !segment_count
    END_DEBUG;

    if line_align <> [] then
      self#dump_line_align_ch (formatters : formatters :> formatters_base) line_align ch;

    Printf.fprintf ch "%s" footer

  (* end of method dump_diff_ch *)

  method dump_delta
      ?(extra_ns_decls=[])
      (tree1 : 'tree_t)
      (tree2 : 'tree_t)
      (uidmapping : node_t UIDmapping.c)
      edits_copy
      fname
      =
    let comp = options#delta_compression in
    let irreversible_flag = options#irreversible_flag in
    let dedits =
      new Delta.Edit.seq options ~irreversible_flag tree1 tree2 uidmapping edits_copy self
    in
    dedits#dump_delta
      ~extra_ns_decls
      ~comp
      uidmapping fname

end (* of class seq *)


let dump_changes options lang tree1 tree2 uidmapping edits_copy edits file =
  DEBUG_MSG "dumping changes...";

  let extract = lang#extract_change in
  
  Xprint.verbose options#verbose_flag "extracting changes...";

  let changes, unused, change_infos, triples =
    extract options tree1 tree2 uidmapping edits_copy 
  in

  Xprint.verbose options#verbose_flag "done.";

  if changes <> [] || unused <> [] then begin

    let dumper ch = 
      Xprint.verbose options#verbose_flag "dumping...";

      let sorted = 
	List.fast_sort 
	  (fun (_, s1, _) (_, s2, _) -> Stdlib.compare s2 s1) 
	  changes 
      in
      if changes <> [] then begin
	Printf.fprintf ch "*** Extracted Changes ***\n";
	List.iter
	  (fun (chg_ty, lv, mess) ->
	    let n = List.length mess in
	    Printf.fprintf ch "\n[significance=%d] %s (%d)\n" lv chg_ty n;
	    List.iter
	      (fun mes -> 
		Printf.fprintf ch "  %s\n" mes
	      ) mess
	  ) sorted
      end;

      if unused <> [] then begin
	Printf.fprintf ch "\n%d edit operations are not classified:\n" (List.length unused);
	List.iter
	  (fun ed ->
	    Printf.fprintf ch "%s\n" (to_string ed)
	  ) unused
      end;
      Xprint.verbose options#verbose_flag "done.";
    in (* dumper *)
(*
    let csv_dumper ch =
      Xprint.verbose options#verbose_flag "dumping csv...";

      let cmp (_, _, _, _, _, loc1, _, _) (_, _, _, _, _, loc2, _, _) =
	if loc1 <> Loc.dummy && loc2 <> Loc.dummy then
	  Stdlib.compare loc1.Loc.start_offset loc2.Loc.start_offset
	else begin
	  WARN_MSG "invalid location";
	  0
	end
      in

      if change_infos <> [] then begin
	let _csv = ref [] in
	List.iter
	  (fun (chg_ty, lv, infos) ->
	    List.iter
	      (fun (desc, adesc, unit1, loc1, unit2, loc2) -> 
		_csv := ( chg_ty,
			  string_of_int lv,
			  desc,
			  adesc,
			  unit1,
			  loc1,
			  unit2,
			  loc2
			)::!_csv
	      ) infos
	  ) change_infos;

	let sorted = 
	  List.map 
	    (fun (ct, lv, d, ad, u1, l1, u2, l2) -> 
	      [ct; lv; d; ad; u1; Loc.to_string l1; u2; Loc.to_string l2]
	    ) (List.stable_sort cmp !_csv) 
	in

	let filtered =
	  if options#multiple_classification_flag then
	    sorted
	  else begin
	    let tbl = Hashtbl.create 0 in
	    List.fold_left
	      (fun l -> 
		function 
		  | [ct; lv; d; ad; u1; l1; u2; l2] ->
		      if Hashtbl.mem tbl (l1, l2) then begin
			DEBUG_MSG "filtered out: [%s] (%s)-(%s)" ct l1 l2;
			l
		      end
		      else begin
			Hashtbl.replace tbl (l1, l2) true;
			[ct; lv; String.escaped d; ad; u1; l1; u2; l2]::l
		      end
		  | _ -> assert false

	      ) [] (List.rev sorted)
	  end
	in
	Csv.save_out_readable ch filtered;

	Xprint.verbose options#verbose_flag "done.";
      end
    in (* csv_dumper *)
*)
    Xfile.dump file dumper;
    (*Xfile.dump (file^".csv") csv_dumper;*)

    if options#fact_for_changes_flag && not (Xset.is_empty triples) then begin
      Xprint.verbose options#verbose_flag "dumping change fact...";
      let into_virtuoso = options#fact_into_virtuoso <> "" in
      let into_directory = options#fact_into_directory <> "" in

      if into_virtuoso then begin
        assert (not into_directory);
        Triple.dump_into_virtuoso options triples
      end
      else if into_directory then
        let cache_name = Cache.get_cache_name options (Filename.dirname file) in
        Triple.dump_into_directory options cache_name triples
      else
        Triple.dump options ~overwrite:false ~comp:options#fact_compression (file^".nt") triples;

      Xprint.verbose options#verbose_flag "done."
    end

  end (* of if changes <> [] || unused <> [] *)

  (* end of func dump_changes *)		


(* * * * *)

let remove_relabels_and_mapping 
    cenv
    tree1 
    tree2 
    edits
    uidmapping
    to_be_removed
    =
  List.iter (* remove incompatible relabels and mapping *)
    (fun (nd1, nd2, by_non_renames) -> begin
      let nodes1 = ref [] in
      let nodes2 = ref [] in
      tree1#scan_whole_initial_subtree nd1 (fun n -> nodes1 := n::!nodes1);
      tree2#scan_whole_initial_subtree nd2 (fun n -> nodes2 := n::!nodes2);
      List.iter
	(fun n ->
	  try
	    let u = n#uid in
	    let u' = uidmapping#find n#uid in
	    let n' = tree2#search_node_by_uid u' in
	    if List.memq n' !nodes2 then begin

	      if by_non_renames then begin
                DEBUG_MSG "by_non_renames=true: u=%a u'=%a" UID.ps u UID.ps u';
		cenv#add_bad_pair u u'
              end;

	      List.iter 
		(fun ed ->
		  DEBUG_MSG "removing %s" (Editop.to_string ed);
		  edits#remove_edit ed
		) (edits#find12 u u');

	      uidmapping#remove u u';

	      let del = Editop.make_delete n in (* generate delete *)
	      DEBUG_MSG "adding %s" (Editop.to_string del);
	      
	      edits#add_edit del;

	      let ins = Editop.make_insert n' in (* generate insert *)
	      DEBUG_MSG "adding %s" (Editop.to_string ins);
	      
	      edits#add_edit ins

	    end
	  with 
	    Not_found -> ()
	) !nodes1
    end
    ) to_be_removed
(* end of func remove_relabels_and_mapping *)


let match_nodes 
    cenv
    tree1
    tree2
    (tbl : ((BID.t * BID.t), (node_t list * node_t list)) Hashtbl.t)
    =
  let compatible_pairs = ref [] in
  Hashtbl.iter 
    (fun (bid1, bid2) (cands1, cands2) -> 
      match cands1, cands2 with
      | [], _ | _, [] -> ()
      | [nd1], [nd2] -> compatible_pairs := (nd1, nd2) :: !compatible_pairs
      | nds1, nds2 ->
	  let pair_weight_list = ref [] in
	  List.iter
	    (fun nd1 ->
	      List.iter
		(fun nd2 ->
		  let w = 
		    Stdlib.truncate ((cenv#get_adjacency_score nd1 nd2) *. 10000.0) 
		  in
		  pair_weight_list := (nd1, nd2, w) :: !pair_weight_list
		) nds2
	    ) nds1;

	  BEGIN_DEBUG
	    DEBUG_MSG "pair_weight_list:";
            List.iter 
	      (fun (n1, n2, w) -> 
		DEBUG_MSG " %a(%a)-%a(%a): %d" 
		  UID.ps n1#uid GI.ps n1#gindex UID.ps n2#uid GI.ps n2#gindex w
	      ) !pair_weight_list
	  END_DEBUG;

	  let pairs, _ =
	    UIDmapping.select_p_pairs (fun _ _ _ _ _ _ -> true) tree1 tree2 !pair_weight_list
	  in
	  compatible_pairs := (List.map (fun (n1, n2, _) -> n1, n2) pairs) @ !compatible_pairs
    ) tbl;
  !compatible_pairs
(* end of func match_nodes *)


let lock_mapping tree1 tree2 uidmapping nd1 nd2 =
  let nodes1 = ref [] in
  let nodes2 = ref [] in
  tree1#scan_whole_initial_subtree nd1 (fun n -> nodes1 := n::!nodes1);
  tree2#scan_whole_initial_subtree nd2 (fun n -> nodes2 := n::!nodes2);
  List.iter
    (fun n ->
      let u = n#uid in
      try
	let u' = uidmapping#find u in
	if List.memq (tree2#search_node_by_uid u') !nodes2 then begin
	  let key = Some (Key.make_pair_key nd1 nd2) in
	  uidmapping#lock_uid ?key:key u;
	  uidmapping#lock_uid ?key:key u';
	end
      with 
	Not_found -> ()
    ) !nodes1
(* end of func lock_mapping *)


let generate_compatible_edits 
    options
    cenv
    (tree1 : Spec.tree_t)
    (tree2 : Spec.tree_t)
    uidmapping
    edits
    compatible_pairs 
    is_incompatible
    =
  List.iter
    (fun (nd1, nd2) -> (* generate compatible edits *)

      if nd1#is_valid && nd2#is_valid then begin

      DEBUG_MSG "compatible pair: %s - %s" nd1#data#to_string nd2#data#to_string;

      let subtree1 = tree1#make_anonymized_subtree_copy nd1 in
      let subtree2 = tree2#make_anonymized_subtree_copy nd2 in
      let subcenv = new Comparison.c options subtree1 subtree2 in
      let m, em, r = 
	Treediff.match_trees cenv subtree1 subtree2 (new UIDmapping.c subcenv) (new UIDmapping.c subcenv) 
      in
      let matches = 
	(Misc.conv_subtree_node_pairs tree1 tree2) (m @ em @ r) 
      in

      BEGIN_DEBUG
	DEBUG_MSG "matches:";
	List.iter
	  (fun (n1, n2) -> DEBUG_MSG "%a-%a" UID.ps n1#uid UID.ps n2#uid)
	  matches;
	DEBUG_MSG "matches (gindex):";
	List.iter
	  (fun (n1, n2) -> DEBUG_MSG "%a-%a" GI.ps n1#gindex GI.ps n2#gindex)
	  matches;
      END_DEBUG;

      List.iter
	(fun (n1, n2) ->
          DEBUG_MSG "%a-%a" UID.ps n1#uid UID.ps n2#uid;
	  let incompat, by_non_renames = is_incompatible n1 n2 in
	  if incompat then
	    DEBUG_MSG "incompatible"
	  else
	    let u1, u2 = n1#uid, n2#uid in

            (* remove conflicting edits *)
	    begin 
	      let eds1 = edits#find1 u1 in
	      let conflict =
		match eds1 with
		| [] -> false
		| [Delete _] -> true
		| [Relabel(_, _, (u2', _, _))] -> u2' <> u2
		| [Relabel(_, _, (u2', _, _));Move(_, _, _, (u2'', _, _))]
		| [Move(_, _, _, (u2'', _, _));Relabel(_, _, (u2', _, _))] ->
		    assert (u2' = u2'');
		    let b = u2' <> u2 in
		    if b then begin
		      let n2' = tree2#search_node_by_uid u2' in
		      let ins = Editop.make_insert n2' in

		      DEBUG_MSG "adding %s" (Editop.to_string ins);
		      
		      edits#add_edit ins
		    end;
		    b
		| _ -> assert false
	      in
	      if conflict then
		List.iter 
		  (fun e -> 
		    DEBUG_MSG "removing %s" (Editop.to_string e);
		    edits#remove_edit e
		  ) eds1
	    end;
	    begin 
	      let eds2 = edits#find2 u2 in
	      let conflict =
		match eds2 with
		| [] -> false
		| [Insert _] -> true
		| [Relabel(_, (u1', _, _), _)] -> u1' <> u1
		| [Relabel(_, (u1', _, _), _);Move(_, _, (u1'', _, _), _)]
		| [Move(_, _, (u1'', _, _), _);Relabel(_, (u1', _, _), _)] ->
		    assert (u1' = u1'');
		    let b = u1' <> u1 in
		    if b then begin
		      let n1' = tree1#search_node_by_uid u1' in
		      let del = Editop.make_delete n1' in
		      DEBUG_MSG "adding %s" (Editop.to_string del);
		      edits#add_edit del
		    end;
		    b
		| _ -> assert false
	      in
	      if conflict then
		List.iter 
		  (fun e -> 
		    DEBUG_MSG "removing %s" (Editop.to_string e);
		    edits#remove_edit e
		  ) eds2
	    end;

	    (* add new edit *)
	    let eds = edits#find12 u1 u2 in
	    if eds = [] then begin
	      if not (n1#data#eq n2#data) then begin
		let rel = Editop.make_relabel n1 n2 in
		DEBUG_MSG "adding %s" (Editop.to_string rel);
		edits#add_edit rel
	      end
	    end
	    else begin
	      BEGIN_DEBUG
		List.iter
		(fun ed ->
		  DEBUG_MSG "found %s" (Editop.to_string ed)
		) eds;
	      END_DEBUG
	    end;

	    (* add new mapping (override) *)
	    if not (uidmapping#has_mapping u1 u2) then begin
              DEBUG_MSG "adding %a -> %a" UID.ps u1 UID.ps u2;
	      let conflict = uidmapping#add_unsettled u1 u2 in
              match conflict with
              | Some u1, None -> begin
                  let del = Editop.make_delete (tree1#search_node_by_uid u1) in
		  DEBUG_MSG "adding %s" (Editop.to_string del);
		  edits#add_edit del
              end
              | None, Some u2 -> begin
                  let ins = Editop.make_insert (tree2#search_node_by_uid u2) in
		  DEBUG_MSG "adding %s" (Editop.to_string ins);
		  edits#add_edit ins
              end
              | Some u1, Some u2 -> begin
                  let del = Editop.make_delete (tree1#search_node_by_uid u1) in
		  DEBUG_MSG "adding %s" (Editop.to_string del);
		  edits#add_edit del;
                  let ins = Editop.make_insert (tree2#search_node_by_uid u2) in
		  DEBUG_MSG "adding %s" (Editop.to_string ins);
		  edits#add_edit ins
              end
              | _ -> ()
            end;

	    let key = Some (Key.make_pair_key nd1 nd2) in
	    uidmapping#lock_uid ?key:key u1;
	    uidmapping#lock_uid ?key:key u2

	) matches
      end

    ) compatible_pairs
(* end of func generate_compatible_edits *)


let mkfilt getlab is_x nd =
  try
    is_x (getlab nd)
  with
    Not_found -> false


let is_def nd = B.is_def nd#data#binding
let is_use nd = B.is_use nd#data#binding
let get_bid nd = B.get_bid nd#data#binding
let get_bid_opt nd = B.get_bid_opt nd#data#binding


let collect_use_renames ?(filt=fun _ _ -> true) edits is_possible_rename =

  let freq_tbl = Hashtbl.create 0 in

  let _use_rename_tbl1 = Hashtbl.create 0 in
  let _use_rename_tbl2 = Hashtbl.create 0 in

  let add_use_rename node1 node2 bid1 bid2 =
    let name1 = node1#data#get_name in
    let name2 = node2#data#get_name in
    DEBUG_MSG "adding %a -> %a (\"%s\" -> \"%s\")" BID.ps bid1 BID.ps bid2 name1 name2;
    let add tbl bi1 bi2 =
      try
	let bs = Hashtbl.find tbl bi1 in
	if not (List.mem bi2 bs) then
	  Hashtbl.replace tbl bi1 (bi2::bs)
      with
	Not_found -> Hashtbl.add tbl bi1 [bi2]
    in
    if is_possible_rename node1 node2 bid1 bid2 then begin 
      DEBUG_MSG "added";
      add _use_rename_tbl1 bid1 bid2;
      add _use_rename_tbl2 bid2 bid1;
      try
	let freq, nm1, nm2 = Hashtbl.find freq_tbl (bid1, bid2) in
        assert (nm1 = name1 && nm2 = name2);
	Hashtbl.replace freq_tbl (bid1, bid2) (freq + 1, name1, name2)
      with 
	Not_found -> Hashtbl.add freq_tbl (bid1, bid2) (1, name1, name2)
    end
    else begin
      DEBUG_MSG "not added";
    end
  in

  edits#iter_relabels
    (function
      | Relabel(_, (u1, info1, ex1), (u2, info2, ex2)) as rel -> begin

	  DEBUG_MSG "checking %s" (Editop.to_string rel);

	  let nd1 = Info.get_node info1 in
	  let nd2 = Info.get_node info2 in
	  if filt nd1 nd2 && is_use nd1 && is_use nd2 then
	    try
	      add_use_rename nd1 nd2 (get_bid nd1) (get_bid nd2)
	    with
	      Not_found -> assert false
      end
      | _ -> assert false
    );

  BEGIN_DEBUG
    DEBUG_MSG "* use rename freq.:";
    Hashtbl.iter
      (fun (bi1, bi2) (freq, nm1, nm2) ->
	DEBUG_MSG " (%a, %a) -> %3d (\"%s\" -> \"%s\")" BID.ps bi1 BID.ps bi2 freq nm1 nm2
      ) freq_tbl
  END_DEBUG;

  freq_tbl, _use_rename_tbl1, _use_rename_tbl2


(* adjust_renames assumes that edit seq. contains correct renames of USEs *)
let adjust_renames
    ?(handle_weak=true)
    options
    cenv
    uidmapping
    edits 
    (filters : (node_t -> bool) array)
    =
  DEBUG_MSG "START!";

  let tree1 = (cenv#tree1 : Spec.tree_t) in
  let tree2 = (cenv#tree2 : Spec.tree_t) in

  (* collect def/use mapping (not relabeled) *)
  let non_rename_bid_tbl1 = Hashtbl.create 0 in (* bid -> bool * bool *)
  let non_rename_bid_tbl2 = Hashtbl.create 0 in
  let set_tbl setter tbl bid =
    try
      let d, u = Hashtbl.find tbl bid in
      Hashtbl.replace tbl bid (setter (Some (d, u)))
    with
      Not_found -> Hashtbl.add tbl bid (setter None)
  in
  let set_tbl_def = set_tbl (function Some (d, u) -> true, u | None -> true, false) in
  let set_tbl_use = set_tbl (function Some (d, u) -> d, true | None -> false, true) in

  uidmapping#iter
    (fun u1 u2 ->
      DEBUG_MSG "non_rename: checking %a-%a" UID.ps u1 UID.ps u2;
      let n1 = tree1#search_node_by_uid u1 in
      let n2 = tree2#search_node_by_uid u2 in
      let context_cond =
        try uidmapping#find n1#initial_parent#uid = n2#initial_parent#uid with _ -> false
      in
      DEBUG_MSG "context_cond=%B" context_cond;
      if context_cond then
      try
	let bi1 = get_bid n1 in
	let bi2 = get_bid n2 in
	if n1#data#eq n2#data then begin

	  if is_def n1 && is_def n2 then begin
	    set_tbl_def non_rename_bid_tbl1 bi1;
	    set_tbl_def non_rename_bid_tbl2 bi2;

	    DEBUG_MSG "non_rename (def): %a-%a" BID.ps bi1 BID.ps bi2;
	  end
	  else if is_use n1 && is_use n2 then begin
	    set_tbl_use non_rename_bid_tbl1 bi1;
	    set_tbl_use non_rename_bid_tbl2 bi2;

	    DEBUG_MSG "non_rename (use): %a-%a" BID.ps bi1 BID.ps bi2;
	  end

	end
      with
	Not_found -> ()
    );

  let non_rename tbl bi =
    try
      match Hashtbl.find tbl bi with
      | true, true -> true
      | _ -> false
    with
      Not_found -> false
  in

  (* non-rename can be rename e.g. fortran: variable-name -> array-element *)
  let is_possible_rename node1 node2 bi1 bi2 = 
    
    let parent_cond, context_cond =
      try
        let pnd1 = node1#initial_parent in
        let pnd2 = node2#initial_parent in
        let c_cond =
          try uidmapping#find pnd1#uid = pnd2#uid with _ -> false
        in
        let p_cond =
          let pbi1_opt = get_bid_opt pnd1 in
          let pbi2_opt = get_bid_opt pnd2 in
          match pbi1_opt, pbi2_opt with
          | Some pbi1, Some pbi2 -> bi1 = pbi1 && bi2 = pbi2
          | Some _, None | None, Some _ -> false
          | None, None -> true
        in
        p_cond, c_cond
      with
        Otreediff.Otree.Parent_not_found _ -> true, true
    in
    DEBUG_MSG "%a-%a (%a-%a): parent_cond=%B context_cond=%B"
      UID.ps node1#uid UID.ps node2#uid BID.ps bi1 BID.ps bi2 parent_cond context_cond;

    if parent_cond then
      let same_name =
        try
          node1#data#get_name = node2#data#get_name && 
          node1#data#get_category <> node2#data#get_category
        with
          _ -> false
      in
      let b =
        let has_conflict =
          context_cond &&
          (non_rename non_rename_bid_tbl1 bi1 || non_rename non_rename_bid_tbl2 bi2)
        in
        not has_conflict || same_name
      in
      if not b then
        DEBUG_MSG "%a-%a: conflicts with exactly matched pair" BID.ps bi1 BID.ps bi2;
      b
    else
      false
  in

  (* collect use renames *)
  let freq_tbl, _use_rename_tbl1, _use_rename_tbl2 =
    let filt n1 n2 =
      try
        uidmapping#find n1#initial_parent#uid = n2#initial_parent#uid
      with
        _-> false
    in
    collect_use_renames ~filt edits is_possible_rename
  in
  let get_freq bi1 bi2 =
    let freq, _, _ = Hashtbl.find freq_tbl (bi1, bi2) in
    freq
  in

  (* select use renames *)
  let selected_renames1 = ref [] in
  let selected_renames2 = ref [] in

  let weak_selected_renames1 = ref [] in
  let weak_selected_renames2 = ref [] in

  let conflicting_bids1 = ref [] in
  let conflicting_bids2 = ref [] in

  let loser_tbl = Hashtbl.create 0 in

  let sel_freq max bi1 bi2 =
    try
      let freq = get_freq bi1 bi2 in
      if freq > max then
	freq
      else
	max
    with
      Not_found -> assert false
  in
  
  Hashtbl.iter
    (fun bi1 bs ->
      DEBUG_MSG "* selecting from: %a -> [%s]" BID.ps bi1 (Xlist.to_string BID.to_string ";" bs);
      let max_freq = List.fold_left (fun max bi2 -> sel_freq max bi1 bi2) 0 bs in
      DEBUG_MSG "  max freq.: %d" max_freq;

      let selected = 
	List.filter (fun bi2 -> max_freq = get_freq bi1 bi2) bs 
      in
      DEBUG_MSG "  selected: %a -> [%s]" BID.ps bi1 (Xlist.to_string BID.to_string ";" selected);

      match selected with
      | []   -> assert false
      | [bi2] -> 
          if max_freq > 1 || handle_weak then
	    selected_renames1 := (bi1, bi2) :: !selected_renames1;
	  if max_freq = 1 && handle_weak then
	    weak_selected_renames1 := (bi1, bi2) :: !weak_selected_renames1;
	  List.iter
	    (fun bi ->
	      if bi != bi2 then
		Hashtbl.add loser_tbl (bi1, bi) true
	    ) bs
      | _    -> 
	  conflicting_bids2 := bs @ !conflicting_bids2

    ) _use_rename_tbl1;

  Hashtbl.iter
    (fun bi2 bs ->
      DEBUG_MSG "* selecting from: [%s] -> %a" (Xlist.to_string BID.to_string ";" bs) BID.ps bi2;
      let max_freq = List.fold_left (fun max bi1 -> sel_freq max bi1 bi2) 0 bs in
      DEBUG_MSG "  max freq.: %d" max_freq;

      let selected = 
	List.filter (fun bi1 -> max_freq = get_freq bi1 bi2) bs 
      in
      DEBUG_MSG "  selected: [%s] -> %a" (Xlist.to_string BID.to_string ";" selected) BID.ps bi2;

      match selected with
      | []   -> assert false
      | [bi1] -> 
          if max_freq > 1 || handle_weak then
	    selected_renames2 := (bi1, bi2) :: !selected_renames2;
	  if max_freq = 1 && handle_weak then
	    weak_selected_renames2 := (bi1, bi2) :: !weak_selected_renames2;
	  List.iter
	    (fun bi ->
	      if bi != bi1 then
		Hashtbl.add loser_tbl (bi, bi2) true
	    ) bs
      | _    ->
	  conflicting_bids1 := bs @ !conflicting_bids1

    ) _use_rename_tbl2;

  DEBUG_MSG "  conflicting_bids1: %s" (Xlist.to_string BID.to_string ";" !conflicting_bids1);
  DEBUG_MSG "  conflicting_bids2: %s" (Xlist.to_string BID.to_string ";" !conflicting_bids2);

  let selected_renames =
    List.filter
      (fun (bi1, bi2) ->
	(not (Hashtbl.mem loser_tbl (bi1, bi2))) && 
	(not (List.mem bi1 !conflicting_bids1)) && 
	(not (List.mem bi2 !conflicting_bids2))
      ) (Xlist.union !selected_renames1 !selected_renames2)
  in

  let weak_selected_renames = 
    Xlist.intersection selected_renames 
      (Xlist.union !weak_selected_renames1 !weak_selected_renames2)
  in
  let weak_selected_renames_from, weak_selected_renames_to =
    List.split weak_selected_renames
  in

  BEGIN_DEBUG
    DEBUG_MSG "* selected use renames:";
    List.iter
      (fun (bi1, bi2) -> 
	DEBUG_MSG " %a -> %a" BID.ps bi1 BID.ps bi2
      ) selected_renames;
    DEBUG_MSG "* weak selected use renames:";
    List.iter
      (fun (bi1, bi2) -> 
	DEBUG_MSG " %a -> %a" BID.ps bi1 BID.ps bi2
      ) weak_selected_renames
  END_DEBUG;

  let rename_tbl1 = Hashtbl.create 0 in
  let rename_tbl2 = Hashtbl.create 0 in
  List.iter
    (fun (bi1, bi2) ->
      Hashtbl.add rename_tbl1 bi1 bi2;
      Hashtbl.add rename_tbl2 bi2 bi1;
    ) selected_renames;

  let is_good_relabel nd1 nd2 =
    try
      let chk n1 n2 =
        try
	  let u1' = uidmapping#find n1#uid in
	  if u1' = n2#uid then
	    n1#data#relabel_allowed n2#data
	  else 
	    false
        with 
	  Not_found -> false
      in
      let pnd1 = nd1#initial_parent in
      let pnd2 = nd2#initial_parent in

      let parent_cond = chk pnd1 pnd2 in

      let chka a1 a2 =
        let l1 = Array.to_list a1 in
        let l2 = Array.to_list a2 in
        List.for_all2 chk l1 l2
      in
      let children_cond =
        let ca1 = nd1#initial_children in
        let ca2 = nd2#initial_children in
        let sz1 = Array.length ca1 in
        let sz2 = Array.length ca2 in
        if sz1 = sz2 then
          if sz1 = 0 then
            let a1 = pnd1#initial_children in
            let a2 = pnd2#initial_children in
            if Array.length a1 = Array.length a2 then
              chka a1 a2
            else if sz1 = 1 || sz2 = 1 then
              true
            else
              (*false*)
              try
                Array.iter
                  (fun s1 ->
                    if
                      s1 != nd1 &&
                      Array.exists (fun s2 -> s2 != nd2 && chk s1 s2) a2
                    then
                      raise Exit
                  ) a1;
                false
              with
                Exit -> true
          else
            chka ca1 ca2
        else
          false
      in
      nd1#data#relabel_allowed nd2#data && parent_cond && children_cond
    with
      Otreediff.Otree.Parent_not_found _ -> false
  in

  let is_incompatible nd1 nd2 =
    let context_cond =
      try uidmapping#find nd1#initial_parent#uid = nd2#initial_parent#uid with _ -> false
    in
    DEBUG_MSG "context_cond=%B" context_cond;
    let same_name =
      try
        nd1#data#get_name = nd2#data#get_name && nd1#data#get_category <> nd2#data#get_category
      with
        _ -> false
    in
    let bi1_opt, non_rename1, bi1'_opt =
      try
	let bi1 = get_bid nd1 in
	let non_rename1 = non_rename non_rename_bid_tbl1 bi1 in
	try
	  let bi1' = Hashtbl.find rename_tbl1 bi1 in
	  Some bi1, non_rename1, Some bi1'
	with
	  Not_found -> Some bi1, non_rename1, None
      with
	Not_found -> None, false, None
    in
    let bi2_opt, non_rename2, bi2'_opt =
      try
	let bi2 = get_bid nd2 in
	let non_rename2 = non_rename non_rename_bid_tbl2 bi2 in
	try
	  let bi2' = Hashtbl.find rename_tbl2 bi2 in
	  Some bi2, non_rename2, Some bi2'
	with
	  Not_found -> Some bi2, non_rename2, None
      with
	Not_found -> None, false, None
    in
    let b, by_non_renames = 
      match bi1_opt, bi2_opt with
      | Some bi1, Some bi2 -> begin
	  if (non_rename1 || non_rename2) && not same_name then begin
	    context_cond, true
	  end
	  else
            context_cond &&
	    (
	     not (List.mem bi1 weak_selected_renames_from) &&
	     not (List.mem bi2 weak_selected_renames_to)
	    ) &&
	    (
	      (match bi1'_opt with Some bi1' -> bi2 <> bi1' | None -> false) ||
	      (match bi2'_opt with Some bi2' -> bi1 <> bi2' | None -> false)
            ), false
      end
      | Some bi1, None ->
	  context_cond && (non_rename1 || (match bi1'_opt with Some _ -> true | None -> false)), non_rename1

      | None, Some bi2 ->
	  context_cond && (non_rename2 || (match bi2'_opt with Some _ -> true | None -> false)), non_rename2

      | None, None -> 
	  false, false
    in
    b, by_non_renames
  in (* is_incompatible *)

(*
  let is_incompatible_def nd1 nd2 =
    let f tbl nd =
      (B.is_used_def nd#data#binding) && 
      (try
	let bid = get_bid nd in
	not (Hashtbl.mem tbl bid)
      with
	Not_found -> false)
    in
    (f rename_tbl1 nd1) || (f rename_tbl2 nd2)
  in (* is_incompatible_def *)
*)

  DEBUG_MSG "* finding incompatible relabels...";

  let to_be_removed = ref [] in

  edits#iter_relabels (* find incompatible relabels *)
    (function
      | Relabel(_, (uid1, info1, _), (uid2, info2, _)) as rel -> begin
	  DEBUG_MSG "finding incompatible relabels: checking %a-%a" UID.ps uid1 UID.ps uid2;
	  let nd1 = Info.get_node info1 in
	  let nd2 = Info.get_node info2 in
	  let incompat, by_non_renames = is_incompatible nd1 nd2 in
	  if incompat then begin
	    DEBUG_MSG "incompatible relabel%s: %s" (if by_non_renames then "[by non-renames]" else "") (Editop.to_string rel);
            let is_good = is_good_relabel nd1 nd2 in
            if is_good then
              DEBUG_MSG "good relabel"
            else
	      to_be_removed := (nd1, nd2, by_non_renames) :: !to_be_removed;
	  end
(*
	  else if is_incompatible_def nd1 nd2 then begin
	    DEBUG_MSG "incompatible relabel (DEF): %s" (Editop.to_string rel);
	    to_be_removed := (nd1, nd2) :: !to_be_removed;
	  end
*)
      end
      | _ -> assert false
    );

  DEBUG_MSG "* removing incompatible relabels and mapping...";

  remove_relabels_and_mapping cenv tree1 tree2 edits uidmapping !to_be_removed;

  DEBUG_MSG "* finding compatible pairs...";

  let cands_pair_tbl = Hashtbl.create 0 in (* (bid * bid) -> node list * node list *)

  let check_tbl1 nd =
    let bid = get_bid nd in
    if Hashtbl.mem rename_tbl1 bid then begin
      let key = bid, (Hashtbl.find rename_tbl1 bid) in
      try
	let cands1, cands2 = Hashtbl.find cands_pair_tbl key in
	if not (List.memq nd cands1) then
	  Hashtbl.replace cands_pair_tbl key ((nd::cands1), cands2)
      with
	Not_found -> 
	  Hashtbl.add cands_pair_tbl key ([nd], [])
    end
  in
  let check_tbl2 nd =
    let bid = get_bid nd in
    if Hashtbl.mem rename_tbl2 bid then begin
      let key = (Hashtbl.find rename_tbl2 bid), bid in
      try
	let cands1, cands2 = Hashtbl.find cands_pair_tbl key in
	if not (List.memq nd cands2) then
	  Hashtbl.replace cands_pair_tbl key (cands1, (nd::cands2))
      with
	Not_found -> 
	  Hashtbl.add cands_pair_tbl key ([], [nd])
    end
  in
  let check check_tbl info =
    let nd = Info.get_node info in
    Array.iteri 
      (fun i filt ->
	try
	  if filt nd then
	    check_tbl nd
	with
	  Not_found -> ()
      ) filters
  in

  edits#iter_deletes 
    (function
      | Delete(_, _, info, ex) -> check check_tbl1 info
      | _ -> assert false
    );
  edits#iter_inserts
    (function
      | Insert(_, _, info, ex) -> check check_tbl2 info
      | _ -> assert false
    );

  BEGIN_DEBUG
    DEBUG_MSG "cands pair table:";
    let cs2s cs = 
      Xlist.to_string (fun n -> Printf.sprintf "%a(%a)" UID.ps n#uid GI.ps n#gindex) ";" cs 
    in
    Hashtbl.iter 
      (fun (bid1, bid2) (cands1, cands2) -> 
	DEBUG_MSG "  (%a,%a) [%s]-[%s]" BID.ps bid1 BID.ps bid2
	  (cs2s cands1) (cs2s cands2)
      ) cands_pair_tbl
  END_DEBUG;

  (* select compatible pairs *)
  let compatible_pairs = ref [] in

  Hashtbl.iter 
    (fun (bid1, bid2) (cands1, cands2) -> 
      match cands1, cands2 with
      | [], _ | _, [] -> ()
      | [nd1], [nd2] -> compatible_pairs := (nd1, nd2) :: !compatible_pairs
      | nds1, nds2 ->
	  let pair_weight_list = ref [] in
	  List.iter
	    (fun nd1 ->
	      List.iter
		(fun nd2 ->
                  let cond = nd1#data#eq nd2#data || nd1#data#relabel_allowed nd2#data in
                  if cond then
		    let w = 
		      Stdlib.truncate ((cenv#get_adjacency_score nd1 nd2) *. 10000.0) 
		    in
		    pair_weight_list := (nd1, nd2, w) :: !pair_weight_list

		) nds2
	    ) nds1;

	  BEGIN_DEBUG
	    DEBUG_MSG "pair_weight_list:";
            List.iter 
	      (fun (n1, n2, w) -> 
		DEBUG_MSG " %a-%a: %d" UID.ps n1#uid UID.ps n2#uid w
	      ) !pair_weight_list
	  END_DEBUG;

	  let pairs, _ =
	    UIDmapping.select_p_pairs (fun _ _ _ _ _ _ -> true) tree1 tree2 !pair_weight_list
	  in
	  compatible_pairs := (List.map (fun (n1, n2, _) -> n1, n2) pairs) @ !compatible_pairs
    ) cands_pair_tbl;

  BEGIN_DEBUG
    List.iter
      (fun (n1, n2) ->
	DEBUG_MSG "compatible_pair: %a-%a (%a-%a)" 
	  UID.ps n1#uid UID.ps n2#uid GI.ps n1#gindex GI.ps n2#gindex
      ) 
      (List.fast_sort 
	 (fun (n1, _) (n2, _) -> Stdlib.compare n1#gindex n2#gindex) 
	 !compatible_pairs);
  END_DEBUG;

  DEBUG_MSG "* locking relabels...";

  edits#iter_relabels (* lock relabels *)
    (function
      | Relabel(_, (u1, info1, ex1), (u2, info2, ex2)) -> begin
	  let nd1 = Info.get_node info1 in
	  let nd2 = Info.get_node info2 in

	  DEBUG_MSG "relabel %a-%a (%a-%a)" UID.ps u1 UID.ps u2 GI.ps nd1#gindex GI.ps nd2#gindex;

	  if (is_use nd1 && is_use nd2) || (is_def nd1 && is_def nd2) then begin
	    let bid1 = get_bid nd1 in
	    let bid2 = get_bid nd2 in

	    DEBUG_MSG "  %a -> %a" BID.ps bid1 BID.ps bid2;
	    try
	      let bid1' = Hashtbl.find rename_tbl1 bid1 in
              let lock =
                bid1' = bid2 && 
                (match Hashtbl.find cands_pair_tbl (bid1, bid2) with
                | [x], [y] -> true
                | _ -> false
                )
              in
	      if lock then begin
		lock_mapping tree1 tree2 uidmapping nd1 nd2
	      end
	    with
	      Not_found -> ()
	  end

      end
      | _ -> assert false
    );

  DEBUG_MSG "* generating compatible edits...";

  generate_compatible_edits options cenv tree1 tree2 uidmapping edits 
    !compatible_pairs is_incompatible;

(*
  let rename_tbl1 = Hashtbl.create 0 in
  let rename_tbl2 = Hashtbl.create 0 in
  edits#iter_relabels
    (function
      | Relabel(_, (u1, info1, ex1), (u2, info2, ex2)) -> begin
	  let n1 = Info.get_node info1 in
	  let n2 = Info.get_node info2 in
	  try
	    let bi1 = get_bid n1 in
	    let bi2 = get_bid n2 in
	    DEBUG_MSG "adding %a-%a" BID.ps bi1 BID.ps bi2;
	    Hashtbl.add rename_tbl1 bi1 bi2;
	    Hashtbl.add rename_tbl2 bi2 bi1
	  with
	    Not_found -> ()
      end
      | _ -> assert false
    );
*)
  cenv#set_is_possible_rename 
    (fun n1 n2 ->
      let bi1_opt = try Some (get_bid n1) with Not_found -> None in
      let bi2_opt = try Some (get_bid n2) with Not_found -> None in
      match bi1_opt, bi2_opt with
      | Some bi1, Some bi2 ->
	  if Hashtbl.mem rename_tbl1 bi1 then
	    Hashtbl.find rename_tbl1 bi1 = bi2
	  else
	    is_possible_rename n1 n2 bi1 bi2

      | Some bi1, None -> 
	  not (non_rename non_rename_bid_tbl1 bi1) && not (Hashtbl.mem rename_tbl1 bi1)

      | None, Some bi2 -> 
	  not (non_rename non_rename_bid_tbl2 bi2) && not (Hashtbl.mem rename_tbl2 bi2)

      | None, None -> true
    );


  DEBUG_MSG "FINISHED!"

(* end of func adjust_renames *)
