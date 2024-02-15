(*
   Copyright 2012-2024 Codinuum Software Lab <https://codinuum.com>

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

let nsps = Misc.nsps
let nups = Misc.nups
let nugsps = Misc.nugsps

type node_t = Spec.node_t
type tree_t = Spec.tree_t

include Edit_base

class seq options = object (self)
  inherit [node_t, tree_t] seq_base options as super

  method dump_delta
      ?(extra_ns_decls=[])
      ?(info_file_path="")
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
      ~info_file_path
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
    end;
    DEBUG_MSG "done."
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
              end;(*do not remove!!!NG!!!*)

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
  DEBUG_MSG "%a-%a" UID.ps nd1#uid UID.ps nd2#uid;
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
  let count = ref 0 in
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
                edits#add_edit rel;
                incr count
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

    ) compatible_pairs;
  !count
(* end of func generate_compatible_edits *)


let mkfilt getlab is_x nd =
  try
    is_x (getlab nd)
  with
    Not_found -> false


let is_def nd = B.is_def nd#data#binding
let is_non_local_def nd = B.is_non_local_def nd#data#binding
let is_use nd = B.is_use nd#data#binding
let get_bid nd = B.get_bid nd#data#binding
let get_bid_opt nd = B.get_bid_opt nd#data#binding


let collect_use_renames ?(filt=fun _ _ -> true) cenv uidmapping edits is_possible_rename =

  let freq_tbl = Hashtbl.create 0 in
  let bonus_tbl = Hashtbl.create 0 in

  let _use_rename_tbl1 = Hashtbl.create 0 in
  let _use_rename_tbl2 = Hashtbl.create 0 in

  let free_freq_tbl = Hashtbl.create 0 in
  let free_bonus_tbl = Hashtbl.create 0 in

  let _free_rename_tbl1 = Hashtbl.create 0 in
  let _free_rename_tbl2 = Hashtbl.create 0 in

  let get_orig_name n = try n#data#get_orig_name with _ -> n#data#get_name in

  let add_use_rename node1 node2 bid1 bid2 =
    let name1 = node1#data#get_name(*get_orig_name node1*) in
    let name2 = node2#data#get_name(*get_orig_name node2*) in
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
      add _use_rename_tbl1 bid1 bid2;
      add _use_rename_tbl2 bid2 bid1;
      DEBUG_MSG "added";
      let k = bid1, bid2 in
      if
        not node1#data#is_order_insensitive && not node2#data#is_order_insensitive &&
        try
          let pnd1 = node1#initial_parent in
          let pnd2 = node2#initial_parent in
          not pnd1#data#is_order_insensitive && not pnd2#data#is_order_insensitive &&
          uidmapping#find pnd1#uid = pnd2#uid &&
          let ppnd1 = pnd1#initial_parent in
          let ppnd2 = pnd2#initial_parent in
          not ppnd1#data#is_order_insensitive && not ppnd2#data#is_order_insensitive &&
          uidmapping#find ppnd1#uid = ppnd2#uid
        with
          _ -> false
      then
        Hashtbl.replace bonus_tbl k 1;
      try
        let freq, nm1, nm2 = Hashtbl.find freq_tbl k in
        DEBUG_MSG "nm1=\"%s\" nm2=\"%s\"" nm1 nm2;
        assert (nm1 = name1 && nm2 = name2);
        Hashtbl.replace freq_tbl k (freq + 1, name1, name2)
      with
        Not_found -> Hashtbl.add freq_tbl k (1, name1, name2)
    end
    else begin
      DEBUG_MSG "not added";
    end
  in

  let add_free_rename node1 node2 =
    let name1 = get_orig_name node1 in
    let name2 = get_orig_name node2 in
    DEBUG_MSG "adding \"%s\" -> \"%s\"" name1 name2;
    let add tbl nm1 nm2 =
      try
        let nms = Hashtbl.find tbl nm1 in
        if not (List.mem nm2 nms) then
          Hashtbl.replace tbl nm1 (nm2::nms)
      with
        Not_found -> Hashtbl.add tbl nm1 [nm2]
    in
    if is_possible_rename node1 node2 BID.dummy BID.dummy then begin
      add _free_rename_tbl1 name1 name2;
      add _free_rename_tbl2 name2 name1;
      DEBUG_MSG "added";
      let k = name1, name2 in
      if
        not node1#data#is_order_insensitive && not node2#data#is_order_insensitive &&
        try
          let pnd1 = node1#initial_parent in
          let pnd2 = node2#initial_parent in
          not pnd1#data#is_order_insensitive && not pnd2#data#is_order_insensitive &&
          uidmapping#find pnd1#uid = pnd2#uid &&
          let ppnd1 = pnd1#initial_parent in
          let ppnd2 = pnd2#initial_parent in
          not ppnd1#data#is_order_insensitive && not ppnd2#data#is_order_insensitive &&
          uidmapping#find ppnd1#uid = ppnd2#uid
        with
          _ -> false
      then
        Hashtbl.replace free_bonus_tbl k 1;
      try
        let freq = Hashtbl.find free_freq_tbl k in
        Hashtbl.replace free_freq_tbl k (freq + 1)
      with
        Not_found -> Hashtbl.add free_freq_tbl k 1
    end
    else begin
      DEBUG_MSG "not added";
    end
  in

  edits#iter_relabels
    (function
      | Relabel(_, (u1, info1, ex1), (u2, info2, ex2)) as rel -> begin
          let _ = rel in
          DEBUG_MSG "checking %s" (Editop.to_string rel);
          let nd1 = Info.get_node info1 in
          let nd2 = Info.get_node info2 in
          if filt nd1 nd2 then begin
            if is_use nd1 && is_use nd2 then begin
              try
                add_use_rename nd1 nd2 (get_bid nd1) (get_bid nd2)
              with
                Not_found -> assert false
            end
            else if
              not (is_def nd1) && not (is_def nd2) &&
              nd1#data#is_named_orig && nd2#data#is_named_orig
            then begin
              try
                add_free_rename nd1 nd2
              with Not_found -> ()
            end
          end
      end
      | _ -> assert false
    );
  (*!20240205!uidmapping#iter
    (fun u1 u2 ->
      DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;
      let nd1 = cenv#tree1#search_node_by_uid u1 in
      let nd2 = cenv#tree2#search_node_by_uid u2 in
      if filt nd1 nd2 then begin
        if is_use nd1 && is_use nd2 then begin
          try
            add_use_rename nd1 nd2 (get_bid nd1) (get_bid nd2)
          with
            Not_found -> assert false
        end
        else if
          not (is_def nd1) && not (is_def nd2) &&
          nd1#data#is_named_orig && nd2#data#is_named_orig
        then begin
          add_free_rename nd1 nd2
        end
      end
    );*)

  Hashtbl.iter
    (fun (bi1, bi2 as k) bonus ->
      try
        let c, name1, name2 = Hashtbl.find freq_tbl k in
        DEBUG_MSG "adding bonus: (%a,%a) -> %d+%d" BID.ps bi1 BID.ps bi2 c bonus;
        Hashtbl.replace freq_tbl k (c + bonus, name1, name2)
      with
        Not_found -> ()
    ) bonus_tbl;

  Hashtbl.iter
    (fun (nm1, nm2 as k) bonus ->
      try
        let c = Hashtbl.find free_freq_tbl k in
        DEBUG_MSG "adding bonus: (\"%s\",\"%s\") -> %d+%d" nm1 nm2 c bonus;
        Hashtbl.replace free_freq_tbl k (c + bonus)
      with
        Not_found -> ()
    ) free_bonus_tbl;

  BEGIN_DEBUG
    DEBUG_MSG "* use rename freq.:";
    Hashtbl.iter
      (fun (bi1, bi2) (freq, nm1, nm2) ->
        DEBUG_MSG " (%a, %a) -> %3d (\"%s\" -> \"%s\")" BID.ps bi1 BID.ps bi2 freq nm1 nm2
      ) freq_tbl;
    DEBUG_MSG "* free rename freq.:";
    Hashtbl.iter
      (fun (nm1, nm2) freq ->
        DEBUG_MSG "(\"%s\"->\"%s\") -> %d" nm1 nm2 freq
      ) free_freq_tbl
  END_DEBUG;

  freq_tbl, _use_rename_tbl1, _use_rename_tbl2, free_freq_tbl, _free_rename_tbl1, _free_rename_tbl2

exception Abort

let is_uniq_child pnd nd =
  let b =
    try
      let pos = Sourcecode.get_logical_pos ?strict:(Some true) nd in
      match Sourcecode.get_logical_nth_child pnd pos with
      | [|_|] -> true
      | _ -> false
    with _ ->
      pnd#initial_nchildren = 1
  in
  DEBUG_MSG "%a -> %B" UID.ps nd#uid b;
  b

let has_uniq_path rt nd =
  let b =
    try
      let prev = ref nd in
      nd#iter_initial_ancestor_nodes
        (fun n ->
          if not (is_uniq_child n !prev) then
            raise Abort;
          if n == rt then
            raise Exit;
          prev := n
        );
      true
    with
    | Abort -> false
    | Exit -> true
  in
  (*DEBUG_MSG "%a (rt=%a) -> %B" nd#uid rt#uid b*)
  b

let has_uniq_paths rt1 rt2 n1 n2 =
  let b = has_uniq_path rt1 n1 && has_uniq_path rt2 n2 in
  DEBUG_MSG "(%a-%a) %a-%a -> %B" UID.ps rt1#uid UID.ps rt2#uid UID.ps n1#uid UID.ps n2#uid b;
  b


(* adjust_renames assumes that edit seq. contains correct renames of USEs *)
let adjust_renames
    ?(handle_weak=true)
    ?(trust_moved_non_renames=true)
    options
    cenv
    uidmapping
    edits
    (filters : (node_t -> bool) array)
    =
  DEBUG_MSG "START! (handle_weak=%B,trust_moved_non_renames=%B)" handle_weak trust_moved_non_renames;

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
        (try uidmapping#find n1#initial_parent#uid = n2#initial_parent#uid with _ -> false) &&
        (trust_moved_non_renames || not (edits#mem_mov12 u1 u2))
      in
      DEBUG_MSG "context_cond=%B" context_cond;
      if context_cond then
      try
        let bi1 = get_bid n1 in
        let bi2 = get_bid n2 in
        if n1#data#eq n2#data then begin

          let name = try n1#data#get_name with _ -> "" in
          let _ = name in

          if (*is_non_local_def*)is_def n1 && (*is_non_local_def*)is_def n2 then begin
            set_tbl_def non_rename_bid_tbl1 bi1;
            set_tbl_def non_rename_bid_tbl2 bi2;

            DEBUG_MSG "non_rename (def): %a-%a (%s)" BID.ps bi1 BID.ps bi2 name;
          end
          else if is_use n1 && is_use n2 then begin
            set_tbl_use non_rename_bid_tbl1 bi1;
            set_tbl_use non_rename_bid_tbl2 bi2;

            DEBUG_MSG "non_rename (use): %a-%a (%s)" BID.ps bi1 BID.ps bi2 name;
          end

        end
      with
        Not_found -> ()
    );

  let non_rename tbl bi =
    let b =
      try
        match Hashtbl.find tbl bi with
        | true, true -> true
        | _ -> false
      with
        Not_found -> false
    in
    DEBUG_MSG "%a -> %B" BID.ps bi b;
    b
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
          | Some pbi1, Some pbi2 -> true(*bi1 = pbi1 && bi2 = pbi2*)
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
      let same_name() =
        let b =
          try
            node1#data#get_name = node2#data#get_name &&
            node1#data#get_category <> node2#data#get_category
          with
            _ -> false
        in
        DEBUG_MSG "%B" b;
        b
      in
      let parent_mapped() =
        let b =
          try
            let p1 = node1#initial_parent in
            let p2 = node2#initial_parent in
            let pu1 = p1#uid in
            let pu2 = p2#uid in
            uidmapping#find pu1 == pu2 &&
            let p1_eq_p2 = p1#data#eq p2#data in
            p1_eq_p2 && has_uniq_paths p1 p2 node1 node2 ||
            not p1_eq_p2 && p1#data#is_named && p2#data#is_named &&
            not (edits#mem_mov12 pu1 pu2)
          with
            _ -> false
        in
        DEBUG_MSG "%B" b;
        b
      in
      let b =
        let has_conflict =
          context_cond &&
          (non_rename non_rename_bid_tbl1 bi1 || non_rename non_rename_bid_tbl2 bi2)
        in
        if has_conflict then
          DEBUG_MSG "%a-%a: conflicts with exactly matched pair" BID.ps bi1 BID.ps bi2;
        not has_conflict || same_name() || parent_mapped()
      in
      if not b then
        DEBUG_MSG "%a-%a: conflicts with exactly matched pair" BID.ps bi1 BID.ps bi2;
      b
    else
      false
  in

  let has_nearest_mapped_ancestor_upto_boundary n1 n2 =
    DEBUG_MSG "%a-%a" nups n1 nups n2;
    let moveon_ n = not n#data#is_boundary && (n#data#is_named || n#data#is_primary) in
    let find_anc = Sourcecode.find_nearest_mapped_ancestor_node ~moveon_ in
    let b =
      try
        let an1 = find_anc uidmapping#mem_dom n1 in
        let an2 = find_anc uidmapping#mem_cod n2 in
        let au1' = uidmapping#find an1#uid in
        DEBUG_MSG "%a->%a" nups an1 UID.ps au1';
        au1' = an2#uid &&
        (
         (try n1#initial_parent == an1 with _ -> false) ||
         (try n2#initial_parent == an2 with _ -> false)
        )
        (*&&
        (an1#data#eq an2#data(* || an1#data#_stripped_label = an2#data#_stripped_label*))*)
      with _ -> false
    in
    DEBUG_MSG "%B" b;
    b
  in

  (* collect use/free renames *)
  let freq_tbl, _use_rename_tbl1, _use_rename_tbl2,
    free_freq_tbl, _free_rename_tbl1, _free_rename_tbl2
      =
    let filt n1 n2 =
      let b =
        (
         try
           let u1' = uidmapping#find n1#initial_parent#uid in
           u1' = n2#initial_parent#uid ||
           n1#data#_anonymized_label = n2#data#_anonymized_label &&
           u1' = n2#initial_parent#initial_parent#uid
         with _ -> false
        ) ||
        n1#data#_anonymized_label = n2#data#_anonymized_label &&
        (
         (try
           let u2' = uidmapping#inv_find n2#initial_parent#uid in
           u2' = n1#initial_parent#initial_parent#uid
          with _ -> false
         ) ||
         has_nearest_mapped_ancestor_upto_boundary n1 n2
        )
      in
      DEBUG_MSG "%a-%a: %B" UID.ps n1#uid UID.ps n2#uid b;
      b
    in
    collect_use_renames ~filt cenv uidmapping edits is_possible_rename
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
      | [bi2] -> begin
          if handle_weak || max_freq > 1 then begin
            DEBUG_MSG "added";
            selected_renames1 := (bi1, bi2) :: !selected_renames1
          end;
          if handle_weak && max_freq = 1 then begin
            DEBUG_MSG "added (weak)";
            weak_selected_renames1 := (bi1, bi2) :: !weak_selected_renames1
          end;
          List.iter
            (fun bi ->
              if bi != bi2 then
                Hashtbl.add loser_tbl (bi1, bi) true
            ) bs
      end
      | _ -> conflicting_bids2 := bs @ !conflicting_bids2
    ) _use_rename_tbl1;

  Hashtbl.iter
    (fun bi2 bs ->
      DEBUG_MSG "* selecting from: [%s] <- %a" (Xlist.to_string BID.to_string ";" bs) BID.ps bi2;
      let max_freq = List.fold_left (fun max bi1 -> sel_freq max bi1 bi2) 0 bs in
      DEBUG_MSG "  max freq.: %d" max_freq;

      let selected =
        List.filter (fun bi1 -> max_freq = get_freq bi1 bi2) bs
      in
      DEBUG_MSG "  selected: [%s] <- %a" (Xlist.to_string BID.to_string ";" selected) BID.ps bi2;

      match selected with
      | []   -> assert false
      | [bi1] -> begin
          if handle_weak || max_freq > 1 then begin
            DEBUG_MSG "added";
            selected_renames2 := (bi1, bi2) :: !selected_renames2
          end;
          if handle_weak && max_freq = 1 then begin
            DEBUG_MSG "added (weak)";
            weak_selected_renames2 := (bi1, bi2) :: !weak_selected_renames2
          end;
          List.iter
            (fun bi ->
              if bi != bi1 then
                Hashtbl.add loser_tbl (bi, bi2) true
            ) bs
      end
      | _ -> conflicting_bids1 := bs @ !conflicting_bids1
    ) _use_rename_tbl2;

  DEBUG_MSG "  conflicting_bids1: %s" (Xlist.to_string BID.to_string ";" !conflicting_bids1);
  DEBUG_MSG "  conflicting_bids2: %s" (Xlist.to_string BID.to_string ";" !conflicting_bids2);

  let tree1 = cenv#tree1 in
  let tree2 = cenv#tree2 in

  let selected_renames =
    List.filter
      (fun ((bi1, bi2) as bp) ->
        let no_conflicts1 = not (List.mem bi1 !conflicting_bids1) in
        let no_conflicts2 = not (List.mem bi2 !conflicting_bids2) in
        begin
         try
           let freq, nm1, nm2 = Hashtbl.find freq_tbl bp in
           DEBUG_MSG "%a-%a \"%s\"-\"%s\" %d" BID.ps bi1 BID.ps bi2 nm1 nm2 freq;
           if freq > 1 && (no_conflicts1 || no_conflicts2) then begin
             cenv#add_rename_pat (nm1, nm2);
             List.iter
               (fun bi1_ ->
                 try
                   let nm1_ = tree1#find_name_for_bid bi1_ in
                   DEBUG_MSG "bi1_=%a nm1_=\"%s\"" BID.ps bi1_ nm1_;
                   cenv#add_rename_pat (nm1_, nm2)
                 with
                   _ -> ()
               ) (tree1#find_mapped_bids bi1);
             List.iter
               (fun bi2_ ->
                 try
                   let nm2_ = tree2#find_name_for_bid bi2_ in
                   DEBUG_MSG "bi2_=%a nm2_=\"%s\"" BID.ps bi2_ nm2_;
                   cenv#add_rename_pat (nm1, nm2_)
                 with
                   _ -> ()
               ) (tree2#find_mapped_bids bi2)
           end
          with _ -> ()
        end;
        not (Hashtbl.mem loser_tbl (bi1, bi2)) && no_conflicts1 && no_conflicts2
      ) (Xlist.union !selected_renames1 !selected_renames2)
  in

  let weak_selected_renames =
    Xlist.intersection selected_renames
      (Xlist.union !weak_selected_renames1 !weak_selected_renames2)
  in
  let weak_selected_renames_from, weak_selected_renames_to =
    List.split weak_selected_renames
  in

  (* select free renames *)
  let selected_free_renames1 = ref [] in
  let selected_free_renames2 = ref [] in

  (*let weak_selected_free_renames1 = ref [] in
  let weak_selected_free_renames2 = ref [] in*)

  let conflicting_nms1 = ref [] in
  let conflicting_nms2 = ref [] in

  let free_loser_tbl = Hashtbl.create 0 in

  let get_free_freq nm1 nm2 =
    let freq = Hashtbl.find free_freq_tbl (nm1, nm2) in
    freq
  in

  let sel_free_freq max bi1 bi2 =
    try
      let freq = get_free_freq bi1 bi2 in
      if freq > max then
        freq
      else
        max
    with
      Not_found -> assert false
  in

  Hashtbl.iter
    (fun nm1 nms ->
      DEBUG_MSG "* selecting from: %s -> [%s]" nm1 (Xlist.to_string (fun x -> x) ";" nms);
      let max_freq = List.fold_left (fun max nm2 -> sel_free_freq max nm1 nm2) 0 nms in
      DEBUG_MSG "  max freq.: %d" max_freq;
      let selected = List.filter (fun nm2 -> max_freq = get_free_freq nm1 nm2) nms in
      DEBUG_MSG "  selected: %s -> [%s]" nm1 (Xlist.to_string (fun x -> x) ";" selected);
      match selected with
      | []   -> assert false
      | [nm2] -> begin
          if handle_weak || max_freq > 1 then
            selected_free_renames1 := (nm1, nm2) :: !selected_free_renames1;
          (*if handle_weak && max_freq = 1 then
            weak_selected_free_renames1 := (nm1, nm2) :: !weak_selected_free_renames1;*)
          List.iter
            (fun nm ->
              if nm != nm2 then
                Hashtbl.add free_loser_tbl (nm1, nm) true
            ) nms
      end
      | _ -> conflicting_nms2 := nms @ !conflicting_nms2
    ) _free_rename_tbl1;

  Hashtbl.iter
    (fun nm2 nms ->
      DEBUG_MSG "* selecting from: [%s] <- %s" (Xlist.to_string (fun x -> x) ";" nms) nm2;
      let max_freq = List.fold_left (fun max nm1 -> sel_free_freq max nm1 nm2) 0 nms in
      DEBUG_MSG "  max freq.: %d" max_freq;
      let selected = List.filter (fun nm1 -> max_freq = get_free_freq nm1 nm2) nms in
      DEBUG_MSG "  selected: [%s] <- %s" (Xlist.to_string (fun x -> x) ";" selected) nm2;
      match selected with
      | []   -> assert false
      | [nm1] -> begin
          if max_freq > 1 || handle_weak then
            selected_free_renames2 := (nm1, nm2) :: !selected_free_renames2;
          (*if max_freq = 1 && handle_weak then
            weak_selected_free_renames2 := (nm1, nm2) :: !weak_selected_free_renames2;*)
          List.iter
            (fun nm ->
              if nm != nm1 then
                Hashtbl.add free_loser_tbl (nm, nm2) true
            ) nms
      end
      | _ -> conflicting_nms1 := nms @ !conflicting_nms1
    ) _free_rename_tbl2;

  DEBUG_MSG "  conflicting_nms1: %s" (Xlist.to_string (fun x -> x) ";" !conflicting_nms1);
  DEBUG_MSG "  conflicting_nms2: %s" (Xlist.to_string (fun x -> x) ";" !conflicting_nms2);

  let selected_free_renames =
    List.filter
      (fun ((nm1, nm2) as nmp) ->
        let no_conflicts1 = not (List.mem nm1 !conflicting_nms1) in
        let no_conflicts2 = not (List.mem nm2 !conflicting_nms2) in
        begin
         try
           let freq = Hashtbl.find free_freq_tbl nmp in
           DEBUG_MSG "\"%s\"-\"%s\" %d" nm1 nm2 freq;
           if (freq > 1 || nm1 = nm2) && (no_conflicts1 || no_conflicts2) then
             cenv#add_rename_pat nmp
          with _ -> ()
        end;
        not (Hashtbl.mem free_loser_tbl nmp) && no_conflicts1 && no_conflicts2
      ) (Xlist.union !selected_free_renames1 !selected_free_renames2)
  in

  (*let weak_selected_free_renames =
    Xlist.intersection selected_free_renames
      (Xlist.union !weak_selected_free_renames1 !weak_selected_free_renames2)
  in*)

  BEGIN_DEBUG
    DEBUG_MSG "* selected use renames:";
    List.iter
      (fun (bi1, bi2) ->
        DEBUG_MSG " %a -> %a" BID.ps bi1 BID.ps bi2
      ) selected_renames;
    DEBUG_MSG "* weakly selected use renames:";
    List.iter
      (fun (bi1, bi2) ->
        DEBUG_MSG " %a -> %a" BID.ps bi1 BID.ps bi2
      ) weak_selected_renames;
    DEBUG_MSG "* selected free renames:";
    List.iter
      (fun (nm1, nm2) ->
        DEBUG_MSG " \"%s\" -> \"%s\"" nm1 nm2
      ) selected_free_renames;
    (*DEBUG_MSG "* weakly selected free renames:";
    List.iter
      (fun (nm1, nm2) ->
        DEBUG_MSG " \"%s\" -> \"%s\"" nm1 nm2
      ) weak_selected_free_renames*)
  END_DEBUG;

  begin
    DEBUG_MSG "checking rename patterns...";
    let rename_pat_tbl = Hashtbl.create 0 in
    List.iter
      (fun l ->
        List.iter
          (fun bp ->
            try
              let freq, nm1, nm2 = Hashtbl.find freq_tbl bp in
              let nmp = nm1, nm2 in
              try
                let c = Hashtbl.find rename_pat_tbl nmp in
                Hashtbl.replace rename_pat_tbl nmp (c+freq)
              with
                Not_found -> Hashtbl.add rename_pat_tbl nmp freq
            with
              _ -> ()
          ) l
      ) [selected_renames(*; weak_selected_renames*)];
    List.iter
      (fun l ->
        List.iter
          (fun nmp ->
            try
              let freq = Hashtbl.find free_freq_tbl nmp in
              try
                let c = Hashtbl.find rename_pat_tbl nmp in
                Hashtbl.replace rename_pat_tbl nmp (c+freq)
              with
                Not_found -> Hashtbl.add rename_pat_tbl nmp freq
            with
              _ -> ()
          ) l
      ) [selected_free_renames(*; weak_selected_free_renames*)];
    Hashtbl.iter
      (fun ((nm1, nm2) as nmp) c ->
        DEBUG_MSG "\"%s\"->\"%s\" %d" nm1 nm2 c;
        if c > 1 then
          cenv#add_rename_pat nmp
      ) rename_pat_tbl;
    DEBUG_MSG "done."
  end;

  cenv#finalize_rename_pat();

  let rename_tbl1 = Hashtbl.create 0 in
  let rename_tbl2 = Hashtbl.create 0 in
  List.iter
    (fun (bi1, bi2) ->
      Hashtbl.add rename_tbl1 bi1 bi2;
      Hashtbl.add rename_tbl2 bi2 bi1;
    ) selected_renames;

  let is_good_relabel nd1 nd2 =
    DEBUG_MSG "%a-%a" UID.ps nd1#uid UID.ps nd2#uid;
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
      DEBUG_MSG "parent_cond=%B children_cond=%B" parent_cond children_cond;
      nd1#data#relabel_allowed nd2#data && parent_cond && children_cond
    with
      Otreediff.Otree.Parent_not_found _ -> false
  in

  let is_incompatible nd1 nd2 =
    let pnd1 = nd1#initial_parent in
    let pnd2 = nd2#initial_parent in
    let context_cond = try uidmapping#find pnd1#uid = pnd2#uid with _ -> false in
    DEBUG_MSG "%a-%a context_cond=%B" nups nd1 nups nd2 context_cond;
    (*let is_stable = not (edits#mem_mov12 nd1#uid nd2#uid) in
    DEBUG_MSG "%a-%a is_stable=%B" UID.ps nd1#uid UID.ps nd2#uid is_stable;*)

    let same_name =
      try
        nd1#data#get_name = nd2#data#get_name && nd1#data#get_category <> nd2#data#get_category
      with
        _ -> false
    in
    let bi1_opt, non_rename1, bi1'_opt =
      try
        let bi1 = get_bid nd1 in
        DEBUG_MSG "bi1=%a" BID.ps bi1;
        let non_rename1 = non_rename non_rename_bid_tbl1 bi1 in
        DEBUG_MSG "non_rename1=%B" non_rename1;
        try
          let bi1' = Hashtbl.find rename_tbl1 bi1 in
          DEBUG_MSG "bi1'=%a" BID.ps bi1';
          Some bi1, non_rename1, Some bi1'
        with
          Not_found -> Some bi1, non_rename1, None
      with
        Not_found -> None, false, None
    in
    let bi2_opt, non_rename2, bi2'_opt =
      try
        let bi2 = get_bid nd2 in
        DEBUG_MSG "bi2=%a" BID.ps bi2;
        let non_rename2 = non_rename non_rename_bid_tbl2 bi2 in
        DEBUG_MSG "non_rename2=%B" non_rename2;
        try
          let bi2' = Hashtbl.find rename_tbl2 bi2 in
          DEBUG_MSG "bi2'=%a" BID.ps bi2';
          Some bi2, non_rename2, Some bi2'
        with
          Not_found -> Some bi2, non_rename2, None
      with
        Not_found -> None, false, None
    in
    let b, by_non_renames =
      let context_cond_ = context_cond(* || is_stable*) in
      match bi1_opt, bi2_opt with
      | Some bi1, Some bi2 -> begin
          DEBUG_MSG "bi1=%a bi2=%a" BID.ps bi1 BID.ps bi2;
          if (non_rename1 || non_rename2) && not same_name then begin
            context_cond_, true
          end
          else
            context_cond_ &&
            (
             not (List.mem bi1 weak_selected_renames_from) &&
             not (List.mem bi2 weak_selected_renames_to)
            )(* &&
            (
              (match bi1'_opt with Some bi1' -> bi2 <> bi1' | None -> false) ||
              (match bi2'_opt with Some bi2' -> bi1 <> bi2' | None -> false)
            )*), false
      end
      | Some _, None when nd2#data#is_literal -> false, false
      | None, Some _ when nd1#data#is_literal -> false, false

      | Some bi1, None ->
          DEBUG_MSG "bi1=%a bi2=None" BID.ps bi1;
          context_cond_ &&
          (non_rename1 || (match bi1'_opt with Some _ -> true | None -> false)), non_rename1

      | None, Some bi2 ->
          DEBUG_MSG "bi1=None bi2=%a" BID.ps bi2;
          context_cond_ &&
          (non_rename2 || (match bi2'_opt with Some _ -> true | None -> false)), non_rename2

      | None, None ->
          false, false
    in
    DEBUG_MSG "%a-%a: b=%B by_non_renames=%B" nups nd1 nups nd2 b by_non_renames;
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

  let remove_from_rename_tbls n1 n2 =
    try
      let bi1 = get_bid n1 in
      let bi2 = get_bid n2 in
      DEBUG_MSG "%a-%a" BID.ps bi1 BID.ps bi2;
      if try Hashtbl.find rename_tbl1 bi1 = bi2 with _ -> false then
        Hashtbl.remove rename_tbl1 bi1;
      if try Hashtbl.find rename_tbl2 bi2 = bi1 with _ -> false then
        Hashtbl.remove rename_tbl2 bi2
    with _ -> ()
  in

  edits#iter_relabels (* find incompatible relabels *)
    (function
      | Relabel(_, (uid1, info1, _), (uid2, info2, _)) as rel -> begin
          let _ = rel in
          DEBUG_MSG "finding incompatible relabels: checking %a-%a" UID.ps uid1 UID.ps uid2;
          let nd1 = Info.get_node info1 in
          let nd2 = Info.get_node info2 in
          let incompat, by_non_renames = is_incompatible nd1 nd2 in
          if incompat then begin
            DEBUG_MSG "incompatible relabel%s: %s"
              (if by_non_renames then "[by non-renames]" else "") (Editop.to_string rel);
            let is_good = is_good_relabel nd1 nd2 in
            if is_good then begin
              if
                nd1#data#is_order_insensitive && nd2#data#is_order_insensitive &&
                nd1#initial_nchildren = 0 && nd2#initial_nchildren = 0
              then begin
                DEBUG_MSG "not so good relabel";
                to_be_removed := (nd1, nd2, by_non_renames) :: !to_be_removed;
                remove_from_rename_tbls nd1 nd2
              end
              else
                DEBUG_MSG "good relabel"
            end
            else begin
              DEBUG_MSG "bad relabel";
              to_be_removed := (nd1, nd2, by_non_renames) :: !to_be_removed;
              remove_from_rename_tbls nd1 nd2
            end
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
      let bid_ = Hashtbl.find rename_tbl1 bid in
      DEBUG_MSG "%a -> %a" BID.ps bid BID.ps bid_;
      let key = bid, bid_ in
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
      let _bid = Hashtbl.find rename_tbl2 bid in
      DEBUG_MSG "%a -> %a" BID.ps _bid BID.ps bid;
      let key = _bid, bid in
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
  edits#iter_relabels
    (function
      | Relabel(_, (_, info1, _), (_, info2, _)) -> begin
          check check_tbl1 info1;
          check check_tbl2 info2
      end
      | _ -> assert false
    );

  BEGIN_DEBUG
    DEBUG_MSG "cands pair table:";
    Hashtbl.iter
      (fun (bid1, bid2) (cands1, cands2) ->
        DEBUG_MSG "  (%a,%a) [%a]-[%a]" BID.ps bid1 BID.ps bid2 nugsps cands1 nugsps cands2
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
     Hashtbl.iter (fun b1 b2 -> DEBUG_MSG "rename_tbl1: %a->%a" BID.ps b1 BID.ps b2) rename_tbl1;
     Hashtbl.iter (fun b2 b1 -> DEBUG_MSG "rename_tbl2: %a<-%a" BID.ps b1 BID.ps b2) rename_tbl2
  END_DEBUG;

  (*DEBUG_MSG "* generating compatible edits...";
  let nrels =
    generate_compatible_edits options cenv tree1 tree2 uidmapping edits
      !compatible_pairs is_incompatible
  in
  DEBUG_MSG "* %d relabels generated." nrels;*)

  DEBUG_MSG "* locking relabels...";

  edits#iter_relabels (* lock relabels *)
    (function
      | Relabel(_, (u1, info1, ex1), (u2, info2, ex2)) -> begin
          let nd1 = Info.get_node info1 in
          let nd2 = Info.get_node info2 in

          DEBUG_MSG "relabel %a-%a (%a-%a)" UID.ps u1 UID.ps u2 GI.ps nd1#gindex GI.ps nd2#gindex;

          let is_final () =
            let b =
              (try
                match cenv#multiple_node_matches#find nd1#data#_label with
                | [_], [_] -> true
                | ns11, ns12 -> DEBUG_MSG "ns11=[%a] ns12=[%a]" nsps ns11 nsps ns12; false
              with
                _ -> true) &&
              (try
                match cenv#multiple_node_matches#find nd2#data#_label with
                | [_], [_] -> true
                | ns21, ns22 -> DEBUG_MSG "ns21=[%a] ns22=[%a]" nsps ns21 nsps ns22; false
              with
                _ -> true)
            in
            DEBUG_MSG "%B" b;
            b
          in

          let use_flag = is_use nd1 && is_use nd2 in
          let def_flag = is_def nd1 && is_def nd2 in

          DEBUG_MSG "use_flag=%B def_flag=%B" use_flag def_flag;

          if use_flag || def_flag then begin
            let bid1 = get_bid nd1 in
            let bid2 = get_bid nd2 in
            DEBUG_MSG "%s vs %s" (B.to_string nd1#data#binding) (B.to_string nd2#data#binding);
            try
              let bid1' = Hashtbl.find rename_tbl1 bid1 in
              let lock, final =
                if bid1' = bid2 then
                  match Hashtbl.find cands_pair_tbl (bid1, bid2) with
                  | [], _ | _, [] -> false, false
                  | [n1], [n2] -> begin
                      DEBUG_MSG "nds1=[%a] nds2=[%a]" nups n1 nups n2;
                      if
                        use_flag && is_use n1 && is_use n2 ||
                        def_flag && is_def n1 && is_def n2
                      then
                        true, is_final()
                      else
                        false, false
                  end
                  | nds1, nds2 -> begin
                      DEBUG_MSG "nds1=[%a] nds2=[%a]" nsps nds1 nsps nds2;
                      let b =
                        is_final() &&
                        if use_flag then
                          let uses1 = List.filter is_use nds1 in
                          let uses2 = List.filter is_use nds2 in
                          DEBUG_MSG "uses1=[%a] uses2=[%a]" nsps uses1 nsps uses2;
                          match uses1, uses2 with
                          | [_], [_] -> true
                          | _ -> false
                        else if def_flag then
                          let defs1 = List.filter is_def nds1 in
                          let defs2 = List.filter is_def nds2 in
                          DEBUG_MSG "defs1=[%a] defs2=[%a]" nsps defs1 nsps defs2;
                          match defs1, defs2 with
                          | [_], [_] -> true
                          | _ -> false
                        else
                          false
                      in
                      b, b
                  end
                else
                  false, false
              in
              DEBUG_MSG "lock=%B final=%B" lock final;
              if lock then begin
                lock_mapping tree1 tree2 uidmapping nd1 nd2;
                if final then
                  uidmapping#finalize_mapping nd1#uid nd2#uid
              end
            with
              Not_found -> ()
          end

      end
      | _ -> assert false
    );

  DEBUG_MSG "* generating compatible edits...";
  let nrels =
    generate_compatible_edits options cenv tree1 tree2 uidmapping edits
      !compatible_pairs is_incompatible
  in
  DEBUG_MSG "* %d relabels generated." nrels;
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
    (fun ?(strict=false) n1 n2 ->
      DEBUG_MSG "strict=%B %a-%a" strict UID.ps n1#uid UID.ps n2#uid;
      let bi1_opt = try Some (get_bid n1) with Not_found -> None in
      let bi2_opt = try Some (get_bid n2) with Not_found -> None in
      match bi1_opt, bi2_opt with
      | Some bi1, Some bi2 -> begin
          DEBUG_MSG "bi1=%a bi2=%a" BID.ps bi1 BID.ps bi2;
          if Hashtbl.mem rename_tbl1 bi1 then begin
            let bi1' = Hashtbl.find rename_tbl1 bi1 in
            DEBUG_MSG "%a->%a" BID.ps bi1 BID.ps bi1';
            bi1' = bi2 ||
            (not strict || not (Hashtbl.mem rename_tbl2 bi2)) && is_possible_rename n1 n2 bi1 bi2
          end
          else if Hashtbl.mem rename_tbl2 bi2 then begin
            let bi2' = Hashtbl.find rename_tbl2 bi2 in
            DEBUG_MSG "%a<-%a" BID.ps bi2' BID.ps bi2;
            bi2' = bi1 ||
            (not strict || not (Hashtbl.mem rename_tbl1 bi1)) && is_possible_rename n1 n2 bi1 bi2
          end
          else
            is_possible_rename n1 n2 bi1 bi2
      end
      | Some bi1, None -> begin
          DEBUG_MSG "bi1=%a" BID.ps bi1;
          (*not strict || !!!NG!!!*)
          not (non_rename non_rename_bid_tbl1 bi1) && not (Hashtbl.mem rename_tbl1 bi1) ||
          is_possible_rename n1 n2 bi1 BID.dummy
      end
      | None, Some bi2 -> begin
          DEBUG_MSG "bi2=%a" BID.ps bi2;
          (*not strict || !!!NG!!!*)
          not (non_rename non_rename_bid_tbl2 bi2) && not (Hashtbl.mem rename_tbl2 bi2) ||
          is_possible_rename n1 n2 BID.dummy bi2
      end
      | None, None -> true
    );


  DEBUG_MSG "FINISHED!";

  nrels > 0

(* end of func adjust_renames *)
