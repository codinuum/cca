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
(* misc.ml *)



module UID = Otreediff.UID
module GI = Otreediff.GIndex

let set_tree_size_limit options sz1 sz2 =
  let comp sz =
    if sz > options#tree_size_threshold then 
      int_of_float 
	((float_of_int sz) 
	   *. ((float_of_int options#tree_size_limit_percent) /. 100.0))
    else 
      max_int
  in
  let lim1 = comp sz1 in
  let lim2 = comp sz2 in
  DEBUG_MSG "%d %d" lim1 lim2;
  options#set_tree_size_limit1 lim1;
  options#set_tree_size_limit2 lim2

let check_tree_size_limit options sz1 sz2 = 
  sz1 > options#tree_size_limit1 || sz2 > options#tree_size_limit2

let check_hard_tree_size_limit options sz1 sz2 =
  sz1 > options#hard_tree_size_limit && sz2 > options#hard_tree_size_limit


class counter =
  object
    val mutable n = 0
    method incr = n <- n + 1
    method value = n
  end

class stopwatch =
  object
    val mutable stime = 0.0
    val mutable time = 0.0
    method start = stime <- (Unix.times()).Unix.tms_utime
    method stop = time <- (Unix.times()).Unix.tms_utime -. stime
    method show = time
  end

let pause() =
  Printf.printf "<<PAUSE!>>\n";
  ignore (input_line stdin)





let contract tree1 tree2 clusters =

  let deferred_cluster = ref None in
  let pruned_clusters = ref [] in

  let contract_cluster cluster =
    let clu1, clu2 = List.split cluster in

    BEGIN_DEBUG
      let clu1u = List.map (fun i -> (tree1#get i)#uid) clu1 in
      let clu2u = List.map (fun i -> (tree2#get i)#uid) clu2 in
      DEBUG_MSG "clu1=[%s](%d/%d)"
	(Xlist.to_string UID.to_string ";" clu1u) (List.length clu1) tree1#size;
      DEBUG_MSG "clu2=[%s](%d/%d)"
	(Xlist.to_string UID.to_string ";" clu2u) (List.length clu2) tree2#size
    END_DEBUG;

    let ftr1 = tree1#frontier_of_cluster clu1 in
    let ftr2 = tree2#frontier_of_cluster clu2 in

    BEGIN_DEBUG
      let ftr1u = List.map (fun i -> (tree1#get i)#uid) ftr1 in
      let ftr2u = List.map (fun i -> (tree2#get i)#uid) ftr2 in
      DEBUG_MSG "ftr1=[%s]" (Xlist.to_string UID.to_string ";" ftr1u);
      DEBUG_MSG "ftr2=[%s]" (Xlist.to_string UID.to_string ";" ftr2u)
    END_DEBUG;

    let path_cands1, topnd_list1, is_rclu1 = 
      tree1#contraction_candidates clu1 ftr1
    in
    let path_cands2, topnd_list2, is_rclu2 = 
      tree2#contraction_candidates clu2 ftr2
    in

    BEGIN_DEBUG
      DEBUG_MSG "topnd_list1=[%s]"
	(Xlist.to_string 
	   (fun nd -> UID.to_string nd#uid)
	   ";" topnd_list1);
      DEBUG_MSG "topnd_list2=[%s]"
	(Xlist.to_string 
	   (fun nd -> UID.to_string nd#uid)
	   ";" topnd_list2);
      DEBUG_MSG "path_cands1={%s}"
	(Xlist.to_string 
	   (fun (i, c) ->
	     let u = (tree1#get i)#uid in
	     let us = List.map (fun i -> (tree1#get i)#uid) c in
	     Printf.sprintf "%a:[%s]" 
	       UID.ps u (Xlist.to_string UID.to_string ";" us)) 
	   "," path_cands1);
      DEBUG_MSG "path_cands2={%s}"
	(Xlist.to_string 
	   (fun (i, c) -> 
	     let u = (tree2#get i)#uid in
	     let us = List.map (fun i -> (tree2#get i)#uid) c in
	     Printf.sprintf "%a:[%s]" 
	       UID.ps u (Xlist.to_string UID.to_string ";" us)) 
	   "," path_cands2)
    END_DEBUG;


    if is_rclu1 || is_rclu2 then 
      deferred_cluster := Some cluster
    else begin
      let prune_cands1 = ref
	  (List.flatten 
	     (List.map 
		(fun tnd -> 
		  tree1#fast_subtree_members tnd#index
		) topnd_list1
	     )
	  )
      in
      let prune_cands2 = ref
	  (List.flatten 
	     (List.map 
		(fun tnd -> 
		  tree2#fast_subtree_members tnd#index
		) topnd_list2
	     )
	  )
      in

      BEGIN_DEBUG
        let num_pruned1 =
	  match ftr1, ftr2 with
	  | [], [] -> 
	      List.fold_left 
	        (fun s nd -> 
		  s + (tree1#whole_initial_subtree_size nd)
	        ) 0 topnd_list1
	  | _ ->
	      if path_cands1 = [] || path_cands2 = [] then
	        0
	      else
	        List.length (List.filter (fun i -> List.mem i clu1) !prune_cands1)
        in
	let f x = (float num_pruned1) /. (float x) in
	let r1 = f tree1#size in
	let r2 = f tree2#size in
	DEBUG_MSG "num of nodes to be pruned: %d (%d/%d=%f) (%d/%d=%f) %f"
	  num_pruned1
	  num_pruned1 tree1#size r1
	  num_pruned1 tree2#size r2
	  (if r1 = 0.0 && r2 = 0.0 then 0.0 else (max r1 r2) /. (min r1 r2))
      END_DEBUG;


      begin (* prune nodes *)
	match ftr1, ftr2 with
	| [], [] -> 
	    tree1#prune_nodes topnd_list1; 
	    tree2#prune_nodes topnd_list2
	| _ ->
	    if path_cands1 = [] || path_cands2 = [] then begin
	      prune_cands1 := []; 
	      prune_cands2 := []
	    end
	    else begin
	      tree1#contract path_cands1 topnd_list1;
	      prune_cands1 := List.filter (fun i -> List.mem i clu1) !prune_cands1;
	      tree2#contract path_cands2 topnd_list2;
	      prune_cands2 := List.filter (fun j -> List.mem j clu2) !prune_cands2
	    end
      end;

      let pruned_cluster = ref [] in

      BEGIN_DEBUG
	let pc1 = List.map (fun i -> (tree1#get i)#uid) !prune_cands1 in
	let pc2 = List.map (fun i -> (tree2#get i)#uid) !prune_cands2 in
	DEBUG_MSG "prune_cands1=[%s]" (Xlist.to_string UID.to_string ";" pc1);
	DEBUG_MSG "prune_cands2=[%s]" (Xlist.to_string UID.to_string ";" pc2)
      END_DEBUG;

      let is_correct =
	(List.length !prune_cands1) = (List.length !prune_cands2) &&
	List.for_all
	  (fun i -> 
	    let j = List.assoc i cluster in
	    pruned_cluster := (i, j)::!pruned_cluster;
	    List.mem j !prune_cands2
	  ) !prune_cands1
      in
      if is_correct then
	pruned_clusters := !pruned_cluster::!pruned_clusters
      else
	failwith "prune_cands mismatch";
    end
  in (* end of contract_cluster *)

  List.iter contract_cluster clusters;


  let mk_uid_cluster cluster = 
    List.map (fun (i, j) -> (tree1#get i)#uid, (tree2#get j)#uid) cluster
  in
  let deferred_uid_cluster = 
    match !deferred_cluster with 
      Some clu -> Some (mk_uid_cluster clu) | None -> None
  in
  let pruned_uid_clusters = 
    List.map (fun clu -> mk_uid_cluster clu) !pruned_clusters
  in

  deferred_uid_cluster, pruned_uid_clusters
(* end of func contract *)



let get_collapsed_children nd =
  List.filter (fun n -> n#pos >= 0) (Array.to_list nd#initial_children)

let _moderate_nchildren ?(threshold=Const.default_moderate_nchildren_threshold) collapsed_children nd =
  let n = List.length collapsed_children in
  let b = n < threshold in
  if not b then
    DEBUG_MSG "too many children: %d" n;
  b

let moderate_nchildren ?(threshold=Const.default_moderate_nchildren_threshold) nd =
  let cc = get_collapsed_children nd in
  _moderate_nchildren ~threshold cc nd


let _to_be_flat collapsed_children nd =
  List.for_all (fun n -> n#is_leaf) collapsed_children

let to_be_flat nd =
  let cc = get_collapsed_children nd in
  _to_be_flat cc nd


let conv_subtree_node_pairs tree1 tree2 = 
  let sea tree nd =
    let gi = nd#gindex in
    if GI.is_valid gi then
      try
	tree#search_node_by_gindex gi
      with 
	Not_found -> 
	  WARN_MSG "gindex=%d" nd#gindex; 
	  assert false
    else
      raise (Invalid_argument "")
  in
  Xlist.filter_map 
    (fun (n1, n2) -> 
      try
	let gi1 = sea tree1 n1 in
	let gi2 = sea tree2 n2 in
	Some (gi1, gi2)
      with
	Invalid_argument _ -> None
    ) 

let get_home_dir () =
  try 
    Sys.getenv "HOME" 
  with 
    Not_found -> 
      let d = Filename.get_temp_dir_name() in
      WARN_MSG "could not find HOME, setting HOME to %s" d;
      d

exception File_found of string

let find_file_name_with_exts fname exts =
  if Xfile.file_exists fname then
    Some fname
  else
    try
      List.iter
        (fun ext ->
          let fn = fname^ext in
          if Xfile.file_exists fn then
            raise (File_found fn)
        ) exts;
      None
    with
      File_found f -> Some f
