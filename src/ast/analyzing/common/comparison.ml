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
(* comparison.ml *)

module B = Binding

open Otreediff
open Misc

let subtree_similarity_thresh = 0.7
let subtree_similarity_ratio_thresh = 0.8
(* let subtree_similarity_ratio_lower_thresh = 0.5 *)
(* let subtree_similarity_lower_thresh = 0.15 *)
let ancestors_similarity_thresh = 0.7
let ancestors_similarity_ratio_thresh = 0.8
(* let ancestors_similarity_ratio_lower_thresh = 0.5 *)
let ncross_sim_ratio_thresh = 0.8
let permutation_hub_count_thresh = 64

let label_match_eq_score = 3
let label_match_eq_named_bonus = 1
(*
let label_match_eq_named_score = label_match_eq_score + label_match_eq_named_bonus
*)
let get_label_match_eq_named_score nd = label_match_eq_score + label_match_eq_named_bonus
(* !!!
  let a =
    try
      String.length nd#data#get_name
    with
      Not_found -> 0
  in
  label_match_eq_score + label_match_eq_named_bonus * a
*)

(* *)

type 'node_t label_match_result = { lm_score   : float;
                                    lm_matches : ('node_t * 'node_t) list;
                                    lm_nmcount : int;
                                  }

exception Found

let compare_pair (x0, y0) (x1, y1) =
  let c = Stdlib.compare x0 x1 in
  if c = 0 then
    Stdlib.compare y0 y1
  else
    c

let estimate_cost_of_move tree1 tree2 uidmapping nd1 nd2 = (* cost = number of accompanying nodes *)
  let lgi2 = (tree2#initial_leftmost nd2)#gindex in
  let gi2 = nd2#gindex in
  let count = ref 0 in
(*  let mapped = ref [] in *)
  tree1#fast_scan_whole_initial_subtree nd1
    (fun n ->
      try
        let n' = tree2#search_node_by_uid (uidmapping#find n#uid) in
        let gi' = n'#gindex in
        if lgi2 <= gi' && gi' <= gi2 then begin
(*        mapped := (n#uid, n'#uid) :: !mapped; *)
          incr count
        end
      with
        Not_found -> ()
    );
(*
    DEBUG_MSG "%a -> %a (%d) [%s]" nups nd1 nups nd2 !count
      (Xlist.to_string (fun (u1, u2) -> sprintf "%a-%a" ups u1 ups u2) ";" !mapped);
*)
  DEBUG_MSG "%a -> %a (%d)" nps nd1 nps nd2 !count;

  !count

let get_bn = get_p_ancestor (fun x -> x#data#is_boundary)
let _is_map uidmapping n1 n2 =
  try
    uidmapping#find n1#uid = n2#uid
  with
    Not_found -> false



class ['node_t] multiple_node_matches (label_to_string : Obj.t -> string) = object
  val tbl = (Hashtbl.create 0: (Obj.t, 'node_t list * 'node_t list) Hashtbl.t)

  method label_to_string _lab = label_to_string _lab

  method add _lab (nds1, nds2) =
    DEBUG_MSG "%s [%a]-[%a]" (label_to_string _lab) nsps nds1 nsps nds2;

    if not (Hashtbl.mem tbl _lab) then
      Hashtbl.add tbl _lab (nds1, nds2)
    else
      WARN_MSG "key(_label) collision?: %s"
        (label_to_string (Obj.obj _lab))

  method find _lab =
    Hashtbl.find tbl _lab

  method replace _lab (nds1, nds2) =
    Hashtbl.replace tbl _lab (nds1, nds2)

  method remove _lab =
    Hashtbl.remove tbl _lab

  method iter (f : Obj.t * 'node_t list * 'node_t list -> unit) =
    let list =
      Hashtbl.fold
        (fun _lab (nds1, nds2) l ->
          (_lab, nds1, nds2)::l
        ) tbl []
    in
    let cmp (_, nds11, nds21) (_, nds12, nds22) =
      let gip1 = (List.hd nds11)#gindex, (List.hd nds21)#gindex in
      let gip2 = (List.hd nds12)#gindex, (List.hd nds22)#gindex in
      Stdlib.compare gip1 gip2

    in
    List.iter f (List.fast_sort cmp list)

end (* of class multiple_node_matches *)



type 'node_t subroot_members_t = 'node_t * 'node_t list

type 'node_t multiple_subtree_match_tbl_t =
    (Digest.t,                          (* digest *)
     'node_t subroot_members_t list *   (* root * members *)
       'node_t subroot_members_t list * (* root * members *)
       int                              (* size *)
    ) Hashtbl.t


class ['node_t] multiple_subtree_matches options = object

  val tbl = (Hashtbl.create 0: 'node_t multiple_subtree_match_tbl_t)
  val mutable match_thresh = 0

  method add d (ndmems1, ndmems2) =
    if not (Hashtbl.mem tbl d) then
      try
        let sz = List.length (snd (List.hd ndmems1)) in
        if sz > options#subtree_match_threshold then
          Hashtbl.add tbl d (ndmems1, ndmems2, sz)
      with
        Failure _(*"hd"*) -> assert false
    else
      WARN_MSG "digest collision?: %s" d

  method mem = Hashtbl.mem tbl

  method find = Hashtbl.find tbl

  method remove d =
    DEBUG_MSG "removing %s" (try Digest.to_hex d with _ -> d);
    Hashtbl.remove tbl d

  method iter
      (f : Digest.t * 'node_t subroot_members_t list * 'node_t subroot_members_t list * int -> unit)
      =
    let list =
      Hashtbl.fold
        (fun d (ndmems1, ndmems2, sz) l ->
          (d, ndmems1, ndmems2, sz)::l
        ) tbl []
    in
    let cmp (_, ndmems11, ndmems21, sz1) (_, ndmems12, ndmems22, sz2) =
      let c1 = Stdlib.compare sz2 sz1 in
      if c1 = 0 then
        let gip1 = (fst (List.hd ndmems11))#gindex, (fst (List.hd ndmems21))#gindex in
        let gip2 = (fst (List.hd ndmems12))#gindex, (fst (List.hd ndmems22))#gindex in
        Stdlib.compare gip1 gip2
      else
        c1
    in
    List.iter f (List.fast_sort cmp list)

  method align (uidmapping : 'node_t UIDmapping.c) =

    let get_digest x =
      match x#data#_digest with
      | Some d -> d
      | None -> raise Not_found
    in

    let inner_ds = Xset.create 0 in
    Hashtbl.iter
      (fun d ->
        function
          | ((_, mems)::_, _, _) -> begin
              List.iter
                (fun nd ->
                  try
                    let d0 = get_digest nd in
                    if d0 <> d then
                      if Hashtbl.mem tbl d0 then
                        Xset.add inner_ds d0
                  with
                    Not_found -> ()
                ) mems
          end
          | _ -> ()
      ) tbl;
    let is_inner d = Xset.mem inner_ds d in

    let ntbl = Hashtbl.create 0 in (* root node -> subtree members *)
    let dtbl1 = Hashtbl.create 0 in (* root node -> digest *)
    let dtbl2 = Hashtbl.create 0 in (* root node -> digest *)
    let bns1 = Xset.create 0 in
    let bns2 = Xset.create 0 in

    let add_bn bns rt =
      try
        Xset.add bns (get_bn rt)
      with _ -> ()
    in
    Hashtbl.iter
      (fun d (ndmems1, ndmems2, _) ->
        if not (is_inner d) then begin
          List.iter
            (fun (rt, nds) ->
              if List.for_all (fun n -> not (uidmapping#mem_dom n#uid)) nds then begin
                Hashtbl.add dtbl1 rt d;
                Hashtbl.add ntbl rt nds;
                add_bn bns1 rt
              end
            ) ndmems1;
          List.iter
            (fun (rt, nds) ->
              if List.for_all (fun n -> not (uidmapping#mem_cod n#uid)) nds then begin
                Hashtbl.add dtbl2 rt d;
                Hashtbl.add ntbl rt nds;
                add_bn bns1 rt
              end
            ) ndmems2
        end
      ) tbl;

    let is_map = _is_map uidmapping in
    let gen_dummy_digest =
      let count = ref 0 in
      fun () ->
        let d = (string_of_int !count : Xhash.t) in
        incr count;
        d
    in
    Xset.iter
      (fun bn1 ->
        Xset.iter
          (fun bn2 ->
              if is_map bn1 bn2 then begin
                if not (Hashtbl.mem dtbl1 bn1) && not (Hashtbl.mem dtbl2 bn2) then begin
                  let d = gen_dummy_digest() in
                  DEBUG_MSG "!!!!! boundary: %s [%s] - %s [%s] d=%s"
                    bn1#data#to_string (Loc.to_string bn1#data#src_loc)
                    bn2#data#to_string (Loc.to_string bn2#data#src_loc)
                    d;
                  Hashtbl.add dtbl1 bn1 d;
                  Hashtbl.add dtbl2 bn2 d;
                end
              end
          ) bns2
      ) bns1;

    let roots1 = Hashtbl.fold (fun nd _ l -> nd::l) dtbl1 [] in
    let roots2 = Hashtbl.fold (fun nd _ l -> nd::l) dtbl2 [] in

    if roots1 = [] || roots2 = [] then
      []
    else

    let cmp nd1 nd2 = Stdlib.compare nd1#gindex nd2#gindex in

    let roota1 = Array.of_list (List.fast_sort cmp roots1) in
    let roota2 = Array.of_list (List.fast_sort cmp roots2) in

    let da1 = Array.map (fun nd -> Hashtbl.find dtbl1 nd) roota1 in
    let da2 = Array.map (fun nd -> Hashtbl.find dtbl2 nd) roota2 in

    let matched, relabeled, _deleted, _inserted = Adiff.adiff da1 da2 in


    BEGIN_DEBUG
      DEBUG_MSG "num of relabels: %d" (List.length relabeled);
      List.iter
        (fun (i, j) ->
          DEBUG_MSG "[relabel] %a - %a" nps roota1.(i) nps roota2.(j)
        ) relabeled
    END_DEBUG;

    let d, i = List.split relabeled in

    let deleted = List.fast_sort Stdlib.compare (_deleted @ d) in
    let inserted = List.fast_sort Stdlib.compare (_inserted @ i) in

    BEGIN_DEBUG
      DEBUG_MSG "alignment:";
      let cur2 = ref 0 in
      let to_hex x = try Digest.to_hex x with _ -> x in
      let n2s n = UID.to_string n#uid in
      let n2cat n = n#data#get_category in
      let n2loc n = Loc.to_string n#data#src_loc in
      Array.iteri
        (fun i nd ->
          if List.mem i deleted then begin
            DEBUG_MSG "(%s) %7s   %7s %s [%s]" (to_hex da1.(i)) (n2s nd) "" (n2cat nd) (n2loc nd)
          end
          else if List.mem_assoc i matched then begin
            let j = List.assoc i matched in

            if !cur2 < j then begin
              for p = !cur2 to j - 1 do
                if List.mem p inserted then begin
                  let ra2p = roota2.(p) in
                  DEBUG_MSG "(%s) %7s   %7s %s [%s]" (to_hex da2.(p)) "" (n2s ra2p) (n2cat ra2p) (n2loc ra2p)
                end
                else
                  DEBUG_MSG "dangling index (right): %d" p
              done
            end;
            let ra2j = roota2.(j) in
            DEBUG_MSG "(%s) %7s - %7s %s [%s]-[%s]" (to_hex da1.(i)) (n2s nd) (n2s ra2j) (n2cat ra2j) (n2loc nd) (n2loc ra2j);
            cur2 := j + 1
          end
          else
            DEBUG_MSG "dangling index (left): %d" i

        ) roota1;

      let sz2 = Array.length roota2 in
      if !cur2 < sz2 then
        for p = !cur2 to sz2 - 1 do
          if List.mem p inserted then begin
            let ra2p = roota2.(p) in
            DEBUG_MSG "(%s) %7s   %7s %s [%s]" (to_hex da2.(p)) "" (n2s ra2p) (n2cat ra2p) (n2loc ra2p)
          end
          else
            DEBUG_MSG "dangling index (right): %d" p
        done

    END_DEBUG;

    let count = ref 0 in

    let added_pairs = ref [] in

    List.iter
      (fun (i, j) ->
        let pi, pj = i - 1, j - 1 in
        let ni, nj = i + 1, j + 1 in
        let d = da1.(i) in
        let nd1, nd2 = roota1.(i), roota2.(j) in
        if
          (if List.mem pi deleted then d = da1.(pi) else false) ||
          (if List.mem ni deleted then d = da1.(ni) else false) ||
          (if List.mem pj inserted then d = da2.(pj) else false) ||
          (if List.mem nj inserted then d = da2.(nj) else false)
        then begin (* suspicious *)

          DEBUG_MSG "suspicious match: %a-%a" nps nd1 nps nd2
        end
        else
          try
            let nds1 = Hashtbl.find ntbl nd1 in
            let nds2 = Hashtbl.find ntbl nd2 in
            incr count;

            List.iter2
              (fun n1 n2 ->
                let u1, u2 = n1#uid, n2#uid in
                let _ = uidmapping#add_settled ~stable:true u1 u2 in
                added_pairs := (u1, u2) :: !added_pairs
              ) nds1 nds2;

            uidmapping#add_settled_roots nd1#uid nd2#uid
          with
            Not_found -> ()
      ) matched;

    DEBUG_MSG "%d pairs (roots) added" !count;

    !added_pairs

    (* end of method align *)


end (* of class multiple_subtree_matches *)


class upairs = object (self)
  val pairs = (Xset.create 0 : (UID.t * UID.t) Xset.t)

  method mem uid1 uid2 = Xset.mem pairs (uid1, uid2)

  method add uid1 uid2 = Xset.add pairs (uid1, uid2)

  method iter f =
    Xset.iter (fun (uid1, uid2) -> f uid1 uid2) pairs

end


exception Elaboration_impossible

class ['node_t, 'tree_t] c
    options
    ?(has_elaborate_edits=false)
    (tree1 : 'tree_t) (tree2 : 'tree_t)

    = object (self)

  val mutable use_adjacency_cache = true
  val mutable use_similarity_cache = true
  val mutable use_mapping_comparison_cache = true

  val ref_upairs = new upairs
  method ref_upairs = ref_upairs

(* *)
  val permutation_hub_tbl = Hashtbl.create 0
  method add_permutation_hub_cand (n1 : 'node_t) (n2 : 'node_t) (lab : string) =
    DEBUG_MSG "%a-%a %s" nugps n1 nugps n2 lab;
    let key = n1, n2, lab in
    try
      let c, lgi1, gi1, lgi2, gi2 = Hashtbl.find permutation_hub_tbl key in
      Hashtbl.replace permutation_hub_tbl key (c+1, lgi1, gi1, lgi2, gi2)
    with
      Not_found -> Hashtbl.add permutation_hub_tbl key (1, GI.dummy, GI.dummy, GI.dummy, GI.dummy)

  method finalize_permutation_hub_tbl() =
    let to_be_removed = ref [] in
    Hashtbl.iter
      (fun ((n1, n2, lab) as key) (c, _, _, _, _) ->
        DEBUG_MSG "%a-%a (%s): %d" nugps n1 nugps n2 lab c;
        if c < permutation_hub_count_thresh then begin
          to_be_removed := key :: !to_be_removed
        end
        else
          Hashtbl.replace permutation_hub_tbl key
            (c, (tree1#initial_leftmost n1)#gindex, n1#gindex, (tree2#initial_leftmost n2)#gindex, n2#gindex)
      ) permutation_hub_tbl;
    List.iter (Hashtbl.remove permutation_hub_tbl) !to_be_removed;

  method under_permutation_hub (n1 : 'node_t) (n2 : 'node_t) =
    let g1 = n1#gindex in
    let g2 = n2#gindex in
    let b =
    try
      Hashtbl.iter
        (fun (r1, r2, _) (_, lgi1, gi1, lgi2, gi2) ->
          if lgi1 <= g1 && g1 < gi1 && lgi2 <= g2 && g2 < gi2 then begin
            DEBUG_MSG "found: %a-%a" nugps r1 nugps r2;
            raise Exit
          end
        ) permutation_hub_tbl;
      false
    with Exit -> true
    in
    DEBUG_MSG "%a-%a -> %B" nps n1 nps n2 b;
    b

  method under_permutation_hub1 (n1 : 'node_t) =
    let g1 = n1#gindex in
    let b =
    try
      Hashtbl.iter
        (fun (r1, r2, _) (_, lgi1, gi1, lgi2, gi2) ->
          if lgi1 <= g1 && g1 < gi1 then begin
            DEBUG_MSG "found: %a-%a" nps r1 nps r2;
            raise Exit
          end
        ) permutation_hub_tbl;
      false
    with Exit -> true
    in
    DEBUG_MSG "%a -> %B" nps n1 b;
    b

  method under_permutation_hub2 (n2 : 'node_t) =
    let g2 = n2#gindex in
    let b =
    try
      Hashtbl.iter
        (fun (r1, r2, _) (_, lgi1, gi1, lgi2, gi2) ->
          if lgi2 <= g2 && g2 < gi2 then begin
            DEBUG_MSG "found: %a-%a" nps r1 nps r2;
            raise Exit
          end
        ) permutation_hub_tbl;
      false
    with Exit -> true
    in
    DEBUG_MSG "%a -> %B" nps n2 b;
    b

  val mutable cache_path = ""

  val mutable is_possible_rename = ((fun ?(strict=false) n1 n2 -> true) : ?strict:bool -> 'node_t -> 'node_t -> bool)
  method is_possible_rename = is_possible_rename
  method set_is_possible_rename f = is_possible_rename <- f

  val bad_pairs = (Xset.create 0 : (UID.t * UID.t) Xset.t)
  method bad_pairs = bad_pairs
  method add_bad_pair u1 u2 = Xset.add bad_pairs (u1, u2)

  val subtree_matches = (Xset.create 0 : ('node_t * 'node_t * int) Xset.t)
  method subtree_matches = subtree_matches
  method add_subtree_match ((nd, _, _) as elem) =
    let lgi, gi = (tree1#initial_leftmost nd)#gindex, nd#gindex in
    let to_be_removed = ref [] in
    try
      Xset.iter
        (fun ((n0, _, _) as e) ->
          let lgi0, gi0 = (tree1#initial_leftmost n0)#gindex, n0#gindex in

          if lgi0 <= lgi && gi < gi0 then
            raise Exit
          else if lgi <= lgi0 && gi0 < gi then
            to_be_removed := e :: !to_be_removed

        ) subtree_matches;
      List.iter (Xset.remove subtree_matches) !to_be_removed;
      Xset.add subtree_matches elem
    with
      Exit -> ()

  val mutable multiple_subtree_matches = (None : 'node_t multiple_subtree_matches option)
  method set_multiple_subtree_matches msm = multiple_subtree_matches <- Some msm
  method multiple_subtree_matches =
    match multiple_subtree_matches with
    | Some msm -> msm
    | None -> raise Not_found

  val mutable multiple_node_matches = (None : 'node_t multiple_node_matches option)
  method set_multiple_node_matches mnm = multiple_node_matches <- Some mnm
  method multiple_node_matches =
    match multiple_node_matches with
    | Some mnm -> mnm
    | None -> raise Not_found

  val adjacency_cache = (Hashtbl.create 0 : (UID.t * UID.t, float * ('node_t * 'node_t) list) Hashtbl.t)
  val mutable adjacency_cache_hit_count = 0

  val mapping_comparison_cache =
    (Hashtbl.create 0 : (bool * UID.t * UID.t * UID.t * UID.t, bool * int option * float option) Hashtbl.t)
  val mutable mapping_comparison_cache_hit_count = 0

  val similarity_cache = (Hashtbl.create 0 : (UID.t * UID.t, float) Hashtbl.t)
  val mutable similarity_cache_hit_count = 0

  val use_tbl1 = Hashtbl.create 0 (* bid -> node list *)
  val use_tbl2 = Hashtbl.create 0 (* bid -> node list *)

  method private use_tbl_add tbl b n =
    try
      let l = Hashtbl.find tbl b in
      Hashtbl.replace tbl b (n::l)
    with
      Not_found -> Hashtbl.add tbl b [n]

  method get_use1 = Hashtbl.find use_tbl1
  method get_use2 = Hashtbl.find use_tbl2

  initializer
    if has_elaborate_edits then begin
      let scan tbl nd =
        let b = nd#data#binding in
        if B.is_use b then
          try
            let bid = B.get_bid b in
            self#use_tbl_add tbl bid nd
          with
            Not_found -> ()
      in
      tree1#scan_whole_initial (scan use_tbl1);
      tree2#scan_whole_initial (scan use_tbl2)
    end


  method tree1 = tree1
  method tree2 = tree2

  method use_adjacency_cache = use_adjacency_cache
  method size_of_adjacency_cache = Hashtbl.length adjacency_cache
  method adjacency_cache_hit_count = adjacency_cache_hit_count

  method use_similarity_cache = use_similarity_cache
  method size_of_similarity_cache = Hashtbl.length similarity_cache
  method similarity_cache_hit_count = similarity_cache_hit_count

  method use_mapping_comparison_cache = use_mapping_comparison_cache
  method set_use_mapping_comparison_cache = use_mapping_comparison_cache <- true
  method clear_use_mapping_comparison_cache = use_mapping_comparison_cache <- false

  method size_of_mapping_comparison_cache = Hashtbl.length mapping_comparison_cache
  method mapping_comparison_cache_hit_count = mapping_comparison_cache_hit_count

  method cache_path = cache_path
  method set_cache_path p = cache_path <- p

  method has_non_trivial_value (nd : 'node_t) = nd#data#has_non_trivial_value
  method has_trivial_value (nd : 'node_t) = nd#data#has_value && not nd#data#has_non_trivial_value

  method has_weak_non_trivial_value =
    match multiple_node_matches with
    | Some mnm -> begin
        fun (nd : 'node_t) ->
          let b =
            nd#data#has_non_trivial_value ||
            nd#data#has_value &&
            match mnm#find nd#data#_label with
            | [_], [_] -> true
            | _ -> false
          in
          DEBUG_MSG "%s -> %B" (mnm#label_to_string nd#data#_label) b;
          (*if b then
            Printf.printf "! has_non_trivial_value: %s\n"
              (mnm#label_to_string nd#data#_label);*)
          b
    end
    | None -> fun nd -> nd#data#has_non_trivial_value

  method has_weak_trivial_value =
    match multiple_node_matches with
    | Some mnm -> begin
        fun (nd : 'node_t) ->
          let b =
            nd#data#has_value && not nd#data#has_non_trivial_value ||
            match mnm#find nd#data#_label with
            | _::_::_, _::_::_ -> true
            | _ -> false
          in
          DEBUG_MSG "%s -> %B" (mnm#label_to_string nd#data#_label) b;
          (*if b then
            Printf.printf "! has_trivial_value: %s\n"
              (mnm#label_to_string nd#data#_label);*)
          b
    end
    | None -> fun nd -> nd#data#has_value && not nd#data#has_non_trivial_value

  method eval_label_match ?(bonus_named=false) nd1 nd2 =
    let v =
      if nd1#data#eq nd2#data then begin

          if bonus_named then
            if nd1#data#is_named_orig then
              get_label_match_eq_named_score nd1
            else
              label_match_eq_score
          else
            label_match_eq_score

      end
      else if nd1#data#_anonymized_label = nd2#data#_anonymized_label then
        2
      else if nd1#data#_anonymized2_label = nd2#data#_anonymized2_label then begin
        try (* ADOPTED *)
          if nd1#data#get_name = nd2#data#get_name then
            2
          else
            1
        with
          Not_found -> 1
      end
      else
        0
    in
(*
  BEGIN_DEBUG
  DEBUG_MSG "%a-%a -> %d" nps nd1 nps nd2 v;
(*
  DEBUG_MSG "anonymity level(0): (%s-%s)"
  nd1#data#label nd2#data#label;
  DEBUG_MSG "anonymity level(1): (%s-%s)"
  nd1#data#anonymized_label nd2#data#anonymized_label;
  DEBUG_MSG "anonymity level(2): (%s-%s)"
  nd1#data#anonymized2_label nd2#data#anonymized2_label;
 *)
  END_DEBUG;
 *)

    v


  method eval_label_match_list ?(bonus_named=false) ?(flat=false) nds1 nds2 =
    let a1 = Array.of_list nds1 in
    let a2 = Array.of_list nds2 in

    let lab_a1 = Array.map (fun n -> n#data#_label) a1 in
    let lab_a2 = Array.map (fun n -> n#data#_label) a2 in

    let mat, _, _, _ = Adiff.adiff lab_a1 lab_a2 in
    let mat1, mat2 = List.split mat in

    let anon_a1 =
      Array.mapi
        (fun i n -> if List.mem i mat1 then n#data#_label else n#data#_anonymized2_label)
        a1
    in
    let anon_a2 =
      Array.mapi
        (fun i n -> if List.mem i mat2 then n#data#_label else n#data#_anonymized2_label)
        a2
    in

    let mat, _, _, _ = Adiff.adiff anon_a1 anon_a2 in

    let score = ref 0.0 in
    let matches = ref [] in
    let nmcount = ref 0 in

    List.iter
      (fun (i, j) ->
        let n1, n2 = a1.(i), a2.(j) in
        let lm =
          if flat then begin
            if n1#data#eq n2#data then
              1.0
            else
              0.7 (* subtree_similarity_thresh *)
          end
          else
            float (self#eval_label_match ~bonus_named n1 n2)
        in

        score := !score +. lm;

        if n1#data#is_named_orig && n1#data#eq n2#data then
          incr nmcount;
(* !!!
          nmcount := !nmcount + (try String.length n1#data#get_name with Not_found -> 0);
*)
        if options#use_adjacency_matches_flag then
          if n1#data#eq n2#data then
            matches := (n1, n2) :: !matches

      ) mat;
(*
      DEBUG_MSG "eval_label_match_list[bonus_named:%B,flat:%B]: score=%f nmcount=%d"
        bonus_named flat !score !nmcount;
*)
    { lm_score=(!score);
      lm_matches=(!matches);
      lm_nmcount=(!nmcount);
    }

  method estimate_cost_of_move (uidmapping : 'node_t UIDmapping.c) nd1 nd2 = (* cost = number of accompanying nodes *)
    estimate_cost_of_move tree1 tree2 uidmapping nd1 nd2

  method find_ancestor_pairs_of_same_category rev_flag nd1 nd2 =
    let id x = x in
    let list_rev0 = if rev_flag then id else List.rev in
    let list_rev1 = if rev_flag then List.rev else id in

    let l1 = (tree1#initial_ancestor_nodeposs nd1) in
    let l2 = (tree2#initial_ancestor_nodeposs nd2) in

    let filt = List.filter (fun (n, p) -> not n#initial_children.(p)#data#is_order_insensitive) in
    let l1 = filt l1 in
    let l2 = filt l2 in

    let a1 = Array.of_list (list_rev0 l1) in
    let a2 = Array.of_list (list_rev0 l2) in

    let anon1 = Array.map (fun (n, p) -> n#data#_anonymized3_label) a1 in
    let anon2 = Array.map (fun (n, p) -> n#data#_anonymized3_label) a2 in
(*
      DEBUG_MSG "anon1: [%s]" (Xarray.to_string (fun (n, _) -> n#data#anonymized3_label) ";" a1);
      DEBUG_MSG "anon2: [%s]" (Xarray.to_string (fun (n, _) -> n#data#anonymized3_label) ";" a2);
*)
    let mat, _, _, _ = Adiff.adiff anon1 anon2 in

    let sorted =
      list_rev1
        (List.fast_sort (fun (i0, _) (i1, _) -> Stdlib.compare i0 i1) mat)
    in
(*
    let get_snd a i =
      try
        let n, _ = a.(i-1) in
        Some n
      with
        _ -> None
    in
*)
    List.map
      (fun (idx1, idx2) ->
        let nd1, pos1 = a1.(idx1) in
        let nd2, pos2 = a2.(idx2) in
(*
        let snd1 = get_snd a1 idx1 in
        let snd2 = get_snd a2 idx2 in
*)
        idx1, idx2, nd1, nd2, pos1, pos2
      ) sorted
(* end of method find_ancestor_pairs_of_same_category *)


  method get_ancestors_similarity nd1 nd2 =
    let _ancs1 = List.rev (tree1#initial_ancestor_nodes nd1) in
    let _ancs2 = List.rev (tree2#initial_ancestor_nodes nd2) in
    (*let flag =
      match _ancs1, _ancs2 with
      | a1::b1::_, a2::b2::_ -> a1#data#is_sequence && b1#data#is_boundary || a2#data#is_sequence && b2#data#is_boundary
      | _ -> false
    in*)
    let filt ancs =
      let _, l =
        List.fold_left
          (fun (skip, l) n ->
            if skip then
              (skip, l)
            else if n#data#is_boundary then
              (true, (*if flag then l @ [n] else *)l)
            else
              (skip, l @ [n])

          ) (false, []) ancs
      in
      l
    in
    let ancs1 = filt _ancs1 in
    let ancs2 = filt _ancs2 in
    if ancs1 = [] && ancs2 = [] then
      1.0
    else
      let res = self#eval_label_match_list ~flat:true ancs1 ancs2 in
      (res.lm_score *. 2.0) /. (float ((List.length ancs1) + (List.length ancs2)))



  method get_similarity_score
      ?(ignore_cache=false)
      ?(bonus_named=false)
      ?(flat=true)
      rt1 rt2 = (* similarity [0.0,1.0] *)
    let sim =
      try
        if not use_similarity_cache || ignore_cache then
          raise Not_found;

        let score = Hashtbl.find similarity_cache (rt1#uid, rt2#uid) in

        similarity_cache_hit_count <- similarity_cache_hit_count + 1;

        score

      with
        Not_found ->

          if rt1#data#subtree_equals rt2#data then begin
(*
            let s =
              let c = ref 0 in
              let m = ref 0 in
              tree1#fast_scan_whole_initial_subtree rt1
                (fun n ->
                  incr c;
                  if n#data#is_named_orig then
                    m := !m + label_match_eq_named_score
                  else
                    m := !m + label_match_eq_score
                );
              (float !m) /. (float !c)
            in
*)
            let s = 1.0 in

            DEBUG_MSG "[subtree match] %a-%a -> %f" nups rt1 nups rt2 s;

            if use_similarity_cache then
              Hashtbl.replace similarity_cache (rt1#uid, rt2#uid) s;
            s
          end
          else begin
            let nds1 = ref [] in
            let nds2 = ref [] in
            tree1#fast_scan_whole_initial_subtree rt1 (fun n -> nds1 := n :: !nds1);
            tree2#fast_scan_whole_initial_subtree rt2 (fun n -> nds2 := n :: !nds2);

            match !nds1, !nds2 with
            | [], _ | _, [] -> 0.0
            | _ ->
                let lmres =
                  self#eval_label_match_list (* ~bonus_named:true *)~bonus_named ~flat !nds1 !nds2
                in
                let s =
                  (lmres.lm_score *. 2.0) /. (float ((List.length !nds1) + (List.length !nds2)))
                in

                DEBUG_MSG "%a-%a -> %f" nups rt1 nups rt2 s;

                if use_similarity_cache then
                  Hashtbl.replace similarity_cache (rt1#uid, rt2#uid) s;
                s
          end
    in
    sim

  (* adjacency : similarity of the context *)
  method _get_adjacency_score nd1 nd2 =
    let uid1 = nd1#uid in
    let uid2 = nd2#uid in

    DEBUG_MSG "evaluating %a-%a..." ups uid1 ups uid2;

    try
      if not use_adjacency_cache then
        raise Not_found;

      let score, ref_pairs = Hashtbl.find adjacency_cache (uid1, uid2) in

      adjacency_cache_hit_count <- adjacency_cache_hit_count + 1;

      DEBUG_MSG "score: %a-%a -> %f" ups uid1 ups uid2 score;

      score, ref_pairs

    with
      Not_found ->

        let score = ref 0.0 in

        let ref_pairs = ref [] in

        let _incr_score
            ?(weight=1.0)
            ?(extra_denom=0)
            ?(bonus_named=false)
            ?(bonus_named_more=false)
            nds1 nds2
            =

          DEBUG_MSG "weight=%f extra_denom=%d bonus_named=%B bonus_named_more=%B"
            weight extra_denom bonus_named bonus_named_more;

          let len1 = List.length nds1 in
          let len2 = List.length nds2 in

          DEBUG_MSG "nds1=[%s] (%d)" (Xlist.to_string (fun n -> n#data#label) ";" nds1) len1;
          DEBUG_MSG "nds2=[%s] (%d)" (Xlist.to_string (fun n -> n#data#label) ";" nds2) len2;

          let lmres = self#eval_label_match_list ~bonus_named nds1 nds2 in

          DEBUG_MSG "score=%f nmcount=%d" lmres.lm_score lmres.lm_nmcount;
          BEGIN_DEBUG
            if lmres.lm_matches <> [] then begin
              DEBUG_MSG "matches:";
              List.iter
                (fun (n1, n2) ->
                  DEBUG_MSG "  %a -- %a (%a-%a)" labps n1 labps n2 nups n1 nups n2
                ) lmres.lm_matches
            end
          END_DEBUG;

          ref_pairs := lmres.lm_matches @ !ref_pairs;
          let s =
            (lmres.lm_score *. 2.0) /. (float (len1 + len2 + extra_denom))
          in
          let s' =
            if bonus_named_more then
              s +. (float lmres.lm_nmcount)
            else
              s
          in
          s' *. weight
        in

        let incr_score
            ?(weight=1.0)
            ?(extra_denom=0)
            ?(bonus_named=false)
            ?(bonus_named_more=false)
            nds1 nds2
            =
          let s = _incr_score ~weight ~extra_denom ~bonus_named ~bonus_named_more nds1 nds2 in
          score := !score +. s
        in

        let get_rightmost_descendants nd =
          let res = ref [] in
          let rec doit n =
            try
              let next = n#initial_children.(n#initial_nchildren - 1) in
              res := next::!res;
              doit next
            with
              Invalid_argument _ -> ()
          in
          doit nd;

          DEBUG_MSG "get_rightmost_descendants: %a -> [%a]" nups nd nsps !res;

          !res
        in

        let get_leftmost_descendants nd =
          let res = ref [] in
          let rec doit n =
            try
              let next = n#initial_children.(0) in
              res := next::!res;
              doit next
            with
              Invalid_argument _ -> ()
          in
          doit nd;

          DEBUG_MSG "get_leftmost_descendants: %a -> [%a]" nups nd nsps !res;

          !res
        in

        let get_descendants offset nd =
          DEBUG_MSG "offset=%d nd=%a" offset nups nd;
          if offset < 0 then
            get_leftmost_descendants nd
          else if offset > 0 then
            get_rightmost_descendants nd
          else
            []
        in

        let rt1, rt2 = tree1#root, tree2#root in

(*
  let weq nd1 nd2 = nd1#data#anonymized2_label = nd2#data#anonymized2_label in (* to be tuned? *)
 *)
        let weq nd1 nd2 = true in

(*
        let get_n_skipped tree snd nd =
          match snd with
          | None -> 0
          | Some rt ->
              tree1#fast_size_of_initial_cluster (rt, [nd])
        in
*)
        let find_anchor nd1 nd2 =
          let rev_flag =
            try
              let pnd1 = nd1#initial_parent in
              let pnd2 = nd2#initial_parent in
              nd1#initial_nchildren = 0 && nd2#initial_nchildren = 0 &&
              not (nd1#data#eq nd2#data) &&
              pnd1#data#is_named && pnd2#data#is_named &&
              pnd1#initial_nchildren == 1 && pnd2#initial_nchildren == 1 &&
              not pnd1#data#is_named_orig && not pnd2#data#is_named_orig &&
              not
                (
                 let ppnd1 = pnd1#initial_parent in
                 let ppnd2 = pnd2#initial_parent in
                 pnd1#data#eq pnd2#data &&
                 ppnd1#data#is_named_orig && ppnd2#data#is_named_orig &&
                 ppnd1#initial_nchildren == 1 && ppnd2#initial_nchildren == 1 &&
                 ppnd1#data#get_orig_name = ppnd2#data#get_orig_name
                )
            with
              _ -> false
          in
          if rev_flag then begin
            DEBUG_MSG "%a-%a (%a-%a)" nups nd1 nups nd2 locps nd1 locps nd2;
            (*Printf.printf "! rev_flag: %s\n" nd1#to_string*)
          end;

          let rec doit = function
            | [] -> raise Not_found
            | (idx1, idx2, anc1, anc2, ipos1, ipos2)::rest ->
                DEBUG_MSG "anc1=%a anc2=%a" nups anc1 nups anc2;
                if anc1 == rt1 || anc2 == rt2 || anc1#data#is_boundary || anc2#data#is_boundary then
                  raise Not_found

                else begin
                  if rev_flag && anc1#initial_nchildren = 1 && anc2#initial_nchildren = 1 then
                    doit rest
                  else if weq anc1 anc2 then begin
                    let d = idx1 + idx2
(*                    (get_n_skipped tree1 snd1 nd1) + (get_n_skipped tree2 snd2 nd2) *)
                    in
                    let ichildren1 = anc1#initial_children in
                    let ichildren2 = anc2#initial_children in
                    let len1 = Array.length ichildren1 in
                    let len2 = Array.length ichildren2 in
                    let left = ipos1 >= 1 (* && *) || ipos2 >= 1 in
                    let right = ipos1 <= len1 - 2 (* && *) || ipos2 <= len2 - 2 in

                    (*if
                      (not left || not right) &&
                      match rest with
                      | (_, _, a1, a2, ip1, ip2)::_ -> begin
                          a1 != rt1 && a2 != rt2 &&
                          not a1#data#is_boundary && not a2#data#is_boundary &&
                          not a1#initial_children.(ip1)#data#is_order_insensitive &&
                          not a2#initial_children.(ip2)#data#is_order_insensitive
                      end
                      | _ -> false
                    then
                      doit rest
                    else*)
                      (left, right, anc1, anc2, ipos1, ipos2, d)

                  end (* if weq anc1 anc2 *)
                  else
                    doit rest

                end (* if not (anc1 == rt1... *)
          in
          doit (self#find_ancestor_pairs_of_same_category rev_flag nd1 nd2)
        in


        let _comp_score
            ?(weight=1.0)
            ?(bonus_named=false)
            ?(bonus_named_more=false)
            ?(extra_denom=0)
            n1 n2
            =
          if n1#data#subtree_equals n2#data then begin
            let sz = tree1#whole_initial_subtree_size n1 in
            let nmcount = ref 0 in
            let s =
              if bonus_named then
                let m = ref 0 in
                tree1#fast_scan_whole_initial_subtree n1
                  (fun n ->
                    if n#data#is_named_orig then begin
                      m := !m + (get_label_match_eq_named_score n);
                      incr nmcount
                    end
                    else
                      m := !m + label_match_eq_score
                  );
                (float (2 * !m)) /. (float (2 * sz + extra_denom))

              else
                (float label_match_eq_score) *. ((float (2 * sz)) /. (float (2 * sz + extra_denom)))
            in
            let s' =
              if bonus_named_more then
                s +. (float !nmcount)
              else
                s
            in
            s' *. weight
          end
          else
            raise Not_found
        in

        let comp_score ?(weight=1.0) ?(bonus_named=false) ?(extra_denom=0) n1 n2 f =
          try
            let s = _comp_score ~weight ~bonus_named ~extra_denom n1 n2 in
            score := !score +. s
          with
            Not_found ->
              f ()
        in

        let score_lr ?(both=false) ?(extra=true) offset anc1 anc2 ipos1 ipos2 d = (* offset: 1 or -1 *)
          DEBUG_MSG "offset=%d, ipos1=%d, ipos2=%d, d=%d" offset ipos1 ipos2 d;
          let ichildren1 = anc1#initial_children in
          let ichildren2 = anc2#initial_children in
          let offset' = -offset in

          begin
            try
              let lr1 = ichildren1.(ipos1 + offset) in
              let lr2 = ichildren2.(ipos2 + offset) in
              DEBUG_MSG "lr1=%a lr2=%a" nups lr1 nups lr2;
              comp_score ~extra_denom:d ~bonus_named:true lr1 lr2
                (fun () ->
                  let lr_list1 = (get_descendants offset' lr1) @ [lr1] in
                  DEBUG_MSG "lr_list1=[%s]"
                    (Xlist.to_string (fun n -> n#data#label) ";" lr_list1);
                  let lr_list2 = (get_descendants offset' lr2) @ [lr2] in
                  DEBUG_MSG "lr_list2=[%s]"
                    (Xlist.to_string (fun n -> n#data#label) ";" lr_list2);
                  incr_score ~extra_denom:d ~bonus_named:true lr_list1 lr_list2;
                  if both then begin
                    let lr_list1' = (get_descendants offset lr1) in
                    let lr_list2' = (get_descendants offset lr2) in
                    incr_score ~extra_denom:d ~bonus_named:true lr_list1' lr_list2'
                  end
                )
            with
              Invalid_argument _ -> ()
          end;
          if extra then begin
            try (* extra addition *)
              let n1 = ichildren1.(ipos1 + offset * 2) in
              let n2 = ichildren2.(ipos2 + offset * 2) in

              comp_score ~extra_denom:d ~bonus_named:true n1 n2
                (fun () ->
                  let lst1 = (get_descendants offset' n1) @ [n1] in
                  let lst2 = (get_descendants offset' n2) @ [n2] in
                  incr_score ~extra_denom:d ~bonus_named:true lst1 lst2;
                  if both then begin
                    let lst1' = (get_descendants offset n1) in
                    let lst2' = (get_descendants offset n2) in
                    incr_score ~extra_denom:d ~bonus_named:true lst1' lst2'
                  end
                )
            with
              Invalid_argument _ -> ()
          end

        in (* end of func score_lr *)


        (* for left and right descendants *)

        begin
          try
            let left, right, anc1, anc2, pos1, pos2, d = find_anchor nd1 nd2 in

            DEBUG_MSG "anchor for %a-%a: %a-%a (left=%B, right=%B, d=%d)"
              ups uid1 ups uid2 ups anc1#uid ups anc2#uid left right d;
(*
            if left && right then begin
              score_lr (-1) anc1 anc2 pos1 pos2 d;
              score_lr 1 anc1 anc2 pos1 pos2 d
            end
            else if left then
              score_lr ~both:true (-1) anc1 anc2 pos1 pos2 d
            else if right then
              score_lr ~both:true 1 anc1 anc2 pos1 pos2 d
*)
            if left then begin
              score_lr (-1) anc1 anc2 pos1 pos2 d;
              (*score_lr (-2) anc1 anc2 pos1 pos2 d*)
            end;
            if right then begin
              score_lr 1 anc1 anc2 pos1 pos2 d;
              (*score_lr 2 anc1 anc2 pos1 pos2 d*)
            end

          with
            Not_found -> ()
        end;

        DEBUG_MSG "score for left and right descendants: %f" !score;


        (* for ancestors *)
        let ancs1 = List.rev (tree1#initial_ancestor_nodes nd1) in
        let ancs2 = List.rev (tree2#initial_ancestor_nodes nd2) in

        let score_anc = _incr_score ~weight:0.5 ~bonus_named:true ancs1 ancs2 in

        DEBUG_MSG "score for ancestors: %f" score_anc;

        (* for descendants *)
        let score_desc =
          try
            _comp_score ~bonus_named:true ~bonus_named_more:true nd1 nd2
          with
            Not_found ->
              let desc1 = ref [] in
              let desc2 = ref [] in
              tree1#fast_scan_whole_initial_subtree nd1
                (fun n ->
                  if n != nd1 then
                    desc1 := n :: !desc1
                );
              tree2#fast_scan_whole_initial_subtree nd2
                (fun n ->
                  if n != nd2 then
                    desc2 := n :: !desc2
                );
              match !desc1, !desc2 with
              | [], [] -> 0.0
              | [], _ | _, [] -> 0.0
              | _ -> _incr_score ~bonus_named:true ~bonus_named_more:true !desc1 !desc2
        in

        let score_siblings, score_parent =
          if
            nd1#initial_nchildren = 0 && nd2#initial_nchildren = 0 &&
            (self#under_permutation_hub nd1 nd2 ||
            nd1#data#eq nd2#data && self#has_trivial_value nd1)
          then
            (try
              let nds1 = List.filter (fun x -> x != nd1) (Array.to_list nd1#initial_parent#initial_children) in
              let nds2 = List.filter (fun x -> x != nd2) (Array.to_list nd2#initial_parent#initial_children) in
              _incr_score ~bonus_named:true ~bonus_named_more:true nds1 nds2
            with _ -> 0.0),
            (try
              self#get_adjacency_score nd1#initial_parent nd2#initial_parent
            with _ -> 0.0)
          else
            0.0, 0.0
        in

        let total_score = !score +. score_anc +. score_desc +. score_siblings +. score_parent in

        BEGIN_DEBUG
          DEBUG_MSG "score for descendants: %f" score_desc;
          DEBUG_MSG "score: %a-%a -> %f" ups uid1 ups uid2 total_score;
          DEBUG_MSG "ref_pairs: [%s]"
            (Xlist.to_string
               (fun (n1, n2) -> Printf.sprintf "%a-%a" nups n1 nups n2)
               ";" !ref_pairs)
        END_DEBUG;

        if use_adjacency_cache then begin
          let key = uid1, uid2 in
          (*let prev_score, _ = try Hashtbl.find adjacency_cache key with _ -> 0.0, [] in
          if total_score > prev_score then*)
            Hashtbl.replace adjacency_cache key (total_score, !ref_pairs)
        end;

        total_score, !ref_pairs
(* end of method _get_adjacency_score *)


  method get_adjacency_score nd1 nd2 =
    let s, _ = self#_get_adjacency_score nd1 nd2 in
    s



  method find_nearest_mapped_ancestor_pair
      (map : UID.t -> UID.t)
      (nd1 : 'node_t)
      (nd2 : 'node_t)
      =
    let rec doit nd =
      try
        let pnd = nd#initial_parent in
        try
          let u' = map pnd#uid in
          let pnd' = tree2#search_node_by_uid u' in
          if tree2#is_initial_ancestor pnd' nd2 then
            (pnd, pnd')
          else
            doit pnd
        with
          Not_found -> doit pnd
      with
        Otreediff.Otree.Parent_not_found _ -> raise Not_found
    in
    doit nd1

  method find_mapped_ancestor_pairs
      (map : UID.t -> UID.t)
      (nd1 : 'node_t)
      (nd2 : 'node_t)
      =
    let rec doit acc n1 n2 =
      try
        let pn1, pn2 = self#find_nearest_mapped_ancestor_pair map n1 n2 in
        doit ((pn1, pn2) :: acc) pn1 pn2
      with
        Not_found -> acc
    in
    doit [] nd1 nd2

(*
  method get_proximity (nd1 : node_t) (nd2 : node_t) =
    let ancs1 = Array.of_list nd1#initial_ancestor_nodes in
    let ancs2 = Array.of_list nd2#initial_ancestor_nodes in
    let lai1 = (Array.length ancs1) - 1 in
    let lai2 = (Array.length ancs2) - 1 in

    let _cands = ref [] in
    for i = lai1 downto 0 do
      let ni = ancs1.(i) in
      let labi = ni#data#_label in
      for j = lai2 downto 0 do
        let nj = ancs2.(j) in
        if labi = nj#data#_label then
          _cands := (ni, nj, i + j) :: !_cands
      done
    done;
    let cands =
      List.fast_sort
        (fun (_, _, p0) (_, _, p1) -> Stdlib.compare p1 p0)
        !_cands
    in
    let prox =
      match cands with
      | (n1, n2, p)::_ ->

            DEBUG_MSG "c#get_proximity: (%a,%a) -> %d (pivot=%a-%a)"
              nups nd1 nups nd2 p nups n1 nups n2;

          p

      | [] -> 0
    in
    prox
(* end of method get_proximity *)
*)



  method check_parents ?(exact=true) (uidmapping : 'node_t UIDmapping.c) nd1 nd2 =

    DEBUG_MSG "checking: %a-%a" nups nd1 nups nd2;

    begin
      try
        let pnd1 = nd1#initial_parent in
        let pnd2 = nd2#initial_parent in
        let puid1 = pnd1#uid in
        let puid2 = pnd2#uid in

        DEBUG_MSG "  parents: %a-%a" ups puid1 ups puid2;

        let cond_exact = pnd1#data#eq pnd2#data && exact in
        let cond_inexact =
          pnd1#data#relabel_allowed pnd2#data && not exact
        in

        if cond_exact || cond_inexact then begin

          let to_be_removed = ref [] in
          let add_ok = ref true in

          begin
            try
              let puid1' = uidmapping#find puid1 in
              if puid1' <> puid2 then
                let pnd1' = tree2#search_node_by_uid puid1' in
                if
                  not (next_to_each_other pnd2 pnd1') &&
                  self#get_adjacency_score pnd1 pnd2 > self#get_adjacency_score pnd1 pnd1'
                then
                  to_be_removed := (puid1, puid1') :: !to_be_removed
                else
                  add_ok := false
            with
              Not_found -> ()
          end;
          begin
            try
              let puid2' = uidmapping#inv_find puid2 in
              if puid2' <> puid1 then
                let pnd2' = tree1#search_node_by_uid puid2' in
                if
                  not (next_to_each_other pnd2' pnd2) &&
                  self#get_adjacency_score pnd1 pnd2 > self#get_adjacency_score pnd2' pnd2
                then
                  to_be_removed := (puid2', puid2) :: !to_be_removed
                else
                  add_ok := false
            with
              Not_found -> ()
          end;

          if !add_ok then begin
            if !to_be_removed <> [] then begin
              List.iter
                (fun (u1, u2) ->
                  DEBUG_MSG "  removing %a-%a" ups u1 ups u2;
                  ignore (uidmapping#remove u1 u2)
                ) !to_be_removed
            end;

            DEBUG_MSG "  adding %a-%a" ups puid1 ups puid2;

            ignore (uidmapping#add_unsettled puid1 puid2)
          end;

          self#check_parents ~exact uidmapping pnd1 pnd2

        end (* of if pnd1#data#eq pnd2#data *)

      with
        Otreediff.Otree.Parent_not_found _ -> ()
    end


  method is_matched_subtree uidmapping r1 r2 x =
    DEBUG_MSG "%a %a: %a" nps r1 nps r2 nps x;
    let b =
      match x#data#_digest with
      | None -> false
      | Some d ->
          try
            match self#multiple_subtree_matches#find d with
            | [], _, _ | _, [], _ -> false
            | nml1, nml2, _ ->
                List.exists (fun (n1, _) -> tree1#is_initial_ancestor r1 n1) nml1 &&
                List.exists (fun (n2, _) -> tree2#is_initial_ancestor r2 n2) nml2
          with
            Not_found -> false
    in
    let b =
      b ||
      try
        match self#multiple_node_matches#find x#data#_label with
        | [], _ | _, [] -> false
        | nl1, nl2 ->
            let nl1_ = List.filter (tree1#is_initial_ancestor r1) nl1 in
            let nl2_ = List.filter (tree2#is_initial_ancestor r2) nl2 in
            match nl1_, nl2_ with
            | [], _ | _, [] -> false
            | [n1], [n2] -> DEBUG_MSG "found: %a - %a" nps n1 nps n2; true
            | _ ->
                List.exists
                  (fun n1 ->
                    try
                      let n1' = tree2#search_node_by_uid (uidmapping#find n1#uid) in
                      let b = List.memq n1' nl2_ in
                      if b then
                        DEBUG_MSG "found: %a - %a" nps n1 nps n1';
                      b
                    with
                      Not_found -> false
                  ) nl1_
      with
        Not_found -> false
    in
    if b then
      DEBUG_MSG "%a -> %B" nps x b;
    b


  method has_matched_subtree uidmapping r1 r2 ?(excluded=[]) n =
    let moveon x = not (List.memq x excluded) in
    has_p_descendant ~moveon (self#is_matched_subtree uidmapping r1 r2) n


  method check_op_mappings_m uidmapping _nd1 _nd2 nd1 nd2 =
    let b =
    _nd1 == nd1 &&
    (try not nd1#initial_parent#data#is_op with _ -> true) &&
    (try not _nd2#initial_parent#data#is_op with _ -> true) &&
    tree2#is_initial_ancestor _nd2 nd2 &&
    self#has_matched_subtree uidmapping _nd1 _nd2 ~excluded:[nd2] _nd2 &&
    (tree2#whole_initial_subtree_size nd2) * 2 < tree2#whole_initial_subtree_size _nd2 &&
    Array.exists
      (fun x ->
        x#data#has_non_trivial_value &&
        Array.exists (fun y -> x#data#eq y#data) nd1#initial_children
      ) nd2#initial_children
  ||
    _nd2 == nd2 &&
    (try not nd2#initial_parent#data#is_op with _ -> true) &&
    (try not _nd1#initial_parent#data#is_op with _ -> true) &&
    tree1#is_initial_ancestor _nd1 nd1 &&
    self#has_matched_subtree uidmapping _nd1 _nd2 ~excluded:[nd1] _nd1 &&
    (tree1#whole_initial_subtree_size nd1) * 2 < tree1#whole_initial_subtree_size _nd1 &&
    Array.exists
      (fun x ->
        x#data#has_non_trivial_value &&
        Array.exists (fun y -> x#data#eq y#data) nd2#initial_children
      ) nd1#initial_children
    in
    b && self#get_similarity_score _nd1 _nd2 > subtree_similarity_thresh


  method check_op_mappings uidmapping size_nd1 size_nd2 _nd1 _nd2 nd1 nd2 =
    nd1 == _nd1 &&
    tree2#is_initial_ancestor _nd2 nd2 &&
    (try
      let n1 = tree1#search_node_by_uid (uidmapping#inv_find _nd2#uid) in
      tree1#is_initial_ancestor nd1 n1 &&
      let sz_n1 = tree1#whole_initial_subtree_size n1 in
      DEBUG_MSG "sz_n1:%d size_nd2:%d" sz_n1 size_nd2;
      sz_n1 < size_nd2
    with
      _ -> false
    ) &&
    self#has_matched_subtree uidmapping nd1 nd2 ~excluded:[_nd2] nd2
  ||
    nd2 == _nd2 &&
    tree1#is_initial_ancestor _nd1 nd1 &&
    (try
      let n2 = tree2#search_node_by_uid (uidmapping#find _nd1#uid) in
      tree2#is_initial_ancestor nd2 n2 &&
      let sz_n2 = tree2#whole_initial_subtree_size n2 in
      DEBUG_MSG "sz_n2:%d size_nd1:%d" sz_n2 size_nd1;
      sz_n2 < size_nd1
    with
      _ -> false
    ) &&
    self#has_matched_subtree uidmapping nd1 nd2 ~excluded:[_nd1] nd1


  method compare_mappings
      (uidmapping : 'node_t UIDmapping.c)
      ?(override=false)
      ?(bonus_self=false)
      ?(bonus_parent=false)
      ?(force_prefer_crossing_count=false)
      nd1old nd2old ?(ncrossing_old=ref (-1)) ?(adjacency_old=ref (-1.0))
      (action_old : int option (* difference of ncrossing *) -> float option -> bool (*force*) -> unit)
      nd1new nd2new ?(ncrossing_new=ref (-1)) ?(adjacency_new=ref (-1.0))
      (action_new : int option (* difference of ncrossing *) -> float option -> bool (*force*) -> unit)
      =

    DEBUG_MSG "[override:%B] %a-%a vs %a-%a" override
      nups nd1old nups nd2old nups nd1new nups nd2new;


    let add_cache ncross_used b ncd ncsim =
      if use_mapping_comparison_cache then
        if not ncross_used then
          Hashtbl.replace mapping_comparison_cache
            (override, nd1old#uid, nd2old#uid, nd1new#uid, nd2new#uid)
            (b, ncd, ncsim)
    in

    begin
      try
        if not use_mapping_comparison_cache then
          raise Not_found;

        let b, ncross_diff, ncross_sim =
          Hashtbl.find mapping_comparison_cache
            (override, nd1old#uid, nd2old#uid, nd1new#uid, nd2new#uid)
        in

        DEBUG_MSG "  cache hit! --> %B%s%s"
          b
          (match ncross_diff with Some i -> Printf.sprintf ", %d" i | None -> "")
          (match ncross_sim with Some x -> Printf.sprintf ", %f" x | None -> "");

        if b then
          action_new ncross_diff ncross_sim false
        else
          action_old ncross_diff ncross_sim false;

        mapping_comparison_cache_hit_count <- mapping_comparison_cache_hit_count + 1;

      with
        Not_found ->

          let check_label_match ~ncross_used =
            let lmatch_old = self#eval_label_match nd1old nd2old in
            let lmatch_new = self#eval_label_match nd1new nd2new in

            DEBUG_MSG "  label match: %d --> %d" lmatch_old lmatch_new;

            let b =
              if override then
                if lmatch_new >= lmatch_old then begin
                  action_new None None false;
                  true
                end
                else begin
                  action_old None None false;
                  false
                end
              else
                if lmatch_new > lmatch_old then begin
                  action_new None None false;
                  true
                end
                else begin
                  action_old None None false;
                  false
                end
            in
            add_cache ncross_used b None None
          in

          let check_adjacency ?(bonus_self=false) ?(bonus_parent=false) ~ncross_used () =
            let adj_old =
              if !adjacency_old < 0.0 then begin
                let bonus =
                  if nd1old#initial_nchildren = 0 && nd2old#initial_nchildren = 0 then begin
                    (if bonus_self then
                      float (self#eval_label_match ~bonus_named:true nd1old nd2old)
                    else
                      0.0) +.
                    (if bonus_parent && nd1old#data#is_named_orig && nd2old#data#is_named_orig then
                      try
                        let pnd1old = nd1old#initial_parent in
                        let pnd2old = nd2old#initial_parent in
                        if
                          pnd1old#data#is_named_orig && pnd1old#data#eq pnd2old#data &&
                          pnd1old#initial_nchildren = 1 && pnd2old#initial_nchildren = 1
                        then
                          let f = float (self#eval_label_match ~bonus_named:true pnd1old pnd2old) in
                          DEBUG_MSG "bonus_parent: %f (%a-%a)" f nups nd1old nups nd2old;
                          f
                        else
                          0.0
                      with
                        _ -> 0.0
                    else
                      0.0)
                  end
                  else
                    0.0
                in
                adjacency_old := (self#get_adjacency_score nd1old nd2old) +. bonus;

                if bonus_self || bonus_parent then
                  DEBUG_MSG "bonus=%f" bonus;

              end;
              !adjacency_old
            in
            let adj_new =
              if !adjacency_new < 0.0 then begin
                let bonus =
                  if nd1new#initial_nchildren = 0 && nd2new#initial_nchildren = 0 then begin
                    (if bonus_self then
                      float (self#eval_label_match ~bonus_named:true nd1new nd2new)
                    else
                      0.0) +.
                    (if bonus_parent && nd1new#data#is_named_orig && nd2new#data#is_named_orig then
                      try
                        let pnd1new = nd1new#initial_parent in
                        let pnd2new = nd2new#initial_parent in
                        if
                          pnd1new#data#is_named_orig && pnd1new#data#eq pnd2new#data &&
                          pnd1new#initial_nchildren = 1 && pnd2new#initial_nchildren = 1
                        then
                          let f = float (self#eval_label_match ~bonus_named:true pnd1new pnd2new) in
                          DEBUG_MSG "bonus_parent: %f (%a-%a)" f nups nd1new nups nd2new;
                          f
                        else
                          0.0
                      with
                        _ -> 0.0
                    else
                      0.0)
                  end
                  else
                    0.0
                in
                adjacency_new := (self#get_adjacency_score nd1new nd2new) +. bonus;

                if bonus_self || bonus_parent then
                  DEBUG_MSG "bonus=%f" bonus;

              end;
              !adjacency_new
            in

            DEBUG_MSG " adjacency: %f --> %f" adj_old adj_new;

            if adj_old = adj_new then begin (* label match score is used *)

              check_label_match ~ncross_used

            end
            else begin (* adj_old <> adj_new *)
              let b =
                if adj_new > adj_old then begin
                  action_new None None false;
                  true
                end
                else begin
                  action_old None None false;
                  false
                end
              in
              add_cache ncross_used b None None
            end
          in (* check_adjacency *)

          let ancsim_old = self#get_ancestors_similarity nd1old nd2old in
          let ancsim_new = self#get_ancestors_similarity nd1new nd2new in

          DEBUG_MSG "ancestors similarity: %f --> %f" ancsim_old ancsim_new;

          let has_matched_subtree = self#has_matched_subtree uidmapping in

          let get_names nd =
            let _nl =
              List.map
                (fun x -> x#data#get_orig_name)
                (get_p_descendants (fun x -> x#data#is_named_orig) nd)
            in
            let nl =
              if nd#data#is_named_orig then
                nd#data#get_orig_name :: _nl
              else
                _nl
            in
            DEBUG_MSG "%a -> [%s]" nups nd (Xlist.to_string (fun x -> x) "," nl);
            nl
          in
          let parent_check pivot pnd nd =
            let b =
              not pnd#data#is_op &&
              not pnd#data#is_named &&
              pnd#initial_nchildren = 2 &&
              pivot#data#is_named_orig &&
              pivot#initial_nchildren > 0 &&
              nd#data#is_named_orig &&
              nd#initial_nchildren > 0 &&
              nd#initial_pos = 1 &&
              let sib = pnd#initial_children.(0) in
              sib#initial_nchildren < 2 &&
              sib#data#anonymized3_label <> pivot#data#anonymized3_label &&
              pnd#data#anonymized3_label <> pivot#data#anonymized3_label &&
              nd#data#anonymized3_label <> pivot#data#anonymized3_label &&
              Xlist.intersection (get_names sib) (get_names pivot) = []
            in
            BEGIN_DEBUG
              if b then begin
                DEBUG_MSG "!!!!!!! pivot=%a:%a[%a]" nups pivot labps pivot locps pivot;
                DEBUG_MSG "parent=%a:%a[%a]" nups pnd labps pnd locps pnd;
                DEBUG_MSG "nd=%a:%a[%a]" nups nd labps nd locps nd;
                let sib = pnd#initial_children.(0) in
                DEBUG_MSG "sib=%a:%a[%a]" nups sib labps sib locps sib;
              end
            END_DEBUG;
            b
          in
          let anc_check pivot anc nd =
            let b =
              not pivot#data#is_op &&
              pivot#data#eq nd#data &&
              not (nd#data#eq anc#data) &&
              nd#data#anonymized2_label <> anc#data#anonymized2_label &&
              nd#data#anonymized3_label <> anc#data#anonymized3_label
            in
            DEBUG_MSG "%a %a %a -> %B" nugps pivot nugps anc nugps nd b;
            b
          in

          let ancsim_old, ancsim_new, prefer_sim =
            if nd1old == nd1new then
              if
                nd2old#initial_parent == nd2new &&
                (nd2new#initial_nchildren = 1 || parent_check nd1old nd2new nd2old)
              then
                ancsim_new, ancsim_new, true
              else if
                nd2old == nd2new#initial_parent &&
                (nd2old#initial_nchildren = 1 || parent_check nd1old nd2old nd2new)
              then
                ancsim_old, ancsim_old, true

              else if
                nd2old#initial_parent != nd2new &&
                anc_check nd1old nd2new nd2old &&
                tree2#is_initial_ancestor nd2new nd2old &&
                not (has_matched_subtree nd1new nd2new ~excluded:[nd2old] nd2new)
              then begin
                DEBUG_MSG "!!!!! nd1old=%a -> nd2old=%a < nd2new=%a" nugps nd1old nugps nd2old nugps nd2new;
                DEBUG_MSG "nd1: %a" ndps nd1old;
                DEBUG_MSG "nd2: %a < %a" ndps nd1old ndps nd1new;
                ancsim_new, ancsim_new, true
              end
              else if
                nd2old != nd2new#initial_parent &&
                anc_check nd1old nd2old nd2new &&
                tree2#is_initial_ancestor nd2old nd2new &&
                not (has_matched_subtree nd1old nd2old ~excluded:[nd2new] nd2old)
              then begin
                DEBUG_MSG "!!!!! nd1old=%a -> nd2old=%a > nd2new=%a" nugps nd1old nugps nd2old nugps nd2new;
                DEBUG_MSG "nd1: %a" ndps nd1old;
                DEBUG_MSG "nd2: %a > %a" ndps nd1old ndps nd1new;
                ancsim_old, ancsim_old, true
              end

              else
                ancsim_old, ancsim_new, false

            else if nd2old == nd2new then
              if
                nd1old#initial_parent == nd1new &&
                (nd1new#initial_nchildren = 1 || parent_check nd2old nd1new nd1old)
              then
                ancsim_new, ancsim_new, true
              else if
                nd1old == nd1new#initial_parent &&
                (nd1old#initial_nchildren = 1 || parent_check nd2old nd1old nd1new)
              then
                ancsim_old, ancsim_old, true

              else if
                nd1old#initial_parent != nd1new &&
                anc_check nd2old nd1new nd1old &&
                tree1#is_initial_ancestor nd1new nd1old &&
                not (has_matched_subtree nd1new nd2new ~excluded:[nd1old] nd1new)
              then begin
                DEBUG_MSG "!!!!! nd1old=%a < nd1new=%a <- nd2old=%a" nugps nd1old nugps nd1new nugps nd2old;
                DEBUG_MSG "nd2: %a" ndps nd2old;
                DEBUG_MSG "nd1: %a < %a" ndps nd1old ndps nd1new;
                ancsim_new, ancsim_new, true
              end
              else if
                nd1old != nd1new#initial_parent &&
                anc_check nd2old nd1old nd1new &&
                tree1#is_initial_ancestor nd1old nd1new &&
                not (has_matched_subtree nd1old nd2old ~excluded:[nd1new] nd1old)
              then begin
                DEBUG_MSG "!!!!! nd1old=%a > nd1new=%a <- nd2old=%a" nugps nd1old nugps nd1new nugps nd2old;
                DEBUG_MSG "nd2: %a" ndps nd2old;
                DEBUG_MSG "nd1: %a > %a" ndps nd1old ndps nd1new;
                ancsim_old, ancsim_old, true
              end

              else
                ancsim_old, ancsim_new, false
            else
              ancsim_old, ancsim_new, false
          in

          DEBUG_MSG "ancestors similarity: %f --> %f" ancsim_old ancsim_new;

          let anc_sim_ratio = (Xlist.min [ancsim_old; ancsim_new]) /. (Xlist.max [ancsim_old; ancsim_new]) in

          DEBUG_MSG "ancestors similarity ratio: %f" anc_sim_ratio;

          DEBUG_MSG "prefer_sim: %B" prefer_sim;


          let subtree_sim_old = self#get_similarity_score nd1old nd2old in
          let subtree_sim_new = self#get_similarity_score nd1new nd2new in

          DEBUG_MSG "subtree similarity: %f --> %f" subtree_sim_old subtree_sim_new;

          let subtree_sim_ratio =
            (Xlist.min [subtree_sim_old; subtree_sim_new]) /. (Xlist.max [subtree_sim_old; subtree_sim_new])
          in

          DEBUG_MSG "subtree similarity ratio: %f" subtree_sim_ratio;

          let size_old1 = tree1#whole_initial_subtree_size nd1old in
          let size_old2 = tree2#whole_initial_subtree_size nd2old in
          let size_new1 = tree1#whole_initial_subtree_size nd1new in
          let size_new2 = tree2#whole_initial_subtree_size nd2new in

          let size_old = size_old1 + size_old2 in
          let size_new = size_new1 + size_new2 in

          DEBUG_MSG "subtree size: %d --> %d" size_old size_new;

          let anc_sim_almost_same = anc_sim_ratio >= ancestors_similarity_ratio_thresh in
          let all_single = size_old = 2 && size_new = 2 in
          let all_double = size_old1 = 2 && size_old2 = 2 && size_new1 = 2 && size_new2 = 2 in
          let all_single_or_double = all_single || all_double in
          let chk_for_old() =
            all_single_or_double ||
            (nd1old == nd1new && tree2#is_initial_ancestor nd2new nd2old ||
            nd2old == nd2new && tree1#is_initial_ancestor nd1new nd1old)
          in
          let chk_for_new() =
            all_single_or_double ||
            (nd1old == nd1new && tree2#is_initial_ancestor nd2old nd2new ||
            nd2old == nd2new && tree1#is_initial_ancestor nd1old nd1new)
          in

          BEGIN_DEBUG
            DEBUG_MSG
            "anc_sim_almost_same: %B (thresh=%f)" anc_sim_almost_same ancestors_similarity_ratio_thresh;
            DEBUG_MSG "all_single: %B" all_single;
            DEBUG_MSG "all_double: %B" all_double;
          END_DEBUG;

          let _is_plausible nd1 nd2 =
            let b =
             (self#has_weak_non_trivial_value nd1 && nd2#data#is_named_orig && not nd2#data#is_string_literal &&
             let v = nd1#data#get_value in
             let nm = nd2#data#get_name in
             DEBUG_MSG "v=%s nm=%s" v nm;
             let count = ref 0 in
             try
               uidmapping#iter
                 (fun u1 u2 ->
                   let n1 = tree1#search_node_by_uid u1 in
                   let n2 = tree2#search_node_by_uid u2 in
                   if try n1#data#get_value = v && n2#data#get_name = nm with _ -> false then begin
                     incr count;
                     if !count > 1 then
                       raise Exit
                   end
                 );
               false
             with
               Exit -> true) ||
            (nd1#data#is_named_orig && not nd1#data#is_string_literal && self#has_weak_non_trivial_value nd2 &&
             let nm = nd1#data#get_name in
             let v = nd2#data#get_value in
             DEBUG_MSG "nm=%s v=%s" nm v;
             let count = ref 0 in
             try
               uidmapping#iter
                 (fun u1 u2 ->
                   let n1 = tree1#search_node_by_uid u1 in
                   let n2 = tree2#search_node_by_uid u2 in
                   if try n1#data#get_name = nm && n2#data#get_value = v with _ -> false then begin
                     incr count;
                     if !count > 1 then
                       raise Exit
                   end
                 );
               false
             with
               Exit -> true)
            in
            DEBUG_MSG "%a-%a -> %B" nups nd1 nups nd2 b;
            b
          in
          let is_plausible nd1 nd2 =
            try
              _is_plausible nd1 nd2
            with _ -> false
          in

          if
            (ancsim_old = 1.0 && subtree_sim_old = 1.0 && ancsim_new < 1.0 && subtree_sim_new < 1.0) ||
            (
             anc_sim_almost_same && subtree_sim_old = 1.0 && subtree_sim_new < 1.0 && chk_for_old() ||
             is_plausible nd1old nd2old && not (is_plausible nd1new nd2new)
            ) ||
            prefer_sim && subtree_sim_old > subtree_sim_new
            (* || (subtree_sim_old > subtree_sim_new && subtree_sim_ratio < subtree_similarity_ratio_lower_thresh) *)
          then begin
            DEBUG_MSG "@";
            let b, ncd, ncsim =
              action_old None None false;
              false, None, None
            in
            add_cache false b ncd ncsim
          end
          else if
            (ancsim_new = 1.0 && subtree_sim_new = 1.0 && ancsim_old < 1.0 && subtree_sim_old < 1.0) ||
            (anc_sim_almost_same && subtree_sim_new = 1.0 && subtree_sim_old < 1.0 && chk_for_new() ||
            is_plausible nd1new nd2new && not (is_plausible nd1old nd2old)) ||
            prefer_sim && subtree_sim_new > subtree_sim_old
            (* || (subtree_sim_new > subtree_sim_old && subtree_sim_ratio < subtree_similarity_ratio_lower_thresh) *)
          then begin
            DEBUG_MSG "@";
            let b, ncd, ncsim =
              action_new None None false;
              true, None, None
            in
            add_cache false b ncd ncsim
          end
          else if ancsim_new = 0.0 && ancsim_old > 0.5 then begin
            DEBUG_MSG "@";
            let b, ncd, ncsim =
              action_old None None false;
              false, None, None
            in
            add_cache false b ncd ncsim
          end
          else if ancsim_old = 0.0 && ancsim_new > 0.5 then begin
            DEBUG_MSG "@";
            let b, ncd, ncsim =
              action_new None None false;
              true, None, None
            in
            add_cache false b ncd ncsim
          end
          else begin
            DEBUG_MSG "@";
            let has_same_children nd1 nd2 =
              let cs1 = nd1#initial_children in
              let cs2 = nd2#initial_children in
              let len1 = Array.length cs1 in
              let len2 = Array.length cs2 in

              if len1 = len2 && len1 > 0 then
                try
                  let b = ref false in
                  Array.iteri
                    (fun i n1 ->
                      let n2 = cs2.(i) in
                      if n1#data#_label = n2#data#_label then
                        b := true;

                      if n1#data#_anonymized_label <> n2#data#_anonymized_label then
                        raise Found
                    ) cs1;
                  !b
                with
                  Found -> false
              else
                false
            in

            let prefer_crossing_count = force_prefer_crossing_count ||

              let size_cond =
                (size_old > 2 && size_new > 2) ||
                (nd1old#data#eq nd2old#data && nd2old#data#eq nd1new#data && nd1new#data#eq nd2new#data &&
                 all_single)
              in
              DEBUG_MSG "size_cond: %B" size_cond;

              if size_cond then begin

                let anc_cond =
                  (ancsim_old >= ancestors_similarity_thresh && ancsim_new >= ancestors_similarity_thresh) ||
                  (
                   (ancsim_old >= ancestors_similarity_thresh || ancsim_new >= ancestors_similarity_thresh) &&
                   anc_sim_almost_same
                  )
                in
                DEBUG_MSG "anc_cond: %B" anc_cond;

                if anc_cond then begin

                  let neighbour_cond =
                    let pairs_old = self#find_mapped_ancestor_pairs uidmapping#find nd1old nd2old in
                    let pairs_new = self#find_mapped_ancestor_pairs uidmapping#find nd1new nd2new in

                    BEGIN_DEBUG
                      let f = Xlist.to_string (fun (n1, n2) -> Printf.sprintf "%a-%a" nups n1 nups n2) ";" in
                      DEBUG_MSG "pairs_old: %s" (f pairs_old);
                      DEBUG_MSG "pairs_new: %s" (f pairs_new)
                    END_DEBUG;

                    let rec get_uniq_pairs p =
                      match p with
                      | l, [] | [], l -> p
                      | (a11, a12)::t1, (a21, a22)::t2 ->
                          if a11 == a21 && a12 == a22 then
                            get_uniq_pairs (t1, t2)
                          else
                            p
                    in
                    try
                      let uniq_pairs_old, uniq_pairs_new = get_uniq_pairs (pairs_old, pairs_new) in

                      BEGIN_DEBUG
                        let f = Xlist.to_string (fun (n1, n2) -> Printf.sprintf "%a-%a" nups n1 nups n2) ";" in
                        DEBUG_MSG "uniq_pairs_old: %s" (f uniq_pairs_old);
                        DEBUG_MSG "uniq_pairs_new: %s" (f uniq_pairs_new)
                      END_DEBUG;

                      let get_n_mapped = function
                        | [] -> 0
                        | (pn1, pn2)::_ -> self#estimate_cost_of_move uidmapping pn1 pn2
                      in
                      let n_mapped_old = get_n_mapped uniq_pairs_old in
                      let n_mapped_new = get_n_mapped uniq_pairs_new in

                      DEBUG_MSG "n_mapped_old: %d n_mapped_new: %d" n_mapped_old n_mapped_new;

                      (*n_mapped_old > 0 && n_mapped_new > 0 &&*)

                      let get_sz = function
                        | [] -> 0
                        | (pn1, pn2)::_ ->
                            (tree1#whole_initial_subtree_size pn1) + (tree2#whole_initial_subtree_size pn2)
                      in

                      let sz_old = get_sz uniq_pairs_old in
                      let sz_new = get_sz uniq_pairs_new in

                      DEBUG_MSG "sz_old: %d sz_new: %d" sz_old sz_new;

                      let denom = sz_old + sz_new in
                      let r =
                        if denom = 0 then
                          0.0
                        else
                          (float (2 * (abs (n_mapped_old - n_mapped_new)))) /. (float denom)
                      in
                      DEBUG_MSG "difference: %f" r;

                      r < options#mapped_neighbours_difference_threshold

                    with
                      Not_found -> true
                  in
                  DEBUG_MSG "neighbour_cond: %B" neighbour_cond;

                  if neighbour_cond then begin

                    let sim_cond =
                      (
                       subtree_sim_old >= subtree_similarity_thresh &&
                       subtree_sim_new >= subtree_similarity_thresh
                      ) ||
                      (has_same_children nd1old nd2old && has_same_children nd1new nd2new)
                    in
                    DEBUG_MSG "sim_cond: %B" sim_cond;
                    (
                     sim_cond ||
                     (anc_cond && subtree_sim_ratio > subtree_similarity_ratio_thresh
(* subtree_sim_old <= subtree_similarity_lower_thresh && subtree_sim_new <= subtree_similarity_lower_thresh *))
                    ) &&
                    ((ancsim_old < 1.0 && ancsim_new < 1.0) || (ancsim_old = 1.0 && ancsim_new = 1.0))
                  end
                  else
                    false
                end
                else
                  false
              end
              else
                false
            in (* prefer_crossing_count *)

            DEBUG_MSG "prefer_crossing_count: %B" prefer_crossing_count;

            (*let prefer_crossing_count =
              prefer_crossing_count ||
              is_cross_boundary uidmapping nd1old nd2old || is_cross_boundary uidmapping nd1new nd2new
            in!!!NG!!!*)

            if (* (nd1old#data#eq nd2old#data || nd1new#data#eq nd2new#data) && *) prefer_crossing_count

            then begin (* crossing count preferred *)
              let ncross_old =
                if !ncrossing_old < 0 then
                  ncrossing_old := uidmapping#count_crossing_or_incompatible_matches nd1old nd2old;
                !ncrossing_old
              in
              let ncross_new =
                if !ncrossing_new < 0 then
                  ncrossing_new := uidmapping#count_crossing_or_incompatible_matches nd1new nd2new;
                !ncrossing_new
              in

              DEBUG_MSG " num of incompatible or crossing matches: %d --> %d" ncross_old ncross_new;

(*
  let similar_ncross =
  let sim =
  ((float (Xlist.min [ncross_old; ncross_new])) /. (float (Xlist.max [ncross_old; ncross_new])))
  in
  DEBUG_MSG "similarity of ncross: %f" sim;

  sim > ncross_sim_ratio_thresh
  in
 *)
              if ncross_old = ncross_new (* similar_ncross *) then begin

                (*let sibling_cond =
                  if nd2old == nd2new then
                    next_to_each_other nd1old nd1new
                  else if nd1old == nd2new then
                    next_to_each_other nd2old nd2new
                  else
                    false
                in
                DEBUG_MSG "sibling_cond=%B" sibling_cond;
                if sibling_cond then
                  check_label_match ~ncross_used:true
                else!!!NG!!!*)
                  check_adjacency ~bonus_self:true ~ncross_used:true ()

              end
              else begin (* ncross_old <> ncross_new *)
                let ncross_sim =
                  ((float (Xlist.min [ncross_old; ncross_new])) /. (float (Xlist.max [ncross_old; ncross_new])))
                in
                DEBUG_MSG "similarity of ncross: %f" ncross_sim;

                let b, ncd, ncsim =
                  if ncross_new < ncross_old then
                    let d = Some (ncross_old - ncross_new) in
                    let s = Some ncross_sim in
                    action_new d s false;
                    true, d, s
                  else
                    let d = Some (ncross_new - ncross_old) in
                    let s = Some ncross_sim in
                    action_old d s false;
                    false, d, s
                in
                add_cache true b ncd ncsim
              end
            end
            else if (* adjacency is useless for siblings *)
              nd1old#initial_parent == nd1new#initial_parent &&
              nd2old#initial_parent == nd2new#initial_parent &&
              abs (nd1old#initial_pos - nd1new#initial_pos) <= 1 &&
              abs (nd2old#initial_pos - nd2new#initial_pos) <= 1
            then begin

              check_label_match ~ncross_used:false

            end

            else if
              ancsim_old = 1.0 && ancsim_new < 1.0 &&
              nd1old#data#is_op && nd2old#data#is_op && nd1new#data#is_op && nd2new#data#is_op &&
              subtree_sim_ratio < subtree_similarity_ratio_thresh &&
              self#check_op_mappings_m uidmapping nd1old nd2old nd1new nd2new
            then begin
              DEBUG_MSG "!!!!!!!! selecting %a - %a" nps nd1new nps nd2new;
              uidmapping#lock_mapping nd1new#uid nd2new#uid;
              action_new None None true
            end
            else if
              ancsim_old < 1.0 && ancsim_new = 1.0 &&
              nd1old#data#is_op && nd2old#data#is_op && nd1new#data#is_op && nd2new#data#is_op &&
              subtree_sim_ratio < subtree_similarity_ratio_thresh &&
              self#check_op_mappings_m uidmapping nd1new nd2new nd1old nd2old
            then begin
              DEBUG_MSG "!!!!!!!! keeping %a - %a" nps nd1old nps nd2old;
              uidmapping#lock_mapping nd1old#uid nd2old#uid;
              action_old None None true
            end
            else if
              ancsim_old = 1.0 && ancsim_new < 1.0 &&
              nd1old#data#is_op && nd2old#data#is_op && nd1new#data#is_op && nd2new#data#is_op &&
              subtree_sim_ratio > subtree_similarity_ratio_thresh &&
              self#check_op_mappings uidmapping size_new1 size_new2 nd1old nd2old nd1new nd2new
            then begin
              DEBUG_MSG "!!!!!!!! selecting %a - %a" nps nd1new nps nd2new;
              uidmapping#lock_mapping nd1new#uid nd2new#uid;
              action_new None None true
            end
            else if
              ancsim_old < 1.0 && ancsim_new = 1.0 &&
              nd1old#data#is_op && nd2old#data#is_op && nd1new#data#is_op && nd2new#data#is_op &&
              subtree_sim_ratio > subtree_similarity_ratio_thresh &&
              self#check_op_mappings uidmapping size_old1 size_old2 nd1new nd2new nd1old nd2old
            then begin
              DEBUG_MSG "!!!!!!!! keeping %a - %a" nps nd1old nps nd2old;
              uidmapping#lock_mapping nd1old#uid nd2old#uid;
              action_old None None true
            end

            else begin (* adjacency is used *)

              check_adjacency ~bonus_self ~bonus_parent ~ncross_used:false ()

            end

          end

    end (* of method compare_mappings *)


  method elaborate_uidmapping
      ?(multi=false)
      ?(multi_node=false)
      (uidmapping : 'node_t UIDmapping.c)
      =
    BEGIN_DEBUG
      DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
      (*DEBUG_MSG "uidmapping (gindex):\n%s\n" uidmapping#to_string_gid;*)
      DEBUG_MSG "multi=%B multi_node=%B" multi multi_node
    END_DEBUG;

    let multiple_subtree_matches =
      try
        self#multiple_subtree_matches
      with
        Not_found -> raise Elaboration_impossible
    in
    let multiple_node_matches =
      try
        self#multiple_node_matches
      with
        Not_found -> raise Elaboration_impossible
    in

    Xprint.verbose options#verbose_flag
      "    elaborating uidmapping (multi:%B,multi_node:%B)..." multi multi_node;

    if multi_node then
      uidmapping#setup_partitions;

    let added_pairs =
      if multi then
        ref (multiple_subtree_matches#align uidmapping)
      else
        ref []
    in
    let removed_pairs = ref [] in

    let check u1 u2 =
      begin
        try
          let u1' = uidmapping#find u1 in
          if u1' <> u2 then begin
            if uidmapping#remove u1 u1' then
              removed_pairs := (u1, u1') :: !removed_pairs
          end
        with
          Not_found -> ()
      end;
      begin
        try
          let u2' = uidmapping#inv_find u2 in
          if u2' <> u1 then begin
            if uidmapping#remove u2' u2 then
              removed_pairs := (u2', u2) :: !removed_pairs
          end
        with
          Not_found -> ()
      end
    in

    let count = ref 0 in

    multiple_subtree_matches#iter
      (fun (d, ndmems1, ndmems2, sz) ->
        DEBUG_MSG "checking subtrees of digest %s" (try Digest.to_hex d with _ -> d);

        let unmapped1, mapped1 =
          List.partition
            (fun (nd, nds) ->
              List.for_all (fun n -> not (uidmapping#mem_dom n#uid)) nds
            ) ndmems1
        in
        let unmapped2, mapped2 =
          List.partition
            (fun (nd, nds) ->
              List.for_all (fun n -> not (uidmapping#mem_cod n#uid)) nds
            ) ndmems2
        in

        let getroots ndmems = List.map (fun (n, _) -> n) ndmems in

        BEGIN_DEBUG
          let ndmem_to_str (n, ns) =
            Printf.sprintf "%a" nups n
            (*Printf.sprintf "%a[%a]" nups n nsps ns *)
          in
          let ndmems_to_str ndmems =
            Xlist.to_string ndmem_to_str "; " ndmems
          in
          DEBUG_MSG "mapped1: %s" (ndmems_to_str mapped1);
          DEBUG_MSG "mapped2: %s" (ndmems_to_str mapped2);
          DEBUG_MSG "unmapped1: %s" (ndmems_to_str unmapped1);
          DEBUG_MSG "unmapped2: %s" (ndmems_to_str unmapped2);
        END_DEBUG;

        let overwrite rtu1 rtu2 mem_pairs =
          DEBUG_MSG "%a-%a: %d mem pairs" ups rtu1 ups rtu2 (List.length mem_pairs);
          incr count;
          List.iter
            (fun (n1, n2) ->
              let u1, u2 = n1#uid, n2#uid in
              begin
                try
                  let u1' = uidmapping#find u1 in
                  if u1' <> u2 then begin
                    if uidmapping#remove u1 u1' then
                      removed_pairs := (u1, u1') :: !removed_pairs
                  end
                with
                  Not_found -> ()
              end;
              begin
                try
                  let u2' = uidmapping#inv_find u2 in
                  if u2' <> u1 then begin
                    if uidmapping#remove u2' u2 then
                      removed_pairs := (u2', u2) :: !removed_pairs
                  end
                with
                  Not_found -> ()
              end;
              ignore (uidmapping#add_settled ~stable:false (* EXPERIMENTAL *) u1 u2);
              added_pairs := (u1, u2) :: !added_pairs

            ) mem_pairs;

          uidmapping#add_settled_roots rtu1 rtu2
        in

        let combi ns1 ns2 =
          List.fold_left (fun l n1 -> l @ (List.map (fun n -> (n1, n)) ns2)) [] ns1
        in

        let unmapped_extra1 = ref [] in
        let unmapped_extra2 = ref [] in

        let mapped_uids1 = List.map (fun (n, _) -> n#uid) mapped1 in
        let mapped_uids2 = List.map (fun (n, _) -> n#uid) mapped2 in

        let all_mapped_uids1 =
          List.fold_left (fun l (_, ns) -> l @ (List.map (fun n -> n#uid) ns)) [] mapped1
        in
        let all_mapped_uids2 =
          List.fold_left (fun l (_, ns) -> l @ (List.map (fun n -> n#uid) ns)) [] mapped2
        in

        let conflicting_pairs_tbl = Hashtbl.create 0 in
        let add_conflicting_pair (n1, n2, ns1, ns2, mps, f) =
          Hashtbl.replace conflicting_pairs_tbl (n1, n2) (ns1, ns2, mps, f)
        in

        List.iter
          (fun (nd1, nds1) ->
            let uid1 = nd1#uid in
            let uids1 = List.map (fun n -> n#uid) nds1 in

            List.iter
              (fun (nd2, nds2) ->
                let uid2 = nd2#uid in
                let uids2 = List.map (fun n -> n#uid) nds2 in

                let is_settled =
                  uidmapping#is_settled_root_pair uid1 uid2 || uidmapping#has_settled_mapping uid1 uid2 (* uidmapping#mem_settled uid1 *)
                in

                DEBUG_MSG " %a-%a --> settled:%B" ups uid1 ups uid2 is_settled;


                if is_settled then
                  () (* overwrite uid1 uid2 (List.combine nds1 nds2) *)

                else
                  let c = ref 0 in
                  let cands = ref [] in

                  List.iter2
                    (fun n1 n2 ->
                      let u1, u2 = n1#uid, n2#uid in
(*
  DEBUG_MSG " checking %a-%a" ups u1 ups u2;
 *)
                      try
                        let u1' = uidmapping#find u1 in
(*
  DEBUG_MSG " found: %a -> %a" ups u1 ups u1';
 *)
                        if u1' = u2 then
                          incr c
                        else
                          cands := (n1, n2) :: !cands
                      with
                        Not_found -> cands := (n1, n2) :: !cands
                    ) nds1 nds2;

                  match !cands with
                  | [] -> ()
                  | _ ->
                      let match_ratio = (float !c) /. (float sz) in

                      DEBUG_MSG "subtree pair %a-%a: %d nodes mapped (ratio=%f)"
                        ups uid1 ups uid2 !c match_ratio;

                      if match_ratio > options#subtree_match_ratio_threshold then begin
                        overwrite uid1 uid2 !cands
                      end
                      else begin
                        begin
                          try
                            let uid1' = uidmapping#find uid1 in
                            if not (List.memq uid1' mapped_uids2) && uid1' <> uid2 then begin
                              let mem_pairs =
                                List.fold_left
                                  (fun l u1 ->
                                    if u1 <> uid1 then
                                      try
                                        let u1' = uidmapping#find u1 in
                                        if not (List.memq u1' all_mapped_uids2) then
                                          (u1, u1')::l
                                        else
                                          l
                                      with
                                        Not_found -> l
                                    else
                                      l
                                  ) [] uids1
                              in
                              add_conflicting_pair
                                (nd1, tree2#search_node_by_uid uid1', [nd1], getroots unmapped2, mem_pairs,
                                 (fun () ->
                                   unmapped_extra1 := (nd1, nds1) :: !unmapped_extra1;
                                   raise Exit)
                                )
                            end
                          with
                            Not_found -> ()
                        end;

                        begin
                          try
                            let uid2' = uidmapping#inv_find uid2 in
                            if not (List.memq uid2' mapped_uids1) && uid2' <> uid1 then begin
                              let mem_pairs =
                                List.fold_left
                                  (fun l u2 ->
                                    if u2 <> uid2 then
                                      try
                                        let u2' = uidmapping#inv_find u2 in
                                        if not (List.memq u2' all_mapped_uids1) then
                                          (u2', u2)::l
                                        else
                                          l
                                      with
                                        Not_found -> l
                                    else
                                      l
                                  ) [] uids2
                              in
                              add_conflicting_pair
                                (tree1#search_node_by_uid uid2', nd2, getroots unmapped1, [nd2], mem_pairs,
                                 (fun () ->
                                   unmapped_extra2 := (nd2, nds2) :: !unmapped_extra2;
                                   raise Exit)
                                )
                            end
                          with
                            Not_found -> ()
                        end

                      end

              ) mapped2

          ) mapped1;


        let to_be_removed = Xset.create 0 in

        Hashtbl.iter
          (fun (cn1, cn2) (ns1, ns2, mem_pairs, act) ->

            DEBUG_MSG "conflicting pair: %a-%a" nups cn1 nups cn2;

            let pairs = combi ns1 ns2 in

            if List.length pairs < options#conflicting_pairs_threshold then begin

              BEGIN_DEBUG
                List.iter
                (fun (n1, n2) ->
                  DEBUG_MSG "  vs %a-%a" nups n1 nups n2
                ) pairs
                END_DEBUG;

              let ncross = ref (-1) in
              let adj = ref (-1.0) in
              try
                List.iter
                  (fun (n1, n2) ->
                    self#compare_mappings uidmapping
                      ?override:None ?bonus_self:None
                      n1 n2 ?ncrossing_old:None ?adjacency_old:None
                      (fun _ _ _ ->
                        Xset.add to_be_removed (cn1#uid, cn2#uid);
                        List.iter (Xset.add to_be_removed) mem_pairs;
                        act()
                      )
                      cn1 cn2 ?ncrossing_new:(Some ncross) ?adjacency_new:(Some adj)
                      (fun _ _ _ -> ())
                  ) pairs
              with
                Exit -> ()

            end

          ) conflicting_pairs_tbl;


        BEGIN_DEBUG
          Xset.iter
          (fun (u1, u2) ->
            DEBUG_MSG " to_be_removed: %a-%a" ups u1 ups u2;
          ) to_be_removed;
          let pr i ume =
            if ume <> [] then
              DEBUG_MSG " unmapped_extra%d: [%s]" i
                (Xlist.to_string (fun (n, _) -> UID.to_string n#uid) ";" ume)
          in
          pr 1 !unmapped_extra1;
          pr 2 !unmapped_extra2
        END_DEBUG;


        Xset.iter
          (fun (u1, u2) ->
            if uidmapping#remove u1 u2 then
              removed_pairs := (u1, u2) :: !removed_pairs
          ) to_be_removed;

        BEGIN_DEBUG
          List.iter
            (fun (n1, ns1) ->
              if List.for_all (fun n -> not (uidmapping#mem_dom n#uid)) ns1 then
                DEBUG_MSG "unmapped_extra1: %a -> OK" nups n1
              else
                DEBUG_MSG "unmapped_extra1: %a -> NG!" nups n1
            ) !unmapped_extra1;

          List.iter
            (fun (n2, ns2) ->
              if List.for_all (fun n -> not (uidmapping#mem_cod n#uid)) ns2 then
                DEBUG_MSG "unmapped_extra2: %a -> OK" nups n2
              else
                DEBUG_MSG "unmapped_extra2: %a -> NG!" nups n2
            ) !unmapped_extra2
        END_DEBUG;

        let unmapped1 = unmapped1 @ !unmapped_extra1 in
        let unmapped2 = unmapped2 @ !unmapped_extra2 in

        match unmapped1, unmapped2 with
        | [(n1, ns1)], [(n2, ns2)] ->
            let u1, u2 = n1#uid, n2#uid in
            let us1 = List.map (fun n -> n#uid) ns1 in
            let us2 = List.map (fun n -> n#uid) ns2 in

            DEBUG_MSG "adding: %a-%a (size=%d) (digest=%s) (%a-%a)"
              ups u1 ups u2 sz (try Digest.to_hex d with _ -> d) GI.ps n1#gindex GI.ps n2#gindex;

            incr count;
            List.iter2
              (fun u1 u2 ->
                check u1 u2;
                ignore (uidmapping#add_settled ~stable:false (* EXPERIMENTAL *) u1 u2);
                added_pairs := (u1, u2) :: !added_pairs
              ) us1 us2;
            uidmapping#add_settled_roots u1 u2;
(*        multiple_subtree_matches#remove d *)

        | [], ndmems | ndmems, [] ->
            if options#lock_matches_flag then begin (* lock residuals *)
              List.iter
                (fun (n, ns) ->

                  DEBUG_MSG "align: locking %a" nups n;

                  List.iter (fun n -> uidmapping#lock_uid n#uid) ns
                ) ndmems
            end;

(*        multiple_subtree_matches#remove d *)

        | _ ->
            if multi then begin (* we are somewhat gambling if multi=true! *)

              let adj_score (n1, ns1) (n2, ns2) =
                let s = self#get_adjacency_score n1 n2 in

                DEBUG_MSG "adj_score: %a-%a --> %f" nups n1 nups n2 s;

                s
              in

              let crossing_score (n1, ns1) (n2, ns2) =
                let s = -(uidmapping#count_crossing_or_incompatible_matches n1 n2) in

                DEBUG_MSG "crossing_score: %a-%a --> %d" nups n1 nups n2 s;

                s
              in

              let prox_score (n1, ns1) (n2, ns2) =
                let nprox = uidmapping#get_proximity n1 n2 in
                let s = nprox#primary_prox in

                DEBUG_MSG "prox_score: %a-%a --> %d" nups n1 nups n2 s;

                s
              in

              let a1 = Array.of_list unmapped1 in
              let a2 = Array.of_list unmapped2 in

              let selected =
                let score_f x y = (adj_score x y, crossing_score x y, prox_score x y) in
                let cmpr = new SMP.ComparatorFloatIntInt.c score_f a1 a2 in
                SMP.get_stable_matches cmpr a1 a2
              in

              BEGIN_DEBUG
                let ltos l =
                  String.concat ";" (List.map (fun (n, _) -> UID.to_string n#uid) l)
                in
                DEBUG_MSG "  unmapped1=[%s]" (ltos unmapped1);
                DEBUG_MSG "  unmapped2=[%s]" (ltos unmapped2);

                if selected <> [] then begin
                  DEBUG_MSG "selected pairs:";
                  List.iter
                    (fun ((n1, ns1), (n2, ns2)) ->
                      DEBUG_MSG "%a-%a" nups n1 nups n2
                    ) selected
                end
              END_DEBUG;

              List.iter
                (fun ((n1, ns1), (n2, ns2)) ->

                  DEBUG_MSG "[MULTI] adding: %a-%a (size=%d) (digest=%s) (%a-%a)"
                    nups n1 nups n2 sz (try Digest.to_hex d with _ -> d)
                    GI.ps n1#gindex GI.ps n2#gindex;

                  List.iter2
                    (fun n1 n2 ->
                      incr count;
                      let u1, u2 = n1#uid, n2#uid in

                      DEBUG_MSG "adding: %a-%a (%a-%a)"
                        ups u1 ups u2 GI.ps n1#gindex GI.ps n2#gindex;
                      check u1 u2;
                      ignore (uidmapping#add_unsettled u1 u2);
                      added_pairs := (u1, u2) :: !added_pairs

                    ) ns1 ns2;


                  (* lock residuals *)
                  if options#lock_matches_flag then begin
                    let _selected1, _selected2 = List.split selected in
                    let selected1 = List.map (fun (n, _) -> n) _selected1 in
                    let selected2 = List.map (fun (n, _) -> n) _selected2 in
                    List.iter
                      (fun (n, ns) ->
                        if not (List.memq n selected1) then begin

                          DEBUG_MSG "align: locking %a" nups n;

                          List.iter (fun n -> uidmapping#lock_uid n#uid) ns
                        end
                      ) unmapped1;
                    List.iter
                      (fun (n, ns) ->
                        if not (List.memq n selected2) then begin

                          DEBUG_MSG "align: locking %a" nups n;

                          List.iter (fun n -> uidmapping#lock_uid n#uid) ns
                        end
                      ) unmapped2
                  end;

(*              multiple_subtree_matches#remove d *)

                ) selected

            end
      );

    multiple_subtree_matches#iter
      (fun (d, ndmems1, ndmems2, _) ->
        let cnt =
          List.fold_left
            (fun c (nd1, nds1) ->
              if uidmapping#mem_settled nd1#uid then
                c + 1
              else
                c
            ) 0 ndmems1
        in
        if cnt = min (List.length ndmems1) (List.length ndmems2) then
          multiple_subtree_matches#remove d
      );

    DEBUG_MSG "%d pairs added by multiple subtree matches." !count;

    if options#multi_node_match_flag then begin

      (*let find_nearest_anc_stmt =
        Sourcecode.find_nearest_p_ancestor_node (fun n -> n#data#is_statement)
      in
      let group_node_matches l1 l2 =
        let pair_list = ref [] in

        let gtbl1 = Hashtbl.create 0 in
        let gtbl2 = Hashtbl.create 0 in
        let others1 = ref [] in
        let others2 = ref [] in

        let group gtbl others l =
          List.iter
            (fun x ->
              try
                let s = find_nearest_anc_stmt x in
                try
                  let xl = Hashtbl.find gtbl s  in
                  Hashtbl.replace gtbl s (x::xl)
                with
                  Not_found -> Hashtbl.add gtbl s [x]
              with
                _ -> others := x :: !others
            ) l
        in
        group gtbl1 others1 l1;
        group gtbl2 others2 l2;

        Hashtbl.iter
          (fun s1 nl1 ->
            try
              let s2 = tree2#search_node_by_uid (uidmapping#find s1#uid) in
              let nl2 = Hashtbl.find gtbl2 s2 in
              DEBUG_MSG "s1=%a s2=%a" nups s1 nups s2;
              DEBUG_MSG "  nl1=[%a]" nsps nl1;
              DEBUG_MSG "  nl2=[%a]" nsps nl2;
              pair_list := (nl1, nl2) :: !pair_list
            with
              _ -> ()
          ) gtbl1;

        if !others1 <> [] && !others1 <> [] then begin
          DEBUG_MSG "OTHERS:";
          DEBUG_MSG "  nl1=[%a]" nsps !others1;
          DEBUG_MSG "  nl2=[%a]" nsps !others2;
          pair_list := (!others1, !others2) :: !pair_list
        end;

        !pair_list
      in*)

      count := 0;

      multiple_node_matches#iter
        (fun (_lab, nds1, nds2) ->
          let l1 = List.filter (fun nd -> not (uidmapping#mem_dom nd#uid)) nds1 in
          let l2 = List.filter (fun nd -> not (uidmapping#mem_cod nd#uid)) nds2 in

          match l1, l2 with
          | [], _ | _, [] -> ()
                (* multiple_node_matches#remove _lab *)

          | [nd1], [nd2] ->
              let u1, u2 = nd1#uid, nd2#uid in

              DEBUG_MSG "node match (%s): |nds1|=1 |nds2|=1"
                (multiple_node_matches#label_to_string _lab);
              DEBUG_MSG "adding: %a-%a (%a-%a)"
                ups u1 ups u2 GI.ps nd1#gindex GI.ps nd2#gindex;

              incr count;
              check u1 u2;
              ignore (uidmapping#add_unsettled u1 u2);
              added_pairs := (u1, u2) :: !added_pairs;
              (* multiple_node_matches#remove _lab *)

          | nd1::_, nd2::_ -> begin
              let len1 = List.length l1 in
              let len2 = List.length l2 in

              DEBUG_MSG "node match (%s): |nds1|=%d |nds2|=%d"
                (multiple_node_matches#label_to_string (Obj.obj _lab)) len1 len2;

              (*if nd1#data#is_op then begin
                if multi_node then begin
                  List.iter
                    (fun (nl1, nl2) ->
                      match nl1, nl2 with
                      | [nd1], [nd2] -> begin
                          let u1, u2 = nd1#uid, nd2#uid in

                          DEBUG_MSG "node match (%s): |nds1|=1 |nds2|=1"
                            (multiple_node_matches#label_to_string _lab);
                          DEBUG_MSG "adding: %a-%a (%a-%a)"
                            ups u1 ups u2 GI.ps nd1#gindex GI.ps nd2#gindex;

                          incr count;
                          check u1 u2;
                          ignore (uidmapping#add_unsettled u1 u2);
                          added_pairs := (u1, u2) :: !added_pairs;

                      end
                      | _ -> ()
                    ) (group_node_matches l1 l2)
                end
              end
              else *)if nd1#data#is_named then begin

                if multi_node then begin

                  let thresh = options#prematch_cands_threshold in

                  if len1 <= thresh && len2 <= thresh then begin

                    DEBUG_MSG "nds1=[%s] nds2=[%s]"
                      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" l1)
                      (Xlist.to_string (fun n -> UID.to_string n#uid) ";" l2);

                    let a1 = Array.of_list l1 in
                    let a2 = Array.of_list l2 in

                    let under_permutation_hub =
                      Array.for_all self#under_permutation_hub1 a1 &&
                      Array.for_all self#under_permutation_hub2 a2
                    in
                    DEBUG_MSG "under_permutation_hub=%B" under_permutation_hub;

                    let crossing_score n1 n2 =
                      let s = -(uidmapping#count_crossing_or_incompatible_matches n1 n2) in
                      DEBUG_MSG "crossing_score: %a-%a --> %d" nups n1 nups n2 s;
                      s
                    in

                    let selected =
                      if under_permutation_hub then
                        let score_f x y = (self#get_adjacency_score x y, crossing_score x y) in
                        let cmpr = new SMP.ComparatorFloatInt.c score_f a1 a2 in
                        SMP.get_stable_matches cmpr a1 a2
                      else
                        let score_f x y = (crossing_score x y, self#get_adjacency_score x y) in
                        let cmpr = new SMP.ComparatorIntFloat.c score_f a1 a2 in
                        SMP.get_stable_matches cmpr a1 a2
                    in
                    List.iter
                      (fun (n1, n2) ->
                        let u1, u2 = n1#uid, n2#uid in

                        DEBUG_MSG "adding: %a-%a (%a-%a)"
                          ups u1 ups u2 GI.ps n1#gindex GI.ps n2#gindex;

                        incr count;
                        check u1 u2;
                        ignore (uidmapping#add_unsettled u1 u2);
                        added_pairs := (u1, u2) :: !added_pairs
                      ) selected

                  end
                  else begin
                    try
                      let pa1 = uidmapping#partition_nodes1 l1 in
                      DEBUG_MSG "---";
                      let pa2 = uidmapping#partition_nodes2 l2 in

                      Array.iteri
                        (fun i ns1 ->
                          match ns1, pa2.(i) with
                          | [n1], [n2] ->
                              let u1, u2 = n1#uid, n2#uid in

                              DEBUG_MSG "adding: %a-%a (%a-%a)"
                                ups u1 ups u2 GI.ps n1#gindex GI.ps n2#gindex;

                              incr count;
                              check u1 u2;
                              ignore (uidmapping#add_unsettled u1 u2);
                              uidmapping#add_stable_pair u1 u2;
                              added_pairs := (u1, u2) :: !added_pairs

                          | _ -> ()
                        ) pa1
                    with
                      Invalid_argument _ -> ()

                  end

                end (* of if multi_node *)
              end (* of nd1#data#is_named *)
          end
        );

      DEBUG_MSG "%d pairs added by multiple node matches." !count

    end; (* if options#multi_node_match_flag *)

    let added_then_removed_pairs = Xlist.intersection !removed_pairs !added_pairs in
    BEGIN_DEBUG
      List.iter
      (fun (u1, u2) ->
        DEBUG_MSG "added then removed pair: %a-%a" ups u1 ups u2
      ) added_then_removed_pairs
    END_DEBUG;
    if added_then_removed_pairs <> [] then begin
      removed_pairs := Xlist.subtract !removed_pairs added_then_removed_pairs;
      added_pairs := Xlist.subtract !added_pairs added_then_removed_pairs
    end;

    BEGIN_DEBUG
      List.iter
        (fun (u1, u2) ->
          let n1 = tree1#search_node_by_uid u1 in
          let n2 = tree2#search_node_by_uid u2 in
          DEBUG_MSG "removed_pair: %a-%a (%a-%a)" ups u1 ups u2 GI.ps n1#gindex GI.ps n2#gindex;
        ) !removed_pairs;
      List.iter
        (fun (u1, u2) ->
          let n1 = tree1#search_node_by_uid u1 in
          let n2 = tree2#search_node_by_uid u2 in
          DEBUG_MSG "added_pair: %a-%a (%a-%a)" ups u1 ups u2 GI.ps n1#gindex GI.ps n2#gindex;
        ) !added_pairs
    END_DEBUG;

    Xprint.verbose options#verbose_flag "    elaborating completed.";

    !removed_pairs, !added_pairs

    (* end of method elaborate_uidmapping *)



end (* of class Comparison.c *)

