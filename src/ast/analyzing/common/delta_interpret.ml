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
(* delta_interpret.ml *)

module GI    = Otreediff.GIndex
module Otree = Otreediff.Otree

open Delta_base
open Delta_common

let cmp_idx x y =
  if x = y then
    0
  else
    match x, y with
    | (Some x0, Some x1), (Some y0, Some y1) -> begin
        if x0 = x1 && y0 = y1 then
          Stdlib.compare x0 y0
        else if x1 < y0 then
          -1
        else if y1 < x0 then
          1
        else
          0
    end
    | (_, Some x1), (Some y0, _) -> begin
        if x1 < y0 then
          -1
        else
          0
    end
    | (Some x0, _), (_, Some y1) -> begin
        if y1 < x0 then
          1
        else
          0
    end
    | _ -> 0

let get_pos p x =
  let idx = ref (-1) in
  begin
    try
      Array.iteri
        (fun i c ->
          if c == x then begin
            idx := i;
            raise Exit
          end
        ) p#initial_children
    with
      Exit -> ()
  end;
  !idx

let _is_anc a n =
  let rec scan x =
    let px = x#initial_parent in
    if px == a then
      true, get_pos px x
    else
      scan px
  in
  try
    scan n
  with
    _ -> false, (-1)

let is_anc a n =
  let b, _ = _is_anc a n in
  b

let _get_latest_common_ancestor nd0 nd1 =
  DEBUG_MSG "nd0=%a nd1=%a" nps nd0 nps nd1;
  let b, i = _is_anc nd0 nd1 in
  if b then
    nd0, -1, i
  else
    let b, i = _is_anc nd1 nd0 in
    if b then
      nd1, i, -1
    else
      let cur = ref nd0 in
      let li = ref (-1) in
      let ri = ref (-1) in
      try
        while true do
          let pnd0 = !cur#initial_parent in
          let b, i = _is_anc pnd0 nd1 in
          if b then begin
            li := get_pos pnd0 !cur;
            ri := i;
            cur := pnd0;
            raise Exit
          end;
          cur := pnd0
        done;
        raise Not_found
      with
      | Exit -> !cur, !li, !ri
      | _ -> raise Not_found

let node_compare nd0 nd1 =
  let a, i0, i1 = _get_latest_common_ancestor nd0 nd1 in
  DEBUG_MSG "a=%a i0=%d i1=%d" nps a i0 i1;
  let r =
    if i0 >= 0 && i1 >= 0 then
      Stdlib.compare i0 i1
    else
      0
  in
  DEBUG_MSG "nd0=%a nd1=%a -> %d" nps nd0 nps nd1 r;
  r

let elem_compare e0 e1 =
  (*compare (float e0.Elem.pos +. e0.Elem.ofs) (float e1.Elem.pos +. e1.Elem.ofs)*)
  let c = compare e0.Elem.pos e1.Elem.pos in
  if c = 0 then
    compare e0.Elem.ofs e1.Elem.ofs
  else
    c

let path_compare ?(weak=false) path0 path1 =
  match path0, path1 with
  | Path.PATH el0, Path.PATH el1 -> begin
      let rec scan = function
        (*| e0::t0, e1::t1 when t0 = [] || t1 = [] -> Elem.compare e0 e1*)
        | e0::t0, e1::t1 -> begin
            let c = elem_compare e0 e1 in
            if c = 0 then
              scan (t0, t1)
            else
              c
        end
        | _::_, [] | [], _::_ when weak -> 0
        | _::_, [] -> -1
        | [], _::_ -> 1
        | [], [] -> 0
      in
      scan (List.rev el0, List.rev el1)
  end

let int_set_to_string s =
  (String.concat
     ";"
     (List.map string_of_int
        (List.fast_sort compare (Xset.to_list s))))

class ['tree] interpreter (tree : 'tree) = object (self)

  val op_tbl = Hashtbl.create 0 (* uid -> mutation list *)

  (* stid -> subtree *)
  val subtree_tbl = (Hashtbl.create 0 : (subtree_id, 'tree) Hashtbl.t)

  (* mid -> subtree *)
  val copied_subtree_tbl = (Hashtbl.create 0 : (MID.t, 'tree) Hashtbl.t)
  val copied_subtree_sz_tbl = (Hashtbl.create 0 : (MID.t, int) Hashtbl.t)

  val del_tbl = Hashtbl.create 0 (* node -> (node * node list) *)

  val marked_keys = Xset.create 0

  val mask_tbl = Hashtbl.create 0 (* key -> index list *)

  val upstream_node_tbl = Hashtbl.create 0 (* node -> count *)
  val quasi_upstream_node_tbl = Hashtbl.create 0 (* node -> count *)

  val parent_key_tbl = Hashtbl.create 0 (* node -> key *)
  val parent_tbl = Hashtbl.create 0 (* node -> (node * pos * ofs) *)
  val parent_resolver_tbl = Hashtbl.create 0 (* node -> unit -> (node * pos * ofs) *)
  val lift_point_tbl = Hashtbl.create 0 (* node -> pos -> ofs *)

  val key_tbl = Hashtbl.create 0 (* node -> key *)

  val del_spec_tbl = Hashtbl.create 0 (* key -> (path * path list) *)
  val sub_del_spec_tbl = Hashtbl.create 0 (* mid -> (path * path list) *)

  val deleted_subtree_tbl = Hashtbl.create 0 (* node -> key *)

  val deleted_mems_tbl = Hashtbl.create 0 (* node -> (node * node list) list *)

  val canceled_dels = Xset.create 0

  val excluded_paths_tbl = Hashtbl.create 0 (* mid -> (node * pos list) *)
  val frontier_tbl = Hashtbl.create 0 (* mid -> (node * pos) list *)

  val key_link_tbl = Hashtbl.create 0

  val boundary_tbl = Hashtbl.create 0 (* key -> boundary *)

  val roots_of_upstream_staying_move = Xset.create 0
  val extra_upstream_roots = Xset.create 0

  val immediately_pruned_nodes = Xset.create 0

  val forced_upstream_nodes = Xset.create 0
  val upstream_dest_tbl = Hashtbl.create 0 (* node -> key *)

  val junc_nodes = Xset.create 0

  val staying_moves = Xset.create 0

  val move_relabel_tbl = Hashtbl.create 0 (* node -> unit *)

  val mutable deferred_relabel_list = [] (* unit -> unit *)

  val pos_trans_tbl = Hashtbl.create 0 (* node -> (pos, ofs) -> node *)
  val extra_pos_trans_tbl = Hashtbl.create 0 (* node -> (pos, ofs) -> node *)

  val nodes_to_be_shifted = Xset.create 0

  val pos_shift_tbl = Hashtbl.create 0 (* for final pos shift *)

  val recovered_nodes = Xset.create 0

  val path_tbl = Hashtbl.create 0 (* node -> Path.t *)
  val path_depth_tbl = Hashtbl.create 0 (* node -> int *)
  val path_key_tbl = Hashtbl.create 0 (* node -> parent key *)

  val composition_tbl = Hashtbl.create 0

  (*val no_trans_mutations = Xset.create 0*)

  method add_deferred_relabel f = deferred_relabel_list <- f::deferred_relabel_list

  method add_move_relabel nd f = Hashtbl.add move_relabel_tbl nd f

  method do_deferred_relabels() =
    DEBUG_MSG "performing deferred relabels...";
    List.iter (fun f -> f()) deferred_relabel_list;
    DEBUG_MSG "done."

  method private get_path nd =
    DEBUG_MSG "nd=%a" nps nd;
    let result =
      if self#is_stable nd then
        nd#apath
      else
        let path = Hashtbl.find path_tbl nd in
        DEBUG_MSG "path=%s" path#to_string;
        match try Some (Hashtbl.find path_key_tbl nd) with Not_found -> None with
        | Some pk -> begin
            DEBUG_MSG "pk=%s" (key_to_string pk);
            let st = self#find_subtree pk in
            DEBUG_MSG "st#root=%a" nps st#root;
            let ppath = Hashtbl.find path_tbl st#root in
            DEBUG_MSG "ppath=%s" ppath#to_string;
            let d = try Hashtbl.find path_depth_tbl nd with Not_found -> assert false in
            let rp = get_rel_path (Path.head path#path (-d)) path#path in
            DEBUG_MSG "d=%d rp=%s" d (Path.to_string rp);
            Path.concat ppath#path rp
        end
        | None -> begin
            match self#find_key_opt nd with
            | Some K_stable -> path#path
            | Some k -> begin
                DEBUG_MSG "k=%s" (key_to_string k);
                if self#is_marked_key k then
                  try
                    match self#get_boundary k with
                    | p::_ -> Path.concat path#path p#path
                    | _ -> assert false
                  with
                    Not_found -> path#path
                else
                  path#path
            end
            | None -> path#path
        end
    in
    DEBUG_MSG "%a -> %s" nps nd (Path.to_string result);
    result

  method private add_composition pk k =
    Hashtbl.replace composition_tbl k pk

  method private is_ancestor_key ak k =
    let rec f x =
      let x' = Hashtbl.find composition_tbl x in
      if x' = ak then
        true
      else
        f x'
    in
    try
      f k
    with
      _ -> false

  method private reg_pos_shift nd i =
    DEBUG_MSG "%a -> %d" nps nd i;
    Hashtbl.add pos_shift_tbl nd i

  method private is_staying_move m =
    Xset.mem staying_moves m

  method private is_junc_node nd =
    Xset.mem junc_nodes nd

  method private force_upstream ?(key_opt=None) nd =
    DEBUG_MSG "%a: forced upstream (key_opt=%s)" nps nd (key_opt_to_string key_opt);
    Xset.add forced_upstream_nodes nd;
    match key_opt with
    | Some k -> Hashtbl.add upstream_dest_tbl nd k
    | None -> ()

  method private unforce_upstream nd =
    DEBUG_MSG "%a: unforced" nps nd;
    Xset.remove forced_upstream_nodes nd;
    Hashtbl.remove upstream_dest_tbl nd

  method private set_upstream_dest nd k =
    DEBUG_MSG "nd=%a k=%s" nps nd (key_to_string k);
    Hashtbl.add upstream_dest_tbl nd k

  method private get_upstream_dest = Hashtbl.find upstream_dest_tbl

  method private is_forced_upstream nd =
    let b = Xset.mem forced_upstream_nodes nd in
    DEBUG_MSG "%a -> %B" nps nd b;
    b

  method private get_deleted_mems nd =
    Hashtbl.find deleted_mems_tbl nd

  method private has_deleted_mems nd =
    Hashtbl.mem deleted_mems_tbl nd

  method private reg_boundary key paths =
    if paths <> [] then
      Hashtbl.add boundary_tbl key paths

  method private get_boundary key =
    Hashtbl.find boundary_tbl key

  method private link_keys key0 key1 =
    try
      let kl = Hashtbl.find key_link_tbl key0 in
      if not (List.mem key1 kl) then
        raise Not_found
    with
      Not_found ->
        DEBUG_MSG "%s -> %s" (key_to_string key0) (key_to_string key1);
        tbl_add key_link_tbl key0 key1

  method private get_child_keys key =
    try
      Hashtbl.find key_link_tbl key
    with
      Not_found -> []

  method private is_deleted nd = Hashtbl.mem deleted_subtree_tbl nd

  method private _find_key_of_deleted nd =
    match Hashtbl.find deleted_subtree_tbl nd with
    | Some k -> k
    | None -> raise Not_found

  method private _find_key_opt_of_deleted nd =
    try
      Hashtbl.find deleted_subtree_tbl nd
    with
      Not_found -> None

  method find_key_opt_of_deleted nd =
    let key_opt = self#_find_key_opt_of_deleted nd in
    match key_opt with
    | Some (K_del _) -> None
    | _ -> key_opt

  method private find_key_of_deleted nd =
    match self#find_key_opt_of_deleted nd with
    | Some k -> k
    | None -> raise Not_found

  method private is_stable nd =
    try
      match self#find_key nd with
      | K_stable -> true
      | _ -> false
    with
      Not_found -> not (self#is_deleted nd)

  method private find_key nd = Hashtbl.find key_tbl nd

  method private remove_key = Hashtbl.remove key_tbl

  method private find_key_opt nd =
    try
      Some (Hashtbl.find key_tbl nd)
    with
      Not_found -> None

  method private is_insert nd =
    let b =
      try
        match self#find_key nd with
        | K_stable -> false
        | _ -> true
      with
        Not_found -> false
    in
    DEBUG_MSG "%a -> %B" nps nd b;
    b

  method private find_parent_key nd = Hashtbl.find parent_key_tbl nd

  method private find_parent_key_opt nd =
    try
      Some (self#find_parent_key nd)
    with
      Not_found -> None

  method private has_parent_key_stable nd =
    let b =
    try
      match self#find_parent_key nd with
      | K_stable -> true
      | _ -> false
    with
      Not_found -> false
    in
    DEBUG_MSG "%a -> %B" nps nd b;
    b

  method private has_parent_key nd = Hashtbl.mem parent_key_tbl nd

  method private find_parent nd =
    try
      Hashtbl.find parent_tbl nd
    with
      Not_found ->
        let resolver = Hashtbl.find parent_resolver_tbl nd in
        resolver()

  method private has_parent nd =
    Hashtbl.mem parent_tbl nd

  method private add_to_parent_tbl nd ((n, p, o) as v) =
    Hashtbl.add parent_tbl nd v;
    tbl_add_tbl lift_point_tbl n p o

  method private register_parent_resolver nd f =
    Hashtbl.add parent_resolver_tbl nd f

  method private remove_from_parent_tbl nd =
    while Hashtbl.mem parent_tbl nd do
      Hashtbl.remove parent_tbl nd
    done

  method private remove_from_parent_key_tbl nd =
    while Hashtbl.mem parent_key_tbl nd do
      Hashtbl.remove parent_key_tbl nd
    done

  method private reg_extra_upstream_root nd =
    DEBUG_MSG "extra_upstream_root: %s" nd#initial_to_string;
    Xset.add extra_upstream_roots nd

  method private is_extra_upstream_root nd =
    Xset.mem extra_upstream_roots nd

  method private reg_root_of_upstream_staying_move nd =
    DEBUG_MSG "registering upstream_staying_move: %s" nd#initial_to_string;
    Xset.add roots_of_upstream_staying_move nd

  method private unreg_root_of_upstream_staying_move nd =
    DEBUG_MSG "unregistering upstream_staying_move: %s" nd#initial_to_string;
    Xset.remove roots_of_upstream_staying_move nd

  method private is_root_of_upstream_staying_move nd =
    Xset.mem roots_of_upstream_staying_move nd

  method private reg_upstream_node nd count =
    DEBUG_MSG "upstream_node: %s (count=%d)" nd#initial_to_string count;
    Hashtbl.add upstream_node_tbl nd count

  method private reg_quasi_upstream_node nd count =
    DEBUG_MSG "quasi_upstream_node: %s (count=%d)" nd#initial_to_string count;
    Hashtbl.add quasi_upstream_node_tbl nd count

  method private get_upstream_count nd =
    try
      Hashtbl.find upstream_node_tbl nd
    with
      Not_found -> 0

  method private get_quasi_upstream_count nd =
    try
      Hashtbl.find quasi_upstream_node_tbl nd
    with
      Not_found -> 0

  method private has_key_opt key_opt nd =
    (self#find_key_opt nd) = key_opt ||
    (self#find_key_opt_of_deleted nd) = key_opt

  method private is_upstream_node nd =
    (self#get_upstream_count nd) > 0

  method private mark_key key =
    DEBUG_MSG "key=%s" (key_to_string key);
    Xset.add marked_keys key

  method private is_marked_key key = Xset.mem marked_keys key

  method private set_mask key pos s =
    DEBUG_MSG "key=%s pos=%d s=[%s]" (key_to_string key) pos (int_set_to_string s);
    Hashtbl.add mask_tbl key (pos, s)

  method private get_mask key pos =
    DEBUG_MSG "key=%s pos=%d" (key_to_string key) pos;
    try
      let pos0, s0 = Hashtbl.find mask_tbl key in
      let d = pos - pos0 in
      DEBUG_MSG "pos0=%d s0=[%s] d=%d" pos0 (int_set_to_string s0) d;
      let s1 =
        if d > 0 then
          Xset.filter_map
            (fun i ->
              let j = i - d in
              if j >= 0 then
                Some j
              else
                None
            ) s0
        else
          s0
      in
      DEBUG_MSG "s1=[%s]" (int_set_to_string s1);
      s1
    with
      Not_found -> Xset.create 0

  method find_subtree = function
    | K_stid stid -> Hashtbl.find subtree_tbl stid
    | K_mid mid   -> Hashtbl.find copied_subtree_tbl mid
    | K_stable    -> raise Not_found
    | K_del _     -> raise Not_found

  method find_copied_subtree_size = function
    | K_mid mid -> Hashtbl.find copied_subtree_sz_tbl mid
    | _ -> raise Not_found

  method mutate_tree t =
    DEBUG_MSG "root=%a" nps t#root;
    t#mutate op_tbl

  method init_mutation nodes =
    Hashtbl.clear op_tbl;
    Xset.iter (fun nd -> nd#init_mutation()) nodes

  method private compare_nodes ?(get_idx_opt=None) =
    let get_idx =
      match get_idx_opt with
      | Some f -> f
      | None ->
          fun n ->
            let idx =
              if self#is_stable n then
                Some (float n#gindex), Some (float n#gindex)
              else begin
                let moveon x = not (self#is_stable x) in
                let pred x =
                  let b =
                  self#is_stable x &&
                  match self#find_parent_key_opt x with
                  | Some (K_stid _ | K_mid _) -> (*false*)self#get_quasi_upstream_count x > 0
                  | Some K_stable -> (*false*)self#get_quasi_upstream_count x > 0
                  | _ -> true
                  in
                  (*DEBUG_MSG "%a -> %B" nps x b;*)
                  b
                in
                let ss0 = get_p_descendants ~moveon pred n in
                match ss0 with
                | [] -> begin
                    let ss1 = get_p_descendants ~moveon pred n in
                    match ss1 with
                    | [] -> None, None
                    | _ ->
                        let st, ed = get_range (List.map (fun x -> x#gindex) ss1) in
                        Some (float st), Some (float ed)
                end
                | _ ->
                    let st, ed = get_range (List.map (fun x -> x#gindex) ss0) in
                    Some (float st), Some (float ed)
              end
            in
            DEBUG_MSG "%a -> %s" nps n
              (match idx with
              | Some st, Some ed -> sprintf "%f-%f" st ed
              | Some st, None -> sprintf "%f-" st
              | None, Some ed -> sprintf "-%f" ed
              | _ -> "?"
              );
            idx
    in
    fun n0 n1 ->
      let r = cmp_idx (get_idx n0) (get_idx n1) in
      let r =
        if r = 0 then
          try
            let p0 = self#get_path n0 in
            let p1 = self#get_path n1 in
            path_compare p0 p1
          with
            _ -> compare n0#gindex n1#gindex
        else
          r
      in
      DEBUG_MSG "%a %a -> %d" nps n0 nps n1 r;
      r

  method private sort_op_tbl ?(get_idx_opt=None) () =

    let chk ms =
      List.exists
        (function
          | Otree.CMinsert(_, _, ofs, _, _) when ofs = 0.0 -> true
          | _ -> false
        ) ms
    in

    let decompose_prune m =
      let ml = ref [] in
      begin
        match m with
        | Otree.CMprune(pos, xl) -> begin
            let insl =
              List.map
                (fun x ->
                  Otree.CMinsert(false, pos, 0.0, x, [])
                ) xl
            in
            ml := (Otree.CMprune(pos, [])) :: insl
        end
        | _ -> ml := [m]
      end;
      BEGIN_DEBUG
        if List.length !ml > 1 then begin
          DEBUG_MSG "before: %s" (Otree.cluster_mutation_to_string m);
          List.iter
            (fun m0 ->
              DEBUG_MSG " after: %s" (Otree.cluster_mutation_to_string m0);
            ) !ml
        end
      END_DEBUG;
      !ml
    in

    (*let compare_nodes =
      let get_idx n =
        if self#is_stable n then
          n#gindex
        else
          match get_p_descendants self#is_stable n with
          | [] -> raise Not_found
          | h::_ -> h#gindex
      in
      fun n0 n1 ->
        try
          Stdlib.compare (get_idx n0) (get_idx n1)
        with
          Not_found -> 0
    in*)
    (*let get_idx =
      match get_idx_opt with
      | Some f -> f
      | None ->
          fun n ->
            let idx =
              if self#is_stable n then
                Some (float n#gindex), Some (float n#gindex)
              else begin
                let moveon x = not (self#is_stable x) in
                let pred x =
                  let b =
                  self#is_stable x &&
                  match self#find_parent_key_opt x with
                  | Some (K_stid _ | K_mid _) -> (*false*)self#get_quasi_upstream_count x > 0
                  | Some K_stable -> (*false*)self#get_quasi_upstream_count x > 0
                  | _ -> true
                  in
                  DEBUG_MSG "%a -> %B" nps x b;
                  b
                in
                let ss0 = get_p_descendants ~moveon pred n in
                match ss0 with
                | [] -> begin
                    let ss1 = get_p_descendants ~moveon pred n in
                    match ss1 with
                    | [] -> None, None
                    | _ ->
                        let st, ed = get_range (List.map (fun x -> x#gindex) ss1) in
                        Some (float st), Some (float ed)
                end
                | _ ->
                    let st, ed = get_range (List.map (fun x -> x#gindex) ss0) in
                    Some (float st), Some (float ed)
              end
            in
            DEBUG_MSG "%a -> %s" nps n
              (match idx with
              | Some st, Some ed -> sprintf "%f-%f" st ed
              | Some st, None -> sprintf "%f-" st
              | None, Some ed -> sprintf "-%f" ed
              | _ -> "?"
              );
            idx
    in
    let compare_nodes n0 n1 =
      let r = cmp_idx (get_idx n0) (get_idx n1) in
      let r =
        if r = 0 then
          try
            let p0 = self#get_path n0 in
            let p1 = self#get_path n1 in
            (*Path.*)path_compare p0 p1
          with
            _ -> compare n0#gindex n1#gindex
        else
          r
      in
      DEBUG_MSG "%a %a -> %d" nps n0 nps n1 r;
      r
    in*)
    let compare_nodes = self#compare_nodes ~get_idx_opt in
    let cmp m0 m1 =
      match m0, m1 with
      | Otree.CMinsert(_, pos0, ofs0, r0, _), Otree.CMinsert(_, pos1, ofs1, r1, _) -> begin
          if pos0 = pos1 then
            if ofs0 = ofs1 then
              compare_nodes r0 r1
            else
              compare ofs0 ofs1
          else
            compare pos0 pos1
      end
      | Otree.CMinsert _, Otree.CMprune _ -> -1
      | Otree.CMprune _, Otree.CMinsert _ -> 1
      | Otree.CMprune(pos0, _), Otree.CMprune(pos1, _) -> compare pos0 pos1
    in
    Hashtbl.iter
      (fun u ms ->
        let ms =
          if chk ms then
            List.flatten (List.map decompose_prune ms)
          else
            ms
        in
        let sorted = List.stable_sort cmp ms in
        Hashtbl.replace op_tbl u sorted
      ) op_tbl


  method private get_nodes_from_uid u =
    let nds = ref [] in
    begin
      try
        nds := (tree#search_node_by_uid u) :: !nds
      with
        Not_found ->
          Hashtbl.iter
            (fun stid t ->
              try
                nds := (t#search_node_by_uid u) :: !nds
              with
                Not_found -> ()
            ) subtree_tbl;
          Hashtbl.iter
            (fun mid t ->
              try
                nds := (t#search_node_by_uid u) :: !nds
              with
                Not_found -> ()
            ) copied_subtree_tbl
    end;
    !nds

  method private setup_pos_trans_tbl() =
    let tbl_add nd pos ofs sr =
      tbl_add_tbl2 pos_trans_tbl nd pos ofs sr;
    in
    let tbl_get nd pos ofs =
      Hashtbl.find (Hashtbl.find (Hashtbl.find pos_trans_tbl nd) pos) ofs
    in
    let get_node_list () = Hashtbl.fold (fun nd _ l -> nd :: l) pos_trans_tbl [] in
    Hashtbl.iter
      (fun u ms ->
        let nds = self#get_nodes_from_uid u in
        List.iter
          (fun nd ->
            DEBUG_MSG "nd=%a" nps nd;
            List.iter
              (fun m ->
                match m with
                | Otree.CMinsert(_, pos, ofs, subroot, fnode_felem_list) -> begin
                    DEBUG_MSG "m=%s" (Otree.cluster_mutation_to_string m);
                    tbl_add nd pos ofs subroot;
                    if ofs = 0.0 then
                      List.iteri
                        (fun i (n, e) ->
                          let p = e.Elem.pos in
                          if p >= 0 then
                            try
                              let c = nd#initial_children.(pos+i) in
                              DEBUG_MSG "%a %d %f %a" nps n p e.Elem.ofs nps c;
                              tbl_add n p e.Elem.ofs c
                            with
                              _ -> Xprint.warning "setup_pos_trans_tbl: %s\nm=%s"
                                  nd#initial_to_string (Otree.cluster_mutation_to_string m)
                        ) fnode_felem_list
                end
                | _ -> ()
              ) ms
          ) nds
      ) op_tbl;

    BEGIN_DEBUG
      DEBUG_MSG "lift_point_tbl:";
      Hashtbl.iter
        (fun nd tbl ->
          Hashtbl.iter
            (fun p o ->
              DEBUG_MSG "%a -> %d -> %f" nps nd p o
            ) tbl
        ) lift_point_tbl
    END_DEBUG;

    List.iter (*!!!!!!*)
      (fun n ->
        try
          let otbl = Hashtbl.find lift_point_tbl n in
          DEBUG_MSG "n=%a" nps n;
          Array.iteri
            (fun i c ->
              DEBUG_MSG "i=%d, c=%a" i nps c;
              if Hashtbl.mem otbl i then
                try
                  ignore (tbl_get n i 0.0)
                with
                  Not_found ->
                    try
                      let nk = self#find_key n in
                      if nk <> K_stable && (self#find_key c) = nk then begin
                        DEBUG_MSG "%a -> %d -> 0.0 -> %a" nps n i nps c;
                        tbl_add_tbl2 extra_pos_trans_tbl n i 0.0 c;
                      end
                    with
                      Not_found -> ()
            ) n#initial_children
        with
          Not_found -> ()

      ) (get_node_list());

    BEGIN_DEBUG
    Hashtbl.iter
      (fun nd tbl ->
        DEBUG_MSG "%s" nd#initial_to_string;
        Hashtbl.iter
          (fun pos tbl2 ->
            DEBUG_MSG " pos=%d" pos;
            let binds =
              Hashtbl.fold (fun ofs c l -> (ofs, c) :: l) tbl2 []
            in
            List.iter
              (fun (ofs, c) ->
                DEBUG_MSG "  %f -> %a" ofs nps c
              ) (List.fast_sort (fun (a, _) (b, _) -> Stdlib.compare a b) binds)
          ) tbl
      ) pos_trans_tbl;
    END_DEBUG

  method private check_pos_trans_tbl () =
    Hashtbl.iter
      (fun nd tbl ->
        DEBUG_MSG "%s" nd#initial_to_string;
        Hashtbl.iter
          (fun pos tbl2 ->
            DEBUG_MSG " pos=%d" pos;
            let tbl2' = Hashtbl.create 0 in
            Hashtbl.iter
              (fun ofs c ->
                let spc = if (fst (modf ofs)) > 0. then "" else "  " in
                let _ = spc in
                try
                  let c' = Hashtbl.find tbl2' ofs in
                  if is_ancestor c' c then
                    ()
                  else if is_ancestor c c' then begin
                    DEBUG_MSG "%s%.4f -> %a" spc ofs nps c;
                    Hashtbl.replace tbl2' ofs c
                  end
                  else begin
                    DEBUG_MSG "%s%.4f -> %a" spc ofs nps c;
                    Hashtbl.add tbl2' ofs c
                  end
                with
                  Not_found ->
                    DEBUG_MSG "%s%.4f -> %a" spc ofs nps c;
                    Hashtbl.add tbl2' ofs c
              ) tbl2;
            Hashtbl.replace tbl pos tbl2'
          ) tbl
      ) pos_trans_tbl

  method private translate_positions () =
    let to_be_translated = ref [] in
    Hashtbl.iter
      (fun u ms ->
        DEBUG_MSG "u=%a |ms|=%d" UID.ps u (List.length ms);
        let nds = self#get_nodes_from_uid u in
        List.iter
          (fun nd ->
            DEBUG_MSG "nd=%a" nps nd;
            let changed_flag = ref false in
            let ms' =
              (*List.map
                (fun m ->
                  if Xset.mem no_trans_mutations m then
                    m
                  else
                  match m with
                  | Otree.CMinsert(flag, pos, ofs, subroot, []) -> begin
                      if Xset.mem recovered_nodes subroot then
                        m
                      else
                      let check n =
                        let b =
                          n#initial_parent == nd &&
                          try
                            let ind = nd#initial_children.(pos) in
                            DEBUG_MSG "n=%a ind=%a" nps n nps ind;
                            n != ind
                          with
                            _ -> true
                        in
                        DEBUG_MSG "%a -> %B" nps n b;
                        b
                      in
                      try
                        let tbl = Hashtbl.find (Hashtbl.find pos_trans_tbl nd) pos in
                        let _binds = Hashtbl.fold (fun o n l -> (o, Some n) :: l) tbl [] in
                        let extra_binds =
                          try
                            if (Hashtbl.find (Hashtbl.find lift_point_tbl nd) pos) >= ofs then
                              let tbl =
                                Hashtbl.find (Hashtbl.find extra_pos_trans_tbl nd) pos
                              in
                              Hashtbl.fold (fun o n l -> (o, Some n) :: l) tbl []
                            else
                              []
                          with
                            Not_found -> []
                        in
                        let _bind =
                          (ofs, try Some (Hashtbl.find tbl ofs) with Not_found -> None)
                        in
                        let __binds = _binds @ extra_binds in
                        let ___binds =
                          if List.mem _bind __binds then
                            __binds
                          else
                            _bind::__binds
                        in
                        let binds =
                          List.fast_sort
                            (fun (o0, _) (o1, _) -> Stdlib.compare o0 o1)
                            ___binds
                        in

                        BEGIN_DEBUG
                          DEBUG_MSG "orig: %s" (Otree.cluster_mutation_to_string m);
                          List.iter
                            (fun (o, n_opt) ->
                              match n_opt with
                              | Some n -> DEBUG_MSG " %f -> %a" o nps n
                              | None   -> DEBUG_MSG " %f" o
                            ) binds;
                        END_DEBUG;

                        let has_frac o = (fst (modf o)) > 0. in

                        let is_mem n_opt =
                          let b =
                            match n_opt with
                            | Some n -> begin
                                try
                                  let key = self#find_key nd in
                                  let k = self#find_key n in
                                  key = k
                                with
                                  _ -> false
                            end
                            | _ -> false
                          in
                          DEBUG_MSG "b=%B" b;
                          b
                        in

                        let rec scan l =
                          match l with
                          | [] -> assert false
                          | [_] -> m
                          | (o0, n0_opt)::((o1, n1_opt)::t as l2) -> begin
                              DEBUG_MSG "o0=%.4f, o1=%.4f" o0 o1;
                              if o0 = ofs then begin
                                match n1_opt with
                                | Some n1 ->
                                    DEBUG_MSG " n1=%s" n1#initial_to_string;
                                    if
                                      (check n1 || o0 < o1) &&
                                      let b =
                                        match self#find_parent_key_opt subroot with
                                        | Some ((K_stid _ | K_mid _) as k) -> begin
                                            try
                                              let ind = nd#initial_children.(pos) in
                                              let ik = self#find_key ind in
                                              DEBUG_MSG "k=%s ik=%s"
                                                (key_to_string k) (key_to_string ik);
                                              k <> ik &&
                                              let b0 =
                                                ofs = 0. ||
                                                (match n0_opt with
                                                | None -> true
                                                | Some n0 -> self#is_deleted n0
                                                ) ||
                                                self#has_stable_descendant ind in
                                              if not b0 then
                                                DEBUG_MSG "ind=%a" nps ind;
                                              b0
                                            with
                                              _ -> true
                                        end
                                        | _ -> true
                                      in
                                      DEBUG_MSG "b=%B" b;
                                      b
                                    then
                                      let p = ref n1#initial_pos in
                                      let o = ref (-1.0) in
                                      let gap = int_of_float(o1 -. o0) in
                                      DEBUG_MSG "gap=%d" gap;
                                      if gap > 1 then begin
                                        try
                                          let k = self#find_key nd in
                                          DEBUG_MSG "k=%s" (key_to_string k);
                                          for i = 2 to gap do
                                            let p0 = !p - 1 in
                                            DEBUG_MSG "p0=%d" p0;
                                            let n0 = nd#initial_children.(p0) in
                                            DEBUG_MSG "n0=%a" nps n0;
                                            let k0 = self#find_key n0 in
                                            DEBUG_MSG "k0=%s" (key_to_string k0);
                                            if k0 = k then begin
                                              p := p0;
                                              if n0_opt = None then
                                                o := if o0 < 0.0 then 1.0 else -1.0
                                            end
                                          done
                                        with
                                          _ -> ()
                                      end;
                                      DEBUG_MSG "p=%d, o=%.4f" !p !o;
                                      Otree.CMinsert(flag, !p, !o, subroot, [])
                                    else
                                      m
                                | None -> assert false
                              end
                              else if
                                o1 = ofs &&
                                (t = [] ||
                                (match t with (o2, _)::t' -> has_frac o2 | _ -> false) ||
                                is_mem n0_opt)
                              then begin
                                match n1_opt with
                                | Some n1 -> begin
                                    DEBUG_MSG " n1=%s" n1#initial_to_string;
                                    if check n1 then
                                      Otree.CMinsert(flag, n1#initial_pos, 1.0, subroot, [])
                                    else
                                      m
                                end
                                | None -> begin
                                    match n0_opt with
                                    | Some n0 ->
                                        DEBUG_MSG " n0=%s" n0#initial_to_string;
                                        if check n0 || (o0 <> 0. && o0 < o1) then
                                          Otree.CMinsert(flag, n0#initial_pos, 1.0, subroot, [])
                                        else
                                          m
                                    | None -> assert false
                                end
                              end
                              else
                                scan l2
                          end
                        in
                        let m' = scan binds in
                        if m != m' then begin
                          changed_flag := true;
                          DEBUG_MSG "modified: %s" (Otree.cluster_mutation_to_string m')
                        end;
                        m'
                      with
                        Not_found -> m
                  end
                  | _ -> m
                ) *)ms
            in
            if !changed_flag then
              to_be_translated := (u, ms') :: !to_be_translated

          ) nds
      ) op_tbl;
    List.iter
      (fun (u, ms') ->
        Hashtbl.replace op_tbl u ms'
      ) !to_be_translated

  method private _mutate ?(get_idx_opt=None) ?(overwrite=true) nodes =
    self#sort_op_tbl ~get_idx_opt ();

    Xset.add_set nodes (self#mutate_tree tree);

    BEGIN_DEBUG
      DEBUG_MSG "operation table:";
      Hashtbl.iter
      (fun u ms ->
        let l = self#get_nodes_from_uid u in
        List.iter
          (fun n ->
            DEBUG_MSG "%s" n#initial_to_string
          ) l;
        List.iter
          (fun m ->
            DEBUG_MSG " %s" (Otree.cluster_mutation_to_string m)
          ) ms
      ) op_tbl;
    END_DEBUG;

    let order_tbl = Hashtbl.create 0 in
    let order_tbl2 = Hashtbl.create 0 in
    let idx_tbl = Hashtbl.create 0 in
    let marked_targets = Xset.create 0 in
    let add_to_order_tbl2 n u p =
      if not (Hashtbl.mem order_tbl2 n) then begin
        Hashtbl.add order_tbl2 n (u, p);
        DEBUG_MSG "%a -> (%a, %d)\n" nps n UID.ps u p
        (*print_string (sprintf "%a -> (%a, %d)\n" nps n UID.ps u p);*)
      end
    in
    Hashtbl.iter
      (fun u ms ->
        let nl = self#get_nodes_from_uid u in
        List.iter
          (function
            | Otree.CMinsert(_, pos, ofs, sr, fnode_felem_list) -> begin
                add_to_order_tbl2 sr u pos;
                let _ =
                  try
                    if self#is_marked_key (self#find_key sr) then begin
                      List.iter
                        (fun n ->
                          DEBUG_MSG "%a is a marked target" nps n;
                          Xset.add marked_targets n;
                          let pn = n#initial_parent in
                          DEBUG_MSG "%a is a marked target" nps pn;
                          Xset.add marked_targets pn
                        ) nl
                    end
                  with
                    _ -> ()
                in
                let gil =
                  List.map
                    (fun n -> let gi = n#gindex in Hashtbl.add idx_tbl n gi; gi)
                    nl
                in
                List.iter (Hashtbl.add idx_tbl sr) gil;
                List.iter
                  (fun x ->
                    if
                      x != sr &&
                      try not (Xset.mem (Hashtbl.find order_tbl x) sr) with _ -> true
                    then begin
                      DEBUG_MSG "x:%a -> sr:%a" nps x nps sr;
                      (*print_string (sprintf "!!! x:%a -> sr:%a\n" nps x nps sr);*)
                      tbl_add_set order_tbl x sr
                    end
                  ) nl;
                List.iter
                  (fun (fn, _) ->
                    add_to_order_tbl2 fn u pos;
                    List.iter (Hashtbl.add idx_tbl fn) gil;
                    if
                      sr != fn &&
                      try not (Xset.mem (Hashtbl.find order_tbl sr) fn) with _ -> true
                    then begin
                      DEBUG_MSG "sr:%a -> fn:%a" nps sr nps fn;
                      (*print_string (sprintf "!!! sr:%a -> fn:%a\n" nps sr nps fn);*)
                      tbl_add_set order_tbl sr fn
                    end
                  ) fnode_felem_list
            end
            | _ -> ()
          ) ms
      ) op_tbl;

    let cmp2 n1 n2 =
      let c =
        try
          let u1, p1 = Hashtbl.find order_tbl2 n1 in
          let u2, p2 = Hashtbl.find order_tbl2 n2 in
          if u1 = u2 then
            compare p1 p2
          else
            0
        with _ -> 0
      in
      (*print_string (sprintf "cmp: %a %a -> %d\n" nps n1 nps n2 c);*)
      c
    in
    let cmp_nodes n0 n1 =
      let rec trace visited target n =
        if Xset.mem visited n then
          ()
        else begin
          Xset.add visited n;
          try
            let s = Hashtbl.find order_tbl n in
            if Xset.mem s target then
              raise Exit
            else
              Xset.iter (trace visited target) s
          with
            Not_found -> ()
        end
      in
      let gt n0 n1 =
        let visited = Xset.create 0 in
        try
          let _ = trace visited n1 n0 in
          false
        with
          Exit -> true
      in
      let res =
        let c = cmp2 n0 n1 in
        if c = 0 then
          if gt n0 n1 then
            1
          else if gt n1 n0 then
            -1
          else
            try
              compare (Hashtbl.find idx_tbl n0) (Hashtbl.find idx_tbl n1)
            with
              _ -> 0
        else
          c
      in
      DEBUG_MSG "cmp: %a %a -> %d\n" nps n0 nps n1 res;
      (*print_string (sprintf "cmp: %a %a -> %d\n" nps n0 nps n1 res);*)
      res
    in

    Hashtbl.iter
      (fun stid t ->
        DEBUG_MSG "stid=\"%s\"" (stid_to_str stid);
        Xset.add_set nodes (self#mutate_tree t)
      ) subtree_tbl;

    Hashtbl.iter
      (fun mid t ->
        DEBUG_MSG "mid=\"%a\"" MID.ps mid;
        Xset.add_set nodes (self#mutate_tree t)
      ) copied_subtree_tbl;

    let node_list0, _node_list =
      List.partition self#is_stable (Xset.to_list nodes)
    in
    let _node_list1, _node_list2 =
      List.partition (fun n -> n#has_later_mutation) _node_list
    in
    let node_list1 =
      if not overwrite && (List.length _node_list1) > 1 then
        List.fast_sort cmp_nodes _node_list1
      else
        _node_list1
    in
    (*let node_list2 = List.fast_sort self#compare_nodes _node_list2 in*)
    let node_list2 = List.fast_sort cmp_nodes _node_list2 in
    DEBUG_MSG "node_list0: [%a]" nsps node_list0;
    DEBUG_MSG "node_list1: [%a]" nsps node_list1;
    DEBUG_MSG "node_list2: [%a]" nsps node_list2;
    (*print_string (sprintf "!!! node_list0: [%a]\n" nsps node_list0);
    print_string (sprintf "!!! node_list1: [%a]\n" nsps node_list1);
    print_string (sprintf "!!! node_list2: [%a]\n" nsps node_list2);*)

    List.iter
      (fun l ->
        List.iter
          (fun nd ->
            DEBUG_MSG "%s" nd#initial_to_string;
            (*let overwrite = (not (Xset.mem marked_targets nd)) && overwrite in*)
            DEBUG_MSG "nd=%a overwrite=%B" nps nd overwrite;
            nd#end_mutation ?overwrite:(Some overwrite) ();
            DEBUG_MSG "-> %s" nd#initial_to_string;
          ) l
      ) (*[node_list0; node_list1; node_list2]*)[node_list2; node_list1; node_list0];

    DEBUG_MSG "finished"

  method dump_dot_ch ch =
    DEBUG_MSG "start";
    let buf = Buffer.create 0 in
    Buffer.add_string buf "digraph I {\nordering=out;\n";

    let mklab nd =
      (*DEBUG_MSG "nd=%s" nd#to_string;*)

      if self#is_deleted nd then
        Buffer.add_string buf
          (sprintf "%a [style=filled,fillcolor=\"%s\",fontcolor=\"%s\"];\n"
             UID.rs nd#uid del_bg del_fg)

      else if self#is_insert nd then
        Buffer.add_string buf
          (sprintf "%a [style=filled,fillcolor=\"%s\",fontcolor=\"%s\"];\n"
             UID.rs nd#uid ins_bg ins_fg);

      let head =
        (try
          sprintf "[%s]\\n" (key_to_string (self#find_key_of_deleted nd))
        with
          Not_found -> "") ^
        (try
          sprintf "[%s]\\n" (key_to_string (self#find_key nd))
        with
          Not_found -> "")
      in
      (*DEBUG_MSG "head=%s" head;*)
      let _tail =
        let k = try key_to_string (self#find_parent_key nd) with Not_found -> "" in
        let c = self#get_quasi_upstream_count nd in
        if k = "" then
          ""
        else
          sprintf "\\n[%s%s]" k (ups_to_str c)
      in
      (*DEBUG_MSG "_tail=%s" _tail;*)
      let tail =
        let c = self#get_upstream_count nd in
        let d = try key_to_string (self#get_upstream_dest nd) with _ -> "" in
        if c > 0 then
          sprintf "\\n[%s%s]" (ups_to_str c) d
        else
          ""
      in
      (*DEBUG_MSG "tail=%s" tail;*)
      let lab = head^(Otree.dot_label_of_node_ini nd)^_tail^tail in
      (*DEBUG_MSG "lab=%s" lab;*)
      lab
    in
    let dot = tree#to_dot_initial ?mklab:(Some mklab) [] in

    Buffer.add_buffer buf dot;
    Buffer.add_string buf "}\n";
    Buffer.output_buffer ch buf;

    DEBUG_MSG "finished."

  method dump_dot fname =
    Xfile.dump fname self#dump_dot_ch

  method mutate() =

    self#setup_pos_trans_tbl();

    let nodes = Xset.create 0 in
    DEBUG_MSG "@";
    self#_mutate(* ~overwrite:false*) nodes;

    (*Printf.printf "*** begin0\n";
    preorder_scan_whole_initial_subtree tree#root
      (fun n -> Printf.printf "%s\n" n#initial_to_string);
    Printf.printf "*** end0\n";*)

    self#check_pos_trans_tbl();

    self#prune_deferred nodes;

    (*Printf.printf "*** begin1\n";
    preorder_scan_whole_initial_subtree tree#root
      (fun n -> Printf.printf "%s\n" n#initial_to_string);
    Printf.printf "*** end1\n";*)

    DEBUG_MSG "finished"


  method prune_deferred nodes =
    DEBUG_MSG "performing deferred pruning...";

    BEGIN_DEBUG
      self#dump_dot "intermediate.dot";
    END_DEBUG;

    let invalidated_nodes = Xset.create 0 in

    let skip_inserted ?(hook=fun x -> ()) n =
      let visited = Xset.create 0 in
      let rec skip visited n =
        DEBUG_MSG "n=%a" nps n;

        if Xset.mem visited n then begin
          WARN_MSG "infinite loop detected!";
          failwith "skip_inserted"
        end
        else
          Xset.add visited n;

        if self#is_insert n then begin
          hook n;
          skip visited n#initial_parent
        end
        else
          n
      in
      skip visited n
    in

    let get_lifters n =
      DEBUG_MSG "n=%a" nps n;
      if
        self#is_deleted n &&
        not (self#has_true_stable_descendant n)
      then
        let p = n#initial_parent in
        if self#is_insert p then
          let k_opts = ref [] in
          let hook x =
            let k_opt = self#find_key_opt x in
            if not (List.mem k_opt !k_opts) then
              k_opts := k_opt :: !k_opts
          in
          let a = skip_inserted ~hook p in
          DEBUG_MSG "a=%a, k_opts=[%s]" nps a
            (String.concat ";" (List.map key_opt_to_string !k_opts));

          if
            self#is_deleted a &&
            self#_find_key_opt_of_deleted a = self#_find_key_opt_of_deleted n
          then
            !k_opts
          else
            []
        else
          []
      else
        []
    in
    let has_lifter n = get_lifters n <> [] in

    let add_parent_key, get_parent_key, iter_parent_key =
      let tbl = Hashtbl.create 0 in
      let add nd key = Hashtbl.add tbl nd key in
      let get = Hashtbl.find tbl in
      let iter f = Hashtbl.iter f tbl in
      add, get, iter
    in
    let add_parent, get_parent, iter_parent =
      let tbl = Hashtbl.create 0 in
      let add nd v = Hashtbl.add tbl nd v in
      let get = Hashtbl.find tbl in
      let iter f = Hashtbl.iter f tbl in
      add, get, iter
    in

    self#init_mutation nodes;

    let subtree_roots = Xset.create 0 in
    Hashtbl.iter (fun _ t -> Xset.add subtree_roots t#root) subtree_tbl;
    (*Hashtbl.iter (fun _ t -> Xset.add subtree_roots t#root) copied_subtree_tbl;*)
    DEBUG_MSG "subtree_roots: [%a]" nsps (Xset.to_list subtree_roots);

    DEBUG_MSG "roots of canceled deletions: [%s]"
      (nodes_to_uids_string (Xset.to_list canceled_dels));

    (*Hashtbl.iter
      (fun nd _ ->
        try
          if nd#initial_parent#initial_children.(nd#initial_pos) != nd then begin
            DEBUG_MSG "dropping %a" nps nd;
            Xset.add canceled_dels nd
          end
        with
          _ -> ()
      ) del_tbl;*)

    Xset.iter (Hashtbl.remove del_tbl) canceled_dels;

    BEGIN_DEBUG
      DEBUG_MSG "del_tbl: (parent) subtree root: frontier node -> members";
      Hashtbl.iter
        (fun nd pl ->
          DEBUG_MSG "del_tbl: (%a) %a:\n%s" nps nd#initial_parent nps nd
            (String.concat "\n"
               (List.map (fun (n, ns) -> sprintf "%a -> [%a]" nps n nsps ns) pl));
        ) del_tbl;
    END_DEBUG;

    let prune_tbl = Hashtbl.create 0 in

    let excluded_insert_roots = Xset.create 0 in

    let extra_roots_tbl = Hashtbl.create 0 in

    let upstream_tbl = Hashtbl.create 0 in (* excluded node -> upstream count *)

    let orig_upward_mem_tbl = Hashtbl.create 0 in (* node -> node *)

    let skip_deleted = self#skip_deleted in

    let rec skip_inserted_ (*visited*) count (nd, pos) =
      (*if List.memq nd visited then begin
        WARN_MSG "infinite loop detected!: nd=%a" nps nd;
        failwith "skip_inserted_"
      end;*)
      DEBUG_MSG "count=%d %a" count nps nd;
      try
        let k = self#find_key nd in
        let _ = k in
        DEBUG_MSG "found: %s" (key_to_string k);
        skip_inserted_ (*(nd::visited)*) (count+1) (nd#initial_parent, nd#initial_pos)
      with
        Not_found -> nd, pos, count
    in

    let rec skip_touched nd =
      let pnd = nd#initial_parent in
      if self#is_stable pnd then
        pnd, nd#initial_pos
      else
        skip_touched pnd
    in

    let skip_touched_li ?(limit=tree#root) ?(target=tree#root) nd =
      DEBUG_MSG "limit=%a target=%a nd=%a" nps limit nps target nps nd;
      let last_inserted = ref [] in
      let rec skip nd =
        DEBUG_MSG "nd=%a" nps nd;
        if nd == limit then
          failwith "skip_touched";
        let pnd = nd#initial_parent in
        if pnd == limit then
          failwith "skip_touched";
        if self#is_insert pnd then begin
          DEBUG_MSG "%a (pos=%d)" nps pnd nd#initial_pos;
          last_inserted := (pnd, nd#initial_pos) :: !last_inserted
        end;
        if self#is_stable pnd || pnd == target && pnd != tree#root then
          pnd, nd#initial_pos, !last_inserted
        else
          skip pnd
      in
      skip nd
    in

    let find_upward_mem nd =
      DEBUG_MSG "nd=%a" nps nd;
      let um, pos, c = skip_inserted_ (*[]*) 0 (skip_deleted nd) in
      DEBUG_MSG "um=%a pos=%d c=%d" nps um pos c;
      let res =
        if c > 0 then begin
          if self#is_stable um then begin
            DEBUG_MSG "%a is stable" nps um;

            if not (self#has_parent_key nd) then begin
              DEBUG_MSG "%a -> %s" nps nd (key_to_string key_stable);
              (*Hashtbl.add parent_key_tbl nd key_stable;*)
              add_parent_key nd key_stable
            end;

            nd#initial_parent
          end

          else if self#is_forced_upstream um#initial_children.(pos) then
            nd#initial_parent

          (*else if self#is_deleted um then begin
            DEBUG_MSG "%a is deleted" nps um;

            let x, _ = skip_deleted um in
            if self#is_stable x then begin
              DEBUG_MSG "%a is stable" nps x;

              if not (Hashtbl.mem parent_key_tbl nd) then begin
                DEBUG_MSG "%a -> %s" nps nd (key_to_string key_stable);
                Hashtbl.add parent_key_tbl nd key_stable;
              end;

              nd#initial_parent
            end
            else
              um
          end!!!!!*)

          else
            um
        end
        else
          nd#initial_parent
      in
      DEBUG_MSG "%a -> %a" nps nd nps res;

      res
    in (* find_upward_mem *)

    let _has_quasi_upstream_descendant key_opt nd =
      DEBUG_MSG "key_opt(deleted)=%s nd=%a" (key_opt_to_string key_opt) nps nd;
      (*let key_opts = ref [key_opt] in*)
      (*let ik_opt = self#find_key_opt nd in
      DEBUG_MSG "ik_opt=%s" (key_opt_to_string ik_opt);*)

      let start_with_del = self#is_deleted nd in
      let start_with_ins = self#is_insert nd in
      DEBUG_MSG "start_with_del=%B" start_with_del;
      DEBUG_MSG "start_with_ins=%B" start_with_ins;

      let rec scan ?(top=true) ?(visited=[]) n =

        let n_is_deleted = self#is_deleted n in
        let n_is_insert = self#is_insert n in

        DEBUG_MSG "top=%B visited=[%s] n=%a%s"
          top
          (String.concat ";"
             (List.map
                (function
                  | (`Ins, n0) -> "I:"^(UID.to_string n0#uid)
                  | (`Del, n0) -> "D:"^(UID.to_string n0#uid)
                  | _ -> "?"
                ) visited))
          nps n (if n_is_insert then " I" else if n_is_deleted then " D" else "");

        let check n =
          let nk = self#find_parent_key n in
          DEBUG_MSG "nk=%s" (key_to_string nk);
          let a =
            if start_with_ins then
              nd
            else if start_with_del then
              let a, _ = skip_deleted nd in
              a
            else
              raise Exit
          in
          DEBUG_MSG "a=%a" nps a;
          let b0 =
            try
              let ak = self#find_key a in
              DEBUG_MSG "ak=%s" (key_to_string ak);
              ak <> nk
            with
              _ -> true
          in
          if b0 then begin
            if
              start_with_ins && nk <> K_stable &&
              try
                let a, _ = skip_deleted nd in
                self#find_key a <> nk
              with
                _ -> true
            then begin
              self#set_upstream_dest nd nk
            end
            else if start_with_del && nk = K_stable && self#is_insert a then begin
              match visited with
              | (`Del,_)::(`Ins,ins)::(`Del,_)::[] -> begin
                  self#force_upstream ~key_opt:(Some nk) ins
              end
              | _ -> ()
            end;
            raise Exit
          end
        in

        if
          not top && not n_is_deleted &&
          self#has_parent_key(*_stable*) n && self#get_quasi_upstream_count n > 0
        then begin
          DEBUG_MSG "%a has parent_key" nps n;
          check n
        end;

        (*let k_opt = self#find_key_opt_of_deleted n in*)
        DEBUG_MSG "n=%a k_opt(deleted)=%s" nps n (key_opt_to_string (self#find_key_opt_of_deleted n));
        (*let ik0_opt = self#find_key_opt n in
        DEBUG_MSG "ik0_opt=%s" (key_opt_to_string ik0_opt);*)
        (*DEBUG_MSG "key_opts=[%s]" (String.concat ";" (List.map key_opt_to_string !key_opts));*)
        let moveon =
          not n_is_deleted || not (self#is_stable n)
          (*not n_is_deleted ||
          List.mem k_opt !key_opts ||
          let b = List.mem (self#find_key_opt_of_deleted n#initial_parent) !key_opts in
          if b then
            key_opts := k_opt :: !key_opts;
          b*)
          (* &&
          (ik_opt = None || ik0_opt = None || ik0_opt = ik_opt || self#find_key_opt n#initial_parent = ik_opt)*)
        in
        DEBUG_MSG "moveon=%B" moveon;
        if
          moveon &&
          (not n_is_insert ||
          match visited with
          | [] | [`Ins,_] -> true
          | [`Ins,_; `Del,_] -> start_with_del
          | [`Del,_] -> begin
              let b0 =
                start_with_del && not (self#is_forced_upstream n) &&
                try
                  let a, _ = skip_deleted nd in
                  let ak = self#find_key a in
                  let nk = self#find_key n in
                  DEBUG_MSG "ak=%s nk=%s" (key_to_string ak) (key_to_string nk);
                  ak <> nk && not (self#is_ancestor_key ak nk)
                with
                  _ -> false
              in
              DEBUG_MSG "b0=%B" b0;
              b0
          end
          | _ -> false)
        then begin
          if
            not n_is_deleted &&
            self#has_parent_key(*_stable*) n && self#get_quasi_upstream_count n > 0
          then begin
            DEBUG_MSG "%a has parent_key" nps n;
            check n
          end
          else if not (self#is_stable n) then
            let visited =
              match visited with
              | [] when n_is_deleted -> (`Del, n) :: visited
              | [] when n_is_insert -> (`Ins, n) :: visited
              | (`Del, _) :: _ -> if n_is_insert then (`Ins, n) :: visited else visited
              | (`Ins, _) :: _ -> if n_is_deleted then (`Del, n) :: visited else visited
              | _ -> visited
            in
            for i = n#initial_nchildren - 1 downto 0 do
              scan ~top:false ~visited n#initial_children.(i)
            done
        end
      in
      let b =
        if start_with_del &&
          try
            let a, _ = skip_deleted nd in
            self#is_stable a
          with
            _ -> false
        then
          false
        else
          try
            scan nd;
            false
          with
            Exit -> true
      in
      DEBUG_MSG "%a -> %B" nps nd b;
      b
    in (* _has_quasi_upstream_descendant *)

    let has_foreign_insert ?(immediate=false) nd n n0 =
      DEBUG_MSG "nd=%a n=%a n0=%s" nps nd nps n n0#initial_to_string;
      if n0#initial_parent != n then
        true
      else if immediate then
        false
      else
        try
          let a, _ = skip_deleted ~limit:nd n0 in
          let _ = a in
          DEBUG_MSG "found: %a" nps a;
          true
        with
          _ -> false
    in

    let dels_with_root_shifts = Xset.create 0 in

    let upstream_roots = Xset.create 0 in
    let add_upstream_root n =
      if
        n#initial_nchildren > 0 &&
        (self#is_insert n || self#is_stable n)
      then begin
        DEBUG_MSG "%a" nps n;
        Xset.add upstream_roots n
      end
    in
    let remove_upstream_root n =
      DEBUG_MSG "%a" nps n;
      Xset.remove upstream_roots n
    in

    let processed_nodes = Xset.create 0 in

    let get_upstream_count n =
      let count = ref 0 in
      let rec scan n =
        DEBUG_MSG "n=%a" nps n;
        let c = self#get_upstream_count n in
        if c > 0 then begin
          if not (Hashtbl.mem upstream_tbl n) then
            Hashtbl.add upstream_tbl n c;

          if not (self#has_parent_key n) then begin
            count := c;
            DEBUG_MSG "%a is an upstream node (count=%d)" nps n c;
            raise Exit
          end
        end
        else
          let k_opt = self#find_key_opt n in
          Array.iter
            (fun x ->
              DEBUG_MSG "x=%a" nps x;
              let n_is_deleted = self#is_deleted n in
              let x_is_deleted = self#is_deleted x in
              let x_is_insert = self#is_insert x in
              if
                not (n_is_deleted && x_is_deleted && not (self#has_key_opt k_opt x)) &&
                not (n_is_deleted && x_is_insert)
              then
                scan x
            ) n#initial_children
      in
      begin
        try
          scan n
        with
          Exit -> ()
      end;
      DEBUG_MSG "%a -> %d" nps n !count;
      !count
    in (* get_upstream_count *)

    let get_skey cache rt n =
      try
        Hashtbl.find cache n
      with
        Not_found ->
          DEBUG_MSG "n=%a" nps n;
          let cur = ref n in
          let posl = ref [] in
          let visited = ref [n] in
          while !cur != rt do
            DEBUG_MSG "  cur=%a pos=%d" nps !cur (!cur)#initial_pos;
            posl := (!cur)#initial_pos :: !posl;
            let parent = (!cur)#initial_parent in
            if List.memq parent !visited then begin
              WARN_MSG "infinite loop detected: cur=%a parent=%a visited=[%a]"
                nps !cur nps parent nsps !visited;
              raise Not_found
            end
            else
              cur := parent;
            visited := parent :: !visited
          done;
          let key = !posl, (get_p_descendants self#is_stable n) in
          Hashtbl.add cache n key;
          BEGIN_DEBUG
            let skey_to_string (il, nl) =
              sprintf "%s,[%a]" (String.concat ":" (List.map string_of_int il)) nsps nl
            in
            DEBUG_MSG "%a -> %s" nps n (skey_to_string key);
          END_DEBUG;
          key
    in
    let cmp_skey_sub sns0 sns1 =
      let sns1_ = Xlist.subtractq sns1 sns0 in
      if sns0 <> [] && sns1_ <> [] then
        let l0, h0 =
          let s0 = sort_nodes_by_gindex sns0 in
          (List.hd s0)#gindex, (Xlist.last s0)#gindex
        in
        let l1, h1 =
          let s1 = sort_nodes_by_gindex sns1_ in
          (List.hd s1)#gindex, (Xlist.last s1)#gindex
        in
        if h0 < l1 then
          -1
        else if h1 < l0 then
          1
        else
          0
      else
        0
    in
    let rec cmp_skey (posl0, sns0) (posl1, sns1) =
      match posl0, posl1 with
      | [], [] -> 0
      | h0::_, [] -> cmp_skey_sub sns0 sns1
      | [], h1::_ -> -1 * (cmp_skey_sub sns1 sns0)
      | h0::t0, h1::t1 ->
          if h0 = h1 then
            cmp_skey (t0, sns0) (t1, sns1)
          else
            Stdlib.compare h0 h1
    in
    let sort_nds nd nds =
      DEBUG_MSG "nd=%a nds=[%a]" nps nd nsps nds;
      let cache = Hashtbl.create 0 in
      let get_skey = get_skey cache nd in
      List.fast_sort (fun n0 n1 -> cmp_skey (get_skey n0) (get_skey n1)) nds
    in

    let is_root x =
      Xset.mem upstream_roots x ||
      (self#is_deleted x#initial_parent && Xset.mem subtree_roots x)
    in
    let mkmoveon0 xs x =
      let b =
        not (self#is_stable x) &&
        (not (self#is_insert x#initial_parent) || not (self#is_deleted x))
      in
      DEBUG_MSG "%a -> %B" nps x b;
      b
    in
    let mkmoveon1 xs x =
      let b =
        not (self#is_stable x) &&
        (List.memq x xs || not (is_root x))
      in
      DEBUG_MSG "%a -> %B" nps x b;
      b
    in
    let rec lr left_opt right_opt n =
      let pn = n#initial_parent in
      let pos = n#initial_pos in
      let children = pn#initial_children in
      let moveon = mkmoveon0 (Array.to_list children) in

      let left_stable_nd_opt = ref None in
      if left_opt = None then begin
        try
          for i = pos - 1 downto 0 do
            rev_scan_whole_initial_subtree ~moveon children.(i)
              (fun x ->
                if self#is_stable x then begin
                  DEBUG_MSG "%a: left stable node found: %a" nps n nps x;
                  left_stable_nd_opt := Some x;
                  raise Exit
                end
              )
          done
        with
          Exit -> ()
      end;

      let right_stable_nd_opt = ref None in
      if right_opt = None then begin
        try
          for i = pos + 1 to (Array.length children) - 1 do
            preorder_scan_whole_initial_subtree ~moveon children.(i)
              (fun x ->
                if self#is_stable x then begin
                  DEBUG_MSG "%a: right stable node found: %a" nps n nps x;
                  right_stable_nd_opt := Some x;
                  raise Exit
                end
              )
          done
        with
          Exit -> ()
      end;
      match !left_stable_nd_opt, !right_stable_nd_opt with
      | Some n0, Some n1 -> Some ((float n0#gindex)+.0.5), Some ((float n1#gindex)-.0.5)
      | None, (Some n) when self#is_stable pn -> None, Some ((float n#gindex)-.0.5)
      | (Some n), None when self#is_stable pn -> Some ((float n#gindex)+.0.5), None
      | None, (Some n) -> lr None (Some ((float n#gindex)-.0.5)) pn
      | (Some n), None -> lr (Some ((float n#gindex)+.0.5)) None pn
      | _ -> lr None None pn
    in (* lr *)
    let idx_to_str = function
      | Some st, Some ed -> sprintf "%f-%f" st ed
      | Some st, None -> sprintf "%f-" st
      | None, Some ed -> sprintf "-%f" ed
      | _ -> "?"
    in
    let _ = idx_to_str in
    let idx_cache = Hashtbl.create 0 in
    let get_idx ?(strict=false) n =
      DEBUG_MSG "n=%a" nps n;
      try
        let idx = Hashtbl.find idx_cache n in
        DEBUG_MSG "%a -> %s" nps n (idx_to_str idx);
        idx
      with
        Not_found ->
          let k_opt = self#find_key_opt n in
          let ins_key_opt_stack = ref [n, k_opt] in
          let idx =
            if self#is_stable n then
              Some (float n#gindex), Some (float n#gindex)
            else begin
              (*let moveon0 = mkmoveon0 [n] in*)
              let pred x =
                DEBUG_MSG "x=%a" nps x;
                ins_key_opt_stack := (* popping *)
                  List.filter (fun (y, _) -> is_ancestor y x) !ins_key_opt_stack;
                begin
                  match self#find_key_opt x with
                  | Some (K_stid _ | K_mid _) as ko ->
                      ins_key_opt_stack := (x, ko) :: !ins_key_opt_stack
                  | _ -> ()
                end;
                self#is_stable x &&
                match self#find_parent_key_opt x with
                | Some ((K_stid _ | K_mid _ | K_stable) as xk) -> begin
                    DEBUG_MSG "qupc=%d xk=%s" (self#get_quasi_upstream_count x) (key_to_string xk);
                    if self#get_quasi_upstream_count x = 0 then
                      if strict then
                        false
                      else
                        (*false*)xk = K_stable
                    else begin
                      match !ins_key_opt_stack with
                      | [] -> false
                      | (_, last_ins_key_opt)::_ -> begin
                          DEBUG_MSG "last_ins_key_opt=%s k_opt=%s"
                            (key_opt_to_string last_ins_key_opt) (key_opt_to_string k_opt);
                          last_ins_key_opt = k_opt ||
                          match last_ins_key_opt, k_opt with
                          | Some lk, Some k -> self#is_ancestor_key k lk
                          | _ -> false
                      end
                    end
                end
                | _ -> true
              in
              let moveon0 x = not (self#is_stable x) in
              let ss0 = get_p_descendants ~moveon:moveon0 pred n in
              DEBUG_MSG "ss0=[%a]" nsps ss0;
              match ss0 with
              | [] -> begin
                  let moveon1 = mkmoveon1 [n] in
                  let ss1 =
                    get_p_descendants ~moveon:moveon1 pred n
                  in
                  DEBUG_MSG "ss1=[%a]" nsps ss1;
                  match ss1 with
                  | [] -> lr None None n
                  | _ ->
                      let st, ed = get_range (List.map (fun x -> x#gindex) ss1) in
                      Some (float st), Some (float ed)
              end
              | _ ->
                  let st, ed = get_range (List.map (fun x -> x#gindex) ss0) in
                  Some (float st), Some (float ed)
            end
          in
          Hashtbl.add idx_cache n idx;
          DEBUG_MSG "%a -> %s" nps n (idx_to_str idx);
          idx
    in (* get_idx *)

    let compare_nodes n0 n1 =
      let r = cmp_idx (get_idx n0) (get_idx n1) in
      let r =
        if r = 0 then
          try
            (*Path.*)path_compare (self#get_path n0) (self#get_path n1)
          with
            _ -> r
        else
          r
      in
      DEBUG_MSG "%a %a -> %d" nps n0 nps n1 r;
      r
    in
    (*let idx_contained ?(exact=true) idx0 idx1 =
      match idx0, idx1 with
      | (Some st0, Some ed0), (Some st1, Some ed1) ->
          if exact then
            st1 < st0 && ed0 < ed1
          else
            st1 < st0 && ed0 < ed1 ||
            st1 = st0 && ed0 < ed1 ||
            st1 < st0 && ed0 = ed1
      | _ -> false
    in*)

    let sort_nodes nodes =
      DEBUG_MSG "sorting nodes=[%a]" nsps nodes;
      let sorted_nodes =
        List.fast_sort
          (fun n0 n1 -> compare_nodes n0 n1)
          nodes
      in
      DEBUG_MSG "sorted_nodes=[%a]" nsps sorted_nodes;
      sorted_nodes
    in

    let filtered_node_tbl = Hashtbl.create 0 in

    let nd_pl_list =
      List.fast_sort
        (fun (nd1, _) (nd2, _) -> Stdlib.compare nd2#gindex nd1#gindex)
        (Hashtbl.fold (fun nd pl l -> (nd, pl)::l) del_tbl [])
    in

    List.iter (* del_tbl *)
      (fun (nd, pl) ->
        DEBUG_MSG "nd=%a" nps nd;

        let fnodes = List.map (fun (x, _) -> x) pl in

        let nodes_having_foreign_inserts_with_quasi_upstream_descendants =
          List.fold_left
            (fun l (n, ns) ->
              DEBUG_MSG "<%a,[%a]>" nps n nsps ns;
              if
                _has_quasi_upstream_descendant (self#find_key_opt_of_deleted n) n &&
                (ns = [] || List.exists (fun n0 -> has_foreign_insert nd n n0) ns)
              then
                n :: l
              else
                l
            ) [] pl
        in
        let nodes_having_foreign_inserts_with_quasi_upstream_descendants =
          if nodes_having_foreign_inserts_with_quasi_upstream_descendants = [] then
            []
          else
            nd::nodes_having_foreign_inserts_with_quasi_upstream_descendants
        in
        DEBUG_MSG "nodes_having_foreign_inserts_with_quasi_upstream_descendants: %a -> [%a]"
          nps nd nsps nodes_having_foreign_inserts_with_quasi_upstream_descendants;

        let has_foreign_inserts_with_quasi_upstream_descendants x =
          let b = List.memq x nodes_having_foreign_inserts_with_quasi_upstream_descendants in
          DEBUG_MSG "%a -> %B" nps x b;
          b
        in

        let extra_roots = ref [] in

        let checked_mems = Xset.create 0 in

        let excluded =
          let proc1 (n, ns) =
            DEBUG_MSG "nd=%a n=%a ns=[%a]" nps nd nps n nsps ns;
            Xset.add checked_mems n;
            (*List.iter (Xset.add checked_mems) ns;*)
            let base_cond = not (has_foreign_inserts_with_quasi_upstream_descendants n) in
            DEBUG_MSG "base_cond=%B" base_cond;
            List.iter
              (fun n0 ->
                DEBUG_MSG "n0=%a" nps n0;
                let has_foreign_ins =
                  has_foreign_insert ~immediate:true nd n n0
                in
                let has_stable_desc = self#has_stable_descendant n0 in
                let another_root_cond =
                  (base_cond ||
                  not (has_foreign_inserts_with_quasi_upstream_descendants n0) ||
                  self#is_insert n0#initial_parent &&
                  List.exists(*for_all*)
                    (fun x ->
                      let b = self#find_parent_key_opt x <> None in
                      if not b then
                        DEBUG_MSG "x=%a" nps x;
                      b
                    ) (get_p_descendants self#is_stable n0)
                  ) &&
                  has_foreign_ins && has_stable_desc
                in
                DEBUG_MSG "%a: has_foreign_ins=%B, has_stable_desc=%B"
                  nps n0 has_foreign_ins has_stable_desc;

                if another_root_cond then begin
                  DEBUG_MSG "%a turned into the root of another deletion" nps n0;
                  if not (List.memq n0 !extra_roots) then
                    extra_roots := n0 :: !extra_roots;
                  let kod = self#find_key_opt_of_deleted n0 in
                  DEBUG_MSG "kod=%s" (key_opt_to_string kod);
                  if
                    self#is_deleted n0 &&
                    let moveon x = self#is_deleted x && self#find_key_opt_of_deleted x = kod in
                    not (has_p_descendant ~moveon self#is_stable n0) &&
                    let moveon x = not (self#is_stable x) && not (self#is_forced_upstream x) in
                    let pred x = self#is_insert x && not (self#is_forced_upstream x) in
                    let il = get_p_descendants ~moveon pred n0 in
                    List.exists (fun x -> _has_quasi_upstream_descendant None x) il
                  then begin
                    DEBUG_MSG "adding %a" nps n0;
                    Xset.add dels_with_root_shifts n0
                  end
                end;

                if
                  has_foreign_ins &&
                  not base_cond &&
                  has_stable_desc
                then begin
                  DEBUG_MSG "adding %a n=%a n0=%a" nps nd nps n nps n0;
                  Xset.add dels_with_root_shifts nd
                end;

                if
                  self#is_insert n#initial_parent &&
                  has_foreign_insert nd n n0 &&
                  (not base_cond &&
                   not (self#is_stable nd#initial_parent) ||
                   not (List.memq n !extra_roots) &&
                   let moveon = self#is_deleted in
                   has_p_descendant ~moveon self#is_forced_upstream n &&
                   self#has_true_stable_descendant n)
                then begin
                  DEBUG_MSG "%a turned into the root of another deletion" nps n;
                  if not (List.memq n !extra_roots) then
                    extra_roots := n :: !extra_roots;
                  DEBUG_MSG "adding %a" nps nd;
                  Xset.add dels_with_root_shifts nd
                end
                else if self#is_insert n#initial_parent then begin
                  DEBUG_MSG "%a turned into the root of another deletion" nps n;
                  if not (List.memq n !extra_roots) then
                    extra_roots := n :: !extra_roots;
                  (*DEBUG_MSG "adding %a" nps n;
                  Xset.add dels_with_root_shifts n*)
                end

              ) ns;

            if ns = [] then begin
              if not base_cond then begin
                DEBUG_MSG "adding %a" nps nd;
                Xset.add dels_with_root_shifts nd
              end
              else if self#is_insert n#initial_parent then begin
                DEBUG_MSG "%a turned into the root of another deletion" nps n;
                if not (List.memq n !extra_roots) then
                  extra_roots := n :: !extra_roots
              end
            end;

            DEBUG_MSG "n=%a ns=[%a]" nps n nsps ns;
            let initial_children = Array.to_list n#initial_children in
            DEBUG_MSG "initial_children=[%a]" nsps initial_children;

            (*let mems = List.map (fun (x, _) -> x) pl in
            let skip_inserted x =
              try
                let cur = ref x#initial_parent in
                while self#is_insert !cur do
                  cur := (!cur)#initial_parent
                done;
                !cur
              with
                _ -> x
            in
            let xs =
              List.filter
                (fun x ->
                  if
                    not (Xset.mem immediately_pruned_nodes x) &&
                    self#is_deleted x &&
                    not (List.memq x mems) &&
                    not (List.memq x initial_children)
                  then begin
                    (skip_inserted x) == nd
                  end
                  else
                    false
                ) (Array.to_list n#children)
            in
            DEBUG_MSG "xs=[%a]" nsps xs;!!!!!*)
            let l =
              List.flatten
                (List.map
                   (fun x ->
                     DEBUG_MSG "x=%a" nps x;
                     if List.memq x ns then begin
                       if List.memq x fnodes then begin
                         DEBUG_MSG "n=%a: %a -> []" nps n nps x;
                         []
                       end
                       else begin
                         let filt x = self#is_insert x && not (self#has_stable_descendant x) in
                         let xs = List.filter filt (Array.to_list x#initial_children) in
                         DEBUG_MSG "n=%a: %a -> [%a]" nps n nps x nsps xs;
                         xs
                       end
                     end
                     else begin
                       DEBUG_MSG "n=%a: %a" nps n nps x;
                       [x]
                     end
                   ) initial_children)
            in
            DEBUG_MSG "n=%a l=[%a]" nps n nsps l;
            l
          in
          List.flatten (List.map proc1 pl)
        in (* excluded *)

        DEBUG_MSG "nd=%a excluded=[%a]" nps nd nsps excluded;

        let excluded =
          match excluded with
          | [] | [_] -> excluded
          | _ ->
              let nds = Xset.create 0 in
              List.iter (Xset.add nds) excluded;
              let l = ref [] in
              begin
                try
                  rev_scan_whole_initial_subtree nd
                    (fun n ->
                      if Xset.mem nds n then begin
                        l := n :: !l;
                        Xset.remove nds n;
                        if Xset.is_empty nds then
                          raise Exit
                      end
                    )
                with
                  Exit -> ()
              end;
              DEBUG_MSG "nd=%a excluded=[%a] (sorted)" nps nd nsps !l;
              !l
        in

        let other_excluded =
          DEBUG_MSG "nd=%a" nps nd;
          let mems = try self#get_deleted_mems nd with Not_found -> [] in
          let has_no_upstream_descendant n =
            let b =
              try
                preorder_scan_whole_initial_subtree (*~moveon:self#is_deleted*) n
                  (fun x ->
                    if
                      (*self#is_deleted x &&*)
                      List.memq x excluded &&
                      (*(self#get_upstream_count x > 0 || self#has_parent_key_stable x)*)
                      self#get_upstream_count x > 0 && not (self#has_parent_key_stable x)
                    then
                      raise Exit
                  );
                  true
                with
                 Exit -> false
            in
            DEBUG_MSG "%a -> %B" nps n b;
            b
          in
          List.flatten
            (List.map
               (fun (n, ns) ->
                 DEBUG_MSG "nd=%a n=%a ns=[%a]" nps nd nps n nsps ns;
                 if Xset.mem checked_mems n then begin
                   DEBUG_MSG "%a is already checked" nps n;
                   []
                 end
                 else begin
                   DEBUG_MSG "deleted_mem: n=%a ns=[%a]" nps n nsps ns;

                   if self#is_insert n#initial_parent then begin
                     DEBUG_MSG "%a turned into the root of another deletion" nps n;
                     if not (List.memq n !extra_roots) then
                       extra_roots := n :: !extra_roots
                   end;

                   let cl = Array.to_list n#initial_children in
                   DEBUG_MSG "cl=[%a]" nsps cl;
                   let ns0 = List.filter (fun x -> not (List.memq x cl)) ns in
                   DEBUG_MSG "ns0=[%a]" nsps ns0;
                   let cut_in n0 =
                     let b =
                       self#has_p_descendant (fun x -> List.memq x ns) n0
                     in
                     DEBUG_MSG "%a -> %B" nps n0 b;
                     b
                   in
                   let is_quasi_upstream x =
                     self#get_quasi_upstream_count x > 0 && self#has_parent_key(*_stable*) x
                   in
                   let extra_cond =
                     self#is_insert nd#initial_parent &&
                     try
                       List.exists
                         (fun c ->
                           not (self#is_forced_upstream c) &&
                           (self#is_true_stable_node c || self#has_true_stable_descendant c)
                         ) cl
                     with
                       _ -> false
                   in
                   DEBUG_MSG "extra_cond=%B" extra_cond;
                   let filt c =
                     DEBUG_MSG "c=%a" nps c;
                     let b =
                       if not (List.memq c ns) then begin
                         if
                           ns0 = [] ||
                           not (cut_in c) ||
                           self#is_insert c &&
                           not (self#is_forced_upstream c) && not (self#has_p_descendant is_quasi_upstream c)
                         then
                           true
                         else begin
                           let extra_root_added = ref false in
                           begin
                             let moveon x = x != nd in
                             let ns1 = List.filter has_no_upstream_descendant ns0 in
                             DEBUG_MSG "ns1=[%a]" nsps ns1;
                             let qu_cond x =
                               let dl = get_p_descendants self#is_insert x in
                               DEBUG_MSG "dl=[%a]" nsps dl;
                               let b =
                                 List.exists (self#has_p_descendant is_quasi_upstream) dl
                               in
                               let b =
                                 b || self#has_p_descendant is_quasi_upstream x
                               in
                               DEBUG_MSG "%a -> %B" nps x b;
                               b
                             in
                             List.iter
                               (fun n0 ->
                                 DEBUG_MSG "n0=%a" nps n0;
                                 scan_ancestors ~moveon n0
                                   (fun x ->
                                     DEBUG_MSG "x=%a" nps x;
                                     if
                                       x == c &&
                                       (qu_cond n0 ||
                                       self#is_insert x &&
                                       (self#is_forced_upstream c || self#has_p_descendant is_quasi_upstream x ||
                                       self#has_true_stable_descendant n0 &&
                                       try
                                         let a, _ = skip_deleted n in
                                         self#is_stable a
                                       with
                                         _ -> false))
                                     then begin
                                       DEBUG_MSG "%a turned into the root of another deletion" nps n0;
                                       if not (List.memq n0 !extra_roots) then
                                         extra_roots := n0 :: !extra_roots;
                                       extra_root_added := true;
                                       if
                                         self#has_p_descendant is_quasi_upstream n0
                                       then begin
                                         DEBUG_MSG "adding %a" nps n0;
                                         Xset.add dels_with_root_shifts n0
                                       end
                                     end
                                   )
                               ) ns1
                           end;
                           !extra_root_added ||
                           extra_cond && c#initial_parent == nd && self#is_forced_upstream c ||
                           try
                             let rec check x =
                               let px = x#initial_parent in
                               (*self#is_stable px ||
                               (self#is_insert px &&
                                match self#find_key_opt px with
                                | Some pk -> begin
                                    DEBUG_MSG "pk=%s" (key_to_string pk);
                                    let pr = (self#find_subtree pk)#root in
                                    DEBUG_MSG "pr=%a" nps pr;
                                    if self#is_forced_upstream pr then
                                     check pr
                                    else
                                      false
                                end
                                | _ -> false) ||*)
                               self#is_deleted px &&
                               let a, _ = skip_deleted x in
                               DEBUG_MSG "a=%a x=%a" nps a nps x;
                               self#is_stable a(* ||
                               match self#find_key_opt a with
                               | Some k -> begin
                                   DEBUG_MSG "k=%s" (key_to_string k);
                                   let r = (self#find_subtree k)#root in
                                   DEBUG_MSG "r=%a" nps r;
                                   if self#is_forced_upstream r then
                                     check r
                                   else
                                     false
                               end
                               | _ -> false*)
                             in
                             check c
                           with
                             _ -> false
                         end
                       end
                       else
                         false
                     in
                     DEBUG_MSG "%a -> %B" nps c b;
                     b
                   in
                   List.filter filt cl
                 end
               ) mems)
        in (* other excluded *)

        let excluded =
          if other_excluded <> [] then begin
            DEBUG_MSG "%a: other excluded=[%a]" nps nd nsps other_excluded;
            other_excluded @ excluded
          end
          else
            excluded
        in

        let excluded =
          if (List.length excluded) > 1 then begin
            let has_no_upstream_descendant =
              try
                preorder_scan_whole_initial_subtree nd
                  (fun x ->
                    if
                      (self#get_upstream_count x) > (*0*)1 ||
                      self#has_parent_key_stable x
                    then
                      raise Exit
                  );
                true
              with
                Exit -> false
            in
            if has_no_upstream_descendant || other_excluded <> [] then begin
              let sorted = sort_nds nd excluded in
              DEBUG_MSG "%a: excluded=[%a] (sorted)" nps nd nsps excluded;
              sorted
            end
            else
              excluded
          end
          else
            excluded
        in

        List.iter
          (fun n ->
            DEBUG_MSG "n=%a" nps n;
            try
              let k = self#find_key n in
              DEBUG_MSG "n=%a k=%s" nps n (key_to_string k);
              let blacklist = Xset.create 0 in
              let moveon x =
                let b =
                  not (Xset.mem blacklist x) &&
                  try
                    let kx = self#find_key x in
                    let _ = kx in
                    DEBUG_MSG "x=%a kx=%s" nps x (key_to_string kx);
                    true
                  with
                    Not_found ->
                      try
                        let kx = self#_find_key_of_deleted x in
                        let _ = kx in
                        DEBUG_MSG "x=%a kx=%s" nps x (key_to_string kx);
                        match get_p_descendants self#is_stable x with
                        | [_] -> true
                        | _ -> false
                      with
                        Not_found -> false
                in
                DEBUG_MSG "%a -> %B" nps x b;
                b
              in
              try
                preorder_scan_whole_initial_subtree ~moveon n
                  (fun x ->
                    DEBUG_MSG "x=%a" nps x;
                    try
                      if self#is_deleted x then begin
                        let moveon = self#is_deleted in
                        List.iter (Xset.add blacklist) (get_p_descendants ~moveon self#is_insert x);
                        raise Not_found
                      end;

                      let pk = self#find_parent_key x in
                      DEBUG_MSG "parent key found: %a -> %s" nps x (key_to_string pk);

                      let ok =
                        pk <> k &&
                        try
                          let i, _ = skip_deleted ~limit:n x in
                          DEBUG_MSG "i=%a" nps i;
                          let ki = self#find_key i in
                          DEBUG_MSG "ki=%s" (key_to_string ki);
                          pk <> ki
                        with
                          _ -> true
                      in
                      if ok then

                      let (pn, pos, ofs) =
                        try
                          if self#get_quasi_upstream_count x > 0 then
                            self#find_parent x
                          else
                            raise Not_found
                        with
                          Not_found ->
                            DEBUG_MSG "%a: parent not found" nps x;
                            let key_stat = ref (0, None) in
                            let composite_del_count = ref 0 in

                            let rec scan (*visited*) n0 =
                              DEBUG_MSG "n0=%a" nps n0;

                              (*if List.memq n0 visited then begin
                                WARN_MSG "infinite loop detected!: n0=%a" nps n0;
                                failwith "scan"
                              end;*)

                              DEBUG_MSG "composite_del_count=%d" !composite_del_count;

                              let dk_opt = self#_find_key_opt_of_deleted n0 in
                              DEBUG_MSG "dk=%s" (key_opt_to_string dk_opt);

                              try
                                let p0 = n0#initial_parent in
                                DEBUG_MSG "p0=%a" nps p0;
                                let pdk_opt = self#_find_key_opt_of_deleted p0 in
                                DEBUG_MSG "pdk=%s" (key_opt_to_string pdk_opt);
                                begin
                                  match !key_stat, dk_opt, pdk_opt with
                                  | (0, None), Some dk, _ -> begin
                                      key_stat := (1, dk_opt);
                                      DEBUG_MSG "key_stat: 0 -> 1"
                                  end
                                  | (1, Some sk), Some dk, Some pdk -> begin
                                      if sk = dk && dk <> pdk then begin
                                        key_stat := (2, pdk_opt);
                                        DEBUG_MSG "key_stat: 1 -> 2"
                                      end
                                  end
                                  | (2, Some sk), Some dk, None -> begin
                                      if sk = dk then begin
                                        key_stat := (3, dk_opt);
                                        DEBUG_MSG "key_stat: 2 -> 3"
                                      end
                                  end
                                  | (3, Some sk), None, Some pdk -> begin
                                      if sk = pdk then begin
                                        incr composite_del_count;
                                        key_stat := (-1, None);
                                        DEBUG_MSG "key_stat: 3 -> -1"
                                      end
                                  end
                                  | _ -> ()
                                end;
                                match self#find_key_opt p0 with
                                | Some k ->
                                    let pos0 = n0#initial_pos in
                                    DEBUG_MSG "k=%s pk=%s" (key_to_string k) (key_to_string pk);
                                    if k = pk then begin
                                      let moveon x0 =
                                        self#find_parent_key_opt x0 == None && not (self#is_forced_upstream x0)
                                      in
                                      let filt x0 =
                                        self#is_stable x0 && self#find_parent_key_opt x0 == None
                                      in
                                      let ns = get_p_descendants ~moveon filt n0 in
                                      DEBUG_MSG "n0=%a ns=[%a]" nps n0 nsps ns;
                                      let gi0 = x#gindex in
                                      let dir =
                                        if ns = [] then
                                          0.
                                        else if List.for_all (fun x0 -> x0#gindex < gi0) ns then
                                          1.0
                                        else
                                          -1.0
                                      in
                                      (p0, pos0, dir)
                                    end
                                    else
                                      scan (*(n0::visited)*) p0
                                | None ->
                                    if (*!!!!!!*)
                                      pk = key_stable &&
                                      self#is_stable p0 &&
                                      (self#is_insert n0 || !composite_del_count = 1)
                                    then begin
                                      self#force_upstream n;
                                      raise Not_found
                                    end
                                    else
                                      scan (*(n0::visited)*) p0
                              with
                              | Otree.Parent_not_found _ -> raise Not_found
                              | _ -> raise Not_found
                            in
                            scan (*[]*) n
                      in
                      DEBUG_MSG "parent found: %a -> %a (pos=%d,ofs=%f)"
                        nps x nps pn pos ofs;

                      DEBUG_MSG "%a -> %s" nps n (key_to_string pk);
                      DEBUG_MSG "%a -> %a (pos=%d,ofs=%f)"
                        nps n nps pn pos ofs;

                      let pn_pos_ofs = (pn, pos, ofs) in

                      let ok =
                        try
                          scan_ancestors ~moveon:(fun x -> not (self#is_stable x)) n
                            (fun x ->
                              try
                                if (get_parent x) = pn_pos_ofs then
                                  raise Exit
                              with
                                Not_found -> ()
                            );
                          true
                        with
                          Exit -> false
                      in
                      DEBUG_MSG "ok=%B" ok;

                      if ok then begin
                        add_parent_key n pk;
                        add_parent n pn_pos_ofs
                      end
                      else begin
                        DEBUG_MSG "invalidated %a" nps n;
                        Xset.add invalidated_nodes n
                      end
                    with
                      Not_found -> ()
                  )
              with
                Exit -> ()
            with
              Not_found -> ()
          ) excluded;

        DEBUG_MSG "extra_roots=[%a] excluded=[%a]" nsps !extra_roots nsps excluded;

        Hashtbl.add extra_roots_tbl nd !extra_roots;

        if !extra_roots = [] || excluded = [] then begin

          let excluded0 =
            List.filter
              (fun e ->
                DEBUG_MSG "e=%a" nps e;
                let count = get_upstream_count e in
                let quc =
                  try
                    self#get_quasi_upstream_count e
                  with _ -> 0
                in
                DEBUG_MSG "e=%a count=%d" nps e count;
                if count = 0 && quc = 0 then begin
                  try
                    if self#is_deleted e then
                      raise Not_found;

                    let k = try self#find_parent_key e with Not_found -> get_parent_key e in
                    DEBUG_MSG "parent key found: %a -> %s" nps e (key_to_string k);
                    match k with
                    | K_mid _ | K_stid _ -> begin
                        let ((pnd, pos, ofs) as pnd_pos_ofs) = try self#find_parent e with Not_found -> get_parent e in
                        DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pnd pos ofs;

                      let ok =
                        let visited = Xset.create 0 in
                        let moveon x =
                          if Xset.mem visited x then begin
                            WARN_MSG "infinite loop detected!";
                            false
                          end
                          else begin
                            Xset.add visited x;
                            not (self#is_stable x)
                          end
                        in
                        try
                          scan_ancestors ~moveon e#initial_parent
                            (fun x ->
                              DEBUG_MSG "x=%a" nps x;
                              try
                                let pn, p, o = get_parent x in
                                DEBUG_MSG "pn=%a p=%d o=%f" nps pn p o;
                                if (pn, p, o) = pnd_pos_ofs then
                                  raise Exit
                              with
                                Not_found -> ()
                            );
                          true
                        with
                          Exit -> false
                      in
                      DEBUG_MSG "ok=%B" ok;

                      if ok then begin
                        add_upstream_root e;
                        self#insert_cluster pnd pos ofs e [];
                        Xset.add processed_nodes e;
                        false
                      end
                      else begin
                        let um = find_upward_mem e in
                        if self#is_root_of_upstream_staying_move um then
                          self#reg_extra_upstream_root e;
                        DEBUG_MSG "remove from parent(_key)_tbl: %a" nps e;
                        self#remove_from_parent_tbl e;
                        self#remove_from_parent_key_tbl e;
                        Hashtbl.replace upstream_tbl e 0;
                        true
                      end
                    end
                    | _ -> raise Not_found
                  with
                    Not_found -> true
                end
                else begin
                  let um = find_upward_mem e in
                  if self#is_root_of_upstream_staying_move um then
                    self#reg_extra_upstream_root e;
                  true
                end
              ) excluded
          in

          DEBUG_MSG "prune_tbl: add %a -> excluded0:[%a]" nps nd nsps excluded0;

          Hashtbl.add prune_tbl nd excluded0;

        end
        else begin (* !extra_roots <> [] && excluded <> [] *)
          let roots =
            if List.memq nd !extra_roots then
              !extra_roots
            else
              nd :: !extra_roots
          in

          DEBUG_MSG "roots: [%a]" nsps roots;

          let rec find_root e =
            let p = e#initial_parent in
            if List.memq p roots then
              p
            else
              find_root p
          in
          DEBUG_MSG "nd=%a" nps nd;
          let tbl = Hashtbl.create 0 in
          List.iter
            (fun e ->
              DEBUG_MSG "e=%a" nps e;

              let r = find_root e in
              DEBUG_MSG "root of excluded: %a -> %a" nps e nps r;

              if r != nd then begin

                let count =
                  if self#is_forced_upstream e then
                    0
                  else
                    get_upstream_count e
                in
                (*let count =
                  if count = 0 && (get_p_descendants self#is_forced_upstream e) <> [] then
                    1
                  else
                    count
                in!!!!!*)
                if count > 0 then begin
                  try
                    if self#is_deleted e then
                      raise Not_found;

                    let k = self#find_parent_key e in
                    DEBUG_MSG "parent key found: %a -> %s" nps e (key_to_string k);
                    match k with
                    | K_mid _ -> begin
                        let (pnd, pos, ofs) = self#find_parent e in
                        DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pnd pos ofs;

                        add_upstream_root e;
                        self#insert_cluster pnd pos ofs e [];
                        Xset.add processed_nodes e
                    end
                    | _ -> raise Not_found
                  with
                    Not_found -> begin
                      let um = find_upward_mem e in
                      DEBUG_MSG "upward mem: %a -> %a" nps e nps um;
                      let r' =
                        if List.memq um roots then
                          um
                        else
                          find_root um
                      in
                      DEBUG_MSG "um=%a -> r'=%a" nps um nps r';
                      DEBUG_MSG "orig of %a -> %a" nps r' nps r;

                      Hashtbl.add orig_upward_mem_tbl r' r;
                      add_upstream_root e;

                      DEBUG_MSG "add: %a -> %a" nps r' nps e;
                      tbl_add tbl r' e
                    end
                end
                else begin (* count <= 0 *)
                  DEBUG_MSG "add: %a -> %a" nps r nps e;
                  tbl_add tbl r e
                end
              end
              else begin (* r == nd *)

                let has_upstream_descendant n =
                  let b =
                    try
                      preorder_scan_whole_initial_subtree n
                        (fun x ->
                          if
                            List.memq x excluded &&
                            ((self#get_upstream_count x) > 0 ||
                            self#has_parent_key_stable x)
                          then
                            raise Exit
                        );
                      false
                    with
                      Exit -> true
                  in
                  DEBUG_MSG "%a -> %B" nps n b;
                  b
                in

                if has_upstream_descendant e && not (self#is_deleted e) then begin
                  try
                    let k = self#find_parent_key e in
                    DEBUG_MSG "parent key found: %a -> %s" nps e (key_to_string k);
                    match k with
                    | K_mid _ -> begin
                        let (pnd, pos, ofs) = self#find_parent e in
                        DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pnd pos ofs;

                        add_upstream_root e;
                        self#insert_cluster pnd pos ofs e [];
                        Xset.add processed_nodes e
                    end
                    | _ -> ()
                  with
                    Not_found -> ()
                end;

                DEBUG_MSG "add: %a -> %a" nps r nps e;
                tbl_add tbl r e
              end

            ) excluded;

          List.iter
            (fun root ->
              let excluded0 =
                try
                  List.rev (Hashtbl.find tbl root)
                with
                  Not_found -> []
              in
              DEBUG_MSG "root=%a excluded0=[%a]" nps root nsps excluded0;

              let changed = ref false in

              let excluded =
                List.filter
                  (fun x ->
                    if self#is_deleted x then
                      not
                        (List.exists
                           (fun a ->
                             let b = is_ancestor a x in
                             if b then begin
                               DEBUG_MSG "%a is an ancestor of %a" nps a nps x;
                               changed := true;
                               Hashtbl.add filtered_node_tbl x a
                             end;
                             b
                           ) excluded0
                        )
                    else
                      true
                  ) excluded0
              in

              BEGIN_DEBUG
                if !changed then
                  DEBUG_MSG "[%a] -> [%a]" nsps excluded0 nsps excluded;

                DEBUG_MSG "prune_tbl: add %a -> [%a]" nps root nsps excluded;
              END_DEBUG;

              Hashtbl.add prune_tbl root excluded

            ) roots
        end

      ) nd_pl_list;

    iter_parent_key
      (fun nd key ->
        DEBUG_MSG "%a -> %s" nps nd (key_to_string key);
        Hashtbl.add parent_key_tbl nd key
      );
    iter_parent
      (fun nd v ->
        BEGIN_DEBUG
          let n, p, o = v in
          DEBUG_MSG "parent_tbl: %a -> (%a, %d, %f)" nps nd nps n p o;
        END_DEBUG;
        self#add_to_parent_tbl nd v
      );

    DEBUG_MSG "handling filtered nodes:";
    Hashtbl.iter
      (fun x a ->
        DEBUG_MSG " %a -> %a" nps x nps a;
        let moveon x = x != a in
        scan_ancestors ~moveon x
          (fun a ->
            try
              let _xs = Hashtbl.find prune_tbl a in
              let xs = sort_nds a (x :: _xs) in
              DEBUG_MSG "prune_tbl: replace %a -> [%a]" nps a nsps xs;
              Hashtbl.replace prune_tbl a xs
            with
              Not_found -> ()
          )
      ) filtered_node_tbl;

    let to_be_removed = Xset.create 0 in
    let not_excluded = Xset.create 0 in
    let not_upstream = Xset.create 0 in

    let op_tbl_modifier_specs = ref [] in

    let extra_dels = ref [] in
    let nox = Xset.create 0 in
    let supsds = Xset.create 0 in
    let add_extra_del ?(notrans=false) ?(stable_upsds=false) n =
      if notrans then
        Xset.add nox n;
      if stable_upsds then
        Xset.add supsds n;
      if not (List.memq n !extra_dels) then
        extra_dels := n :: !extra_dels
    in
    let is_extra_del n =
      List.memq n !extra_dels
    in

    let ins_tbl = Hashtbl.create 0 in
    let ins_tbl_ = Hashtbl.create 0 in
    let nodes_added_to_ins_tbl_ = Xset.create 0 in

    let get_dir pnd pos nd =
      DEBUG_MSG "pnd=%a pos=%d nd=%a" nps pnd pos nps nd;
      let nd0 = pnd#initial_children.(pos) in
      DEBUG_MSG "nd0=%a" nps nd0;
      let strict = self#is_stable pnd in
        match get_idx ~strict nd0, get_idx ~strict nd with
        | (Some st0, Some ed0), (Some st, Some ed) when ed < st0 -> -1
        | (Some st0, Some ed0), (Some st, Some ed) when ed0 < st -> 1
        | (Some st0, Some ed0), (Some st, Some ed) when st = st0 && ed0 > ed -> -1
        | (Some st0, Some ed0), (Some st, Some ed) when st > st0 && ed0 = ed -> 1
        (*| (Some st0, Some ed0), (Some st, Some ed) when st > st0 && ed0 > ed -> begin
            try
              let d = path_compare (self#get_path nd) (self#get_path nd0) in
              DEBUG_MSG "d=%d" d;
              d
            with
              _ -> 0
        end
        | (Some st0, Some ed0), (Some st, Some ed) when st0 < st && ed < ed0 -> begin
            try
              let d = path_compare (self#get_path nd) (self#get_path nd0) in
              DEBUG_MSG "d=%d" d;
              d
            with
              _ -> 0
        end*)
        | _ -> begin
            try
              let d = path_compare ~weak:true (self#get_path nd) (self#get_path nd0) in
              DEBUG_MSG "nd=%a nd0=%a -> %d" nps nd nps nd0 d;
              if d = 0 then
                raise Not_found
              else
                d
            with
              _ ->
                DEBUG_MSG "nd=%a nd0=%a" nps nd nps nd0;
                if
                  is_ancestor nd0 nd(* &&
                  (not (has_p_descendant self#is_stable nd) ||
                  (self#is_deleted nd0 && self#is_stable nd))*)
                then
                  let canceled = Xset.create 0 in
                  (*let cond x =
                    self#has_parent_key_stable x && self#get_quasi_upstream_count x > 0
                  in*)
                  let is_stable x =
                    (*DEBUG_MSG "x=%a" nps x;*)
                    if self#is_stable x && (not (self#has_parent_key x)(* || cond x*)) then
                      let is_canceled =
                        let rec f m =
                          let r = (self#find_subtree (self#find_key m))#root in
                          self#is_root_of_upstream_staying_move r || Xset.mem nodes_added_to_ins_tbl_ r ||
                          let i, _ = skip_deleted ~limit:nd0 r in
                          f i
                        in
                        try
                          f x#initial_parent
                        with
                          _ -> false
                      in
                      if is_canceled then begin
                        DEBUG_MSG "%a is canceled" nps x;
                        Xset.add canceled x
                      end;
                      let b = not is_canceled in
                      DEBUG_MSG "x=%a b=%B" nps x b;
                      b
                    else begin
                      DEBUG_MSG "x=%a b=false" nps x;
                      false
                    end
                  in
                  let moveon x =
                    (not (self#is_stable x) || not (self#has_parent_key x)) &&
                    (*not (self#is_forced_upstream x) && *)not (Xset.mem canceled x) &&
                    not (Xset.mem nodes_added_to_ins_tbl_ x)
                  in
                  let is_upstable x =
                    self#is_stable pnd &&
                    self#is_stable x && self#has_parent_key_stable x &&
                    self#get_quasi_upstream_count x = 0
                  in
                  let reset x =
                    let b =
                      self#is_insert x && Xset.mem nodes_added_to_ins_tbl_ x
                    in
                    DEBUG_MSG "x=%a b=%B" nps x b;
                    b
                  in
                  let left_stable_nodes =
                    let lsnl0 = get_p_left_nodes ~moveon ~reset is_stable nd nd0 in
                    DEBUG_MSG "lsnl0=[%a]" nsps lsnl0;
                    let lsnl1 = get_p_left_nodes ~moveon is_upstable nd nd0 in
                    DEBUG_MSG "lsnl1=[%a]" nsps lsnl1;
                    Xlist.union lsnl0 lsnl1
                  in
                  let right_stable_nodes =
                    let rsnl0 = get_p_right_nodes ~moveon ~reset is_stable nd nd0 in
                    DEBUG_MSG "rsnl0=[%a]" nsps rsnl0;
                    let rsnl1 = get_p_right_nodes ~moveon is_upstable nd nd0 in
                    DEBUG_MSG "rsnl1=[%a]" nsps rsnl1;
                    Xlist.union rsnl0 rsnl1
                  in
                  DEBUG_MSG "nd=%a nd0=%a left_stable_nodes=[%a]" nps nd nps nd0 nsps left_stable_nodes;
                  DEBUG_MSG "nd=%a nd0=%a right_stable_nodes=[%a]" nps nd nps nd0 nsps right_stable_nodes;
                  if left_stable_nodes = [] then
                    if right_stable_nodes = [] then
                      0
                    else
                      -1
                  else if right_stable_nodes = [] then
                    1
                  else begin
                    List.iter
                      (fun s ->
                        if is_ancestor nd s then
                          try
                            let a, _ = skip_deleted ~limit:nd s in
                            if a == nd || is_ancestor a nd then begin
                              DEBUG_MSG "%a will be shifted" nps s;
                              Xset.add nodes_to_be_shifted s
                            end
                          with
                            _ -> ()
                      ) right_stable_nodes;
                    0
                  end
                else
                  0
        end
    in

    let has_quasi_upstream_descendant nd =
      let key_opt = self#find_key_opt_of_deleted nd#initial_parent in
      _has_quasi_upstream_descendant key_opt nd
    in

    let excluded_nodes_cache = Hashtbl.create 0 in

    let extra_upstream_nodes = Xset.create 0 in

    let rec find_anc n =
      let pn = n#initial_parent in
      if not (self#is_deleted pn) then
        pn, n#initial_pos
      else
        find_anc pn
    in

    let rec find_excluded_nodes ?(lv=0) rt nd =
      let indent = String.make lv ' ' in
      let _ = indent in
      DEBUG_MSG "%s[%d] rt=%a nd=%a" indent lv nps rt nps nd;

      if lv > 0 && Hashtbl.mem prune_tbl nd then begin
        DEBUG_MSG "%s[%d] %a is to be removed" indent lv nps nd;
        Xset.add to_be_removed nd
      end;

      try
        Hashtbl.find excluded_nodes_cache nd
      with
        Not_found ->

      let upc =
        try
          Hashtbl.find upstream_tbl nd
        with
          Not_found -> 0
      in
      DEBUG_MSG "%s[%d] upstream count of %a: %d" indent lv nps nd upc;

      let visited = Xset.create 0 in

      let excluded =
        try
          let nds =
            Hashtbl.find prune_tbl nd
          in

          DEBUG_MSG "%s[%d] nd=%a nds=[%a]" indent lv nps nd nsps nds;

          let nds_, _ =
            (*if upc = 0 && List.exists self#is_forced_upstream nds then begin
              (List.filter
                 (fun n ->
                   if self#is_forced_upstream n then
                     true
                   else begin
                     DEBUG_MSG "extra_del: %a" nps n;
                     add_extra_del n;
                     false
                   end
                 ) nds),
              0
            end
            else !!!!!*)if upc > 0 then begin
              DEBUG_MSG "upc=%d" upc;

              let rec scan prev_ins rt nd (nupsds, upsds, count, is_ins) =
                Xset.add visited nd;
                DEBUG_MSG "%a -> visited" nps nd;
                let ins = self#is_insert nd in
                DEBUG_MSG "%s[%d] %a prev_ins=%B ins=%B"
                  indent lv nps nd prev_ins ins;

                if not prev_ins && ins && List.memq nd nds then begin
                  Xset.remove visited nd;
                  DEBUG_MSG "%a -> not visited" nps nd;
                  nupsds, upsds, count, false
                end
                else if self#is_stable nd then begin

                  let count' = count - 1 in
                  DEBUG_MSG "%s[%d] count: %d -> %d" indent lv count count';

                  if count' >= 0 then begin
                    if prev_ins = false then begin
                      if self#is_insert rt then begin
                        DEBUG_MSG "extra_del: %a" nps nd;
                        add_extra_del nd
                      end
                    end;
                    nupsds, (nd :: upsds), count', false
                  end
                  else (* count' < 0 *)
                    (nd :: nupsds), upsds, count', false
                end
                else begin
                  Array.fold_right
                    (fun c acc ->
                      scan ins rt c acc
                    ) nd#initial_children (nupsds, upsds, count, is_ins && ins)
                end
              in (* scan *)

              if
                lv > 0 &&
                self#is_junc_node nd &&
                (match self#find_key_opt_of_deleted nd with
                | Some (K_mid m) ->
                    self#is_staying_move m && self#has_deleted_mems nd
                | _ -> false)
              then begin
                List.filter
                  (fun n ->
                    let is_forced_u = self#is_forced_upstream n in
                    if is_forced_u then
                      DEBUG_MSG "%a is a forced upstream node" nps n
                    else begin
                      let pnd = nd#initial_parent in
                      let ns =
                        sort_nds pnd (n::(Hashtbl.find prune_tbl pnd))
                      in
                      DEBUG_MSG "prune_tbl: replace %a -> [%a]" nps pnd nsps ns;
                      Hashtbl.replace prune_tbl pnd ns
                    end;
                    is_forced_u
                  ) nds,
                0
              end
              else

              let unds = Xset.create 0 in
              List.fold_right
                (fun n (l, count) ->
                  DEBUG_MSG "n=%a count=%d" nps n count;
                  if count <= 0 then begin
                    if
                      Hashtbl.mem upstream_tbl n ||
                      self#is_forced_upstream n
                    then begin
                      BEGIN_DEBUG
                        if Hashtbl.mem upstream_tbl n then
                          DEBUG_MSG "%a is upstream node" nps n;
                        if self#is_forced_upstream n then
                          DEBUG_MSG "%a is forced upstream node" nps n;
                      END_DEBUG;
                      (n::l), count
                    end
                    else begin

                      let is_del = self#is_deleted n in
                      let has_non_del =
                        not is_del || self#has_stable_or_inserted_descendant n
                      in
                      if not (Xset.mem unds n) && has_non_del then begin
                        DEBUG_MSG "%s[%d] not an upstream node: %a" indent lv nps n;
                        Xset.add not_upstream n;
                        remove_upstream_root n
                      end;
                      if
                        Xset.mem processed_nodes n ||
                        is_extra_del n ||
                        self#has_parent_key_stable n
                      then
                        l, count
                      else
                        (n::l), count

                    end
                  end
                  else if not (Xset.mem visited n) then begin
                    DEBUG_MSG "%s[%d] scanning %a" indent lv nps n;

                    let nupsds, upsds, count', is_ins' =
                      let is_ins = self#is_insert n in
                      scan is_ins n n ([], [], count, is_ins)
                    in

                    if count' < 0 then begin
                      DEBUG_MSG "%s[%d] partial upstream node: %a" indent lv nps n;
                      DEBUG_MSG "%s[%d] upstream descendants: [%a]" indent lv nsps upsds;
                      DEBUG_MSG "%s[%d] non-upstream descendants: [%a]" indent lv nsps nupsds;

                      DEBUG_MSG "prune_tbl: replace %a -> [%a]" nps rt nsps nupsds;
                      Hashtbl.replace prune_tbl rt nupsds;

                      Xset.add not_upstream rt;
                      remove_upstream_root rt;

                      List.iter (Xset.add unds) upsds

                    end;

                    List.iter add_upstream_root upsds;

                    (n :: l), count'
                  end
                  else
                    l, count
                ) nds ([], upc)
            end
            else begin (* upc = 0 *)
              DEBUG_MSG "upc=%d" upc;

              if Xset.mem dels_with_root_shifts rt then begin
                DEBUG_MSG "rt=%a is a del with root shifts" nps rt;
                let upsds, nupsds =
                  List.partition
                    (fun x ->
                      (has_quasi_upstream_descendant x(* && (
                       self#is_stable x ||
                       try
                         let a, _ = skip_deleted ~limit:rt#initial_parent x in
                         a != rt
                       with
                         _ -> false
                      )*)
                      ) ||
                      self#is_forced_upstream x
                    ) nds
                in
                DEBUG_MSG "upsds=[%a]" nsps upsds;

                let _extra_upsds = ref [] in
                List.iter
                  (fun n ->
                    DEBUG_MSG "n=%a" nps n;
                    let ns = try Hashtbl.find prune_tbl n with Not_found -> [] in
                    DEBUG_MSG "descendent del root: %a -> %a" nps rt nps n;
                    List.iter
                      (fun n0 ->
                        DEBUG_MSG "n0=%a" nps n0;
                        if
                          has_quasi_upstream_descendant n0 ||
                          self#is_forced_upstream n0 ||
                          self#has_parent_key_stable n0 && self#get_quasi_upstream_count n0 = 0
                        then begin
                          if not (List.memq n0 !_extra_upsds) then
                            _extra_upsds := n0 :: !_extra_upsds;
                        end
                      ) ns
                  ) (try Hashtbl.find extra_roots_tbl rt with Not_found -> []);

                let extra_upsds =
                  List.filter
                    (fun n ->
                      DEBUG_MSG "n=%a" nps n;
                      let b =
                        not (List.exists (fun x -> is_ancestor x n) !_extra_upsds) ||
                        self#is_insert n &&
                        let k = self#find_key n in
                        DEBUG_MSG "k=%s" (key_to_string k);
                        not
                          (List.exists
                             (fun x ->
                               try
                                 let xk = self#find_key x in
                                 DEBUG_MSG "x=%a xk=%s" nps x (key_to_string xk);
                                 k = xk && is_ancestor x n
                               with _ -> false
                             ) !_extra_upsds)
                      in
                      DEBUG_MSG "b=%B" b;
                      if b then
                        Xset.add extra_upstream_nodes n;
                      b
                    ) !_extra_upsds
                in
                DEBUG_MSG "extra_upsds=[%a]" nsps extra_upsds;

                let upsds = upsds @ extra_upsds in

                DEBUG_MSG "nds=[%a]" nsps nds;

                DEBUG_MSG "%s[%d] quasi(forced)-upstream nodes: [%a]" indent lv
                  nsps upsds;

                DEBUG_MSG "%s[%d] non-quasi(forced)-upstream nodes: [%a]" indent lv
                  nsps nupsds;

                if upsds <> [] then begin
                  let l =
                    Xlist.filter_map
                      (fun n ->
                        if self#has_parent n && self#has_parent_key n && Xset.mem processed_nodes n then
                          None
                        else if self#is_insert n then
                          Some n
                        else if self#has_parent_key_stable n && self#get_quasi_upstream_count n = 0 then
                          Some n
                        else if self#is_insert rt#initial_parent && self#is_deleted rt then
                          None
                        else begin
                          try
                            let (a0, p0 as a0_p0) = skip_deleted ~limit:rt n in
                            (*(self#find_subtree (self#find_key a0))#root*)
                            let a1, p1, _ = skip_inserted_ (*[]*) 0 a0_p0 in
                            let n' = a1#initial_children.(p1) in
                            if List.memq n' upsds then
                              None
                            else if Xset.mem processed_nodes n' then
                              None
                            else
                              Some n'
                          with
                            _ -> Some n
                        end
                      ) upsds
                  in
                  DEBUG_MSG "l=[%a]" nsps l;
                  let upsds_ = Xlist.uniqq (sort_nds rt l) in

                  DEBUG_MSG "%s[%d] upsds_=[%s]" indent lv
                    (nodes_to_uids_string upsds_);

                  if upsds_ <> [] then begin

                    let sn, pos = skip_touched rt in
                    DEBUG_MSG "rt=%a sn=%a pos=%d" nps rt nps sn pos;

                    (*let u0 = List.hd upsds_ in
                    let dir = get_dir sn pos u0 in*)
                    List.iter
                      (fun n ->
                        DEBUG_MSG "n=%a" nps n;
                        Xset.add not_excluded n;
                        add_upstream_root n;

                        begin
                          try
                            let pk =
                              try
                                self#get_upstream_dest n
                              with
                                _ -> self#find_parent_key n
                            in
                            DEBUG_MSG "pk=%s" (key_to_string pk);
                            match pk with
                            | K_mid _ | K_stid _ -> begin
                                let dn, pos, dir =
                                  let tn =
                                    get_p_ancestor
                                      (fun x ->
                                        try
                                          self#find_key x#initial_parent = pk
                                        with _ -> false
                                      ) n
                                  in
                                  DEBUG_MSG "tn=%a" nps tn;
                                  let dn = tn#initial_parent in
                                  let pos = tn#initial_pos in
                                  let dir =
                                    if self#is_deleted tn then
                                      0
                                    else
                                      get_dir dn pos n
                                  in
                                  dn, pos, dir
                                in
                                DEBUG_MSG "%s[%d] ins_tbl_: add %a pos=%d %a dir=%d" indent lv
                                  nps dn pos nps n dir;

                                tbl_add ins_tbl_ dn (pos, n, dir);
                                Xset.add nodes_added_to_ins_tbl_ n
                            end
                            | _ -> raise Not_found
                          with
                            Not_found ->
                              let dir = get_dir sn pos n in

                              DEBUG_MSG "%s[%d] ins_tbl_: add %a pos=%d %a dir=%d" indent lv
                                nps sn pos nps n dir;

                              tbl_add ins_tbl_ sn (pos, n, dir);
                              Xset.add nodes_added_to_ins_tbl_ n
                        end
                      ) upsds_

                  end;

                  let nupsds_ =
                    List.filter
                      (fun n ->
                        DEBUG_MSG "n=%a" nps n;

                        try
                          if self#get_quasi_upstream_count n > 0 then
                            raise Not_found;
                          let (pnd, pos, ofs) = self#find_parent n in
                          DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pnd pos ofs;
                          self#insert_cluster pnd pos ofs n [];
                          let anc, pos = find_anc n in
                          self#_prune_cluster anc pos [];
                          Xset.add not_excluded n;
                          false
                        with
                          Not_found ->

                        try
                          let _a, _ = skip_deleted ~limit:rt n in
                          let a = (self#find_subtree (self#find_key _a))#root in
                          DEBUG_MSG "a=%a" nps a;
                          let b = List.memq n nupsds && List.memq a upsds_ in
                          if b then begin
                            DEBUG_MSG "extra_del: %a" nps n;
                            add_extra_del ~stable_upsds:(List.memq n upsds) n
                          end;
                          not b
                        with
                          _ ->
                            if self#is_stable n && self#has_parent_key_stable n then begin
                              let a, pos = skip_deleted n in
                              if self#is_stable a then begin
                                let dir = get_dir a pos n in
                                DEBUG_MSG "%s[%d] ins_tbl_: add %a pos=%d %a dir=%d " indent lv
                                  nps a pos nps n dir;

                                tbl_add ins_tbl_ a (pos, n, dir);
                                Xset.add nodes_added_to_ins_tbl_ n;
                                false
                              end
                              else
                                true
                            end
                            else
                              true
                      )
                      (let filt x =
                        self#is_stable x &&
                        not (self#has_parent_key_stable x && self#get_quasi_upstream_count x = 0)
                      in
                      let l = Xlist.union nupsds (List.filter filt upsds) in
                      DEBUG_MSG "l=[%a]" nsps l;
                      let l = sort_nds rt l in
                      DEBUG_MSG "l=[%a] (sorted)" nsps l;
                      l)
                  in
                  DEBUG_MSG "%s[%d] nupsds_=[%s]" indent lv (nodes_to_uids_string nupsds_);

                  let nupsds__ =
                    Xlist.uniqq
                      (Xlist.filter_map
                         (fun n ->
                           try
                             let a, pos = skip_deleted ~limit:rt n in
                             let rec find_ins x =
                               let i = (self#find_subtree (self#find_key x))#root in
                               try
                                 find_ins i#initial_parent
                               with _ -> i
                             in
                             let ins = find_ins a in
                             DEBUG_MSG "ins=%a" nps ins;

                             if List.memq n nupsds && self#is_insert ins#initial_parent then
                               Xset.add not_excluded ins;

                             if Xset.mem upstream_roots ins then begin
                               DEBUG_MSG "%a is an upstream root" nps ins;
                               None
                             end
                             else begin

                               DEBUG_MSG "extra_del: %a" nps n;
                               add_extra_del n;

                               if List.memq ins nupsds_ then
                                 None
                               else
                                 Some ins

                             end
                           with
                             _ -> Some n
                         ) nupsds_)
                  in
                  DEBUG_MSG "%s[%d] nupsds__=[%s]" indent lv (nodes_to_uids_string nupsds__);

                  nupsds__, 0
                end
                else
                  nds, 0
              end
              else begin (* not (Xset.mem dels_with_root_shifts rt) *)
                DEBUG_MSG "rt=%a is not a del with root shifts" nps rt;
                let check_anc_ins ?(has_parent_key_stable=false) e =
                  DEBUG_MSG "e=%a" nps e;
                  DEBUG_MSG "is_insert=%B, is_forced_upstream=%B"
                    (self#is_insert e) (self#is_forced_upstream e);

                  let dest_key_opt =
                    try
                      let dk = self#get_upstream_dest e in
                      DEBUG_MSG "dk=%s" (key_to_string dk);
                      Some dk
                    with _ -> None
                  in

                  let stable_cond () =
                    let b =
                      has_parent_key_stable && self#is_stable e &&
                      (self#get_quasi_upstream_count e) = 0 &&
                      try
                        let a, _ = skip_deleted e in
                        a == rt#initial_parent && self#is_insert a
                      with
                        _ -> false
                    in
                    DEBUG_MSG "b=%B" b;
                    b
                  in

                  if
                    (self#is_insert e && self#is_forced_upstream e) ||
                    stable_cond()
                    (*&& self#has_true_stable_descendant e*)(*!!!!!!*)
                  then begin
                    let target =
                      try
                        let k = self#get_upstream_dest e in
                        let a =
                          get_p_ancestor
                            (fun x ->
                              let b =
                                match self#find_key_opt x with
                                | Some k0 -> k0 = k
                                | None -> false
                              in
                              DEBUG_MSG "b=%B" b;
                              b
                            ) e
                        in
                        DEBUG_MSG "a=%a" nps a;
                        a
                      with
                        _ -> tree#root
                    in
                    let sn, pos, lis = skip_touched_li ~target e in
                    DEBUG_MSG "sn=%a pos=%d" nps sn pos;

                    (*let lis_ =
                      List.filter
                        (fun (x, _) -> x#initial_nchildren > 1)
                        lis
                    in*)
                    match lis with
                    | (ins, p)::rest -> begin
                        DEBUG_MSG "ins=%a p=%d" nps ins p;

                        match self#find_key_opt ins with
                        (*| Some (K_mid _) -> true*)
                        | None -> assert false
                        | Some key -> begin
                            DEBUG_MSG "key=%s" (key_to_string key);
                            (*let ins_p_list =
                              match lis_ with
                              | (ins0, p0)::_ -> lis_
                              | _ -> [ins, p]
                            in*)
                            (*let get_dir_ext () =
                              let ep = Hashtbl.find path_tbl e in
                              let pos = ep#position in
                              let ofs = ep#offset in
                              let is_qu_s x =
                                DEBUG_MSG "x=%a" nps x;
                                let b =
                                  self#is_stable x &&
                                  let quc = self#get_quasi_upstream_count x in
                                  DEBUG_MSG "quc=%d" quc;
                                  quc > 0 && self#has_parent_key_stable x ||
                                  quc = 0 &&
                                  match self#find_parent_key_opt x with
                                  | Some K_stable -> (*true*)false
                                  | Some (K_stid _ | K_mid _ as k) -> begin
                                      DEBUG_MSG "k=%s" (key_to_string k);
                                      (try
                                        let ea = get_p_ancestor
                                            (fun x ->
                                              match self#find_key_opt x with
                                              | Some xk -> xk = k
                                              | None -> false
                                            ) e
                                        in
                                        DEBUG_MSG "found: ea=%a" nps ea;
                                        true
                                      with _ -> false) &&
                                      let r = (self#find_subtree k)#root in
                                      DEBUG_MSG "r=%a" nps r;
                                      self#is_stable r#initial_parent ||
                                      try
                                        let a, _ = skip_deleted r in
                                        DEBUG_MSG "a=%a" nps a;
                                        self#is_stable a
                                      with _ -> false
                                  end
                                  | _ -> false
                                in
                                DEBUG_MSG "x=%a b=%B" nps x b;
                                b
                              in
                              DEBUG_MSG "e=%a ep=%s pos=%d ofs=%f" nps e ep#to_string pos ofs;
                              if
                                ofs <> 0. && ep#upstream > 0 && ep#key_opt = Some K_stable &&
                                let x = e#initial_parent#initial_children.(pos) in
                                DEBUG_MSG "x=%a" nps x;
                                is_qu_s x || has_p_descendant is_qu_s x
                              then begin
                                DEBUG_MSG "!!!!!";
                                0
                              end
                              else
                                raise Not_found
                            in*)
                            (*let _ =
                              try
                                let d = get_dir_ext() in
                                DEBUG_MSG "d=%d" d
                              with
                                _ -> ()
                            in*)
                            let dest_key_cond =
                              match dest_key_opt with
                              | Some dest_key when dest_key = key -> true
                              | _ -> false
                            in
                            DEBUG_MSG "dest_key_cond=%B" dest_key_cond;
                            let dir =
                              (*try
                                get_dir_ext()
                              with
                                _ ->*)(*REGRESSION:k9mail/k-9 220, igvteam/igv 9 vs linkedin/sensei 33, ohmage/server 108*)
                              let left_stable = ref [] in
                              let right_stable = ref [] in
                              let cur =
                                if dest_key_cond then
                                  ref e
                                else
                                let rec f key = function
                                  | [] -> e
                                  | (ins0, _)::rest0 -> begin
                                      DEBUG_MSG "ins0=%a" nps ins0;
                                      match self#find_key_opt ins0 with
                                      | Some k0 -> begin
                                          DEBUG_MSG "k0=%s" (key_to_string k0);
                                          if
                                            k0 = key ||
                                            let kl = self#get_child_keys key in
                                            DEBUG_MSG "kl=[%s]" (String.concat ";" (List.map key_to_string kl));
                                            List.mem k0 kl
                                          then
                                            f k0 rest0
                                          else
                                            ins0
                                      end
                                      | _ -> ins0
                                  end
                                in
                                ref (f key rest)
                              in
                              DEBUG_MSG "cur=%a" nps !cur;
                              let ca = ref !cur#initial_parent#initial_children in
                              begin
                                try
                                  while not (self#is_stable !cur#initial_parent)(* && !cur != ins*) do
                                    DEBUG_MSG "cur=%a" nps !cur;
                                    let pos = (!cur)#initial_pos in
                                    DEBUG_MSG "pos=%d" pos;

                                    let k_opt = self#find_key_opt !cur in
                                    let _ = k_opt in
                                    DEBUG_MSG "k_opt=%s" (key_opt_to_string k_opt);
                                    (*begin
                                      match k_opt with
                                      | Some k when k <> key ->
                                          DEBUG_MSG "k=%s != key=%s" (key_to_string k) (key_to_string key);
                                          left_stable := []; right_stable := []
                                      | _ -> ()
                                    end;*)
                                    Array.iteri
                                      (fun i ci ->
                                        DEBUG_MSG "i=%d ci=%a" i nps ci;
                                        if
                                          i <> pos &&
                                          (self#is_stable ci ||
                                          self#has_true_stable_descendant ~weak:true ci &&
                                          try
                                            let moveon x = not (self#is_true_stable_node x) in
                                            let _ =
                                              get_p_ancestor ~moveon
                                                (Hashtbl.mem upstream_dest_tbl)(*self#is_forced_upstream*) ci
                                            in
                                            false
                                          with _ -> true)
                                        then begin
                                          DEBUG_MSG "found: ci=%a" nps ci;
                                          if i < pos then
                                            left_stable := ci :: !left_stable
                                          else if pos < i then
                                            right_stable := ci :: !right_stable;

                                          (*if !left_stable <> [] && !right_stable <> [] then
                                            raise Exit*)
                                        end
                                      ) !ca;
                                    cur := (!cur)#initial_parent;
                                    ca := (!cur)#initial_parent#initial_children
                                  done
                                with Exit -> ()
                              end;
                              DEBUG_MSG "left_stable=[%a]" nsps !left_stable;
                              DEBUG_MSG "right_stable=[%a]" nsps !right_stable;
                              let weak = ins#initial_parent != sn in
                              DEBUG_MSG "weak=%B" weak;
                              let filt x =
                                (not dest_key_cond || is_ancestor ins x) &&
                                if self#is_stable x then
                                  (try self#find_parent_key x <> key with _ -> true) &&
                                  self#is_true_stable_node ~weak:dest_key_cond(*~weak:true*) x
                                else
                                  self#has_true_stable_descendant ~weak x
                              in
                              let rec check ?(top=true) left right =
                                if left <> [] && right <> [] then
                                  if top then
                                    let left' = List.filter filt left in
                                    let right' = List.filter filt right in
                                    DEBUG_MSG "left'=[%a]" nsps left';
                                    DEBUG_MSG "right'=[%a]" nsps right';
                                    check ~top:false left' right'
                                  else
                                    0
                                else if left <> [] then
                                  1
                                else if right <> [] then begin
                                  -1
                                end
                                else
                                  0
                              in
                              check !left_stable !right_stable
                            in
                            DEBUG_MSG "dir=%d" dir;

                            let ipn = ins#initial_parent in
                            let has_del =
                              ipn != sn &&
                              try
                                let st, ed =
                                  if dir = 1 then
                                    ins#initial_pos + 1, ipn#initial_nchildren - 1
                                  else
                                    0, ins#initial_pos - 1
                                in
                                for i = st to ed do
                                  let n = ipn#initial_children.(i) in
                                  if self#is_stable n || self#has_true_stable_descendant n then
                                    raise Exit
                                done;
                                false
                              with
                                Exit -> true
                            in
                            DEBUG_MSG "has_del=%B" has_del;

                            Xset.add not_excluded e;
                            add_upstream_root e;

                            if has_del && not dest_key_cond then begin
                              let gen_op_tbl_modifier_spec() =
                                let ml =
                                  try
                                    Hashtbl.find op_tbl sn#uid
                                  with
                                    Not_found -> []
                                in
                                Xlist.filter_map
                                  (function
                                    | Otree.CMprune(p0, nds) when p0 = pos -> begin
                                        DEBUG_MSG "sn=%a p0=%d dir=%d e=%a ins=%a" nps sn p0 dir nps e nps ins;
                                        DEBUG_MSG "nds=[%a]" nsps nds;

                                        if sn == ins && nds = [] then
                                          Some (sn, p0, ins, e, 0)
                                        else
                                          let d =
                                            if dir = 0 then
                                              try
                                                if path_compare (self#get_path e) (self#get_path ins) > 0 then
                                                  1
                                                else
                                                  -1
                                              with _ ->
                                                if node_compare e ins > 0 then
                                                  1
                                                else
                                                  -1
                                            else
                                              dir
                                          in
                                          Some (sn, p0, ins, e, d)

                                    end
                                    | m -> None
                                  ) ml
                              in (* gen_op_tbl_modifier_spec *)
                              op_tbl_modifier_specs := gen_op_tbl_modifier_spec :: !op_tbl_modifier_specs
                            end
                            else begin
                              if not (Xset.mem nodes_added_to_ins_tbl_ e) then begin
                                DEBUG_MSG "%s[%d] ins_tbl_: add %a pos=%d %a dir=%d"
                                  indent lv nps sn pos nps e dir;

                                tbl_add ins_tbl_ sn (pos, e, dir);
                                Xset.add nodes_added_to_ins_tbl_ e
                              end
                            end;

                            false
                        end
                    end
                    | _ -> true
                  end
                  else
                    true
                in (* check_anc_ins *)
                let count = ref (-1) in
                let hidden_forced_upstream_nodes = Xset.create 0 in
                let find_hidden_fups e =
                  DEBUG_MSG "e=%a" nps e;
                  match self#find_key_opt e with
                  | None -> ()
                  | Some k ->
                      let moveon x = self#find_key_opt x = Some k in
                      let pred x =
                        let b = self#is_forced_upstream x in
                        if b then
                          Xset.add hidden_forced_upstream_nodes x;
                        b
                      in
                      ignore (get_p_descendants ~moveon pred e)
                in
                let nds' =
                  List.filter
                    (fun e ->
                      incr count;
                      DEBUG_MSG "e=%a" nps e;
                      find_hidden_fups e;
                      if self#is_deleted e then
                        true
                      else if Xset.mem nodes_added_to_ins_tbl_ e then
                        false
                      else begin
                        try
                          let k = self#find_parent_key e in
                          DEBUG_MSG "e=%a k=%s" nps e (key_to_string k);
                          let is_odd =
                            try
                              (self#find_key e) = k
                            with
                              Not_found -> false
                          in
                          DEBUG_MSG "is_odd=%B" is_odd;
                          if is_odd then
                            true
                          else begin
                            DEBUG_MSG "%s[%d] parent key found: %s"
                              indent lv (key_to_string k);

                            let lifter_cond0 =
                              (!count = 0) &&
                              let lifters = get_lifters e#initial_parent in
                              lifters <> [] &&
                              not (List.mem (Some k) lifters)
                            in
                            DEBUG_MSG "lifter_cond0=%B" lifter_cond0;

                            if
                              lifter_cond0 &&
                              let pnd, pos = skip_deleted e in
                              match self#find_key_opt pnd with
                              | Some k0 when k0 = k || self#get_quasi_upstream_count e > 0 && k <> K_stable ->
                                  self#insert_cluster pnd pos 0.0 e []; true
                              | _ -> false
                            then begin
                              false
                            end
                            else

                            match k with
                            | K_mid _ | K_stid _ when self#get_quasi_upstream_count e = 0 -> begin
                                let (pnd, pos, ofs) = self#find_parent e in
                                DEBUG_MSG "%s[%d] parent found: %a pos=%d ofs=%f"
                                  indent lv nps pnd pos ofs;

                                add_upstream_root e;
                                Xset.add processed_nodes e;
                                self#insert_cluster pnd pos ofs e [];
                                false
                            end
                            | _ -> check_anc_ins ~has_parent_key_stable:true e
                          end
                        with
                          Not_found -> check_anc_ins e
                      end
                    ) nds
                in
                let _ =
                  Xset.iter
                    (fun e ->
                      DEBUG_MSG "e=%a" nps e;
                      let target =
                        try
                          let k = self#get_upstream_dest e in
                          let a =
                            get_p_ancestor
                              (fun x ->
                                let b =
                                  match self#find_key_opt x with
                                  | Some k0 -> k0 = k
                                  | None -> false
                                in
                                DEBUG_MSG "b=%B" b;
                                b
                              ) e
                          in
                          DEBUG_MSG "a=%a" nps a;
                          a
                        with
                          _ -> tree#root
                      in
                      let sn, pos, lis = skip_touched_li ~target e in
                      DEBUG_MSG "sn=%a pos=%d" nps sn pos;

                      let dir = 0 in

                      DEBUG_MSG "%s[%d] ins_tbl_: add %a pos=%d %a dir=%d"
                        indent lv nps sn pos nps e dir;

                      tbl_add ins_tbl_ sn (pos, e, dir);
                      Xset.add nodes_added_to_ins_tbl_ e;
                      self#_prune_cluster e#initial_parent e#initial_pos []

                    ) hidden_forced_upstream_nodes
                in
                let nds'' =
                  List.map
                    (fun n ->
                      DEBUG_MSG "n=%a" nps n;
                      let qupc = self#get_quasi_upstream_count n in
                      let not_upstream =
                        not (self#is_upstream_node n) &&
                        (qupc > 0 || not (self#has_parent_key_stable n))
                      in
                      DEBUG_MSG "qupc=%d not_upstream=%B" qupc not_upstream;

                      let cond_stable = (self#is_stable n) && not_upstream in
                      let cond_insert0 = self#is_insert n in
                      let cond_insert1 =
                        let moveon x = x == n || not (Xset.mem upstream_roots x) in
                        try
                          preorder_scan_whole_initial_subtree ~moveon n
                            (fun x ->
                              if self#has_parent_key_stable x then
                                if (self#get_quasi_upstream_count x) = 0 then
                                  raise Exit);
                          true
                        with
                          Exit -> false
                      in
                      let cond_insert = cond_insert0 && not_upstream && cond_insert1 in
                      DEBUG_MSG "cond_stable=%B cond_insert=%B" cond_stable cond_insert;
                      DEBUG_MSG "cond_insert0=%B cond_insert1=%B" cond_insert0 cond_insert1;

                      if cond_stable || cond_insert then begin
                        try
                          let a, _ = skip_deleted ~limit:rt n in
                          DEBUG_MSG "a=%a" nps a;
                          let rec find_ins x =
                            let i = (self#find_subtree (self#find_key x))#root in
                            try
                              find_ins i#initial_parent
                            with _ -> i
                          in
                          let ins = find_ins a in

                          DEBUG_MSG "ins=%a" nps ins;

                          let ins =
                            try
                              let _, _, il = skip_touched_li ~limit:rt n in
                              match il with
                              | (i, _)::_ -> i
                              | _ -> ins
                            with
                              Failure _ -> ins
                          in

                          DEBUG_MSG "ins=%a" nps ins;

                          let is_staying =
                            try
                              let _ =
                                get_p_ancestor
                                  (fun x ->
                                    (x#gindex = ins#gindex) &&
                                    x#data#src_loc <> Loc.dummy
                                  ) ins
                              in
                              true
                            with
                              Not_found -> false
                          in
                          DEBUG_MSG "is_staying=%B" is_staying;

                          if
                            is_staying ||
                            (get_upstream_count ins) > 0 ||
                            (not (self#is_stable n) && has_quasi_upstream_descendant n)
                          then
                            n
                          else begin
                            DEBUG_MSG "%s[%d] foreign insert found: %a from %a (rt=%a)"
                              indent lv nps ins nps n nps rt;

                            if
                              Xset.exists (fun x -> is_ancestor x ins) extra_upstream_nodes
                            then begin
                              Xset.add not_excluded ins;
                              DEBUG_MSG "not_excluded: %a" nps ins;
                            end;

                            if qupc = 0 then begin
                              DEBUG_MSG "extra_del: %a" nps n;
                              add_extra_del ~notrans:true n;
                            end;

                            ins
                          end
                        with
                          _ -> n
                      end
                      else
                        n
                    ) nds'
                in
                nds'', 0
              end
            end
          in (* nds_ *)

          (*let nds_ = Xlist.uniqq nds_ in*)

          DEBUG_MSG "%s[%d] nds_=[%s]" indent lv (nodes_to_uids_string nds_);

          List.flatten
            (List.map (find_excluded_nodes ~lv:(lv+1) rt) nds_)
        with
          Not_found ->
            if lv = 0 then
              []
            else begin
              let is_root_of_upstream_staying_move =
                  if self#is_root_of_upstream_staying_move nd then
                    try
                      let x, _ = skip_deleted nd in
                      let is_stable = self#is_stable x in
                      DEBUG_MSG "x=%a is_stable=%B" nps x is_stable;
                      not is_stable
                    with
                      Failure _ -> true
                  else
                    false
              in

              BEGIN_DEBUG
                DEBUG_MSG "is_root_of_upstream_staying_move=%B"
                  is_root_of_upstream_staying_move;

                if self#is_extra_upstream_root nd then
                  DEBUG_MSG "%a is extra upstream root" nps nd;
              END_DEBUG;

              let has_upstream_sibling =
                Array.exists
                  (fun x ->
                    x != nd && self#is_stable x &&
                    try (Hashtbl.find upstream_tbl x) > 0 with _ -> false
                  ) nd#initial_parent#initial_children
              in
              DEBUG_MSG "has_upstream_sibling=%B" has_upstream_sibling;

              if
                is_root_of_upstream_staying_move || self#is_extra_upstream_root nd
              then begin
                Xset.add not_excluded nd;
                let sn, pos = skip_touched rt in

                let dir = get_dir sn pos nd in
                DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=%d" nps sn pos nps nd dir;
                tbl_add ins_tbl_ sn (pos, nd, dir);
                Xset.add nodes_added_to_ins_tbl_ nd;
                []
              end
              else if upc = 0 && has_upstream_sibling then begin
                let moveon x = not (self#is_stable x) in
                try
                  let un = get_p_ancestor ~moveon self#is_forced_upstream nd in
                  DEBUG_MSG "un=%a" nps un;
                  Xset.add not_excluded nd;
                  if self#is_deleted un#initial_parent then begin
                    DEBUG_MSG "initial parent of %a is deleted" nps un;
                    let pn, pos = skip_deleted un in
                    let dir = get_dir pn pos nd in
                    DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=%d" nps pn pos nps nd dir;
                    tbl_add ins_tbl_ pn (pos, nd, dir);
                    Xset.add nodes_added_to_ins_tbl_ nd;
                    []
                  end
                  else
                    [nd]
                with
                  Not_found -> [nd]
              end
              else
                [nd]
            end
      in (* excluded *)

      Hashtbl.add excluded_nodes_cache nd excluded;

      DEBUG_MSG "%s[%d] rt=%a nd=%a excluded=[%a]" indent lv
        nps rt nps nd nsps excluded;

      excluded
    in (* find_excluded_nodes *)


    begin
      DEBUG_MSG "updating prune_tbl...";
      let ktbl = Hashtbl.create 0 in
      List.iter
        (fun n ->
          try
            let k = self#find_key_of_deleted n in
            tbl_add ktbl k n
          with _ -> ()
        ) (Hashtbl.fold (fun nd _ l -> nd :: l) prune_tbl []);
      let is_stable_and_has_parent_key_stable x =
        self#is_stable x && self#has_parent_key_stable x
      in
      Hashtbl.iter
        (fun k ns ->
          match ns with
          | [] | [_] -> ()
          | _ -> begin
              let ns = sort_nodes_by_gindex ns in
              DEBUG_MSG "%s -> [%a]" (key_to_string k) nsps ns;
              let rec scan = function
                | [] | [_] -> ()
                | x::rest -> begin
                    DEBUG_MSG "x=%a" nps x;
                    List.iter
                      (fun y ->
                        DEBUG_MSG "y=%a" nps y;
                        let ys = (try Hashtbl.find prune_tbl y with _ -> []) in
                        if List.exists is_stable_and_has_parent_key_stable ys then
                          List.iter
                            (fun z ->
                              if
                                self#is_insert z && not (self#is_forced_upstream x) &&
                                is_ancestor z x &&
                                List.exists
                                  is_stable_and_has_parent_key_stable
                                  (try Hashtbl.find prune_tbl x with _ -> [])
                              then begin
                                DEBUG_MSG "prune_tbl: add %a -> %a" nps z nps x;
                                Xset.add excluded_insert_roots z;
                                tbl_add prune_tbl z x
                              end
                            ) ys
                      ) (List.filter (fun w -> is_ancestor w x) rest);
                    scan rest
                end
              in
              scan ns
          end
        ) ktbl;
      let rec get_nds nd =
        List.flatten
          (List.map
             (fun x ->
               (*if x == nd then
                 []
               else *)if self#is_deleted x then
                 try get_nds x with
                   Not_found -> [x]
                     (*let ds =
                       (List.filter
                          (self#is_deleted)
                          (Array.to_list x#initial_children))
                     in
                     if ds = [] then
                       [x]
                     else
                       List.flatten (List.map get_nds ds)*)
               else if self#is_insert x then begin
                 DEBUG_MSG "x=%a" nps x;
                 Xset.add excluded_insert_roots x;
                 try
                   let filt x =
                     self#has_parent_key_stable x ||
                     self#is_forced_upstream x &&
                     try self#get_upstream_dest x = K_stable with _ -> false
                   in
                   let nds = x::(List.filter filt (get_nds x)) in
                   DEBUG_MSG "%a: nds=[%a]" nps x nsps nds;
                   List.iter
                     (fun x ->
                       if self#is_forced_upstream x then
                         self#unforce_upstream x;
                       if self#is_root_of_upstream_staying_move x then
                         self#unreg_root_of_upstream_staying_move x
                     ) nds;
                   nds
                 with
                   Not_found -> [x]
               end
               else
                 [x]
             ) (Hashtbl.find prune_tbl nd)
          )
      in
      let tbl = Hashtbl.create 0 in
      let got_nds = Xset.create 0 in
      List.iter
        (fun n ->
          DEBUG_MSG "n=%a" nps n;
          let nds =
            List.filter (fun x -> not (Xset.mem got_nds x)) (get_nds n)
          in
          DEBUG_MSG "nds=[%a]" nsps nds;
          List.iter (Xset.add got_nds) nds;
          Hashtbl.add tbl n nds
        ) (sort_nodes_by_gindex ~descending:true
             (Hashtbl.fold (fun nd _ l -> nd :: l) prune_tbl []));

      DEBUG_MSG "excluded_insert_roots=[%a]" nsps (Xset.to_list excluded_insert_roots);

      Hashtbl.clear prune_tbl;
      Hashtbl.iter
        (fun n ns ->
          if not (Xset.mem excluded_insert_roots n) then begin
            DEBUG_MSG "%a -> [%a]" nps n nsps ns;
            Hashtbl.add prune_tbl n ns
          end
        ) tbl;
      DEBUG_MSG "prune_tbl updated.";
    end;

    DEBUG_MSG "dels_with_root_shifts: [%s]"
      (nodes_to_uids_string (Xset.to_list dels_with_root_shifts));

    let composed_prune_tbl = Hashtbl.create 0 in

    List.iter
      (fun nd ->
        let nds = Xlist.uniqq (find_excluded_nodes nd nd) in

        DEBUG_MSG "%a -> excluded:[%a]" nps nd nsps nds;

        Hashtbl.add composed_prune_tbl nd nds
      )
      (sort_nodes_by_gindex ~descending:true
         (Hashtbl.fold (fun nd _ l -> if List.memq nd l then l else nd :: l) prune_tbl []));


    DEBUG_MSG "not_upstream: [%a]" nsps (Xset.to_list not_upstream);
    DEBUG_MSG "processing non-upstream nodes...";

    Xset.iter
      (fun n ->
        DEBUG_MSG "n=%a" nps n;
        let r, ind_upward =
          try
            let orig = Hashtbl.find orig_upward_mem_tbl n in
            DEBUG_MSG "orig_upward_mem of %a: %a" nps n nps orig;
            orig, false
          with
            Not_found ->
              try
                let orig = Hashtbl.find orig_upward_mem_tbl (find_upward_mem n) in
                DEBUG_MSG "orig_upward_mem of %a: %a" nps n nps orig;
                orig, true
              with
                Not_found -> n, false
        in
        DEBUG_MSG "r=%a ind_upward=%B" nps r ind_upward;

        let ns =
          try
            Hashtbl.find prune_tbl n
          with
            Not_found ->
              if ind_upward && self#is_stable n then
                [n]
              else
                []
        in
        DEBUG_MSG "ns=[%a]" nsps ns;

        let a, pos = find_anc r in
        DEBUG_MSG "a=%a pos=%d" nps a pos;

        List.iter
          (fun n0 ->
            Xset.add not_excluded n0;
            if
              not (Xset.mem upstream_roots n0) &&
              not (self#is_deleted n0)
            then begin
              let has_ins_desc =
                try
                  preorder_scan_whole_initial_subtree n
                    (fun x ->
                      if self#is_insert x then
                        raise Exit
                    );
                  false
                with
                  Exit -> true
              in
              if has_ins_desc then begin
                let dir = get_dir a pos n0 in
                DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=%d" nps a pos nps n0 dir;
                tbl_add ins_tbl_ a (pos, n0, dir);
                Xset.add nodes_added_to_ins_tbl_ n0
              end
            end
          ) ns

      ) not_upstream;

    let find_anc_of_key ?(excluded_keys=[]) ?(weak=false) k nd =
      DEBUG_MSG "k=%s nd=%a excluded_keys=[%s]"
        (key_to_string k) nps nd (String.concat ";" (List.map key_to_string excluded_keys));
      (*Printf.printf "k=%s nd=%s\n" (key_to_string k) nd#initial_to_string;*)
      let key_filt x = try not (List.mem (self#find_key x) excluded_keys) with _ -> true in
      let stable_nodes = Xset.create 0 in
      let moveon x = not (self#is_stable x) && not (self#is_forced_upstream x) && key_filt x in
      let add_ss p pos =
        (*Printf.printf "add_ss: p=%s pos=%d\n" (UID.to_string p#uid) pos;*)
        List.iter
          (fun x ->
            DEBUG_MSG "x=%a" nps x;
            let ss =
              if (*self#is_stable x*)(self#is_true_stable_node ~weak) x then
                if key_filt x#initial_parent then
                  [x]
                else
                  []
              else
                get_p_descendants ~moveon (*self#is_stable*)(self#is_true_stable_node ~weak) x
            in
            DEBUG_MSG "stable nodes: [%a]" nsps ss;
            List.iter (Xset.add stable_nodes) ss
          )
          (List.filter
             (fun x -> x#initial_pos <> pos && key_filt x)
             (Array.to_list p#initial_children))
      in
      let rec find (*visited*) n =
        (*if List.memq n visited then begin
          WARN_MSG "infinite loop detected!: n=%a" nps n;
          failwith "find"
        end;*)
        let p = n#initial_parent in
        let pos = n#initial_pos in
        (*Printf.printf "n=%s\n" n#initial_to_string;*)
        (*DEBUG_MSG "%a (parent=%a)" nps n nps p;*)
        try
          let k' = self#find_key p in
          (*Printf.printf "%s has key %s\n" (UID.to_string p#uid) (key_to_string k');*)
          (*DEBUG_MSG "key found for %a: %s" nps p (key_to_string k');*)
          if k' = k then
            p, pos
          else begin
            add_ss p pos;
            find (*(n::visited)*) p
          end
        with
          Not_found ->
            if self#is_deleted p then begin
              (*Printf.printf "%s is deleted\n" (UID.to_string p#uid);*)
              add_ss p pos;
              find (*(n::visited)*) p
            end
            else
              p, n#initial_pos
      in
      let anc, pos = find (*[]*) nd in

      anc, pos, (Xset.to_list stable_nodes)
    in (* find_anc_of_key *)

    DEBUG_MSG "processing extra deletions...";

    let rec skip_ins_del count a0_p0 =
      let a1, p1, count_ = skip_inserted_ (*[]*) 0 a0_p0 in
      if self#is_deleted a1 then
        let (a2, p2 as a2_p2) = find_anc a1 in
        if self#is_stable a2 then
          a2_p2
        else
          try
            skip_ins_del count_ a2_p2
          with
            _ -> a0_p0
      else
        a1, p1
    in

    List.iter
      (fun n ->
        DEBUG_MSG "n=%a" nps n;
        let anc, pos =
          if self#has_parent_key_stable n then begin
            let (a0, p0 as a0_p0) = find_anc n in
            if Xset.mem supsds n then
              a0_p0
            else if self#is_insert a0 then
              skip_ins_del 0 a0_p0
            else
              a0_p0
          end
          else
            find_anc n
        in
        DEBUG_MSG "%a is to be pruned: pos=%d of %a" nps anc#initial_children.(pos) pos nps anc;
        self#_prune_cluster anc pos [];
        Xset.add not_excluded n;

        let anc, pos =
          try
            let pk = self#find_parent_key n in

            begin
              match pk with
              | K_stable -> if self#is_stable n then raise Not_found
              | _ -> ()
            end;

            let a, p, _ = find_anc_of_key pk n in
            a, p
          with
            Not_found -> anc, pos
        in

        let dir = get_dir anc pos n in
        DEBUG_MSG "ins_tbl: add %a pos=%d %a dir=%d " nps anc pos nps n dir;

        tbl_add (if (Xset.mem nox n) then ins_tbl_ else ins_tbl) anc (pos, n, dir)

      ) (List.rev !extra_dels);


    DEBUG_MSG "upstream_roots: [%a]" nsps (Xset.to_list upstream_roots);
    DEBUG_MSG "not_excluded: [%a]" nsps (Xset.to_list not_excluded);
    DEBUG_MSG "to_be_removed: [%a]" nsps (Xset.to_list to_be_removed);

    let find_parent_key nd =

      if Xset.mem invalidated_nodes nd then
        raise Not_found
      else

      let self_key_opt = self#find_key_opt nd in
      DEBUG_MSG "nd=%a self_key_opt=%s" nps nd (key_opt_to_string self_key_opt);
      let key_opt = ref None in
      let qupc = ref 0 in
      let rec scan keys ?(force=false) n =
        DEBUG_MSG "n=%a, force=%B" nps n force;

        if self#is_true_stable_node n then
          raise Not_found;

        if force || not (Xset.mem upstream_roots n) then begin
          try
            let pk_opt = Some (self#find_parent_key n) in
            DEBUG_MSG "pk_opt=%s" (key_opt_to_string pk_opt);
            if
              pk_opt <> self_key_opt &&
              match pk_opt with
              | Some pk when List.mem pk keys -> false
              | _ -> true
            then
              key_opt := pk_opt;
            qupc := self#get_quasi_upstream_count n;
            if !qupc > 0 then
              raise Exit
          with
            Not_found ->
              let k_opt = self#find_key_opt n in
              DEBUG_MSG "k_opt=%s" (key_opt_to_string k_opt);
              let keys' =
                match k_opt with
                | Some k when not (List.mem k keys) -> k::keys
                | _ -> keys
              in
              let n_is_del = self#is_deleted n in
              Array.iteri
                (fun i x ->
                  if
                    not (n_is_del && self#is_deleted x && not (self#has_key_opt k_opt x)) &&
                    not (n_is_del && self#is_insert x)
                  then
                    scan keys' ~force x
                  else if i = 0 && has_lifter n then
                    scan keys' ~force:true x
                ) n#initial_children
        end
      in
      begin
        try
          scan [] nd
        with
          Exit -> ()
      end;
      DEBUG_MSG "%a -> %s (qupc=%d)" nps nd (key_opt_to_string !key_opt) !qupc;
      match !key_opt with
      | Some k -> k, !qupc
      | None -> raise Not_found
    in (* find_parent_key *)

    let find_parent key nd =
      DEBUG_MSG "key=%s nd=%a" (key_to_string key) nps nd;
      let parent_opt = ref None in
      let moveon x =
        try
          (self#find_key x) = key
        with
          Not_found -> self#is_stable x && x#initial_nchildren = 0
      in
      begin
        try
          preorder_scan_whole_initial_subtree ~moveon nd
            (fun n ->
              DEBUG_MSG "%a" nps n;
              try
                let (pnd, pos, ofs) = self#find_parent n in
                let b =
                  try
                    (self#find_key pnd) <> key
                  with
                    Not_found -> true
                in
                if pnd != nd && b then
                  parent_opt := Some (pnd, pos, ofs);
                raise Exit
              with
                Not_found -> ()
            )
        with
          Exit -> ()
      end;
      match !parent_opt with
      | Some p -> p
      | None -> raise Not_found
    in

    Hashtbl.iter
      (fun nd nds ->
        DEBUG_MSG "nd=%a nds=[%a]" nps nd nsps nds;
        if Xset.mem to_be_removed nd then
          DEBUG_MSG "%a is to be removed" nps nd
        else begin
          let filt n =
            let to_be_shifted =
              Xset.mem nodes_to_be_shifted n ||
              get_p_descendants (Xset.mem nodes_to_be_shifted) n <> []
            in
            DEBUG_MSG "to_be_shifted: %a -> %B" nps n to_be_shifted;
            if to_be_shifted then begin
              let sn, pos = skip_touched nd in
              DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=1" nps sn pos nps n;
              tbl_add ins_tbl_ sn (pos, n, 1);
              Xset.add nodes_added_to_ins_tbl_ n
            end;
            not (Xset.mem not_excluded n) && not to_be_shifted
          in
          let nds =
            if Xset.mem not_upstream nd then
              nds
            else
              List.filter filt nds
          in
          let pnd = nd#initial_parent in

          DEBUG_MSG "pruning %a: prune_cluster %a (pos=%d) [%a]"
            nps nd nps pnd nd#initial_pos nsps nds;

          let frontier =
            try
              let k = self#find_key pnd in
              if k = (self#find_key_of_deleted nd) then
                match k with
                | K_mid mid -> Hashtbl.find frontier_tbl mid
                | _ -> []
              else
                []
            with
              Not_found -> []
          in
          DEBUG_MSG "frontier=[%s]"
            (String.concat ";" (List.map (fun (n, p) -> sprintf "%a:%d" nps n p) frontier));
          DEBUG_MSG "|frontier|=%d" (List.length frontier);

          let nds_ =
            (*if (List.length frontier) < 2 || (List.length nds) < 2 then begin*)
            if
              (List.length frontier) < 2 ||
              let filtered =
                List.filter
                  (fun n ->
                    not (self#get_upstream_count n = 0 && self#has_parent_key n)
                  ) nds
              in
              DEBUG_MSG "filtered=[%a]" nsps filtered;
              (List.length filtered) < 2
            then begin

              let all_qun =
                nds <> [] &&
                List.for_all
                  (fun n ->
                    try
                      let pk, qupc = find_parent_key n in
                      DEBUG_MSG "parent key: %s" (key_to_string pk);
                      (*pk = K_stable && *)qupc > 0
                    with
                      Not_found -> false
                  ) nds
              in
              DEBUG_MSG "all_qun=%B" all_qun;

              let anc_ins_cond =
                try
                  let x, _, il = skip_touched_li nd#initial_parent in
                  self#is_stable x && il <> []
                with
                  Failure _ -> false
              in
              DEBUG_MSG "anc_ins_cond=%B" anc_ins_cond;
              let sub_cond = anc_ins_cond || not all_qun in

              List.filter
                (fun n ->
                  DEBUG_MSG "n=%a" nps n;
                  try
                    if self#has_parent n && self#get_quasi_upstream_count n = 0 then begin
                      let (pn, pos, ofs) = self#find_parent n in
                      DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pn pos ofs;
                      self#insert_cluster pn pos ofs n [];
                      false
                    end
                    else

                    let pk, qupc = find_parent_key n in
                    DEBUG_MSG "parent key: %s (qupc=%d)" (key_to_string pk) qupc;

                    let k = self#find_key pnd in
                    DEBUG_MSG "original parent key: %s (pnd=%a)" (key_to_string k) nps pnd;

                    let is_upward_node x =
                      self#is_forced_upstream x || has_quasi_upstream_descendant x
                    in

                    let change_cond =
                      pk <> k &&
                      self#get_quasi_upstream_count n = 0 &&
                      (self#is_stable n || qupc > 0) &&
                      sub_cond
                    in
                    DEBUG_MSG "change_cond=%B" change_cond;

                    if change_cond then begin
                      let excluded_keys =
                        let kr = (self#find_subtree k)#root in
                        DEBUG_MSG "kr=%a" nps kr;
                        let l = ref [] in
                        if is_upward_node kr then
                          l := k :: !l;
                        let cur = ref kr#initial_parent in
                        begin
                          try
                            while true do
                              let ak = self#find_key !cur in
                              if ak = pk then
                                raise Exit;
                              DEBUG_MSG "ak=%s" (key_to_string ak);
                              let akr = (self#find_subtree ak)#root in
                              DEBUG_MSG "akr=%a" nps akr;
                              if is_upward_node akr then begin
                                l := ak :: !l;
                                if self#is_ancestor_key ak k then
                                  l := k :: !l
                              end;
                              cur := akr#initial_parent
                            done
                          with
                            _ -> ()
                        end;
                        !l
                      in
                      let anc, pos, stable_nd_list = find_anc_of_key ~excluded_keys ~weak:true pk n in
                      DEBUG_MSG "stable_nd_list=[%a]" nsps stable_nd_list;
                      let stable_nd_list =
                        let n = anc#initial_children.(pos) in
                        if self#is_insert n then begin
                          DEBUG_MSG "%a is an insert" nps n;
                          let l =
                            List.filter
                              (fun x ->
                                not (self#has_parent_key x)
                              ) stable_nd_list
                          in
                          DEBUG_MSG "stable_nd_list -> [%a]" nsps l;
                          l
                        end
                        else
                          stable_nd_list
                      in
                      let min_gi, max_gi =
                        try
                          get_range (List.map (fun x -> x#gindex) stable_nd_list)
                        with
                          Invalid_argument _ -> GI.dummy, GI.dummy
                      in
                      DEBUG_MSG "anc of n=%a -> %a (pos=%d,min_gi=%a,max_gi=%a)"
                        nps n nps anc pos GI.ps min_gi GI.ps max_gi;

                      let dir =
                        if max_gi > 0 && min_gi > 0 then begin
                          let ss =
                            if self#is_stable n then
                              [n]
                            else
                              get_p_descendants self#is_stable n
                          in
                          DEBUG_MSG "n=%a ss=[%a]" nps n nsps ss;
                          if ss = [] then begin
                            let lss = get_p_left_nodes self#is_stable n nd in
                            DEBUG_MSG "lss=[%a]" nsps lss;
                            if lss = [] then begin
                              let rss = get_p_right_nodes self#is_stable n nd in
                              DEBUG_MSG "rss=[%a]" nsps rss;
                              if List.for_all (fun x -> x#gindex > max_gi) rss then
                                1
                              else
                                -1
                            end
                            else if List.for_all (fun x -> x#gindex > max_gi) lss then
                              1
                            else
                              -1
                          end
                          else if List.for_all (fun x -> x#gindex > max_gi) ss then
                            1
                          else if List.for_all (fun x -> x#gindex < min_gi) ss then
                            -1
                          else
                            0
                        end
                        else
                          -1
                      in
                      DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=%d"
                        nps anc pos nps n dir;
                      Xset.add not_excluded n;
                      tbl_add ins_tbl_ anc (pos, n, dir);
                      Xset.add nodes_added_to_ins_tbl_ n
                    end;

                    not change_cond
                  with
                    Not_found ->
                      if Xset.mem invalidated_nodes n then
                        true
                      else
                      try
                        let k = self#find_key n in
                        let (pn, pos, ofs) = find_parent k n in
                        DEBUG_MSG "parent found: %a pos=%d ofs=%f" nps pn pos ofs;
                        self#insert_cluster pn pos ofs n [];
                        false
                      with
                        Not_found -> true
                ) nds
            end
            else begin (* |frontier| > 1 && |nds| > 1 *)
              nds
            end
          in (* nds_ *)
          DEBUG_MSG "nds_=[%a]" nsps nds_;

          let inconsistent =
            try pnd#initial_children.(nd#initial_pos) != nd with _ -> false
          in
          DEBUG_MSG "inconsistent=%B" inconsistent;

          if nds_ = [] && inconsistent then
            DEBUG_MSG "dropping inconsistent %a" nps nd
          else begin
            if
              (List.length nds_) = 1 ||
              (Xset.length upstream_roots) = 0 ||
              List.for_all
                (fun n ->
                  not (Xset.mem upstream_roots n) &&
                  self#get_upstream_count n = 0
                ) nds_
            then
              self#_prune_cluster pnd nd#initial_pos nds_
            else begin
              let sorted_nds_ = sort_nodes nds_ in
              self#_prune_cluster pnd nd#initial_pos sorted_nds_
            end;
            if inconsistent then begin
              let x = pnd#initial_children.(nd#initial_pos) in
              DEBUG_MSG "recovering %a" nps x;
              self#insert_cluster pnd nd#initial_pos (-1.) x [];
              Xset.add recovered_nodes x
            end
          end
        end (* if not (Xset.mem to_be_removed nd) *)
      ) composed_prune_tbl;

    Xset.iter
      (fun nd ->
        if
          not (Xset.mem nodes_added_to_ins_tbl_ nd) &&
          not (Xset.mem not_excluded nd) &&
          try
            let i, _ = skip_deleted nd in
            not (self#is_stable i)
          with _ -> true
        then begin
          DEBUG_MSG "nd=%a" nps nd;
          let target =
            try
              let k = self#get_upstream_dest nd in
              let a =
                get_p_ancestor
                  (fun x ->
                    let b =
                      match self#find_key_opt x with
                      | Some k0 -> k0 = k
                      | None -> false
                    in
                    DEBUG_MSG "b=%B" b;
                    b
                  ) nd
              in
              DEBUG_MSG "a=%a" nps a;
              a
            with
              _ -> tree#root
          in
          let sn, pos, lis = skip_touched_li ~target nd in
          DEBUG_MSG "sn=%a pos=%d" nps sn pos;

          let dir = 1 in

          DEBUG_MSG "ins_tbl_: add %a pos=%d %a dir=%d" nps sn pos nps nd dir;

          tbl_add ins_tbl_ sn (pos, nd, dir);
          Xset.add nodes_added_to_ins_tbl_ nd;
          self#_prune_cluster nd#initial_parent nd#initial_pos []
        end
      ) forced_upstream_nodes;

    let do_ins dir anc pos_n_dir_list =
      DEBUG_MSG "dir=%d anc=%a" dir nps anc;

      let compare =
        if dir > 0 then
          Stdlib.compare
        else
          fun x y -> Stdlib.compare y x
      in

      let is_stable x = self#is_stable x(* && not (self#has_parent_key x)*) in
      let moveon x = true(*not (self#is_stable x) || not (self#has_parent_key x)*) in

      let sort = function
        | [] -> []
        | [n] as nl -> nl
        | nl -> begin
            let rec _cmp n0 n1 acc (al0, al1) =
              let default() =
                if acc = 0 then
                  dir * (compare_nodes n0 n1)
                else
                  acc
              in
              match al0, al1 with
              | [], _ | _, [] -> default()
              | (a0, p0)::l0, (a1, p1)::l1 ->
                  if a0 == a1 then begin
                    if p0 = p1 then begin
                      match l0, l1 with
                      | [], _::_ -> begin
                          let _ss0 = get_p_descendants ~moveon is_stable n0 in
                          let ss1 = get_p_descendants ~moveon is_stable n1 in
                          let ss0 = Xlist.subtractq _ss0 ss1 in
                          DEBUG_MSG "ss0=[%a]" nsps ss0;
                          DEBUG_MSG "ss1=[%a]" nsps ss1;
                          if (List.length ss0) > 0 && (List.length ss1) > 0 then
                            if (Xlist.last ss0)#gindex < (List.hd ss1)#gindex then
                              dir*(-1)
                            else if (Xlist.last ss1)#gindex < (List.hd ss0)#gindex then
                              dir*1
                            else
                              default()
                          else
                            default()
                      end
                      | _::_, [] -> begin
                          let _ss1 =
                            if is_stable n1 then
                              [n1]
                            else
                              get_p_descendants ~moveon is_stable n1
                          in
                          let ss0 =
                            if is_stable n0 then
                              [n0]
                            else
                              get_p_descendants ~moveon is_stable n0
                          in
                          let ss1 = Xlist.subtractq _ss1 ss0 in
                          DEBUG_MSG "ss0=[%a]" nsps ss0;
                          DEBUG_MSG "ss1=[%a]" nsps ss1;
                          if (List.length ss0) > 0 && (List.length ss1) > 0 then
                            if (Xlist.last ss0)#gindex < (List.hd ss1)#gindex then
                              dir*(-1)
                            else if (Xlist.last ss1)#gindex < (List.hd ss0)#gindex then
                              dir*1
                            else
                              default()
                          else
                            default()
                      end
                      | _ -> _cmp n0 n1 (compare p0 p1) (l0, l1)
                    end
                    else
                      compare p0 p1
                  end
                  else
                    default()
            in
            let cmp (n0, nal0) (n1, nal1) =
              let c = _cmp n0 n1 0 (nal0, nal1) in
              DEBUG_MSG "%a vs %a: %d" nps n0 nps n1 c;
              c
            in
            let nal = List.map (fun n -> n, get_ancestors n) nl in
            List.map (fun (n, _) -> n) (List.fast_sort cmp nal)
        end
      in (* sort *)

      let tbl = Hashtbl.create 0 in (* pos -> node list *)
      List.iter
        (fun (pos, n, _) ->
          tbl_add tbl pos n
        ) pos_n_dir_list;
      Hashtbl.iter
        (fun pos nl ->
          DEBUG_MSG "pos=%d" pos;
          let sorted = sort nl in
          let ofs = ref (float dir) in
          List.iter
            (fun n ->
              DEBUG_MSG "  n=%a" nps n;
              self#insert_cluster anc pos !ofs n [];
              ofs := !ofs +. (float dir)
            ) sorted
        ) tbl
    in (* do_ins *)
    let do_ins0 anc pos_n_dir_list =
      let tbl = Hashtbl.create 0 in (* pos -> node list *)
      List.iter
        (fun (pos, n, _) ->
          tbl_add tbl pos n
        ) pos_n_dir_list;
      Hashtbl.iter
        (fun pos nl ->
          DEBUG_MSG "pos=%d" pos;
          let sorted = List.rev nl in
          List.iteri
            (fun i n ->
              DEBUG_MSG "i=%d n=%a" i nps n;
              self#insert_cluster(* ~no_trans:true*) anc pos 0.0 n [];
            ) sorted
        ) tbl
    in (* do_ins0 *)
    let handle_ins_tbl =
      Hashtbl.iter
        (fun anc pos_n_dir_list ->
          DEBUG_MSG "anc=%a" nps anc;
          let zero, non_zero =
            List.partition (fun (_, _, dir) -> dir = 0) pos_n_dir_list
          in
          let plus, minus =
            List.partition (fun (_, _, dir) -> dir > 0) non_zero
          in
          if zero <> [] then
            do_ins0 anc zero;
          if plus <> [] then
            do_ins 1 anc plus;
          if minus <> [] then
            do_ins (-1) anc minus
        )
    in
    let sanitize_ins_tbl tbl =
      Hashtbl.iter
        (fun nd pos_n_dir_list ->
          let nds = Xset.create 0 in
          let pos_n_dir_list =
            List.filter
              (fun (pos, n, dir) ->
                if Xset.mem nds n then begin
                  false
                end
                else begin
                  Xset.add nds n;
                  true
                end
              ) pos_n_dir_list
          in
          Hashtbl.replace tbl nd pos_n_dir_list
        ) tbl
    in
    sanitize_ins_tbl ins_tbl;
    sanitize_ins_tbl ins_tbl_;

    begin
      let to_be_removed = ref [] in
      Hashtbl.iter
        (fun nd pos_n_dir_list ->
          DEBUG_MSG "ins_tbl:";
          List.iter
            (fun (pos, n, dir) ->
              DEBUG_MSG "pos=%d n=%a dir=%d" pos nps n dir
            ) pos_n_dir_list;
          try
            let pos_n_dir_list_ = Hashtbl.find ins_tbl_ nd in
            DEBUG_MSG "ins_tbl_:";
            List.iter
              (fun (pos, n, dir) ->
                DEBUG_MSG "pos=%d n=%a dir=%d" pos nps n dir
              ) pos_n_dir_list_;

            to_be_removed := nd :: !to_be_removed;
            Hashtbl.replace ins_tbl_ nd (pos_n_dir_list @ pos_n_dir_list_)
          with
            Not_found -> ()
        ) ins_tbl;
      List.iter (Hashtbl.remove ins_tbl) !to_be_removed
    end;

    begin
      let tbl = Hashtbl.create 0 in
      let r_tbl = Hashtbl.create 0 in
      let l_tbl = Hashtbl.create 0 in
      let sns = Xset.create 0 in
      List.iter
        (fun f ->
          List.iter
            (fun (sn, p0, ins, e, d) ->
              DEBUG_MSG "sn=%a p0=%d ins=%a e=%a d=%d" nps sn p0 nps ins nps e d;
              Xset.add sns sn;
              if sn == ins && d = 0 then
                tbl_add_tbl tbl sn p0 e
              else if d > 0 then
                tbl_add_tbl2_list r_tbl sn p0 ins e
              else if d < 0 then
                tbl_add_tbl2_list l_tbl sn p0 ins e
            ) (f())
        ) !op_tbl_modifier_specs;
      Xset.iter
        (fun sn ->
          DEBUG_MSG "sn=%a" nps sn;
          let ml =
            try
              Hashtbl.find op_tbl sn#uid
            with
              Not_found -> []
          in
          let tbl' = try Hashtbl.find tbl sn with _ -> Hashtbl.create 0 in
          let r_tbl' = try Hashtbl.find r_tbl sn with _ -> Hashtbl.create 0 in
          let l_tbl' = try Hashtbl.find l_tbl sn with _ -> Hashtbl.create 0 in
          let ml' =
            List.map
              (function
                | Otree.CMprune(p0, nds) -> begin
                    try
                      let e = Hashtbl.find tbl' p0 in
                      if nds = [] then
                        Otree.CMprune(p0, [e])
                      else
                        raise Not_found
                    with
                      Not_found -> begin
                        let rmap = try Hashtbl.find r_tbl' p0 with _ -> Hashtbl.create 0 in
                        let lmap = try Hashtbl.find l_tbl' p0 with _ -> Hashtbl.create 0 in
                        let nds' =
                          List.flatten
                            (List.map
                               (fun nd ->
                                 let rl = try Hashtbl.find rmap nd with _ -> [] in
                                 let ll = try Hashtbl.find lmap nd with _ -> [] in
                                 if rl <> [] || ll <> [] then begin
                                   let l = ll @ (nd::rl) in
                                   DEBUG_MSG "%a -> [%a]" nps nd nsps l;
                                   l
                                 end
                                 else
                                   [nd]
                               ) nds)
                        in
                        Otree.CMprune(p0, nds')
                      end
                end
                | m -> m
              ) ml
          in
          Hashtbl.replace op_tbl sn#uid ml'
        ) sns
    end;

    handle_ins_tbl ins_tbl;
    self#translate_positions();
    handle_ins_tbl ins_tbl_;
    Xset.clear nodes;
    DEBUG_MSG "@";
    self#_mutate ~get_idx_opt:(Some get_idx) nodes;

    BEGIN_DEBUG
      self#dump_dot "final.dot";
    END_DEBUG;

    DEBUG_MSG "finished"


  method reg_deleted (path : path_c) (paths : boundary) (key_opt : subtree_key option) =
    let move_flag =
      match key_opt with
      | Some (K_mid _ as mk) -> begin
          if List.for_all (fun x -> x#upstream = 0 && x#key_opt = None && x#sub_path_opt = None) paths then
            Hashtbl.add del_spec_tbl mk (path, paths);
          true
      end
      | Some (K_del _ as dk) -> begin
          if List.for_all (fun x -> x#upstream = 0 && x#key_opt = None && x#sub_path_opt = None) paths then
            Hashtbl.add del_spec_tbl dk (path, paths);
          false
      end
      | _ -> false
    in
    let nd = self#acc path#path in
    DEBUG_MSG "path=%s paths=[%s]" path#to_string (String.concat "; " (List.map (fun x -> x#to_string) paths));
    DEBUG_MSG "nd=%a key=%s move_flag=%B" nps nd (key_opt_to_string key_opt) move_flag;
    let nds =
      List.map (fun p -> self#acc (Path.concat path#path p#path)) paths
    in
    let mems = ref [] in
    let sub_del_mem_tbl = Hashtbl.create 0 in
    scan_initial_cluster nd nds
      (fun n ->
        DEBUG_MSG "%a -> %s" nps n (key_opt_to_string key_opt);

        if move_flag && n != nd && Hashtbl.mem deleted_mems_tbl n then begin
          try
            match Hashtbl.find deleted_subtree_tbl n with
            | Some (K_del _ as dk) -> begin
                DEBUG_MSG "%s may be contained in %s" (key_to_string dk) (key_opt_to_string key_opt);
                let ms = List.map (fun (x, _) -> x) (Hashtbl.find deleted_mems_tbl n) in
                Hashtbl.add sub_del_mem_tbl dk ms
            end
            | Some (K_mid _ as mk) -> begin
                DEBUG_MSG "%s may be contained in %s" (key_to_string mk) (key_opt_to_string key_opt);
                let ms = List.map (fun (x, _) -> x) (Hashtbl.find deleted_mems_tbl n) in
                Hashtbl.add sub_del_mem_tbl mk ms
            end
            | _ -> ()
          with
            Not_found -> ()
        end;

        Hashtbl.add deleted_subtree_tbl n key_opt;
        mems := n :: !mems
      );

    if move_flag then begin
      match key_opt with
      | Some (K_mid mid) -> begin
          let rt_path = path#path in
          Hashtbl.iter
            (fun dk ms ->
              if List.for_all (fun x -> List.memq x !mems) ms then begin
                DEBUG_MSG "%s is properly contained in %s" (key_to_string dk) (key_opt_to_string key_opt);
                try
                  let p, pl = Hashtbl.find del_spec_tbl dk in
                  let p' = new path_c (get_rel_path rt_path p#path) in
                  let sub_del_spec = (p', pl) in
                  DEBUG_MSG "sub_del_spec: (%s,[%s])"
                    p'#to_string (String.concat ";" (List.map (fun x -> x#to_string) pl));
                  Hashtbl.add sub_del_spec_tbl mid sub_del_spec
                with
                  _ -> DEBUG_MSG "%s contains decorated paths" (key_to_string dk);
              end
            ) sub_del_mem_tbl
      end
      | _ -> ()
    end;

    Hashtbl.add deleted_mems_tbl nd
      (List.map
         (fun n ->
           let ns =
             List.filter
               (fun n0 -> List.memq n0 !mems)
               (Array.to_list n#initial_children)
           in
           n, ns
         ) !mems)

  method reg_parent (path : path_c) (paths : boundary) =
    List.iter
      (fun p ->
        match p#key_opt with
        | Some k -> begin
            let n = self#acc (Path.concat path#path p#path) in

            let nl = ref [n] in

            if not (self#is_stable n) then begin
              DEBUG_MSG "%a is not stable! adjusting..." nps n;
              let moveon x = not (self#is_stable x) in
              nl := get_p_descendants ~moveon self#is_stable n;
              DEBUG_MSG "%a -> [%a]" nps n nsps !nl
            end;

            List.iter
              (fun n ->
                DEBUG_MSG "n=%a" nps n;
                Hashtbl.add parent_key_tbl n k;
                DEBUG_MSG "%a -> %s" nps n (key_to_string k);
                match p#sub_path_opt with
                | Some sp -> begin
                    DEBUG_MSG "sp=%s" (Path.to_string sp);
                    try
                      let subtree = self#find_subtree k in
                      DEBUG_MSG "subtree:\n%s\n" subtree#to_string;
                      let a = subtree#initial_acc_parent ?ignore_ofs:(Some true) ?from:None sp in
                      DEBUG_MSG "a.partial=%B a.nelems=%d" a.Otree.partial a.Otree.nelems;
                      let pnd, elem = a.Otree.node, a.Otree.elem in
                      DEBUG_MSG "pnd=%a" nps pnd;
                      if a.Otree.partial then begin
                        let rp = Path.remove_head_n a.Otree.nelems sp in
                        let resolver() = (* parent resolution is deferred *)
                          DEBUG_MSG "n=%a, pnd=%a" nps n nps pnd;
                          DEBUG_MSG "sp=%s, rp=%s" (Path.to_string sp) (Path.to_string rp);
                          let a' =
                            subtree#initial_acc_parent ?ignore_ofs:(Some true) ?from:(Some pnd) rp
                          in
                          let pnd', elem' = a'.Otree.node, a'.Otree.elem in
                          let pos', ofs' = elem'.Elem.pos, elem'.Elem.ofs in
                          DEBUG_MSG "(%a, %d, %f)" nps pnd' pos' ofs';
                          (pnd', pos', ofs')
                        in
                        self#register_parent_resolver n resolver
                      end
                      else begin
                        let pos, ofs = elem.Elem.pos, elem.Elem.ofs in
                        DEBUG_MSG "parent_tbl: %a -> (%a, %d, %f)" nps n nps pnd pos ofs;
                        self#add_to_parent_tbl n (pnd, pos, ofs)
                      end
                    with
                      Not_found -> ()
                end
                | None -> ()
              ) !nl
        end
        | None -> ()
      ) paths

  method reg_subtree
      ?(key_opt=(None : subtree_key option))
      ?(adj_opt=(None : int option))
      ?(depth_opt=(None : int option))
      ?(shift_opt=(None : int option))
      stid subtree (path : path_c) (paths : boundary) =
    Hashtbl.add subtree_tbl stid subtree;
    Hashtbl.add path_tbl subtree#root path;
    begin
      match key_opt with
      | Some k -> Hashtbl.add path_key_tbl subtree#root k
      | None -> ()
    end;
    begin
      match depth_opt with
      | Some d -> Hashtbl.add path_depth_tbl subtree#root d
      | None -> ()
    end;

    if path#key_opt = Some K_stable then begin
      self#reg_root_of_upstream_staying_move subtree#root;
    end;

    let key = key_of_stid stid in
    subtree#scan_whole_initial
      (fun n ->
        DEBUG_MSG "%a -> %s" nps n (key_to_string key);

        Hashtbl.add key_tbl n key
      );
(*
    let keys = Xlist.filter_map (fun p -> p#key_opt) paths in
    DEBUG_MSG "keys=[%s]" (String.concat "," (List.map key_to_string keys));
    begin
      match keys with
      | [] -> ()
      | [k] -> self#mark_key k
      | k::rest ->
          if List.for_all (fun x -> x = k) rest then
            self#mark_key k
    end;
*)
    List.iter
      (fun p ->
        match p#key_opt with
        | Some k -> begin
            self#mark_key k;
            DEBUG_MSG "marked: %s" (key_to_string k);
            self#link_keys key k
        end
        | _ -> ()
      ) paths;

    self#reg_boundary key paths


  method reg_moved_subtree
      ?(key_opt=(None : subtree_key option))
      ?(adj_opt=(None : int option))
      ?(depth_opt=(None : int option))
      ?(shift_opt=(None : int option))
      mid
      (path_from : path_c) (paths_from : boundary)
      (path_to : path_c) (paths_to : boundary)
      =
    DEBUG_MSG "mid=%a path_from=%s" MID.ps mid path_from#to_string;

    let is_staying =
      Path.is_prefix path_from#path path_to#path
    in
    DEBUG_MSG "is_staying=%B" is_staying;
    if is_staying then
      Xset.add staying_moves mid;

    let nd = self#acc path_from#path in

    DEBUG_MSG "nd=%s" nd#initial_to_string;

    let find_move_relabel nd =
      Hashtbl.find move_relabel_tbl nd
    in
    let subtree =
      tree#make_subtree_copy ?find_hook:(Some find_move_relabel) nd
    in

    subtree#setup_apath;

    DEBUG_MSG "root=%s" subtree#root#initial_to_string;

    Hashtbl.add excluded_paths_tbl mid (subtree, nd, paths_from);

    let excluded = List.map (fun p -> subtree#acc ?from:None p#path) paths_from in
    DEBUG_MSG "excluded:\n%s"
      (String.concat "\n" (List.map (fun n -> n#initial_to_string) excluded));

    subtree#prune_initial_nodes excluded;

    DEBUG_MSG "copied subtree:\n%s" subtree#initial_to_string;

    let finally_deleted_nodes = Xset.create 0 in
    begin
      try
        let compo_tbl = Hashtbl.create 0 in
        let specs = Hashtbl.find_all sub_del_spec_tbl mid in
        let specs_ =
          List.map
            (fun (p, pl) ->
              DEBUG_MSG "sub_del_spec found: (%s,[%s])"
                p#to_string (String.concat ";" (List.map (fun x -> x#to_string) pl));
              let rt = subtree#acc ?from:None p#path in
              let pnd = rt#initial_parent in
              let pos = rt#initial_pos in
              let nds = List.map (fun x -> subtree#acc ?from:(Some rt) x#path) pl in
              DEBUG_MSG "rt=%a pnd=%a pos=%d nds=[%a]" nps rt nps pnd pos nsps nds;
              Hashtbl.add compo_tbl rt (pos, nds);
              (rt, pnd, pos, nds)
            ) specs
        in
        let specs_ =
          List.fast_sort (fun (n0, _, _, _) (n1, _, _, _) -> compare n1#gindex n0#gindex) specs_
        in
        let used = Xset.create 0 in
        let rec trace n =
          try
            let pos, nds = Hashtbl.find compo_tbl n in
            Xset.add used n;
            List.flatten (List.map trace nds)
          with
            Not_found -> [n]
        in
        List.iter
          (fun (rt, pnd, pos, nds) ->
            if not (Xset.mem used rt) then begin
              let nds' = List.flatten (List.map trace nds) in
              DEBUG_MSG "rt=%a pnd=%a pos=%d nds'=[%a]" nps rt nps pnd pos nsps nds';
              if nds' <> [] then begin
                scan_initial_cluster rt nds' (fun x -> Xset.add finally_deleted_nodes x#uid)
              end;
              self#prune_cluster pnd pos nds'
            end
          ) specs_
      with
        Not_found -> ()
    end;

    Hashtbl.add copied_subtree_tbl mid subtree;
    Hashtbl.add copied_subtree_sz_tbl mid (subtree#size_of_initial_cluster (subtree#root, []));
    Hashtbl.add path_tbl subtree#root path_to;
    begin
      match key_opt with
      | Some k -> Hashtbl.add path_key_tbl subtree#root k
      | None -> ()
    end;
    begin
      match depth_opt with
      | Some d -> Hashtbl.add path_depth_tbl subtree#root d
      | None -> ()
    end;

    if path_to#key_opt = Some K_stable then begin
      self#reg_root_of_upstream_staying_move nd;
      self#reg_root_of_upstream_staying_move subtree#root;
    end;

    let key = key_of_mid mid in
    subtree#scan_whole_initial
      (fun n ->
        DEBUG_MSG "%a -> %s" nps n (key_to_string key);
        if Xset.mem finally_deleted_nodes n#uid then
          DEBUG_MSG "canceled since %a will be deleted at last" nps n
        else
          Hashtbl.add key_tbl n key
      );
(*
    let keys = Xlist.filter_map (fun p -> p#key_opt) paths_to in
    DEBUG_MSG "keys=[%s]" (String.concat "," (List.map key_to_string keys));
    begin
      match keys with
      | [] -> ()
      | [k] -> self#mark_key k
      | k::rest ->
          if List.for_all (fun x -> x = k) rest then
            self#mark_key k
    end;
*)
    List.iter
      (fun p ->
        DEBUG_MSG "p=%s" p#to_string;
        match p#key_opt with
        | Some k ->
            self#mark_key k;
            DEBUG_MSG "marked: %s" (key_to_string k);
            self#link_keys key k
        | _ -> ()
      ) paths_to;

    self#reg_boundary key paths_to

  method has_p_descendant ?(moveon=fun _ -> true) pred n =
    let rec scan n =
      (*DEBUG_MSG "n=%a" nps n;*)
      if moveon n then
        if pred n then
          raise Exit
        else
          Array.iter scan n#initial_children
    in
    try
      scan n;
      false
    with
      Exit -> true

  method has_stable_descendant = self#has_p_descendant self#is_stable

  method private skip_deleted ?(limit=tree#root) nd =
    DEBUG_MSG "limit=%a nd=%a" nps limit nps nd;
    let rec skip nd =
      if nd == limit then
        failwith "skip_deleted";
      let pnd = nd#initial_parent in
      if pnd == limit then
        failwith "skip_deleted"
      else if self#is_deleted pnd then
        skip pnd
      else
        pnd, nd#initial_pos
    in
    let (n, pos) as res = skip nd in
    DEBUG_MSG "-> %a pos=%d" nps n pos;
    res

  method private _check_key key =
    DEBUG_MSG "key=%s" (key_to_string key);
    try
      let sr = (self#find_subtree key)#root in
      DEBUG_MSG "sr=%a" nps sr;
      self#is_forced_upstream sr ||
      match self#find_key_opt sr#initial_parent with
      | Some k -> self#_check_key k
      | None -> begin
          let a, _ = self#skip_deleted sr#initial_parent in
          DEBUG_MSG "a=%a" nps a;
          self#is_stable a
      end
    with
      _ -> false

  method is_true_stable_node ?(weak=false) n =
    self#is_stable n &&
    if weak then
      match self#find_parent_key_opt n with
      | None -> true
      | Some K_stable -> DEBUG_MSG "!!!!! n=%a" nps n; true
      | Some (K_mid _ | K_stid _ as k) when self#_check_key k -> DEBUG_MSG "!!!!! n=%a" nps n; true
      | _ -> false
    else
      not (self#has_parent_key n)

  method has_true_stable_descendant ?(weak=false) =
    let moveon x =
      not (self#is_forced_upstream x && not (Hashtbl.mem upstream_dest_tbl x)) &&
      (not (self#is_stable x) ||
      if weak then
        match self#find_parent_key_opt x with
        | None -> true
        | Some K_stable -> DEBUG_MSG "!!!!! x=%a" nps x; true
        | Some (K_mid _ | K_stid _ as k) when self#_check_key k -> DEBUG_MSG "!!!!! x=%a" nps x; true
        | _ -> false
      else
        not (self#has_parent_key x))
    in
    self#has_p_descendant ~moveon (self#is_true_stable_node ~weak)

  method has_stable_or_inserted_descendant =
    self#has_p_descendant (fun x -> self#is_stable x || self#is_insert x)

  method make_frontier_tbl() =
    Hashtbl.iter
      (fun mid (subtree, nd, paths_from) ->
        DEBUG_MSG "mid=%a" MID.ps mid;

        let excluded_nds =
          List.map (fun p -> self#acc (Path.concat nd#apath p#path)) paths_from
        in
        let _is_excluded n = List.memq n excluded_nds in
        let is_excluded lv n = List.memq n excluded_nds in

        let pos_cache = Hashtbl.create 0 in

        let get_adj_path =
          let f _ = raise Not_found in
          get_adjusted_path f f f pos_cache (fun _ -> None) _is_excluded is_excluded self#is_stable nd
        in

        let tbl =
          List.fold_left
            (fun l path ->
              let nx = self#acc (Path.concat nd#apath path#path) in
              DEBUG_MSG "path=%s" path#to_string;

              if self#has_stable_descendant nx then begin
                DEBUG_MSG "has stable descendant: %s" nx#initial_to_string;

                let ap = get_adj_path path#path in

                DEBUG_MSG "adjusted path=%s" (Path.to_string ap);

                let pos = Path.get_position ap in
                let n = subtree#acc ?from:None path#path in
                (n#initial_parent, pos)::l
              end
              else
                l

            ) [] (List.filter (fun p -> p#upstream = 0) paths_from)
        in
        Hashtbl.add frontier_tbl mid (List.rev tbl)

      ) excluded_paths_tbl



  method insert_cluster(* ?(no_trans=false)*) ?(partial=false) nd pos ofs sr l =
    DEBUG_MSG "%a partial=%B pos=%d ofs=%f subroot=%a [%s]"
      nps nd partial pos ofs nps sr
      (Xlist.to_string
         (fun (n, e) -> sprintf "<%a,%s>" nps n (Elem.to_string e)) ";" l);

    if ofs = 0. then begin
      List.iteri
        (fun i (n, e) ->
          if Elem.has_frac_ofs e then begin
            let n = nd#initial_children.(pos+i) in
            Xset.add canceled_dels n
          end
        ) l
    end;
    let m = Otree.CMinsert(partial, pos, ofs, sr, l) in
    (*if no_trans then
      Xset.add no_trans_mutations m;*)
    tbl_add op_tbl nd#uid m

  method _prune_cluster nd pos nds =
    DEBUG_MSG "nd=%a pos=%d nds=[%a]" nps nd pos nsps nds;
    tbl_add op_tbl nd#uid (Otree.CMprune(pos, nds))

  method prune_cluster nd pos nds =
    DEBUG_MSG "%a pos=%d nds=[%a]" nps nd pos nsps nds;

    (*if nds = [] then begin
      DEBUG_MSG "pruned immediately";
      Xset.add immediately_pruned_nodes nd#initial_children.(pos);
      self#_prune_cluster nd pos []
    end
    else *)begin
      DEBUG_MSG "deferred";
      let fnodes = get_frontier_nodes nds in
      DEBUG_MSG "fnodes=[%a]" nsps fnodes;
      let pl = (* frontier node * member node list *)
        List.map
          (fun n ->
            let ns =
              List.filter
                (fun n -> not (List.memq n nds))
                (Array.to_list n#initial_children)
            in
            DEBUG_MSG "n=%a ns=[%a]" nps n nsps ns;
            n, ns
          ) fnodes
      in
      let rt = nd#initial_children.(pos) in
      DEBUG_MSG "subtree_root: %a -> frontier: %s" nps rt
        (Xlist.to_string (fun (n, ns) -> sprintf "%a[%a]" nps n nsps ns) ";" pl);

      Hashtbl.add del_tbl rt pl
    end

  method acc p =
    try
      tree#initial_acc ?from:None p
    with
      Invalid_argument _ ->
        failwith "Delta.interpreter#acc: invalid tree path access"

  method acc_from r p =
    try
      tree#initial_acc ?from:(Some r) p
    with
      Invalid_argument _ ->
        failwith "Delta.interpreter#acc: invalid tree path access"

  method acc_parent ?(ignore_ofs=false) p =
    try
      tree#initial_acc_parent ?ignore_ofs:(Some ignore_ofs) ?from:None p
    with
      Invalid_argument _ ->
        failwith "Delta.interpreter#acc_parent: invalid tree path access"

  method boundary_to_nds_from ?(junc=false) rt (paths : boundary) =
    List.map
      (fun bpath ->
        let nd = self#acc_from rt bpath#path in
        if bpath#upstream > 0 then begin
          match bpath#key_opt with
          | Some _ -> self#reg_quasi_upstream_node nd bpath#upstream
          | _ -> self#reg_upstream_node nd bpath#upstream
        end;
        if junc && bpath#upstream < 2 then begin
          DEBUG_MSG "junc_node: %a" nps nd;
          Xset.add junc_nodes nd
        end;
        nd
      ) paths

  method boundary_to_nds ?(junc=false) path (paths : boundary) =
    List.map
      (fun bpath ->
        let nd = self#acc (Path.concat path#path bpath#path) in
        if bpath#upstream > 0 then begin
          match bpath#key_opt with
          | Some _ -> self#reg_quasi_upstream_node nd bpath#upstream
          | _ -> self#reg_upstream_node nd bpath#upstream
        end;
        if junc && bpath#upstream < 2 then begin
          DEBUG_MSG "junc_node: %a" nps nd;
          Xset.add junc_nodes nd
        end;
        nd
      ) paths


  method insert_into_subtree ?(adj=0) ?(shift=0) (path : path_c) depth parent_tree subtree nes =
    let head_path = Path.head path#path (-depth) in
    DEBUG_MSG "head_path=%s" (Path.to_string head_path);

    let rel_path = get_rel_path head_path path#path in
    DEBUG_MSG "rel_path=%s" (Path.to_string rel_path);

    let a =
      parent_tree#initial_acc_parent ?ignore_ofs:(Some true) ?from:None rel_path
    in
    let pnd, elem = a.Otree.node, a.Otree.elem in
    let pos, ofs = elem.Elem.pos, elem.Elem.ofs in

    DEBUG_MSG "-> pos=%d ofs=%s adj=%d" pos (Elem.ofs_to_str ofs) adj;

    if pos = 0 && ofs = 0.0 && adj = 0 && shift > 0 then begin
      self#reg_pos_shift subtree#root shift
    end;

    let pos' =
      let pa = pos - adj in
      if pa < 0 then
        0
      else
        pa
    in
    self#insert_cluster pnd pos' ofs subtree#root nes



  method interpret_delete (path : path_c) (paths : boundary) =
    let nd = self#acc path#path in
    DEBUG_MSG "nd=%s" nd#initial_to_string;
    DEBUG_MSG "root=%a" nps nd;
    let nds =
      List.map
        (fun p ->
          let p' = Path.concat path#path p#path in
          DEBUG_MSG "p'=%s" (Path.to_string p');

          let n = self#acc p' in

          if p#upstream > 0 then
            self#reg_quasi_upstream_node n p#upstream;

          n
        ) paths
    in

    DEBUG_MSG "nds:%s"
      (if nds = [] then
        ""
      else
        "\n"^(String.concat "\n"
                (List.map (fun n -> n#initial_to_string) nds))^"\n");

    self#prune_cluster nd#initial_parent nd#initial_pos nds


  method shift_positions () =
    let parents = Xset.create 0 in
    Hashtbl.iter
      (fun nd _ ->
        Xset.add parents nd#initial_parent
      ) pos_shift_tbl;
    Xset.iter
      (fun pnd ->
        DEBUG_MSG "pnd=%a" nps pnd;
        let nchildren = Array.length pnd#initial_children in
        let tbl = Hashtbl.create 0 in
        let l = ref [] in
        Array.iteri
          (fun i c ->
            DEBUG_MSG "%d: %a" i nps c;
            if Hashtbl.mem pos_shift_tbl c then begin
              let s = (Hashtbl.find pos_shift_tbl c) in
              DEBUG_MSG "  %a: %d" nps c s;
              Hashtbl.add tbl c s
            end
            else begin
              Hashtbl.iter
                (fun x s ->
                  if s <= 1 then begin
                    DEBUG_MSG "  %a added" nps x;
                    l := x :: !l;
                    Hashtbl.remove tbl x
                  end
                  else begin
                    DEBUG_MSG "  %a: %d -> %d" nps x s (s-1);
                    Hashtbl.replace tbl x (s-1)
                  end
                ) tbl;
              DEBUG_MSG "  %a added" nps c;
              l := c :: !l
            end;

          ) pnd#initial_children;

        DEBUG_MSG "[%a]" nsps (List.rev !l);

        let children = Array.of_list (List.rev !l) in
        if Array.length children = nchildren then
          pnd#set_initial_children children

      ) parents


  method interpret_insert key (subtree : 'tree)
                          path (paths : boundary)
                          key_opt adj_opt depth_opt shift_opt
      =
    DEBUG_MSG "key=%s path=%s paths=%s key_opt=%s adj_opt=%s depth_opt=%s shift_opt=%s"
      (key_to_string key) path#to_string (boundary_to_string paths)
      (key_opt_to_string key_opt) (int_opt_to_string adj_opt)
      (int_opt_to_string depth_opt) (int_opt_to_string shift_opt);

    if path#upstream > 0(* || path#key_opt = Some K_stable*) then begin
      self#reg_upstream_node subtree#root path#upstream;
      self#force_upstream ~key_opt:path#key_opt subtree#root
    end;

    let acc = self#acc_parent path#path in
    let pnd, elem = acc.Otree.node, acc.Otree.elem in
    let pos, ofs = elem.Elem.pos, elem.Elem.ofs in

    DEBUG_MSG "pnd=%a pos=%d ofs=%s" nps pnd pos (Elem.ofs_to_str ofs);

    let masked = self#get_mask key pos in
    DEBUG_MSG "key=%s pos=%d masked=[%s]" (key_to_string key) pos (int_set_to_string masked);

    let npaths = List.length paths in

    let prev = ref [] in
    let key_tbl = Hashtbl.create 0 in

    let used_keys = ref [] in

    let nes =
      Xlist.filter_map
        (fun (i, path0) ->
          DEBUG_MSG "i=%d path0=%s" i path0#to_string;
          begin
            match path0#key_opt with
            | Some k -> self#add_composition key k
            | _ -> ()
          end;
	  let a0 =
            subtree#initial_acc_parent ?ignore_ofs:(Some false) ?from:None path0#path
          in
          let pnd0, elem0 = a0.Otree.node, a0.Otree.elem in
          let ne = pnd0, elem0 in

          let res =
            if Xset.mem masked i || Elem.has_frac_ofs (Path.tail path0#path) then begin
              Some (pnd0, Elem.make (-1))
            end
            else begin
              match path0#key_opt with
              | Some k when not (List.mem k !used_keys) -> begin
                  used_keys := k :: !used_keys;
                  try
                    let st = self#find_subtree k in
                    DEBUG_MSG "k=%s st:\n%s"
                      (key_to_string k) st#initial_to_string;

                    if i = npaths - 1 && ofs = 0. && i > 0 && not (Hashtbl.mem key_tbl k) then begin
                      match !prev with
                      | p0::p1::[] ->
                          let elem1 = (Path.tail p1#path) in
                          if
                            Elem.has_frac_ofs (Path.tail p0#path) &&
                            not (Elem.has_frac_ofs elem1) &&
                            elem1.Elem.ofs = 0.
                          then begin
                            let s0 = Xset.create 0 in
                            for i0 = 0 to i - 1 do
                              Xset.add s0 i0
                            done;
                            self#set_mask k pos s0
                          end
                      | _ -> ()
                    end;

                    let boundary = self#get_boundary k in
                    DEBUG_MSG "boundary=[%s]" (boundary_to_string boundary);
                    for j = 1 to (List.length boundary) - 1 do
                      DEBUG_MSG "masking %d" (i + j);
                      Xset.add masked (i + j);
                    done;

                    let pos0, ofs0 = elem0.Elem.pos, elem0.Elem.ofs in
                    if
                      try Hashtbl.find key_tbl k <> (pnd0, pos0, ofs0) with _ -> true
                    then
                      self#insert_cluster pnd0 pos0 ofs0 st#root [];

                    Hashtbl.replace key_tbl k (pnd0, pos0, ofs0);
                    Some (pnd0, Elem.make (-1))
                  with
                    Not_found -> Some ne
              end
              | Some _ -> Some ne
              | None -> Some ne
            end
          in
          begin
            match !prev with
            | x::_ when Elem.has_frac_ofs (Path.tail x#path) -> ()
            | _ -> prev := path0 :: !prev
          end;
          res
        ) (List.mapi (fun i p -> (i, p)) paths)
    in (* nes *)
    DEBUG_MSG "nes=[%s]"
      (Xlist.to_string
         (fun (n, e) -> sprintf "<%a,%s>" nps n (Elem.to_string e)) ";" nes);

    let default() =
      let pos', ofs' =
        match shift_opt with
        | Some s -> pos + s, -1.
        | None -> pos, ofs
      in
      DEBUG_MSG "-> pos'=%d ofs'=%s" pos' (Elem.ofs_to_str ofs');

      let partial = self#is_marked_key key in

      self#insert_cluster ~partial pnd pos' ofs' subtree#root nes
    in (* default *)

    begin
      match key_opt, depth_opt with
      | Some key, Some depth -> begin
          try
            let parent_tree = self#find_subtree key in

	    DEBUG_MSG "parent tree (key=%s):\n%s"
              (key_to_string key) parent_tree#initial_to_string;

            let adj =
              match adj_opt with
              | Some a -> a
              | None -> 0
            in
            DEBUG_MSG "-> adj=%d" adj;

            let shift =
              match shift_opt with
              | Some s -> s
              | None -> 0
            in
            DEBUG_MSG "-> shift=%d" shift;

            self#insert_into_subtree ~adj ~shift path depth parent_tree subtree nes

          with
            Not_found -> default()
      end
      | None, None -> default()
      | _ ->
          let ed_str =
	    sprintf "Dinsert(path:%s,paths:%s,key_opt:%s,adj_opt:%s,depth_opt:%s,shift_opt:%s\n%s)"
	      path#to_string (boundary_to_string paths)
              (key_opt_to_string key_opt) (int_opt_to_string adj_opt)
              (int_opt_to_string depth_opt) (int_opt_to_string shift_opt)
              subtree#initial_to_string
          in
          Xprint.error "invalid delta: %s" ed_str

    end


  method interpret_move ?(mctl=Mfull)
      mid
      (path_from : path_c) paths_from path_to paths_to
      key_opt adj_opt depth_opt shift_opt
      =
    DEBUG_MSG "mctl=%s mid=%a key_opt=%s adj_opt=%s depth_opt=%s"
      (move_control_to_string mctl) MID.ps mid
      (key_opt_to_string key_opt) (int_opt_to_string adj_opt)
      (int_opt_to_string depth_opt);

    try
      let nd = self#acc path_from#path in
      DEBUG_MSG "nd=%s" nd#initial_to_string;
      let junc = self#is_staying_move mid in
      let nds = self#boundary_to_nds ~junc path_from paths_from in

      DEBUG_MSG "copy: excluded: [%s]" (nodes_to_uids_string nds);

      if mctl <> MinsertOnly then
        self#prune_cluster nd#initial_parent nd#initial_pos nds;

      if mctl <> MdeleteOnly then
        let subtree = Hashtbl.find copied_subtree_tbl mid in
        self#interpret_insert
          (key_of_mid mid) subtree path_to paths_to
          key_opt adj_opt depth_opt shift_opt
    with
      Not_found ->
        failwith "Delta.interpreter#interpret_move"


  method interpret_change ?(mctl=Mfull) (path : path_c) paths (subtree : 'tree) =
    let apply nd =
      DEBUG_MSG "nd=%s" nd#initial_to_string;
      let q = Queue.create() in
      subtree#preorder_scan_all
        (fun n ->
	  DEBUG_MSG "     queueing %s" n#initial_to_string;
	  Queue.add n#data q
        );
      (*let nds = self#boundary_to_nds path paths in*)
      let nds =
        try
          self#boundary_to_nds_from nd paths
        with
          _ ->
            DEBUG_MSG "boundary_to_nds_from: failed";
            self#boundary_to_nds path paths
      in
      BEGIN_DEBUG
        List.iteri (fun i n -> DEBUG_MSG "%d: %s" i n#initial_to_string) nds;
      END_DEBUG;
      scan_initial_cluster nd nds
        (fun n ->
	  DEBUG_MSG "     n -> %s" n#initial_to_string;
	  try
	    let d = Queue.take q in
	    n#set_data d
	  with
            Queue.Empty ->
              let ed_str =
	        sprintf "Dchange(path:%s,%s,\n%s)"
	          path#to_string (boundary_to_string paths)
                  subtree#initial_to_string
              in
	      Xprint.error
                "invalid delta: inconsistent change: %s" ed_str
        )
    in
    let nd = self#acc path#path in
    DEBUG_MSG "nd=%s" nd#initial_to_string;
    match mctl with
    | MdeleteOnly -> self#add_deferred_relabel (fun () -> apply nd)
    | MinsertOnly -> self#add_move_relabel nd apply
    | Mfull -> apply nd

  method interpret_change_attr ?(mctl=Mfull) (path : path_c) attr v =
    let apply nd = nd#data#change_attr attr v in
    let nd = self#acc path#path in
    DEBUG_MSG "nd=%s" nd#initial_to_string;
    match mctl with
    | MdeleteOnly -> self#add_deferred_relabel (fun () -> apply nd)
    | MinsertOnly -> self#add_move_relabel nd apply
    | Mfull -> apply nd

  method interpret_delete_attr ?(mctl=Mfull) (path : path_c) attr =
    let apply nd = nd#data#delete_attr attr in
    let nd = self#acc path#path in
    DEBUG_MSG "nd=%s" nd#initial_to_string;
    match mctl with
    | MdeleteOnly -> self#add_deferred_relabel (fun () -> apply nd)
    | MinsertOnly -> self#add_move_relabel nd apply
    | Mfull -> apply nd

  method interpret_insert_attr ?(mctl=Mfull) (path : path_c) attr v =
    let apply nd = nd#data#insert_attr attr v in
    let nd = self#acc path#path in
    DEBUG_MSG "nd=%s" nd#initial_to_string;
    match mctl with
    | MdeleteOnly -> self#add_deferred_relabel (fun () -> apply nd)
    | MinsertOnly -> self#add_move_relabel nd apply
    | Mfull -> apply nd


end (* of class Delta.interpreter *)
