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
(* edit_base.ml *)


open Stat.File

module GI  = Otreediff.GIndex
module Otree = Otreediff.Otree

let sprintf = Printf.sprintf
let fprintf = Printf.fprintf

let del_bg = "#B8E3B1"
let del_fg = "#000000"
let ins_bg = "#B2CDF9"
let ins_fg = "#000000"
let mov_bg = "#E6DFCF"
let mov_fg = "#000000"
let rel_bg = "#FFC0A9"
let rel_fg = "#000000"
let movrel_bg = "#FFDFCF"
let movrel_fg = "#000000"

type line_match = 
  | LM_single of (int * int)      (* (l1, l2) *)
  | LM_multi of (int * int * int) (* (l1, l2, ext) *)

let get_label nd =
  let lab = nd#data#label in
  if nd#data#is_named then 
    lab
  else
    let extra =
      Array.fold_left 
	(fun labs cnd -> 
	  if cnd#data#is_named then cnd#data#label :: labs
	  else labs
	) [] nd#children
    in
    if extra = [] then lab
    else lab^"<"^(String.concat ";" extra)^">"


let _same_digest tree1 tree2 n1 n2 =
  let d1 =
    match n1#data#_digest with
    | Some d -> d
    | None -> tree1#get_digest n1
  in
  let d2 =
    match n2#data#_digest with
    | Some d -> d
    | None -> tree2#get_digest n2
  in
  d1 = d2

let tbl_add tbl key v =
  try
    let l = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (v::l)
  with
    Not_found -> Hashtbl.add tbl key [v]

(* segments *)

type segment = int * int

let sort_segments (segs : segment list) =
  List.fast_sort (fun (s1, _) (s2, _) -> compare s1 s2) segs

let segments_to_string : segment list -> string = 
  Xlist.to_string (fun (s, e) -> sprintf "%d %d" s e) " "

let merge_segments segs_list =

  BEGIN_DEBUG
    DEBUG_MSG "segs list";
    List.iter (Printf.printf "\t%s\n") (List.map segments_to_string segs_list)
  END_DEBUG;

  let all_segs = List.concat segs_list in
  let phantoms, normals = List.partition (fun (s, e) -> s > e) all_segs in

  let extra =
    List.fold_left 
      (fun l (e, s) -> 
	if List.mem (s, e) normals then
	  l
	else
	  (s, s)::l
      ) [] phantoms
  in

  BEGIN_DEBUG
    DEBUG_MSG "normals: %s" (segments_to_string normals);
    DEBUG_MSG "phantoms: %s" (segments_to_string phantoms);
    DEBUG_MSG "extra: %s" (segments_to_string extra)
  END_DEBUG;

  let sorted = sort_segments (normals @ extra) in

  let l, last =
    List.fold_left
      (fun (a, cand) (s, e) -> 
	match cand with
	| None -> a, Some (s, e)
	| Some (s0, e0) -> 
	    if s = e0 + 1 then
	      a, Some (s0, e)

	    else if s > e0 + 1 then
	      (s0, e0)::a, Some (s, e)

	    else if s0 <= s && e <= e0 then
		a, Some (s0, e0)

	    else if s0 <= s && s <= e0 && e > e0 then begin
	      WARN_MSG "overlap: cand=(%d,%d) elem=(%d,%d)" s0 e0 s e;
	      a, Some (s0, e)
	    end

	    else begin
	      FATAL_MSG	"cand=(%d,%d) elem=(%d,%d)" s0 e0 s e;
	      exit 1
	    end
		
      ) ([], None) sorted
  in
  let res = 
    match last with 
    | None -> List.rev l
    | Some c -> List.rev (c::l)
  in

  DEBUG_MSG "result: %s" (segments_to_string res);

  res


(* edit operations *)
include Editop

let get_mid = function
  | Move(mid, _, _, _) -> !mid
  | _ -> assert false

let add tbl u e =
  try
    let e' = Hashtbl.find tbl u in
    if e <> e' then 
      WARN_MSG "already have %a -> %s (not added: %s)" UID.ps u
	(to_string e') (to_string e)
  with 
    Not_found -> Hashtbl.add tbl u e

let add2 tbl1 tbl2 u1 u2 e =
  let is_ok tbl u =
    try
      let e' = Hashtbl.find tbl u in
      let a = 
	if e <> e' then
	  "conflicting "
	else
	  ""
      in
      WARN_MSG "already have %s%a -> %s (not added: %s)" a UID.ps u
	(to_string e') (to_string e);
      false
    with 
      Not_found -> true
  in
  let ok1 = is_ok tbl1 u1 in
  let ok2 = is_ok tbl2 u2 in

  if ok1 && ok2 then begin
    Hashtbl.add tbl1 u1 e;
    Hashtbl.add tbl2 u2 e
  end


let gid_of_edit1 = function
  | Delete(_, _, info, _)
  | Relabel(_, (_, info, _), _)
  | Move(_, _, (_, info, _), _) -> Info.get_gid info
  | Insert(_, _, info, _) -> raise Not_found

let gid_of_edit2 = function
  | Insert(_, _, info, _)
  | Relabel(_, _, (_, info, _))
  | Move(_, _, _, (_, info, _)) -> Info.get_gid info
  | Delete(_, _, info, _) -> raise Not_found


let _sort_edit_list_topdown gid_of_edit =
  List.fast_sort 
    (fun ed1 ed2 ->
      let c = 
        try
          let gi1, gi2 = gid_of_edit ed1, gid_of_edit ed2 in
          Stdlib.compare gi2 gi1
        with
          Not_found -> 0
      in
      let c' = 
        if c = 0 then
          match ed1, ed2 with
          | Move _, Relabel _ -> -1
          | Relabel _, Move _ -> 1
          | _ -> 0
        else
          c
      in
      (* DEBUG_MSG "%s vs %s --> %d" (to_string ed1) (to_string ed2) c'; *)
      c'
    )

let sort_edit_list_topdown l =
  let l' = _sort_edit_list_topdown gid_of_edit1 l in
  let l'' = _sort_edit_list_topdown gid_of_edit2 l' in
  l''

let _sort_edit_list_bottomup gid_of_edit =
  List.fast_sort 
    (fun ed1 ed2 ->
      let c = 
        try
          let gi1, gi2 = gid_of_edit1 ed1, gid_of_edit1 ed2 in
          Stdlib.compare gi1 gi2
        with
          Not_found -> 0
      in
      if c = 0 then
        match ed1, ed2 with
        | Relabel _, Move _ -> -1
        | Move _, Relabel _ -> 1
        | _ -> 0
      else
        c
    )

let sort_edit_list_bottomup l =
  let l' = _sort_edit_list_bottomup gid_of_edit1 l in
  let l'' = _sort_edit_list_bottomup gid_of_edit2 l' in
  l''

let tbl_remove tbl k =
  (*while Hashtbl.mem tbl k do*)
    Hashtbl.remove tbl k
  (*done*)

(* base class for edit sequences *)

exception Found
exception Abort

let is_ghost_node nd = nd#data#src_loc = Loc.ghost


class formatters_base ~delete ~insert ~relabel ~move ~align = 
  object
    method delete  = (delete : int -> int -> segment list -> string)
    method insert  = (insert : int -> int -> segment list -> string)
    method relabel = (relabel : bool -> int -> int -> segment list -> int -> int -> segment list -> string)
    method move    = (move : int -> int -> segment list -> int -> int -> segment list -> string)
    method align   = (align : line_match -> string)
  end


type move_region_tbl_t = (MID.t, (GI.t*GI.t*GI.t*GI.t)) Hashtbl.t

class ['node_t, 'tree_t] seq_base options = object (self : 'edits)

  val mutable del_tbl  = Hashtbl.create 0
  val mutable ins_tbl  = Hashtbl.create 0
  val mutable rel1_tbl = Hashtbl.create 0
  val mutable rel2_tbl = Hashtbl.create 0
  val mutable mov1_tbl = Hashtbl.create 0
  val mutable mov2_tbl = Hashtbl.create 0

  val mutable tables = []

  val mov_gr_tbl = Hashtbl.create 0 (* move id -> group id *)
  val mov_gr_mem_tbl = Hashtbl.create 0 (* move id -> move list *)

  val mutable list = ([] : ('node_t t) list)

  method _init =
      tables <- [del_tbl; ins_tbl; rel1_tbl; rel2_tbl; mov1_tbl; mov2_tbl]

  initializer
    self#_init

  method find_mov_gr = Hashtbl.find mov_gr_tbl
  method find_mov_gr_mems = Hashtbl.find mov_gr_mem_tbl

  method _del_tbl  = del_tbl
  method _ins_tbl  = ins_tbl
  method _rel1_tbl = rel1_tbl
  method _rel2_tbl = rel2_tbl
  method _mov1_tbl = mov1_tbl
  method _mov2_tbl = mov2_tbl

  method content = list

  method _set_del_tbl tbl  = del_tbl <- tbl
  method _set_ins_tbl tbl  = ins_tbl <- tbl
  method _set_rel1_tbl tbl = rel1_tbl <- tbl
  method _set_rel2_tbl tbl = rel2_tbl <- tbl
  method _set_mov1_tbl tbl = mov1_tbl <- tbl
  method _set_mov2_tbl tbl = mov2_tbl <- tbl

  method copy =
    let eds = new seq_base options in
    eds#_set_del_tbl (Hashtbl.copy self#_del_tbl);
    eds#_set_ins_tbl (Hashtbl.copy self#_ins_tbl);
    eds#_set_rel1_tbl (Hashtbl.copy self#_rel1_tbl);
    eds#_set_rel2_tbl (Hashtbl.copy self#_rel2_tbl);
    eds#_set_mov1_tbl (Hashtbl.copy self#_mov1_tbl);
    eds#_set_mov2_tbl (Hashtbl.copy self#_mov2_tbl);
    eds#_init;
    eds

  method private edit_tbl_to_list tbl =
    Hashtbl.fold (fun _ e l -> e::l) tbl []

  method clear = 
    list <- [];
    Hashtbl.clear del_tbl;
    Hashtbl.clear ins_tbl;
    Hashtbl.clear rel1_tbl;
    Hashtbl.clear rel2_tbl;
    Hashtbl.clear mov1_tbl;
    Hashtbl.clear mov2_tbl;
    Hashtbl.clear mov_gr_tbl;
    Hashtbl.clear mov_gr_mem_tbl;


  method is_consistent_with ed =
    let result = 
      match ed with
      | Delete(_, u, _, _) ->
	  let eds = self#find1 u in
	  begin
	    match eds with
	      [] -> true
	    | [ed'] -> 
		let b = ed = ed' in 

		BEGIN_DEBUG
		  if b then 
		    DEBUG_MSG "duplication: %s" (to_string ed)
		  else 
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed';ed''] -> 
		DEBUG_MSG "%s conflicts %s and %s" 
		  (to_string ed) (to_string ed') (to_string ed'');

		false

	    | _ -> assert false
	  end

      | Insert(_, v, _, _)  ->
	  let eds = self#find2 v in
	  begin
	    match eds with
	    | [] -> true
	    | [ed'] ->
		let b = ed = ed' in

		BEGIN_DEBUG
		  if b then 
		    DEBUG_MSG "duplication: %s" (to_string ed)
		  else 
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed';ed''] ->
		DEBUG_MSG "%s conflicts %s and %s" 
		  (to_string ed) (to_string ed') (to_string ed'');

		false

	    | _ -> assert false
	  end

      | Relabel(_, (u, _, _), (v, _, _)) -> 
	  let eds = self#find12 u v in
	  begin
	    match eds with
	    | [] -> true
	    | [Move(_, _, (u', _, _), (v', _, _)) as ed'] -> 
		let b = u = u' && v = v' in 
		
		BEGIN_DEBUG
		  if not b then
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed'] ->
		let b = ed = ed' in

		BEGIN_DEBUG
		  if b then 
		    DEBUG_MSG "duplication: %s" (to_string ed)
		  else 
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed';ed''] ->
		DEBUG_MSG "%s conflicts %s and %s" 
		  (to_string ed) (to_string ed') (to_string ed'');

		false

	    | _ -> assert false
	  end

      | Move(_, _, (u, _, _), (v, _, _)) ->
	  let eds = self#find12 u v in
	  begin
	    match eds with
	    | [] -> true
	    | [Relabel(_, (u', _, _), (v', _, _)) as ed'] ->
		let b = u = u' && v = v' in

		BEGIN_DEBUG
		  if not b then 
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed'] ->
		let b = ed = ed' in

		BEGIN_DEBUG
		  if b then 
		    DEBUG_MSG "duplication: %s" (to_string ed)
		  else 
		    DEBUG_MSG "%s conflicts %s" (to_string ed) (to_string ed')
		END_DEBUG;

		b

	    | [ed';ed''] ->
		DEBUG_MSG "%s conflicts %s and %s" 
		  (to_string ed) (to_string ed') (to_string ed'');

		false

	    | _ -> assert false
	  end
    in
    result
 (* end of method is_consistent_with *)

  method add_edit edit =

    BEGIN_DEBUG
      DEBUG_MSG "%s" (to_string edit);
      assert (self#is_consistent_with edit)
    END_DEBUG;

    match edit with
    | Delete(_, uid, _, _) -> add del_tbl uid edit
    | Insert(_, uid, _, _) -> add ins_tbl uid edit
    | Relabel(_, (uid1, _, _), (uid2, _, _))    -> add2 rel1_tbl rel2_tbl uid1 uid2 edit
    | Move(_, _, (uid1, _, _), (uid2, _, _)) -> add2 mov1_tbl mov2_tbl uid1 uid2 edit



  method add_edits (edits : 'edits) = 
(*
    BEGIN_DEBUG
      DEBUG_MSG "%d edit(s)" edits#get_nedits;
      edits#sync;
      List.iter 
	(fun ed -> 
	  DEBUG_MSG "adding: %s" (to_string ed);
	  assert (self#is_consistent_with ed)
	) edits#content
    END_DEBUG;
*)
    edits#iter self#add_edit
    


  method remove_edit ed =

    DEBUG_MSG "%s" (to_string ed);

    match ed with
    | Delete(_, u, _, _) -> (*Hashtbl.remove*)tbl_remove del_tbl u
    | Insert(_, u, _, _) -> (*Hashtbl.remove*)tbl_remove ins_tbl u
    | Relabel(_, (u1, _, _), (u2, _, _)) ->
	(*Hashtbl.remove*)tbl_remove rel1_tbl u1;
	(*Hashtbl.remove*)tbl_remove rel2_tbl u2
    | Move(_, _, (u1, _, _), (u2, _, _)) ->
	(*Hashtbl.remove*)tbl_remove mov1_tbl u1;
	(*Hashtbl.remove*)tbl_remove mov2_tbl u2


  method remove_del uid =
    DEBUG_MSG "%a" UID.ps uid;
    Hashtbl.remove del_tbl uid

  method remove_ins uid =
    DEBUG_MSG "%a" UID.ps uid;
    Hashtbl.remove ins_tbl uid

  method is_empty = self#get_nedits = 0

  method iter f = (* unordered *)
    List.iter
      (fun tbl ->
	Hashtbl.iter (fun _ ed -> f ed) tbl
      ) [mov1_tbl; rel1_tbl; del_tbl; ins_tbl]

  method iter_deletes_and_inserts f = (* unordered *)
    List.iter
      (fun tbl ->
	Hashtbl.iter (fun _ ed -> f ed) tbl
      ) [del_tbl; ins_tbl]

  method iter_deletes f =
    Hashtbl.iter (fun _ ed -> f ed) del_tbl

  method iter_inserts f =
    Hashtbl.iter (fun _ ed -> f ed) ins_tbl

  method iter_moves f =
    Hashtbl.iter (fun _ ed -> f ed) mov1_tbl

  method iter_moves_topdown f =
    let movs = self#edit_tbl_to_list mov1_tbl in
    List.iter f (sort_edit_list_topdown movs)

  method iter_moves_bottomup f =
    let movs = self#edit_tbl_to_list mov1_tbl in
    List.iter f (sort_edit_list_bottomup movs)

  method iter_relabels f =
    Hashtbl.iter (fun _ ed -> f ed) rel1_tbl


  method get_ndeletes =
    Hashtbl.length del_tbl

  method get_ninserts =
    Hashtbl.length ins_tbl

  method get_nmoves =
    let n1 = Hashtbl.length mov1_tbl in
    let n2 = Hashtbl.length mov2_tbl in
    assert (n1 = n2);
    n1

  method get_nmoves_of_move_id_filt filt move_id =
    let count = ref 0 in
    self#iter_moves
      (function
	| Move(mid, _, (_, inf1, _), (_, inf2, _)) ->
	    if !mid = move_id then
	      let n1 = Info.get_node inf1 in
	      let n2 = Info.get_node inf2 in
	      if filt n1 n2 then
		incr count
	| _ -> assert false
      );
    !count

  method get_nmoves_of_move_id move_id =
    let count = ref 0 in
    self#iter_moves
      (function
	| Move(mid, _, _, _) ->
	    if !mid = move_id then
	      incr count
	| _ -> assert false
      );
    !count

  method get_nmove_groups = (* count distinct mids *)
    let tbl = Hashtbl.create 0 in
    self#iter_moves
      (function
	| Move(mid, _, _, _) ->
	    if not (Hashtbl.mem tbl !mid) then
	      Hashtbl.add tbl !mid true
	| _ -> assert false
      );
    Hashtbl.length tbl

  method get_nrelabels =
    let n1 = Hashtbl.length rel1_tbl in
    let n2 = Hashtbl.length rel2_tbl in
(*
    if n1 <> n2 then begin
      DEBUG_MSG "size of rel1_tbl: %d" n1;
      DEBUG_MSG "size of rel2_tbl: %d" n2;

      let eds1 = ref [] in
      let eds2 = ref [] in

      Hashtbl.iter (fun uid ed -> eds1 := ed::!eds1) rel1_tbl;
      Hashtbl.iter (fun uid ed -> eds2 := ed::!eds2) rel2_tbl;

      let isect = Xlist.intersection !eds1 !eds2 in
      
      DEBUG_MSG "edits only in rel1_tbl:";
      List.iter (fun e -> DEBUG_MSG "%s" (to_string e)) (Xlist.subtract !eds1 isect);
      DEBUG_MSG "edits only in rel2_tbl:";
      List.iter (fun e -> DEBUG_MSG "%s" (to_string e)) (Xlist.subtract !eds2 isect)
    end;
*)
    assert (n1 = n2);
    n1

  method get_nedits =
    let ndel = self#get_ndeletes in
    let nins = self#get_ninserts in
    let nrel = self#get_nrelabels in
    let nmov = self#get_nmoves in
    ndel + nins + nrel + nmov

  method size = self#get_nedits

  method get_ndeleted_nodes =
    let count = ref 0 in
    self#iter_deletes
      (function
	| Delete(_, _, info, excl) ->
	    count := !count + (Info.get_size info)
	| _ -> assert false
      );
    !count

  method get_ninserted_nodes =
    let count = ref 0 in
    self#iter_inserts
      (function
	| Insert(_, _, info, excl) ->
	    count := !count + (Info.get_size info)
	| _ -> assert false
      );
    !count

  method get_nmoved_nodes ?(minsize=1) () =
    let count_tbl = Hashtbl.create 0 in (* mid -> count *)
    self#iter_moves
      (function
	| Move(mid, _, (_, inf1, excl1), (_, inf2, excl2)) ->
	    let sz = Info.get_size inf1 in
	    begin
	      try
		let c = Hashtbl.find count_tbl !mid in
		Hashtbl.replace count_tbl !mid (sz + c)
	      with
		Not_found -> Hashtbl.add count_tbl !mid sz
	    end
	| _ -> assert false
      );

    BEGIN_DEBUG
      let total = ref 0 in
      DEBUG_MSG "* size of moves:";
      let list =
	List.fast_sort (fun (m1, _) (m2, _) -> Stdlib.compare m1 m2)
	  (Hashtbl.fold (fun mid sz l -> (mid, sz)::l) count_tbl [])
      in
      List.iter
	(fun (mid, sz) ->
          total := !total + sz;
	  DEBUG_MSG "mid:%a --> %d node%s" MID.ps mid sz (if sz = 1 then "" else "s")
	) list;
      DEBUG_MSG "total=%d" !total;
    END_DEBUG;

    let total =
      Hashtbl.fold
        (fun mid c count ->
	  if c >= minsize then
	    c + count
	  else
	    count
        ) count_tbl 0
    in
    DEBUG_MSG "total=%d (minsize=%d)" total minsize;
    total

  method get_nrelabeled_nodes =
    let count = ref 0 in
    self#iter_relabels
      (function
	| Relabel(_, (_, inf1, excl1), (_, inf2, excl2)) ->
	    let sz = Info.get_size inf1 in
	    count := !count + sz
	| _ -> assert false
      );
    !count


  method get_nedited_nodes =
    let ndel = self#get_ndeleted_nodes in
    let nins = self#get_ninserted_nodes in
    let nrel = self#get_nrelabeled_nodes in
    let nmov = self#get_nmoved_nodes () in
    ndel + nins + nrel + nmov


  method get_nmoved_and_relabeled_nodes 
      ?(minsize=1)
      tree1
      uidmapping
      =
    let count_tbl = Hashtbl.create 0 in (* mid -> count *)
    self#iter_moves
      (function
	| Move(mid, _, (u1, inf1, excl1), (_, _, _)) ->
	    tree1#scan_initial_cluster 
	      (Info.get_node inf1, List.map Info.get_node !excl1) 
	      (fun n -> 
		try
		  let u' = uidmapping#find n#uid in
		  if self#mem_rel12 n#uid u' then begin
		    try
		      let c = Hashtbl.find count_tbl !mid in
		      Hashtbl.replace count_tbl !mid (c + 1)
		    with
		      Not_found -> Hashtbl.add count_tbl !mid 1
		  end
		with 
		  Not_found -> assert false
	      )

	| _ -> assert false
      );
    Hashtbl.fold
      (fun mid c count ->
	if c >= minsize then
	  c + count
	else
	  count
      ) count_tbl 0



  method get_moved_nodes tree1 = (* valid after final fixup *)
(*
    let count_tbl = Hashtbl.create 0 in
    let add_count mid x =
      try
        let c = Hashtbl.find count_tbl mid in
        Hashtbl.replace count_tbl mid (c + x)
      with
        Not_found -> Hashtbl.add count_tbl mid x
    in
*)
    let nds = Xset.create 0 in
    self#iter_moves
      (function
	| Move(mid, _, (u1, inf1, excl1), (_, _, _)) ->
	    tree1#scan_initial_cluster
	      (Info.get_node inf1, List.map Info.get_node !excl1)
	      (fun n -> Xset.add nds n)
	| _ -> assert false
      );
    (*Xset.to_list *)nds

  method _get_moved_nodes_of_move_id move_id = (* valid before final fixup *)
    let nds = Xset.create 0 in
    self#iter_moves
      (function
	| Move(mid, _, (_, inf1, _), (_, _, _)) ->
	    if !mid = move_id then
	      let n = Info.get_node inf1 in
              Xset.add nds n
	| _ -> assert false
      );
    (*Xset.to_list *)nds

  method private setup_multi_mov_gr_tbl uidmapping is_anon =

    if false then begin (* eliminate single move of anonymous entity *)

      let scan_initial_cluster nd nds (f : 'node_t -> unit) =
        let rec scan n =
          f n;
          Array.iter
            (fun c ->
	      if List.memq c nds then () else scan c
            ) n#initial_children
        in
        scan nd
      in
      self#iter_moves
        (function
	  | Move(mid, _, (u1, inf1, excl1), (u2, inf2, excl2)) as mov -> begin
              let count = ref 0 in
              let rt1 = Info.get_node inf1 in
              let rt2 = Info.get_node inf2 in
              scan_initial_cluster rt1 (List.map Info.get_node !excl1)
	        (fun n -> incr count);

              DEBUG_MSG "%a(count=%d): %s -> %s" MID.ps !mid !count rt1#data#label rt2#data#label;
              if !count = 1 && (is_anon rt1 || is_anon rt2) then begin
                DEBUG_MSG "single move of anonymous node: %s" (to_string mov);
                uidmapping#remove u1 u2;
		self#remove_edit mov;
                begin
                  try
                    match self#find_rel12 u1 u2 with
		    | Relabel _ as rel ->
		        self#remove_edit rel
                    | _ -> assert false
                  with
                    Not_found -> ()
                end;
		self#add_edit (Delete(false, u1, inf1, excl1));
		self#add_edit (Insert(false, u2, inf2, excl2));
              end
	  end
	  | _ -> assert false
        )
    end;

    let mid_gen = options#moveid_generator in

    let tbl = Hashtbl.create 0 in (* mid -> move list *)

    self#iter_moves
      (function
	| Move(mid, _, _, _) as mov -> begin
	    try
	      let members = Hashtbl.find tbl !mid in
	      Hashtbl.replace tbl !mid (mov::members)
	    with 
              Not_found -> Hashtbl.add tbl !mid [mov]
	end
	| _ -> assert false
      );

    Hashtbl.iter (* singletons are excluded and assign fresh id for members *)
      (fun mid movs ->
	if (List.length movs) > 1 then begin
          List.iter
            (function
	      | Move(m, _, _, _) ->
                  let m' = mid_gen#gen in
                  DEBUG_MSG "%a -> %a" MID.ps !m MID.ps m';
                  m := m';
                  Hashtbl.add mov_gr_tbl m' mid
              | _ -> assert false
            ) movs;
	  Hashtbl.add mov_gr_mem_tbl mid (sort_edit_list_topdown movs)
        end
      ) tbl

  method finalize (uidmapping : 'node_t UIDmapping.c) (is_anon : 'node_t -> bool) =
    self#sync;
    self#setup_multi_mov_gr_tbl uidmapping is_anon

  method to_string = 
    let buf = Buffer.create 0 in
    Buffer.add_string buf (sprintf "%d edit(s):\n" self#get_nedits);
    self#iter_topdown
      (fun ed -> 
	Buffer.add_string buf (to_string ed);
	Buffer.add_string buf "\n"
      );
    Buffer.contents buf

  method to_string_gid = 
    let buf = Buffer.create 0 in
    Buffer.add_string buf (sprintf "%d edit(s):\n" self#get_nedits);
    self#iter_topdown
      (fun ed -> 
	Buffer.add_string buf (to_string_gid ed);
	Buffer.add_string buf "\n"
      );
    Buffer.contents buf

  method private _find12 table_pairs uid1 uid2 =
    let res = ref [] in
    List.iter
      (fun (tbl1, tbl2) ->
	try
	  let e1 = Hashtbl.find tbl1 uid1 in
	  let e2 = Hashtbl.find tbl2 uid2 in
	  if e1 = e2 then 
	    res := e1::!res
	with 
	  Not_found -> ()
      ) table_pairs;

    if !res <> [] then
      DEBUG_MSG "%a-%a -> [%s]" 
	UID.ps uid1 UID.ps uid2 (Xlist.to_string to_string ";" !res);

    !res

  method find12 uid1 uid2 =
    self#_find12 [(rel1_tbl, rel2_tbl); (mov1_tbl, mov2_tbl)] uid1 uid2

  method find21 uid2 uid1 =
    self#_find12 [(rel1_tbl, rel2_tbl); (mov1_tbl, mov2_tbl)] uid1 uid2

  method find_mov12 uid1 uid2 =
    match self#_find12 [(mov1_tbl, mov2_tbl)] uid1 uid2 with
    | [] -> raise Not_found
    | [ed] -> ed
    | _ -> assert false

  method find_mid12 uid1 uid2 =
    match self#find_mov12 uid1 uid2 with
    | Move(mid, _, _, _) -> !mid
    | _ -> assert false

  method find_rel12 uid1 uid2 =
    match self#_find12 [rel1_tbl, rel2_tbl] uid1 uid2 with
    | [] -> raise Not_found
    | [ed] -> ed
    | _ -> assert false

(*
  method has_rel12 uid1 uid2 =
    try
      let _ = self#find_rel12 uid1 uid2 in
      true
    with
      Not_found -> false
*)

  method find_maps_of_relabels =
    let res = ref [] in
    Hashtbl.iter 
      (fun _ ed ->
	match ed with
	| Relabel(_, (u, _, _), (v, _, _)) -> res := (u, v)::!res
	| _ -> ()
      ) rel1_tbl;
    !res
      
  method find1 uid =
    let res = ref [] in
    List.iter
      (fun tbl ->
	try
	  res := (Hashtbl.find tbl uid)::!res
	with 
	  Not_found -> ()
      ) [del_tbl; rel1_tbl; mov1_tbl];
    !res

  method find_del uid =
    Hashtbl.find del_tbl uid

  method find2 uid =
    let res = ref [] in
    List.iter
      (fun tbl ->
	try
	  res := (Hashtbl.find tbl uid)::!res
	with 
	  Not_found -> ()
      ) [ins_tbl; rel2_tbl; mov2_tbl];
    !res

  method find_ins uid =
    Hashtbl.find ins_tbl uid

  method find_mov1 uid =
    Hashtbl.find mov1_tbl uid

  method find_mov2 uid =
    Hashtbl.find mov2_tbl uid


  method mem1 uid = match self#find1 uid with [] -> false | _ -> true
  method mem2 uid = match self#find2 uid with [] -> false | _ -> true

  method mem_del uid =
    Hashtbl.mem del_tbl uid

  method mem_ins uid =
    Hashtbl.mem ins_tbl uid

  method mem_mov12 uid1 uid2 =
    try
      let e1 = Hashtbl.find mov1_tbl uid1 in
      let e2 = Hashtbl.find mov2_tbl uid2 in
      e1 = e2
    with 
      Not_found -> false

  method mem_mov21 uid2 uid1 =
    self#mem_mov12 uid1 uid2

  method mem_mov1 uid = Hashtbl.mem mov1_tbl uid
  method mem_mov2 uid = Hashtbl.mem mov2_tbl uid

  method mem_rel12 uid1 uid2 =
    try
      let e1 = Hashtbl.find rel1_tbl uid1 in
      let e2 = Hashtbl.find rel2_tbl uid2 in
      e1 = e2
    with 
      Not_found -> false

  method mem_rel1 uid = Hashtbl.mem rel1_tbl uid
  method mem_rel2 uid = Hashtbl.mem rel2_tbl uid


  method sort_topdown =
    list <- 
      sort_edit_list_topdown
        (List.flatten
	   (List.map 
	      self#edit_tbl_to_list 
	      [del_tbl; ins_tbl; mov1_tbl; rel1_tbl]))


  method sync =
    let dels, inss, rels, movs = ref [], ref [], ref [], ref [] in
    List.iter
      (fun (tbl, eds) -> Hashtbl.iter (fun _ e -> eds := e::!eds) tbl) 
      [del_tbl, dels; ins_tbl, inss; rel1_tbl, rels; mov1_tbl, movs];
    list <- !dels @ !movs @ !inss @ !rels


  method iter_topdown f =
    self#sort_topdown;
    List.iter f list


  method filter f =
    List.iter
      (fun tbl ->
	Hashtbl.iter 
	  (fun u e -> if not (f e) then Hashtbl.remove tbl u) tbl)
      tables

  method filter_deletes f =
    Hashtbl.iter 
      (fun u e -> if not (f e) then Hashtbl.remove del_tbl u) del_tbl

  method filter_inserts f =
    Hashtbl.iter 
      (fun u e -> if not (f e) then Hashtbl.remove ins_tbl u) ins_tbl

  method filter_relabels f =
    List.iter
      (fun tbl ->
	Hashtbl.iter 
	  (fun u e -> if not (f e) then Hashtbl.remove tbl u) tbl)
      [rel1_tbl; rel2_tbl]

  method filter_moves f =
    List.iter
      (fun tbl ->
	Hashtbl.iter 
	  (fun u e -> if not (f e) then Hashtbl.remove tbl u) tbl)
      [mov1_tbl; mov2_tbl]


  method get_line_align 
      (tree1 : 'tree_t)
      (tree2 : 'tree_t)
      (uidmapping : 'node_t UIDmapping.c)
      =
    let moved_nodes = self#get_moved_nodes tree1 in
    let moved_uids = Xset.create 0 in
    Xset.iter (fun n -> Xset.add moved_uids n#uid) moved_nodes;
    let aligns = Xset.create 0 in
    uidmapping#iter
      (fun u1 u2 ->
        let n1 = tree1#search_node_by_uid u1 in
        let n2 = tree2#search_node_by_uid u2 in
        let l1 = n1#data#src_loc.Loc.start_line in
        let l2 = n2#data#src_loc.Loc.start_line in
	if 
	  not (Xset.mem moved_uids u1) && 
	  not (self#mem_rel12 u1 u2) &&
          l1 > 0 && l2 > 0
	then
	  Xset.add aligns (l1, l2)
      );

    let alignl = 
      List.fast_sort (fun (l, _) (l', _) -> Stdlib.compare l l') (Xset.to_list aligns)
    in

    let to_lm = function
      | [p] -> LM_single p
      | [(x0, y0); (x1, y1)] ->
          let dx = x1 - x0 in
          let dy = y1 - y0 in
          if dx = dy && dx > 0 then
            LM_multi (x0, y0, dx)
          else
            assert false
      | _ -> assert false
    in

    let (r, reduced) =
      List.fold_left 
	(fun (range, lst) (l1, l2) ->
	  match range with
	  | [] -> [(l1, l2)], lst

	  | [(l1', l2')] -> 
	      if l1 = l1' + 1 && l2 = l2' + 1 then
		[(l1', l2'); (l1, l2)], lst
	      else
		[(l1, l2)], (to_lm range)::lst

	  | [(l1', l2'); (l1'', l2'')] ->
	      if l1 = l1'' + 1 && l2 = l2'' + 1 then
		[(l1', l2'); (l1, l2)], lst
	      else
		[(l1, l2)], (to_lm range)::lst

	  | _ -> assert false
	) ([], []) alignl
    in
    let res =
      List.rev
	(match r with
	| [] -> reduced
	| [p] -> (to_lm r)::reduced
	| [(x0, y0); (x1, y1)] -> (to_lm r)::reduced
	| _ -> assert false
	)
    in
    res


  method dump_line_align_ch ~formatters align ch =
    List.iter (fun a -> output_string ch (formatters#align a)) align

  method dump_diff_simple ?(line_align=[]) tree1 tree2 fname =
    Xfile.dump fname (self#dump_diff_simple_ch ~line_align tree1 tree2)

  method dump_diff_simple_ch ?(line_align=[]) (tree1 : 'tree_t) (tree2 : 'tree_t) =
    let formatters = 
      new formatters_base
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
    in
    self#dump_diff_ch ~header:"" ~footer:"" ~formatters ~line_align tree1 tree2

  method dump_diff_json ?(line_align=[]) tree1 tree2 fname =
    Xfile.dump fname (self#dump_diff_json_ch ~line_align tree1 tree2)

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
      new formatters_base
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
        ~align:
        (function
          | LM_single(l1, l2) -> mkalign l1 l2
          | LM_multi(l1, l2, ext) -> mkalign ~ext l1 l2
        )
    in
    self#dump_diff_ch ~header:"[" ~footer:"null]" ~formatters ~line_align tree1 tree2

  method private dump_diff_ch
      ~header ~footer ~formatters
      ?(line_align=[]) 
      (tree1 : 'tree_t) (tree2 : 'tree_t) 
      ch 
      =

    DEBUG_MSG "* DUMPING DIFF DATA (%d edit(s))\n" self#get_nedits;

    let get_segments info excludes =

      BEGIN_DEBUG
	DEBUG_MSG "info=%s" (Info.to_string info);
	DEBUG_MSG "excludes:";
	List.iter (fun i -> DEBUG_MSG "%s" (Info.to_string i)) !excludes
      END_DEBUG;

      let excludes' = !excludes in
      let segs = Info.segment (info, (Info.sort_infos excludes')) in

      DEBUG_MSG "result=%s" (segments_to_string segs);

      sort_segments segs
    in
  
    let get_mov_segs_pair = function
      | Move(mid, _, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	  let segs1 = get_segments info1 excludes1 in
	  let segs2 = get_segments info2 excludes2 in
	  segs1, segs2
      | _ -> assert false
    in
    
    fprintf ch "%s" header;

    let segment_count = ref 0 in

    self#iter_topdown
      (fun ed ->

	DEBUG_MSG "scanning %s" (to_string ed);

	try
	  match ed with
	  | Delete(_, uid, info, excludes) ->
              let loc = Info.get_loc info in
              let st, ed = loc.Loc.start_offset, loc.Loc.end_offset in
	      let segs = get_segments info excludes in
              output_string ch (formatters#delete st ed segs)

	  | Insert(_, uid, info, excludes) ->
              let loc = Info.get_loc info in
              let st, ed = loc.Loc.start_offset, loc.Loc.end_offset in
	      let segs = get_segments info excludes in
	      output_string ch (formatters#insert st ed segs)
		
	  | Relabel(movrel, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
              let loc1 = Info.get_loc info1 in
              let loc2 = Info.get_loc info2 in
              let st1, ed1 = loc1.Loc.start_offset, loc1.Loc.end_offset in
              let st2, ed2 = loc2.Loc.start_offset, loc2.Loc.end_offset in
	      let segs1 = get_segments info1 excludes1 in
	      let segs2 = get_segments info2 excludes2 in
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

    BEGIN_DEBUG
      DEBUG_MSG "delete:%d insert:%d relabel:%d move:%d (total:%d)\n" 
	self#get_ndeletes self#get_ninserts self#get_nrelabels self#get_nmoves self#get_nedits;
      DEBUG_MSG "%d Segment exception(s) raised" !segment_count
    END_DEBUG;

    if line_align <> [] then
      self#dump_line_align_ch formatters line_align ch;

    fprintf ch "%s" footer

  (* end of method dump_diff_ch *)





  method dump_diff_info fname tree1 tree2 =
    Xfile.dump fname (self#dump_diff_info_ch tree1 tree2)


  method dump_diff_info_ch (tree1 : 'tree_t) (tree2 : 'tree_t) ch =
    let get_gid nd = 
      let g = nd#data#gid in
      if g > 0 then g else nd#gindex
    in

    let finished_movs = Xset.create 0 in (* mid set *)

    let get_size_of_mov = function
      | Move(_, _, (_, info1, _), _) -> Info.get_size info1
      | _ -> assert false
    in

    let excludes_to_str ex =
      " ["^
      (Xlist.to_string 
	 (fun inf -> 
	   sprintf "(%a:%a)" UID.ps (Info.get_uid inf) GI.ps (Info.get_gid inf)
	 ) ";" ex)^
      "]"
    in

    let dump_mov ch tab = function
      | Move(mid, kind, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	  let nd1 = Info.get_node info1 in
	  let nds1 = List.map Info.get_node !excludes1 in
	  let elems1 = ref [] in
	  tree1#scan_initial_cluster (nd1, nds1) (fun n -> elems1 := (get_gid n) :: !elems1);

	  let kind_str = 
	    if !kind = Mpermutation then ":PERMUTATION" else "" 
	  in
	  fprintf ch "%sMOVE%a(%d)%s %s%s -> %s%s <%s>\n" tab MID.p !mid
            (Info.get_size info1) kind_str (Info.to_string info1) 
	    (if !excludes1 = [] then 
	      "" 
	    else
	      excludes_to_str !excludes1
	    )
	    (Info.to_string info2)
	    (if !excludes2 = [] then 
	      "" 
	    else
	      excludes_to_str !excludes2
	    )
	    (GI.list_to_string !elems1)

      | _ -> assert false
    in

    self#iter 
      (fun ed ->
	try
	  match ed with
	  | Delete(_, uid, info, excludes) ->
	      let nd = Info.get_node info in
	      let nds = List.map Info.get_node !excludes in
	      let elems = ref [] in
	      tree1#scan_initial_cluster (nd, nds) (fun n -> elems := (get_gid n) :: !elems);

	      fprintf ch "DELETE(%s) %s%s <%s>\n" 
		(sprintf "%d" info.Info.i_size)
		(Info.to_string info)
		(if !excludes = [] then 
		  "" 
		else
		  excludes_to_str !excludes
		)
		(GI.list_to_string !elems)

	  | Insert(_, uid, info, excludes) ->
	      let nd = Info.get_node info in
	      let nds = List.map Info.get_node !excludes in
	      let elems = ref [] in
	      tree2#scan_initial_cluster (nd, nds) (fun n -> elems := (get_gid n) :: !elems);

	      fprintf ch "INSERT(%s) %s%s <%s>\n" 
		(sprintf "%d" info.Info.i_size)
		(Info.to_string info)
		(if !excludes = [] then 
		  "" 
		else
		  excludes_to_str !excludes
		)
		(GI.list_to_string !elems)

	  | Relabel(movrel, (uid1, info1, excludes1), (uid2, info2, excludes2)) ->
	      let marker =
		if !movrel then
		  "MOVREL"
		else
		  "RELABEL"
	      in
	      fprintf ch "%s %s%s -> %s%s\n"
		marker
		(Info.to_string info1) 
		(if !excludes1 = [] then 
		  "" 
		else
		  excludes_to_str !excludes1
		)
		(Info.to_string info2)
		(if !excludes2 = [] then 
		  "" 
		else
		  excludes_to_str !excludes2
		)

	  | Move(mid, kind, _, _) as mov ->
	      if not (Xset.mem finished_movs !mid) then begin
		try
                  let mg = self#find_mov_gr !mid in
		  let movs = self#find_mov_gr_mems mg in
                  List.iter (Xset.add finished_movs) (List.map get_mid movs);
		  let sz = List.fold_left (fun s m -> s + (get_size_of_mov m)) 0 movs in
		  let kind_str = if !kind = Mpermutation then ":PERMUTATION" else "" in
		  fprintf ch "MOVE(GROUP)(%d)%a%s:\n" sz MID.p mg kind_str;
		  List.iter (dump_mov ch "|- ") movs
		with Not_found ->
		  dump_mov ch "" mov
	      end

	with 
          Info.Segment -> 
	    DEBUG_MSG "Info.Segment exception raised!"
      )
  (* end of method dump_diff_info_ch *)


  method get_diff_summary tree1 tree2 uidmapping =
    
    (* uid -> (node * message * edits) *)
    let to_be_notified_tbl1 = Hashtbl.create 0 in
    let to_be_notified_tbl2 = Hashtbl.create 0 in

    let add_to_tbn tbl nd mes ed =
      let uid = nd#uid in
      try
	let (_, mes_r, edl_r, nmaps_r) = Hashtbl.find tbl uid in
	edl_r := ed::!edl_r;
	if !mes_r = "" then mes_r := mes
      with Not_found ->
	Hashtbl.add tbl uid (nd, ref mes, ref [ed], ref 0)
    in
    let make_notification tbl tree uid mes ed =
      let nd = tree#search_node_by_uid uid in
      if nd#data#to_be_notified then
	add_to_tbn tbl nd mes ed;
      let ancestors = tree#initial_ancestor_nodes nd in
      List.iter 
	(fun a -> 
	  if a#data#to_be_notified then add_to_tbn tbl a "" ed
	) ancestors
    in
    let make_notification1 = 
      make_notification to_be_notified_tbl1 tree1 in

    let make_notification2 = 
      make_notification to_be_notified_tbl2 tree2 in

    self#iter 
      (fun ed ->
	match ed with
	| Delete(_, uid, info, excludes) ->
	    make_notification1 uid "DELETED" ed;

	| Insert(_, uid, info, excludes) ->
	    make_notification2 uid "INSERTED" ed;
	    
	| Relabel(_, (uid1, info1, excludes1), (uid2, info2, excludes2))
	  ->
	    make_notification1 uid1 "RENAMED" ed;

	| Move(mid, _, 
	       (uid1, info1, excludes1), (uid2, info2, excludes2))
	  ->
	    make_notification1 uid1 "MOVED" ed;
      );
    Hashtbl.iter 
      (fun uid (nd, _, edl_r, _) ->
	try
	  let uid1 = uidmapping#inv_find uid in
	  let (_, _, edl_r', _) = 
	    Hashtbl.find to_be_notified_tbl1 uid1 
	  in
	  edl_r' := !edl_r' @ !edl_r;
	  Hashtbl.remove to_be_notified_tbl2 uid
	with Not_found -> ()
      ) to_be_notified_tbl2;

    let units_to_be_notified1 = tree1#get_units_to_be_notified in

    let unmodified =
      List.filter 
	(fun nd -> not (Hashtbl.mem to_be_notified_tbl1 nd#uid))
	units_to_be_notified1
    in

    uidmapping#iter
      (fun uid _ ->
	tree1#iter_initial_ancestors uid
	  (fun auid ->
	    try
	      let (_, _, _, nmaps_r) = 
		Hashtbl.find to_be_notified_tbl1 auid 
	      in
	      incr nmaps_r
	    with Not_found -> ()
	  )
      );

    to_be_notified_tbl1, to_be_notified_tbl2, 
    units_to_be_notified1, unmodified
  (* end of method get_diff_summary *)


  method dump_diff_summary fname tree1 tree2 uidmapping =
    Xfile.dump fname (self#dump_diff_summary_ch tree1 tree2 uidmapping)

  method dump_diff_summary_ch
      (tree1 : 'tree_t) 
      (tree2 : 'tree_t) 
      uidmapping
      ch
      =

    let to_be_notified_tbl1, to_be_notified_tbl2, _, unmodified =
      self#get_diff_summary tree1 tree2 uidmapping
    in

    fprintf ch "*** Modified Units ***\n";

    let show tbl =
      Hashtbl.iter 
	(fun uid (nd, mes_r, edl_r, nmaps_r) ->
	  let ((d, dg), (i, ig), (r, rg), (m, mg)) = 
	    List.fold_left 
	      (fun (((d, dg) as dd), 
		    ((i, ig) as ii), 
		    ((r, rg) as rr), 
		    ((m, mg) as mm)) ed 
		->
		  match ed with
		  | Delete(_, _, info, _) -> 
		      (d + (Info.get_size info), dg + 1), ii, rr, mm
		  | Insert(_, _, info, _) -> 
		      dd, (i + (Info.get_size info), ig + 1), rr, mm
		  | Relabel(_, (_, info, _), _) -> 
		      dd, ii, (r + (Info.get_size info), rg + 1), mm
		  | Move(_, _, (_, info, _), _) -> 
		      dd, ii, rr, (m + (Info.get_size info), mg + 1)
	      ) ((0, 0), (0, 0), (0, 0), (0, 0)) !edl_r
	  in
	  let count = d + i + r + m in
	  let count_gr = dg + ig + rg + mg in
	  fprintf ch "%s(%s): [%s] %d(%d) changed [%s], %d mapped\n" 
	    (get_label nd) (Loc.to_string nd#data#src_loc)
	    (if !mes_r = "" then "MODIFIED" else !mes_r) 
	    count
	    count_gr
	    (sprintf 
	       "d:%d(%d)i:%d(%d)r:%d(%d)m:%d(%d)" d dg i ig r rg m mg)
	    !nmaps_r
	) tbl
    in
    show to_be_notified_tbl1;
    show to_be_notified_tbl2;

    fprintf ch "*** Unmodified Units ***\n";

    List.iter
      (fun nd -> 
	fprintf ch "%s(%s)\n" 
	  nd#data#label (Loc.to_string nd#data#src_loc)
      ) unmodified
  (* end of method dump_diff_summary_ch *)


  method get_diff_stat tree1 tree2 uidmapping =
    let _, _, units, unmodified =
      self#get_diff_summary tree1 tree2 uidmapping
    in
    let nunits = List.length units in
    let nunmodified = List.length unmodified in

    let ndels    = self#get_ndeleted_nodes in
    let ninss    = self#get_ninserted_nodes in
    let nrels    = self#get_nrelabeled_nodes in
    let nmovrels = self#get_nmoved_and_relabeled_nodes tree1 uidmapping in
    let nmovs    = self#get_nmoved_nodes () in

    let nmovrels2 = self#get_nmoved_and_relabeled_nodes ~minsize:2 tree1 uidmapping in
    let nmovs2    = self#get_nmoved_nodes ~minsize:2 () in

    let ndelgrs = self#get_ndeletes in
    let ninsgrs = self#get_ninserts in
    let nmovgrs = self#get_nmove_groups in

    let nmaps = uidmapping#size in
    let total = ndels + ninss + nrels + (* nmovs *) nmovgrs in
    let nnodes1 = tree1#_initial_size in
    let nnodes2 = tree2#_initial_size in
    let sim =
      if total = 0 then
        "1.0"
      else
        let spm = nmaps - nmovs - nrels + nmovrels in
        let _sim = float (spm * 2) /. float (nnodes1 + nnodes2) in
        sprintf "%.6f" _sim
    in
    let cr =
      sprintf "%.6f" ((float_of_int total) /. (float_of_int nmaps))
    in
    let ur =
      sprintf "%.6f"
	((float_of_int nunmodified) /. (float_of_int nunits))
    in
    let ahs =
      let n = ndels + ninss + nmovs in
      let ng = ndelgrs + ninsgrs + nmovgrs in
      sprintf "%.6f" ((float_of_int n) /. (float_of_int ng))
    in
    { s_nnodes1 = nnodes1; s_nnodes2 = nnodes2;
      s_deletes     = ndels;
      s_deletes_gr  = ndelgrs;
      s_inserts     = ninss;
      s_inserts_gr  = ninsgrs;
      s_relabels    = nrels;
      s_relabels_gr = self#get_nrelabels;
      s_movrels     = nmovrels;
      s_moves       = nmovs;
      s_moves_gr    = nmovgrs;
      s_mapping     = nmaps;
      s_units            = nunits;
      s_unmodified_units = nunmodified;
      s_total_changes    = total;
      s_similarity       = sim;
      s_change_ratio     = cr;
      s_unmodified_rate  = ur;
      s_SPSM = nmaps - nmovs - nrels + nmovrels;
      s_SPM  = nmaps - nmovs;
      s_MGSM = nmovs2 - nmovrels2;
      s_MGM  = nmovs2;
      s_AHS  = ahs;
    }
  (* end of method get_diff_stat *)

  method dump_diff_stat_ch ?(short=false) tree1 tree2 (uidmapping : 'node_t UIDmapping.c) =
    let s = self#get_diff_stat tree1 tree2 uidmapping in
    dump_diff_stat_ch ~short s

  method dump_diff_stat ?(short=false) fname tree1 tree2 uidmapping =
    Xfile.dump fname (self#dump_diff_stat_ch ~short tree1 tree2 uidmapping)

  method show_diff_stat ?(short=false) tree1 tree2 uidmapping =
    if not options#viewer_flag then begin
      self#dump_diff_stat_ch ~short tree1 tree2 uidmapping stdout
    end
  (* end of method show_diff_stat *)


  method private get_cluster id_gen label bg fg tree inf excluded =
    let nd = Info.get_node inf in
    let nds = List.map Info.get_node !excluded in

    let buf = Buffer.create 0 in
(*
    Buffer.add_string buf (sprintf "subgraph cluster_%d {\n" (id_gen()));
    Buffer.add_string buf (sprintf "label=\"%s\";\n" label);
*)
    if label <> "" then
      Buffer.add_string buf
        (sprintf "%a [xlabel=\"%s\"];\n" UID.rs nd#uid label);

    tree#scan_initial_cluster (nd, nds)
      (fun n ->
        Buffer.add_string buf
          (sprintf "%a [%sstyle=filled,fillcolor=\"%s\",fontcolor=\"%s\"];\n"
             UID.rs n#uid (if n == nd then "peripheries=2," else "") bg fg)
      );
(*
    Buffer.add_string buf "};\n";
*)
    buf

  method dump_dot_ch1
      ?(final=false)
      tree1
      tree2
      (uidmapping : 'node_t UIDmapping.c)
      ch
      =
    let mid_tbl = Hashtbl.create 0 in
    self#iter_moves
      (function
        | Move(mid, _, (_, info1, excluded1), _) ->
            let lab = sprintf "%a" MID.ps !mid in
            let nd1 = Info.get_node info1 in
            Hashtbl.add mid_tbl nd1 lab
        | _ -> assert false
      );

    let mklab =
      if final then
        fun nd ->
          let head =
            try
              sprintf "[%s]\\n" (Hashtbl.find mid_tbl nd)
            with
              Not_found -> ""
          in
          let tail =
            try
              let nd' =
                tree2#search_node_by_uid (uidmapping#find nd#uid)
              in
              sprintf "\\n->%a" GI.ps nd'#gindex
            with
              _ -> ""
          in
          head^(Otree.dot_label_of_node_ini nd)^tail
      else
        fun nd -> Otree.dot_label_of_node nd
    in
    let dot1 = 
      if final then
	tree1#to_dot_initial ?mklab:(Some mklab) []
      else
	tree1#to_dot ?mklab:(Some mklab) []
    in
    let id_gen =
      let count = ref 0 in
      fun () ->
        let id = !count in
        incr count;
        id
    in
    let buf = Buffer.create 0 in
    Buffer.add_string buf "digraph D1 {\nordering=out;\n";
    Buffer.add_buffer buf dot1;

    self#iter_deletes
      (function
	| Delete(_, _, inf, excluded) ->
            let buf0 =
              self#get_cluster id_gen "" del_bg del_fg tree1 inf excluded
            in
            Buffer.add_buffer buf buf0

	| _ -> assert false
      );
    self#iter_moves
      (function
        | Move(mid, _, (_, info1, excluded1), (_, info2, excluded2)) ->
            let lab = ""(*sprintf "MOVE:%a" MID.ps !mid*) in
            let buf0 =
              self#get_cluster id_gen lab mov_bg mov_fg tree1 info1 excluded1
            in
            Buffer.add_buffer buf buf0

        | _ -> assert false
      );
    self#iter_relabels
      (function
	| Relabel(movrel, (_, info1, _), (_, info2, _)) ->
	    let nd1 = Info.get_node info1 in
            Buffer.add_string buf
              (sprintf "%a [style=filled,fillcolor=\"%s\",fontcolor=\"%s\"];\n"
                 UID.rs nd1#uid
                 (if !movrel then movrel_bg else rel_bg)
                 (if !movrel then movrel_fg else rel_fg)
              )

	| _ -> assert false
      );

    Buffer.add_string buf "}\n";

    Buffer.output_buffer ch buf

  (* end of method dump_dot_ch1 *)

  method dump_dot_ch2
      ?(final=false)
      tree2
      tree1
      (uidmapping : 'node_t UIDmapping.c)
      ch
      =
    let mid_tbl = Hashtbl.create 0 in
    self#iter_moves
      (function
        | Move(mid, _, _, (_, info2, excluded2)) ->
            let lab = sprintf "%a" MID.ps !mid in
            let nd2 = Info.get_node info2 in
            Hashtbl.add mid_tbl nd2 lab
        | _ -> assert false
      );

    let mklab =
      if final then
        fun nd ->
          let head =
            try
              sprintf "[%s]\\n" (Hashtbl.find mid_tbl nd)
            with
              Not_found -> ""
          in
          let tail =
            try
              let nd' =
                tree1#search_node_by_uid (uidmapping#inv_find nd#uid)
              in
              sprintf "\\n%a->" GI.ps nd'#gindex
            with
              _ -> ""
          in
          head^(Otree.dot_label_of_node_ini nd)^tail
      else
        fun nd -> Otree.dot_label_of_node nd
    in
    let dot2 =
      if final then
	tree2#to_dot_initial ?mklab:(Some mklab) []
      else
	tree2#to_dot ?mklab:(Some mklab) []
    in
    let id_gen =
      let count = ref 0 in
      fun () ->
        let id = !count in
        incr count;
        id
    in
    let buf = Buffer.create 0 in
    Buffer.add_string buf "digraph D2 {\nordering=out;\n";
    Buffer.add_buffer buf dot2;

    self#iter_inserts
      (function
	| Insert(_, _, inf, excluded) ->
            let buf0 = self#get_cluster id_gen "" ins_bg ins_fg tree2 inf excluded in
            Buffer.add_buffer buf buf0

	| _ -> assert false
      );
    self#iter_moves
      (function
        | Move(mid, _, (_, info1, excluded1), (_, info2, excluded2)) ->
            let lab = ""(*sprintf "MOVE:%a" MID.ps !mid*) in
            let buf0 =
              self#get_cluster id_gen lab mov_bg mov_fg tree2 info2 excluded2
            in
            Buffer.add_buffer buf buf0

        | _ -> assert false
      );
    self#iter_relabels
      (function
	| Relabel(movrel, (_, info1, _), (_, info2, _)) ->
	    let nd2 = Info.get_node info2 in
            Buffer.add_string buf
              (sprintf "%a [style=filled,fillcolor=\"%s\",fontcolor=\"%s\"];\n"
                 UID.rs nd2#uid
                 (if !movrel then movrel_bg else rel_bg)
                 (if !movrel then movrel_fg else rel_fg)
              )

	| _ -> assert false
      );

    Buffer.add_string buf "}\n";

    Buffer.output_buffer ch buf

  (* end of method dump_dot_ch2 *)

  method dump_dot1 ?(final=false) fname (tree1 : 'tree_t) (tree2 : 'tree_t) (uidmapping : 'node_t UIDmapping.c) =
    Xfile.dump fname (self#dump_dot_ch1 ~final tree1 tree2 uidmapping)

  method dump_dot2 ?(final=false) fname (tree2 : 'tree_t) (tree1 : 'tree_t) (uidmapping : 'node_t UIDmapping.c) =
    Xfile.dump fname (self#dump_dot_ch2 ~final tree2 tree1 uidmapping)


  (* checks whether this edit seq is correct or not *)
  (* NB: this function modifies the trees in-place *)
  method check 
      (tree1 : 'tree_t) 
      (tree2 : 'tree_t) 
      (uidmapping : 'node_t UIDmapping.c)
      =
    BEGIN_DEBUG
      DEBUG_MSG "checking result";
      DEBUG_MSG "%s" self#to_string
    END_DEBUG;

    let normal_flag = ref true in

    (* initializing trees *)
    if not options#weak_flag then begin
      tree1#recover_true_children ~initial_only:false ();
      tree2#recover_true_children ~initial_only:false ()
    end;
    tree1#init;
    tree2#init;
(* tree1#setup_uid_table; tree2#setup_uid_table; *)

    DEBUG_MSG "after initialization:\nT1:\n%s\nT2:\n%s"
      tree1#to_string tree2#to_string;
    
    (* relabeling nodes in tree1 *)
    DEBUG_MSG "relabeling nodes in tree1";
    self#iter_relabels
      (function
	| Relabel(_, (u1, _, infos1), (u2, _, infos2)) ->
	    let us1 = List.map Info.get_uid !infos1 in
	    let us2 = List.map Info.get_uid !infos2 in

	    (* do relabel *)
	    let targetq = Queue.create() in
	    begin
	      try
		DEBUG_MSG "adding relabel targets %a{%s}-%a{%s}" 
		  UID.ps u1 (Xlist.to_string UID.to_string "," us1)
		  UID.ps u2 (Xlist.to_string UID.to_string "," us2);

		tree2#scan_cluster_u
		  (u2, us2) 
		  (fun nd -> Queue.add nd targetq);

		DEBUG_MSG "relabeling %a-%a" UID.ps u1 UID.ps u2;

		tree1#scan_cluster_u (u1, us1) 
		  (fun nd ->
		    let nd' = Queue.take targetq in
		    let d = nd'#data in
		    if nd#data#equals d then begin
		      normal_flag := false;
		      WARN_MSG "relabel: not a relabel: %a-%a" 
			UID.ps u1 UID.ps u2;
		      WARN_MSG "relabel: <%a:%s> = <%a:%s>" 
			UID.ps nd#uid nd#data#to_string UID.ps nd'#uid d#to_string
		    end;
		    nd#set_data d)
	      with
		Queue.Empty ->
		  normal_flag := false;
		  WARN_MSG "relabel failed: %a > %a" UID.ps u1 UID.ps u2;
	    end;
	    if not (Queue.is_empty targetq) then begin
	      normal_flag := false;
	      WARN_MSG "relabel failed: %a < %a" UID.ps u1 UID.ps u2
	    end
	| _ -> assert false
      ); (* end of relabeling *)

    DEBUG_MSG "before node deletion:\nT1:\n%s\nT2:\n%s" 
      tree1#to_string tree2#to_string;

    let get_uids infos = List.map (fun i -> Info.get_uid i) infos in

    DEBUG_MSG "deleting nodes from T1 and T2";

    (* deleting nodes from tree1 and tree2 *)
    let deleted1 = Hashtbl.create 0 in (* uid -> uid list *)
    let deleted2 = Hashtbl.create 0 in (* uid -> uid list *)
    let upd tbl uid =
      let rec follow u = 
	try 
	  let uids = Hashtbl.find tbl u in
	  List.flatten (List.map follow uids)
	with Not_found -> [u]
      in
      follow uid
    in
    self#iter
      (function
	| Delete(_, uid, info, infos) -> begin
	    try
	      let uids = get_uids !infos in
	      Hashtbl.add deleted1 uid uids;
	      let upd_uids = upd deleted1 uid in

              BEGIN_DEBUG
                let nd = Info.get_node info in
	        DEBUG_MSG "deleting (del) %a(pos=%d,parent=%a) [%s] -> [%s]" 
		  UID.ps uid nd#pos UID.ps nd#parent#uid 
		  (Xlist.to_string UID.to_string ";" uids)
		  (Xlist.to_string UID.to_string ";" upd_uids)
              END_DEBUG;

	      tree1#prune_cluster (uid, upd_uids)
	    with 
	      Not_found -> DEBUG_MSG "already deleted: %a" UID.ps uid
	  end
	| Insert(_, uid, info, infos) -> begin
	    try
	      let uids = get_uids !infos in
	      Hashtbl.add deleted2 uid uids;
	      let upd_uids = upd deleted2 uid in

              BEGIN_DEBUG
                let nd = Info.get_node info in
	        DEBUG_MSG "deleting (ins) %a(pos=%d,parent=%a[%s]) [%s] -> [%s]" 
		  UID.ps uid nd#pos UID.ps nd#parent#uid 
		  (Xlist.to_string UID.to_string ";" nd#parent#children_uids)
		  (Xlist.to_string UID.to_string ";" uids)
		  (Xlist.to_string UID.to_string ";" upd_uids)
              END_DEBUG;

	      tree2#prune_cluster (uid, upd_uids)
	    with 
	      Not_found -> DEBUG_MSG "already deleted: %a" UID.ps uid
	end
	| Move(mid, _, (uid1, info1, infos1), (uid2, info2, infos2)) ->
	    Hashtbl.add deleted1 uid1 (get_uids !infos1);
	    Hashtbl.add deleted2 uid2 (get_uids !infos2);
	    let uids1 = upd deleted1 uid1 in
	    let uids2 = upd deleted2 uid2 in

            BEGIN_DEBUG
              let nd1 = Info.get_node info1 in
              let nd2 = Info.get_node info2 in
	      DEBUG_MSG "deleting (mid:%a) %a(pos=%d,parent=%a) [%s] - %a(pos=%d,parent=%a) [%s]" 
	        MID.ps !mid
	        UID.ps uid1 nd1#pos UID.ps nd1#parent#uid (Xlist.to_string UID.to_string ";" uids1)
	        UID.ps uid2 nd2#pos UID.ps nd2#parent#uid (Xlist.to_string UID.to_string ";" uids2)
            END_DEBUG;

	    let q = Queue.create() in
	    tree2#scan_cluster_u (uid2, uids2) (fun n -> Queue.add n q);
	    tree1#scan_cluster_u (uid1, uids1)
	      (fun n ->
		let nd' = Queue.take q in
		let d = nd'#data in
		if not (n#data#eq d) then begin
		  normal_flag := false;
		  WARN_MSG "move: <%a:%s> != <%a:%s>" 
		    UID.ps n#uid n#data#to_string UID.ps nd'#uid d#to_string
		end
	      );

	    tree1#prune_cluster (uid1, uids1);
	    tree2#prune_cluster (uid2, uids2)

	| _ -> ()
      );

    tree1#init; tree2#init;

    DEBUG_MSG "AFTER node deletion (before equality check):\nT1:\n%s\nT2:\n%s" 
      tree1#to_string tree2#to_string;

    (* now tree1 should equals to tree2 *)
    let result = tree1#equals tree2 && !normal_flag in

    result


  method remove_unmapped
      (tree1 : 'tree_t) 
      (tree2 : 'tree_t) 
      =
    tree1#init;
    tree2#init;

    let get_uids infos = List.map (fun i -> Info.get_uid i) infos in

    DEBUG_MSG "deleting nodes from T1 and T2";

    (* deleting nodes from tree1 and tree2 *)
    let deleted1 = Hashtbl.create 0 in (* uid -> uid list *)
    let deleted2 = Hashtbl.create 0 in (* uid -> uid list *)
    let upd tbl uid =
      let rec follow u = 
	try 
	  let uids = Hashtbl.find tbl u in
	  List.flatten (List.map follow uids)
	with Not_found -> [u]
      in
      follow uid
    in
    let deleted_nodes1 = ref [] in
    let deleted_nodes2 = ref [] in
    self#iter
      (function
	| Delete(_, uid, info, infos) -> begin
	    try
	      let nd = tree1#search_node_by_uid uid in
	      Hashtbl.add deleted1 uid (get_uids !infos);
	      let uids = upd deleted1 uid in

	      DEBUG_MSG "deleting (del) %a(pos=%d,parent=%a) [%s]" 
		UID.ps uid nd#pos UID.ps nd#parent#uid 
		(Xlist.to_string UID.to_string ";" uids);

	      tree1#prune_cluster (uid, uids);

	      if (Info.get_size info) > options#dump_size_threshold then
		deleted_nodes1 := nd::!deleted_nodes1

	    with 
              Not_found -> DEBUG_MSG "already deleted: %a" UID.ps uid
	  end
	| Insert(_, uid, info, infos) -> begin
	    try
	      let nd = tree2#search_node_by_uid uid in
	      Hashtbl.add deleted2 uid (get_uids !infos);
	      let uids = upd deleted2 uid in

	      DEBUG_MSG "deleting (ins) %a(pos=%d,parent=%a[%s]) [%s]" 
		UID.ps uid nd#pos UID.ps nd#parent#uid 
		(Xlist.to_string UID.to_string ";" nd#parent#children_uids)
		(Xlist.to_string UID.to_string ";" uids);

	      tree2#prune_cluster (uid, uids);

	      if (Info.get_size info) > options#dump_size_threshold then
		deleted_nodes2 := nd::!deleted_nodes2

	    with 
              Not_found -> DEBUG_MSG "already deleted: %a" UID.ps uid
	end
	| _ -> ()
      );
    (!deleted_nodes1, !deleted_nodes2)
   (* end of method remove_unmapped *)


  method ungroup (tree1 : 'tree_t) (tree2 : 'tree_t)
      =
    DEBUG_MSG "ungrouping...";

    (*let group_tbl = Hashtbl.create 0 in*)

    let gensubedits tree ?(whole=false) ?(exclude=[]) node =
      let res = ref [] in
      let rec doit nd =
        DEBUG_MSG "nd=%a%!" UID.ps nd#uid;
        if not (List.memq nd exclude) then begin
          let ex =
            List.map
              Info.make
              (List.filter
                 (tree#is_initial_ancestor node)
                 exclude)
          in
	  res := (nd#uid, Info.make nd, ex)::!res;
          Array.iter doit nd#initial_children
        end
      in
      if whole then
        doit node
      else
        Array.iter doit node#initial_children;
      !res
    in

    let process_delete_or_insert ed inf ex =

      DEBUG_MSG "processing %s..." (to_string ed);

      let nd = Info.get_node inf in

      if !ex = [] then begin
        self#remove_edit ed;
	match ed with
	| Delete _ -> 
	    List.iter 
	      (fun (u, i, e) ->
                let ed' = Delete(e = [], u, i, ref e) in
                (*tbl_add group_tbl ed ed';*)
                self#add_edit ed'
              ) (gensubedits tree1 ~whole:true nd)

	| Insert _ ->
	    List.iter 
	      (fun (u, i, e) ->
                let ed' = Insert(e = [], u, i, ref e) in
                (*tbl_add group_tbl ed ed';*)
                self#add_edit ed'
              ) (gensubedits tree2 ~whole:true nd)

	| _ -> assert false

      end
      else begin
	let exnds = List.map Info.get_node !ex in
	let processed = ref [] in

	let tree = 
	  match ed with
	  | Delete _ -> tree1
	  | Insert _ -> tree2
	  | _ -> assert false
	in

	if tree#size_of_initial_cluster (nd, exnds) > 1 then begin

	  self#remove_edit ed;

	  tree#scan_initial_cluster (nd, exnds)
	    (fun n ->
	      if 
		not (List.exists (fun p -> tree#initial_subtree_mem p n) !processed) && 
                not (is_ghost_node n)
	      then begin

		let new_info = Info.make n in
		let new_ex =
                  List.map
                    Info.make
                    (List.filter (tree#is_initial_ancestor n) exnds)
                in
                let whole = new_ex = [] in

		let new_ed =
		  match ed with
		  | Delete _ -> Delete(whole, n#uid, new_info, ref new_ex)
		  | Insert _ -> Insert(whole, n#uid, new_info, ref new_ex)
		  | _ -> assert false
		in
                (*tbl_add group_tbl ed new_ed;*)
		self#add_edit new_ed;

		if whole then begin
		  begin
		    match ed with
		    | Delete _ -> 
			List.iter 
			  (fun (u, i, e) ->
                            let ed' = Delete(e = [], u, i, ref e) in
                            (*tbl_add group_tbl ed ed';*)
                            self#add_edit ed'
                          ) (gensubedits tree1 n)

		    | Insert _ ->
			List.iter 
			  (fun (u, i, e) ->
                            let ed' = Insert(e = [], u, i, ref e) in
                            (*tbl_add group_tbl ed ed';*)
                            self#add_edit ed'
			  ) (gensubedits tree2 n)

		    | _ -> assert false
		  end;
		  processed := n :: !processed
		end

	      end
	    )

	end

      end
    in (* end of func process_delete_or_insert *)


    let to_be_added = Xset.create 0 in
    let add_ed ed = 
      DEBUG_MSG "%s" (to_string ed);
      Xset.add to_be_added ed
    in

    let process_move ed mid kind inf1 ex1 inf2 ex2 =

      DEBUG_MSG "processing %s..." (to_string ed);

      let nd1 = Info.get_node inf1 in
      let nd2 = Info.get_node inf2 in
      
      if !ex1 = [] && !ex2 = [] then begin
        self#remove_edit ed;
        List.iter2 
          (fun (u1, i1, e1) (u2, i2, e2) ->
            let ed' = Move(mid, kind, (u1, i1, ref e1), (u2, i2, ref e2)) in
            (*tbl_add group_tbl ed ed';*)
            add_ed ed'
          ) (gensubedits tree1 ~whole:true nd1) (gensubedits tree2 ~whole:true nd2)
      end
      else begin
	let exnds1 = List.map Info.get_node !ex1 in
	let exnds2 = List.map Info.get_node !ex2 in

        let processed = ref [] in

        if tree1#size_of_initial_cluster (nd1, exnds1) > 1 then begin
          self#remove_edit ed;

          let nds1 = ref [] in
          let nds2 = ref [] in
          
          tree1#scan_initial_cluster (nd1, exnds1) (fun n1 -> nds1 := n1 :: !nds1);
          tree2#scan_initial_cluster (nd2, exnds2) (fun n2 -> nds2 := n2 :: !nds2);

          List.iter2 
            (fun n1 n2 ->
	      if 
		not (List.exists (fun p -> tree1#initial_subtree_mem p n1) !processed) && 
                not (is_ghost_node n1)
	      then begin
                let u1, u2 = n1#uid, n2#uid in

                let mk_inf tree n exnds =
		  let new_info = Info.make n in
                  let new_ex =
                    List.map
                      Info.make
                      (List.filter (tree#is_initial_ancestor n) exnds)
                  in
                  new_info, new_ex
                in

                let new_info1, new_ex1 = mk_inf tree1 n1 exnds1 in
                let new_info2, new_ex2 = mk_inf tree2 n2 exnds2 in

                let whole = new_ex1 = [] && new_ex2 = [] in

		let new_ed =
                  Move(mid, kind, (u1, new_info1, ref new_ex1), (u2, new_info2, ref new_ex2))
		in
                DEBUG_MSG "adding move: mid=%a %a --> %a (whole=%B)%!" MID.ps !mid
                  UID.ps u1 UID.ps u2 whole;

                (*tbl_add group_tbl ed new_ed;*)
                add_ed new_ed;

		if whole then begin
                  let se1 = gensubedits tree1 n1 in
                  let se2 = gensubedits tree2 n2 in
                  DEBUG_MSG "number of subedits: %a->%d, %a->%d" 
                    UID.ps u1 (List.length se1) UID.ps u2 (List.length se2);
                  List.iter2 
                    (fun (u1, i1, e1) (u2, i2, e2) -> 
                      DEBUG_MSG "adding move: mid=%a %a --> %a%!" MID.ps !mid
                        UID.ps u1 UID.ps u2;
                      let ed' = Move(mid, kind, (u1, i1, ref e1), (u2, i2, ref e2)) in
                      (*tbl_add group_tbl ed ed';*)
                      add_ed ed'
                    ) se1 se2;
		  processed := n1 :: !processed
		end

	      end              
            ) !nds1 !nds2;

        end (* if tree1#size_of_initial_cluster (nd1, exnds1) > 1 *)
      end

    in (* end of func process_move *)

    self#iter
      (fun ed ->
	match ed with
	| Delete(_, _, inf, ex) -> process_delete_or_insert ed inf ex
	| Insert(_, _, inf, ex) -> process_delete_or_insert ed inf ex
        | Move(mid, kind, (_, inf1, ex1), (_, inf2, ex2)) -> process_move ed mid kind inf1 ex1 inf2 ex2
        | _ -> ()
      );

    Xset.iter 
      (fun ed -> 
        DEBUG_MSG "adding %s" (to_string ed);
        self#add_edit ed
      ) to_be_added;

    (*group_tbl*)
  (* end of method ungroup *)

  method private get_non_ghost_children node =
    let rec get nd =
      List.flatten
	(List.map
	   (fun n -> 
	     if is_ghost_node n then get n else [n]
	   ) (Array.to_list nd#initial_children))
    in
    get node

  method cleanup_ghost (tree1 : 'tree_t) (tree2 : 'tree_t) =
    DEBUG_MSG "cleaning up ghost...";

    let is_ghost_info info = is_ghost_node (Info.get_node info) in
    let cleanup tree info ex =
      let rec proc_ex rt = function
	| [] -> []
	| inf::rest ->
	    let nd = Info.get_node inf in
	    let infos = proc_ex rt rest in
	    if tree#is_initial_ancestor rt nd then
	      if is_ghost_node nd then
		(List.map Info.make (self#get_non_ghost_children nd)) @ infos
	      else
		inf :: infos
	    else
	      infos
      in
      let node = Info.get_node info in
      if is_ghost_node node then begin
	let nodes = self#get_non_ghost_children node in
        List.rev
	  (List.fold_left
	     (fun l n ->
	       let new_ex = proc_ex n ex in
	       let new_exnds = List.map Info.get_node new_ex in
	       if List.memq n new_exnds then
	         l
	       else
	         (n#uid, Info.make n, new_ex)::l
	     ) [] nodes)
      end
      else
	[node#uid, info, proc_ex node ex]
    in

    let to_be_converted info ex =
      is_ghost_info info || List.exists is_ghost_info !ex
    in

    let eds_for_ghost = ref [] in
    let add ed = eds_for_ghost := ed :: !eds_for_ghost in

    self#filter
      (function
	| Delete(_, _, info, ex) ->
	    let b = to_be_converted info ex in
	    if b then begin
	      List.iter
		(fun (u, inf, e) -> 
		  add (Delete(e = [], u, inf, ref e)))
		(cleanup tree1 info !ex)
	    end;
	    not b

	| Insert(_, _, info, ex) ->
	    let b = to_be_converted info ex in
	    if b then begin
	      List.iter
		(fun (u, inf, e) -> add (Insert(e = [], u, inf, ref e)))
		(cleanup tree2 info !ex)
	    end;
	    not b

	| Relabel(_, (uid1, info1, ex1), (uid2, info2, ex2)) ->
	    let isw info = (Info.get_node info)#initial_nchildren = 0 in
	    let b1 = to_be_converted info1 (ref []) in
	    let b2 = to_be_converted info2 (ref []) in

	    if b1 then
	      add (Insert(isw info2, uid2, info2, ex2));

	    if b2 then
	      add (Delete(isw info1, uid1, info1, ex1));

	    not (b1 || b2)

	| Move(mid, k, (uid1, info1, ex1), (uid2, info2, ex2)) ->
	    let b1 = to_be_converted info1 ex1 in
	    let b2 = to_be_converted info2 ex2 in
	    if b1 || b2 then begin

              let q = Queue.create() in
              let uids1 = List.map Info.get_uid !ex1 in
              let uids2 = List.map Info.get_uid !ex2 in
              let mkxs n =
                List.map Info.make (self#get_non_ghost_children n)
              in
              tree2#scan_cluster_u (uid2, uids2) (fun n -> Queue.add n q);
              tree1#scan_cluster_u (uid1, uids1)
                (fun n ->
                  let n' = Queue.take q in
                  match is_ghost_node n, is_ghost_node n' with
                  | true, false -> begin
                      let xs' = mkxs n' in
                      add (Insert(xs' = [], n'#uid, Info.make n', ref xs'))
                  end
                  | false, true -> begin
                      let xs = mkxs n in
                      add (Delete(xs = [], n#uid, Info.make n, ref xs))
                  end
                  | false, false -> begin
                      let xs = mkxs n in
                      let xs' = mkxs n' in
                      add (Move(mid, k,
                                (n#uid, Info.make n, ref xs),
                                (n'#uid, Info.make n', ref xs')))
                  end
                  | true, true -> ()
                );

	    end;
	    not (b1 || b2)
      );
    List.iter self#add_edit !eds_for_ghost;
    DEBUG_MSG "done."



  method shrink_moves_rp (* shrink moves to improve SPSM (for move root pairs) *)
      (tree1 : 'tree_t)
      (tree2 : 'tree_t)
      uidmapping
      (move_region_tbl : move_region_tbl_t)
      =

    DEBUG_MSG "uidmapping:\n%s\n" uidmapping#to_string;
    DEBUG_MSG "uidmapping (gindex):\n%s\n" uidmapping#to_string_gid;

    let gen_cands tree tree' map mem_mov mem_mov1 mem_del_or_ins is_mov nd nd0 =
      let uid0 = nd0#uid in
      
      DEBUG_MSG "nd#uid:%a uid0:%a" UID.ps nd#uid UID.ps uid0;

      let cands = ref [] in
      Array.iter
        (fun cnd ->
          let cuid = cnd#uid in

          DEBUG_MSG "cuid:%a" UID.ps cuid;

          try
            let cuid' = map cuid in
            if not (mem_mov cuid cuid') then
              let cnd' = tree'#search_node_by_uid cuid' in
              let nd' = cnd'#initial_parent in
              let uid' = nd'#uid in

              DEBUG_MSG "uid':%a"UID.ps uid';

              let cond0 = 
                not (List.mem uid' !cands) && 
                (nd'#data#eq nd#data) && 
                (mem_del_or_ins uid' || (mem_mov1 uid' && uid0 <> uid')) &&
                (not (is_mov nd nd'))
              in

              DEBUG_MSG "not (List.mem uid' !cands) --> %B" (not (List.mem uid' !cands));
              DEBUG_MSG "nd'#data#eq nd#data --> %B" (nd'#data#eq nd#data);
              DEBUG_MSG "mem_del_or_ins uid' || (mem_mov1 uid' && uid0 <> uid') --> %B"
                (mem_del_or_ins uid' || (mem_mov1 uid' && uid0 <> uid'));
              DEBUG_MSG "not (is_mov nd nd') --> %B" (not (is_mov nd nd'));
              DEBUG_MSG "cuid:%a --> cond0:%B" UID.ps cuid cond0;

              if cond0 then
                let cond1 =
                  try
                    tree#fast_scan_whole_initial_subtree nd
                      (fun n ->
                        if n != nd then
                          let u = n#uid in
                          try
                            let u' = map u in
                            if not (mem_mov u u') then
                              let n' = tree'#search_node_by_uid u' in
                              if not (tree'#is_initial_ancestor nd' n') then
                                raise Exit
                          with
                            Not_found -> ()
                      );
                    true
                  with
                    Exit -> false
                in
                DEBUG_MSG "cuid:%a --> cond1:%B" UID.ps cuid cond1;
                if cond1 then
                  cands := uid' :: !cands
          with
            _ -> ()
        ) nd#initial_children;
      !cands
    in (* gen_cands *)

    let same_digest = _same_digest tree1 tree2 in

    let move_region_list =
      List.fast_sort
        (fun (_, _, gi, _, _) (_, _, gi', _, _) -> Stdlib.compare gi gi')
        (Hashtbl.fold
           (fun mid (lgi1, gi1, lgi2, gi2) l ->
             (mid, lgi1, gi1, lgi2, gi2) :: l
           ) move_region_tbl [])
    in


    List.iter
      (fun (mid, lgi1, gi1, lgi2, gi2) ->

        let nd1 = tree1#search_node_by_gindex gi1 in
        let nd2 = tree2#search_node_by_gindex gi2 in

        let l1 = gi1 - lgi1 in
        let l2 = gi2 - lgi2 in

        DEBUG_MSG "checking root pairs of move%a (%a-%a) [%a:%a(%d)]-[%a:%a(%d)]..." 
          MID.ps mid UID.ps nd1#uid UID.ps nd2#uid
          GI.ps lgi1 GI.ps gi1 (l1+1) GI.ps lgi2 GI.ps gi2 (l2+1);

        let moveon =
          if same_digest nd1 nd2 then begin
            assert (l1 = l2);
            let rec loop i =
              if i > l1 then
                false
              else
                let n1 = tree1#search_node_by_gindex (lgi1 + i) in
                let n2 = tree2#search_node_by_gindex (lgi2 + i) in
                if not (uidmapping#has_mapping n1#uid n2#uid) then
                  true
                else
                  loop (i + 1)
            in
            loop 0
          end
          else
            true
        in

        if moveon then begin

          let is_mov1 n1 n2 = self#is_crossing_with_untouched uidmapping n1 n2 in
          let is_mov2 n2 n1 = self#is_crossing_with_untouched uidmapping n1 n2 in

          let ndps = ref [] in

          let nd1x, nd2x = ref nd1, ref nd2 in

          let cands01, cands02 = ref [], ref [] in
          
          let lv = ref 0 in

          begin
            try
              while true do
                cands01 := gen_cands tree2 tree1 uidmapping#inv_find self#mem_mov21 self#mem_mov1 self#mem_del is_mov2 !nd2x !nd1x;
                cands02 := gen_cands tree1 tree2 uidmapping#find self#mem_mov12 self#mem_mov2 self#mem_ins is_mov1 !nd1x !nd2x;

                DEBUG_MSG "[%d] cands01(%a): [%s]" !lv UID.ps !nd2x#uid (Xlist.to_string UID.to_string ";" !cands01);
                DEBUG_MSG "[%d] cands02(%a): [%s]" !lv UID.ps !nd1x#uid (Xlist.to_string UID.to_string ";" !cands02);

                if !cands01 <> [] || !cands02 <> [] then begin
                  ndps := (!nd1x, !nd2x) :: !ndps;
                  raise Exit
                end;

                DEBUG_MSG "nd1x=%a(%d) nd2x=%a(%d)" 
                  UID.ps !nd1x#uid !nd1x#initial_nchildren UID.ps !nd2x#uid !nd2x#initial_nchildren;

                if (!nd1x)#initial_nchildren = 1 && (!nd2x)#initial_nchildren = 1 then begin
                  let nx1 = (!nd1x)#initial_children.(0) in
                  let nx2 = (!nd2x)#initial_children.(0) in
                  let ux1, ux2 = nx1#uid, nx2#uid in
                  if self#mem_mov12 ux1 ux2 then begin
                    ndps := (!nd1x, !nd2x) :: !ndps;
                    nd1x := nx1;
                    nd2x := nx2;
                  end
                  else
                    raise Exit
                end
                else
                  raise Exit;

                incr lv
              done
            with
              Exit -> ()
          end;

          let remove_orig() =
            List.iter
              (fun (n1, n2) ->
                let u1, u2 = n1#uid, n2#uid in
                DEBUG_MSG "%a-%a" UID.ps u1 UID.ps u2;

                let es1 = self#find1 u1 in
                List.iter self#remove_edit es1;

                let es2 = self#find2 u2 in
                List.iter self#remove_edit es2;
                
                uidmapping#remove u1 u2
              ) !ndps
          in

          let check1 u1 =
            let b = 
              let p1 = ref u1 in
              let pn1 = ref (tree1#search_node_by_uid u1) in
              try
                List.iter
                  (fun (_, n2) -> 

                    if self#is_crossing_with_untouched uidmapping !pn1 n2 then
                      raise Exit;

                    pn1 := (tree1#search_node_by_uid !p1)#initial_parent;
                    p1 := (!pn1)#uid
                  ) !ndps;
                true
              with
                Exit -> false
            in
            DEBUG_MSG "%a --> %B" UID.ps u1 b;
            b
          in

          let check2 u2 =
            let b =
              let p2 = ref u2 in
              let pn2 = ref (tree2#search_node_by_uid u2) in
              try
                List.iter
                  (fun (n1, _) -> 

                    if self#is_crossing_with_untouched uidmapping n1 !pn2 then
                      raise Exit;

                    pn2 := (tree2#search_node_by_uid !p2)#initial_parent;
                    p2 := (!pn2)#uid
                  ) !ndps;
                true
              with
                Exit -> false
            in
            DEBUG_MSG "%a --> %B" UID.ps u2 b;
            b
          in


          let handle1 u1 =
            DEBUG_MSG "u1:%a" UID.ps u1;
            let p1 = ref u1 in
            let pn1 = ref (tree1#search_node_by_uid u1) in
            List.iter
              (fun (_, n2) -> 
                if self#mem_del !p1 then begin
                  DEBUG_MSG "%a -> del" UID.ps !p1;
                  self#remove_edit (self#find_del !p1)
                end
                else if self#mem_mov1 !p1 then begin
                  DEBUG_MSG "%a -> mov1" UID.ps !p1;
                  let u1' = uidmapping#find !p1 in
                  List.iter self#remove_edit (self#find12 !p1 u1');
                  uidmapping#remove !p1 u1';
                  self#add_edit (make_insert (tree2#search_node_by_uid u1'))
                end;
                ignore (uidmapping#add_unsettled !p1 n2#uid);

                if not ((!pn1)#data#eq n2#data) && not (self#mem_rel12 !p1 n2#uid) then
                  self#add_edit (make_relabel !pn1 n2);

                pn1 := (tree1#search_node_by_uid !p1)#initial_parent;
                p1 := (!pn1)#uid
              ) !ndps
          in

          let handle2 u2 =
            DEBUG_MSG "u2:%a" UID.ps u2;
            let p2 = ref u2 in
            let pn2 = ref (tree2#search_node_by_uid u2) in
            List.iter
              (fun (n1, _) -> 
                if self#mem_ins !p2 then begin
                  DEBUG_MSG "%a -> ins" UID.ps !p2;
                  self#remove_edit (self#find_ins !p2)
                end
                else if self#mem_mov2 !p2 then begin
                  DEBUG_MSG "%a -> mov2" UID.ps !p2;
                  let u2' = uidmapping#inv_find !p2 in
                  List.iter self#remove_edit (self#find12 u2' !p2);
                  uidmapping#remove u2' !p2;
                  self#add_edit (make_delete (tree1#search_node_by_uid u2'))
                end;
                ignore (uidmapping#add_unsettled n1#uid !p2);

                if not (n1#data#eq (!pn2)#data) && not (self#mem_rel12 n1#uid !p2) then
                  self#add_edit (make_relabel n1 !pn2);

                pn2 := (tree2#search_node_by_uid !p2)#initial_parent;
                p2 := (!pn2)#uid
              ) !ndps
          in

          cands01 := List.filter check1 !cands01;
          cands02 := List.filter check2 !cands02;

          match !cands01, !cands02 with
          | [uid1'], [uid2'] ->
              remove_orig();
              handle1 uid1';
              handle2 uid2'

          | [uid1'], [] ->
              remove_orig();
              handle1 uid1';
              List.iter
                (fun (n1, _) -> 
                  if not (uidmapping#mem_dom n1#uid) then begin
                    DEBUG_MSG "making del: %a" UID.ps n1#uid;
                    self#add_edit (make_delete n1)
                  end
                ) !ndps

          | [], [uid2'] ->
              remove_orig();
              handle2 uid2';
              List.iter
                (fun (_, n2) -> 
                  if not (uidmapping#mem_cod n2#uid) then begin
                    DEBUG_MSG "making ins: %a" UID.ps n2#uid;
                    self#add_edit (make_insert n2)
                  end
                ) !ndps

          | _ -> ()

        end
      ) move_region_list
    (* end of method shrink_moves_rp *)




  method shrink_moves (* shrink moves to improve SPSM *)
      (tree1 : 'tree_t)
      (tree2 : 'tree_t)
      (uidmapping : 'node_t UIDmapping.c)
      (move_region_tbl : move_region_tbl_t)
      =
    let make_subtree_copy1 =
      (tree1#make_subtree_copy : ?find_hook:('node_t -> 'node_t -> unit) -> 'node_t -> 'tree_t)
    in
    let make_subtree_copy2 = tree2#make_subtree_copy in

    let same_digest = _same_digest tree1 tree2 in

    let move_root_tbl = Hashtbl.create 0 in (* mid -> root node pair list *)

    self#iter_moves
      (function
	| Move(mid, k, (u1, i1, _), (u2, i2, _)) -> begin
            DEBUG_MSG "mid:%a %a-%a" MID.ps !mid UID.ps u1 UID.ps u2;
	    if not (self#mem_rel12 u1 u2) (* && !k <> Mpermutation *) then begin
	      let n1 = Info.get_node i1 in
	      let n2 = Info.get_node i2 in

              DEBUG_MSG "digests: %a(%s) %a(%s)" 
                UID.ps u1 n1#data#_digest_string 
                UID.ps u2 n2#data#_digest_string;

	      if same_digest n1 n2 then begin
		let gi1, gi2 = n1#gindex, n2#gindex in
		let lgi1 = (tree1#initial_leftmost n1)#gindex in
		let lgi2 = (tree2#initial_leftmost n2)#gindex in
		try
		  let add_ok = ref true in
		  let pairs =
		    List.filter
		      (fun (cand1, cand2) ->
			let cgi1, cgi2 = cand1#gindex, cand2#gindex in
			let clgi1 = (tree1#initial_leftmost cand1)#gindex in
			let clgi2 = (tree2#initial_leftmost cand2)#gindex in

			if clgi1 <= gi1 && gi1 < cgi1 && clgi2 <= gi2 && gi2 < cgi2 then
			  add_ok := false;

			let b = lgi1 <= cgi1 && cgi1 < gi1 && lgi2 <= cgi2 && cgi2 < gi2 in
			not b
		      ) (Hashtbl.find move_root_tbl !mid)
		  in
                  DEBUG_MSG "add_ok=%B" !add_ok;
		  let pairs' =
		    if !add_ok then
		      (n1, n2)::pairs
		    else
		      pairs
		  in
		  Hashtbl.replace move_root_tbl !mid pairs'

		with
		  Not_found -> 
                    DEBUG_MSG "%a --> %a-%a" MID.ps !mid
                      UID.ps n1#uid UID.ps n2#uid;

                    Hashtbl.add move_root_tbl !mid [n1, n2]
              end

            end (* if not (self#mem_rel12 u1 u2) *)
	end
	| _ -> assert false
      );

    BEGIN_DEBUG
      DEBUG_MSG "move_root_tbl:";
      Hashtbl.iter 
	(fun mid pairs ->
	  List.iter 
	    (fun (nd1, nd2) -> 
	      DEBUG_MSG "%a -> %a-%a (%a-%a)" MID.ps mid
		UID.ps nd1#uid UID.ps nd2#uid GI.ps nd1#gindex GI.ps nd2#gindex
	    ) pairs
	) move_root_tbl
    END_DEBUG;

    let rec find_stably_mapped_ancestor tree umap find nd =
      try
	let pnd = nd#initial_parent in
	let puid = pnd#uid in
	try
	  let puid' = umap puid in
	  match find puid puid' with
	  | [] | [Relabel _] -> 
	      let pnd' = tree#search_node_by_uid puid' in
	      pnd, pnd'
	  | _ -> find_stably_mapped_ancestor tree umap find pnd (* raise Not_found *)
	with
	  Not_found -> find_stably_mapped_ancestor tree umap find pnd (* raise Not_found *)
      with
	Otree.Parent_not_found _ -> raise Not_found
    in

    let contain_stably_mapped tree root umap find =
      try
	tree#fast_scan_whole_initial_subtree root
	  (fun n ->
	    try
	      let u = n#uid in
	      let u' = umap u in
	      match find u u' with
	      | [] | [Relabel _] -> raise Found
	      | _ -> ()
	    with
	      Not_found -> ()
	  );
	false
      with
	Found -> true
    in

    let full_tree_matcher t1 t2 =
      let _, m, _ = Treediff.find t1 t2 in
      List.filter
	(fun (nd1, nd2) ->
	  nd1#data#eq nd2#data
	)
	(List.map 
	   (fun (i, j) -> 
	     tree1#search_node_by_gindex (t1#get i)#gindex, 
	     tree2#search_node_by_gindex (t2#get j)#gindex
	   ) m)
    in
    let fast_tree_matcher t1 t2 =
      let matches = ref [] in
      let rec scan (nd1, nd2) =
	if nd1#data#eq nd2#data then
	  matches := (nd1, nd2)::!matches;
	let c1, c2 = nd1#children, nd2#children in
	let mk nd = nd#data#_label in
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
      scan (t1#root, t2#root);
      List.map
	(fun (n1, n2) ->
	  tree1#search_node_by_gindex n1#gindex, tree2#search_node_by_gindex n2#gindex
	) !matches
    in
    let tree_matcher t1 t2 =
(*
      BEGIN_DEBUG
	DEBUG_MSG "t1:";
	Printf.printf "%s\n" t1#to_string;
	DEBUG_MSG "t2:";
	Printf.printf "%s\n" t2#to_string
      END_DEBUG;
*)
      if t1#size > options#match_algo_threshold || t2#size > options#match_algo_threshold then
	fast_tree_matcher t1 t2
      else
	full_tree_matcher t1 t2
    in

    let label_find tree an nd =
      DEBUG_MSG "finding %s edited below %a" nd#data#label UID.ps an#uid;
      let cands = ref [] in
      let dcands = ref [] in
      let acands = ref [] in
      let lab = nd#data#_label in
      let alab = nd#data#_anonymized_label in
      tree#preorder_scan_whole_initial_subtree an
	(fun n ->
	  if n != an then
	    let lmatch = n#data#_label = lab in
	    let amatch = n#data#_anonymized_label = alab in
	    if lmatch || amatch then begin
	      let u = n#uid in

              if _same_digest tree tree nd n && nd#data#weight > 1 then begin
                DEBUG_MSG "digest match: %a-%a (weight=%d)" UID.ps n#uid UID.ps nd#uid nd#data#weight;
                dcands := n :: !dcands
              end;

              if
                self#mem_del u || self#mem_ins u || 
                self#mem_mov1 u || self#mem_mov2 u || 
                self#mem_rel1 u || self#mem_rel2 u
              then begin
	        if lmatch then
	          cands := n :: !cands;
	        if amatch then
	          acands := n :: !acands
              end;
(*
              DEBUG_MSG "dcands: [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" !dcands);
              DEBUG_MSG "cands: [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" !cands);
              DEBUG_MSG "acands: [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" !acands);
*)
            end
	);
      match !cands with
      | [] -> 
          if !acands = [] then 
            !dcands <> [], !dcands 
          else 
            false, !acands
      | _ -> false, !cands
    in (* label_find *)

    let is_crossing = UIDmapping.is_crossing in
    let is_incompatible = UIDmapping.is_incompatible tree1 tree2 in

    DEBUG_MSG "-----";

    let cand_tbl = Hashtbl.create 0 in

    Hashtbl.iter
      (fun mid pairs ->
	DEBUG_MSG "* checking move%a..." MID.ps mid;

	List.iter
	  (fun (nd1, nd2) ->

	    DEBUG_MSG "  move %a-%a:" UID.ps nd1#uid UID.ps nd2#uid;

	    let cands1, c1an = 
	      try
		let an1, an2 = find_stably_mapped_ancestor tree2 uidmapping#find self#find12 nd1 in

		if tree2#is_initial_ancestor an2 nd2 then
		  [], None

		else begin
                  BEGIN_DEBUG
                    let au1, au2 = an1#uid, an2#uid in
		    DEBUG_MSG "    stably_mapped_ancestor: %a-%a (%a-%a)" 
		      UID.ps au1 UID.ps au2 GI.ps an1#gindex GI.ps an2#gindex
                  END_DEBUG;

		  let is_extra2, rcands2 = label_find tree2 an2 nd1 in

		  DEBUG_MSG "    rcands2: [%s]%s" 
                    (Xlist.to_string (fun n -> UID.to_string n#uid) ";" rcands2)
                    (if is_extra2 then " (EXTRA)" else "");

                  let count = ref 0 in

                  let rcands2' =
                    try
                      List.filter
                        (fun n2 ->
                          if !count > 1 then
                            raise Exit;

                          let b =
                            is_extra2 ||
		            let crossing =
			      try
			        tree1#fast_scan_whole_initial_subtree an1
			          (fun n ->
			            let u = n#uid in
			            try
				      let u' = uidmapping#find u in
				      match self#find12 u u' with
				      | [] | [Relabel _] -> 
				          let n' = tree2#search_node_by_uid u' in
				          if is_crossing nd1 n2 n n' || is_incompatible nd1 n2 n n' then
				            raise Found
				      | _ -> ()
			            with
				      Not_found -> ()
			          );
			        false
			      with
			        Found -> true
		            in
                            not 
                              (crossing ||
		              tree2#is_initial_ancestor n2 nd2 || 
		              tree2#is_initial_ancestor nd2 n2 ||
		              (contain_stably_mapped tree2 n2 uidmapping#inv_find self#find21 && n2#initial_nchildren > 0))
                          in
                          if b then
                            incr count;
                          b
                        ) rcands2
                    with
                      Exit -> []
                  in

		  DEBUG_MSG "    rcands2': [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" rcands2');

		  match rcands2' with
		  | [] -> [], None
		  | [n2] -> begin
		      try
			let t1 = make_subtree_copy1 nd1 in
			List.map
			  (fun r2 ->
			    if r2 == nd2 then
			      []
			    else
			      let t2 = make_subtree_copy2 r2 in
			      tree_matcher t1 t2
			  ) rcands2',
			Some (an1, an2)
		      with
			Invalid_argument _ -> [], None
		  end
		  | _ -> [], None
                end
	      with
		Not_found -> [], None

	    in (* cands1, c1an *)
	    
	    begin
	      match c1an with
	      | None -> ()
	      | Some (an1, an2) ->
		  try
		    let l = Hashtbl.find cand_tbl (an1, an2) in
		    Hashtbl.replace cand_tbl (an1, an2) (cands1 @ l)
		  with
		    Not_found -> Hashtbl.add cand_tbl (an1, an2) cands1
	    end;

	    let cands2, c2an = 
	      try
		let an2, an1 = find_stably_mapped_ancestor tree1 uidmapping#inv_find self#find21 nd2 in

		if tree1#is_initial_ancestor an1 nd1 then
		  [], None

		else begin
                  BEGIN_DEBUG
                    let au1, au2 = an1#uid, an2#uid in
		    DEBUG_MSG "    stably_mapped_ancestor: %a-%a (%a-%a)" 
		      UID.ps au1 UID.ps au2 GI.ps an1#gindex GI.ps an2#gindex
                  END_DEBUG;

		  let is_extra1, rcands1 = label_find tree1 an1 nd2 in

		  DEBUG_MSG "    rcands1: [%s]%s" 
                    (Xlist.to_string (fun n -> UID.to_string n#uid) ";" rcands1)
                    (if is_extra1 then " (EXTRA)" else "");

                  let count = ref 0 in

                  let rcands1' =
                    try
                      List.filter
                        (fun n1 ->
                          if !count > 1 then
                            raise Exit;

                          let b =
                            is_extra1 ||
		            let crossing =
			      try
			        tree2#fast_scan_whole_initial_subtree an2
			          (fun n ->
			            let u = n#uid in
			            try
				      let u' = uidmapping#inv_find u in
				      match self#find12 u' u with
				      | [] | [Relabel _] -> 
				          let n' = tree1#search_node_by_uid u' in
				          if is_crossing n1 nd2 n' n || is_incompatible n1 nd2 n' n then
				            raise Found
				      | _ -> ()
			            with
				      Not_found -> ()
			          );
			        false
			      with
			        Found -> true
		            in
                            not
			      (crossing ||
			      tree1#is_initial_ancestor n1 nd1 || 
			      tree1#is_initial_ancestor nd1 n1 ||
			      (contain_stably_mapped tree1 n1 uidmapping#find self#find12 && n1#initial_nchildren > 0))
                          in

                          if b then
                            incr count;
                          b
                        ) rcands1
                    with
                      Exit -> []
                  in

		  DEBUG_MSG "    rcands1': [%s]" (Xlist.to_string (fun n -> UID.to_string n#uid) ";" rcands1');

		  match rcands1' with
		  | [] -> [], None
		  | [n1] -> begin
		      try
			let t2 = make_subtree_copy2 nd2 in
			List.map
			  (fun r1 ->
			    if r1 == nd1 then
			      []
			    else
			      let t1 = make_subtree_copy1 r1 in
			      tree_matcher t1 t2
			  ) rcands1',
			Some (an1, an2)
		      with
			Invalid_argument _ -> [], None
		  end
		  | _ -> [], None
                end
	      with
		Not_found -> [], None
	    in (* cands2, c2an *)

	    begin
	      match c2an with
	      | None -> ()
	      | Some (an1, an2) ->
		  try
		    let l = Hashtbl.find cand_tbl (an1, an2) in
		    Hashtbl.replace cand_tbl (an1, an2) (cands2 @ l)
		  with
		    Not_found -> Hashtbl.add cand_tbl (an1, an2) cands2
	    end

	  ) pairs

      ) move_root_tbl;

    DEBUG_MSG "-----";


    (* reducing candidates *)
    Hashtbl.iter
      (fun (an1, an2) cands ->

	DEBUG_MSG "stably mapped: %a-%a:" UID.ps an1#uid UID.ps an2#uid;

	let stable_matches = ref [] in

	tree1#fast_scan_whole_initial_subtree an1
	  (fun n1 ->
	    if n1 != an1 then
	      try
		let u1 = n1#uid in
		let u2 = uidmapping#find u1 in
		let n2 = tree2#search_node_by_uid u2 in
		if tree2#initial_subtree_mem an2 n2 then begin
		  match self#find12 u1 u2 with
		  | [] | [Relabel _] -> stable_matches := (n1, n2) :: !stable_matches
		  | _ -> ()
		end
	      with
		Not_found -> ()
	  );

	BEGIN_DEBUG
	  DEBUG_MSG "stable matches: [%s]" 
	    (Xlist.to_string 
	       (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" !stable_matches)
	END_DEBUG;


	let is_directly_connected n1 n2 =
	  n1 == n2#initial_parent ||
	  n2 == n1#initial_parent
	in
	let is_directly_connected_pair (n11, n12) (n21, n22) =
	  is_directly_connected n11 n21 && is_directly_connected n12 n22
	in

	let get_largest_connected_subset pairs =
	  let tbl = Hashtbl.create 0 in
	  List.iter
	    (fun (nd1, nd2) ->
	      let set = Xset.create 0 in
	      List.iter 
		(fun (n1, n2) -> 
		  if nd1 != n1 && nd2 != n2 then
		    if is_directly_connected_pair (nd1, nd2) (n1, n2) then
		      Xset.add set (n1, n2)
		) pairs;
	      Hashtbl.add tbl (nd1, nd2) set
	    ) pairs;
	  
	  let trace_pair pair =
	    let set = Xset.create 0 in
	    let rec trace pair =
	      let len_before = Xset.length set in
	      Xset.add set pair;
	      if Xset.length set <> len_before then
		Xset.iter trace (Hashtbl.find tbl pair)
	    in
	    trace pair;
	    set
	  in
	  let connected_subsets =
	    List.fold_left
	      (fun l pair ->
		let s = trace_pair pair in
		if List.for_all (fun x -> not (Xset.equals s x)) l then
		  s :: l
		else
		  l
	      ) [] pairs
	  in

	  let connected_cands = List.map Xset.to_list connected_subsets in

	  DEBUG_MSG "[%s] -> %s"
	    (Xlist.to_string 
	       (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" pairs)
	    (Xlist.to_string
	       (fun cand ->
		 sprintf "[%s]"
		   (Xlist.to_string
		      (fun (n1, n2) ->
			sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid
		      ) ";" cand)
	       ) ";" connected_cands);
	  
	  connected_cands
	in

	let directly_connected_to_stable_match (n1, n2) =
	  let stable = (an1, an2)::!stable_matches in
	  List.exists (is_directly_connected_pair (n1, n2)) stable
	in
        let stable_matches1, stable_matches2 = List.split !stable_matches in
        let is_invalid_cand cand =
          let b =
            List.exists (fun (n1, _) -> List.memq n1 stable_matches1) cand ||
            List.exists (fun (_, n2) -> List.memq n2 stable_matches2) cand
          in
	  DEBUG_MSG "[%s] -> %B"
	    (Xlist.to_string
               (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand) b;
          b
        in

	let not_contained_in_move cand =
	  let ns1, ns2 = List.split cand in
	  let sort ns =
	    List.fast_sort (fun n1 n2 -> Stdlib.compare n2#gindex n1#gindex) ns
	  in
	  let sorted_ns1 = sort ns1 in
	  let sorted_ns2 = sort ns2 in
	  let b =
	    match sorted_ns1, sorted_ns2 with
	    | [], [] -> false
	    | n1::_, n2::_ -> begin
		let g1, g2 = n1#gindex, n2#gindex in
		try
		  Hashtbl.iter
		    (fun mid (lgi1, gi1, lgi2, gi2) ->
		      if (lgi1 < g1 && g1 < gi1) || (lgi2 < g2 && g2 < gi2) then begin
			DEBUG_MSG "  contained in mid:%a" MID.ps mid;
			raise Found
		      end
		    ) move_region_tbl;
		  true
		with
		  Found -> false
	    end
	    | _ -> assert false
	  in
	  DEBUG_MSG "[%s] -> %B"
	    (Xlist.to_string 
	       (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand) b;
	  b
	in

	let filter_cands cs =
	  List.filter 
	    (fun cand ->
              not (is_invalid_cand cand) &&
	      (List.exists directly_connected_to_stable_match cand ||
	      List.length cand > 1 ||
	      not_contained_in_move cand(* ||
              match cand with
              | [n1, n2] -> n1#data#equals n2#data
              | _ -> false*)
              )
            ) cs
	in

	let cands = (* extract connected pairs *)
	  List.flatten (List.map get_largest_connected_subset cands)
	in

        let filtered_cands = 
          List.map
            (fun cand ->
              List.fast_sort 
		(fun (n0, _) (n1, _) -> Stdlib.compare n1#gindex n0#gindex)
                cand
            ) (filter_cands cands)
        in

	BEGIN_DEBUG
	  List.iter
	    (fun cand ->
	      DEBUG_MSG "filtered cand: [%s]"
		(Xlist.to_string 
		   (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand)
	    ) filtered_cands
	END_DEBUG;

	let cands = 
	  List.map 
	    (fun cand -> 
              match cand with
              | (r1, r2)::_ ->
                  if same_digest r1 r2 && r1#data#weight > 1 then
                    if
		      List.for_all 
		        (fun (sn1, sn2) ->
                          (tree1#initial_subtree_mem r1 sn1 || tree2#initial_subtree_mem r2 sn2) ||
			  (not (UIDmapping.is_incompatible tree1 tree2 sn1 sn2 r1 r2) && 
			   not (UIDmapping.is_crossing sn1 sn2 r1 r2))
	                  ) !stable_matches
                    then
                      cand
                    else
                      []
                  else
		    List.filter 
		      (fun (n1, n2) -> 
		        List.for_all 
		          (fun (sn1, sn2) -> 
			    (not (UIDmapping.is_incompatible tree1 tree2 sn1 sn2 n1 n2) && 
			     not (UIDmapping.is_crossing sn1 sn2 n1 n2))
	                  ) !stable_matches
		      ) cand
              | [] -> []
	    ) filtered_cands
	in
	let cands = filter_cands cands in

(*
	let get_top cand = match cand with (n, _)::_ -> n | _ -> assert false in
	let cands =
	  List.fast_sort
	    (fun cand0 cand1 ->
	      let len0 = List.length cand0 in
	      let len1 = List.length cand1 in
	      let top0 = get_top cand0 in
	      let top1 = get_top cand1 in
	      if len0 = len1 then
		Stdlib.compare top1#gindex top0#gindex
	      else
		Stdlib.compare len1 len0
	    ) cands
	in
*)
	BEGIN_DEBUG
	  List.iter
	    (fun cand ->
	      DEBUG_MSG "cand: [%s]"
		(Xlist.to_string 
		   (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand)
	    ) cands
	END_DEBUG;

	match cands with
	| [] -> Hashtbl.remove cand_tbl (an1, an2)
	| [c] -> Hashtbl.replace cand_tbl (an1, an2) [c]
	| _ ->
	    let ctbl = Hashtbl.create 0 in
	    let get_top_pair_len cand = 
	      match cand with 
	      | (n1, n2)::_ ->
		  Hashtbl.add ctbl (n1, n2) cand;
		  n1, n2, (List.length cand)
	      | _ -> assert false 
	    in
	    let pair_weight_list = List.map get_top_pair_len cands in
	    let compat, _ = 
	      UIDmapping.select_compatible_and_not_crossing_pairs tree1 tree2 pair_weight_list
	    in
	    let selected = List.map (fun (n1, n2, _) -> Hashtbl.find ctbl (n1, n2)) compat in
	    Hashtbl.replace cand_tbl (an1, an2) selected
(*
	| c::_ -> (* Crossing candidates are not yet checked. We can however select one safely *)
	    Hashtbl.replace cand_tbl (an1, an2) [c]
*)
      ) cand_tbl;



    (* *)
    DEBUG_MSG "-----";

    let find_ed1 uid =
      try
        [self#find_del uid], [], []
      with
        Not_found ->
          let eds = self#find1 uid in
          let m, ad =
            match eds with
            | [Move(_, _, (u1, i1, _), (u2, i2, _))]
            | [Relabel(_, (u1, i1, _), (u2, i2, _))] 
            | [Move(_, _, (u1, i1, _), (u2, i2, _));Relabel _]
            | [Relabel _;Move(_, _, (u1, i1, _), (u2, i2, _))] -> (u1, u2), make_insert (Info.get_node i2)
            | [] -> raise Not_found
            | _ -> assert false
          in
          eds, [m], [ad]
    in
    let find_ed2 uid =
      try
        [self#find_ins uid], [], []
      with
        Not_found ->
          let eds = self#find2 uid in
          let m, ad =
            match eds with
            | [Move(_, _, (u1, i1, _), (u2, i2, _))]
            | [Relabel(_, (u1, i1, _), (u2, i2, _))] 
            | [Move(_, _, (u1, i1, _), (u2, i2, _));Relabel _]
            | [Relabel _;Move(_, _, (u1, i1, _), (u2, i2, _))] -> (u1, u2), make_delete (Info.get_node i1)
            | [] -> raise Not_found
            | _ -> assert false
          in
          eds, [m], [ad]
    in

    let used_matches = ref [] in

    Hashtbl.iter
      (fun (an1, an2) cands ->

	let cands' = 
	  List.map 
	    (fun cand -> 
	      Xlist.subtract cand (Xlist.intersection cand !used_matches)
	    ) cands 
	in

	BEGIN_DEBUG
	  let an1an2 = sprintf "%a-%a" UID.ps an1#uid UID.ps an2#uid in
	  DEBUG_MSG "cands (%s):" an1an2;
	  List.iter
	    (fun cand ->
	      DEBUG_MSG "[%s]" 
		(Xlist.to_string (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand)
	    ) cands;
	  DEBUG_MSG "cands' (%s):" an1an2;
	  List.iter
	    (fun cand ->
	      DEBUG_MSG "[%s]" 
		(Xlist.to_string (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand)
	    ) cands'
	END_DEBUG;

	List.iter
	  (fun cand ->

	    DEBUG_MSG "  cand: [%s]"
	      (Xlist.to_string (fun (n1, n2) -> sprintf "%a-%a" UID.ps n1#uid UID.ps n2#uid) ";" cand);

	    List.iter
	      (fun (nd1, nd2) ->
		let uid1, uid2 = nd1#uid, nd2#uid in

		try
		  let to_be_removed, umap_to_be_removed, to_be_added =
		    try
		      let uid1' = uidmapping#find uid1 in
		      let m = uid1, uid1' in
		      let es = self#find12 uid1 uid1' in
		      let ins = make_insert (tree2#search_node_by_uid uid1') in
		      try
                        let rms, ms, ad = find_ed2 uid2 in
			rms @ es, m :: ms, ins :: ad
		      with
			Not_found -> raise Abort
(*
  try
  let uid2' = uidmapping#inv_find uid2 in
  mov::(self#find12 uid2' uid2), [uid2', uid2; m], [make_delete (tree1#search_node_by_uid uid2');ins]
  with
  Not_found -> [mov], [m], [ins]
 *)
		    with 
		      Not_found ->
			try
			  let uid2' = uidmapping#inv_find uid2 in
			  let m = uid2', uid2 in
			  let es = self#find12 uid2' uid2 in
			  let del = make_delete (tree1#search_node_by_uid uid2') in
			  try
                            let rms, ms, ad = find_ed1 uid1 in
			    rms @ es, m :: ms, del :: ad
			  with
			    Not_found -> raise Abort
(*
  try
  let uid1' = uidmapping#find uid1 in
  mov::(self#find12 uid1 uid1'), [uid1, uid1'; m], [make_insert (tree2#search_node_by_uid uid1');del]
  with
  Not_found -> [mov], [m], [del]
 *)
				
			with
			  Not_found -> 
			    let dels = try [self#find_del uid1] with Not_found -> [] in
			    let inss = try [self#find_ins uid2] with Not_found -> [] in
			    dels @ inss, [], []
		  in

		  List.iter self#remove_edit to_be_removed;
		  List.iter 
		    (fun (u1, u2) ->
		      DEBUG_MSG "removing %a-%a" UID.ps u1 UID.ps u2;

		      uidmapping#remove u1 u2;

		      assert (not (uidmapping#mem_cod u2));
			
		    ) umap_to_be_removed;
		  List.iter self#add_edit to_be_added;

		  DEBUG_MSG "adding %a-%a" UID.ps uid1 UID.ps uid2;

		  ignore (uidmapping#add_unsettled uid1 uid2);
		  used_matches := (nd1, nd2) :: !used_matches
		with
		  Abort -> ()
	      ) cand

	  ) cands'

      ) cand_tbl;

    Xprint.verbose options#verbose_flag "%d matches added by move shrinkage" (List.length !used_matches)
  
    (* end of method shrink_moves *) 

  method is_crossing_with_untouched ?(mask=[]) (uidmapping : 'node_t UIDmapping.c) nd1 nd2 =
    DEBUG_MSG "%a-%a" UID.ps nd1#uid UID.ps nd2#uid;
    try
      uidmapping#iter_crossing_or_incompatible_mapping nd1 nd2
	(fun u1 u2 ->
(*
          DEBUG_MSG "checking %a-%a" UID.ps u1 UID.ps u2;
*)
	  if not (List.mem (u1, u2) mask) && not (self#mem_mov12 u1 u2) then
	    let n1 = uidmapping#search_node_by_uid1 u1 in
	    let n2 = uidmapping#search_node_by_uid2 u2 in
	    if (not (is_ghost_node n1)) && (not (is_ghost_node n2)) then begin
              DEBUG_MSG "%a-%a is crossing with %a-%a" UID.ps nd1#uid UID.ps nd2#uid UID.ps u1 UID.ps u2;
	      raise Exit
            end
	);
      false
    with
      Exit -> true

  method dump_delta
      ?(extra_ns_decls=([] : (string * string) list))
      (tree1 : 'tree_t)
      (tree2 : 'tree_t)
      (uidmapping : 'node_t UIDmapping.c)
      (edits_copy : 'edits)
      (fname : string)
      =
    ()

end (* of class Edit_base.seq_base *)
