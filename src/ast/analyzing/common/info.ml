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
(* info.ml *)


module UID = Otreediff.UID
module GI = Otreediff.GIndex
module Otree = Otreediff.Otree


let sprintf = Printf.sprintf


exception Segment

exception Dummy_info


type 'node_t t = { i_node         : 'node_t option;
	           i_loc          : Loc.t;
	           mutable i_size : int;
	         }

let make nd = { i_node=Some nd;
                i_loc=nd#data#src_loc;
                i_size=1;
              }

let get_node info =
  match info.i_node with
  | None    -> raise Dummy_info
  | Some nd -> nd

let get_desc info = 
  match info.i_node with
  | None -> "dummy"
  | Some nd ->
      sprintf "%s[%d]%s" 
	nd#data#label nd#initial_pos (if nd#is_collapsed then "$" else "")

let get_gindex info =
  match info.i_node with
  | None -> GI.unknown
  | Some nd -> nd#gindex

let get_gid info =
  match info.i_node with
  | None -> GI.unknown
  | Some nd -> 
      if nd#data#gid > 0 then nd#data#gid else nd#gindex

let get_uid info =
  match info.i_node with
  | None -> UID.unknown
  | Some nd -> nd#uid

let get_loc info = info.i_loc
let get_size info = info.i_size
let set_size info sz = info.i_size <- sz

let is_included info0 info =
  (get_loc info).Loc.start_offset <= (get_loc info0).Loc.start_offset &&
  (get_loc info0).Loc.end_offset <= (get_loc info).Loc.end_offset

let of_region ?(fname="") (st, ed) = { i_node=None; 
			               i_loc=Loc.make ~fname st ed 0 0 0 0;
			               i_size = -1;
			 }

let to_string ({ i_node=nd_op; i_loc=loc; i_size=sz; } as info) =
  match nd_op with
  | None -> sprintf "<none>(%s)" (Loc.to_string loc)
  | Some nd ->
      sprintf "(%a:%a)%s[%d]%s(%s)%d" 
	UID.ps nd#uid GI.ps (get_gid info)
	nd#data#label 
	nd#initial_pos 
	(if nd#is_collapsed then "$" else "") 
	(Loc.to_string loc) sz

let to_string_gid ({ i_node=nd_op; i_loc=loc; i_size=sz; } as info) =
  match nd_op with
  | None -> sprintf "<dummy>(%s)" (Loc.to_string loc)
  | Some nd ->
      sprintf "(%a)%s[%d]%s(%s)%d" 
	GI.ps (get_gid info)
	nd#data#label 
	nd#initial_pos 
	(if nd#is_collapsed then "$" else "") 
	(Loc.to_string loc) sz

let to_region info =
  let loc = get_loc info in
  loc.Loc.start_offset, loc.Loc.end_offset

let infos_to_string infos =
  (Xlist.to_string 
     (fun info -> 
       let s, e = to_region info in 
       sprintf "%d-%d" s e) 
     ", " infos)
    
let resolve_inclusion_of_infos infos =

  DEBUG_MSG "%s" (infos_to_string infos);

  let filtered =
    List.filter
      (fun info ->
	not 
	  (List.exists 
	     (fun info' -> is_included info' info && info' <> info) 
	     infos)
      ) infos
  in
  DEBUG_MSG "filtered: %s" (infos_to_string filtered);

  filtered
    
let sort_infos infos = (* assumes disjoint *)

  DEBUG_MSG "input: %s" (infos_to_string infos);

  let infos = resolve_inclusion_of_infos infos in

  DEBUG_MSG "resolved: %s" (infos_to_string infos);

  let infos = 
    let mem i l = List.exists (fun j -> get_loc j = get_loc i) l in
    List.fold_left (fun l x -> if mem x l then l else x::l) [] infos 
  in
  let cmp info1 info2 =
    let m1 =
      float_of_int((get_loc info1).Loc.start_offset 
		     + (get_loc info1).Loc.end_offset) 
	/. 2.0
    in
    let m2 =
      float_of_int((get_loc info2).Loc.start_offset 
		     + (get_loc info2).Loc.end_offset) 
	/. 2.0
    in
    compare m1 m2
  in
  let sorted = List.fast_sort (fun info1 info2 -> cmp info1 info2) infos in

  DEBUG_MSG "sorted: %s" 
    (Xlist.to_string 
       (fun info -> 
	 let s, e = to_region info in 
	 sprintf "%d-%d" s e) 
       ", " sorted);
  
  sorted

let rec fuse_locs = function 
  | [] -> []
  | loc0::loc1::rest -> 
      let eo0 = loc0.Loc.end_offset in
      let so1 = loc1.Loc.start_offset in
      if eo0 + 1 = so1
      then fuse_locs((Loc._merge loc0 loc1)::rest)
      else loc0::(fuse_locs(loc1::rest))
  | locs -> locs
	

let is_contained info info0 = 
  Loc.is_contained (get_loc info) (get_loc info0)

exception Contained

let is_contained_some info infos =
  try 
    List.iter (fun i -> if is_contained info i then raise Contained) infos;
    false
  with Contained -> true

exception Phantom of int * int

let segment (info, infos) = (* assume sorted infos *)

  BEGIN_DEBUG
    DEBUG_MSG "info=%s" (to_string info);
    DEBUG_MSG "infos:\n\t%s" (Xlist.to_string to_string "\n\t" infos)
  END_DEBUG;

  let loc = get_loc info in

  let _locs = 
    Xlist.filter_map 
      (fun i -> 
        try
          Some (Loc.meet loc (get_loc i))
        with
          Failure _ -> None
      ) infos 
  in

  let locs = fuse_locs _locs in
  let start_offset = loc.Loc.start_offset in
  let end_offset = loc.Loc.end_offset in

  BEGIN_DEBUG
    DEBUG_MSG "locs:\n\t%s" (Xlist.to_string Loc.to_string "\n\t" locs);
    DEBUG_MSG "start offset: %d, end offset: %d" start_offset end_offset
  END_DEBUG;

  let l = ref [start_offset] in

  try
    List.iter 
      (fun loc0 ->
	let so = loc0.Loc.start_offset in
	let eo = loc0.Loc.end_offset in
	if so = start_offset && eo = end_offset then (* treat as phantom *)
	  raise (Phantom (eo, so)) (* swap offsets to denote phantom *)
	else
	  if (eo <> 0) then
	    if not (so = start_offset && eo = end_offset) then
	      if so = start_offset then 
		l := [eo + 1]
	      else if eo = end_offset then 
		l := (so - 1)::!l
	      else 
		l := (eo + 1)::(so - 1)::!l
	    else 
	      raise Segment
      ) locs;

    if ((List.length !l) mod 2) <> 0 then 
      l := end_offset::!l;

    let rec pair = function
      | [] -> []
      | a::b::r -> (a, b)::(pair r)
      | _ -> []
    in

    let res = pair (List.rev !l) in

    DEBUG_MSG "result: %s" (Xlist.to_string (fun (s, e) -> sprintf "%d-%d" s e) ", " res);

    res

  with 
    Phantom(x, y) -> [x, y]
(* end of func segment *)


let excluded_to_string excluded =
  if List.length excluded <> 0 then
    sprintf " excluded [%s]"
      (Xlist.to_string (fun info -> to_string info) ";" excluded)
  else ""

let includes_to_string includes =
  if List.length includes <> 0 then
    sprintf " includes [%s]"
      (Xlist.to_string (fun info -> to_string info) ";" includes)
  else ""

let excluded_to_nodes x = List.map get_node x

