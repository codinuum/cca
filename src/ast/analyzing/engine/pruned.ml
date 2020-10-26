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
(* pruned.ml *)


module UID = Otreediff.UID
module GI = Otreediff.GIndex
module Otree = Otreediff.Otree


let sprintf = Printf.sprintf


(*** pruned nodes ***)

type kind = 
  | Isomorphic 
  | Migratory (* relative move *) 
  | Other

type gindex = GI.t

type 'node_t t = 
    kind ref 
      * UID.t * (gindex (* leftmost *) * gindex) (* for old tree *)
      * UID.t * (gindex (* leftmost *) * gindex) (* for new tree *)
      * ('node_t Info.t) (* for old tree *)
      * ('node_t Info.t) (* for new tree *)

let kind_to_string = function
  | Isomorphic -> "ISOMORPHIC" 
  | Migratory  -> "MIGRATORY" 
  | Other      -> "OTHER"

let to_string (kind, uid1, (lgi1, gi1), uid2, (lgi2, gi2), info1, info2) = 
  let s = kind_to_string !kind in
  sprintf "[PRUNED(%s)]: (%s:%d-%d)%s -> (%s:%d-%d)%s" s
    (UID.to_string uid1) lgi1 gi1 (Info.to_string info1) 
    (UID.to_string uid2) lgi2 gi2 (Info.to_string info2)

let to_string_short (kind, uid1, (lgi1, gi1), uid2, _, _, _) =
  sprintf "<%s-%s(%d)>" (UID.to_string uid1) (UID.to_string uid2) (gi1 - lgi1 + 1)

let is_single (_, _, (lgi1, gi1), _, _, _, _) = lgi1 = gi1

let make_isomorphic uid1 lgi_gi1 uid2 lgi_gi2 info1 info2 = 
  (ref Isomorphic, uid1, lgi_gi1, uid2, lgi_gi2, info1, info2)
let make_migratory uid1 lgi_gi1 uid2 lgi_gi2 info1 info2 = 
  (ref Migratory, uid1, lgi_gi1, uid2, lgi_gi2, info1, info2)
let make_other uid1 lgi_gi1 uid2 lgi_gi2 info1 info2 = 
  (ref Other, uid1, lgi_gi1, uid2, lgi_gi2, info1, info2)


class ['node_t, 'tree_t] nodes = object (self)
  val mutable list = ([] : 'node_t t list)

  method add_pruned pruned = 
    DEBUG_MSG "adding %s" (to_string_short pruned);
    list <- list @ [pruned]


  method add_pruned_nodes pruned_nodes =
    DEBUG_MSG "adding [%s]" (Xlist.to_string to_string_short "," pruned_nodes);
    list <- list @ pruned_nodes

  val mutable aborted1 = ([] : 'node_t list)
  method add_aborted1 nd = aborted1 <- nd::aborted1
  method add_abortedl1 nds = aborted1 <- nds @ aborted1
  method aborted1 = aborted1

  val mutable aborted2 = ([] : 'node_t list)
  method add_aborted2 nd = aborted2 <- nd::aborted2
  method add_abortedl2 nds = aborted2 <- nds @ aborted2
  method aborted2 = aborted2

      (* iso which contains aborted deletes or inserts *)
  val mutable para_iso1 = ([] : 'node_t list)
  method add_para_iso1 nds = para_iso1 <- nds @ para_iso1
  method para_iso1 = para_iso1

  val mutable para_iso2 = ([] : 'node_t list)
  method add_para_iso2 nds = para_iso2 <- nds @ para_iso2
  method para_iso2 = para_iso2

  method iter f = List.iter f list
  method find p = List.find p list
  method exists p = List.exists p list

  method get_kind u1 u2 =
    let s, _, _, _, _, _, _ =
      self#find
	(fun (kind, uid1, _, uid2, _, _, _) -> u1 = uid1 && u2 = uid2)
    in
    !s

  method set_kind uid1 uid2 kind = 
    self#iter
      (fun (s, u1, _, u2, _, _, _) -> 
	if u1 = uid1 && u2 = uid2 then s := kind)

(* does pruned subtree (old) contain node_data? *)
  method mem1 (tree1 : 'tree_t) (nd : 'node_t) = 
    DEBUG_MSG "is_whole=%B" tree1#is_whole;
    let uid = nd#uid in
    self#exists
      (function 
	  (kind, uid', (lgi, gi), _, _, info', _) ->
	    match !kind with
	      Other -> uid' = uid
	    | _ ->
		let b2 = 
		  try
		    let gidx = nd#gindex in
		    lgi <= gidx && gidx <= gi
		  with Not_found -> false
		in
		b2
      )

(* does pruned subtree (new) contain node_data? *)
  method mem2 (tree2 : 'tree_t) (nd : 'node_t) = 
    DEBUG_MSG "is_whole=%B" tree2#is_whole;
    let uid = nd#uid in
    self#exists
      (function 
	  (kind, _, _, uid', (lgi, gi), _, info') ->
	    match !kind with
	      Other -> uid' = uid
	    | _ -> 
		let b2 = 
		  try
		    let gidx = nd#gindex in
		    lgi <= gidx && gidx <= gi
		  with Not_found -> false
		in
		b2
      )


  method to_string = sprintf 
      "%d pruned node(s):\n%s" (List.length list) 
      (Xlist.to_string 
	 (fun p -> sprintf "%s" (to_string p)) 
	 "\n" list)

end (* of class Pruned.nodes *)
