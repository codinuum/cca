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
(* editop.ml *)


module UID = Otreediff.UID
module MID = Moveid

let sprintf = Printf.sprintf


type move_id = Moveid.t

type move_kind = 
  | Mnormal 
  | Mpermutation
  | Modd

let move_id_to_string = Moveid.to_string

let move_kind_to_string = function 
  | Mnormal      -> "NORMAL"
  | Mpermutation -> "PERMUTATION"
  | Modd         -> "ODD"

type 'node_t t =
  | Delete of bool * (* whole subtree flag *)
	UID.t * ('node_t Info.t) *
	('node_t Info.t) list ref (* excluded *)

  | Insert of bool * (* whole subtree flag *)
	UID.t * ('node_t Info.t) *
	('node_t Info.t) list ref (* excluded *)

  | Relabel of bool ref * (* is movrel *)
	(UID.t * ('node_t Info.t)
	   * ('node_t Info.t) list ref (* excluded *)
	) * 
	(UID.t * ('node_t Info.t)
	   * ('node_t Info.t) list ref (* excluded *)
	)
  | Move of move_id ref * 
	move_kind ref *
	(UID.t * ('node_t Info.t)
	   * ('node_t Info.t) list ref (* excluded *)
	) * 
	(UID.t * ('node_t Info.t)
	   * ('node_t Info.t) list ref (* excluded *)
	)

type tag =
  | Tdel
  | Tins
  | Trel
  | Tmov

let tag_to_string = function
  | Tdel -> "DEL"
  | Tins -> "INS"
  | Trel -> "REL"
  | Tmov -> "MOV"

let hash = function
  | Delete(whole, uid, info, excluded) -> (* Hashtbl.hash *) (Tdel, uid, UID.dummy)
  | Insert(whole, uid, info, excluded) -> (* Hashtbl.hash *) (Tins, UID.dummy, uid)
  | Relabel(_, (uid1, info1, excluded1), (uid2, info2, excluded2))      -> (* Hashtbl.hash *) (Trel, uid1, uid2)
  | Move(mid, kind, (uid1, info1, excluded1), (uid2, info2, excluded2)) -> (* Hashtbl.hash *) (Tmov, uid1, uid2)


let to_string = function
  | Delete(whole, uid, info, excluded) -> 
      sprintf "[DELETE%s]: %s%s" 
	(if whole then "(W)" else "")
	(Info.to_string info) (Info.excluded_to_string !excluded)

  | Insert(whole, uid, info, excluded) -> 
      sprintf "[INSERT%s]: %s%s" 
	(if whole then "(W)" else "")
	(Info.to_string info) (Info.excluded_to_string !excluded)

  | Relabel(movrel, (uid1, info1, excluded1), (uid2, info2, excluded2)) -> 
      sprintf "[RELABEL%s]: %s%s -> %s%s"
	(if !movrel then "(M)" else "")
	(Info.to_string info1) (Info.excluded_to_string !excluded1)
	(Info.to_string info2) (Info.excluded_to_string !excluded2)

  | Move(mid, kind, (uid1, info1, excluded1), (uid2, info2, excluded2)) -> 
      sprintf "[MOVE(%a:%s)]: %s%s -> %s%s"
	MID.ps !mid (move_kind_to_string !kind)
	(Info.to_string info1) (Info.excluded_to_string !excluded1)
	(Info.to_string info2) (Info.excluded_to_string !excluded2)

let to_string_gid = function
  | Delete(whole, uid, info, excluded) -> 
      sprintf "[DELETE%s]: %s" 
	(if whole then "(W)" else "")
	(Info.to_string_gid info)

  | Insert(whole, uid, info, excluded) -> 
      sprintf "[INSERT%s]: %s" 
	(if whole then "(W)" else "")
	(Info.to_string_gid info)

  | Relabel(movrel, (uid1, info1, excluded1), (uid2, info2, excluded2)) -> 
      sprintf "[RELABEL%s]: %s -> %s"
	(if !movrel then "(M)" else "")
	(Info.to_string_gid info1)
	(Info.to_string_gid info2)

  | Move(mid, kind, (uid1, info1, excluded1), (uid2, info2, excluded2)) -> 
      sprintf "[MOVE(%s)]: %s -> %s"
	(move_kind_to_string !kind)
	(Info.to_string_gid info1)
	(Info.to_string_gid info2)


let is_whole nd = nd#is_collapsed

let make_delete nd = Delete(is_whole nd, nd#uid, Info.make nd, ref [])

let make_insert nd = Insert(is_whole nd, nd#uid, Info.make nd, ref [])

let make_relabel nd1 nd2 = 
  Relabel(ref false, (nd1#uid, Info.make nd1, ref []), (nd2#uid, Info.make nd2, ref []))

let _make_move mid kind (uid1, info1) (uid2, info2) = 
  Move(ref mid, ref kind, (uid1, info1, ref []), (uid2, info2, ref []))

let make_move mid (uid1, info1) (uid2, info2) = 
  _make_move mid Mnormal (uid1, info1) (uid2, info2)

let make_move_permutation mid (uid1, info1) (uid2, info2) = 
  _make_move mid Mpermutation (uid1, info1) (uid2, info2)

let make_move_odd mid (uid1, info1) (uid2, info2) = 
  _make_move mid Modd (uid1, info1) (uid2, info2)
