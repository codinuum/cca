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
(* proximity *)


module UID = Otreediff.UID

type confidence = Chigh | Clow

class ['node_t] node_proximity = object
  val mutable confidence = Chigh

  val mutable primary_prox = 0
  val mutable secondary_prox = 0

  val mutable primary_pivot = (None : ('node_t * 'node_t) option)
  val mutable secondary_pivot = (None : ('node_t * 'node_t) option)

  method lower_confidence = confidence <- Clow

  method low_confidence = confidence = Clow
  method high_confidence = confidence = Chigh

  method primary_prox   = primary_prox
  method secondary_prox = secondary_prox
  method set_primary_prox p   = primary_prox <- p
  method set_secondary_prox p = secondary_prox <- p

  method primary_pivot = 
    match primary_pivot with
    | None -> raise Not_found
    | Some piv -> piv

  method secondary_pivot = 
    match secondary_pivot with
    | None -> raise Not_found
    | Some piv -> piv

  method set_primary_pivot p   = primary_pivot <- Some p
  method set_secondary_pivot p = secondary_pivot <- Some p

end (* of class node_proximity *)


let null_uid_tbl = (Hashtbl.create 0 : (UID.t, UID.t) Hashtbl.t)
(*
let get_proximity ?(extra=null_uid_tbl) tree1 tree2 (uidmapping : Spec.uidmapping_t) nd1 nd2 =
  let ancs1 = Array.of_list nd1#initial_ancestor_nodes in
  let ancs2 = Array.of_list nd2#initial_ancestor_nodes in
  let lai1 = (Array.length ancs1) - 1 in
  let lai2 = (Array.length ancs2) - 1 in

  let prox = new node_proximity in

  let _cands = ref [] in

  for i = lai1 downto 0 do
    let ui = ancs1.(i)#uid in
    try
      let u, high_conf = 
	try
	  let x = uidmapping#find ui in
	  x, true
	with 
	  Not_found ->
	    let x = Hashtbl.find extra ui in
	    DEBUG_MSG "(%a,%a): pivot derived from extra map" 
	      UID.ps nd1#uid UID.ps nd2#uid;
	    x, false
      in
      for j = lai2 downto 0 do
	if u = ancs2.(j)#uid then
	  let ni = tree1#search_node_by_uid ui in
	  let n = tree2#search_node_by_uid u in
	  _cands := (high_conf, ni, n, i + j) :: !_cands
      done
    with 
      Not_found -> ()
  done;

  let cands =
    List.fast_sort 
      (fun (_, _, _, p0) (_, _, _, p1) -> Pervasives.compare p1 p0)
      !_cands
  in
  begin
    match cands with
    | [] -> ()
    | (high, n1, n2, p)::rest ->

	DEBUG_MSG "(%a,%a): prox=%d pivot=(%a,%a) (confidence=%s)" 
	  UID.ps nd1#uid UID.ps nd2#uid p UID.ps n1#uid UID.ps n2#uid
	  (if high then "high" else "low");

	prox#set_primary_prox p;
	prox#set_primary_pivot (n1, n2);
	if not high then
	  prox#lower_confidence;

	let rest' =
	  List.filter (fun (h, _, _, _) -> h = not high) rest
	in
	match rest' with
	| [] -> ()
	| (_, n1, n2, p)::_ ->
	    prox#set_secondary_prox p;
	    prox#set_secondary_pivot (n1, n2)
  end;
  prox
(* end of func get_proximity *)

*)
