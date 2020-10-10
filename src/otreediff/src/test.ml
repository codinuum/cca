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
(* test.ml *)

open Printf
open Common

let cmd_name = Filename.basename(Sys.argv.(0))
let dirnames = ref []

let usage_msg = Printf.sprintf "usage: %s DIR1 DIR2" cmd_name

let speclist = 
  [
 ]

let _ = Arg.parse
    speclist
    (fun s -> dirnames := s::!dirnames)
    (usage_msg)

let dir1, dir2 = 
  match !dirnames with
    d1::d2::[] -> d2, d1
  | _ -> Arg.usage speclist usage_msg; exit 1


class node_data (lab : string) =
  object (self)
    inherit Otree.data2
    method label = lab
    method to_string = lab
    method to_rep = self#to_string
    method to_elem_data = self#to_string, [], ""
    method equals nd = self#label = nd#label
    method eq nd = false
    method digest = None
    method _digest = None
    method set_digest d = ()
    method reset_digest = ()
  end

class tree (root : node_data Otree.node2) =
  object(self)
    inherit [ 'node ] Otree.otree2 root true
    method path idx = (self#get idx)#path#to_string
  end

exception Dotfile

let file_to_tree file =
  let uidgen = new UID.generator in
  let rec file_to_node path =
    let name = Filename.basename path in
    if name = "." || name = ".." then raise Dotfile;
    if (Unix.stat path).Unix.st_kind = Unix.S_DIR
    then 
      begin
	let obj = new node_data name in
	let dirh = Unix.opendir path in
	let nodes = ref [] in
	(try
	  while true do
	    let cpath = sprintf "%s/%s" path (Unix.readdir dirh) in
	    try
	      nodes := (file_to_node cpath)::!nodes
	    with Dotfile -> ()
	  done
	with End_of_file -> ());
	Unix.closedir dirh;
	let cmp nd1 nd2 = compare nd1#to_rep nd2#to_rep in
	Otree.create_node2 uidgen obj (Array.of_list (List.fast_sort cmp !nodes))
      end
    else
      let obj1 = new node_data name in 
      Otree.create_leaf2 uidgen obj1
  in
  new tree (file_to_node file)


let analyze tree1 tree2 eds mapping iso =
  let get_lab1 i = (tree1#get i)#data#label in
  let get_lab2 i = (tree2#get i)#data#label in
  let proc_one = function
      Edit.Relabel(i1, i2) -> 
	let lab1, lab2 = get_lab1 i1, get_lab2 i2 in
	(try 
	  let c = 
	    Array.map 
	      (fun nd ->
		let i = nd#index in
		if List.mem i iso then Mapping.find i mapping
		else raise Not_found
	      ) ((tree1#get i1)#children)
	  in
	  let c' = 
	    Array.map (fun nd -> nd#index) ((tree2#get i2)#children) 
	  in
	  if c = c' then printf "[RENAMED]: \"%s/%s\" --> \"%s/%s\"\n" 
	      (tree1#path i1) lab1 (tree2#path i2) lab2
	with Not_found -> ())

    | Edit.Delete i -> 
	let lab = get_lab1 i in
	printf "[DELETED]: \"%s/%s\"\n" (tree1#path i) lab

    | Edit.Insert(i, _) ->
	let lab = get_lab2 i in
	printf "[INSERTED]: \"%s/%s\"\n" (tree2#path i) lab

  in
  Edit.seq_iter proc_one eds


let dtree1, dtree2 = (file_to_tree dir1), (file_to_tree dir2)

let _ = printf "dir1:\n"; dtree1#print
let _ = printf "dir2:\n"; dtree2#print

let _ = dtree1#save_dot "dir1" [] "dir1.dot"
let _ = dtree2#save_dot "dir2" [] "dir2.dot"

let cost t1 t2 i j = 1

let edits, mapping, iso = ZS.Int.find cost dtree1 dtree2

let _ = printf "\nEdit seq:\n%s\n\n" (Edit.seq_to_string edits)

let _ = analyze dtree1 dtree2 edits mapping iso

(*
let _ = Otdiff.to_dot "test.dot" dtree1 dtree2 edits mapping []
*)
