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
(* delta_format.ml *)

module SB = Spec_base

open Delta_base
open Delta_common
open Delta_interpret

let get_mctl_attr xnode =
  try
    move_control_of_string (get_attr xnode move_control_attr)
  with
    Attribute_not_found _ -> Mfull

let get_attr_boundary xnode a =
  try
    boundary_of_string (get_attr xnode a)
  with
    Attribute_not_found _ -> []

let _get_attr_opt conv xnode a =
  try
    Some (conv (get_attr xnode a))
  with
    Attribute_not_found _ -> None

let get_attr_opt xnode a = _get_attr_opt (fun x -> x) xnode a

let get_iattr_opt = _get_attr_opt int_of_string

let get_key_attr_opt xnode a = _get_attr_opt key_of_string xnode a

let is_edit xnode =
  match xnode#node_type with
  | PxpD.T_element name -> is_edit_tag name
  | _ -> false

exception Skip

module IrreversibleFormat = struct

  type 'tree edit =
    | Ddelete of path_c * boundary

    | Dinsert of subtree_id * 'tree tree_t
          * path_c * boundary
          * subtree_key option * int option * int option * int option

    | Dmove of move_control * MID.t
          * path_c * boundary * path_c * boundary
          * subtree_key option * int option * int option * int option

    | Dchange of move_control * path_c * boundary * 'tree tree_t

    | Dchange_attr of move_control * path_c * attr * value
    | Ddelete_attr of move_control * path_c * attr
    | Dinsert_attr of move_control * path_c * attr * value

  type 'tree t = ('data node_t #SB.tree_t_shared as 'tree) edit list

  let edit_to_string = function
    | Ddelete(path, paths) ->
	sprintf "Ddelete(path:%s,%s)"
	  path#to_string (boundary_to_string paths)

    | Dinsert(stid, tree, path, paths, key_opt, adj_opt, depth_opt, shift_opt) ->
        let l =
          [ "path",      path#to_string;
            "paths",     boundary_to_string paths;
            "stid",      stid_to_str stid;
            "key_opt",   key_opt_to_string key_opt;
            "adj_opt",   int_opt_to_string adj_opt;
            "depth_opt", int_opt_to_string depth_opt;
            "shift_opt", int_opt_to_string shift_opt;
          ]
        in
	sprintf "Dinsert(%s,\n%s)" (attrs_to_string l) tree#initial_to_string

    | Dmove(mctl, mid, path_from, paths_from, path_to, paths_to,
            key_opt, adj_opt, depth_opt, shift_opt) ->
        let l =
          [ "mctl",       move_control_to_string mctl;
            "mid",        MID.to_string mid;
            "path_from",  path_from#to_string;
            "paths_from", boundary_to_string paths_from;
            "path_to",    path_to#to_string;
            "paths_to",   boundary_to_string paths_to;
            "key_opt",    key_opt_to_string key_opt;
            "adj_opt",    int_opt_to_string adj_opt;
            "depth_opt",  int_opt_to_string depth_opt;
            "shift_opt",  int_opt_to_string shift_opt;
          ]
        in
        sprintf "Dmove(%s)" (attrs_to_string l)

    | Dchange(mctl, path, paths, tree2) ->
	sprintf "Dchange(%s,path:%s,%s,\n%s)" (move_control_to_string mctl)
	  path#to_string (boundary_to_string paths) tree2#initial_to_string

    | Dchange_attr(mctl, path, attr, nv) ->
	sprintf "Dchange_attr(%s,path:%s,attr:%s,%s)"
          (move_control_to_string mctl) path#to_string attr nv

    | Ddelete_attr(mctl, path, attr) ->
	sprintf "Ddelete_attr(%s,path:%s,attr:%s)"
          (move_control_to_string mctl) path#to_string attr

    | Dinsert_attr(mctl, path, attr, v) ->
	sprintf "Dinsert_attr(%s,path:%s,attr:%s,%s)"
          (move_control_to_string mctl) path#to_string attr v

  let to_string d =
    if d = [] then "" else "\n" ^ (Xlist.to_string edit_to_string ";\n" d)

  let of_doc options tree_factory doc_root = (* delta doc -> Delta.IrreversibleFormat.t *)

    let rec get_tree xnode = (* node -> tree *)
      let children = xnode#sub_nodes in
      let edits, trees = List.partition is_edit children in
      match edits with
      | [] -> begin
	  match trees with
	  | [tree] -> tree_factory#from_xnode tree
	  | _ -> invalid_delta xnode
		"original entity not included or multiple original entities"
	end
      | _ -> invalid_delta xnode "illegal sub edits"

    and scan_edit xnode =
      match xnode#node_type with
      | PxpD.T_element name ->
	  if name = del_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let paths = get_attr_boundary xnode bdry_attr in
	    Ddelete(path, paths)

	  else if name = ins_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let paths = get_attr_boundary xnode bdry_attr in
	    let stid = stid_of_str (get_attr xnode stid_attr) in
	    let tree = get_tree xnode in
            let key_opt = _get_attr_opt key_of_string xnode parent_attr in
            let adj_opt = get_iattr_opt xnode adj_attr in
            let depth_opt = get_iattr_opt xnode depth_attr in
            let shift_opt = get_iattr_opt xnode shift_attr in
	    Dinsert(stid, tree, path, paths, key_opt, adj_opt, depth_opt, shift_opt)

	  else if name = mov_tag then
            let mid = MID.of_string (get_attr xnode mid_attr) in
	    let path_from = path_of_string (get_attr xnode path_from_attr) in
	    let paths_from = get_attr_boundary xnode bdry_from_attr in
	    let path_to = path_of_string (get_attr xnode path_to_attr) in
	    let paths_to = get_attr_boundary xnode bdry_to_attr in
            let key_opt = _get_attr_opt key_of_string xnode parent_attr in
            let adj_opt = get_iattr_opt xnode adj_attr in
            let depth_opt = get_iattr_opt xnode depth_attr in
            let shift_opt = get_iattr_opt xnode shift_attr in
            let mctl = get_mctl_attr xnode in
            Dmove(mctl, mid,
                  path_from, paths_from, path_to, paths_to,
                  key_opt, adj_opt, depth_opt, shift_opt)

	  else if name = chg_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let paths = get_attr_boundary xnode bdry_attr in
	    let children = xnode#sub_nodes in
            let mctl = get_mctl_attr xnode in
	    match children with
	    | [x] -> Dchange(mctl, path, paths, tree_factory#from_xnode x)
	    | _ -> invalid_delta xnode "invalid change element"

	  else if name = achg_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let attr = get_attr xnode attr_attr in
	    let nv = get_attr xnode nv_attr in
            let mctl = get_mctl_attr xnode in
	    Dchange_attr(mctl, path, attr, nv)

	  else if name = adel_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let attr = get_attr xnode attr_attr in
            let mctl = get_mctl_attr xnode in
	    Ddelete_attr(mctl, path, attr)

	  else if name = ains_tag then
	    let path = path_of_string (get_attr xnode path_attr) in
	    let attr = get_attr xnode attr_attr in
	    let v = get_attr xnode v_attr in
            let mctl = get_mctl_attr xnode in
	    Dinsert_attr(mctl, path, attr, v)

          else if is_file_edit_tag name then raise Skip

	  else invalid_delta xnode ("scan_edit: invalid tag: " ^ name)

      | nt ->
	  invalid_delta xnode
	    (sprintf
	       "scan_edit: invalid node type: %s[%s]"
	       (node_type_to_string nt) xnode#data)

    and scan_edits xnodes =
      List.fold_right (fun n l -> (scan_edit n)::l) xnodes []

    in

    let scan_delta xnode =
      match xnode#node_type with
      | PxpD.T_element name ->
	  if name <> root_tag then invalid_delta xnode "invalid root element";
	  let edits = scan_edits xnode#sub_nodes in
	  edits

      | _ -> invalid_delta xnode "invalid root element"
    in
    let delta = scan_delta doc_root in
    delta
    (* end of func Delta.IrreversibleFormat.of_doc *)

  let patch options tree_factory ?(fail_on_error=true) file doc_root ch =
    try
      let delta = of_doc options tree_factory doc_root in

      let tree = tree_factory#from_file file in

      DEBUG_MSG "T:\n%s\n" tree#to_string;

      if not options#weak_flag then begin
        tree#recover_true_children ~initial_only:false ();
        DEBUG_MSG "T:\n%s\n" tree#to_string
      end;

      let interpreter = new interpreter tree in

      List.iter
        (fun ed ->
          match ed with
	  | Dchange(mctl, path, paths, subtree) ->
	      DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_change ~mctl path paths subtree

	  | Dchange_attr(mctl, path, attr, nv) ->
	      DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_change_attr ~mctl path attr nv

	  | Ddelete_attr(mctl, path, attr) ->
	      DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_delete_attr ~mctl path attr

	  | Dinsert_attr(mctl, path, attr, v) ->
	      DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_insert_attr ~mctl path attr v
          | _ -> ()
        ) delta;

      let dstid_count = ref 0 in
      let gen_del_stid_key() =
        let i = K_del !dstid_count in
        incr dstid_count;
        i
      in
      List.iter
        (fun ed ->
          match ed with
          | Ddelete(path, paths) ->
              interpreter#reg_deleted path paths (Some (gen_del_stid_key()))

          | Dinsert(stid, subtree, path, paths, key_opt, adj_opt, depth_opt, shift_opt) ->
              interpreter#reg_subtree
                ~key_opt ~adj_opt ~depth_opt ~shift_opt stid subtree path paths

          | Dmove(mctl, mid, path_from, paths_from, path_to, paths_to,
                  key_opt, adj_opt, depth_opt, shift_opt) -> begin
                    if mctl <> MinsertOnly then
                      interpreter#reg_deleted path_from paths_from (Some (key_of_mid mid));
                    if mctl <> MdeleteOnly then
                      interpreter#reg_moved_subtree
                        ~key_opt ~adj_opt ~depth_opt ~shift_opt
                        mid path_from paths_from path_to paths_to;
                  end
          | _ -> ()
        ) delta;

      interpreter#do_deferred_relabels();

      List.iter
        (fun ed ->
          match ed with
          | Ddelete(path, paths) ->
              interpreter#reg_parent path paths

          | Dmove(mctl, mid, path_from, paths_from, path_to, paths_to, _, _, _, _) ->
              interpreter#reg_parent path_from paths_from

          | _ -> ()
        ) delta;

      interpreter#make_frontier_tbl();

      List.iter
        (fun ed ->
          match ed with
	  | Ddelete(path, paths) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_delete path paths

	  | Dinsert(stid, subtree,
                    path, paths, key_opt, adj_opt, depth_opt, shift_opt) ->
                      DEBUG_MSG "<** %s **>" (edit_to_string ed);
                      interpreter#interpret_insert
                        (key_of_stid stid) subtree path paths
                        key_opt adj_opt depth_opt shift_opt

          | Dmove(mctl, mid, path_from, paths_from, path_to, paths_to,
                  key_opt, adj_opt, depth_opt, shift_opt)
            ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_move ~mctl
                mid
                path_from paths_from path_to paths_to
                key_opt adj_opt depth_opt shift_opt

          | _ -> ()
        ) delta;

      interpreter#mutate();

      interpreter#shift_positions();

      (*DEBUG_MSG "final tree:\n%s" tree#initial_to_string;*)

      (*try*)
        tree#unparse_ch ~fail_on_error ch
      (*with
        exc -> begin
          let fname_astml = file#fullpath^".err.ast" in
          tree#dump_astml ?comp:(Some options#ast_compression) fname_astml;
	  Xprint.error "failed to unparse. AST (in ASTML) saved in \"%s\"" fname_astml;
          raise exc
        end*)

    with
    | Invalid_delta err ->
	Xprint.error
          "[%s %dL,%dC] invalid delta: %s" err.file err.line err.pos err.reason


end (* of module Delta.IrreversibleFormat *)


module ReversibleFormat = struct

  type 'tree edit =
    | Ddelete of subtree_id * 'tree tree_t
          * path_c * boundary
          * path_c * boundary
          * subtree_key option * int option * int option * int option

    | Dinsert of subtree_id * 'tree tree_t
          * path_c * boundary
          * subtree_key option * int option * int option * int option
          * path_c * boundary

    | Dmove of MID.t
          * path_c * boundary * path_c * boundary
          * subtree_key option * int option * int option * int option
          * path_c * boundary * path_c * boundary
          * subtree_key option * int option * int option * int option

    | Dchange of path_c * boundary * 'tree tree_t
          * path_c * boundary * 'tree tree_t

    | Dchange_attr of path_c * path_c * attr * value * value
    | Ddelete_attr of path_c * path_c * attr * value
    | Dinsert_attr of path_c * path_c * attr * value

  type 'tree t = ('data node_t #SB.tree_t_shared as 'tree) edit list

  let edit_to_string = function
    | Ddelete(stid, tree, path, paths,
              path', paths', key_opt', adj_opt', depth_opt', shift_opt') ->
        let l =
          [ "stid",       stid_to_str stid;
            "path",       path#to_string;
            "paths",      boundary_to_string paths;
            "path'",      path'#to_string;
            "paths'",     boundary_to_string paths';
            "key_opt'",   key_opt_to_string key_opt';
            "adj_opt'",   int_opt_to_string adj_opt';
            "depth_opt'", int_opt_to_string depth_opt';
            "shift_opt'", int_opt_to_string shift_opt';
          ]
        in
	sprintf "Ddelete(%s,\n%s)" (attrs_to_string l) tree#initial_to_string

    | Dinsert(stid, tree, path, paths, key_opt, adj_opt, depth_opt, shift_opt,
              path', paths') ->
        let l =
          [ "stid",      stid_to_str stid;
            "path",      path#to_string;
            "paths",     boundary_to_string paths;
            "key_opt",   key_opt_to_string key_opt;
            "adj_opt",   int_opt_to_string adj_opt;
            "depth_opt", int_opt_to_string depth_opt;
            "shift_opt", int_opt_to_string shift_opt;
            "path'",     path'#to_string;
            "paths'",    boundary_to_string paths';
          ]
        in
	sprintf "Dinsert(%s,\n%s)" (attrs_to_string l) tree#initial_to_string

    | Dmove(mid,
            path1from, paths1from, path1to, paths1to,
            key_opt1, adj_opt1, depth_opt1, shift_opt1,
            path2from, paths2from, path2to, paths2to,
            key_opt2, adj_opt2, depth_opt2, shift_opt2) ->
              let l =
                [ "mid",        MID.to_string mid;
                  "path1from",  path1from#to_string;
                  "paths1from", boundary_to_string paths1from;
                  "path1to",    path1to#to_string;
                  "paths1to",   boundary_to_string paths1to;
                  "key_opt1",   key_opt_to_string key_opt1;
                  "adj_opt1",   int_opt_to_string adj_opt1;
                  "depth_opt1", int_opt_to_string depth_opt1;
                  "shift_opt1", int_opt_to_string shift_opt1;
                  "path2from",  path2from#to_string;
                  "paths2from", boundary_to_string paths2from;
                  "path2to",    path2to#to_string;
                  "paths2to",   boundary_to_string paths2to;
                  "key_opt2",   key_opt_to_string key_opt2;
                  "adj_opt2",   int_opt_to_string adj_opt2;
                  "depth_opt2", int_opt_to_string depth_opt2;
                  "shift_opt2", int_opt_to_string shift_opt2;
               ]
              in
              sprintf "Dmove(%s)"
                (String.concat "," (List.map (fun (a,s) -> sprintf "%s:%s" a s) l))

    | Dchange(path1, paths1, tree1, path2, paths2, tree2) ->
	sprintf "Dchange(path1:%s,%s,\n%s,\npath2:%s,%s\n%s)"
	  path1#to_string (boundary_to_string paths1) tree1#initial_to_string
	  path2#to_string (boundary_to_string paths2) tree2#initial_to_string

    | Dchange_attr(path1, path2, attr, ov, nv) ->
	sprintf "Dchange_attr(path1:%s,path2:%s,attr:%s,%s,%s)"
	  path1#to_string path2#to_string attr ov nv

    | Ddelete_attr(path1, path2, attr, v) ->
	sprintf "Ddelete_attr(path1:%s,path2:%s,attr:%s,%s)"
	  path1#to_string path2#to_string attr v

    | Dinsert_attr(path1, path2, attr, v) ->
	sprintf "Dinsert_attr(path1:%s,path2:%s,attr:%s,%s)"
	  path1#to_string path2#to_string attr v

  let to_string d =
    if d = [] then "" else "\n" ^ (Xlist.to_string edit_to_string ";\n" d)


  let reverse_edit = function
    | Ddelete(stid, tree, path, paths,
              path', paths', key_opt', adj_opt', depth_opt', shift_opt')
      ->
	Dinsert(stid, tree,
                path', paths', key_opt', adj_opt', depth_opt', shift_opt',
                path, paths)

    | Dinsert(stid, tree, path, paths, key_opt, adj_opt, depth_opt, shift_opt,
              path', paths')
      ->
	Ddelete(stid, tree, path', paths',
                path, paths, key_opt, adj_opt, depth_opt, shift_opt)

    | Dmove(mid,
            path1from, paths1from, path1to, paths1to,
            key_opt1, adj_opt1, depth_opt1, shift_opt1,
            path2from, paths2from, path2to, paths2to,
            key_opt2, adj_opt2, depth_opt2, shift_opt2)
      ->
        Dmove(mid,
              path2from, paths2from, path2to, paths2to,
              key_opt2, adj_opt2, depth_opt2, shift_opt2,
              path1from, paths1from, path1to, paths1to,
              key_opt1, adj_opt1, depth_opt1, shift_opt1)

    | Dchange(path1, paths1, tree1, path2, paths2, tree2) ->
	Dchange(path2, paths2, tree2, path1, paths1, tree1)

    | Dchange_attr(path1, path2, attr, ov, nv) ->
	Dchange_attr(path2, path1, attr, nv, ov)

    | Ddelete_attr(path1, path2, attr, v) -> Dinsert_attr(path2, path1, attr, v)
    | Dinsert_attr(path1, path2, attr, v) -> Ddelete_attr(path2, path1, attr, v)

  let reverse_delta eds =
    let rels, dels, inss =
      List.fold_left
	(fun (rels, dels, inss) ed ->
	  match ed with
	  | Ddelete _ -> (rels, (reverse_edit ed)::dels, inss)
	  | Dinsert _ -> (rels, dels, (reverse_edit ed)::inss)
	  | _ -> ((reverse_edit ed)::rels, dels, inss)
	) ([], [], []) eds
    in
    (List.rev rels) @ (List.rev inss) @ (List.rev dels)



  let of_doc options tree_factory doc_root = (* delta doc -> Delta.ReversibleFormat.t *)

    let extract tag xnode =
      match xnode#node_type with
      | PxpD.T_element name -> begin
	  if name = tag then begin
	    match xnode#sub_nodes with
	    | [x] -> tree_factory#from_xnode x
	    | _ -> invalid_delta xnode ("empty " ^ tag)
	  end
	  else
            invalid_delta xnode (sprintf "tag name mismatch: %s != %s" name tag)
      end
      | _ -> invalid_delta xnode (sprintf "not an \"%s\" element" tag)
    in

    let rec get_tree xnode = (* node -> tree *)
      let children = xnode#sub_nodes in
      let edits, trees = List.partition is_edit children in
      match edits with
      | [] -> begin
	  match trees with
	  | [tree] -> tree_factory#from_xnode tree
	  | _ -> invalid_delta xnode
		"original entity not included or multiple original entities"
	end
      | _ -> invalid_delta xnode "illegal sub edits"

    and scan_edit xnode =
      match xnode#node_type with
      | PxpD.T_element name ->
	  if name = del_tag then
	    let stid = stid_of_str (get_attr xnode stid_attr) in
	    let tree = get_tree xnode in
	    let path = path_of_string (get_attr xnode path1_attr) in
	    let paths = get_attr_boundary xnode bdry1_attr in
	    let path' = path_of_string (get_attr xnode path2_attr) in
	    let paths' =
              match get_attr_boundary xnode bdry2_attr with
              | [] -> paths
              | ps' -> ps'
            in
            let key_opt' = _get_attr_opt key_of_string xnode parent_attr in
            let adj_opt' = get_iattr_opt xnode adj_attr in
            let depth_opt' = get_iattr_opt xnode depth_attr in
            let shift_opt' = get_iattr_opt xnode shift_attr in
	    Ddelete(stid, tree, path, paths,
                    path', paths', key_opt', adj_opt', depth_opt', shift_opt')

	  else if name = ins_tag then
	    let stid = stid_of_str (get_attr xnode stid_attr) in
	    let tree = get_tree xnode in
	    let path = path_of_string (get_attr xnode path1_attr) in
	    let paths = get_attr_boundary xnode bdry1_attr in
            let key_opt = _get_attr_opt key_of_string xnode parent_attr in
            let adj_opt = get_iattr_opt xnode adj_attr in
            let depth_opt = get_iattr_opt xnode depth_attr in
            let shift_opt = get_iattr_opt xnode shift_attr in
	    let path' = path_of_string (get_attr xnode path2_attr) in
	    let paths' =
              match get_attr_boundary xnode bdry2_attr with
              | [] -> paths
              | ps' -> ps'
            in
	    Dinsert(stid, tree, path, paths, key_opt, adj_opt, depth_opt, shift_opt,
                    path', paths')

	  else if name = mov_tag then
            let mid = MID.of_string (get_attr xnode mid_attr) in
	    let path1from = path_of_string (get_attr xnode path1from_attr) in
	    let paths1from = get_attr_boundary xnode bdry1from_attr in
	    let path1to = path_of_string (get_attr xnode path1to_attr) in
	    let paths1to = get_attr_boundary xnode bdry1to_attr in
            let key_opt1 = _get_attr_opt key_of_string xnode parent1_attr in
            let adj_opt1 = get_iattr_opt xnode adj1_attr in
            let depth_opt1 = get_iattr_opt xnode depth1_attr in
            let shift_opt1 = get_iattr_opt xnode shift1_attr in
	    let path2from = path_of_string (get_attr xnode path2from_attr) in
	    let paths2from = get_attr_boundary xnode bdry2from_attr in
	    let path2to = path_of_string (get_attr xnode path2to_attr) in
	    let paths2to = get_attr_boundary xnode bdry2to_attr in
            let key_opt2 = _get_attr_opt key_of_string xnode parent2_attr in
            let adj_opt2 = get_iattr_opt xnode adj2_attr in
            let depth_opt2 = get_iattr_opt xnode depth2_attr in
            let shift_opt2 = get_iattr_opt xnode shift2_attr in
            Dmove(mid,
                  path1from, paths1from, path1to, paths1to,
                  key_opt1, adj_opt1, depth_opt1, shift_opt1,
                  path2from, paths2from, path2to, paths2to,
                  key_opt2, adj_opt2, depth_opt2, shift_opt2)

	  else if name = chg_tag then
	    let path1 = path_of_string (get_attr xnode path1_attr) in
	    let paths1 = get_attr_boundary xnode bdry1_attr in
	    let path2 = path_of_string (get_attr xnode path2_attr) in
	    let paths2 = get_attr_boundary xnode bdry2_attr in
	    let children = xnode#sub_nodes in
	    match children with
	    | [o; n] ->
                Dchange(path1, paths1, extract old_tag o,
			path2, paths2, extract new_tag n)
	    | _ ->
		invalid_delta xnode "invalid change element"

	  else if name = achg_tag then
	    let path1 = path_of_string (get_attr xnode path1_attr) in
	    let path2 = path_of_string (get_attr xnode path2_attr) in
	    let attr = get_attr xnode attr_attr in
	    let ov = get_attr xnode ov_attr in
	    let nv = get_attr xnode nv_attr in
	    Dchange_attr(path1, path2, attr, ov, nv)

	  else if name = adel_tag then
	    let path1 = path_of_string (get_attr xnode path1_attr) in
	    let path2 = path_of_string (get_attr xnode path2_attr) in
	    let attr = get_attr xnode attr_attr in
	    let v = get_attr xnode v_attr in
	    Ddelete_attr(path1, path2, attr, v)

	  else if name = ains_tag then
	    let path1 = path_of_string (get_attr xnode path1_attr) in
	    let path2 = path_of_string (get_attr xnode path2_attr) in
	    let attr = get_attr xnode attr_attr in
	    let v = get_attr xnode v_attr in
	    Dinsert_attr(path1, path2, attr, v)

          else if is_file_edit_tag name then raise Skip

	  else invalid_delta xnode ("scan_edit: invalid tag: " ^ name)

      | nt ->
	  invalid_delta xnode
	    (sprintf
	       "scan_edit: invalid node type: %s[%s]"
	       (node_type_to_string nt) xnode#data)

    and scan_edits xnodes =
      List.fold_right (fun n l -> (scan_edit n)::l) xnodes []
    in

    let scan_delta xnode =
      match xnode#node_type with
      | PxpD.T_element name ->
	  if name <> root_tag then invalid_delta xnode "invalid root element";
	  let edits = scan_edits xnode#sub_nodes in
	  edits

      | _ -> invalid_delta xnode "invalid root element"
    in
    let delta = scan_delta doc_root in
    delta
  (* end of func Delta.ReversibleFormat.of_doc *)

  let patch options tree_factory ?(fail_on_error=true) ?(reverse=false) file doc_root ch =
    try
      let delta =
	let orig = of_doc options tree_factory doc_root in
	if reverse then
          reverse_delta orig
	else
          orig
      in
      let tree = tree_factory#from_file file in

      if not options#weak_flag then
        tree#recover_true_children ~initial_only:true ();

      let interpreter = new interpreter tree in

      List.iter
        (fun ed ->
          match ed with
	  | Dchange(path, paths, _, _, _, subtree) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_change path paths subtree

	  | Dchange_attr(path, _, attr, _, nv) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_change_attr path attr nv

	  | Ddelete_attr(path, _, attr, _) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_delete_attr path attr

	  | Dinsert_attr(path, _, attr, v) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_insert_attr path attr v
          | _ -> ()
        ) delta;

      List.iter
        (fun ed ->
          match ed with
          | Ddelete(_, _, path, paths, _, _, _, _, _, _) ->
              interpreter#reg_deleted path paths None

          | Dinsert(stid, subtree, path, paths,
                    key_opt, adj_opt, depth_opt, shift_opt, _, _) ->
              interpreter#reg_subtree
                ~key_opt ~adj_opt ~depth_opt ~shift_opt stid subtree path paths

          | Dmove(mid, path_from, paths_from, path_to, paths_to,
                  key_opt, adj_opt, depth_opt, shift_opt,
                  _, _, _, _, _, _, _, _)
            ->
              begin
                interpreter#reg_deleted path_from paths_from (Some (key_of_mid mid));
                interpreter#reg_moved_subtree
                  ~key_opt ~adj_opt ~depth_opt ~shift_opt
                  mid path_from paths_from path_to paths_to
              end
          | _ -> ()
        ) delta;

      List.iter
        (fun ed ->
          match ed with
          | Ddelete(_, _, path, paths, _, _, _, _, _, _) ->
              interpreter#reg_parent path paths

          | Dmove(mid, path_from, paths_from, path_to, paths_to, _, _, _, _,
                  _, _, _, _, _, _, _, _)
            ->
              interpreter#reg_parent path_from paths_from

          | _ -> ()
        ) delta;

      interpreter#make_frontier_tbl();

      List.iter
        (fun ed ->
	  match ed with
	  | Ddelete(_, _, path, paths, _, _, _, _, _, _) ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_delete path paths

	  | Dinsert(stid, subtree,
                    path, paths, key_opt, adj_opt, depth_opt, shift_opt, _, _)
            ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_insert
                        (key_of_stid stid) subtree path paths
                        key_opt adj_opt depth_opt shift_opt

          | Dmove(mid, path1from, paths1from, path1to, paths1to,
                  key_opt1, adj_opt1, depth_opt1, shift_opt1,
                  _, _, _, _, _, _, _, _)
            ->
              DEBUG_MSG "<** %s **>" (edit_to_string ed);
              interpreter#interpret_move
                mid path1from paths1from
                path1to paths1to
                key_opt1 adj_opt1 depth_opt1 shift_opt1
          | _ -> ()
        ) delta;

      interpreter#mutate();

      interpreter#shift_positions();

      tree#unparse_ch ~fail_on_error ch

    with
      Invalid_delta err ->
	Xprint.error
          "[%s %dL,%dC] invalid delta: %s" err.file err.line err.pos err.reason


end (* of module Delta.ReversibleFormat *)

module Format = struct

  let _patch
      options
      (tree_factory : 'tree tree_t SB.tree_factory_t)
      ?(fail_on_error=true)
      ?(reverse=false)
      file
      (doc_root : SB.xnode_t)
      reversible
      =
    if reversible then begin
      verbose_msg options "reversible delta format";
      ReversibleFormat.patch options tree_factory ~fail_on_error ~reverse file doc_root
    end
    else begin
      verbose_msg options "irreversible delta format";
      if reverse then
        failwith "Delta.Format.patch: irreversible delta"
      else
        IrreversibleFormat.patch options tree_factory ~fail_on_error file doc_root
    end

  let patch
      options
      (tree_factory : 'tree tree_t SB.tree_factory_t)
      ?(fail_on_error=true)
      ?(reverse=false)
      file
      delta_file
      =
    let doc_root, reversible =
      parse_file options tree_factory#namespace_manager delta_file
    in
    _patch options tree_factory ~fail_on_error ~reverse file doc_root reversible


end (* of module Delta.Format *)
