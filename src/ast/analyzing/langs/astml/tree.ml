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
(* astml/tree.ml *)

open Common
open Label

module Tree = Sourcecode.Tree (Label)
open Tree


let comment_tag = "Comment"
let ignored_tag = "Ignored"

let conv_loc loc =
  try
    Scanf.sscanf loc "%d:%d(%d)-%d:%d(%d)"
      (fun sl sc so el ec eo ->
	Loc.make so eo sl sc el ec
      )
  with _ -> try
    Scanf.sscanf loc "%d(%d)-%d(%d)"
      (fun sl so el eo ->
	Loc.make so eo sl (-1) el (-1)
      )
  with _ -> try
    Scanf.sscanf loc "%d-%d"
      (fun so eo ->
	Loc.make so eo (-1) (-1) (-1) (-1)
      )
  with _ -> try
    Scanf.sscanf loc "%dL,%dC-%dC(%d-%d)"
      (fun sl sc ec so eo -> Loc.make so eo sl sc sl ec)
  with _ -> try
    Scanf.sscanf loc "%dL(%d-%d)"
      (fun sl so eo -> Loc.make so eo sl (-1) sl (-1))
  with _ -> try
    Scanf.sscanf loc "%dL,%dC-%dL,%dC(%d-%d)"
      (fun sl sc el ec so eo -> Loc.make so eo sl sc el ec)
  with _ -> try
    Scanf.sscanf loc "%dL-%dL(%d-%d)"
      (fun sl el so eo -> Loc.make so eo sl (-1) el (-1))

  with 
  | _ ->
      DEBUG_MSG "illegal location format: %s" loc;
      Loc.dummy




let conv_attrs attrs =
  List.map 
    (fun (n, v) -> (n, Conf.pxp_att_value_to_string v))
    (List.filter (* filter out non-label attributes *)
       (fun (n, _) -> not (is_non_label_attr n)) 
       attrs
    )

exception Ignore


(* toplevel conversion function *)
let of_file (options : #Parser_options.c) parser_name file doc =

  let ignored_regions = ref [] in

  let ast_ns = (get_conf parser_name)#ast_ns in

  DEBUG_MSG "file=\"%s\" astns=\"%s\"" file#path ast_ns;

  let rec scan_xnode ?(ast_ns="") xnode =
    match xnode#node_type with
    | Pxp_document.T_element name ->

        if xnode#localname = comment_tag || xnode#localname = ignored_tag then begin
          let v = xnode#attribute location_attr_name in
          let l = conv_loc (Conf.pxp_att_value_to_string v) in
          ignored_regions := (l.Loc.start_offset, l.Loc.end_offset) :: !ignored_regions;
          raise Ignore
        end;

	let children = scan_xnodes ~ast_ns xnode#sub_nodes in
(*
	DEBUG_MSG "elem=%s attrs=[%s]" 
	  name 
	  (Xlist.to_string 
	     (fun (n, v) -> sprintf "%s='%s'" n (Conf.pxp_att_value_to_string v))"; " xnode#attributes);
*)
	let nd = 
	  mknode options
	    { elem_name   = xnode#localname; 
	      elem_attrs  = conv_attrs xnode#attributes; 
	      elem_parser = parser_name;
	      elem_ast_ns = ast_ns;
	    } 
	    children
	in
	let attr = xnode#attribute in

        let is_initializer_list =
	  try
	    let v = attr is_initializer_list_attr_name in
	    (Conf.pxp_att_value_to_string v) = "true"
	  with 
	    Not_found -> false
        in
        let nd =
          if
            is_initializer_list &&
            options#ignore_huge_arrays_flag &&
            (List.length children) >= options#huge_array_threshold
          then
            let hash = (new c options nd false)#digest in
	    mknode options
	      { elem_name   = "HugeArray";
	        elem_attrs  = ("hash",Xhash.to_hex hash)::(conv_attrs xnode#attributes);
	        elem_parser = parser_name;
	        elem_ast_ns = ast_ns;
	      } []
          else
            nd
        in

	begin
	  try
	    let v = attr location_attr_name in
	    let l = conv_loc (Conf.pxp_att_value_to_string v) in
	    nd#data#set_loc l
	  with 
	    Not_found -> ()
	end;
	begin
	  try
	    let v = attr frommacro_attr_name in
	    nd#data#set_frommacro (Conf.pxp_att_value_to_string v)
	  with 
	    Not_found -> ()
	end;
	begin
	  try
	    let v = attr gid_attr_name in
	    let gid = int_of_string (Conf.pxp_att_value_to_string v) in
	    nd#data#set_gid gid
	  with 
	    Not_found -> ()
	end;
	nd

   | t -> raise Ignore

  and scan_xnodes ?(ast_ns="") nodes =
    List.fold_right 
      (fun n l -> try (scan_xnode ~ast_ns n)::l with Ignore -> l) nodes []
  in

  let root = doc#root in

  let nd = 
    match root#sub_nodes with
    | [xnode] -> scan_xnode ~ast_ns xnode 
    | xnds    -> Xprint.failure "\"%s\": invalid ASTML" file#path
  in

  DEBUG_MSG "got nodes";

  let tree = new c options nd true in

  tree#set_ignored_regions !ignored_regions;

  begin
    try
      let p = Conf.pxp_att_value_to_string (root#attribute Astml.parser_attr_name) in
      tree#set_parser_name p;
      DEBUG_MSG "parser_name <- %s" p
    with 
      Not_found -> warning_msg "parser name not found in \"%s\"" file#path
  end;
  begin
    try
      let s = Conf.pxp_att_value_to_string (root#attribute Astml.source_attr_name) in
      tree#set_source_path s;
      DEBUG_MSG "source_path <- %s" s
    with
      Not_found -> warning_msg "source name not found in \"%s\"" file#path
  end;
  begin
    try
      let d = Conf.pxp_att_value_to_string (root#attribute Astml.source_digest_attr_name) in
      tree#set_source_digest (Xhash.of_hex (decode_digest d));
      DEBUG_MSG "source_digest <- %s" d
    with 
      Not_found -> warning_msg "source digest not found in \"%s\"" file#path
  end;

  DEBUG_MSG "tree built";

  tree#collapse;

  DEBUG_MSG "tree collapsed";

  tree
