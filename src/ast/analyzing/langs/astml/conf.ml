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
(* astml/conf.ml *)



let sprintf = Printf.sprintf

let conf_ns = "http://codinuum.com/diffts/diffast/conf#"

let conf_prefix = "dc"

(*
let add_conf_prefix = Astml.add_prefix conf_prefix
*)
let add_conf_prefix ln = conf_ns ^ ln

let collapse_targets_tag         = add_conf_prefix "collapse_targets"
let category_tag                 = add_conf_prefix "category"
let attr_cond_tag                = add_conf_prefix "attr_cond"
let pair_tag                     = add_conf_prefix "pair"
let relabel_allowed_tag          = add_conf_prefix "relabel_allowed"
let relabel_disallowed_tag       = add_conf_prefix "relabel_disallowed"
let anonymize2_rules_tag         = add_conf_prefix "anonymize2_rules"
let anonymize3_rules_tag         = add_conf_prefix "anonymize3_rules"
let to_be_notified_tag           = add_conf_prefix "to_be_notified"
let forced_to_be_collapsible_tag = add_conf_prefix "forced_to_be_collapsible"
let boundary_nodes_tag           = add_conf_prefix "boundary_nodes"
let partition_nodes_tag          = add_conf_prefix "partition_nodes"
let sequence_nodes_tag           = add_conf_prefix "sequence_nodes"

let cannot_be_keyroot_tag        = add_conf_prefix "cannot_be_keyroot"

let name_attr                    = "name"
let ast_ns_attr                  = "astns"


let ns_mgr = Pxp_dtd.create_namespace_manager()

let config = 
  { Pxp_types.default_config with 
    Pxp_types.enable_namespace_processing = Some ns_mgr;
  } 
let spec = Pxp_tree_parser.default_namespace_spec
let transform_dtd dtd =
  dtd#namespace_manager#add_namespace conf_prefix conf_ns;
  dtd


let escape s = s

let pxp_att_value_to_string = function
  | Pxp_core_types.A.Value s       -> escape s
  | Pxp_core_types.A.Valuelist sl  -> Xlist.to_string escape "," sl
  | Pxp_core_types.A.Implied_value -> "<implied>"

let conv_pxp_attrs =
  List.map (fun (n, v) -> n, pxp_att_value_to_string v)

let attrs_to_string = 
  Xlist.to_string (fun (n, v) -> n^"=\""^v^"\"") " "

let elem_pat_to_string (tag_pat, attrs) =
  sprintf "\"%s\"%s" 
    tag_pat
    (if attrs = [] then "" else sprintf " (%s)" (attrs_to_string attrs))

let do_elem nd name f =
  match nd#node_type with
  | Pxp_document.T_element _ -> 
      let n = nd#namespace_uri ^ nd#localname in
      if n = name then f nd
  | _ -> ()


exception Not_parsed

class c conf_file = object (self)

  val mutable doc = None
  method get_doc = 
    match doc with 
    | None -> raise Not_parsed
    | Some doc -> doc

  val mutable ast_ns = "unknown"
  val mutable prefix = "unknown"
  method ast_ns = ast_ns

  val mutable collapse_targets = [] (* TAG_NAME_PAT * ATTRS *)
  method collapse_targets = collapse_targets

  val mutable forced_to_be_collapsible = [] (* TAG_NAME_PAT * ATTRS *)
  method forced_to_be_collapsible = forced_to_be_collapsible

  val mutable relabel_allowed_table = [] (* (TAG_NAME_PAT * ATTRS) * (TAG_NAME_PAT * ATTRS) *)
  method relabel_allowed_table = relabel_allowed_table

  val mutable relabel_disallowed_table = [] (* (TAG_NAME_PAT * ATTRS) * (TAG_NAME_PAT * ATTRS) *)
  method relabel_disallowed_table = relabel_disallowed_table

  val mutable anonymize2_rules = [] (* (TAG_NAME * ATTRS) * (TAG_NAME * ATTRS) *)
  method anonymize2_rules = anonymize2_rules

  val mutable anonymize3_rules = [] (* (TAG_NAME * ATTRS) * (TAG_NAME * ATTRS) *)
  method anonymize3_rules = anonymize3_rules

  val mutable to_be_notified = [] (* TAG_NAME_PAT * ATTRS *)
  method to_be_notified = to_be_notified

  val mutable boundary_nodes = [] (* TAG_NAME_PAT * ATTRS *)
  method boundary_nodes = boundary_nodes

  val mutable partition_nodes = [] (* TAG_NAME_PAT * ATTRS *)
  method partition_nodes = partition_nodes

  val mutable sequence_nodes = [] (* TAG_NAME_PAT * ATTRS *)
  method sequence_nodes = sequence_nodes


  val mutable cannot_be_keyroot = [] (* TAG_NAME_PAT * ATTRS *)
  method cannot_be_keyroot = cannot_be_keyroot


  method parse =
    DEBUG_MSG "reading \"%s\"..." conf_file;
    try
      let source = Pxp_types.from_file conf_file in
      let d = Pxp_tree_parser.parse_wfdocument_entity ~transform_dtd config source spec in
      doc <- Some d;
      let a = 
	try 
	  pxp_att_value_to_string (d#root#attribute ast_ns_attr)
	with 
	| Not_found -> Xprint.failure "Astml.Conf.c#parse: \"%s\" attribute not found" ast_ns_attr
      in
      ast_ns <- a;
      DEBUG_MSG "ast name space=\"%s\"" a;
      try
	prefix <- Astml.get_prefix_by_ns a;
	DEBUG_MSG "prefix=\"%s\"" prefix
      with 
      | Not_found -> Xprint.failure "Astml.Conf.c#parse: prefix for \"%s\" not found" a
    with
    | Failure msg -> Xprint.error "%s" msg; exit 1
    | e -> Xprint.error "%s" (Pxp_types.string_of_exn e); exit 1

  method private get_attr_cond cat_nd =
    let list = cat_nd#sub_nodes in
    let cond = ref [] in
    List.iter
      (fun nd ->
	do_elem nd attr_cond_tag 
	  (fun n ->
	    cond := (conv_pxp_attrs n#attributes) @ !cond
	  )
      ) list;
    !cond

  method private get_pattern_list heading_tag =
    let doc = self#get_doc in

    let pat_list = ref [] in

    let proc_category nd =
      let cat = pxp_att_value_to_string (nd#attribute name_attr) in
      let attr_cond = self#get_attr_cond nd in
      pat_list := (cat, attr_cond)::!pat_list
    in

    let proc_heading nd =
      let targets = nd#sub_nodes in
      List.iter
	(fun nd ->
	  do_elem nd category_tag proc_category
	) targets
    in
    begin
      try
	let list = doc#root#sub_nodes in
	List.iter
	  (fun nd -> 
	    do_elem nd heading_tag proc_heading
	  ) list;

	BEGIN_DEBUG
	  List.iter 
	    (fun elem_pat -> 
	      DEBUG_MSG "(%s): %s" heading_tag
		(elem_pat_to_string elem_pat)
	    ) !pat_list;
	END_DEBUG

      with 
      | e -> Xprint.error "(%s): %s" heading_tag (Printexc.to_string e); exit 1
    end;
    DEBUG_MSG "(%s): done." heading_tag;
    !pat_list


  method set_collapse_targets =
    collapse_targets <- self#get_pattern_list collapse_targets_tag

  method set_forced_to_be_collapsible =
    forced_to_be_collapsible <- self#get_pattern_list forced_to_be_collapsible_tag

  method set_to_be_notified =
    to_be_notified <- self#get_pattern_list to_be_notified_tag

  method set_boundary_nodes =
    boundary_nodes <- self#get_pattern_list boundary_nodes_tag

  method set_partition_nodes =
    partition_nodes <- self#get_pattern_list partition_nodes_tag

  method set_sequence_nodes =
    sequence_nodes <- self#get_pattern_list sequence_nodes_tag


  method set_cannot_be_keyroot =
    cannot_be_keyroot <- self#get_pattern_list cannot_be_keyroot_tag


  method private get_pattern_pair_list heading_tag =
    let doc = self#get_doc in

    let pat_pair_list = ref [] in

    let proc_pair nd =
      let cats = nd#sub_nodes in
      let l = ref [] in
      List.iter 
	(fun nd -> 
	  do_elem nd category_tag
	    (fun n ->
	      let c = pxp_att_value_to_string (n#attribute name_attr) in
	      let attr_cond = self#get_attr_cond n in
	      l := (c, attr_cond)::!l
	    )
	) cats;
      match !l with
      | c1::c2::_ ->
	  pat_pair_list := (c2, c1)::!pat_pair_list
      | _ -> WARN_MSG "not a pair"
    in

    let proc_heading nd =
      let pairs = nd#sub_nodes in
      List.iter
	(fun nd ->
	  do_elem nd pair_tag proc_pair
	) pairs
    in
    begin
      try
	let list = doc#root#sub_nodes in
	List.iter
	  (fun nd ->
	    do_elem nd heading_tag proc_heading
	  ) list;

	BEGIN_DEBUG
	  List.iter 
	    (fun (epat1, epat2) -> 
	      DEBUG_MSG "(%s): %s -- %s" heading_tag
		(elem_pat_to_string epat1) (elem_pat_to_string epat2)
	    ) !pat_pair_list;
	END_DEBUG

      with 
      | e -> Xprint.error "(%s): %s" heading_tag (Printexc.to_string e); exit 1
    end;
    DEBUG_MSG "(%s): done." heading_tag;
    !pat_pair_list


  method set_relabel_allowed_table =
    relabel_allowed_table <- self#get_pattern_pair_list relabel_allowed_tag

  method set_relabel_disallowed_table =
    relabel_disallowed_table <- self#get_pattern_pair_list relabel_disallowed_tag

  method set_anonymize2_rules =
    anonymize2_rules <- self#get_pattern_pair_list anonymize2_rules_tag

  method set_anonymize3_rules =
    anonymize3_rules <- self#get_pattern_pair_list anonymize3_rules_tag

  initializer
    self#parse;
    self#set_collapse_targets;
    self#set_forced_to_be_collapsible;
    self#set_relabel_allowed_table;
    self#set_relabel_disallowed_table;
    self#set_anonymize2_rules;
    self#set_anonymize3_rules;
    self#set_to_be_notified;
    self#set_boundary_nodes;
    self#set_partition_nodes;
    self#set_sequence_nodes;
    self#set_cannot_be_keyroot;

end (* of class Conf.c *)
