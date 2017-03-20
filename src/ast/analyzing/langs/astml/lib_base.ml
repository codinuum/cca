(*
   Copyright 2012-2017 Codinuum Software Lab <http://codinuum.com>

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
(* astml/lib_base.ml *)

open Common

let sprintf = Printf.sprintf

let comp_ext_list = Sastml.comp_ext_list

let decomp = Sastml.decomp

let gen_temp_file_path () = Filename.temp_file "cca_" Astml.extension

let ns_mgr = Pxp_dtd.create_namespace_manager()

let config = 
  { XML.default_config with 
    Pxp_types.enable_namespace_processing = Some ns_mgr;
  } 

let spec = Pxp_tree_parser.default_namespace_spec

let setup_ns_mgr ns_mgr =
  ns_mgr#add_namespace Astml.c_prefix       Astml.c_ns;
  ns_mgr#add_namespace Astml.cx_prefix      Astml.cx_ns;
  ns_mgr#add_namespace Astml.ccx_prefix     Astml.ccx_ns;
  ns_mgr#add_namespace Astml.java_prefix    Astml.java_ns;
  ns_mgr#add_namespace Astml.python_prefix  Astml.python_ns;
  ns_mgr#add_namespace Astml.verilog_prefix Astml.verilog_ns

let transform_dtd dtd =
  setup_ns_mgr dtd#namespace_manager;
  dtd

let xexists file =
  let tree = file#tree in
  List.exists
    (fun ext ->
      let f = new Storage.file tree (file#path^ext) in
      f#exists
    ) comp_ext_list

let check_comp file = (* AST -> (AST[.COMPRESSION] * IS_COMPRESSED * COMP_EXT) *)
  let rec sea = function
    | [] -> 
	failwith 
          (sprintf "ASTML file \"%s{%s}\" not found" 
             file#path (Xlist.to_string (fun x -> x) "," comp_ext_list))

    | ext::rest ->
	let f = new Storage.file file#tree (file#path^ext) in
	if f#exists then 
	  (f, ext <> "", ext) 
	else 
	  sea rest
  in
  sea comp_ext_list

	

let build_tree options file =

  DEBUG_MSG "parsing \"%s\"..." file#path;

  let doc =
    let path = file#get_local_file in
    try
      XML.parse_file ~transform_dtd ~config ~spec path
    with
      _ ->
        let rec find = function
          | ext::rest -> begin
              let path_ = path^ext in
              try
                XML.parse_file ~transform_dtd ~config ~spec path_
              with
                exn ->
                  if rest = [] then
                    Xprint.failure "\"%s\": not found" path_
                  else
                    find rest
          end
          | [] -> raise Not_found
        in
        find comp_ext_list
  in

  DEBUG_MSG "parsed";

  BEGIN_DEBUG
    DEBUG_MSG "root node: %s" (XML.node_type_to_string doc#root#node_type);
    List.iter
      (fun (a, v) -> DEBUG_MSG "%s = '%s'" a (Conf.pxp_att_value_to_string v))
      doc#root#attributes;
  END_DEBUG;

  (* checking root element *)
  if doc#root#node_type = Pxp_document.T_element Astml.astml_tag then
    () (* OK *)
  else
    Xprint.failure "\"%s\": not an ASTML file: root=%s"
      file#path (XML.node_type_to_string doc#root#node_type);

  let parser_name = 
    try
      Conf.pxp_att_value_to_string (doc#root#attribute Label.parser_attr_name)
    with 
    | Not_found -> 
	Xprint.failure "\"%s\" does not have %s attribute in root node" 
	  file#path Astml.parser_attr_name
  in

  DEBUG_MSG "building tree...";
  let tree = Tree.of_file options parser_name file doc in
  DEBUG_MSG "done.";

  tree#set_parser_name parser_name;

  tree
(* end of func build_tree *)



type astml_header =
    { mutable h_parser        : string;
      mutable h_source        : string;
      mutable h_source_digest : string;
    }

let make_null_header() =
    { h_parser        = "";
      h_source        = "";
      h_source_digest = "";
    }

exception Got_header of astml_header
exception Header_not_found

let get_header astml =
  let ns_mgr = Pxp_dtd.create_namespace_manager() in
  let _ = setup_ns_mgr ns_mgr in
  let config = 
    { Pxp_types.default_config with 
      Pxp_types.enable_namespace_processing = Some ns_mgr
    }
  in
  let source = Pxp_types.from_file astml in
  let ent_mgr = Pxp_ev_parser.create_entity_manager config source in

  let proc_ev = function
    | Pxp_types.E_start_tag(tag_name, attrs, _, _) ->
	let prefix, tag = Pxp_event.namespace_split tag_name in
	if tag = "astml" (* && (ns_mgr#get_primary_uri prefix) = Astml.default_ns *) then
	  let h = make_null_header() in
	  List.iter
	    (fun (a, v) ->
	      let p, la = Pxp_event.namespace_split a in
	      if (ns_mgr#get_primary_uri p) = Astml.ast_ns then
		if la = Astml.local_parser_attr_name then
		  h.h_parser <- v
		else if la = Astml.local_source_attr_name then
		  h.h_source <- v
		else if la = Astml.local_source_digest_attr_name then
		  h.h_source_digest <- v

	    ) attrs;
	  if h.h_parser <> "" && h.h_source <> "" && h.h_source_digest <> "" then begin
	    DEBUG_MSG
	      "got source digest from the header: source=\"%s\" digest=\"%s\"" 
	      h.h_source h.h_source_digest;
	    raise (Got_header h)
	  end

    | _ -> ()
  in
  try
    Pxp_ev_parser.process_entity
      config (`Entry_document []) ent_mgr
      proc_ev;
    raise Header_not_found
  with 
  | Pxp_types.At(_, Got_header h) -> h
  | e -> failwith (Pxp_types.string_of_exn e)


module T         = Sourcecode.Tree (Label)
module FactCCX   = Fact.CCX (Label)


let file_digest_hex file =
  let ccs_mode = file#get_extension = Astml.ccs_ext in

  let get_digest_hex decompressed =
    if ccs_mode then
      Xhash.digest_hex_of_file file#tree#hash_algo decompressed
    else
      try
	let h = get_header decompressed in
        decode_digest h.h_source_digest
      with
	Not_found -> Xprint.failure "\"%s\": malformed ASTML file" file#path
  in

  let ast, comp_flag, comp_ext = check_comp file in

  if comp_flag then begin
    let astml = gen_temp_file_path() in
    decomp comp_ext ast astml;
    let d = get_digest_hex astml in
    begin
      try
	Sys.remove astml
      with 
	Sys_error s -> Xprint.failure "cannot remove: %s" s
    end;
    d
  end
  else
    let d = get_digest_hex ast#get_local_file in
    d

let extract_fact options cache_path tree =
()


class tree_builder options = object (self)
  inherit Lang_base.tree_builder
  method from_xnode = T.of_xnode options
  method build_tree file = build_tree options file
end



(* for external parsers *)

let ext_file_digest_hex file = 
  try
    Xhash.to_hex file#digest
  with
  | e -> Xprint.failure "failed to compute file digest: %s" file#path


class ext_tree_builder xpname options = object (self)
  inherit Lang_base.tree_builder

  method from_xnode = T.of_xnode options

  method private get_astml_file src = 
    let astml_file = new Storage.file src#tree (src#path^Astml.extension) in
    if xexists astml_file then
      astml_file
    else begin
      DEBUG_MSG "not found: \"%s\"" astml_file#path;
      let cache_path = options#get_cache_path_for_file1 src in
      let p = Filename.concat cache_path Astml.default_file_name in
      let astml_file = Fs.file_of_path options p in
      if xexists astml_file then
        astml_file
      else begin
        DEBUG_MSG "not found: \"%s\"" astml_file#path;
        let spath = src#get_local_file in
        try
          let cmd = sprintf "%s %s" (options#get_external_parser xpname) spath in
          DEBUG_MSG "parsing \"%s\"..." spath;
          DEBUG_MSG "command=\"%s\"" cmd;
          if Sys.command cmd <> 0 then
            warning_msg "failed to execute \"%s\"" cmd
          else begin
            let apath = spath^Astml.extension in
            let p = astml_file#fullpath in
            Cache.prepare_dir options#default_dir_permission (Filename.dirname p);
            Sys.rename apath p;
            let cmd = sprintf "gzip %s" p in
            DEBUG_MSG "compressing \"%s\"..." p;
            DEBUG_MSG "command=\"%s\"" cmd;
            if Sys.command cmd <> 0 then
              warning_msg "failed to execute \"%s\"" cmd
          end;
          astml_file
        with
          Not_found -> raise (Astml.External_parser_not_found xpname)
      end
    end

  method build_tree file = build_tree options (self#get_astml_file file)
end
