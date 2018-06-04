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
(* stat.ml *)


module C = Compression

let printf  = Printf.printf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf

let fact_file_name = "fact.nt"

let info_file_name    = "info"
let summary_file_name = "summary"
let stat_file_name    = "stat"
let source_file_name  = "source"
let parser_file_name  = "parser"




exception Stat_not_found
exception Malformed_stat of string
	
let fscanf ic =
  let ib = Scanf.Scanning.from_channel ic in
  Scanf.bscanf ib

  
let scan ?(max_retry_count=10) scanner path = 
  let rec _scan n path (scanner : in_channel -> 'a) =
    try
      let ch = open_in path in
      try
	let res = scanner ch in
	begin
	  try
	    close_in ch
	  with 
	    Sys_error s -> WARN_MSG s
	end;
	res
      with
      | Scanf.Scan_failure _
      | Failure _ 
      | End_of_file
      | Invalid_argument _ -> 
	  (try
	    close_in ch;
	  with _ -> ());
	  raise (Malformed_stat path)
    with 
    | Sys_error s -> (* WARN_MSG s; *) raise Stat_not_found
    | Malformed_stat _ ->
	if n > max_retry_count then 
	  raise Stat_not_found
	else begin
	  Xprint.warning "malformed cache \"%s\": retrying...(%d)" path n;
	  Unix.sleep 1;
	  _scan (n+1) path scanner
	end
  in
  let x = _scan 1 path scanner in
  x

let scan_paths ?(max_retry_count=10) scanner paths =
  let scan_path = scan ~max_retry_count:1 scanner in
  let rec doit count ps =
    if count < 0 then
      raise Stat_not_found
    else
      match ps with
      | [] -> raise Stat_not_found
      | [x] -> begin
          try
            scan_path x.Cache.sr_path
          with
            _ -> 
              Unix.sleep 1; 
              doit (count-1) ps
      end
      | x::xs -> begin
          try
            scan_path x.Cache.sr_path
          with
            _ -> 
              Unix.sleep 1; 
              doit (count-1) xs
      end
  in
  doit max_retry_count paths



module File = struct

  type info = { 
      i_nodes      : int; 
      i_units      : int; 
      i_LOC        : int;
      i_missed_LOC : int;
    }

  let dummy_info = {
    i_nodes      = 0; 
    i_units      = 0; 
    i_LOC        = 0;
    i_missed_LOC = 0;
  }  

  let info_fmt () = 
    "nodes: %d\n" ^^
    "units: %d\n" ^^
    "LOC: %d\n" ^^
    "missed LOC: %d\n"


  let mkinfo n u l m =
    { i_nodes      = n; 
      i_units      = u; 
      i_LOC        = l;
      i_missed_LOC = m;
    }  

  let show_info info =
    let fmt = info_fmt() in
    printf fmt info.i_nodes info.i_units info.i_LOC info.i_missed_LOC


  let dump_info_ch info ch =
    let fmt = info_fmt() in
    fprintf ch fmt info.i_nodes info.i_units info.i_LOC info.i_missed_LOC

  let get_tree_info tree =
    let units = tree#get_units_to_be_notified in
    mkinfo tree#initial_size (List.length units) tree#total_LOC tree#misparsed_LOC

  let dump_info cache_path tree =
    let path = Filename.concat cache_path info_file_name in
    let info = get_tree_info tree in
    Xfile.dump path (dump_info_ch info)

  let scan_info paths =
    scan_paths
      (fun ch ->
        fscanf ch (info_fmt())
	  (fun nnodes nunits tloc mloc ->
	    { i_nodes      = nnodes; 
	      i_units      = nunits; 
	      i_LOC        = tloc; 
	      i_missed_LOC = mloc; 
	    }
	  )
      ) paths


end (* module Stat.File *)



module Dir = struct

  let info_fmt () =
    "nodes:        %d\n" ^^
    "source files: %d\n" ^^
    "AST nodes:    %d\n"

  let dump_info_ch dtree ast_nodes ch =
    let fmt = info_fmt() in
    fprintf ch fmt 
      dtree#initial_size 
      (List.length dtree#get_whole_initial_leaves)
      ast_nodes


end (* module Stat.Dir *)


let _get_vkey vkind ver =
  if not (Entity.vkind_is_unknown vkind) && ver <> "" then
    Entity.mkvkey vkind ver
  else
    ""

let get_vkey tree = _get_vkey tree#vkind tree#version

let dump_source_path_ch p ch =
  Printf.fprintf ch "%s\n" p

let _dump_source_path cache_path (k, v) spath =
  let vkey = _get_vkey k v in
  let a = if vkey = "" then "" else "."^vkey in
  let path = (Filename.concat cache_path source_file_name)^a in
  Xfile.dump path (dump_source_path_ch spath)

let dump_source_ch tree = dump_source_path_ch tree#source_path

let _dump_source cache_path tree =
  _dump_source_path cache_path (tree#vkind, tree#version) tree#source_path

let dump_source options file tree =
  let cache_path = options#get_cache_path_for_file1 file in
  _dump_source cache_path tree

let dump_file_info options file tree =
  let cache_path = options#get_cache_path_for_file1 file in
  File.dump_info cache_path tree

let dump_parser_name_ch pname ch =
  Printf.fprintf ch "%s\n" pname

let dump_parser_name file parser_name =
  Xfile.dump file (dump_parser_name_ch parser_name)

let dump_parser_ch tree = dump_parser_name_ch tree#parser_name

let _dump_parser cache_path tree =
  let path = Filename.concat cache_path parser_file_name in
  Xfile.dump path (dump_parser_ch tree)

let dump_parser options file tree =
  let cache_path = options#get_cache_path_for_file1 file in
  _dump_parser cache_path tree

