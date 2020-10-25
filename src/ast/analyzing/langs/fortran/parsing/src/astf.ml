(*
   Copyright 2013-2018 RIKEN
   Copyright 2018-2020 Chiba Institude of Technology

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

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)



open Printf

open Common
open Ast
module Loc = Astloc


let fortran_ns = "http://codinuum.com/ontologies/2013//05/fortran-entity#"
let src_ns = "http://codinuum.com/ontologies/2012/10/source-code-entity#"
let start_tag = sprintf "<astf xmlns='%s' xmlns:src='%s'>" fortran_ns src_ns
let end_tag = "</astf>"

let astf_ext = ".astf"



let dump_in_xml_ch root ch =
  let attrs_to_string attrs =
    String.concat "" (List.map (fun (a, v) -> sprintf " %s='%s'" a v) attrs)
  in
  let get_loc_attr loc =
    let pos = 
      sprintf "%d:%d(%d)-%d:%d(%d)" 
        loc.Loc.start_line loc.Loc.start_char loc.Loc.start_offset
        loc.Loc.end_line loc.Loc.end_char loc.Loc.end_offset
    in
    ["src:pos",pos;"src:file",loc.Loc.filename]
  in
  let get_tag_data nd =
    let name, _attrs = nd#to_tag in
    let ba =
      match nd#binding with
      | B.NoBinding -> []
      | _ -> ["src:bid",BID.to_raw (B.get_bid nd#binding)]
    in
    let attrs = _attrs @ ba @ (get_loc_attr nd#loc) in
    name, attrs
  in
  let rec put nd =
    let children = nd#children in
    let name, attrs = get_tag_data nd in
    if children = [] then
      fprintf ch "<%s%s/>" name (attrs_to_string attrs)
    else begin
      fprintf ch "<%s%s>" name (attrs_to_string attrs);
      List.iter put children;
      fprintf ch "</%s>" name
    end
  in

  output_string ch XML.header;
  output_string ch start_tag;

  let children = root#children in
  let name, _attrs = root#to_tag in
  let loc_attr =
    match children with
    | [] -> []
    | [nd] -> get_loc_attr nd#loc
    | nd::nds -> 
        let lloc = LLoc.merge nd#lloc (Xlist.last nds)#lloc in
        let loc = lloc#get_loc in
        get_loc_attr loc
  in
  let attrs = _attrs @ loc_attr in

  if children = [] then
    fprintf ch "<%s%s/>" name (attrs_to_string attrs)
  else begin
    fprintf ch "<%s%s>" name (attrs_to_string attrs);
    List.iter put children;
    fprintf ch "</%s>" name
  end;

  output_string ch end_tag


let dump_in_xml root =
  dump_in_xml_ch root Stdlib.stdout

let save_in_xml fname root =
  Xfile.dump fname (dump_in_xml_ch root)
