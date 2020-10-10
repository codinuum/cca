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
(* XML.ml *)


module Comp = Compression

let sprintf = Printf.sprintf

let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
(*
let rules = [
  "&",  "&amp;";
  "<",  "&lt;";
  ">",  "&gt;";
  "\"", "&quot;";
  "'",  "&apos;";
]

let inv_rules = List.map (fun (o, n) -> n, o) rules

let mkpat rules =
  Str.regexp
    (String.concat "\\|"
       (List.map (fun (x, _) -> sprintf "\\(%s\\)" x) rules))

let subst rules =
  let pat = mkpat rules in
  let conv s =
    let ss = Str.matched_string s in
    try
      List.assoc ss rules
    with
      Not_found -> s
  in
  Str.global_substitute pat conv


let encode_string = subst rules

let decode_string = subst inv_rules
*)


(*let unsafe_chars = "'\t\n"^Netencoding.Html.unsafe_chars_html4*)
let unsafe_chars = Netencoding.Html.unsafe_chars_html4

let encode_string s =
  let x = String.escaped s in
  Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 ~unsafe_chars () x

let decode_string s =
  let x =
    Netencoding.Html.decode ~entity_base:`Xml ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () s
  in
  Scanf.unescaped x


let get_local_part qualified_name =
  try
    let i = String.rindex qualified_name ':' in
    let len = (String.length qualified_name) - i - 1 in
    String.sub qualified_name (i+1) len
  with
    Not_found -> qualified_name


let node_type_to_string = function
  | Pxp_document.T_element name -> sprintf "T_element(%s)" name
  | Pxp_document.T_data         -> "T_data"
  | Pxp_document.T_super_root   -> "T_super_root"
  | Pxp_document.T_pinstr s     -> sprintf "T_pinstr(%s)" s
  | Pxp_document.T_comment      -> "T_comment"
  | Pxp_document.T_none         -> "T_none"
  | Pxp_document.T_attribute s  -> sprintf "T_attribute(%s)" s
  | Pxp_document.T_namespace s  -> sprintf "T_namespace(%s)" s


(* parsing XML *)

class warner =
  object
    method warn w =
      Xprint.warning ~head:"[XML]" "Pxp: %s" w
  end

let default_config = 
  { Pxp_types.default_config with 
    Pxp_types.warner   = new warner; 
    Pxp_types.encoding = `Enc_utf8;
  } 

let parse_xchannel 
    ?(transform_dtd=(fun x -> x))
    ?(config=default_config) 
    ?(spec=Pxp_tree_parser.default_spec)
    (ch : Xchannel.in_channel) 
    =
  try
    let obj_ch = ch#in_obj_channel in

    let source = Pxp_types.from_obj_channel obj_ch in

    let doc = 
      Pxp_tree_parser.parse_wfdocument_entity ~transform_dtd config source spec 
    in
    ch#close_extra;
    doc

  with
    e -> failwith ("XML.parse_xchannel: "^(Pxp_types.string_of_exn e))



let parse_file 
    ?(transform_dtd=(fun x -> x))
    ?(config=default_config) 
    ?(spec=Pxp_tree_parser.default_spec)
    (fname : string) 
    =
  let comp = Comp.from_filename fname in
  if comp#is_compressed then begin
    let ch = new Xchannel.in_channel ~comp (Xchannel.Source.of_file fname) in
    parse_xchannel ~transform_dtd ~config ~spec ch
  end
  else begin
    try
      let source = Pxp_types.from_file fname in
      let doc = 
        Pxp_tree_parser.parse_wfdocument_entity ~transform_dtd config source spec 
      in
      doc

    with
      e -> failwith ("XML.parse_file: "^(Pxp_types.string_of_exn e))
  end

