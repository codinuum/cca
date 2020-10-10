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

let test() =

  let file = Sys.argv.(1) in

  let ns_mgr = Pxp_dtd.create_namespace_manager() in

  let transform_dtd dtd = 
(*
    dtd#namespace_manager#add_namespace "addr" "http://addresses.org";
    dtd#namespace_manager#add_namespace "nm" "http://names.org";
*)
    dtd
  in

  let config = 
    { XML.default_config with
      Pxp_types.enable_namespace_processing = Some ns_mgr
    } 
  in

  let spec = Pxp_tree_parser.default_namespace_spec in

  let doc = 
    XML.parse_file
      ~transform_dtd 
      ~config 
      ~spec
      file
  in

  let sprintf = Printf.sprintf in
  let printf = Printf.printf in

  let node_to_string nd =
    match nd#node_type with
    | Pxp_document.T_element name -> 
        sprintf "T_element(%s): normprefix=%s display_prefix=%s localname=%s ns_uri=%s" 
          name nd#normprefix nd#display_prefix nd#localname nd#namespace_uri

    | Pxp_document.T_data         -> sprintf "T_data:%s" nd#data

    | Pxp_document.T_super_root   -> "T_super_root"
    | Pxp_document.T_pinstr s     -> sprintf "T_pinstr(%s)" s
    | Pxp_document.T_comment      -> "T_comment"
    | Pxp_document.T_none         -> "T_none"
    | Pxp_document.T_attribute s  -> sprintf "T_attribute(%s)" s
    | Pxp_document.T_namespace s  -> sprintf "T_namespace(%s)" s
  in

  Pxp_document.iter_tree 
    ~post:(fun nd -> 

      printf "%s\n" (node_to_string nd);

    ) doc#root


let _ = test()
