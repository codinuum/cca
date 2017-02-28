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
include Lib_base

let _ =
  Lang_base.register Sastml.parser_name
    (new Lang_base.c 
       ~make_tree_builder:(new tree_builder)
       ~get_cache_key:file_digest_hex
       ~extract_fact:extract_fact
    )

(* for external parsers *)

let _ =
  Lang_base.register_external Sastml.parser_name Sastml.xparser_name_ccx
    (fun subname name ->
      new Lang_base.c 
        ~subname
        ~make_tree_builder:(new ext_tree_builder subname)
        ~get_cache_key:ext_file_digest_hex
        ~extract_fact:FactCCX.extract
        ~node_filter:FactCCX.node_filter
        name
    )
