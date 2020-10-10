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
(* java_lib_base.ml *)


module Label     = Java_label
module Fact      = Java_fact
module Tree      = Sourcecode.Tree (Label)

let sprintf = Printf.sprintf

class tree_builder options = 
  let _parser = new Lib.parser_c in
  object (self)
    inherit Lang_base.tree_builder

    method from_xnode = Java_tree.of_xnode options

    method build_tree file =
      Xprint.verbose options#verbose_flag "parsing \"%s\"...%!" file#path;
      try
        let ast = _parser#parse_file file in
        let tree = Java_tree.of_ast options ast in
        tree#set_source_info file;
        tree#set_parser_name Sjava.parser_name;
        tree
      with 
        Common.Parse_error(head, msg) -> 
          raise (Lang_base.Parse_error
                   (sprintf "[Java]%s" head, sprintf "%s: %s" msg file#path))

    initializer
      _parser#_set_verbose_flag options#verbose_flag;
      _parser#set_search_path_list options#search_path_list;
      _parser#_set_keep_going_flag options#keep_going_flag

  end
