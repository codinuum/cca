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
(* cpp/cpp_lib.ml *)

include Cpp_lib_base

module Analyzing = Analyzing.F (Label)
module Change    = Cpp_change.F (Label)

let elaborate_edits
    options
    (cenv : (Tree.node_t, Tree.c) Comparison.c)
    uidmapping
    edits
    =
  let mkfilt = Edit.mkfilt Fact.getlab in
(*
  let is_type_decl_stmt = mkfilt Label.is_type_decl_stmt in
*)

  let is_named = mkfilt Label.is_named in

  let filters = [|
    is_named;
  |]
  in
  let handle_weak = not options#dump_delta_flag in
  Edit.adjust_renames ~handle_weak options cenv uidmapping edits filters


let _ =
  Lang.register Scpp.parser_name
    (new Lang.c
       ~make_tree_comparator:(new Analyzing.tree_comparator)
       ~make_tree_builder:(new tree_builder)
       ~extract_change:Change.extract
       ~extract_fact:Fact.extract
       ~node_filter:Fact.node_filter
       ~node_pair_filter:Fact.node_pair_filter
       ~elaborate_edits:(Some elaborate_edits)
    )
