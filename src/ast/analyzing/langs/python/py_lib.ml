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
(* py_lib.ml *)

include Py_lib_base

module Analyzing = Analyzing.F (Label)
module DF        = Delta_format.Format

let extract_change options tree1 tree2 uidmapping edits =
  [], [], [], (Xset.create 0) (* not yet *)


class tree_patcher options tree_factory = object
  inherit Lang.tree_patcher

  method _patch = DF._patch options tree_factory

  method patch = DF.patch options tree_factory

end

let _ =
  Lang.register Spython.parser_name
    (new Lang.c
       ~make_tree_comparator:(new Analyzing.tree_comparator)
       ~make_tree_builder:(new tree_builder)
       ~extract_change:extract_change
       ~extract_fact:extract_fact
       ~make_tree_patcher:(new tree_patcher)
    )
