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
(* verilog/v_lib_p.ml *)

include V_lib_base

let _ =
  Lang_base.register Sverilog.parser_name
    (new Lang_base.c
       ~make_tree_builder:(new tree_builder)
       ~extract_fact:Fact.extract
       ~node_filter:Fact.node_filter
    )
