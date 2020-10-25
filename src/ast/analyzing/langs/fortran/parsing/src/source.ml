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



class omp_cc_lines = object (self) (* OpenMP Conditional Compilation *)

  val ranges = Xset.create 0

  val ranges_QCC = Xset.create 0 (* quasi-CC *)

  val masks = Xset.create 0

  val heads = Xset.create 0

  method init =
    Xset.clear ranges

  method add ln = 
    DEBUG_MSG "adding %d" ln;
    Xset.add ranges ln

  method remove ln =
    DEBUG_MSG "removing %d" ln;
    Xset.remove ranges ln

  method add_QCC ln = 
    DEBUG_MSG "adding %d" ln;
    Xset.add ranges_QCC ln

  method is_CC_line ln =
    Xset.mem ranges ln

  method is_QCC_line ln =
    Xset.mem ranges_QCC ln

  method is_CC_normal_line ln =
    not (self#is_CC_line ln)

  method _set_head ln =
    Xset.add heads ln

  method _is_head ln =
    Xset.mem heads ln

  method is_head ?(mask=false) ?(adjust=0) incomplete ln =
    let prev = ln - 1 - adjust in
    let b =
      if Xset.mem masks ln then
        false
      else
        (self#is_CC_normal_line prev) && 
        (self#is_CC_line ln)
    in
    if b && mask then
      Xset.add masks ln;

    if b then
      self#_set_head ln;

    if 
      (not (self#_is_head prev) || incomplete) && 
      (self#is_QCC_line ln) && (self#is_CC_line prev) 
    then
      self#add ln;

    DEBUG_MSG "%d -> %B (mask=%B,adjust=%d)" ln b mask adjust;
    b

  method is_normal_head ?(mask=false) ?(adjust=0) ln =
    let prev = ln - 1 - adjust in
    DEBUG_MSG "is_CC_line(%d):%B, is_CC_normal_line(%d):%B" 
      prev (self#is_CC_line prev) ln (self#is_CC_normal_line ln);
    let b =
      if Xset.mem masks ln then
        false
      else
        (self#is_CC_line prev) && 
        (self#is_CC_normal_line ln)
    in
    if b && mask then
      Xset.add masks ln;

    DEBUG_MSG "%d -> %B (mask=%B,adjust=%d)" ln b mask adjust;
    b

  method is_tail ln =
    let b = self#is_CC_line ln in
    DEBUG_MSG "%d -> %B" ln b;
    b

end (* class omp_cc_lines *)


class c file = object (self)
  inherit Source_base.c file as super

  val lang_config = new Common.LangConfig.c

  method lang_config = lang_config

  method parse_d_lines = lang_config#parse_d_lines

  method lang_spec = lang_config#spec
  method set_spec_F77 = lang_config#set_spec_F77
  method set_spec_F90 = lang_config#set_spec_F90
  method set_spec_F95 = lang_config#set_spec_F95
  method set_spec_F2003 = lang_config#set_spec_F2003
  method set_spec_F2008 = lang_config#set_spec_F2008

  method lang_exts = lang_config#exts
  method add_ext_Fujitsu = lang_config#add_ext_Fujitsu
  method add_ext_IBM     = lang_config#add_ext_IBM
  method add_ext_Intel   = lang_config#add_ext_Intel
  method add_ext_PGI     = lang_config#add_ext_PGI
  method add_ext_PGI_CUDA = lang_config#add_ext_PGI_CUDA
  method add_ext_Apollo  = lang_config#add_ext_Apollo

  method set_source_form form = 
    DEBUG_MSG "%s" self#path;
    lang_config#set_source_form form

  method is_free_source_form =
    DEBUG_MSG "%s" self#path;
    lang_config#is_free_source_form

  method is_fixed_source_form =
    DEBUG_MSG "%s" self#path;
    lang_config#is_fixed_source_form

  method max_line_length =
    lang_config#max_line_length

  method set_max_line_length_fixed n =
    DEBUG_MSG "%s" self#path;
    lang_config#set_max_line_length_fixed n

  method set_max_line_length_free n =
    DEBUG_MSG "%s" self#path;
    lang_config#set_max_line_length_free n

  val omp_cc_lines = new omp_cc_lines

  method omp_cc_lines = omp_cc_lines
      
end
