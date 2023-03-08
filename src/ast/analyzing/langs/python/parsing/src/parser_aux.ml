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
(* parser_aux.ml *)

open Printf
open Common
open Ast


class env = object (self)
  inherit [Source_base.c] Env_base.c as super

  val mutable with_stmt_enabled = true (* always enabled in v2.6+ *)

  val mutable keep_going_flag = true
  val mutable shift_flag = false
  val mutable last_token = Obj.repr ()
  val mutable paren_level = 0
  val mutable brace_level = 0
  val mutable bracket_level = 0
  val mutable block_level = 0

  method with_stmt_enabled = with_stmt_enabled
  method enable_with_stmt = with_stmt_enabled <- true
  method disable_with_stmt = with_stmt_enabled <- false

  method keep_going_flag = keep_going_flag
  method _set_keep_going_flag b = keep_going_flag <- b

  method shift_flag = shift_flag
  method set_shift_flag () =
    DEBUG_MSG "set";
    shift_flag <- true
  method clear_shift_flag () =
    DEBUG_MSG "clear";
    shift_flag <- false

  method last_token = last_token
  method set_last_token o = last_token <- o

  method reset_paren_level () = paren_level <- 0;
  method paren_level = paren_level
  method open_paren () = paren_level <- paren_level + 1
  method close_paren () = paren_level <- paren_level - 1

  method brace_level = brace_level
  method open_brace () = brace_level <- brace_level + 1
  method close_brace () = brace_level <- brace_level - 1

  method bracket_level = bracket_level
  method open_bracket () = bracket_level <- bracket_level + 1
  method close_bracket () = bracket_level <- bracket_level - 1

  method block_level = block_level
  method open_block () = block_level <- block_level + 1
  method close_block () = block_level <- block_level - 1

  method init =
    super#init

  initializer
    self#init

end (* of class Parser_aux.env *)


module type STATE_T = sig
  val env     : env
end

let warning_loc loc = PB.parse_warning_loc ~head:"[Python]" loc

module F (Stat : STATE_T) = struct

  open Stat

  let get_range start_offset end_offset =
    let pos_mgr = env#current_pos_mgr in
    let start_line, start_char = pos_mgr#get_position start_offset in
    let end_line, end_char = pos_mgr#get_position end_offset in
(*
    DEBUG_MSG "%d:%d-%d:%d(%d-%d)"
      start_line start_char end_line end_char start_offset end_offset;
*)
    (start_line, start_char), (end_line, end_char), start_offset, end_offset

  let get_loc start_offset end_offset =
    let (sl, sc), (el, ec), so, eo = get_range start_offset end_offset in
    let loc = Loc.make so eo sl sc el ec in
    loc

  let parse_warning start_ofs end_ofs =
    let loc = get_loc start_ofs end_ofs in
    warning_loc loc

  let parse_error start_ofs end_ofs (fmt : ('a, unit, string, 'b) format4) : 'a =
    let loc = get_loc start_ofs end_ofs in
    let loc_str = Astloc.to_string ~short:false ~prefix:"[" ~suffix:"]" loc in
    Printf.ksprintf
      (fun msg ->
        if env#keep_going_flag then
          Printf.fprintf stderr "[Python][WARNING]%s%s %s\n%!" PB.cmd_name loc_str msg
        else
          fail_to_parse ~head:loc_str msg
      ) fmt

  let mkstmt so eo d = { stmt_desc=d; stmt_loc=(get_loc so eo) }
  let mksstmt so eo d = { sstmt_desc=d; sstmt_loc=(get_loc so eo) }
  let mkexpr so eo d = { expr_desc=d; expr_loc=(get_loc so eo) }
  let mkprim so eo d = { prim_desc=d; prim_loc=(get_loc so eo) }
  let mkprimexpr so eo d = { expr_desc=(Eprimary (mkprim so eo d)); expr_loc=(get_loc so eo) }
  let mkde so eo d = { delem_desc=d; delem_loc=(get_loc so eo) }

  let mktestlist l c y = { list=l; comma=c; yield=y }

  let emptyarglist = Ast.Loc.dummy, []
  let emptyvarargslist = emptyarglist
  let emptytypedargslist = emptyarglist

  let mkerrexpr so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { expr_desc=Eerror; expr_loc=loc }

  let mkerrsstmt so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { sstmt_desc=SSerror; sstmt_loc=loc }

  let mkerrstmt so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { stmt_desc=Serror; stmt_loc=loc }

  let mkmarkerstmt so eo s =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { stmt_desc=Smarker s; stmt_loc=loc }

end (* of functor Parser_aux.F *)



