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

  method with_stmt_enabled = with_stmt_enabled
  method enable_with_stmt = with_stmt_enabled <- true
  method disable_with_stmt = with_stmt_enabled <- false

  method init =
    super#init

  initializer
    self#init

end (* of class Parser_aux.env *)


module type STATE_T = sig
  val env     : env
end


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

  let parse_error start_offset end_offset msg = 
    let (sl, sc), (el, ec), so, eo = get_range start_offset end_offset in
    let line, char = env#current_pos_mgr#get_position (eo + 1) in
    let head = sprintf "[%d:%d]" line char in
    fail_to_parse ~head msg



  let mkstmt so eo d = { stmt_desc=d; stmt_loc=(get_loc so eo) }
  let mksstmt so eo d = { sstmt_desc=d; sstmt_loc=(get_loc so eo) }
  let mkexpr so eo d = { expr_desc=d; expr_loc=(get_loc so eo) }
  let mkprim so eo d = { prim_desc=d; prim_loc=(get_loc so eo) }

  let mktestlist l c y = { list=l; comma=c; yield=y }

  let emptyarglist = Ast.Loc.dummy, [], None, None


end (* of functor Parser_aux.F *)



