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
(* lib.ml *)

open Common

module Aux = Parser_aux
module PB = Parserlib_base


class parser_c = object (self)
  inherit [Tokens_.token, Ast.c] PB.sb_c (new Aux.env) as super

  val mutable parser_main = fun _ -> Obj.magic ()
  val mutable scanner     = Obj.magic ()
  val mutable _parse      = fun () -> Obj.magic ()

  method enable_with_stmt = env#enable_with_stmt
  method disable_with_stmt = env#disable_with_stmt


  method parser_init =
    scanner#init


  method _parse = _parse()


  method __parse =
(*    self#parser_init; *)
    try
      let root = parser_main scanner#get_token in
      let ast = new Ast.c root in

      ast#set_lines_read env#current_pos_mgr#lines_read;
      ast#set_comment_regions env#comment_regions#get_offsets;
      ast#set_comment_LOC env#comment_regions#get_LOC;
      ast#set_missed_regions env#missed_regions#get_offsets;
      ast#set_missed_LOC env#missed_regions#get_LOC;
(*
      ast#set_ignored_regions (env#ignored_regions#get_offsets);
      ast#set_ignored_LOC (env#ignored_regions#get_LOC);
*)
      ast
    with 
    | Parsing.Parse_error ->
	let l, c = env#current_pos_mgr#get_current_position in
        fail_to_parse 
          ~head:(Printf.sprintf "[%s:%d:%d]" env#current_filename l c)
          "syntax error"


  initializer
    let module S = struct 
      let env      = env
    end 
    in
    let module U = Ulexer.F (S) in
    let module P = Parser.Make (S) in
    parser_main <- PB.mkparser P.main;
    scanner <- new U.scanner;
    _parse <- 
      (fun () ->
	try
	  self#__parse
	with
	| P.Error ->
	    let l, c = env#current_pos_mgr#get_current_position in
            fail_to_parse 
              ~head:(Printf.sprintf "[%s:%d:%d]" env#current_filename l c)
              "syntax error"
      )


end (* of class Lib.parser_c *)

