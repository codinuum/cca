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
(* lib.ml *)

module PB = Parserlib_base
module Aux = Parser_aux


class parser_c = object (self)
  inherit [Tokens_.token, Ast.c] PB.sb_c (new Aux.env) as super

  val mutable parser_main = fun ul -> Obj.magic ()
  val mutable scanner     = Obj.magic ()
  val mutable _parse      = fun () -> Obj.magic ()

  method parser_init =
    env#begin_scope();
    env#set_last_rawtoken (Obj.repr Tokens_.T.EOF)


  method _parse = _parse()


  method __parse =
(*    self#parser_init; *)
    try
      let srcdir =
        let current_file = env#current_source#file in
        let tree = current_file#tree in
	try
	  match Common.guess_src_dir current_file with
	  | Common.SD_unnamed s ->
              let dir = tree#get_entry s in
	      env#classtbl#add_package ~dir "";
	      dir

	  | Common.SD_named s -> tree#get_entry s
	with 
	  Failure mes -> 
	    (*WARN_MSG mes;*)
            Xprint.verbose env#verbose "%s" mes;
	    Storage.dummy_entry
      in
      if srcdir != Storage.dummy_entry then begin
	DEBUG_MSG "guessed source directory: \"%s\"" srcdir#path;
	Xprint.verbose env#verbose "guessed source directory: \"%s\"" srcdir#path;
	env#classtbl#set_source_dir srcdir;
      end;

      let cu = parser_main scanner#get_token in
      let ast = new Ast.c cu in
      ast#set_lines_read env#current_pos_mgr#lines_read;
      ast#set_comment_regions env#comment_regions#get_offsets;
      ast#set_comment_LOC env#comment_regions#get_LOC;
      ast#set_missed_regions env#missed_regions#get_offsets;
      ast#set_missed_LOC env#missed_regions#get_LOC;

      let resolve_qname nattr_ref qname =
        DEBUG_MSG "resolving \"%s\"..." qname;
        let attrs = env#lookup_global_qname qname in
        match attrs with
        | [iattr] -> begin
            try
              nattr_ref := Ast.iattr_to_nattr iattr
            with
              _ -> ()
        end
        | _ -> ()
      in

      let rec resolve_name name =
        let qname = Printer.name_to_simple_string name in
	DEBUG_MSG "resolving \"%s\"..." qname;
	match name.Ast.n_desc with
	| Ast.Nsimple(nattr_r, id) -> begin
            match !nattr_r with
	    | Ast.NAtype _ -> env#finalize_name_attribute nattr_r
            | Ast.NAambiguous | Ast.NAunknown -> resolve_qname nattr_r qname
            | _ -> ()
        end
	| Ast.Nqualified(nattr_r, n, _) -> begin
            begin
              match !nattr_r with
              | Ast.NAtype _ -> env#finalize_name_attribute nattr_r
              | Ast.NAambiguous | Ast.NAunknown -> resolve_qname nattr_r qname
              | _ -> ()
            end;
	    resolve_name n
        end
      in
      ast#iter_name resolve_name;
      ast
    with
    | Parsing.Parse_error ->
	let l, c = env#current_pos_mgr#get_current_position in
        Common.fail_to_parse 
          ~head:(Printf.sprintf "[%s:%d:%d]" env#current_filename l c)
          "syntax error" 

    | Ulexer.EOF_reached ->
        Common.fail_to_parse 
          ~head:(Printf.sprintf "[%s]" env#current_filename)
          "EOF reached unexpectedly"

  initializer
    let module S = struct 
      let env      = env
    end 
    in
    let module U = Ulexer.F (S) in
    let module P = Parser.Make (S) in
    let module Scan = Scanner.F (S) in
    parser_main <- PB.mkparser P.main;
    scanner <- new Scan.c;
    _parse <- 
      (fun () ->
	try
	  self#__parse
	with
	| P.Error ->
	    let l, c = env#current_pos_mgr#get_current_position in
            Common.fail_to_parse 
              ~head:(Printf.sprintf "[%s:%d:%d]" env#current_filename l c)
              "syntax error"
      )

end (* of class Lib.parser_c *)

