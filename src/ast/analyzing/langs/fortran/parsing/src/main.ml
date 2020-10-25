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

(* 
 * A parser for Fortran
 *
 * main.ml
 *
 *)

open Common
module Aux = Parser_aux

let compile_mode = ref false
let filename = ref ""
let arg_count = ref 0
let dump_ast = ref false

let error msg = 
  print_string "[ERROR] ";
  print_string msg; print_newline(); exit 0

let read_hint file =
  let _parser = new Lib.parser_c in
  let _ = _parser#parse_file file in
  _parser#macrotbl


let _parser = new Lib.parser_c

let options = new Basic_options.c

let _ = 
  Arg.parse 
    [
     "-verbose", Arg.Unit (fun () -> _parser#set_verbose_flag), "\tdisplay verbose messages";
     "-I", Arg.String _parser#add_search_path, "PATH\tadd search path";
     "-k", Arg.Unit (fun () -> _parser#set_keep_going_flag), "\tcontinue parsing in spite of errors";
     "-parse-d-lines", Arg.Unit (fun () -> _parser#set_parse_d_lines_flag), "\tparse d-lines as code";
     "-max-line-length", Arg.Int _parser#set_max_line_length, "N\tset max line length to N";
     "-proj-root", Arg.String options#set_root_path, "P\tset project root path to P";
     "-dump-ast", Arg.Unit (fun () -> dump_ast := true), "\tdump AST";
    ] 
    (fun s -> incr arg_count; filename := s) 
    ("usage: " ^ Filename.basename (Sys.argv.(0)) 
     ^ " [OPTIONS] [FILE]\noptions are:")

let _ =

  if !arg_count > 1 then Xprint.error "too many arguments";
  if !arg_count = 1 then compile_mode := true;

  try
(*
    let macrotbl_opt = None in
*)
    let macrotbl_opt =
      let f = 
	Fs.file_of_path options (Filename.concat (Filename.dirname (Sys.argv.(0))) "standard.f") 
      in
      Some (read_hint f)
    in
    begin match macrotbl_opt with 
    | Some tbl -> tbl#set_readonly
    | None -> ()
    end;

    let file = Fs.file_of_path options !filename in

    _parser#add_search_path file#dirname;

    while true do

      begin match macrotbl_opt with
      | Some tbl -> _parser#set_predefined_macrotbl macrotbl_opt
      | None -> ()
      end;

      let ast = 
	if !compile_mode then
	  _parser#parse_file file
	else
	  _parser#parse_stdin
      in

      if !dump_ast then
        Printer.dump ast#root
      else begin
        BEGIN_INFO
	  Printer.dump ast#root;
        END_INFO
      end;

      Printf.printf "*** PARSED! ***\n";

      Printf.printf "AST nodes: %d\n" ast#size;

      let na = ast#count_ambiguous_nodes in
      if na > 0 then
        Printf.printf "ambiguous nodes: %d\n" na;

      let n_omp_errors = ast#count_omp_error_nodes in
      if n_omp_errors > 0 then
        Printf.printf "OMP error nodes: %d\n" n_omp_errors;

(*
      Common.inst0#show;
      Common.inst1#show;
      Common.inst2#show;
      Common.inst3#show;
      Common.inst4#show;
      Common.inst5#show;
*)


      _parser#dump_ignored_regions;

      let ignored_LOC = _parser#ignored_LOC in
      if ignored_LOC > 0 then
	Printf.printf "ignored LOC: %d\n" ignored_LOC;

(*
      _parser#dump_missed_regions;
*)
      let missed_LOC = _parser#missed_LOC in
      if missed_LOC > 0 then
	Printf.printf "missed LOC: %d\n" missed_LOC;

      BEGIN_INFO
	Printf.printf "%d lines read\n" _parser#lines_read
      END_INFO;

      Printf.printf "\n";

      exit 0
    done
  with 
  | Sys_error msg -> Xprint.error ~out:stdout "%s" msg
(*
  | Failure msg -> Xprint.error ~out:stdout ~head:"[FAILURE]" "%s" msg
*)
  | Parse_error(head, msg) -> Xprint.error ~out:stdout ~head "%s" msg

  | Internal_error msg -> Xprint.error ~out:stdout ~head:"[INTERNAL]" "%s" msg

(*
  | exn -> Xprint.error ~out:stdout ~head:"[EXCEPTION]" "%s" (Printexc.to_string exn)
*)
