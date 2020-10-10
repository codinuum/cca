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
(* 
 * A parser for Verilog
 *
 * main.ml
 *
 *)

open Printf

open Common
module Aux = Parser_aux



let compile_mode = ref false
let filename = ref ""
let arg_count = ref 0
let outfile = ref ""

let error msg = 
  print_string "[ERROR] ";
  print_string msg; print_newline(); exit 0

let read_hint file =
  let _parser = new Lib.parser_c in
  let _ = _parser#parse_file file in
  _parser#macrotbl


let _parser = new Lib.parser_c

let _ = 
  Arg.parse 
    [
     "-verbose", Arg.Unit (fun () -> _parser#set_verbose_flag), "\tdisplay verbose messages";
     "-I", Arg.String _parser#add_search_path, "PATH\tadd search path";
     "-o", Arg.String (fun s -> outfile := s), (sprintf "FILE\tdump result into FILE (default:SRC%s)" Astv.astv_ext);
     "-k", Arg.Unit (fun () -> _parser#set_keep_going_flag), "\t\tcontinue parsing in spite of errors";
     "-ignore-include", Arg.Unit (fun () -> _parser#set_ignore_include_flag), "\tignore include lines";
    ] 
    (fun s -> incr arg_count; filename := s) 
    ("usage: " ^ Filename.basename (Sys.argv.(0)) 
     ^ " [OPTIONS] [SRC]\noptions are:")

let _ =
  let options = new Basic_options.c in

  if !arg_count > 1 then Xprint.error "too many arguments";
  if !arg_count = 1 then compile_mode := true;
  try

    let macrotbl_opt = None in
(*
    let macrotbl_opt =
      let f = 
	Fs.file_of_path options (Filename.concat (Filename.dirname (Sys.argv.(0))) "standard.f") 
      in
      Some (read_hint f)
    in
*)
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

      if !outfile = "" then
        if !filename = "" then
          Astv.dump_in_xml ast#root
        else
          Astv.save_in_xml (!filename^Astv.astv_ext) ast#root
      else
        Astv.save_in_xml !outfile ast#root;


      fprintf stderr "*** PARSED! ***\n";

      let ignored_LOC = _parser#ignored_LOC in
      if ignored_LOC > 0 then
	fprintf stderr "ignored LOC: %d\n" ignored_LOC;

      let missed_LOC = _parser#missed_LOC in
      if missed_LOC > 0 then
	fprintf stderr "missed LOC: %d\n" missed_LOC;


      fprintf stderr "%d lines read\n" _parser#lines_read;

      exit 0
    done
  with 
  | Sys_error msg -> Xprint.error "%s" msg
  | Failure msg -> Xprint.error ~head:"[FAILURE]" "%s" msg

  | Parse_error(head, msg) -> Xprint.error ~head "%s" msg

  | Internal_error msg -> Xprint.error ~head:"[INTERNAL]" "%s" msg


  | exn -> Xprint.error ~head:"[EXCEPTION]" "%s" (Printexc.to_string exn)

