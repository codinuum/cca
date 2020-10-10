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
 * A parser for the Java Language 
 *
 * main.ml
 *
 *)

open Common

let compile_mode = ref false
let filename = ref ""
let arg_count = ref 0
let dump_flag = ref false
let keep_going_flag = ref false

let _ = 
  Arg.parse 
    [
     "-dump", Arg.Unit (fun () -> dump_flag := true), "\tdump result";
     "-k", Arg.Unit (fun () -> keep_going_flag := true), "\tparse tolerantly";
    ]
    (fun s -> incr arg_count; filename := s)
    ("usage: " ^ Filename.basename (Sys.argv.(0))
     ^ " [OPTIONS] [FILE]\noptions are:")

let _ =
  if !arg_count > 1 then begin
    Xprint.error "too many arguments";
    exit 1
  end;

  let options = new Basic_options.c in

  if !arg_count = 1 then compile_mode := true;
  try
    let _parser = new Lib.parser_c in
    _parser#_set_keep_going_flag !keep_going_flag;
    while true do
      let ast = 
	if !compile_mode then
	  _parser#parse_file (Fs.file_of_path options !filename)
	else
	  _parser#parse_stdin
      in

      Printf.printf "*** PARSED! ***\n";

      if !dump_flag then begin
	Printer.pr_compilation_unit ast#compilation_unit;
	Printf.printf "%d lines read\n" _parser#lines_read
      end
      else begin
        BEGIN_INFO
	  Printer.pr_compilation_unit ast#compilation_unit;
          Printf.printf "%d lines read\n" _parser#lines_read;
        END_INFO
      end;

      exit 0
    done
  with 
  | Sys_error msg
  | Failure msg 
    -> Xprint.error "%s" msg; exit 1

  | Parse_error(head, msg) -> Xprint.error ~head "%s" msg; exit 1

