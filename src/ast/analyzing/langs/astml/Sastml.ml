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

let parser_name = "astml"

let compressors = (* EXT * DECOMP_CMD_FMT *)
  [ ".bz2", format_of_string "bunzip2 --keep --stdout %s > %s";
    ".gz", format_of_string "gunzip --stdout %s > %s";
  ]
let comp_ext_list = "" :: (fst (List.split compressors))

let decomp ext ifile ofile =
  let ipath = ifile#get_local_file in
  try
    let cmd = Printf.sprintf (List.assoc ext compressors) ipath ofile in
    DEBUG_MSG "uncompressing \"%s\"..." ipath;
    DEBUG_MSG "command=\"%s\"" cmd;
    let _ = Sys.command cmd in
    DEBUG_MSG "done."
  with 
    Not_found -> 
      failwith (Printf.sprintf "decomp: unsupported compression: %s" ext)


let extensions = 
  List.flatten
    (List.map 
       (fun base_ext -> 
	 List.map 
	   (fun comp_ext -> 
	     base_ext^comp_ext
	   ) comp_ext_list
       ) [Astml.extension; Astml.ccs_ext])

let _ = Lang_base.register_spec parser_name extensions

let xparser_name_ccx = "ccx"

let external_parser_specs = [ xparser_name_ccx, [".c"; ".cpp"; ".cc"; ".h"; ".hh"; ".hpp"];
                            ]

let _ = Lang_base.register_external_specs parser_name external_parser_specs
