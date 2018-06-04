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
(* common.ml *)

module PB = Parserlib_base

type java_language_specification =
  | JLSx
  | JLS2 
  | JLS3

exception Internal_error of string

exception Parse_error = PB.Parse_error

exception Pkg_found of string


let warning_loc loc   = PB.parse_warning_loc ~head:"[Java]" loc

let fail_to_parse = PB.fail_to_parse

let warning_msg = Xprint.warning ~head:"[Java]"



let find_package_name file =
  let pat = Str.regexp "^[ \t]*package[ \t]+\\(.+\\)[ \t]*;[ \t]*[\r]?$" in
  try
    let ich = file#get_channel in
    try
      while true do
	let line = ich#input_line() in
	if Str.string_match pat line 0 then
	  raise (Pkg_found (Str.matched_group 1 line))
      done;
      raise Not_found
    with
    | Pkg_found p -> ich#close_in(); p
    | _ -> ich#close_in(); raise Not_found
  with
    _ -> raise Not_found


let pkg_to_path, path_to_pkg =
  let dot_pat = Str.regexp_string "." in
  let sep_pat = Str.regexp Filename.dir_sep in

  let pkg_to_path pkg = 
    Str.global_replace dot_pat Filename.dir_sep pkg
  in
  let path_to_pkg path =
    Str.global_replace sep_pat "." path
  in
  pkg_to_path, path_to_pkg


type src_dir = SD_unnamed of string | SD_named of string


let guess_src_dir file =
  try
    let pkg = find_package_name file in
    let pkg_p = pkg_to_path pkg in

    DEBUG_MSG "package path: \"%s\"" pkg_p;

    let pkg_pat = Str.regexp (Filename.dir_sep^pkg_p^".*$") in

    let apath = file#path in

    if Xstring.startswith apath pkg_p then
      SD_named ""
    else
      try
        let _ = Str.search_forward pkg_pat apath 0 in
        SD_named (Str.global_replace pkg_pat "" apath)
      with
      | Not_found ->
          failwith (Printf.sprintf "cannot guess source directory for \"%s\"" apath)
  with
  | Not_found -> SD_unnamed file#dirname

let decompose_qname qname = (* a.b.c.d *)
  let len = String.length qname in
  try
    let i = String.rindex qname '.' in
    let prefix = String.sub qname 0 i in
    let base = String.sub qname (i + 1) (len - i - 1) in
    prefix, base
  with
    Not_found -> raise (Invalid_argument "Common.decompose_qname")

let is_qualified_qname qname =
  try
    let _ = String.rindex qname '.' in
    true
  with
    Not_found -> false

let dot_pat = Str.regexp_string "."

let replace_dot_with_dollar s =
  Str.global_replace dot_pat "$" s
