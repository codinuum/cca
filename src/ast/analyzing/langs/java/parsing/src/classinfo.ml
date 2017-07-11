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
(* classinfo.ml *)


open Common

exception Found of string

class classtbl_c = object (self)

  (* package name -> local name set (for standard library) *)
  val stdtbl = Classtbl.stdtbl           

  (* package name -> local name set *)
  val pkg_mem_class_tbl = Hashtbl.create 0

  val mutable pkgs = Hashtbl.create 0 (* package name -> path *)

  val mutable searched_packages = []

  val mutable source_dir = None


  method set_source_dir d = source_dir <- Some d
  method has_source_dir = 
    match source_dir with
    | None -> false
    | Some _ -> true

  method get_source_dir =
    match source_dir with
    | None -> raise Not_found
    | Some d -> d
    

  method private clear_packages = Hashtbl.clear pkgs


  method add_package ?(dir=Storage.dummy_entry) pname =
    try
      if pname = "" then
        raise Exit;

      DEBUG_MSG "ADDING PACKAGE \"%s\"" pname;
      DEBUG_MSG "               dir=%s" dir#path;

      Hashtbl.replace pkgs pname dir;

      let d, recursive =
        if dir == Storage.dummy_entry then begin
          match source_dir with
          | None -> raise Exit
          | Some srcdir -> srcdir, true
        end
        else
          dir, false
      in

      DEBUG_MSG "SEARCHING %s for \"%s\"..." d#path pname;

      let pname_path = pkg_to_path pname in

      Storage.scan_dir ~recursive d
        (fun ent ->
          let path = ent#path in
	  if Xstring.endswith path ".java" then
	    if Xstring.endswith ent#dirname pname_path then
	      let lname = Filename.chop_extension ent#name in
	      self#add pname lname
        ) 
    with
      Exit -> ()

  method add_api_package pname =
    try
      if pname = "" then
        raise Exit;

      DEBUG_MSG "ADDING API PACKAGE \"%s\"" pname;

      Hashtbl.replace pkgs pname Storage.dummy_entry;

    with
      _ -> ()

  method private add pname lname =
    DEBUG_MSG "ADDING: \"%s\" -> \"%s\"" pname lname;
    let s = 
      try
        Hashtbl.find pkg_mem_class_tbl pname
      with 
	Not_found -> 
          let s' = Xset.create 1 in
          Hashtbl.add pkg_mem_class_tbl pname s';
          s'
    in
    Xset.add s lname

  method add_fqn fqn =
    let pname, lname = decompose_qname fqn in
    self#add pname lname
    
  method private __resolve tbl pname lname =
    let mems = Hashtbl.find tbl pname in
    if Xset.mem mems lname then
      pname^"."^lname
    else
      raise Not_found

  method _resolve lname =
    Hashtbl.iter
      (fun pkg _ ->
	try
	  raise (Found (self#__resolve pkg_mem_class_tbl pkg lname))
	with 
	  Not_found -> ()
      ) pkgs;
    Hashtbl.iter
      (fun pkg _ ->
	try
	  raise (Found (self#__resolve stdtbl pkg lname))
	with 
	  Not_found -> ()
      ) pkgs

  method resolve lname =
    DEBUG_MSG "lname=\"%s\"" lname;
    try
      self#_resolve lname;
      self#__resolve stdtbl "java.lang" lname
    with 
      Found fqn -> 
        DEBUG_MSG "found: \"%s\"" fqn;
	fqn

  method is_resolvable lname =
    try
      let _ = self#resolve lname in
      true
    with
      _ -> false

  method is_package qname =
    (Hashtbl.mem pkgs qname) || (Hashtbl.mem stdtbl qname)


  (* due to nested classes *)
  method get_package_name qname = (* qname = "a.b.c.lname" *)
    DEBUG_MSG "qname=\"%s\"" qname;

    let rec find_pkg_prefix tbl qn =
      try
        let prefix, base = decompose_qname qn in
        if Hashtbl.mem tbl prefix then begin
          DEBUG_MSG "found: \"%s\"" prefix;
          prefix
        end
        else
          find_pkg_prefix tbl prefix
      with
        Invalid_argument _ -> raise Not_found
    in
    try
      find_pkg_prefix pkgs qname
    with
      Not_found ->
        DEBUG_MSG "searching stdtbl...";
        find_pkg_prefix stdtbl qname

          
  method _resolve_qualified_type_name pkg qname =
    DEBUG_MSG "pkg=%s qname=%s" pkg qname;
    if pkg = qname then
      failwith "Classinfo#_resolve_qualified_type_name";
    let lname = Str.replace_first (Str.regexp_string (pkg^".")) "" qname in
    Xstring.replace lname '.' '$';
    if pkg = "" then
      lname
    else
      pkg^"."^lname

  method resolve_qualified_type_name qname =
    let pkg = self#get_package_name qname in
    DEBUG_MSG "pkg=\"%s\"" pkg;
    self#_resolve_qualified_type_name pkg qname


end (* class Classinfo.classtbl_c *)
