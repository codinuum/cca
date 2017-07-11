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
(* Storage *)

let open_temp_file rpath =
  let rp = Str.global_replace (Str.regexp_string Filename.dir_sep) "_" rpath in
  Filename.open_temp_file "cca" rp

type kind =
  | K_DUMMY
  | K_FS
  | K_GIT

let kind_to_string = function
  | K_DUMMY -> "dummy"
  | K_FS    -> "fs"
  | K_GIT   -> "git"

let kind_is_fs = function
  | K_FS    -> true
  | _ -> false

let kind_is_git = function
  | K_GIT   -> true
  | _ -> false

class type entry_t = object
  method path        : string
  method dirname     : string
  method name        : string
  method is_dir      : bool
  method size        : int
  method entries     : entry_t list
  method file_digest : Xhash.t
  method dir_digest  : Xhash.t option
end (* of class type Storage.entry_t *)

let rec scan_dir (* for files *)
    ?(recursive=true) 
    dent 
    (f : entry_t -> unit)  
    =
  List.iter
    (fun ent ->
      try
        if ent#is_dir then
          if recursive then
            scan_dir ent f 
          else
            ()
        else
          f ent
      with
        exn ->
          Xprint.warning "%s" (Printexc.to_string exn)
    ) dent#entries

let rec scan_dir_for_dirs exists dent (f : entry_t -> unit) =
  List.iter
    (fun ent ->
      if exists ent#path then
        if ent#is_dir then begin
          f ent;
          scan_dir_for_dirs exists ent f 
        end
    ) dent#entries


class converter filt = object (self)
  val buf = Buffer.create 128

  method conv ibuf eof obuf =
    let _contents = Netbuffer.contents ibuf in
    Netbuffer.clear ibuf;
    let contents = (Buffer.contents buf)^_contents in
    Buffer.clear buf;
    String.iter
      (function
        | '\n' -> begin
            Netbuffer.add_string obuf (filt (Buffer.contents buf));
            Netbuffer.add_char obuf '\n';
            Buffer.clear buf
        end
        | c -> Buffer.add_char buf c
      ) contents;
    if eof then 
      Netbuffer.add_string obuf (filt (Buffer.contents buf))

end

class filtered_in_channel ch filt_ch = object (self)
  inherit Netchannels.in_obj_channel_delegation filt_ch
  method close_in() =
    filt_ch#close_in();
    ch#close_in()
end

class virtual tree = object (self)
  method virtual hash_algo       : Xhash.algo
  method virtual kind            : kind
  method virtual id              : string
  method virtual get_entry       : ?ignore_case:bool -> string -> entry_t (* "" means root *)
  method virtual get_channel     : ?ignore_case:bool -> string -> Netchannels.in_obj_channel
  method virtual get_local_file  : ?ignore_case:bool -> string -> string
  method virtual free_local_file : string -> unit 

  val kept_local_path_set = Xset.create 0 (* path to be kept *)

  method private keep_local_path path =
    Xset.add kept_local_path_set path

  method private is_kept_local_path path =
    Xset.mem kept_local_path_set path

  val filter_tbl = (Hashtbl.create 0 : (string, (string -> string)) Hashtbl.t)

  method set_filter extl filt =
    List.iter (fun ext -> Hashtbl.add filter_tbl (String.lowercase ext) filt) extl

  method get_filter_by_ext ext = Hashtbl.find filter_tbl (String.lowercase ext)

  method get_filter_by_name name =
    let ext = Xfile.get_extension name in
    self#get_filter_by_ext ext

  method get_filt_channel name =
    let ch = self#get_channel name in
    try
      let filt = self#get_filter_by_name name in
      let conv = (new converter filt)#conv in
      let pipe = new Netchannels.pipe ~conv () in
      let filt_ch = new Netchannels.input_filter ch pipe in
      new filtered_in_channel ch filt_ch
    with
    | Not_found | Xfile.No_extension _ -> ch

  method name =
    Printf.sprintf "%s:%s" (kind_to_string self#kind) self#id

  method exists ?(ignore_case=false) path =
    try
      let _ = self#get_entry ~ignore_case path in
      true
    with
      _ -> false

  method is_dir ?(ignore_case=false) path =
    try
      (self#get_entry ~ignore_case path)#is_dir
    with
      _ -> false

  method is_file ?(ignore_case=false) path =
    try
      not (self#get_entry ~ignore_case path)#is_dir
    with
      _ -> false

  method search_path ?(ignore_case=false) root_path path =
    DEBUG_MSG "%s\n%!" path;
    let dent = self#get_entry ~ignore_case root_path in
    let found = ref [] in
    scan_dir_for_dirs (self#exists ~ignore_case) dent
      (fun ent ->
        let p = Xfile.normpath (Filename.concat ent#path path) in
        if self#exists ~ignore_case p then begin
          DEBUG_MSG "found %s\n%!" p;
          found := (ent, p) :: !found
        end
      );
    !found

end (* of class Storage.tree *)


class file ?(digest_opt=None) ?(ignore_case=false) (t : tree) (_path : string) =
  let path =
    if ignore_case then
      (t#get_entry ~ignore_case:true _path)#path
    else
      _path
  in
  object (self)

  val mutable digest_opt = digest_opt

  val mutable extra_ext = None

  method set_extra_ext ext =
    extra_ext <- Some ext

    method set_digest d =
      digest_opt <- Some d

  method tree          = t
  method fullpath      = Xfile.normpath (Filename.concat t#id path)
  method path          = path
  method basename      = Filename.basename path
  method dirname       = Filename.dirname path
  method is_dir        = self#get_entry#is_dir
  method exists        = t#exists path
  method size          = self#get_entry#size
  method kind          = t#kind

  method digest        = 
    match digest_opt with
    | Some d -> d
    | None -> self#get_entry#file_digest

  method get_extension   = 
    match extra_ext with
    | None -> Xfile.get_extension path
    | Some x -> x

  method get_entry       = t#get_entry path
  method get_channel     = t#get_channel path
  method get_local_file  = t#get_local_file path
  method free_local_file = t#free_local_file path

  method set_filter extl filt = t#set_filter extl filt

end (* of class Storage.file *)


class dummy_entry : entry_t = object
  method path    = "<dummy>"
  method dirname = "<dummy>"
  method name    = "<dummy>"
  method is_dir  = false
  method size    = 0
  method entries = []
  method file_digest  = ""
  method dir_digest = None
end

let dummy_entry = new dummy_entry

class dummy_tree = object
  inherit tree
  method hash_algo         = Xhash.MD5
  method kind              = K_DUMMY
  method id                = "<dummy>"
  method get_entry ?(ignore_case=false) p       = dummy_entry
  method exists ?(ignore_case=false) p          = false
  method get_channel ?(ignore_case=false) p     = new Netchannels.input_string "<dummy>"
  method get_local_file ?(ignore_case=false) p  = "<dummy>"
  method free_local_file p = ()
end

let dummy_tree = new dummy_tree



let stdin = new file dummy_tree "<stdin>"

