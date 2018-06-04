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
(* xchannel.ml *)


module C = Compression

exception Error of string
exception File_exists of string
exception File_not_found of string

let sprintf = Printf.sprintf



(* output channel *)

module Destination = struct
  type t =
    | File of string
    | Buffer of Buffer.t
    | Channel of out_channel

  let of_file f     = File f
  let of_buffer b   = Buffer b
  let of_channel ch = Channel ch

  let to_string = function
    | File s    -> sprintf "<file:%s>" s
    | Buffer _  -> "<buffer>"
    | Channel _ -> "<out_channel>"
end

module D = Destination

class out_channel ?(overwrite=true) ?(comp=C.none) dest 
    = 
  object (self)

    val mutable extchs = []

    val mutable och = Obj.magic ()

    method private close_extra =
      List.iter (fun c -> c#close_out()) extchs

    initializer
      let ooch =
        try
	  if comp = C.none then begin
	    match dest with
	    | D.File file -> begin
	        if Sys.file_exists file && not overwrite then
		  raise (File_exists file)
	        else
		  new Netchannels.output_channel (open_out file)
            end
	    | D.Buffer buf -> new Netchannels.output_buffer buf
            | D.Channel ch -> new Netchannels.output_channel ch
	  end
	  else if comp = C.gzip then begin
	    let lv = comp#level in
	    match dest with
	    | D.File file -> begin
	        if Sys.file_exists file && not overwrite then
		  raise (File_exists file)
	        else
		  let go = Gzip.open_out ~level:lv file in
		  new Netgzip.output_gzip go
            end
	    | D.Buffer buf ->
	        let c = new Netchannels.output_buffer buf in
	        extchs <- c :: extchs;
	        let pipe = new Netgzip.deflating_pipe ~level:lv () in
	        new Netchannels.output_filter pipe c

            | D.Channel ch ->
	        let c = new Netchannels.output_channel ch in
	        extchs <- c :: extchs;
	        let pipe = new Netgzip.deflating_pipe ~level:lv () in
	        new Netchannels.output_filter pipe c
	  end 
	  else
	    raise (Error "unknown compression")
        with
        | File_exists s -> raise (File_exists s)
        | e -> raise (Error (Printexc.to_string e))
      in
      och <- ooch
      
    method out_obj_channel = och

    method flush = 
      och#flush()

    method output (buf : bytes) pos len =
      try
	och#output buf pos len
      with
      | e -> raise (Error (Printexc.to_string e))

    method output_ (buf : string) pos len =
      try
       och#output (Bytes.of_string buf) pos len
      with
      | e -> raise (Error (Printexc.to_string e))

    method close =
      try
	och#close_out();
        self#close_extra
      with
      | e -> raise (Error (Printexc.to_string e))

  end (* of class Xchannel.out_channel *)


let output_bytes (ch : out_channel) b =
  ignore (ch#output b 0 (Bytes.length b))

let output_string (ch : out_channel) s =
  let b = Bytes.of_string s in
  output_bytes ch b

let fprintf ch fmt = Printf.ksprintf (output_string ch) fmt

let dump : ?comp:C.c -> ?add_ext:bool -> string -> ('och -> unit) -> unit =
  fun ?(comp=C.none) ?(add_ext=true) fname dumper ->
    let fname =
      if add_ext then
        fname^comp#ext
      else
        fname
    in
    try
      let dest = Destination.of_file fname in
      let ch = new out_channel ~comp dest in
      begin
	try
	  dumper ch
	with 
	| Error s -> WARN_MSG s
      end;
      try
	ch#close
      with 
      | Error s -> WARN_MSG s
    with 
    | Error s -> WARN_MSG s


(* input channel *)

module Source = struct
  type t =
    | File of string
    | Channel of in_channel

  let of_file f     = File f
  let of_channel ch = Channel ch

  let to_string = function
    | File s    -> sprintf "<file:%s>" s
    | Channel _ -> "<in_channel>"
end

module S = Source


class in_channel ?(comp=C.none) source 
    = 
  object (self)

    val mutable extchs = []

    val mutable ich = Obj.magic ()

    method close_extra = 
      List.iter (fun c -> c#close_in()) extchs

    initializer
      let ioch =
        try
	  if comp = C.none then begin
	    match source with
	    | S.File file -> begin
	        if not (Sys.file_exists file) then
		  raise (File_not_found file)
	        else
		  new Netchannels.input_channel (open_in file)
            end
            | S.Channel ch -> new Netchannels.input_channel ch
	  end
	  else if comp = C.gzip then begin
	    match source with
	    | S.File file -> begin
	        if Sys.file_exists file then
		  let gi = Gzip.open_in file in
		  new Netgzip.input_gzip gi
	        else
	          raise (File_not_found file)
            end
            | S.Channel ch ->
	        let c = new Netchannels.input_channel ch in
	        extchs <- c :: extchs;
	        let pipe = new Netgzip.inflating_pipe () in
	        new Netchannels.input_filter c pipe
	  end 
	  else
	    raise (Error "unknown compression")
        with
        | File_exists s    -> raise (File_exists s)
        | File_not_found s -> raise (File_not_found s)
        | e -> raise (Error (Printexc.to_string e))
      in
      ich <- ioch

    method in_obj_channel = ich

    method input buf pos len =
      try
	ich#input buf pos len
      with
      | e -> raise (Error (Printexc.to_string e))

    method close =
      try
	ich#close_in();
	self#close_extra
      with
      | e -> raise (Error (Printexc.to_string e))

  end (* of class Xchannel.in_channel *)
