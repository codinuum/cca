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
(* *)

open Compat

module Loc = Astloc
module NC = Netconversion


(*
let available_encodings = NC.available_input_encodings()
*)

let default_encoding = Ulexing.default_encoding

class c (file : Storage.file) = 
  let tree = file#tree in
  let path = file#path in 
  object (self)

    val pos_mgr = new Position.manager path

    val queue = Queue.create()

    val mutable eof_reached = false
    val mutable eof_loc : Loc.t option = None

    val mutable channel = (None : Netchannels.in_obj_channel option)

    val mutable encoding = default_encoding
    val mutable ustring_length = NC.ustring_length default_encoding
    val mutable create_cursor = NC.create_cursor default_encoding

    val mutable ulexbuf_list = []

    method reset_feed = Queue.clear queue

    method update_encoding enc =
      encoding <- enc;
      ustring_length <- NC.ustring_length enc;
      create_cursor <- NC.create_cursor enc

    method ustring_length = ustring_length
    method create_cursor = create_cursor

    method tree = tree

    method path = path

    method init =
      pos_mgr#reset;
      self#reset_feed

    method eof_reached = eof_reached
    method set_eof_reached = eof_reached <- true

    method eof_loc = eof_loc
    method set_eof_loc loc = eof_loc <- Some loc

    method file = file

    method filename = pos_mgr#filename
    method set_filename fn = pos_mgr#set_filename fn
    method pos_mgr = pos_mgr

    method exists = file#exists

    method get_channel =
      let ch = 
        try
          tree#get_channel self#filename
        with 
	  Sys_error s -> failwith ("Source_base.c#get_channel: "^s)
      in
      ch

    method get_ulexbuf_from_channel ch =
      let ulexbuf = 
        Ulexing.create (self#refill ch) 
      in
      ulexbuf_list <- ulexbuf :: ulexbuf_list;
      ulexbuf

    method get_ulexbuf =
      let ch = self#get_channel in
      channel <- Some ch;
      self#get_ulexbuf_from_channel ch

    method close =
      match channel with
      | Some ch ->
          ch#close_in();
          DEBUG_MSG "%s closed" path
      | None -> ()

    method get_ulexbuf_from_stdin =
      Ulexing.from_utf8_channel stdin

    method private purify (str : bytes) =
      try
        while true do
          try
            NC.verify default_encoding (Bytes.to_string str);
            raise Exit
          with
            NC.Malformed_code_at i ->
              Bytes.set str i '?'
        done
      with
        Exit -> ()


    method private proc buf pos n s =
      let len = String.length s in

      let _proc s =
        DEBUG_MSG "[%s] ^%s$" (NC.string_of_encoding encoding) s;
        if len > n then begin
          let cur = self#create_cursor s in
          let nc = ref 0 in
          let nb = ref 0 in
          let cp = ref 0 in
          try
            while not (NC.cursor_at_end cur) do
              DEBUG_MSG "cp=%d: %d" !cp (NC.uchar_at cur);
              NC.move cur;
              nb := !cp;
              cp := NC.cursor_pos cur;
              if !cp > n then
                raise Exit
              else
                incr nc
            done;
            assert false
          with
            Exit ->
              DEBUG_MSG "n=%d, nb=%d" n !nb;
	      Bytes.blit_string s 0 buf pos !nb;
	      let n' = len - !nb in
	      let s' = Bytes.create n' in
	      Bytes.blit_string s !nb s' 0 n';
              Queue.add (Bytes.to_string s') queue;
	      pos_mgr#feed !nc;
	      !nb
        end
        else begin
          let nc = self#ustring_length s in
	  Bytes.blit_string s 0 buf pos len;
          pos_mgr#feedline nc;
	  len
        end
      in
      try
        _proc s
      with
        NC.Malformed_code -> 
          Xprint.warning "\"%s\": malformed code found in \"%s\"" file#fullpath s;
          let b = Bytes.of_string s in
          self#purify b;
          Xprint.warning "\"%s\": malformed characters are converted into '?'" file#fullpath;
          _proc (Bytes.to_string b)
(*
          let guessed_enc = ref None in
          try
            List.iter
              (fun enc ->
                try
                  NC.verify enc s;
                  guessed_enc := Some enc;
                  raise Exit
                with
                  NC.Malformed_code_at x -> ()
              ) available_encodings;
            self#purify s;
            _proc()
          with
            Exit ->
              match !guessed_enc with
              | Some enc ->
                  Xprint.warning "\"%s\": encoding \"%s\" found" 
                    file#fullpath (NC.string_of_encoding enc);

                  self#update_encoding enc;
                  List.iter 
                    (fun ulexbuf -> 
                      let ulb = ulexbuf.Ulexing.ulb in
                      Netulex.ULB.set_encoding enc ulb
                    ) ulexbuf_list;
                  _proc()
              | None -> assert false
*)

    method refill ch buf pos n =
      DEBUG_MSG "pos=%d n=%d" pos n;
      try
        let s =
          try
            Queue.take queue
          with
            Queue.Empty ->
              let line = ch#input_line() in
              line^"\n"
        in
        self#proc buf pos n s
      with 
        End_of_file -> 0

 



end (* of class Sourcefile_base.c *)
