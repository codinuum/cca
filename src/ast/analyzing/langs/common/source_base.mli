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
module Loc = Astloc
module NC = Netconversion

val default_encoding : Netconversion.encoding

class c : Storage.file -> object
  method close : unit
  method create_cursor : string -> NC.cursor
  method eof_loc : Loc.t option
  method eof_reached : bool
  method exists : bool
  method file : Storage.file
  method filename : string
  method get_channel : Netchannels.in_obj_channel
  method get_ulexbuf : Compat.Ulexing.lexbuf
  method get_ulexbuf_from_channel : Netchannels.in_obj_channel -> Compat.Ulexing.lexbuf
  method get_ulexbuf_from_stdin : Compat.Ulexing.lexbuf
  method init : unit
  method path : string
  method pos_mgr : Position.manager
  method private proc : bytes -> int -> int -> string -> int
  method private purify : bytes -> unit
  method refill : Netchannels.in_obj_channel -> bytes -> int -> int -> int
  method reset_feed : unit
  method set_eof_loc : Loc.t -> unit
  method set_eof_reached : unit
  method set_filename : string -> unit
  method tree : Storage.tree
  method update_encoding : Netconversion.encoding -> unit
  method ustring_length : ?range_pos:int -> ?range_len:int -> string -> int
end
