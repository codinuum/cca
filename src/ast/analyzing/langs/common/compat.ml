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
(* Ulexing emuration based on *)
(* $Id: netulex.ml 799 2004-07-08 23:04:25Z stolpmann $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


module Loc = Astloc

open Netulex


module Ulexing = struct

  type lexbuf =
      {         ulb        : ULB.unicode_lexbuf;
	mutable offset     : int;
	mutable pos        : int;
	mutable start      : int;
	mutable marked_pos : int;
	mutable marked_val : int;
                base_loc   : Loc.t;
      }

  exception Error

  let from_ulb_lexbuf ?(base_loc=Loc.dummy) ulb =
    { ulb        = ulb;
      offset     = 0;
      pos        = 0;
      start      = 0;
      marked_pos = 0;
      marked_val = 0;
      base_loc   = base_loc;
    }

  let lexeme_start lexbuf  = lexbuf.start + lexbuf.offset
  let lexeme_end lexbuf    = lexbuf.pos + lexbuf.offset
  let lexeme_length lexbuf = lexbuf.pos - lexbuf.start

  let lexeme lexbuf =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf lexbuf.start (lexbuf.pos - lexbuf.start)

  let sub_lexeme lexbuf pos len =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf (lexbuf.start + pos) len

  let lexeme_char lexbuf pos =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    buf.(lexbuf.start + pos)

  let utf8_lexeme lexbuf =
    ULB.utf8_sub_string lexbuf.start (lexbuf.pos - lexbuf.start) lexbuf.ulb

  let utf8_sub_lexeme lexbuf pos len =
    ULB.utf8_sub_string (lexbuf.start + pos) len lexbuf.ulb

  let utf8_sub_lexeme_length lexbuf pos len =
    ULB.utf8_sub_string_length (lexbuf.start + pos) len lexbuf.ulb
    
  (* "Internal" interface *)
  let start lexbuf =
    lexbuf.start <- lexbuf.pos;
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- (-1)

  let mark  lexbuf i =
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- i
      
  let backtrack lexbuf =
    lexbuf.pos <- lexbuf.marked_pos;
    lexbuf.marked_val

  let rollback lexbuf =
    lexbuf.pos <- lexbuf.start

  let eof = (-1)

  let refill lexbuf = 
    try
      (* Delete all characters in ulexbuf before the current lexeme: *)
      if lexbuf.start > 0 then (
	let n = lexbuf.start in
	ULB.delete n lexbuf.ulb;
	lexbuf.offset <- lexbuf.offset + n;
	lexbuf.pos <- lexbuf.pos - n;
	lexbuf.marked_pos <- lexbuf.marked_pos - n;
	lexbuf.start <- 0;
      );
      ULB.refill lexbuf.ulb;
      (* raises either End_of_file, or ensures there is one char in ulb *)
      lexbuf.ulb.ULB.ulb_chars.(lexbuf.pos)
    with
	End_of_file ->
	  (* We cannot modify the buffer as the original Ulexing implementation
	   *)
	  eof
	  

  let next lexbuf =
    let ulb = lexbuf.ulb in
    let i =
      if lexbuf.pos = ulb.ULB.ulb_chars_len then
	refill lexbuf
      else 
	ulb.ULB.ulb_chars.(lexbuf.pos)
    in
    if i <> eof then lexbuf.pos <- lexbuf.pos + 1;
    i




  let default_encoding = `Enc_utf8

  let create ?(enc_change_hook=fun x -> ()) refill =
    let ulb = ULB.from_function ~enc_change_hook ~refill default_encoding in
    from_ulb_lexbuf ulb

  let from_utf8_string ?(base_loc=Loc.dummy) s =
    let ulb = ULB.from_string default_encoding s in
    from_ulb_lexbuf ~base_loc ulb

  let from_utf8_in_obj_channel ch =
    let ulb = ULB.from_in_obj_channel default_encoding ch in
    from_ulb_lexbuf ulb

  let from_utf8_channel ch =
    let ioch = new Netchannels.input_channel ch in
    from_utf8_in_obj_channel ioch

  let get_start lexbuf = lexbuf.start

  let get_pos lexbuf = lexbuf.pos

  let get_buf lexbuf = lexbuf.ulb.ULB.ulb_rawbuf

end (* of module Compat.Ulexing *)
