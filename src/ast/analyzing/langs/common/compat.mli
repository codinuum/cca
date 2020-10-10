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
module Ulexing : sig
  type lexbuf =
      {         ulb        : Netulex.ULB.unicode_lexbuf;
	mutable offset     : int;
	mutable pos        : int;
	mutable start      : int;
	mutable marked_pos : int;
	mutable marked_val : int;
                base_loc   : Astloc.t;
      }

  exception Error

  val from_ulb_lexbuf : ?base_loc:Astloc.t -> Netulex.ULB.unicode_lexbuf -> lexbuf
  val lexeme_start : lexbuf -> int
  val lexeme_end : lexbuf -> int
  val lexeme : lexbuf -> int array
  val lexeme_length : lexbuf -> int
  val sub_lexeme : lexbuf -> int -> int -> int array
  val lexeme_char : lexbuf -> int -> int
  val utf8_lexeme : lexbuf -> string
  val utf8_sub_lexeme : lexbuf -> int -> int -> string
  val utf8_sub_lexeme_length : lexbuf -> int -> int -> int
  val start : lexbuf -> unit
  val mark : lexbuf -> int -> unit
  val backtrack : lexbuf -> int
  val rollback : lexbuf -> unit
  val eof : int
  val refill : lexbuf -> int
  val next : lexbuf -> int
  val default_encoding : Netconversion.encoding
  val create : ?enc_change_hook:(Netulex.ULB.unicode_lexbuf -> unit) -> (bytes -> int -> int -> int) -> lexbuf
  val from_utf8_string : ?base_loc:Astloc.t -> string -> lexbuf
  val from_utf8_in_obj_channel : Netchannels.in_obj_channel -> lexbuf
  val from_utf8_channel : in_channel -> lexbuf
  val get_start : lexbuf -> int
  val get_pos : lexbuf -> int
  val get_buf : lexbuf -> bytes
end
