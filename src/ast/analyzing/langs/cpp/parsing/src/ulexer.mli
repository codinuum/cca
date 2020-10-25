(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>
   Copyright 2020 Chiba Institute of Technology

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

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)

open Compat

module F : functor (Stat : Parser_aux.STATE_T) -> sig

  val _token : Ulexing.lexbuf -> Tokens_.token * Lexing.position * Lexing.position

  val add_comment_region : 'a -> unit

  val mklexpos : int -> Lexing.position
  val offsets_to_loc : int -> int -> Astloc.t

  val mktok : ?st_opt:(int option) -> 'a -> Ulexing.lexbuf -> 'a * Lexing.position * Lexing.position

end
