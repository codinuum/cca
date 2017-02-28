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

module Aux = Parser_aux
module PB  = Parserlib_base


module F (Stat : Aux.STATE_T) = struct

  module U = Ulexer.F (Stat)

  open Stat

  class c = object (self)
    inherit [Tokens_.token] PB.scanner

    val mutable ulexbuf_opt = None

    val queue = Queue.create()

    method enter_source src =
      DEBUG_MSG "source=\"%s\"" src#filename;
      let ulexbuf =
        if src#filename = "<stdin>" then begin
          src#get_ulexbuf_from_stdin
        end
        else begin
          src#get_ulexbuf
        end
      in
      ulexbuf_opt <- Some ulexbuf;
      ulexbuf


    method get_token() = 
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          let token = U.get_token queue ulexbuf in
          DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr token);
          token
      end
      | None -> failwith "Scanner.F.c#get_token"

    initializer
      env#set_enter_source_callback self#enter_source

  end (* of class Scanner.F.c *)


end (* of functor Scanner.F *)
