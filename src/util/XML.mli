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

module Comp = Compression

val sprintf : ('a, unit, string) format -> 'a
val header : string
val encode_string : string -> string
val decode_string : string -> string
val get_local_part : string -> string
val node_type_to_string : Pxp_document.node_type -> string

class warner : object
  method warn : string -> unit
end

val default_config : Pxp_types.config
val parse_xchannel :
  ?transform_dtd:(Pxp_dtd.dtd -> Pxp_dtd.dtd) ->
  ?config:Pxp_types.config ->
  ?spec:('a Pxp_document.node Pxp_document.extension as 'a) Pxp_document.spec ->
  Xchannel.in_channel -> 'a Pxp_document.document
val parse_file :
  ?transform_dtd:(Pxp_dtd.dtd -> Pxp_dtd.dtd) ->
  ?config:Pxp_types.config ->
  ?spec:('a Pxp_document.node Pxp_document.extension as 'a) Pxp_document.spec ->
  string -> 'a Pxp_document.document
