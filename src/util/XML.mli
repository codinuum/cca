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
