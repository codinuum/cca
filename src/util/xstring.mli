val startswith : string -> string -> bool
val endswith : string -> string -> bool
val rstrip : ?strs:(string list) -> string -> string
val lstrip : ?strs:(string list) -> string -> string
val strip : ?strs:(string list) -> string -> string
val to_int_array : string -> int array
val to_char_array : string -> char array
val encode : string -> string
val decode : string -> string
val escaped : string -> string
val ntriples_escaped : string -> string
