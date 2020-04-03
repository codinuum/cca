type ext_cache_t = (string * string, string) Hashtbl.t

val ext_sep : char
val ext_sep_pat : Str.regexp
val dir_sep : string
val dir_sep_pat : Str.regexp
val loc_sep : string
val loc_sep_pat : Str.regexp
val is_extended : string -> bool
val extend : ?cache:ext_cache_t -> ?force:bool -> string -> string -> string
val strip : string -> string
val get_extension : string -> string
val escape : string -> string
val to_string : ?show_ext:bool -> ?short:bool -> string -> string
