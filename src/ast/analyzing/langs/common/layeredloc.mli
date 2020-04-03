class loc_stack : object
  method get_layers : Astloc.t ref list
  method get_level : int
  method init : unit
  method pop : unit
  method push : Astloc.t -> unit
  method to_string : string
end

(*val sep : string*)
(*val sep_pat : Str.regexp*)
val encode_layers : Astloc.t ref list -> string
val decode_layers : string -> Astloc.t ref list

class c : ?layers:Astloc.t ref list -> Astloc.t -> object
  method collapse_backward : unit
  method get_layers : int -> Astloc.t ref list
  method get_level : int
  method get_loc : Astloc.t
  method get_loc_of_level : int -> Astloc.t
  method get_orig_loc : Astloc.t
  method size : int
  method to_loc : ?cache:(Fname.ext_cache_t option) -> unit -> Astloc.t
  method to_string : ?short:bool -> unit -> string
end

val dummy : c
val layers_eq : Astloc.t ref list -> Astloc.t ref list -> bool
val get_common_layers : c -> c -> Astloc.t ref list
val get_common_level : c -> c -> int
val merge : c -> c -> c
val dump_llocs : c list -> unit
val lines_of_llocs : c list -> int
val of_loc : Astloc.t -> c
val of_lexpos : Lexing.position -> c
