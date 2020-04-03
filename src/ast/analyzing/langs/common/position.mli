val ofs_array_block_size : int
val ofs_array_size_thresh : int

exception Pos_found of int * int

class manager : string -> object
  method filename : string
  method set_filename : string -> unit
  method feed : int -> unit
  method feedline : int -> unit
  method get_current_position : int * int
  method get_position : int -> int * int
  method lines_read : int
  method _offsets_to_loc : int -> int -> int -> int -> int -> int -> Astloc.t
  method offsets_to_loc : int -> int -> Astloc.t
  method lexposs_to_loc : ?get_position:bool -> Lexing.position -> Lexing.position -> Astloc.t
  method reset : unit
  method show_status : unit
  method to_string : string
end
