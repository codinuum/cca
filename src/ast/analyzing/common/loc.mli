type t = {
  mutable filename : string;
  start_offset : int;
  end_offset : int;
  start_line : int;
  start_char : int;
  end_line : int;
  end_char : int;
}

val dummy : t
val ghost : t
val make : ?fname:string -> int -> int -> int -> int -> int -> int -> t
val _merge : t -> t -> t
val lines : t -> int
val is_contained : t -> t -> bool
val merge : t -> t -> t
val extend_end : t -> int -> t
val to_string : ?long:bool -> t -> string
val to_attr_value : t -> string
val compare : t -> t -> int
val meet : t -> t -> t
