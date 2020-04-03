
val to_string : ('a -> string) -> string -> 'a array -> string
val filter : ('a -> bool) -> 'a array -> 'a array
val exists : ('a -> bool) -> 'a array -> bool
val for_all : ('a -> bool) -> 'a array -> bool
val iter2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> unit

exception Index_out_of_bounds of int

class ['a] array_list : int -> object
  method clear : unit
  method add : 'a -> unit
  method get : int -> 'a
  method set : int -> 'a -> unit
  method is_empty : bool
  method iter : ('a -> unit) -> unit
  method iteri : (int -> 'a -> unit) -> unit
  method length : int
  method to_string : ('a -> string) -> string
end

module A = Bigarray.Array1

val kind : (int, Bigarray.int_elt) Bigarray.kind
val layout : Bigarray.c_layout Bigarray.layout

class int_array_list : int -> object
  method clear : unit
  method add : int -> unit
  method get : int -> int
  method set : int -> int -> unit
  method is_empty : bool
  method iter : (int -> unit) -> unit
  method iteri : (int -> int -> unit) -> unit
  method length : int
  method to_string : string
end
