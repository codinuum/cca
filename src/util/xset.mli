type 'a t
val create : int -> 'a t
val clear : 'a t -> unit
val length : 'a t -> int
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val add : 'a t -> 'a -> unit
val add_set : 'a t -> 'a t -> unit
val remove : 'a t -> 'a -> unit
val remove_set : 'a t -> 'a t -> unit
val mem : 'a t -> 'a -> bool
val copy : 'a t -> 'a t
val to_list : 'a t -> 'a list
val from_list : 'a list -> 'a t
val for_all : ('a -> bool) -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val subset_eq : 'a t -> 'a t -> bool
val equals : 'a t -> 'a t -> bool
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
