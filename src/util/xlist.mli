val to_string : ('a -> string) -> string -> 'a list -> string
val uniq : 'a list -> 'a list
val uniqq : 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val unionq : 'a list -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val intersectionq : 'a list -> 'a list -> 'a list
val overlap : 'a list -> 'a list -> bool
val overlapq : 'a list -> 'a list -> bool
val subtract : 'a list -> 'a list -> 'a list
val subtractq : 'a list -> 'a list -> 'a list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val max : 'a list -> 'a
val min : 'a list -> 'a
val first : 'a list -> 'a
val last : 'a list -> 'a
val firstn : int -> 'a list -> 'a list
val lastn : int -> 'a list -> 'a list
val balance : 'a list * 'b list -> 'a list * 'b list
val range : int -> int list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val partition_at_last : 'a list -> 'a list * 'a
