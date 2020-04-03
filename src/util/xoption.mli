val to_string : ('a -> string) -> ?prefix:string -> ?suffix:string -> 'a option -> string
val to_list : 'a option -> 'a list
val list_to_list : 'a option list -> 'a list
val list_opt_to_list : 'a list option -> 'a list
val map : ('a -> 'b) -> 'a option -> 'b option
val to_list_map : ('a -> 'b) -> 'a option -> 'b list
val iter : ('a -> unit) -> 'a option -> unit
