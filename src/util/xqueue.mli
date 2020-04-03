exception Empty
type 'a cell = { mutable content : 'a; mutable next : 'a cell; }
type 'a t = { mutable length : int; mutable tail : 'a cell; }
class ['a] c : object ('c)
  method add : 'a -> unit
  method clear : unit
  method copy : 'c
  method filter : ('a -> bool) -> unit
  method fold : ('b -> 'a -> 'b) -> 'b -> 'b
  method is_empty : bool
  method iter : ('a -> unit) -> unit
  method length : int
  method peek : 'a
  method peek_last : 'a
  method peek_nth : int -> 'a
  method prepend : 'a -> unit
  method prepend_from : 'c -> unit
  method remove_last : unit
  method replace : ('a -> 'a) -> unit
  method repr : 'a t
  method take : 'a
  method transfer : 'c -> unit
end
