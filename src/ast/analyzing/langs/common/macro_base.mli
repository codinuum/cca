module Loc = Astloc
type stat = Unresolved | Resolved of Obj.t

val stat_to_string : stat -> string
val stat_resolved : stat -> bool
val tok_of_stat : stat -> 'a

type line = {
  ln_raw : string;
  mutable ln_stat : stat;
  mutable ln_conditional : bool;
  ln_loc : Loc.t;
}

val mk_line : ?loc:Loc.t -> ?raw:string -> ?conditional:bool -> ('a -> 'b) -> 'a -> line
val line_to_string : line -> string
val tok_of_line : line -> 'a
val _resolve_line : line -> Obj.t -> unit
val resolve_line : line -> 'a -> unit
val line_resolved : line -> bool
type body = Object of line | Function of string list * line
val mk_obj_body : ?loc:Loc.t -> ?stat:stat -> ?conditional:bool -> string -> body
val mk_fun_body : ?loc:Loc.t -> ?stat:stat -> ?conditional:bool -> string list -> string -> body
val line_of_body : body -> line
val body_to_string : body -> string
val body_to_rep : body -> string
val body_length : body -> int
val _resolve_body : Obj.t -> body -> unit
val resolve_body : 'a -> body -> unit
val body_is_conditional : body -> bool
val body_set_conditional : body -> unit
val body_clear_conditional : body -> unit

class table : string -> object
  method clear : unit
  method define : ?conditional:bool -> string -> body -> unit
  method expose : string -> unit
  method find : string -> body
  method find_all : string -> body list
  method hide : string -> unit
  method is_defined : string -> bool
  method is_unconditionally_defined : string -> bool
  method is_undefined : string -> bool
  method is_uniquely_defined : string -> bool
  method readonly : bool
  method set_readonly : unit
  method undefine : string -> unit
end
