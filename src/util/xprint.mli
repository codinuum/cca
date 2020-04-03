val cmd_name : string
val failure : ('a, unit, string, 'b) format4 -> 'a
val message : ('a, unit, string, unit) format4 -> 'a
val verbose : bool -> ('a, unit, string, unit) format4 -> 'a
val warning : ?out:out_channel -> ?head:string -> ('a, unit, string, unit) format4 -> 'a
val error   : ?out:out_channel -> ?head:string -> ('a, unit, string, unit) format4 -> 'a
val println : ('a, unit, string, unit) format4 -> 'a
