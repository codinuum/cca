class c : object
  method comment_LOC : int
  method comment_regions : (int * int) list
  method file_name : string
  method ignored_LOC : int
  method ignored_regions : (int * int) list
  method lines_read : int
  method missed_LOC : int
  method missed_regions : (int * int) list
  method set_comment_LOC : int -> unit
  method set_comment_regions : (int * int) list -> unit
  method set_file_name : string -> unit
  method set_ignored_LOC : int -> unit
  method set_ignored_regions : (int * int) list -> unit
  method set_lines_read : int -> unit
  method set_missed_LOC : int -> unit
  method set_missed_regions : (int * int) list -> unit
end
