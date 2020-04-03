exception File_found of Storage.file
class ['b] c : object
  constraint 'b = #Source_base.c
  method _set_keep_going_flag : bool -> unit
  method _set_verbose_flag : bool -> unit
  method add_extra_source_file : Storage.file -> unit
  method add_lines_read : int -> unit
  method add_search_path : string -> unit
  method clear_keep_going_flag : unit
  method clear_lines_read : unit
  method clear_verbose_flag : unit
  method comment_regions : Regions.c
  method current_filename : string
  method current_pos_mgr : Position.manager
  method current_source : 'b
  method enter_source : 'b -> Compat.Ulexing.lexbuf
  method exit_source : unit
  method extra_source_files : Storage.file list
  method find_path : ?ignore_case:bool -> string -> Storage.file list
  method ignored_regions : Regions.c
  method in_included_file : bool
  method init : unit
  method keep_going : bool
  method lines_read : int
  method missed_regions : Regions.c
  method set_current_filename : string -> unit
  method set_enter_source_callback : ('b -> Compat.Ulexing.lexbuf) -> unit
  method set_keep_going_flag : unit
  method set_search_path_list : string list -> unit
  method set_verbose_flag : unit
  method source_entered : 'b -> bool
  method verbose : bool
  method verbose_msg : ('a, unit, string, unit) format4 -> 'a
end
