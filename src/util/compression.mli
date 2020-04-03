val gzip_default_compression_level : int
class virtual c : object
  method virtual ext : string
  method virtual is_compressed : bool
  method virtual level : int
  method virtual set_level : int -> unit
  method virtual to_string : string
end
val none : c
val gzip : c
val extensions : (string * c) list
val from_filename : string -> c
