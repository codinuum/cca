module C = Cryptokit
module NC = Netchannels
type t = Digest.t
type algo = MD5 | SHA1 | SHA256 | RIPEMD160
val algo_to_string : algo -> string
val _algo_to_hash : algo -> Cryptokit.hash
val digest_of_string : algo -> string -> t
val digest_of_file : algo -> string -> t
val git_digest_of_ch : in_channel -> string
val git_digest_of_file : string -> string
val path_digest_of_ch : string -> in_channel -> string
val path_digest_of_file : string -> string
val to_hex : t -> string
val digest_hex_of_string : algo -> string -> string
val digest_hex_of_file : algo -> string -> string
val of_hex : string -> string
