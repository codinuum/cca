(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* xhash.ml *)

module C = Cryptokit
module NC = Netchannels

type t = Digest.t

type algo =
  | MD5
  | SHA1
  | SHA256
  | RIPEMD160

let algo_to_string = function
  | MD5       -> "MD5"
  | SHA1      -> "SHA1"
  | SHA256    -> "SHA256"
  | RIPEMD160 -> "RIPEMD160"

let _algo_to_hash algo =
  match algo with
  | MD5       -> C.Hash.md5()
  | SHA1      -> C.Hash.sha1()
  | SHA256    -> C.Hash.sha256()
  | RIPEMD160 -> C.Hash.ripemd160()

let digest_of_string algo str =
  (C.hash_string (_algo_to_hash algo) str : t)

let digest_of_file algo fname =
  (Xfile.load fname (C.hash_channel (_algo_to_hash algo)) : t)

let git_digest_of_ch ch =
  let hash = C.Hash.sha1() in
  let len = Stdlib.in_channel_length ch in
  let header = Printf.sprintf "blob %d\000" len in
  let ich = NC.input_channel ch in
  begin
    hash#add_string header;
    hash#add_string (NC.string_of_in_obj_channel ich)
  end;
  ich#close_in();
  hash#result

let git_digest_of_file fname =
  Xfile.load fname git_digest_of_ch

let path_digest_of_ch path ch =
  let hash = C.Hash.sha1() in
  let header = Printf.sprintf "%s\000" path in
  let ich = NC.input_channel ch in
  begin
    hash#add_string header;
    hash#add_string (NC.string_of_in_obj_channel ich)
  end;
  hash#result

let path_digest_of_file fname =
  Xfile.load fname (path_digest_of_ch fname)


let to_hex (d : t) = 
  C.transform_string (C.Hexa.encode()) d

let digest_hex_of_string algo str =
  to_hex (digest_of_string algo str)

let digest_hex_of_file algo fname =
  to_hex (digest_of_file algo fname)

let of_hex s =
  C.transform_string (C.Hexa.decode()) s
