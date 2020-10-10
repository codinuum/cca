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
(* 
 * Unique IDs 
 *
 * UID.ml
 *
 *)

type t = Int64.t

let compare = Int64.compare

let succ = Int64.succ
let pred = Int64.pred

let to_raw (u : t) = Int64.to_string u
let to_string (u : t) = "#"^(to_raw u)^"U"

let dummy = (0L : t)

let unknown = (-1L: t)

class generator = object
  val mutable id = (0L : t)
  method reset = id <- (0L : t)
  method gen =
    id <- succ id;
    (id : t)
end


let p ch (u : t) = Stdlib.output_string ch (to_string u)

let r ch (u : t) = Stdlib.output_string ch (to_raw u)

let ps () (u : t) = to_string u

let rs () (u : t) = to_raw u
