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
 * binding.ml
 *)

module ID = struct

  type t = Local of int | Global of string

  let compare = Stdlib.compare

  let succ = succ
  let pred = pred

  let make_global s = Global s

  let is_global = function
    | Global _ -> true
    | _ -> false

  let is_local = function
    | Local _ -> true
    | _ -> false

  let to_raw = function
    | Local i -> string_of_int i
    | Global s -> s

  let to_string = function
    | Local i -> Printf.sprintf "#%db" i
    | Global s -> s

  let dummy = Local 0

  let is_dummy bid = bid = dummy

  class generator = object
    val mutable id = 0
    method reset = id <- 0
    method gen =
      id <- succ id;
      Local id
  end

  let p ch bid = Stdlib.output_string ch (to_string bid)

  let r ch bid = Stdlib.output_string ch (to_raw bid)

  let ps () bid = to_string bid

end (* of module Binding.ID *)

type use = Used of int | Unknown

let use_to_string = function
  | Used i  -> Printf.sprintf "Used:%d" i
  | Unknown -> "Unknown"

type is_local = bool

type t =
  | NoBinding
  | Def of ID.t * use * is_local
  | Use of ID.t * Loc.t option (* loc of def *)

let to_string = function
  | NoBinding         -> "NoBinding"
  | Def(bid, use, is_local) -> Printf.sprintf "Def(%a,%s,%B)" ID.ps bid (use_to_string use) is_local
  | Use(bid, loc_opt) ->
      Printf.sprintf "Use(%a%s)" ID.ps bid
        (match loc_opt with Some loc -> ":"^(Loc.to_string loc) | None -> "")

let make_def id u b = Def(id, u, b)

let make_used_def id n b = Def(id, Used n, b)
let make_unused_def id b = Def(id, Used 0, b)
let make_unknown_def id b = Def(id, Unknown, b)

let make_use ?(loc_opt=None) id = Use(id, loc_opt)

let is_none = function
  | NoBinding -> true
  | _ -> false

let is_use = function
  | Use _ -> true
  | _ -> false

let is_def = function
  | Def _ -> true
  | _ -> false

let is_local_def = function
  | Def(_, _, b) -> b
  | _ -> false

let is_non_local_def = function
  | Def(_, _, b) -> not b
  | _ -> false

let is_used_def = function
  | Def(_, Used n, _) -> n > 0
  | _ -> false

let is_unused_def = function
  | Def(_, Used 0, _) -> true
  | _ -> false

let get_bid = function
  | Def(bid, _, _) | Use(bid, _) -> bid
  | NoBinding -> raise Not_found

let get_bid_opt = function
  | Def(bid, _, _) | Use(bid, _) -> Some bid
  | NoBinding -> None

let get_use_count = function
  | Def(_, Used n, _) -> n
  | _ -> raise Not_found

let get_loc = function
  | Use(_, Some loc) -> loc
  | _ -> raise Not_found
