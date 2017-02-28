(*
   Copyright 2012-2017 Codinuum Software Lab <http://codinuum.com>

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

  let compare = Pervasives.compare

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

  let p ch bid = Pervasives.output_string ch (to_string bid)

  let r ch bid = Pervasives.output_string ch (to_raw bid)

  let ps () bid = to_string bid

end (* of module Binding.ID *)

type use = Used of int | Unknown

let use_to_string = function
  | Used i  -> Printf.sprintf "Used:%d" i
  | Unknown -> "Unknown"

type t = None | Def of ID.t * use | Use of ID.t

let to_string = function
  | None          -> "None"
  | Def(bid, use) -> Printf.sprintf "Def(%a,%s)" ID.ps bid (use_to_string use)
  | Use bid       -> Printf.sprintf "Use(%a)" ID.ps bid

let make_def id u = Def(id, u)

let make_used_def id n = Def(id, Used n)
let make_unused_def id = Def(id, Used 0)
let make_unknown_def id = Def(id, Unknown)

let make_use id = Use id

let is_none = function
  | None -> true
  | _ -> false

let is_use = function
  | Use _ -> true
  | _ -> false

let is_def = function
  | Def _ -> true
  | _ -> false

let is_used_def = function
  | Def(_, Used n) -> n > 0
  | _ -> false

let is_unused_def = function
  | Def(_, Used 0) -> true
  | _ -> false

let get_bid = function
  | Def(bid, _) | Use bid -> bid
  | None -> raise Not_found

let get_bid_opt = function
  | Def(bid, _) | Use bid -> Some bid
  | None -> None

let get_use_count = function
  | Def(_, Used n) -> n
  | _ -> raise Not_found
