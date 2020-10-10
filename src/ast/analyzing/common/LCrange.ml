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

open Region

module LineColumn : (Range.POSITION_T with type value = int * int) = struct
  type value = int * int

  let kind = "LC"

  let eq p1 p2 = 
    let l1, c1 = p1#value in
    let l2, c2 = p2#value in
    l1 = l2 && c1 = c2

  let leq p1 p2 = 
    let l1, c1 = p1#value in
    let l2, c2 = p2#value in
    l1 < l2 || (l1 = l2 && c1 <= c2)

  let compare p1 p2 =
    if eq p1 p2 then 
      0
    else if leq p1 p2 then
      -1
    else
      1

  class c = object (self)
    val mutable _value = ((-1, -1) : value)

    method is_valid  = (fst _value) >= 0 && (snd _value) >= 0
    method value     = _value

    method to_string = Printf.sprintf "%d,%d" (fst _value) (snd _value)

    method of_string str = 
      match Str.split (Str.regexp_string ",") str with
      | [l;c] -> begin
	  try
	    _value <- (int_of_string l, int_of_string c)
	  with _ -> raise (Invalid_format str)
      end
      | _ -> raise (Invalid_format str)
  end
      
  class type t = c
      
end (* of module LineColumn *)

module M = Range.F(LineColumn)

class c = M.c
let eq = M.eq
let contains = M.contains
let has_meet = M.has_meet
let load = M.load
