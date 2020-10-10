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

module Line : (Range.POSITION_T with type value = int) = struct
  type value = int

  let kind = "L"

  let eq p1 p2  = p1#value = p2#value
  let leq p1 p2 = p1#value <= p2#value
  let compare p1 p2 = Stdlib.compare p1#value p2#value

  class c = object (self)
    val mutable _value = (-1 : value)
    method is_valid      = _value >= 0
    method value         = _value
    method to_string     = string_of_int _value
    method of_string str = 
      try
	_value <- (int_of_string str)
      with _ -> raise (Invalid_format str)
  end

  class type t = c

end (* of module Line *)

module M = Range.F(Line)

class c = M.c

let eq = M.eq
let contains = M.contains
let has_meet = M.has_meet
let load = M.load
