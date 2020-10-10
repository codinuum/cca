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

module type POSITION_T = sig
  type value

  val kind : string

  class type t = object ('self)
    val mutable _value : value
    method is_valid  : bool
    method value     : value
    method to_string : string
    method of_string : string -> unit
  end

  val eq      : t -> t -> bool
  val leq     : t -> t -> bool
  val compare : t -> t -> int

  class c : t

end (* of module type Range.POSITION_T *)


module F (Pos : POSITION_T) = struct

  class c = object (self)

    val mutable _start = new Pos.c
    val mutable _end   = new Pos.c

    val mutable _rep = ""

    method is_valid =
      _start#is_valid && _end#is_valid && (Pos.leq _start _end)

    method start = _start
    method end_  = _end

    method set (st : Pos.t) (ed : Pos.t) =
      _start <- st;
      _end <- ed

    method to_string =
      if _rep = "" then begin
	_rep <- Printf.sprintf "%s-%s" _start#to_string _end#to_string;
	_rep
      end
      else
	_rep

    method of_string str =
      match Str.split (Str.regexp_string "-") str with
      | [s;e] -> 
	  _start#of_string s;
	  _end#of_string e

      | _ -> raise (Invalid_format str)

  end (* of class Range.F.c *)

  let eq r1 r2 = Pos.eq r1#start r2#start && Pos.eq r1#end_ r2#end_

  let contains r1 r2 = Pos.leq r1#start r2#start && Pos.leq r2#end_ r1#end_

  let pos_max p1 p2 = if Pos.leq p1 p2 then p2 else p1
  let pos_min p1 p2 = if Pos.leq p1 p2 then p1 else p2

  let has_meet r1 r2 =
    let st1, ed1 = r1#start, r1#end_ in
    let st2, ed2 = r2#start, r2#end_ in
    let st = pos_max st1 st2 in
    let ed = pos_min ed1 ed2 in
    Pos.leq st ed
    

  let load = Loader.load (fun () -> new c)

end (* of functor Range.F *)
