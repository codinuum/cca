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

module F (Pos : Range.POSITION_T) = struct

  module R = Range.F(Pos)

  class c = object (self)
    val mutable _root     = new R.c
    val mutable _excluded = []  (* range list *)

    val mutable _rep = ""       (* ex. RANGE~RANGE,...,RANGE *)

    method is_valid =
      _root#is_valid &&
      (List.for_all (fun r -> r#is_valid) _excluded)

    method set rt exc =
      _root     <- rt;
      _excluded <- exc

    method root     = _root
    method excluded = _excluded 

    method to_string = 
      if _rep = "" then begin
	_rep <- Printf.sprintf "%s~%s" 
	    _root#to_string
	    (Xlist.to_string (fun r -> r#to_string) "," _excluded);
	_rep
      end
      else
	_rep

    method of_string str =
      match Str.split (Str.regexp_string "~") str with
      | [rt;exc] ->
	  _root#of_string rt;
	  _excluded <-
	    (List.map
	       (fun s ->
		 let r = new R.c in
		 r#of_string s;
		 r
	       ) (Str.split (Str.regexp_string ",") exc)
	    )

      | _ -> raise (Invalid_format str)
	    

  end (* of class Fragment.F.c *)

  let load = Region.Loader.load (fun () -> new c)
  
end (* of module Fragment.F *)
