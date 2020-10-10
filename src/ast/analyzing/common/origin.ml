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
(* origin.ml *)


open Bigarray

let printf  = Printf.printf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf

let attr_unknown = (-2)
let attr_not_initialized = (-1)


type range = {
    st    : int; 
    ed    : int;
    attri : int; 
  }

let create_range a s e =
  {st=s; ed=e; attri=a}

let range_to_string {st=s; ed=e; attri=a} =
  sprintf "<%d-%d(%d)>" s e a

class abuf bufsize = object(self)
  val mutable _buf = 
    Array1.create Bigarray.int16_signed Bigarray.c_layout bufsize
    
  initializer
    Array1.fill _buf attr_unknown
      
  method put range =
    DEBUG_MSG "%d-%d" range.st range.ed;
    let ed = 
      if range.ed >= bufsize then begin
	WARN_MSG "range too large: end=%d (bufsize=%d)" range.ed bufsize;
	bufsize - 1 
      end
      else 
	range.ed 
    in
    for i = range.st to ed do
      _buf.{i} <- range.attri
    done

  method to_list =
    let l = ref [] in
    let st = ref (-1) in
    let attri = ref attr_not_initialized in

    for i = 0 to bufsize - 1 do

      if !st >= 0 then begin
	if !attri <> _buf.{i} then begin
	  l := (create_range !attri !st (i - 1))::!l;
	  st := i;
	  attri := _buf.{i}
	end
      end
      else begin
	st := i;
	attri := _buf.{i}
      end

    done;
    let result = List.rev !l in
    result

  method to_string =
    String.concat "\n" (List.map (fun r -> range_to_string r) self#to_list)


  method _dump_ch ch =
    List.iter
      (fun r ->
	let a = r.attri in
	if a <> attr_unknown then
	  fprintf ch "%d %d %d\n" a r.st r.ed
      ) self#to_list

  method dump fname = Xfile.dump fname self#_dump_ch


end (* of class Origin.abuf *)
