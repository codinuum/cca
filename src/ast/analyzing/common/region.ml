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


exception Invalid_format of string

module Loader = struct

  let load new_region fpath =
    let l = ref [] in
    begin
      try
	let ich = open_in fpath in
	begin
	  try
	    while true do
	      let line = input_line ich in


	      let reg = new_region() in
	      begin
		try
		  reg#of_string line
		with 
		  Invalid_format s -> WARN_MSG "invalid format: %s" s
	      end;
	      l := reg::!l

	    done
	  with End_of_file -> ()
	end;
	close_in ich
      with exn ->
	WARN_MSG "load: %s" (Printexc.to_string exn);
	raise exn
    end;
    let res = List.rev !l in
    res

end
