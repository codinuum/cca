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

class c = object
  val mutable ctime = 0.0
  val mutable count = 0
  val stack = Stack.create()

  method reset =
    ctime <- 0.0;
    count <- 0;
    Stack.clear stack

  method start = 
    Stack.push ((Unix.times()).Unix.tms_utime) stack;
    count <- count + 1

  method stop = 
    let stime = Stack.pop stack in
    if Stack.length stack = 0 then begin
      let time = (Unix.times()).Unix.tms_utime -. stime in
      ctime <- ctime +. time
    end

  method ctime = ctime
  method count = count

  method show =
    Printf.printf "[Inst.c] total time: %f count: %d\n" ctime count

end
