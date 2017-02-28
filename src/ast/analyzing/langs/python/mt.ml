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

let _ =
  let mutex = Mutex.create() in
  Xthread.register_functions
    { Xthread.f_enter_critical_section = (fun () -> Mutex.lock mutex);
      Xthread.f_exit_critical_section  = (fun () -> Mutex.unlock mutex);
    }
