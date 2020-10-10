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

(* diffviewer *)
let viewer_mode_status_SAME         = char_of_int(0)
let viewer_mode_status_OK           = char_of_int(1)
let viewer_mode_status_ERROR        = char_of_int(2)
let viewer_mode_status_EXT_MISMATCH = char_of_int(3)

let viewer_mode_status_PARSE        = char_of_int(4)
let viewer_mode_status_OUTLINE_COMP = char_of_int(5)
let viewer_mode_status_POSTPROCESS  = char_of_int(6)
let viewer_mode_status_EDITSEQ_GEN  = char_of_int(7)
let viewer_mode_status_DONE         = char_of_int(8)


let default_moderate_nchildren_threshold = 64


