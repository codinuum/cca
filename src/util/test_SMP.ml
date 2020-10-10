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

let test() =
  let a1 = [|621;583;352|] in
  let a2 = [|3412;3080;3078;1808|] in
  let score x y =
    match x, y with
    | 621, 3412 -> 3.69
    | 621, 3080 -> 4.10
    | 621, 3078 -> 4.14
    | 621, 1808 -> 4.16

    | 583, 3412 -> 4.69
    | 583, 3080 -> 3.74
    | 583, 3078 -> 4.76
    | 583, 1808 -> 4.72

    | 352, 3412 -> 5.66
    | 352, 3080 -> 4.22
    | 352, 3078 -> 6.47
    | 352, 1808 -> 6.52

    | _ -> 0.0
  in
  let cmpr = new SMP.ComparatorFloat.c score a1 a2 in
  let matches = SMP.get_stable_matches ~check:true cmpr a1 a2 in
  List.iter
    (fun (x, y) -> Printf.printf "%d-%d\n" x y)
    matches


let _ =
  test()
