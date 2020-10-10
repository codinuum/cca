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

module H = HIS.Int

let _ =
  let a1 = [|"X";"M";"J";"Y";"A";"U";"Z"|] in
  let a2 = [|"M";"Z";"J";"A";"W";"X";"U"|] in
  let w i j x = 1 (* if a1.(i) = a2.(j) then 1 else 0 *) in
  let res = H.compute w a1 a2 in
  List.iter
    (fun (i, j) ->
      Printf.printf "%s\n" a1.(i)
    ) res;
  Printf.printf "\n"

let _ =
  let w_tbl = [|6;5;2;1;59;13;20|] in
  let a1 = [|0;1;2;3;4;5;6|] in
  let a2 = [|0;1;4;2;3;5;6|] in
  let w i j x = w_tbl.(a2.(j)) in
  let res = H.compute w a1 a2 in
  List.iter
    (fun (i, j) ->
      Printf.printf "%d\n" a1.(i)
    ) res;
  Printf.printf "\n"

let _ =
  let a1 = Array.of_list (Xlist.range 100) in
  let a2 = [|18;48;29;39;76;36;81;33;43;4;49;92;74;12;45;6;7;30;58;9;88;59;26;1;47;14;84;38;94;77;2;10;61;66;11;89;97;31;8;75;20;73;71;3;15;40;27;90;21;46;99;42;60;91;41;79;98;70;55;17;83;80;86;51;16;82;24;64;53;78;34;63;32;96;13;0;67;25;93;19;28;65;62;95;56;5;69;54;22;35;87;57;50;85;52;72;23;44;68;37|] in
  let res = ref [] in
  for c = 1 to 100 do
    let w i j _ = 1 in
    res := H.compute w a1 a2
  done;
  List.iter
    (fun (i, j) ->
      Printf.printf "%d\n" a1.(i)
    ) !res;
  Printf.printf "\n"


let _ = 
  let w_tbl = [|6;9;5;9;9|] in
  let a1 = [|"A";"A";"B";"C";"D"|] in
  let a2 = [|"A";"D";"E";"F";"D"|] in
  let ai1 = [|0;1;2;3;4|] in
  let ai2 = [|2;3;4;1;0|] in
  let w i j x = 
    let k1 = ai1.(i) in
    let k2 = ai2.(j) in
    w_tbl.(k1) + w_tbl.(k2)
  in
  let res = H.compute w a1 a2 in
  List.iter
    (fun (i, j) ->
      Printf.printf "(%s,%d) - (%s,%d)\n" a1.(i) ai1.(i) a2.(j) ai2.(j)
    ) res
