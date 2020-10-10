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
(* 
 * position handling 
 *
 * position.ml
 *
 *)



let ofs_array_block_size = 512
let ofs_array_size_thresh = 9 (* > 1 *)

exception Pos_found of int * int


class manager (fn : string) = object (self)

  val mutable filename = fn

  val mutable lines_read = 0
  val mutable start_offset = 0
  val mutable characters_read = 0

  val mutable newline = 0

  val mutable last_lines_read = 0
  val mutable last_newline = 0

  val ofs_array = new Xarray.int_array_list ofs_array_block_size

  method to_string =
    let l = [
      "lines_read", lines_read;
      "characters_read", characters_read;
      "newline", newline;
      "last_lines_read", last_lines_read;
      "last_newline", last_newline;
    ] in
    String.concat ","
      ((Printf.sprintf "filename:\"%s\"" filename)::
       (List.map (fun (k, v) -> Printf.sprintf "%s=%d" k v) l))

  method filename = filename
  method set_filename f = filename <- f


  method feed n = characters_read <- characters_read + n

  method feedline n =
    last_newline <- newline;
    last_lines_read <- lines_read;

    lines_read <- lines_read + 1;
    start_offset <- characters_read;
    characters_read <- characters_read + n;
    newline <- characters_read - 1;
    ofs_array#add newline

  method get_position cnum =
    let len = ofs_array#length in
    match cnum with
    | 0 -> 1, 0
    | _ when cnum > newline -> begin
        let (l, c) as lc = lines_read + 1, cnum - newline - 1 in
        lc
    end
    | _ when cnum > last_newline -> begin
        let (l, c) as lc = last_lines_read + 1, cnum - last_newline - 1 in
        lc
    end
    | _ when len < ofs_array_size_thresh -> begin
        (*Printf.fprintf stderr "cnum=%d ofs_array=%s\n%!"
          cnum (ofs_array#to_string string_of_int);*)
        if 0 < cnum && cnum <= ofs_array#get(0) then begin
          1, cnum
        end
        else begin
          try
            for i = 1 to len - 1 do
              let st = ofs_array#get(i - 1) in
              let ed = ofs_array#get(i) in
              (*Printf.fprintf stderr "  i=%d st=%d ed=%d\n%!" i st ed;*)
              if st < cnum && cnum <= ed then
                raise (Pos_found (i + 1, cnum - st - 1))
            done;
            0, 0
          with
          | Pos_found (l, c) -> l, c
        end
    end
    | _ -> begin
        let l = ref 0 in
        let r = ref (ofs_array#length - 1) in
        let m = ref ((!l + !r) / 2) in
        (*Printf.fprintf stderr "cnum=%d |ofs_array|=%d\n%!" cnum ofs_array#length;*)
        try
          while true do
            if !l > !r then
              raise Not_found
            else
              if !m = 0 then
                if 0 < cnum && cnum <= ofs_array#get(0) then
                  raise (Pos_found (1, cnum))
                else
                  raise Not_found
              else
                let st = ofs_array#get(!m - 1) in
                let ed = ofs_array#get(!m) in
                (*Printf.fprintf stderr "  l=%d r=%d m=%d st=%d ed=%d\n%!" !l !r !m st ed;*)
                if st < cnum && cnum <= ed then
                  raise (Pos_found (!m + 1, cnum - st - 1))
                else if cnum <= st then
                  r := !m - 1
                else if cnum > ed then
                  l := !m + 1;
                m := (!l + !r) / 2
          done;
          0, 0
        with
        | Pos_found (l, c) -> l, c
        | Not_found -> 0, 0
    end

  method get_current_position =
    self#get_position start_offset

  method reset =
    lines_read <- 0; 
    start_offset <- 0; 
    characters_read <- 0;
    newline <- 0;
    last_lines_read <- 0;
    last_newline <- 0;
    ofs_array#clear

  method lines_read = lines_read

  method show_status = 
    Printf.printf "\nLines read: %d\nCharacters read: %d\n" 
      lines_read characters_read

  method _offsets_to_loc start_offset end_offset start_line start_char end_line end_char =
    let loc =
      Astloc.make ~fname:filename
        start_offset end_offset start_line start_char end_line end_char
    in
    loc

  method offsets_to_loc start_offset end_offset =
    let start_line, start_char = self#get_position start_offset in
    let end_line, end_char     = self#get_position end_offset in
    let loc =
      self#_offsets_to_loc start_offset end_offset start_line start_char end_line end_char
    in
    loc

  method lexposs_to_loc ?(get_position=true) st_pos ed_pos =
    if get_position then
      self#offsets_to_loc st_pos.Lexing.pos_cnum ed_pos.Lexing.pos_cnum
    else
      let so = st_pos.Lexing.pos_cnum in
      let eo = ed_pos.Lexing.pos_cnum in
      self#_offsets_to_loc so eo
        st_pos.Lexing.pos_lnum (so - st_pos.Lexing.pos_bol)
        ed_pos.Lexing.pos_lnum (eo - ed_pos.Lexing.pos_bol)


  initializer
    self#reset

    
end (* of class Position.manager *)
