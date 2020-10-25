(*
   Copyright 2013-2018 RIKEN
   Copyright 2018-2020 Chiba Institude of Technology

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

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)

(* 
 * AST for Fortran
 *
 * fortran/tree.ml
 *
 *)

module L = F_label
module B = Binding
module P = Printer
module I = Pinfo
module H = Labels.HeaderFile

let sprintf = Printf.sprintf

let conv_loc = L.conv_loc

let set_loc nd loc = nd#data#set_loc (conv_loc loc)

module Tree = Sourcecode.Tree (L)
open Tree


let make_local_name mn un = 
  if mn = "" then
    un
  else
    String.concat "-" [mn; un]

let make_include_node options ast_nd =
  let f = Fname.strip (ast_nd#lloc#get_loc_of_level 1).Common.Loc.filename in
  let h = H.mkgenerated f in
  let nd = 
    mknode options (L.PpDirective (L.PpDirective.mk (L.PpDirective.Include h))) [] 
  in
  set_loc nd (ast_nd#lloc#get_loc_of_level 0);
  nd

let rec has_subprogram ast_nd =
  match ast_nd#label with
  | L.InternalSubprogram _
  | L.ModuleSubprogram _ -> true
  | _ -> List.exists has_subprogram ast_nd#children


let of_ast options ast =
(*
  let mktid nd =
    Lang.mktid
      (if options#incomplete_info_flag then 
        "" 
      else 
        Xhash.to_hex (new c options nd false)#digest)
      (if options#incomplete_info_flag then 
        "" 
      else 
        nd#data#anonymized_label)
  in
*)
  let utbl = Hashtbl.create 0 in

  let proj_root = try options#fact_proj_roots.(0) with _ -> "" in
  let version = try options#fact_versions.(0) with _ -> Entity.unknown_version in

  let rec conv ?(orig_loc_flag=false) ?(label=None) ast_nd =
    DEBUG_MSG "orig_loc_flag=%B, ast_nd=%s" orig_loc_flag ast_nd#to_string;

    let lab =
      match label with
      | Some lab' -> lab'
      | None -> ast_nd#label
    in

    let is_incl nd =
      ast_nd#lloc#get_level = 0 && nd#lloc#get_level > 0
    in

    let proc_included_node nd =
      DEBUG_MSG "nd=%s" nd#to_string;
      let fn = nd#data#src_loc.Loc.filename in
      DEBUG_MSG "fn=%s" fn;
      try
        let fn_, path =
          if Filename.is_relative fn then
            (Filename.concat proj_root fn), fn
          else
            fn, (Xfile.relpath proj_root fn)
        in
        let digest = Xhash.digest_of_file options#fact_algo fn_ in
        let fid_str =
          Triple._encode_fid options ~digest ~path proj_root version
        in
        if nd#data#source_fid = "" then begin
          nd#data#set_source_fid fid_str
        end
      with
        _ ->
          WARN_MSG "failed to compute digest of %s" nd#to_string;
    in

    let rec conv_children = function
      | nd1::(nd2::rest as l) -> begin
          match nd1#label, nd2#label with
          | L.PartName n, L.SectionSubscriptList _ -> begin
              let x_opt0 = conv ~orig_loc_flag nd1 in
              let x_opt1 = conv ~orig_loc_flag ~label:(Some (L.SectionSubscriptList n)) nd2 in
              match x_opt0, x_opt1 with
              | Some x0, Some x1 -> x0 :: x1 :: (conv_children rest)
              | None, None -> conv_children rest
              | _ -> begin
                  Common.warning_msg "odd node sequence:\n%s\n%s"
                    nd1#to_string nd2#to_string;

                  conv_children rest
              end
          end
          | _ -> begin
              if is_incl nd1 then begin
                DEBUG_MSG "nd1=%s" nd1#to_string;
                match nd1#label with
                | L.InternalSubprogram _
                | L.ModuleSubprogram _
                | (L.PpBranch|L.PpSectionIfdef _|L.PpSectionIfndef _|L.PpSectionIf _) when has_subprogram nd1
                  -> begin (* to avoid dangling call sites *)
                    match conv ~orig_loc_flag:true nd1 with
                    | Some x -> x :: (conv_children l)
                    | None -> conv_children l
                  end
                | _ -> (make_include_node options nd1) :: (conv_children l)
              end
              else begin
                DEBUG_MSG "nd1=%s" nd1#to_string;
                match conv ~orig_loc_flag nd1 with
                | Some x -> x :: (conv_children l)
                | None -> conv_children l
              end
          end
      end
      | [nd] -> begin
          if is_incl nd then begin
            DEBUG_MSG "nd=%s" nd#to_string;
            match nd#label with
            | L.InternalSubprogram _
            | L.ModuleSubprogram _
            | (L.PpBranch|L.PpSectionIfdef _|L.PpSectionIfndef _|L.PpSectionIf _) when has_subprogram nd
              -> begin (* to avoid dangling call sites *)
                match conv ~orig_loc_flag:true nd with
                | Some x -> [x]
                | None -> []
              end
            | _ -> [make_include_node options nd]
          end
          else begin
            DEBUG_MSG "nd=%s" nd#to_string;
            Xoption.to_list (conv ~orig_loc_flag nd)
          end
      end
      | [] -> []
    in

    let children = conv_children ast_nd#children in

    if ast_nd#lloc#get_level > 0 && children = [] && not orig_loc_flag then
      None
    else begin
      DEBUG_MSG "ast_nd=%s" ast_nd#to_string;

      let binding = ast_nd#binding in
      let bindings = ast_nd#bindings in

      let info = ast_nd#info in

      let annot =
        let specs = ref [] in
        let lnames = ref [] in
        I.iter_external
          (fun (mn, un) ->
            lnames := (make_local_name mn un) :: !lnames
          ) info;
        begin
          match !lnames with
          | [] -> ()
          | _ -> specs := (L.Annotation.mkrequire !lnames) :: !specs
        end;
        begin
          match binding with
          | B.Def _ -> begin
              I.iter_name_spec
                (fun nspec ->
                  specs := (L.Annotation.mkspec nspec) :: !specs
                ) info
          end
          | _ -> ()
        end;
        begin
          List.iter
            (fun binding ->
              match binding with
              | B.Def _ -> begin
                  I.iter_name_spec
                    (fun nspec ->
                      specs := (L.Annotation.mkspec nspec) :: !specs
                    ) info
              end
              | _ -> ()
            ) bindings
        end;
        begin
          try
            let n = L.get_external_subprogram_name ast_nd#label in
            specs := (L.Annotation.mkprovide [n]) :: !specs
          with
            Not_found -> ()
        end;
        L.Annotation.from_specs !specs
      in

      let nd = mknode options ~annot lab children in
      begin
        match binding with
        | B.NoBinding -> ()
        | B.Def(bid, use) -> begin
            DEBUG_MSG "bid=%a" B.ID.ps bid;
            let b =
              match use with
              | B.Unknown -> begin
                  try
                    B.make_used_def bid (Hashtbl.find utbl bid)
                  with
                    Not_found -> binding
              end
              | B.Used c -> binding
            in
            nd#data#set_binding b
        end
        | B.Use(bid, _) -> begin
            DEBUG_MSG "bid=%a" B.ID.ps bid;
            nd#data#set_binding binding;
            try
              let c = Hashtbl.find utbl bid in
              Hashtbl.replace utbl bid (c+1)
            with
              Not_found ->
                Hashtbl.add utbl bid 1
        end
      end;
      begin
        List.iter
          (fun binding ->
            match binding with
            | B.NoBinding -> ()
            | B.Def(bid, use) -> begin
                DEBUG_MSG "bid=%a" B.ID.ps bid;
                let b =
                  match use with
                  | B.Unknown -> begin
                      try
                        B.make_used_def bid (Hashtbl.find utbl bid)
                      with
                        Not_found -> binding
                  end
                  | B.Used c -> binding
                in
                nd#data#add_binding b
            end
            | B.Use(bid, _) -> begin
                DEBUG_MSG "bid=%a" B.ID.ps bid;
                nd#data#add_binding binding;
                try
                  let c = Hashtbl.find utbl bid in
                  Hashtbl.replace utbl bid (c+1)
                with
                  Not_found ->
                    Hashtbl.add utbl bid 1
            end
          ) bindings
      end;
      let loc =
        if orig_loc_flag then
          ast_nd#orig_loc
        else
          ast_nd#loc
      in
      set_loc nd loc;
      if orig_loc_flag then
        proc_included_node nd;
      Some nd
    end
  in (* let rec conv *)

  let root_node =
    let rt = ast#root in
    match conv rt with
    | Some rn -> rn
    | None -> begin
        try
          make_include_node options rt
        with
          Failure _ -> assert false
    end
  in
  let tree = new c options root_node true in
  tree#collapse;
  tree#set_total_LOC ast#lines_read;
  tree#set_ignored_regions (ast#comment_regions @ ast#ignored_regions);
  tree#set_misparsed_regions ast#missed_regions;
  tree
