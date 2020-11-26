(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Cmdliner
open Printf
open Git
open Git_unix

let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";
  `P "Masatomo Hashimoto  <m.hashimoto@stair.center>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/ocaml-git/issues.";
]

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("\r%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let term_info title ~doc ~man =
  let man = man @ help_sections in
  Term.info ~sdocs:global_option_section ~doc ~man title

type command = {
  name: string;
  doc : string;
  man : Manpage.block list;
  term: unit Term.t;
}

let command c =
  let man = [
    `S "DESCRIPTION";
    `P c.doc;
  ] @ c.man in
  c.term, term_info c.name ~doc:c.doc ~man

let mk (fn:'a): 'a Term.t = Term.(pure (fun () -> fn) $ setup_log)

(* Helpers *)
let mk_flag ?section flags doc =
  let doc = Arg.info ?docs:section ~doc flags in
  Arg.(value & flag & doc)

let mk_opt ?section flags value doc mk default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(value & opt mk default & doc)

(*let mk_required ?section flags value doc mk default =
  let doc = Arg.info ?docs:section ~docv:value ~doc flags in
  Arg.(required & opt mk default & doc)*)

let endp =
  let cv x = Ok (endpoint (Uri.of_string x)) in
  let pr ppf name = Format.pp_print_string ppf (Uri.to_string name.Net.uri) in
  Arg.conv ~docv:"<endpoint>" (cv, pr)

let remote =
  let doc = Arg.info ~docv:"REPOSITORY"
      ~doc:"Location of the remote repository." [] in
  Arg.(required & pos 0 (some (*gri*)endp) None & doc)

module S = struct (* Git_unix.Store does not work. Why? *)
  module I = Git.Inflate
  module D = Git.Deflate
  include Git.Store.Make (Digestif.SHA1) (Git_unix.Fs) (I) (D)

  let v ?dotgit ?compression ?buffer root =
    v ?dotgit ?compression ?buffer () root
end

module Hash   = S.Hash
module Value  = S.Value
module Blob   = Value.Blob
module Commit = Value.Commit
module Tree   = Value.Tree
module Tag    = Value.Tag

let unpack =
  mk_flag ["unpack"] "Unpack the received pack archive."

let run t =
  Lwt_main.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

(* DIFFAST *)
let verbose() =
  match Logs.level() with
  | Some Logs.Info -> true
  | _ -> false

class diffast_args = object (self)

  method repo =
    let doc = Arg.info [] ~docv:"REPO_PATH" ~doc:"The repository path." in
    let path =
      let cv x = Ok Fpath.(v x) in
      let pr = Fpath.pp in
      Arg.conv ~docv:"<path>" (cv, pr)
    in
    Arg.(required & pos 0 (some path) None & doc ) 

  method clearcache = mk_flag ["clearcache"] "Clear diff cache."

  method external_parser = mk_flag ["parser:external"] "Rely on external parsers."

  method dump_delta = mk_flag ["dump:delta"] "Output delta."

  method dump_fact = mk_flag ["dump:fact"] "Output fact."

  method disable_parser =
    mk_opt ["parser:disable"] "P,..." "Disable parsers."
      Arg.(some string) None

  method cache_dir_base =
    mk_opt ["cache"] "DIR" "Set cache dir base to DIR."
      Arg.(some string) None

  method fact_proj = 
    mk_opt ["fact:project"] "ID" "Set project ID." 
      Arg.(some string) None

  method fact_into_virtuoso = 
    mk_opt ["fact:into-virtuoso"] "URI" "Output fact into graph <URI> in virtuoso." 
      Arg.(some string) None

  method fact_into_directory = 
    mk_opt ["fact:into-directory"] "DIR" "Output fact into directory DIR." 
      Arg.(some string) None

  method fact_enc = 
    mk_opt ["fact:encoding"] "ENC" "Set fact encoding to ENC (FDO|FDLC|FDLO|FDLCO)." 
      Arg.(some string) (Some "FDLCO")

  method fact_size_thresh = 
    mk_opt ["fact:size-thresh"] "N" "Set fact buffer size threshold to N." 
      Arg.(some int) None

  method local_cache_name =
    mk_opt ["local-cache-name"] "NAME" "Set local cache name to NAME."
      Arg.(some string) None

    
end (* of class diffast_options *)

let get_opts 
    ~verbose
    ~external_parser
    ~disable_parser
    ~clearcache
    ~cache_dir_base
    ~dump_delta
    ~dump_fact
    ~fact_proj
    ~fact_size_thresh
    ~fact_enc
    ~fact_into_virtuoso
    ~fact_into_directory
    ~local_cache_name
    =
  let options = new Options.c in

  if verbose then
    options#set_verbose_flag;

  if external_parser then
    options#set_external_parser_flag;

  if clearcache then
    options#set_clear_cache_flag;

  if dump_delta then
    options#set_dump_delta_flag;

  begin
    match disable_parser with
    | Some ps -> List.iter options#disable_parser (Str.split (Str.regexp ",") ps)
    | None -> ()
  end;
  begin
    match cache_dir_base with
    | Some p -> options#set_cache_dir_base p
    | None -> ()
  end;

  if dump_fact then begin
    options#set_fact_flag;

    begin
      match fact_size_thresh with
      | Some n -> options#set_fact_size_threshold n
      | None -> ()
    end;
    options#set_fact_algo Xhash.SHA1;
    options#set_fact_for_changes_flag;
    options#set_fact_for_mapping_flag;
    options#set_fact_for_ast_flag;
    begin
      match fact_proj with
      | Some p -> options#set_fact_proj p
      | None -> ()
    end;
    begin
      match fact_enc with
      | Some e -> begin
          match e with
          | "FDO" ->   options#set_fact_enc Entity.FDO
          | "FDLO" ->  options#set_fact_enc Entity.FDLO
          | "FDLC" ->  options#set_fact_enc Entity.FDLC
          | "FDLCO" -> options#set_fact_enc Entity.FDLCO
          | e ->
              eprintf "[ERROR] invalid encoding \"%s\"\n" e;
              exit 1
      end
      | None -> ()
    end;
    begin
      match fact_into_virtuoso with
      | Some p -> options#set_fact_into_virtuoso p
      | None -> ()
    end;
    begin
      match fact_into_directory with
      | Some p -> options#set_fact_into_directory p
      | None -> ()
    end
  end;
  begin
    match local_cache_name with
    | Some n -> options#set_local_cache_name n
    | None -> ()
  end;

  options

let extract = {
  name = "extract";
  doc = "Extract fact from Git object.";
  man = [];
  term = 
  let args = new diffast_args in
(*
  let sha1 =
    let doc = Arg.info [] ~docv:"SHA1" ~doc:"A Git object." in
    Arg.(required & pos 1 (some string) None & doc)
  in
*)
  let sha1s =
    let doc = Arg.info ~docv:"SHA1" ~doc:"Git object." [] in
    Arg.(non_empty & pos_right 0 string [] & doc)
  in
  let extract
      external_parser disable_parser clearcache cache_dir_base dump_delta
      fact_proj fact_into_virtuoso fact_into_directory fact_enc
      fact_size_thresh local_cache_name
      root sha1s =
    run begin

      let options = 
        get_opts ~verbose:(verbose())
          ~external_parser ~disable_parser ~clearcache ~cache_dir_base ~dump_delta
          ~dump_fact:true ~fact_proj ~fact_size_thresh ~fact_enc
          ~fact_into_virtuoso ~fact_into_directory
          ~local_cache_name
      in
      Lang.setup_options options;

      S.v root >>= function
        | Error err -> eprintf "[ERROR] %s\n" (Fmt.strf "%a" S.pp_error err); Lwt.return_unit
        | Ok t -> begin

        let module GS = Git_storage.F(S) in
        let repo_name = Fpath.to_string root in
        begin
          try
            Lwt_list.iter_s
              (fun sha1 ->
                options#set_fact_versions [|(Entity.V_GITREV, sha1)|];
                GS.make_obj options repo_name t [Hash.of_hex sha1] >>= fun objs -> begin
                  let diffast = new Astcore.c (options :> Parser_options.c) in
                  match objs with
                  | [GS.Tree tree] -> begin
                      diffast#extract_fact_from_dir tree;
                      Lwt.return_unit
                  end
                  | [GS.File src] -> begin
                      Logs.info (fun m -> m "not yet");
                      Lwt.return_unit
                  end
                  | _ -> assert false
                end
              ) sha1s
          with
            exn -> 
	      eprintf "[EXCEPTION] %s\n%s\n"
                (Printexc.to_string exn) (Printexc.get_backtrace());
	      Lwt.return_unit
        end
      end
    end
  in
  Term.(mk extract $
        args#external_parser $ args#disable_parser $
        args#clearcache $ args#cache_dir_base $ args#dump_delta $
        args#fact_proj $ args#fact_into_virtuoso $
        args#fact_into_directory $ args#fact_enc $ args#fact_size_thresh $ 
        args#local_cache_name $
        args#repo $ sha1s)
}

let diffast = {
  name = "diffast";
  doc = "Compare abstract syntax trees node-by-node.";
  man = [];
  term =
  let recurse = mk_flag ["r";"recurse"] "Compare trees recursively." in
  let ignore_unmodified = mk_flag ["ignore-unmodified"] "Ignore unmodified files." in
  let args = new diffast_args in
  let sha1s =
    let doc = Arg.info [] ~docv:"SHA1" ~doc:"Git object." in
    Arg.(non_empty & pos_right 0 string [] & doc)
  in
  let diffast recurse ignore_unmodified
      external_parser disable_parser clearcache cache_dir_base dump_delta
      dump_fact fact_proj fact_into_virtuoso fact_into_directory fact_enc
      fact_size_thresh local_cache_name
      root sha1s =
    run begin

      let options = 
        get_opts ~verbose:(verbose())
          ~external_parser ~disable_parser ~clearcache ~cache_dir_base ~dump_delta
          ~dump_fact ~fact_proj ~fact_size_thresh ~fact_enc
          ~fact_into_virtuoso ~fact_into_directory
          ~local_cache_name
      in

      if ignore_unmodified then
        options#set_ignore_unmodified_flag
      else
        options#clear_ignore_unmodified_flag;

      if recurse then
        options#set_recursive_flag;

      Lang.setup_options options;

      S.v root >>= function
        | Error err -> eprintf "[ERROR] %s\n" (Fmt.strf "%a" S.pp_error err); Lwt.return_unit
        | Ok t -> begin

        (match sha1s with
        | [s0; s1] -> Lwt.return (s0, s1)
        | s0::s1::_ -> begin
            eprintf "[WARNING] only first two SHA1(s) used\n";
            Lwt.return (s0, s1)
        end
        | [s1] -> begin
            S.read_exn t (Hash.of_hex s1) >>= fun v ->
              match v with
              | Value.Commit commit -> begin
                  match Commit.parents commit with
                  | [] -> begin
                      eprintf "[ERROR] specify another SHA1\n";
                      exit 1
                  end
                  | h::_ -> begin
                      let s0 = Hash.to_hex h in
                      Lwt.return (s0, s1)
                  end
              end
              | _ -> begin
                  eprintf "[ERROR] specify another SHA1\n";
                  exit 1
              end
        end
        | [] -> begin
            eprintf "[ERROR] specify SHA1(s)\n";
            exit 1
        end) >>= fun (sha1_0, sha1_1) ->

        options#set_fact_versions [|(Entity.V_GITREV, sha1_0);(Entity.V_GITREV, sha1_1)|];

        let module GS = Git_storage.F(S) in

	(*let dump_obj sha1 =
	  S.read_exn t sha1 >>= fun v -> GS.dump_value sha1 v; Lwt.return_unit
	in*)

        let repo_name = Fpath.to_string root in

	begin
          try
            GS.make_obj options repo_name t [Hash.of_hex sha1_0; Hash.of_hex sha1_1] >>= fun objs -> begin

              eprintf "comparing %s with %s...\n" sha1_0 sha1_1;
              
              match objs with
              | [GS.Tree tree0; GS.Tree tree1] -> begin

                  if recurse then begin
                    let diffast = new Diffastcore.c options in
                    diffast#compare_trees tree0 tree1
                  end
                  else begin
                    let info = Dirtree.compare_trees options tree0 tree1 in
                    eprintf "cache path: %s\n" info.Dirtree.i_cache_path;

                    let nmodified = List.length info.Dirtree.i_modified in
                    if nmodified > 0 then begin
                      printf "[%d modified files]\n" nmodified;
                      List.iter 
                        (fun (f1, f2) ->
                          if f1#path = f2#path then
                            printf "* %s\n" f1#path
                          else
                            printf "* %s --> %s\n" f1#path f2#path;
                          printf "  %s %s\n" (Xhash.to_hex f1#digest) (Xhash.to_hex f2#digest)
                        ) info.Dirtree.i_modified
                    end;
                    let nrenamed = List.length info.Dirtree.i_renamed in
                    if nrenamed > 0 then begin
                      printf "[%d renamed files]\n" nrenamed;
                      List.iter 
                        (fun (f1, f2) ->
                          printf "* %s --> %s\n" f1#path f2#path;
                          printf "  %s %s\n" (Xhash.to_hex f1#digest) (Xhash.to_hex f2#digest)
                        ) info.Dirtree.i_renamed
                    end;
                    let nmoved = List.length info.Dirtree.i_moved in
                    if nmoved > 0 then begin
                      printf "[%d moved files]\n" nmoved;
                      List.iter 
                        (fun (f1, f2) ->
                          printf "* %s --> %s\n" f1#path f2#path;
                          printf "  %s %s\n" (Xhash.to_hex f1#digest) (Xhash.to_hex f2#digest)
                        ) info.Dirtree.i_moved
                    end;
                    let nremoved = List.length info.Dirtree.i_removed in
                    if nremoved > 0 then begin
                      printf "[%d removed files]\n" nremoved;
                      List.iter
                        (fun f ->
                          printf "* %s\n" f#path;
                          printf "  %s\n" (Xhash.to_hex f#digest)
                        ) info.Dirtree.i_removed
                    end;
                    let nadded = List.length info.Dirtree.i_added in
                    if nadded > 0 then begin
                      printf "[%d added files]\n" nadded;
                      List.iter
                        (fun f ->
                          printf "* %s\n" f#path;
                          printf "  %s\n" (Xhash.to_hex f#digest)
                        ) info.Dirtree.i_added
                    end;
                    let ncopied = List.length info.Dirtree.i_copied in
                    if ncopied > 0 then begin
                      printf "[%d copied files]\n" ncopied;
                      List.iter 
                        (fun (f, fs) -> 
                          printf "* %s --> [%s]\n" f#path (String.concat ";" (List.map (fun f -> f#path) fs));
                          printf "  %s %s\n" (Xhash.to_hex f#digest) (Xhash.to_hex (List.hd fs)#digest)
                        ) info.Dirtree.i_copied
                    end;
                    let nglued = List.length info.Dirtree.i_glued in
                    if nglued > 0 then begin
                      printf "[%d glued files]\n" nglued;
                      List.iter 
                        (fun (fs, f) -> 
                          printf "* [%s] --> %s\n" (String.concat ";" (List.map (fun f -> f#path) fs)) f#path;
                          printf "  %s %s\n" (Xhash.to_hex (List.hd fs)#digest) (Xhash.to_hex f#digest)
                        ) info.Dirtree.i_glued
                    end
                  end;
                  Lwt.return_unit
              end
              | [GS.File src0; GS.File src1] -> begin
                  let diffast = new Diffastcore.c options in
                  let cache_path = diffast#get_cache_path2 src0 src1 in
                  let stat_paths = diffast#search_cache_for_stat cache_path in
                  let dstat =
	            if stat_paths <> [] && (not options#clear_cache_flag) then begin
	              diffast#verbose_msg "cache found. skipping...";
                      Stat.File.scan_diff_stat ~max_retry_count:options#max_retry_count
                        stat_paths
                    end
	            else
                      diffast#compare_files ~cache_path src0 src1
                  in
                  Stat.File.dump_diff_stat_ch ~short:true dstat stdout;
                  Lwt.return_unit
              end
              | _ -> begin
                  eprintf "[ERROR] comparing incompatible objects\n";
                  exit 1
              end
            end
          with
            exn -> 
	      eprintf "[EXCEPTION] %s\n%s\n" (Printexc.to_string exn) (Printexc.get_backtrace());
	      Lwt.return_unit
	end

      end
    end
  in
  Term.(mk diffast $ recurse $ ignore_unmodified $
        args#external_parser $ args#disable_parser $
        args#clearcache $ args#cache_dir_base $ args#dump_delta $
        args#dump_fact $ args#fact_proj $ args#fact_into_virtuoso $
        args#fact_into_directory $ args#fact_enc $ args#fact_size_thresh $
        args#local_cache_name $
        args#repo $ sha1s)
}

let catch_ f =
  Lwt.catch f (function
      | Not_found ->
        eprintf "unknown revision or path not in the working tree\n%!";
        exit 1
      | e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1
    )

(* CAT-FILE *)
let cat_file = {
  name = "cat-file";
  doc  = "Provide content or type and size information for repository objects";
  man  = [];
  term =
    let ty_flag = mk_flag ["t"] "Instead of the content, show the object type." in
    let sz_flag = mk_flag ["s"] "Instead of the content, show the object size." in
    let id =
      let doc = Arg.info ~docv:"Hash1" ~doc:"The Hash1 of the repository object." [] in
      Arg.(required & pos 0 (some string) None & doc)
    in
    let cat_file ty_flag sz_flag id =
      run begin
      S.v Fpath.(v (Sys.getcwd ())) >>= function
        | Error err -> eprintf "[ERROR] %s\n" (Fmt.strf "%a" S.pp_error err); Lwt.return_unit
        | Ok t ->

        catch_ (fun () ->
            S.read_exn t (Hash.of_hex id) >>= fun v ->
            let t, c, s = match v with
              | Value.Blob blob ->
                let c = Blob.to_string blob in
                "blob", c, String.length c
              | Value.Commit commit ->
                let c = Fmt.to_to_string Commit.pp commit in
                "commit", c, String.length c
              | Value.Tree tree ->
                let c = Fmt.to_to_string Tree.pp tree in
                "tree", c, String.length c
              | Value.Tag tag ->
                let c = Fmt.to_to_string Tag.pp tag in
                "tag", c, String.length c
            in
            if ty_flag then Printf.printf "%s%!\n" t;
            if sz_flag then Printf.printf "%d%!\n" s;
            if not ty_flag && not sz_flag then Printf.printf "%s%!\n" c;
            Lwt.return_unit)
      end
    in
    Term.(mk cat_file $ ty_flag $ sz_flag $ id)
}

(* LS-REMOTE *)
let ls_remote = {
  name = "ls-remote";
  doc  = "List references in a remote repository.";
  man  = [];
  term =
    let ls remote =
      let module Sync = Sync(S) in
      run begin
      S.v Fpath.(v (Sys.getcwd ())) >>= function
        | Error err -> eprintf "[ERROR] %s\n" (Fmt.strf "%a" S.pp_error err); Lwt.return_unit
        | Ok t ->

        Sync.ls t remote >>= function
          | Error err ->
              eprintf "[ERROR] %s\n" (Fmt.strf "%a" Sync.pp_error err); Lwt.return_unit
          | Ok references ->
        Printf.printf "From %s\n" (Uri.to_string remote.Net.uri);
        let print (h, r, b) =
          Printf.printf "%s        %s (%B)\n" (Hash.to_hex h) (Reference.to_string r) b
        in
        List.iter print references;
        Lwt.return_unit
      end in
    Term.(mk ls $ remote)
}

(* LS-TREE *)
let ls_tree = {
  name = "ls-tree";
  doc  = "List the contents of a tree object.";
  man  = [];
  term =
    let recurse_flag = mk_flag ["r"] "Recurse into sub-trees." in
    let show_tree_flag =
      mk_flag ["t"] "Show tree entries even when going to recurse them."
    in
    let only_tree_flag = mk_flag ["d"] "Show only the named tree entry itself." in
    let oid =
      let doc = Arg.info [] ~docv:"Hash1"
          ~doc:"The Hash1 of the tree."
      in
      Arg.(required & pos 0 (some string) None & doc )
    in
    let get_kind = function
      | `Dir    -> "tree",   true
      | `Commit -> "commit", false
      | _       -> "blob",   false
    in
    let ls recurse show_tree only_tree oid =
      let pp_blob path h =
        printf "blob %s %s\n" (Hash.to_hex h) path;
        Lwt.return_unit
      in
      let pp_tree mode kind path e =
        printf "%s %s %s\t%s\n" mode kind (Hash.to_hex e.Tree.node) path
      in
      let pp_tag path h =
        printf "tag %s %s\n" (Hash.to_hex h) path;
        Lwt.return_unit
      in
      let rec walk t path h =
        S.read_exn t h >>= function
        | Value.Commit c  -> walk t path (Commit.tree c)
        | Value.Blob _    -> pp_blob path h
        | Value.Tag _     -> pp_tag path h
        | Value.Tree tree ->
          Lwt_list.iter_s (fun e ->
              let path = Filename.concat path e.Tree.name in
              let kind, is_dir = get_kind e.Tree.perm in
              let mode = Tree.string_of_perm e.Tree.perm in
              let show =
                if is_dir then not recurse || show_tree || only_tree
                else not only_tree
              in
              if show then pp_tree mode kind path e;
              if is_dir && recurse then walk t path e.Tree.node
              else Lwt.return_unit
            ) (Tree.to_list tree)
      in
      run begin
        S.v Fpath.(v (Sys.getcwd ())) >>= function
          | Error err -> eprintf "[ERROR] %s\n" (Fmt.strf "%a" S.pp_error err); Lwt.return_unit
          | Ok t ->

        let h = Hash.of_hex oid in
        catch_ (fun () -> walk t "" h)
      end in
    Term.(mk ls $ recurse_flag $ show_tree_flag
          $ only_tree_flag $ oid)
}

(* HELP *)
let help = {
  name = "help";
  doc  = "Display help about ogit and ogit commands.";
  man  = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ];
  term =
    let topic =
      let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
      Arg.(value & pos 0 (some string) None & doc )
    in
    let help man_format cmds topic () = match topic with
      | None       -> `Help (`Pager, None)
      | Some topic ->
        let topics = "topics" :: cmds in
        let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e                -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
        | `Ok t                   -> `Help (man_format, Some t) in
    Term.(ret (pure help $Term.man_format $Term.choice_names $topic $setup_log))
}

let default =
  let doc = "Mirage application builder" in
  let man = [
    `S "DESCRIPTION";
    `P "ogit is a small tool to experiement with the pure-OCaml implementation \
        of the Git format and protocol. Very few options are available, and it  \
        is not expected that this number grows very much in a near future.";
    `P "ogit is a prototype, so use it at your own risk (eg. it might corrupt \
        your data if you are particulary unlucky).";
    `P "Use either $(b,ogit <command> --help) or $(b,ogit help <command>) \
        for more information on a specific command.";
  ] @  help_sections
  in
  let usage _ =
    Printf.printf
      "usage: ogit [--version]\n\
      \            [--help]\n\
      \            <command> [<args>]\n\
       \n\
       See 'ogit help <command>' for more information on a specific command.\n%!"
      in
  Term.(pure usage $ setup_log),
  Term.info "ogit"
    ~version:"%%VERSION%%"
    ~sdocs:global_option_section
    ~doc
    ~man

let commands = List.map command [
    cat_file;
    ls_remote;
    ls_tree;
    diffast;
    extract;
    help;
  ]

let () =
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _ -> ()
