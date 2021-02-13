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


let sprintf = Printf.sprintf

type key = K_any | K_has_param of string * string
type path_elem = PE_sect of string * key | PE_param of string

let key_to_string = function
  | K_any -> "*"
  | K_has_param(n,v) -> sprintf "%s=%s" n v

let path_elem_to_string = function
  | PE_sect(s, k) -> sprintf "<%s,%s>" s (key_to_string k)
  | PE_param s -> s

let path_to_string p = String.concat "" (List.map path_elem_to_string p)

let search_config path tree =
  let conv v =
    match v with
    | `Bool b -> string_of_bool b
    | `Float f -> string_of_float f
    | `Int i -> string_of_int i
    | `String s -> s
  in
  let chk tree_list = function
    | K_any -> true
    | K_has_param(name, value) ->
	List.exists
	  (fun tree ->
	    match tree with
	    | `Parameter(n, v) ->
		n = name && (conv v) = value
	    | _ -> false
	  ) tree_list
  in
  let rec search acc path =
(*
    printf "search: |acc|=%d path=%s\n" (List.length acc) (path_to_string path);
*)
    match path with
    | PE_sect(name, key)::path_rest -> begin
	search
	  (List.fold_left
	     (fun l x ->
	       match x with
	       | `Section(n, tree_rest) ->
		   if n = name then
		     if chk tree_rest key then
		       tree_rest @ l
		     else
		       l
		   else
		     l
	       | _ -> l
	     ) [] acc)
	  path_rest
    end
    | [PE_param name] ->
	List.fold_left
	  (fun l x ->
	    match x with
	    | `Parameter(n, v) ->
		if n = name then
		  (conv v) :: l
		else
		  l
	    | _ -> l
	  ) [] acc
    | _ ->
	[]
  in
  search [tree] path


let search_config_for_docroot tree =
  let l =
    search_config [ PE_sect("netplex",K_any);
		    PE_sect("service",K_has_param("name","nethttpd"));
		    PE_sect("processor",K_has_param("type","nethttpd"));
		    PE_sect("host",K_has_param("pref_name","localhost"));
		    PE_sect("uri",K_has_param("path","/"));
		    PE_sect("service",K_any);
		    PE_param "docroot";
		  ]
      tree
  in
  match l with
  | x::_ -> x
  | [] -> raise Not_found


let start () =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let use_mt = ref false in

  let opt_list' =
    [ "-mt", Arg.Set use_mt,
      "  Use multi-threading instead of multi-processing";

      "-debug", Arg.String (fun s -> Netlog.Debug.enable_module s),
      "<module>  Enable debug messages for <module>";

      "-debug-all", Arg.Unit (fun () -> Netlog.Debug.enable_all()),
      "  Enable all debug messages";

      "-debug-list", Arg.Unit (fun () ->
                                 List.iter print_endline (Netlog.Debug.names());
                                 exit 0),
      "  Show possible modules for -debug, then exit";

      "-debug-win32", Arg.Unit (fun () ->
                                  Netsys_win32.Debug.debug_c_wrapper true),
      "  Special debug log of Win32 wrapper"

    ] @ opt_list
  in
  Arg.parse
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    (Printf.sprintf "usage: %s [options]" Xprint.cmd_name);

  let parallelizer =
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp ~keep_fd_open:true () (* multi-processing *)
  in
  let diffast =
    { Nethttpd_services.dyn_handler = Diffast_ws.handler;
      dyn_activation                = Diffast_ws.activation;
      dyn_uri                       = None;          (* not needed *)
      dyn_translator                = (fun _ -> ""); (* not needed *)
      dyn_accept_all_conditionals   = false;
    }
  in
  let nethttpd_factory =
    Nethttpd_plex.nethttpd_factory
      ~config_cgi:Diffast_ws.config
      ~handlers:[ "diffast", diffast ]
      ()
  in
  begin
    match Netplex_main.config_filename_opt cmdline_cfg with
    | None -> ()
    | Some fn ->
	let cfgtree = (Netplex_config.read_config_file fn)#tree in
	begin
	  try
	    let d = search_config_for_docroot cfgtree in
	    Diffast_ws.conf#set_root_dir d
	  with
	    Not_found -> ()
	end
  end;
  Diffast_ws.conf#show;
  Diffast_ws.init_nprocs_file();
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories                (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory ]                        (* make this nethttpd available *)
    cmdline_cfg


let _ =
  Netsys_signal.init();
  start()
