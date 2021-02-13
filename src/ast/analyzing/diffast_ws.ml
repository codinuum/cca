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


class configuration = object
  val mutable max_file_size = 256.0 (* KB *)
  val mutable max_nprocs = 2
  val mutable workspace_timeout = 60.0 (* in sec. *)
  val mutable root_dir = Filename.concat Filename.temp_dir_name "WS"
  val mutable workspace_db_file = Filename.concat Filename.temp_dir_name "diffast.wsdb"
  val mutable nprocs_file = Filename.concat Filename.temp_dir_name "diffast.nprocs"

  method max_file_size = max_file_size
  method max_nprocs = max_nprocs
  method max_response_body_length = Int64.mul (Int64.of_float max_file_size) 1024L
  method workspace_timeout = workspace_timeout
  method root_dir = root_dir
  method workspace_db_file = workspace_db_file
  method nprocs_file = nprocs_file

  method set_max_file_size sz = max_file_size <- sz
  method set_max_nprocs n = max_nprocs < n
  method set_workspace_timeout t = workspace_timeout <- t
  method set_root_dir d = root_dir <- d
  method set_workspace_db_file f = workspace_db_file <- f
  method set_nprocs_file f = nprocs_file <- f

  method show =
    Netlog.logf `Info "max file size: %.0f (KB)" max_file_size;
    Netlog.logf `Info "max nprocs: %d" max_nprocs;
    Netlog.logf `Info "workspace timeout: %.0f (s)" workspace_timeout;
    Netlog.logf `Info "root directory: %s" root_dir;
    Netlog.logf `Info "workspace DB file: %s" workspace_db_file;
    Netlog.logf `Info "nprocs file: %s" nprocs_file

end

let conf = new configuration

(**********************************************************************)

open Netcgi
open Nethttp_client
open Neturl
open Printf

exception Comparison_error of string



(* DB MANAGEMENT *)

let db_init db =
  let rc =
    Sqlite3.exec db "CREATE TABLE WS (ws_creation float, ws_name varchar)"
  in
  if rc <> Sqlite3.Rc.OK then begin
    let msg = Sqlite3.errmsg db in
    Netlog.logf `Err "db_init: table creation failed: %s" msg
  end

let db_exec db ?(cb=fun _ _ -> ()) stmt =
  let rc = Sqlite3.exec db ~cb stmt in
  if rc <> Sqlite3.Rc.OK then begin
    let msg = Sqlite3.errmsg db in
    if Xstring.startswith msg "no such table" then begin
      db_init db;
      let rc = Sqlite3.exec db ~cb stmt in
      if rc <> Sqlite3.Rc.OK then begin
	let msg = Sqlite3.errmsg db in
	Netlog.logf `Err "db_exec: query failed: %s: %s" msg stmt
      end
    end
    else
      Netlog.logf `Err "db_exec: query failed: %s: %s" msg stmt
  end

let db_close db =
  let b = Sqlite3.db_close db in
  if not b then
    Netlog.log `Err "db_close: cannot close DB"


(* WORKSPACE MANAGEMENT *)

let get_expired_workspaces() =
  if Sys.file_exists conf#workspace_db_file then
    let expired = ref [] in
    begin
      try
	let t_valid = Unix.time() -. conf#workspace_timeout in
	let db = Sqlite3.db_open ~mutex:(`FULL) conf#workspace_db_file in
	let chk row _ =
	  match row.(0) with
	  | Some s -> expired := s :: !expired
	  | None -> ()
	in
	let stmt = sprintf "SELECT ws_name FROM WS WHERE ws_creation < %f" t_valid in
	db_exec db ~cb:chk stmt;
	db_close db
      with
      | Sqlite3.Error msg ->
	  Netlog.logf `Err "get_expired_workspaces: DB error: %s" msg
      | _ ->
	  Netlog.log `Err "get_expired_workspaces: DB error"
    end;
    !expired
  else
    []

let unregister_workspace name =
  try
    let db = Sqlite3.db_open ~mutex:(`FULL) conf#workspace_db_file in
    let stmt = sprintf "DELETE FROM WS WHERE ws_name='%s'" name in
    db_exec db stmt;
    db_close db
  with
  | Sqlite3.Error msg ->
      Netlog.logf `Err "unregister_workspace: DB error: %s" msg
  | _ ->
      Netlog.log `Err "unregister_workspace: DB error"


let register_workspace name =
  let now = Unix.gettimeofday() in
  try
    let db = Sqlite3.db_open ~mutex:(`FULL) conf#workspace_db_file in
    let stmt = sprintf "INSERT INTO WS VALUES (%.0f,'%s')" now name in

    db_exec db stmt;

    db_close db
  with
  | Sqlite3.Error msg ->
      Netlog.logf `Err "register_workspace: DB error: %s" msg
  | _ ->
      Netlog.log `Err "register_workspace: DB error"


let check_workspaces() =
  let expired = get_expired_workspaces() in
  List.iter
    (fun wsn ->
      Netlog.logf `Info "check_workspaces: removing \"%s\"..." wsn;
      try
	Xfile.rmdir (Filename.concat conf#root_dir wsn);
	unregister_workspace wsn
      with
      | Xfile.Error msg ->
	  Netlog.logf `Err "check_workspaces: %s" msg
    ) expired


(* COMPARISON PROCESS MANAGEMENT *)

let init_nprocs_file() =
(*
  Netlog.logf `Info "init_nprocs_file: \"%s\"" conf#nprocs_file;
*)
  try
    let ch = open_out conf#nprocs_file in
    output_string ch "0\n";
    close_out ch
  with
  | Sys_error msg ->
      Netlog.logf `Err "init_nprocs_file: %s" msg;
      exit 1


let nprocs_read fd =
  let ch = Unix.in_channel_of_descr fd in
  let line = input_line ch in
  try
    let n = int_of_string line in
    n
  with
  | Failure _ ->
      Netlog.logf `Err
	"nprocs_read: malformed nprocs file: \"%s\" (length:%d)"
	(String.escaped line) (String.length line);
      -1

let nprocs_write fd n =
  let s = (string_of_int n)^"\n" in
  let len = String.length s in
  Unix.ftruncate fd 0;
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let r = Unix.write fd s 0 len in
  if r <> len then
    Netlog.logf `Err "nprocs_write: write failed: only %d characters written" r



let comparison_proc_begin() =
  let fdop = ref None in
  try
    let fd = Unix.openfile conf#nprocs_file [Unix.O_RDWR] 0o640 in
    fdop := Some fd;
    Unix.lockf fd Unix.F_LOCK 0;
    let n = nprocs_read fd in

    Netlog.logf `Info "comparison_proc_begin: current nprocs: %d" n;

    if n < 0 then begin
      Netlog.logf `Err "comparison_proc_begin: malformed nprocs file: nprocs=%d" n;
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd;
    end
    else if n >= conf#max_nprocs then begin
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd;
      raise (Comparison_error "Too many comparisons")
    end
    else begin
      nprocs_write fd (n + 1);
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd
    end
  with
  | Comparison_error msg -> raise (Comparison_error msg)
  | e ->
      let msg = Printexc.to_string e in
      Netlog.logf `Err "comparison_proc_begin: %s" msg;
      match !fdop with
      | Some fd -> Unix.close fd
      | None -> ()

let comparison_proc_end() =
  let fdop = ref None in
  try
    let fd = Unix.openfile conf#nprocs_file [Unix.O_RDWR] 0o640 in
    fdop := Some fd;
    Unix.lockf fd Unix.F_LOCK 0;
    let n = nprocs_read fd in
    if n <= 0 then begin
      Netlog.logf `Err "comparison_proc_end: malformed nprocs file: nprocs=%d" n;
      nprocs_write fd 0;
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd
    end
    else begin
      nprocs_write fd (n - 1);
      Unix.lockf fd Unix.F_ULOCK 0;
      Unix.close fd
    end
  with
  | e ->
      let msg = Printexc.to_string e in
      Netlog.logf `Err "comparison_proc_end: %s" msg;
      match !fdop with
      | Some fd -> Unix.close fd
      | None -> ()




(**********************************************************************
 * HTML GENERATORS
 *
 * Some functions helping to generate HTML.
 **********************************************************************)

let text = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ()
  (* This function encodes "<", ">", "&", double quotes, and Latin 1
     characters as character entities. E.g. text "<" = "&lt;", and
     text "ä" = "&auml;" *)


(* Encodes [x] such that it can be safely used inside the *names*
   parameters of HTML forms.  We have to be careful because there
   might be bugs in browsers.

   The chosen encoding: Alphanumeric characters are represented by
   themselves.  The other characters are represented by an underscore
   followed by the hexadecimal encoding.
*)
let encode_filename x =
  let q = Netencoding.Q.encode x in      (* Uses '=' instead of '_' *)
  Pcre.qreplace ~pat:"=" ~templ:"_" q

let decode_filename x =
  (* The reverse function to [encode_filename] *)
  let q = Pcre.qreplace ~pat:"_" ~templ:"=" x in
  Netencoding.Q.decode q


let begin_page (cgi : cgi) title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi#out_channel#output_string in
  out "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \
	\"http://www.w3.org/TR/html4/strict.dtd\">\n";
  out "<html>\n";
  out "<head>\n";
  out ("<title>" ^ text title ^ "</title>\n");
  out ("<link rel=\"stylesheet\" type=\"text/css\" href=\"diffast_ws.css\" />\n");
  out "</head>\n";
  out "<body>\n";
  out ("<h1>" ^ text title ^ "</h1>\n")

let end_page (cgi : cgi) =
  cgi#out_channel#output_string "</body>\n</html>\n"


let begin_form ?(enctype = "application/x-www-form-urlencoded") (cgi : cgi) =
  cgi#out_channel#output_string
    (sprintf "<form method=POST action=\"%s\" enctype=\"%s\">\n"
	 (text(cgi#url()))
	 enctype)

let end_form (cgi : cgi) =
  cgi#out_channel#output_string "</form>\n"

let hidden_field (cgi : cgi) name value =
  cgi#out_channel#output_string
    (sprintf "<input type=hidden name=\"%s\" value=\"%s\">\n"
       (text name)
       (text value))

let restart_button (cgi : cgi) =
  begin_form cgi;
  cgi#out_channel#output_string "<input type=\"submit\" name=\"RESTART\" value=\"Restart\">";
  end_form cgi

(* Check whether there is a CGI argument whose name is passed in the
   list [possible_buttons].  If so, the name of the argument is
   returned.  If not, "no_button" is returned.  *)
let pressed_button (cgi:cgi) possible_buttons =
  let is_button arg = List.mem arg#name possible_buttons in
  try
    (List.find is_button cgi#arguments)#name
  with
    Not_found -> "no_button"


(* HTTP Downloader *)
let download url toFile =
  let pipeline = new pipeline in
  let get_call = new get url in
  get_call#set_max_response_body_length conf#max_response_body_length;
  get_call#set_response_body_storage (`File (fun () -> toFile));
  pipeline#add get_call;
  try
    pipeline#run()
  with
  | Nethttp_client.Response_too_large ->
      raise (Comparison_error (sprintf "File too large: \"%s\"" url))
  | e ->
      Netlog.logf `Warning "download: %s" (Printexc.to_string e);
      raise (Comparison_error (sprintf "Failed to download \"%s\"" url))


(* *)
let diffast options (cgi:cgi) wdir url0 url1 file0 file1 =
(*
  Netlog.logf `Info "wdir=\"%s\" file0=\"%s\" file1=\"%s\"" wdir file0 file1;
*)
  let wdir_name = Filename.basename wdir in
  let filename0 = Filename.basename file0 in
  let filename1 = Filename.basename file1 in

  options#set_cache_dir_base wdir;

  Lang.setup_options options;

  let diffast_engine = new Astcore.c options in

  comparison_proc_begin();
  begin
    try
      ignore (diffast_engine#compare_files
                (Fs.file_of_path options file0)
                (Fs.file_of_path options file1))
    with
      e ->
	Netlog.log `Err (Printexc.to_string e);
	raise e
  end;
  comparison_proc_end();

  let out = cgi#out_channel#output_string in
  begin_page cgi "Diff/AST DiffViewer";

  out "<center>\n";

  out (sprintf "<p>%s - %s</p>\n" url0 url1);
  out "<object class=\"diffviewer\" type=\"application/x-java-applet\" width=\"100%\" height=\"480\">\n";
  out "<param name=\"codebase\" value=\".\" />\n";
  out "<param name=\"archive\" value=\"diffviewer.jar\" />\n";
  out "<param name=\"code\" value=\"usx.diffviewer.DiffViewerApplet\" />\n";
  out (sprintf "<param name=\"diffs\" value=\"/%s\" />\n" wdir_name);
  out (sprintf "<param name=\"file0\" value=\"/%s/%s\" />\n" wdir_name filename0);
  out (sprintf "<param name=\"file1\" value=\"/%s/%s\" />\n" wdir_name filename1);

  out "<applet class=\"diffviewer\" code=\"usx.diffviewer.DiffViewerApplet\" codebase=\".\" archive=\"diffviewer.jar\" width=\"100%\" height=\"480\">\n";
  out (sprintf "<param name=\"diffs\" value=\"/%s\" />\n" wdir_name);
  out (sprintf "<param name=\"file0\" value=\"/%s/%s\" />\n" wdir_name filename0);
  out (sprintf "<param name=\"file1\" value=\"/%s/%s\" />\n" wdir_name filename1);
  out "Applet failed to run.  No Java plug-in found.";
  out "</applet>\n";

  out "</object>\n";

  out "</center>\n";

  restart_button cgi;
(*  out (sprintf "<a href=\"%s\">Restart</a>\n" (text (cgi#url()))); *)
  end_page cgi



let getTargetFileName url_str =
  try
    let url = parse_url url_str in
    Xlist.last (url_path url)
  with
  | Malformed_URL ->
      raise (Comparison_error (sprintf "Malformed URL: \"%s\"" url_str))


let display_comparison_page (cgi:cgi) =
  let srcURL0 = cgi#argument_value "srcURL0" in
  let srcURL1 = cgi#argument_value "srcURL1" in

  if srcURL0 <> "" && srcURL1 <> "" then begin
    let wdir = Xfile.make_temp_dir ~temp_dir:conf#root_dir "" "" in

    let toFile0 = Filename.concat wdir (getTargetFileName srcURL0) in
    let toFile1 = Filename.concat wdir (getTargetFileName srcURL1) in

    let ext0 = Xfile.get_extension toFile0 in
    let ext1 = Xfile.get_extension toFile1 in
    if ext0 <> ext1 then
      raise (Comparison_error "Different extensions");

    download srcURL0 toFile0;
    download srcURL1 toFile1;

    (* Compute and show the result *)
    let options = new Options.c in
    diffast options cgi wdir srcURL0 srcURL1 toFile0 toFile1;

    register_workspace (Filename.basename wdir);

  end
  else
    let out = cgi#out_channel#output_string in
    begin_page cgi "Diff/AST";
    begin_form cgi;
    out "<fieldset class=\"comparison\">\n";
    out "<legend>Please enter URLs of source files to be compared</legend>\n";
    out "<table class=\"comparison\" cellspacing=\"0\">\n";
    out "<tbody>\n";
    out "<tr>\n";
    out (sprintf "<td class=\"comparison\"><label for=\"src0\">Source File URL0:</label></td><td class=\"comparison\"><input type=text size=\"48\" maxlength=\"128\" name=\"srcURL0\" id=\"src0\" value=\"%s\"></td>\n" srcURL0);
    out "</tr>\n<tr>\n";
    out (sprintf "<td class=\"comparison\"><label for=\"src1\">Source File URL1:</label></td><td class=\"comparison\"><input type=text size=\"48\" maxlength=\"128\" name=\"srcURL1\" id=\"src1\" value=\"%s\"></td>\n" srcURL1);
    out "</tr>\n<tr>\n";
    out "<td class=\"comparison\">&nbsp;</td>\n";
    out "<td class=\"comparison\"><input type=submit name=\"COMPARE\" value=\"Compare\"></td>\n";
    out "</tr>\n";
    out "</tbody>\n";
    out "</table>\n";
    out "</fieldset>\n";
    end_form cgi;
    end_page cgi



(**********************************************************************
 * REQUEST BROKER
 *
 * This function calls the various page-specific handlers.
 **********************************************************************)

let handler _ (cgi:cgi) =
  try
    check_workspaces();

    cgi#set_header
      ~cache:`No_cache
      ~content_type: "text/html; charset=iso-8859-1"
      ();

    display_comparison_page cgi;

    (* Commit everything (if channel not closed already): *)
    (try cgi#out_channel#commit_work() with _ -> ());
    cgi#finalize() (* Cleanup *)
  with
  | Comparison_error s ->
      (* An error has happened.  Generate now an error page instead of
         the current page.  By rolling back the output buffer, any
         uncomitted material is deleted.  *)
      cgi#out_channel#rollback_work();
      let out = cgi#out_channel#output_string in
      begin_page cgi "Error";
      out "<p class=\"error\">";
      out (sprintf "%s.\n" s);
      out "</p>\n";
(*      out (sprintf "<a href=\"%s\">Restart</a>\n" (text (cgi#url()))); *)
      restart_button cgi;
      end_page cgi;
      cgi#out_channel#commit_work();	(* Commit the error page *)
      cgi#finalize()

  | Xfile.No_extension s ->
      cgi#out_channel#rollback_work();
      let out = cgi#out_channel#output_string in
      begin_page cgi "Error";
      out "<p class=\"error\">";
      out (sprintf "no extension: \"%s\"" s);
      out "</p>\n";
      out (sprintf "<a href=\"%s\">Restart</a>\n" (text (cgi#url())));
      end_page cgi;
      cgi#out_channel#commit_work();	(* Commit the error page *)
      cgi#finalize()


let activation =
  let arg_store _ name _ =
    if name = "srcURL0" || name = "srcURL1" then
      `File_max conf#max_file_size
    else
      `Memory
  in
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  let activation_opt =
    { Nethttpd_services.stdactv_processing = Some arg_store;
      Nethttpd_services.stdactv_operating_type               = Some (`Transactional buffered);
    }
  in
  Nethttpd_services.std_activation (`Std_activation activation_opt)

let config = default_config
