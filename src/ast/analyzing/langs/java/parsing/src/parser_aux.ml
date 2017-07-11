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
(* parser_aux.ml *)

open Printf
open Common
open Ast

open Compat

module P = Printer

exception Attrs_found of identifier_attribute list
exception Unknown of string
exception Str_found of string



exception Frame_found of frame

class env = object (self)
  inherit [Source_base.c] Env_base.c as super

  val mutable java_lang_spec = JLSx

  val mutable current_package_name = ""

  val classtbl = new Classinfo.classtbl_c

  val stack = Stack.create()

  val mutable last_rawtoken = Obj.repr ()

  val mutable global_frame = new frame FKother

  method get_global_frame = global_frame

  method last_rawtoken = last_rawtoken
  method set_last_rawtoken o = last_rawtoken <- o

  method classtbl = classtbl

  method java_lang_spec = java_lang_spec
  method is_java_lang_spec_unknown = java_lang_spec = JLSx
  method is_java_lang_spec_JLS3    = java_lang_spec = JLS3
  method is_java_lang_spec_JLS2    = java_lang_spec = JLS2
  method set_java_lang_spec_unknown = java_lang_spec <- JLSx
  method set_java_lang_spec_JLS3    = java_lang_spec <- JLS3
  method set_java_lang_spec_JLS2    = java_lang_spec <- JLS2


  method set_current_package_name n = 
    current_package_name <- n

  method current_package_name = current_package_name

  method begin_scope ?(kind=FKother) ?(frame_opt=None) () =
    DEBUG_MSG "PUSH(%d)" (Stack.length stack);
    let frame =
      match frame_opt with
      | None -> new frame kind
      | Some f -> f
    in
    Stack.push frame stack

  method end_scope() =
    DEBUG_MSG "POP(%d)" (Stack.length stack);
    try
      ignore (Stack.pop stack)
    with 
      Stack.Empty ->
	raise (Internal_error "Parser_aux.end_scope: stack empty")

  method current_frame =
    try
      Stack.top stack
    with 
      Stack.Empty ->
	raise (Internal_error "Parser_aux.current_frame: stack empty")

  method copy_current_stack =
    Stack.copy stack

  method private get_frame attr =
    if attr <> IAtypeparameter && self#current_frame#is_typeparameter_frame then begin
      try
        Stack.iter
          (fun f ->
            if not f#is_typeparameter_frame then
              raise (Frame_found f)
          ) stack;
        assert false
      with
        Frame_found f -> f
    end
    else
      self#current_frame


  method register_identifier ident attr =
    DEBUG_MSG "REGISTER(%d): \"%s\" -> %s" 
      (Stack.length stack) ident (iattr_to_str attr);

    let frame = self#get_frame attr in
    frame#add ident attr 


  method lookup_identifier ?(afilt=(fun _ -> true)) ident =
    DEBUG_MSG "LOOKUP(%d): \"%s\"" (Stack.length stack) ident;
    try
      Stack.iter
	(fun frame ->
          (*DEBUG_MSG "%s" frame#to_string;*)
	  try
	    let attrs = frame#find_all ident in
	    let filtered = List.filter afilt attrs in
	    if filtered <> [] then
	      raise (Attrs_found filtered)
	  with 
	    Not_found -> ()
	) stack;
      raise Not_found
    with 
      Attrs_found attrs -> 
	if attrs = [] then
	  raise Not_found
	else begin
          DEBUG_MSG "FOUND: %s -> [%s]" ident (Xlist.to_string iattr_to_str ";" attrs);
	  attrs
        end


  method register_qname qname attr =
    if is_qualified_qname qname then begin
      DEBUG_MSG "REGISTER(%d): \"%s\" -> %s"
        (Stack.length stack) qname (iattr_to_str attr);

      let frame = self#get_frame attr in
      frame#qadd qname attr
    end

  method register_global_qname qname attr =
    if is_qualified_qname qname then begin
      DEBUG_MSG "REGISTER(%d): \"%s\" -> %s"
        (Stack.length stack) qname (iattr_to_str attr);

      let frame = self#get_global_frame in
      frame#qadd qname attr
    end


  method lookup_qname ?(afilt=(fun _ -> true)) qname =
    DEBUG_MSG "LOOKUP(%d): \"%s\"" (Stack.length stack) qname;
    try
      Stack.iter
	(fun frame ->
          (*DEBUG_MSG "%s" frame#to_string;*)
	  try
	    let attrs = frame#qfind_all qname in
	    let filtered = List.filter afilt attrs in
	    if filtered <> [] then
	      raise (Attrs_found filtered)
	  with
	    Not_found -> ()
	) stack;
      raise Not_found
    with
      Attrs_found attrs ->
        DEBUG_MSG "FOUND: %s -> [%s]" qname (Xlist.to_string iattr_to_str ";" attrs);
	attrs

  method lookup_global_qname qname =
    let attrs = global_frame#qfind_all qname in
    DEBUG_MSG "FOUND: %s -> [%s]" qname (Xlist.to_string iattr_to_str ";" attrs);
    attrs

  method get_naive_fqn name =
    let qn = P.name_to_simple_string name in
    try
      let _ = classtbl#get_package_name qn in
      qn
    with
      _ ->
        let _, id = leftmost_of_name name in
        let fqn =
          try
            let lfqn = classtbl#resolve id in
            let prefix, _ = decompose_qname lfqn in
            prefix^"."^qn
          with
            _ -> qn
        in
        fqn


  method inner_most_class =
    let s = Stack.copy stack in
    let _ = Stack.pop s in
    try
      Stack.iter
	(fun frame ->
	  frame#iter
	    (fun id a ->
	      match a with
	      | IAclass s -> raise (Attrs_found [a])
	      | _ -> ()
	    )
	) s;
      raise Not_found
    with
      Attrs_found [a] -> a


  method resolve name =
    DEBUG_MSG "resolving \"%s\"" (P.name_to_simple_string name);
    let ss = P.name_to_simple_string name in
    let res =
      if is_simple name then begin
	try
	  let afilt = function 
	    | IAclass _ | IAinterface _ | IAtypename _ 
            | IAtypeparameter 
              -> true
	    | _ -> false
	  in
	  let attrs = self#lookup_identifier ~afilt ss in
	  let res =
	    match List.nth attrs 0 with
	    | IAclass s | IAinterface s | IAtypename s -> s
            | IAtypeparameter -> rightmost_identifier name
	    | _ -> assert false
	  in
	  R_resolved res
	with
	  Not_found ->
	    let frms = self#copy_current_stack in
	    try
	      let res = classtbl#resolve ss in
	      R_deferred(ss, frms, res)
	    with 
	      Not_found -> R_deferred(ss, frms, "")
      end
      else begin (* qualified name: unresolved inner class or FQN *)
	let qname = ss in
	try
	  let res = classtbl#resolve_qualified_type_name qname in
	  DEBUG_MSG "resolved: %s --> %s" qname res;
	  R_resolved res
	with 
	  Not_found ->
	    let s = Bytes.copy ss in
	    Xstring.replace s '.' '$';
	    let frms = self#copy_current_stack in
	    try
	      let res = classtbl#resolve s in
	      R_deferred(s, frms, res)
	    with 
	      Not_found -> R_deferred(ss, frms, "")
      end
    in
    DEBUG_MSG "\"%s\" -> \"%s\"" ss (resolve_result_to_str res);
    res
  (* method resolve *)

  method finalize_name_attribute nattr_ref =
    try
      let a =
        match !nattr_ref with
        | NAtype rr ->
	    let ln = self#finalize_resolve_result rr in
	    DEBUG_MSG "TYPE(%s)" ln;
            NAtype (R_resolved ln)
        | _ -> raise Not_found
      in
      nattr_ref := a
    with
      Not_found -> ()

  method finalize_resolve_result = function
    | R_resolved s -> s
    | R_deferred(id, frames, cand) ->
        DEBUG_MSG "id=\"%s\" cand=\"%s\"" id cand;
	let _resolve_lname lname =
	  DEBUG_MSG "\"%s\"" lname;
	  try
	    Stack.iter
	      (fun frame ->
		try
		  match frame#find lname with
		  | IAclass s | IAinterface s | IAtypename s -> 
                      raise (Str_found s)
		  | _ -> ()
		with 
		  Not_found -> ()
	      ) frames;
	    raise Not_found
	  with 
	  | Str_found s -> s
	  | Not_found ->
	      if cand <> "" then
		cand
	      else
		classtbl#resolve lname
	in
	let resolve_lname lname =
	  try
	    _resolve_lname lname
	  with
	    Not_found ->
	      if is_inner lname then begin
		let parent, rest = split_inner lname in
		DEBUG_MSG "parent=\"%s\", rest=\"%s\"" parent rest;
		(_resolve_lname parent)^rest
	      end
	      else
		raise Not_found
	in
	try
	  if cand = "" && String.contains id '.' then begin
	    try
	      classtbl#resolve_qualified_type_name id
	    with
	    | Not_found ->
		let s = Bytes.copy id in
		Xstring.replace s '.' '$';
		resolve_lname s
	  end
	  else
	    resolve_lname id
	with 
	  Not_found ->
            (*warning_msg "name not resolved: %s" id;*)
            (*sprintf "/*[unresolved]*/%s" id*)
            id
  (* method finalize_resolve_result *)

  method reclassify_identifier (attr, ident) =
    DEBUG_MSG "attr=%s ident=%s" (P.name_attribute_to_string !attr) ident;
    let check2() =
      DEBUG_MSG "checking...";
      try (* not yet (only class names are resolved) *)
	let fqn = classtbl#resolve ident in
	NAtype (R_deferred(ident, self#copy_current_stack, fqn))
      with 
	Not_found -> 
	  (* lookup_name ident *)
	  NAunknown (* NApackage *) 
    in
    let check1() =
      DEBUG_MSG "checking...";
      try
	let afilt = function
	  | IAvariable | IAparameter | IAfield
	  | IAclass _ | IAinterface _ | IAtypename _ 
          | IAtypeparameter
            -> true
	  | _ -> false
	in
	let attrs = self#lookup_identifier ~afilt ident in
	match List.nth attrs 0 with
	| IAvariable | IAparameter | IAfield -> NAexpression

	| IAclass s | IAinterface s | IAtypename s -> begin
	    NAtype (R_deferred(ident, self#copy_current_stack, s))
        end
        | IAtypeparameter -> NAtype (R_resolved ident)
	| _ -> assert false
      with 
	Not_found -> check2()
    in
    attr := check1()

  method get_package_name name = (* package or normal type or nested type *)
    let qname = P.name_to_simple_string name in
    DEBUG_MSG "name=%s" (P.name_to_string name);

    if classtbl#is_package qname then begin
      set_name_attribute NApackage name;
      DEBUG_MSG "pname=%s" (P.name_to_string name);
      qname
    end
    else begin
      try
        let prefix_name, base = decompose_name name in
        let prefix = P.name_to_simple_string prefix_name in

        let spath = self#classtbl#get_source_dir#path in
        let ppath = Filename.concat spath (pkg_to_path prefix) in

	let _path = Filename.concat ppath base in
          
	if self#current_source#tree#is_dir ppath then begin

          if not (classtbl#is_package prefix) then begin
            classtbl#add_package ~dir:(self#current_source#tree#get_entry ppath) prefix;
          end;
          set_name_attribute NApackage prefix_name;

          if self#current_source#tree#is_dir _path then begin
            classtbl#add_package ~dir:(self#current_source#tree#get_entry _path) qname;
            set_name_attribute NApackage name;
            DEBUG_MSG "pname=%s" (P.name_to_string name);
            qname
          end
          else begin
            DEBUG_MSG "pname=%s" (P.name_to_string prefix_name);
            prefix
          end

        end
        else if classtbl#is_package prefix then begin
          set_name_attribute NApackage prefix_name;
          DEBUG_MSG "pname=%s" (P.name_to_string prefix_name);
          prefix
        end
        else
          classtbl#get_package_name prefix

      with
        _ -> failwith "Parser_aux.env#get_package_name"
    end

  method resolve_qualified_type_name name =
    let qname = P.name_to_simple_string name in
    classtbl#_resolve_qualified_type_name (self#get_package_name name) qname

  method init =
    global_frame <- new frame FKother;
    super#init;
    Stack.clear stack;
    self#begin_scope ~frame_opt:(Some global_frame) ();
    self#set_java_lang_spec_unknown

  initializer
    self#init

end (* of class Parser_aux.env *)




module type STATE_T = sig
  val env      : env
end


module F (Stat : STATE_T) = struct

  open Stat

  let get_range start_offset end_offset =
    let pos_mgr = env#current_pos_mgr in
    let start_line, start_char = pos_mgr#get_position start_offset in
    let end_line, end_char = pos_mgr#get_position end_offset in
(*
    DEBUG_MSG "%d:%d-%d:%d(%d-%d)" 
      start_line start_char end_line end_char start_offset end_offset;
*)
    (start_line, start_char), (end_line, end_char), start_offset, end_offset

  let get_loc start_offset end_offset =
    let (sl, sc), (el, ec), so, eo = get_range start_offset end_offset in
    let pos_mgr = env#current_pos_mgr in
    let loc = Loc.make ~fname:pos_mgr#filename so eo sl sc el ec in
    loc

  let parse_error start_offset end_offset msg = 
    let (sl, sc), (el, ec), so, eo = get_range start_offset end_offset in
    let line, char = env#current_pos_mgr#get_position (eo + 1) in
    let head = sprintf "[%d:%d]" line char in
    fail_to_parse ~head msg

  let parse_error_loc loc msg = 
    let head = sprintf "[%s]" (Loc.to_string loc) in
    fail_to_parse ~head msg

  let parse_warning start_ofs end_ofs =
    let loc = get_loc start_ofs end_ofs in
    warning_loc loc

  let get_loc_for_lex lexbuf =
    let pos_mgr = env#current_pos_mgr in
    let get_range lexbuf =
      let start_offset = Ulexing.lexeme_start lexbuf in
      let end_offset = (Ulexing.lexeme_end lexbuf) - 1 in
      let start_line, start_char = pos_mgr#get_position start_offset in
      let end_line, end_char = pos_mgr#get_position end_offset in
      (start_line, start_char), (end_line, end_char), start_offset, end_offset
    in
    let (sl, sc), (el, ec), so, eo = get_range lexbuf in
    let loc = Loc.make so eo sl sc el ec in
    loc


  let register_identifier_as_class fqn id     = env#register_identifier id (IAclass fqn)
  let register_identifier_as_interface fqn id = env#register_identifier id (IAinterface fqn)
  let register_identifier_as_typename fqn id  = env#register_identifier id (IAtypename fqn)
  let register_identifier_as_method id        = env#register_identifier id IAmethod
  let register_identifier_as_static_member id = env#register_identifier id IAstatic
  let register_identifier_as_parameter id t   = env#register_identifier id IAparameter
  let register_identifier_as_variable id t    = env#register_identifier id IAvariable
  let register_identifier_as_field id t       = env#register_identifier id IAfield
  let register_identifier_as_constructor id   = env#register_identifier id IAconstructor
  let register_identifier_as_label id         = env#register_identifier id IAlabel
  let register_identifier_as_typeparameter id = env#register_identifier id IAtypeparameter

  let register_qname n a =
    try
      let ss = P.name_to_simple_string n in
      let _, id = leftmost_of_name n in
      if env#classtbl#is_resolvable id then
        env#register_global_qname ss a
      else
        env#register_qname ss a
    with
      _ -> ()

  let register_qname_as_class n         = register_qname n (IAclass "")
  let register_qname_as_interface n     = register_qname n (IAinterface "")
  let register_qname_as_typename n      = register_qname n (IAtypename "")
  let register_qname_as_method n        = register_qname n IAmethod
  let register_qname_as_static_member n = register_qname n IAstatic
  let register_qname_as_parameter n     = register_qname n IAparameter
  let register_qname_as_variable n      = register_qname n IAvariable
  let register_qname_as_field n         = register_qname n IAfield
  let register_qname_as_constructor n   = register_qname n IAconstructor
  let register_qname_as_label n         = register_qname n IAlabel
  let register_qname_as_typeparameter n = register_qname n IAtypeparameter
  let register_qname_as_expression n    = register_qname n IAexpression
  let register_qname_as_array n         = register_qname n IAarray


  let _mkty loc d = { ty_desc=d; ty_loc=loc }
  let _mktyargs loc tyargs = { tas_type_arguments=tyargs; tas_loc=loc }
  let mktyparams loc typarams = { tps_type_parameters=typarams; tps_loc=loc }
  let _mktype loc d = { ty_desc=d; ty_loc=loc }
  let _mkmod loc d = { m_desc=d; m_loc=loc }
  let annots_to_mods = List.map (fun a -> _mkmod a.a_loc (Mannotation a))
  let _mkexpr loc d = { e_desc=d; e_loc=loc }
  let _mkprim loc d = { p_desc=d; p_loc=loc }
  let _mkannot loc d = { a_desc=d; a_loc=loc }
  let name_to_ty a n = _mkty n.n_loc (TclassOrInterface [TSname(a, n)])
  let name_to_ty_args loc a n args = _mkty loc (TclassOrInterface [TSapply(a, n, args)])

  let add_ann loc a { ty_desc=d; ty_loc=loc } =
    match d with
    | Tprimitive(a0, pt) -> begin
        _mkty loc (Tprimitive(a@a0, pt))
    end
    | TclassOrInterface (spec::tl) -> begin
        let spec' =
          match spec with
          | TSname(a0, n)        -> TSname(a@a0, n)
          | TSapply(a0, n, args) -> TSapply(a@a0, n, args)
        in
        _mkty loc (TclassOrInterface(spec'::tl))
    end
    | _ -> assert false

  let _simple_name_to_prim loc n = _mkprim loc (Pname n)
  let _simple_name_to_expr loc n = _mkexpr loc (Eprimary (_simple_name_to_prim loc n))
  let mkpkgdecl loc a n = { pd_annotations=a; pd_name=n; pd_loc=loc }
  let _mkcbd loc d = { cbd_desc=d; cbd_loc=loc }
  let mkfd loc ms ty vds = { fd_modifiers=ms; 
			     fd_type=ty; 
			     fd_variable_declarators=vds; 
			     fd_loc=loc;
			   }

  let mkec loc annots id args cb = { ec_annotations=annots; 
				     ec_identifier=id; 
				     ec_arguments=args; 
				     ec_class_body=cb;
				     ec_loc=loc
				   }

  let mkfp loc ms ty vdid va = { fp_modifiers=ms;
				 fp_type=ty;
				 fp_variable_declarator_id=vdid;
				 fp_variable_arity=va;
				 fp_loc=loc
			       }
  let _mkargs loc args = { as_arguments=args; as_loc=loc }
  let _mkated loc d = { ated_desc=d; ated_loc=loc }
  let _mkch loc ms id ts_opt s_opt i_opt = { ch_modifiers=ms;
                                             ch_identifier=id;
                                             ch_type_parameters=ts_opt;
                                             ch_extends_class=s_opt;
                                             ch_implements=i_opt;
                                             ch_loc=loc;
                                           }
  let _mkcd loc d = { cd_desc=d; cd_loc=loc }
  let _mkifh loc ms id ts_opt s_opt = { ifh_modifiers=ms;
                                        ifh_identifier=id;
                                        ifh_type_parameters=ts_opt;
                                        ifh_extends_interfaces=s_opt;
                                        ifh_loc=loc;
                                      }
  let _mkifd loc d = { ifd_desc=d; ifd_loc=loc }
  let mkmh loc m tp rt id pl p d t = { mh_modifiers=m;
				       mh_type_parameters=tp;
				       mh_return_type=rt;
				       mh_name=id;
				       mh_parameters_loc=pl;
				       mh_parameters=p;
				       mh_dims=d;
				       mh_throws=t;
				       mh_loc=loc
				     }
  let mkimd loc mh b = { amd_method_header=mh; amd_body=b; amd_loc=loc }
  let mkcnd loc m tp n pl p t b = { cnd_modifiers=m;
				    cnd_type_parameters=tp;
				    cnd_name=n;
				    cnd_parameters_loc=pl;
				    cnd_parameters=p;
				    cnd_throws=t;
				    cnd_body=b;
				    cnd_loc=loc
				  }


  let _mkfqn sep id =
    let fqn =
      try
	match env#inner_most_class with
	| IAclass s -> s^sep^id
	| _ -> raise Not_found
      with 
	Not_found ->
	  let pname = env#current_package_name in
	  if pname = "" then 
	    id 
	  else 
	    pname^"."^id
    in
    DEBUG_MSG "\"%s\" -> \"%s\"" id fqn;
    fqn

  let mkfqn_cls = _mkfqn "$"


  let is_local_name n =
    DEBUG_MSG "\"%s\"" (P.name_to_simple_string n);
    if is_qualified n then begin
      false
    end
    else begin
      let is_local n =
	let id = rightmost_identifier n in
	try
	  let attrs = env#lookup_identifier id in
          let rec iter = function
            | [] -> false
            | (IAfield | IAstatic)::_ -> false
            | (IAvariable | IAparameter)::_ -> true
            | (IAexpression | IAarray)::rest -> iter rest
            | _ -> false
          in
          iter attrs
	with
	  Not_found -> false
      in
      let b = is_local n in
      DEBUG_MSG "\"%s\" --> %B" (P.name_to_simple_string n) b;
      b
    end

  let is_implicit_field_name n =
    DEBUG_MSG "\"%s\"" (P.name_to_simple_string n);
    let b =
      if is_qualified n then begin
        false
      end
      else begin
        let id = rightmost_identifier n in
	try
	  let attrs = env#lookup_identifier id in
          let rec iter = function
            | [] -> raise Not_found
            | (IAfield | IAstatic)::_ -> true
            | (IAexpression | IAarray)::rest -> iter rest
            | _ -> false
          in
          iter attrs
	with
	  Not_found -> not (env#classtbl#is_resolvable id)
      end
    in
    DEBUG_MSG "\"%s\" --> %B" (P.name_to_simple_string n) b;
    b

  let get_type_fqn n =
    DEBUG_MSG "\"%s\"" (P.name_to_simple_string n);
    let doit n =
      if is_qualified n then begin
        let fqn = env#get_naive_fqn n in
        try
          env#classtbl#resolve_qualified_type_name fqn
        with
	  _ -> begin
	    DEBUG_MSG "\"%s\" --> unknown" fqn;
	    raise (Unknown fqn)
          end
      end
      else begin (* not (is_qualified n) *)
        let id = rightmost_identifier n in
	try
	  let attrs = env#lookup_identifier id in
          DEBUG_MSG "FOUND: %s -> [%s]" id (Xlist.to_string iattr_to_str ";" attrs);
	  let rec doit i =
	    match List.nth attrs i with
	    | IAclass s | IAinterface s | IAtypename s -> s
            | IAtypeparameter -> id
	    | _ -> raise Not_found
	  in
	  doit 0
	with
	  Not_found -> begin
	    try
	      env#classtbl#resolve id
	    with 
	      Not_found -> begin
		try
		  let s_path = env#classtbl#get_source_dir#path in
		  let path = Filename.concat s_path (id^".java") in
		  if env#current_source#tree#exists path then
		    id
		  else
		    raise Not_found
		with 
		  Not_found ->
                    try
                      env#classtbl#resolve id
                    with
                      Not_found ->
		        DEBUG_MSG "\"%s\" --> unknown" id;
		        raise (Unknown id)
              end
          end
      end
    in
    let fqn = doit n in
    DEBUG_MSG "\"%s\" --> %s" (P.name_to_simple_string n) fqn;
    fqn
  (* func get_type_fqn *)

  let is_type_name n =
    let qn = P.name_to_simple_string n in
    DEBUG_MSG "\"%s\"" qn;
    let afilt = function
      | IAclass _ | IAinterface _ | IAtypename _ | IAtypeparameter -> true
      | _ -> false
    in
    let b =
      try
        if is_simple n then begin
          try
            let _ = env#lookup_identifier ~afilt qn in
            true
          with
            Not_found ->
              let _ = env#classtbl#resolve qn in
              true
        end
        else
          raise Not_found
      with
        Not_found ->
          try
            let _ = env#lookup_qname ~afilt qn in
            true
          with
            Not_found -> false
    in
    DEBUG_MSG "%s --> %B" (P.name_to_string n) b;
    b

  let is_expr_name n =
    let qn = P.name_to_simple_string n in
    DEBUG_MSG "\"%s\"" qn;
    let afilt = function
      | IAexpression -> true
      | _ -> false
    in
    let b =
      try
        let _ = env#lookup_qname ~afilt qn in
        true
      with
        Not_found -> false
    in
    DEBUG_MSG "%s --> %B" (P.name_to_string n) b;
    b

  let is_field_access n =
    DEBUG_MSG "\"%s\"" (P.name_to_simple_string n);
    let b =
      if is_qualified n then begin
        let _, id = leftmost_of_name n in
        let afilt = function
          | IAfield | IAvariable | IAparameter -> true
          | _ -> false
        in
        try
          let _ = env#lookup_identifier ~afilt id in
          true
        with
          Not_found ->
            not (env#classtbl#is_resolvable id)
      end
      else begin
        let id = rightmost_identifier n in
        let afilt = function
          | IAfield -> true
          | _ -> false
        in
        try
          let _ = env#lookup_identifier ~afilt id in
          true
        with
          Not_found -> false
      end
    in
    (*Printf.printf "\"%s\" -> %B\n%!" (P.name_to_string n) b;*)
    DEBUG_MSG "\"%s\" --> %B" (P.name_to_simple_string n) b;
    b



  let mkty so eo d = _mkty (get_loc so eo) d
  let mkwb so eo d = { wb_desc=d; wb_loc=(get_loc so eo) }
  let mktyarg so eo d = { ta_desc=d; ta_loc=(get_loc so eo) }
  let mktyparam so eo (al, tvar) tb = { tp_type_variable=tvar;
                                        tp_annotations=al;
				        tp_type_bound=tb; 
				        tp_loc=(get_loc so eo) 
				}
  let mktb so eo rty ab = { tb_reference_type=rty; 
			    tb_additional_bounds=ab; 
			    tb_loc=(get_loc so eo) 
			  }
  let mkab so eo intf = { ab_interface=intf; ab_loc=(get_loc so eo) }

  let _mkname loc d = { n_desc=d; n_loc=loc }
  let mkname so eo d = _mkname (get_loc so eo) d

  let mktyargs so eo d = _mktyargs (get_loc so eo) d
  let mktype so eo d = _mktype (get_loc so eo) d
  let mkmod so eo d = _mkmod (get_loc so eo) d
  let mkmods so eo ms = { ms_modifiers=ms; ms_loc=(get_loc so eo) }
  let mkaop so eo d = { ao_desc=d; ao_loc=(get_loc so eo) }
  let mkstmt so eo d = { s_desc=d; s_loc=(get_loc so eo) }
  let mkerrstmt so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { s_desc=Serror; s_loc=loc }

  let mkexpr so eo d = _mkexpr (get_loc so eo) d

  let mklp so eo d = { lp_desc=d; lp_loc=(get_loc so eo) }

  let mkerrexpr so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { e_desc=Eerror; e_loc=loc }


  let mkstmtexpr so eo d = { se_desc=d; se_loc=(get_loc so eo) }

  let mkerrstmtexpr so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { se_desc=SEerror; se_loc=loc }

  let mkprim so eo d = _mkprim (get_loc so eo) d

  let mkerrprim so eo =
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { p_desc=Perror; p_loc=loc }


  let mkannot so eo d = _mkannot (get_loc so eo) d
  let mkev so eo d = { ev_desc=d; ev_loc=(get_loc so eo) }
  let simple_name_to_prim so eo n = _simple_name_to_prim (get_loc so eo) n

  let rec name_to_facc name =
    DEBUG_MSG "%s" (P.name_to_string name);
    match name.n_desc with
    | Nsimple(a, i) ->
        a := NAexpression;
        let lab =
          try
            match env#lookup_identifier i with
            | IAfield::_ -> PfieldAccess(FAimplicit i)
            | _ -> Pname name
          with
            Not_found -> PfieldAccess(FAimplicit i)
        in
        _mkprim name.n_loc lab
    | Nqualified(a, n, i) ->
        a := NAexpression;
        _mkprim name.n_loc (PfieldAccess(FAprimary(name_to_facc n, i)))

  let _name_to_prim loc n =
    DEBUG_MSG "[%s] %s" (Loc.to_string loc) (P.name_to_string n);
    let id = rightmost_identifier n in
    try
      let q = get_qualifier n in
      if is_type_name q then begin
        set_name_attribute NAexpression n;
	_mkprim loc (Pname n)
      end
      else if is_field_access q then begin
        name_to_facc n
      end
      else begin
        _mkprim loc (Pname n)
      end
    with
      Not_found -> begin
	try
          let rec iter = function
            | [] -> begin
                set_name_attribute NAexpression n;
                _mkprim loc (PfieldAccess(FAimplicit id))
            end
	    | IAfield::_ -> begin
                set_name_attribute NAexpression n;
                _mkprim loc (PfieldAccess(FAimplicit id))
            end
	    | (IAvariable | IAparameter)::_ -> begin
                set_name_attribute NAexpression n;
                _simple_name_to_prim loc n
            end
	    | _::rest -> iter rest
          in
          iter (env#lookup_identifier id)
	with
	  Not_found -> begin
            set_name_attribute NAexpression n;
            _mkprim loc (PfieldAccess(FAimplicit id))
          end
      end

  let _name_to_expr loc n = _mkexpr loc (Eprimary (_name_to_prim loc n))
  let name_to_prim so eo n = _name_to_prim (get_loc so eo) n
  let simple_name_to_expr so eo n = _simple_name_to_expr (get_loc so eo) n
  let name_to_expr so eo n = _name_to_expr (get_loc so eo) n
  let mkimpdecl so eo d = { id_desc=d; id_loc=(get_loc so eo) }
  let mkcic so eo d = { cic_desc=d; cic_loc=(get_loc so eo) }
  let mkcb so eo decls = { cb_class_body_declarations=decls; cb_loc=(get_loc so eo) }
  let mkcbd so eo d = _mkcbd (get_loc so eo) d
  let mkerrcbd so eo = 
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { cbd_desc=CBDerror; cbd_loc=loc }

  let mksb so eo l = { sb_switch_block_stmt_grps=l; sb_loc=(get_loc so eo) }

  let mkexc so eo c = { exc_class=c; exc_loc=(get_loc so eo) }
  let mkexi so eo ifs = { exi_interfaces=ifs; exi_loc=(get_loc so eo) }
  let mkim so eo ifs = { im_interfaces=ifs; im_loc=(get_loc so eo) }
  let mkeb so eo ecs cbds = { eb_enum_constants=ecs; 
			      eb_class_body_declarations=cbds; 
			      eb_loc=(get_loc so eo)
			    }
  let mkvd so eo vdid vdini = { vd_variable_declarator_id=vdid;
				vd_variable_initializer=vdini;
				vd_is_local=(ref true);
				vd_loc=(get_loc so eo)
			      }
  let mkvi so eo d = { vi_desc=d; vi_loc=(get_loc so eo) }
  let mkth so eo es = { th_exceptions=es; th_loc=(get_loc so eo) }
  let mkcnb so eo eci bss = { cnb_explicit_constructor_invocation=eci;
			      cnb_block=bss;
			      cnb_loc=(get_loc so eo)
			    }
  let mkb so eo bss = { b_block_statements=bss; b_loc=(get_loc so eo) }
  let mkeci so eo d = { eci_desc=d; eci_loc=(get_loc so eo) }
  let mkmr so eo d = { mr_desc=d; mr_loc=(get_loc so eo) }
  let mkargs so eo d = _mkargs (get_loc so eo) d
  let mkib so eo imds = { ib_member_declarations=imds; ib_loc=(get_loc so eo) }
  let mkatb so eo ateds = { atb_element_declarations=ateds; atb_loc=(get_loc so eo) }
  let mkated so eo d = _mkated (get_loc so eo) d
  let mkbs so eo d = { bs_desc=d; bs_loc=(get_loc so eo) }

  let mkerrbs so eo = 
    let loc = get_loc so eo in
    env#missed_regions#add loc;
    { bs_desc=BSerror; bs_loc=loc }


  let mklvd so eo ms ty vds = { lvd_modifiers=ms; 
				lvd_type=ty; 
				lvd_variable_declarators=vds;
				lvd_loc=(get_loc so eo)
			      }
  let mkfi so eo d = { fi_desc=d; fi_loc=(get_loc so eo) }
  let mkcatch so eo param b = { c_formal_parameter=param; c_block=b; c_loc=(get_loc so eo) }
  let mkfinally so eo b = { f_block=b; f_loc=(get_loc so eo)}
  let mkmi so eo d = { mi_desc=d; mi_loc=(get_loc so eo) }
  let mktd so eo d = { td_desc=d; td_loc=(get_loc so eo) }
  let mkaa so eo d = { aa_desc=d; aa_loc=(get_loc so eo) }
  let mkch so eo ms id ts_opt s_opt i_opt = _mkch (get_loc so eo) ms id ts_opt s_opt i_opt
  let mkcd so eo d = _mkcd (get_loc so eo) d
  let mkifh so eo ms id ts_opt s_opt = _mkifh (get_loc so eo) ms id ts_opt s_opt
  let mkifd so eo d = _mkifd (get_loc so eo) d
  let mkde so eo d = { de_desc=d; de_loc=(get_loc so eo) }
  let mksl so eo d = { sl_desc=d; sl_loc=(get_loc so eo) }
  let mkevp so eo d = { evp_desc=d; evp_loc=(get_loc so eo) }

  let mkcu p i t = { cu_package=p; cu_imports=i; cu_tydecls=t }

  let begin_scope ?(kind=FKother) () = env#begin_scope ~kind ()
  let end_scope() = env#end_scope()

  let end_typeparameter_scope = function
    | Some ts -> List.iter (fun _ -> end_scope()) ts.tps_type_parameters
    | None -> ()


end (* of functor Parser_aux.F *)
