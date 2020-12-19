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
(* sourcecode.ml *)


open Otreediff

module GI  = GIndex
module MID = Moveid
module C   = Compression
module SB  = Spec_base

let sprintf = Printf.sprintf

type name = string

let n_lines_for_file_check = 32

let bison_pat = Str.regexp "#define[ \t]+YYBISON[ \t]+1"
let flex_pat = Str.regexp "#define[ \t]+FLEX_SCANNER"

let search_pat pats file =
  let b = ref false in
  let ch = file#get_channel in
  begin
    try
      for i = 1 to n_lines_for_file_check do
        let s = ch#input_line() in
        try
          List.iter 
            (fun pat ->
              let _ = (Str.search_forward pat s 0) in
              b := true;
              raise Exit
            ) pats
        with
          Not_found -> ()
      done
    with
    | End_of_file -> ()
    | Exit -> ()
  end;
  ch#close_in();
  !b

let is_generated f = search_pat [bison_pat;flex_pat] f


let unknown_origin = "-1"
let earliest_origin = "0"


let dump_gmap_ch gmap ch =
  List.iter (fun (gi1, gi2) -> Printf.fprintf ch "%d - %d\n" gi1 gi2) gmap

let mkgmapfilepath ?(gmap_ext=".gmap") cache_path frag =
  Filename.concat cache_path (frag#hash ^ gmap_ext)

let dump_gmap gmap fpath = 
  let d = Filename.dirname fpath in
  if not (Xfile.dir_exists d) then begin
    Xfile.mkdir d
  end;
  Xfile.dump fpath (dump_gmap_ch gmap)

let load_gmap_ch ch =
  let l = ref [] in
  try
    while true do
      let s = input_line ch in
      Scanf.sscanf s "%d - %d" (fun i j -> l := (i, j)::!l)
    done;
    []
  with 
    End_of_file -> List.rev !l

let load_gmap fpath =
  DEBUG_MSG "fpath=\"%s\"" fpath;
  try
    Xfile.load fpath load_gmap_ch
  with 
    Failure _ -> []


let digest_of_file options file =
  DEBUG_MSG "file=\"%s\"" file;
  Xhash.digest_of_file options#hash_algo file

let encode_digest options d =
  (Xhash.algo_to_string options#fact_algo)^Entity.sub_sep^(Xhash.to_hex d)

let encoded_digest_of_file options file =
  let d = digest_of_file options file in
  encode_digest options d



let int_of_origin ogn =
  if ogn = unknown_origin then 
    Origin.attr_unknown
  else begin
    try
      int_of_string ogn
    with
      Invalid_argument s -> ERROR_MSG "int_of_origin: %s" s; exit 1
  end

let merge_locs nds =
  let lmerge loc1 loc2 = 
    if loc1 = Loc.dummy then loc2 
    else if loc2 = Loc.dummy then loc1 
    else Loc.merge loc1 loc2
  in
  List.fold_left (fun l n -> lmerge l n#data#src_loc) Loc.dummy nds

let is_ghost_node nd = nd#data#src_loc = Loc.ghost

module Tree (L : Spec.LABEL_T) = struct

  let of_elem_data name attrs =
    let lname = XML.get_local_part name in
    let lattrs = List.map (fun (a, v) -> XML.get_local_part a, v) attrs in
    L.of_elem_data lname lattrs ""

  let compare_node nd1 nd2 = compare nd1#data#label nd2#data#label

  exception Found
  exception Malformed_row of string

  let get_lab nd = (Obj.obj nd#data#_label : L.t)
  let get_orig_lab_opt nd =
    match nd#data#orig_lab_opt with
    | Some o -> Some (Obj.obj o : L.t)
    | None -> None

  let get_annotation nd = (Obj.obj nd#data#_annotation : L.annotation)

  class ordinal_tbl (l : int list) = object (self) (* handling logical ordinal of a child *)
    val mutable ordinal_list = l

    method get_ordinal nth =
      let rec doit i a = function
        | [] -> raise Not_found
        | h::t ->
            let ah = a + h in
            if a <= nth && nth < ah then
              i
            else
              doit (i+1) ah t
      in
      doit 0 0 ordinal_list

    method list = ordinal_list

    method add_list l =
      ordinal_list <- ordinal_list @ l

    method to_string =
      "["^(Xlist.to_string string_of_int ";" ordinal_list)^"]"
  end

  let null_ordinal_tbl = new ordinal_tbl []

  class node_data 
      options 
      ?(annot=L.null_annotation) 
      ?(ordinal_tbl_opt=(None : ordinal_tbl option))
      ?(orig_lab_opt=(None : L.t option))
      (lab : L.t)
      =
    let is_named      = L.is_named lab in
    let is_named_orig = L.is_named_orig lab in
    let category      = L.get_category lab in

    object (self : 'self)

      inherit Otree.data2

      constraint 'node = 'self Otree.node2

      val mutable prefix = ""
      method set_prefix s = prefix <- s
      method get_prefix = prefix

      val mutable suffix = ""
      method set_suffix s = suffix <- s
      method get_suffix = suffix

      val mutable _eq = fun _ -> false

      val mutable source_fid = ""
      method set_source_fid x = source_fid <- x
      method source_fid = source_fid

      method get_ordinal nth =
        match ordinal_tbl_opt with
        | None -> nth
        | Some tbl -> tbl#get_ordinal nth

      method add_to_ordinal_list l =
        match ordinal_tbl_opt with
        | None -> failwith "Sourcecode.node_data#add_to_ordinal_list"
        | Some tbl -> tbl#add_list l

      val mutable lab = lab
      val mutable orig_lab_opt = orig_lab_opt

      val mutable _label = Obj.repr ()
      method _label = _label

      val mutable label = ""
      method label = label

      val mutable rep = ""
      method to_rep = rep

      val mutable short_string = ""
      method to_short_string = short_string

      val mutable _digest = None
      method _digest = _digest

      val mutable digest = None
      method digest = digest

      method private update =
        label <- L.to_string lab;
        _label <- Obj.repr lab;
        let ignore_identifiers_flag = options#ignore_identifiers_flag in
        let short_str = L.to_short_string ~ignore_identifiers_flag lab in
        rep <- short_str;
        short_string <- short_str;
        match _digest with
        | Some d -> rep <- String.concat "" [rep;"<";d;">"]
        | None -> ()

      method elem_name_for_delta =
        let n, _, _ = self#to_elem_data_for_delta in
        n

      method orig_elem_name_for_delta =
        let n, _, _ = self#orig_to_elem_data_for_delta in
        n

      method elem_attrs_for_delta =
        let _, attrs, _ = self#to_elem_data_for_delta in
        attrs

      method orig_elem_attrs_for_delta =
        let _, attrs, _ = self#orig_to_elem_data_for_delta in
        attrs
        
      method change_attr (attr : string) (v : string) =
        let name, attrs, _ = self#orig_to_elem_data_for_delta in
        if List.mem_assoc attr attrs then begin
          let attrs' = List.remove_assoc attr attrs in
          let lab' = of_elem_data name ((attr, v)::attrs') in
          DEBUG_MSG "%s -> %s" (L.to_string lab) (L.to_string lab');
          orig_lab_opt <- Some lab';
          (*self#update*)
        end

      method delete_attr (attr : string) =
        let name, attrs, _ = self#orig_to_elem_data_for_delta in
        let attrs' = List.remove_assoc attr attrs in
        orig_lab_opt <- Some (of_elem_data name attrs');
        (*self#update*)

      method insert_attr (attr : string) (v : string) =
        let name, attrs, _ = self#orig_to_elem_data_for_delta in
        let attrs' = List.remove_assoc attr attrs in
        orig_lab_opt <- Some (of_elem_data name ((attr, v)::attrs'));
        (*self#update*)


      val successors = (Xset.create 0 : 'node Xset.t)
      method successors = successors
      method add_successor nd = Xset.add successors nd

      val mutable binding = Binding.NoBinding
      method binding = binding
      method set_binding b = binding <- b

      val mutable bindings = []
      method bindings = bindings
      method add_binding (b : Binding.t) =
        if not (List.mem b bindings) then
          bindings <- b :: bindings


      method get_ident_use = L.get_ident_use lab

      val mutable origin = unknown_origin
      method origin = origin
      method set_origin o = origin <- o

      val mutable ending = unknown_origin
      method ending = ending
      method set_ending e = ending <- e

      val mutable frommacro = ""
      method frommacro = frommacro
      method set_frommacro fm = frommacro <- fm
      method is_frommacro = frommacro <> ""
      method not_frommacro = frommacro = ""

      val _annotation = Obj.repr annot
      method _annotation = _annotation

      method quasi_eq (ndat : 'self) =
	L.quasi_eq lab (Obj.obj ndat#_label : L.t)

      method relabel_allowed (ndat : 'self) =
	L.relabel_allowed (lab, (Obj.obj ndat#_label : L.t))

      method is_compatible_with (ndat : 'self) =
        L.is_compatible lab (Obj.obj ndat#_label : L.t) ||
        match orig_lab_opt, ndat#orig_lab_opt with
        | Some l1, Some o2 -> L.is_compatible l1 (Obj.obj o2)
        | _ -> false

      method is_order_insensitive = L.is_order_insensitive lab

      method move_disallowed = L.move_disallowed lab

      method is_common = L.is_common lab

      method _anonymized_label = Obj.repr (L.anonymize lab)

      (*method _more_anonymized_label = Obj.repr (L.anonymize ~more:true lab)*)

      val mutable anonymized_label = None
      method anonymized_label =
	match anonymized_label with
	| None ->
	    let alab = L.to_string (L.anonymize lab) in
	    anonymized_label <- Some alab;
	    alab
	| Some alab -> alab

      val mutable more_anonymized_label = None
      method more_anonymized_label =
	match more_anonymized_label with
	| None ->
	    let alab = L.to_string (L.anonymize ~more:true lab) in
	    more_anonymized_label <- Some alab;
	    alab
	| Some alab -> alab

      val mutable anonymized2_label = None
      method _anonymized2_label = Obj.repr (L.anonymize2 lab)
      method anonymized2_label =
	match anonymized2_label with
	| None ->
	    let alab = L.to_string (L.anonymize2 lab) in
	    anonymized2_label <- Some alab;
	    alab
	| Some alab -> alab

      val mutable anonymized3_label = None
      method _anonymized3_label = Obj.repr (L.anonymize3 lab)
      method anonymized3_label =
	match anonymized3_label with
	| None ->
	    let alab = L.to_string (L.anonymize3 lab) in
	    anonymized3_label <- Some alab;
	    alab
	| Some alab -> alab


      method to_simple_string = L.to_simple_string lab


      method _set_digest d = 
	_digest <- Some d;
        let ignore_identifiers_flag = options#ignore_identifiers_flag in
	rep <- String.concat "" 
	    [L.to_short_string ~ignore_identifiers_flag lab;"<";d;">"]

      method set_digest d = 
	digest <- Some d;
        self#_set_digest d

      method reset_digest = digest <- None


      method to_be_notified = L.is_to_be_notified lab

      method is_boundary = L.is_boundary lab

      method is_partition = L.is_partition lab

      method is_sequence = L.is_sequence lab

      method is_phantom = L.is_phantom lab

      method is_special = L.is_special lab

      method to_elem_data = L.to_elem_data self#src_loc lab

      method to_elem_data_for_delta = L.to_elem_data ~strip:true self#src_loc lab

      method orig_to_elem_data_for_delta =
        let lab_ =
          match orig_lab_opt with
          | Some l -> l
          | None -> lab
        in
        L.to_elem_data ~strip:true self#src_loc lab_

      method eq ndat = _eq ndat
        (*_label = ndat#_label && self#orig_lab_opt = ndat#orig_lab_opt*)

      method subtree_equals ndat = 
	self#eq ndat && _digest = ndat#_digest && _digest <> None

      method equals ndat = self#eq ndat && digest = ndat#digest

      val mutable src_loc = Loc.dummy
      method set_loc loc = src_loc <- loc
      method src_loc = src_loc

      method to_string = 
	sprintf "%s%s[%s]"
          self#label
          (match orig_lab_opt with Some l -> "("^(L.to_string l)^")" | None -> "")
          (Loc.to_string src_loc)

      method is_named = is_named
      method is_named_orig = is_named_orig

      method is_anonymous = not is_named
      method is_anonymous_orig = not is_named_orig

      method feature =
	if self#is_named then 
	  self#_label, None
	else 
	  self#_label, self#digest


      method get_category = category
      method get_name = L.get_name lab
      method get_value = L.get_value lab
      method has_value = L.has_value lab
      method has_non_trivial_value = L.has_non_trivial_value lab
      method is_string_literal = L.is_string_literal lab
      method is_int_literal = L.is_int_literal lab
      method is_real_literal = L.is_real_literal lab

      val mutable move_id = MID.unknown
      method set_mid mid = move_id <- mid
      method mid = move_id

      method orig_lab_opt =
        match orig_lab_opt with
        | Some l -> Some (Obj.repr l)
        | None -> None

      initializer
        _eq <-
          if options#weak_flag then
            (fun x ->
              _label = x#_label && self#orig_lab_opt = x#orig_lab_opt ||
              (not self#is_named_orig) && (not self#has_value) &&
              (not x#is_named_orig) && (not x#has_value) &&
              self#elem_name_for_delta = x#elem_name_for_delta ||
              (match self#orig_lab_opt, x#orig_lab_opt with
              | Some o1, Some o2 -> o1 = o2
              | Some o1, None -> L.is_compatible (Obj.obj o1) (Obj.obj x#_label)
              | None, Some o2 -> L.is_compatible (Obj.obj _label) (Obj.obj o2)
              | _ -> false) ||
              self#is_compatible_with x)
          else
            (fun x -> _label = x#_label && self#orig_lab_opt = x#orig_lab_opt);
	self#update

	    
      (* for searchast *)
      val mutable char = None
      method char =
	if char = None then begin
	  let c = L.to_char lab in
	  char <- Some c;
	  c
	end
	else
	  match char with
	  | Some c -> c
	  | _ -> assert false
		
	    


    end (* of class Sourcecode.node_data *)


  let mknode options
      ?(annot=L.null_annotation)
      ?(ordinal_tbl_opt=None)
      ?(orig_lab_opt=None)
      lab nodes
      =
    Otree.create_node2 options#uid_generator
      (new node_data options ~annot ~ordinal_tbl_opt ~orig_lab_opt lab) (Array.of_list nodes)

  let mklnode options ?(annot=L.null_annotation) ?(orig_lab_opt=None) lab nodes =
    mknode options ~annot ~ordinal_tbl_opt:(Some null_ordinal_tbl) ~orig_lab_opt lab nodes

  let mkleaf options ?(annot=L.null_annotation) ?(orig_lab_opt=None) lab =
    Otree.create_node2 options#uid_generator
      (new node_data options ~annot ~orig_lab_opt lab) [||]

  let _get_logical_nth_child nd nth =
    let l = ref [] in
    Array.iteri
      (fun i x ->
        if (nd#data#get_ordinal i) = nth then
          l := x :: !l
      ) nd#children;
    Array.of_list (List.rev !l)

  let get_logical_nth_child nd nth =
    let l = ref [] in
    Array.iteri
      (fun i x ->
        if (nd#data#get_ordinal i) = nth then
          l := x :: !l
      ) nd#initial_children;
    Array.of_list (List.rev !l)


  class node_maker options = object (self)
    method private mknode
        ?(annot=L.null_annotation)
        ?(ordinal_tbl_opt=None)
        ?(orig_lab_opt=None)
        lab nodes
        =
      mknode options ~annot ~ordinal_tbl_opt ~orig_lab_opt lab nodes

    method private mklnode ?(annot=L.null_annotation) ?(orig_lab_opt=None) lab nodes =
      mklnode options ~annot ~orig_lab_opt lab nodes

    method private mkleaf ?(annot=L.null_annotation) ?(orig_lab_opt=None) lab =
      mkleaf options ~annot ~orig_lab_opt lab
  end


  type node_t = Spec.node_t


  let make_unparser unparse node ch =
    let redirect = not (SB.OutChannel.is_stdout ch) in
    if redirect then begin
      try
        Format.set_formatter_out_channel (SB.OutChannel.to_pervasives ch)
      with
        _ ->
          let xc = SB.OutChannel.to_xchannel ch in
          let out s pos len = ignore (xc#output_ s pos len) in
          let flush () = xc#close in
          Format.set_formatter_output_functions out flush
    end;
    unparse node;
    if redirect then begin
      Format.set_formatter_out_channel Stdlib.stdout
    end



  class c options (root : node_t) (is_whole : bool) = object (self : 'self)

    inherit node_maker options
    inherit [ node_t ] Otree.otree2 root is_whole as super


    method private create root is_whole = new c options root is_whole

    method extra_namespaces = ([] : (string * string) list)

    method unparse_subtree_ch : ?fail_on_error:bool -> node_t -> SB.OutChannel.t -> unit =
      fun ?(fail_on_error=true) _ _ ->
        failwith "Sourcecode.unparse_subtree_ch: unparser is not implemented yet"
        
    method unparse_ch ?(fail_on_error=true) = self#unparse_subtree_ch ~fail_on_error root

    method get_digest nd =
      let st = self#create nd false in
      let d = st#digest in
      nd#data#_set_digest d;
      d

    val mutable true_parent_tbl = (Hashtbl.create 0 : (UID.t, node_t) Hashtbl.t)
    method set_true_parent_tbl tbl = true_parent_tbl <- tbl
    method find_true_parent uid =
      Hashtbl.find true_parent_tbl uid

    val mutable true_children_tbl = (Hashtbl.create 0 : (node_t, node_t array) Hashtbl.t)
    method set_true_children_tbl tbl = true_children_tbl <- tbl

    method recover_true_children ~initial_only () =
      (*Printf.printf "! [before] initial_size=%d (initial_only=%B)\n"
        self#initial_size initial_only;*)
      DEBUG_MSG "initial_only=%B" initial_only;
      let modified = ref false in
      Hashtbl.iter 
	(fun nd c ->

	  DEBUG_MSG "recovering true children: %a -> [%s]"
	    UID.ps nd#uid (Xarray.to_string (fun n -> UID.to_string n#uid) ";" c);

          (*if (Array.length nd#initial_children) <> (Array.length c) then begin
            Printf.printf "!!! %s\n" nd#initial_to_string;
	    Printf.printf "!!! [%s] -> [%s]\n"
              (Xarray.to_string (fun n -> UID.to_string n#uid) ";" nd#initial_children)
	      (Xarray.to_string (fun n -> UID.to_string n#uid) ";" c)
          end;*)

	  nd#set_initial_children c;
          if not initial_only then
	    nd#set_children c;
	  Array.iteri
            (fun i n ->
              n#set_initial_parent nd;
              n#set_initial_pos i;
              if not initial_only then begin
                n#set_parent nd;
                n#set_pos i
              end
            ) c;
	  modified := true
	) true_children_tbl;

      BEGIN_DEBUG
	Hashtbl.iter
	  (fun uid nd ->
	    DEBUG_MSG "true parent: %a -> %a" UID.ps uid UID.ps nd#uid
	  ) true_parent_tbl
      END_DEBUG;

      if !modified then begin
	self#fast_scan_whole_initial (fun nd -> nd#set_gindex GI.unknown);
	self#setup_initial_size;
	self#setup_gindex_table;
	self#setup_initial_leftmost_table;
        self#setup_apath
      end(*;
      Printf.printf "! [after] initial_size=%d\n" self#initial_size*)

    val mutable source_path = "unknown"
    method set_source_path p = source_path <- p
    method source_path = source_path

    val mutable source_fullpath = "unknown"
    method set_source_fullpath p = source_fullpath <- p
    method source_fullpath = source_fullpath

    val mutable source_kind = Storage.kind_dummy
    method set_source_kind k = source_kind <- k
    method source_kind = source_kind

    val mutable vkind = Entity.V_UNKNOWN
    method set_vkind k = vkind <- k
    method vkind = vkind

    val mutable version = ""
    method set_version n = version <- n
    method version = version

    val mutable proj_root = ""
    method set_proj_root r = proj_root <- r
    method proj_root = proj_root

    val mutable source_digest = "unknown"
    method set_source_digest d = source_digest <- d
    method source_digest = source_digest
    method encoded_source_digest = encode_digest options source_digest

    method set_source_info (file : Storage.file) =
      self#set_source_path file#path;
      self#set_source_fullpath file#fullpath;
      self#set_source_kind file#kind;
      self#set_source_digest file#digest

    val mutable parser_name = "unknown"
    method set_parser_name n = parser_name <- n
    method parser_name = parser_name

    method private get_attrs =
      let attrs_to_string attrs =
	String.concat "" (List.map (fun (a, v) -> sprintf " %s='%s'" a v) attrs)
      in
      let lang_attr =
        try
          ["xmlns:"^L.lang_prefix, List.assoc L.lang_prefix Astml.parser_tbl;]
        with
          Not_found -> []
      in
      let l =
        lang_attr @
	[ "xmlns:"^Astml.default_prefix, Astml.ast_ns;
	  Astml.parser_attr_name, self#parser_name;
	  Astml.source_attr_name, Filename.basename self#source_path;
	  Astml.source_digest_attr_name, self#encoded_source_digest;
	]
      in
      attrs_to_string l

    method dump_astml ?(comp=C.none) fname =
      let pre_tags = sprintf "<%s%s>" Astml.astml_tag self#get_attrs in
      let post_tags = sprintf "</%s>" Astml.astml_tag in
      super#save_in_xml ~initial:true ~comp ~pre_tags ~post_tags fname


    method collapse =
      let filt nd = 
	let lab = get_lab nd in
	if L.forced_to_be_collapsible lab then
	  nd#set_collapsible;
	L.is_collapse_target options lab
      in
      self#collapse_nodes filt


    (* subtree copy (gindexes are inherited) *)
    method make_subtree_copy ?(find_hook=fun _ -> raise Not_found) (nd : node_t) =
      let hooked = ref [] in
      let rec doit nd =
	let gi = nd#gindex in
	if GI.is_valid gi then
	  let children = Xlist.filter_map doit (Array.to_list nd#initial_children) in
	  let lab = get_lab nd in
          let orig_lab_opt = get_orig_lab_opt nd in
	  let new_nd = self#mknode ~orig_lab_opt lab children in
	  new_nd#set_gindex gi;
          begin
            try
              let a = find_hook nd in
              hooked := (new_nd, a) :: !hooked
            with
              Not_found -> ()
          end;
	  Some new_nd
	else
	  None
      in
      let root = 
	match doit nd with
	| Some r -> r
	| None -> raise (Invalid_argument "Sourcecode.c#make_subtree_copy")
      in
      let tree = self#create root false in
      tree#_register_gindexes;
      List.iter
        (fun (n, a) ->
          a n
        ) !hooked;
      tree

    (* subtree copy (gindexes are inherited) *)
    method private make_anonymizedx_subtree_copy anonymize ?(uids_left_named=[]) (nd : node_t) =
      let rec doit n =
	let gi = n#gindex in
	if GI.is_valid gi then
	  let children = Xlist.filter_map doit (Array.to_list n#initial_children) in
	  let alab = 
	    if List.mem n#uid uids_left_named then
	      get_lab n
	    else
	      anonymize n
	  in
	  let new_nd = self#mknode alab children in
	  new_nd#set_gindex gi;
	  Some new_nd
	else
	  None
      in
      let root = 
	match doit nd with
	| Some r -> r
	| None -> raise (Invalid_argument "Sourcecode.c#make_anonymized_subtree_copy")
      in
      let tree = self#create root false in
      tree#_register_gindexes;
      tree

    method make_anonymized_subtree_copy ?(uids_left_named=[]) (nd : node_t) =
      let anonymize n = (Obj.obj n#data#_anonymized_label : L.t) in
      self#make_anonymizedx_subtree_copy anonymize ~uids_left_named nd

    method make_anonymized2_subtree_copy ?(uids_left_named=[]) (nd : node_t) =
      let anonymize n = (Obj.obj n#data#_anonymized2_label : L.t) in
      self#make_anonymizedx_subtree_copy anonymize ~uids_left_named nd

    method make_anonymized3_subtree_copy ?(uids_left_named=[]) (nd : node_t) =
      let anonymize n = (Obj.obj n#data#_anonymized3_label : L.t) in
      self#make_anonymizedx_subtree_copy anonymize ~uids_left_named nd


    method get_ident_use_list gid =
      let nd = self#search_node_by_gindex gid in
      let res = ref [] in
      self#preorder_scan_whole_initial_subtree nd
	(fun n ->
	  let s = n#data#get_ident_use in
	  if not (List.mem s !res) && s <> "" then
	    res := s::!res
	);
      List.rev !res


	
    method subtree_to_simple_string gid =
      let nd = self#search_node_by_gindex gid in
      let children = Array.to_list nd#initial_children in
      match children with
      | [] -> nd#data#to_simple_string 
      | _ ->
	  sprintf "%s(%s)" 
	    nd#data#to_simple_string 
	    (String.concat "," 
	       (List.map 
		  (fun nd -> self#subtree_to_simple_string nd#gindex)
		  children
	       )
	    )

(*
  val mutable line_terminator = ""
  method set_line_terminator s = line_terminator <- s
  method line_terminator = line_terminator
  method line_terminator_name =
  match line_terminator with
  "\013\010" -> "CRLF"
  | "\013" -> "CR"
  | "\010" -> "LF"
  | _ -> "??"
 *)

    method dump_line_ranges fpath =
      let ch = open_out fpath in
      self#fast_scan_whole_initial
	(fun nd ->
	  Printf.fprintf ch "%d-%d\n" 
	    nd#data#src_loc.Loc.start_line nd#data#src_loc.Loc.end_line
	)

    val mutable ignored_regions = ([] : (int * int) list)
    method set_ignored_regions r = ignored_regions <- r
    method ignored_regions = ignored_regions

    val mutable misparsed_regions = ([] : (int * int) list)
    method set_misparsed_regions r = misparsed_regions <- r
    method misparsed_regions = misparsed_regions

    val mutable total_LOC = 0
    method set_total_LOC n = total_LOC <- n
    method total_LOC = total_LOC

    val mutable misparsed_LOC = 0
    method set_misparsed_LOC n = misparsed_LOC <- n
    method misparsed_LOC = misparsed_LOC

    method get_units_to_be_notified =
      let res = ref [] in
      self#fast_scan_whole_initial
	(fun nd ->
	  if nd#data#to_be_notified then
	    res := nd::!res
	);
      !res

    method get_nearest_containing_unit uid =
      let nd = self#search_node_by_uid uid in
      let ancs = List.rev (self#initial_ancestor_nodes nd) in
      if nd#data#to_be_notified then 
	nd
      else
	let rec scan = function
	    h::t -> if h#data#to_be_notified then h else scan t
	  | [] -> raise Not_found
	in
	scan ancs


    method make_subtree_from_node nd =
      let tree = self#create nd false in
      tree#_set_gindex_table gindex_table;
      tree#_set_initial_leftmost_table initial_leftmost_table;
      tree

    method make_subtree_from_uid uid =
      let nd = self#search_node_by_uid uid in
      self#make_subtree_from_node nd

    method make_subtree_from_path path =
      let nd = self#initial_acc path in
      self#make_subtree_from_node nd

    method label_match kw =
      let re = Str.regexp (".*"^kw^".*") in
      try
	self#fast_scan_whole_initial
	  (fun nd ->
	    if Str.string_match re nd#data#label 0 then begin
	      Xprint.verbose options#verbose_flag "keyword=\"%s\" matched=%s" kw nd#data#to_string;
	      raise Found
	    end
	  );
	false
      with 
        Found -> true


    method merge_locs_adjusting_to_boundary gids =
      BEGIN_DEBUG
	DEBUG_MSG "tree size: %d" self#size;
	List.iter 
	  (fun gid -> 
	    if gid >= self#size then begin
	      WARN_MSG "invalid gid: %a" GI.ps gid;
	      exit 1
	    end
	  ) gids
      END_DEBUG;

      let groups = ref [] in
      let nds = List.map self#search_node_by_gindex gids in
      let rec scan = function
	| []        -> ()
	| nd0::rest -> 
	    let group = ref [nd0] in
	    List.iter 
	      (fun nd -> 
		if not (self#cross_boundary nd0 nd) then
		  group := nd::!group
	      ) rest;
	    groups := !group::!groups
      in
      scan nds;
      let max_group = ref [] in
      let max = ref 0 in
      List.iter
	(fun g -> 
	  if (List.length g) > !max then begin
	    max_group := g;
	    max := List.length g
	  end
	) !groups;
      merge_locs !max_group
	

    method private cross_boundary nd1 nd2 =
      let ands1 = List.filter (fun n -> n#data#not_frommacro) (self#initial_ancestor_nodes nd1) in
      let ands2 = List.filter (fun n -> n#data#not_frommacro) (self#initial_ancestor_nodes nd2) in
      let rec scan b = function
	| h1::t1, h2::t2 -> 
	    if h1 == h2 then 
	      scan false (t1, t2)
	    else
	      scan (b || h1#data#is_boundary || h2#data#is_boundary) (t1, t2)
	| [], rest | rest, [] ->
	    if b then 
	      true
	    else
	      let rec restscan = function
		  h::t -> 
		    if h#data#is_boundary then 
		      true
		    else
		      restscan t
		| [] -> false
	      in
	      restscan rest
      in
      scan false (ands1, ands2)


    method get_nearest_boundary nd = 
      let ancs = List.rev (self#initial_ancestor_nodes nd) in
      if nd#data#is_boundary then 
	nd
      else
	try
	  let rec scan = function
	      h::t -> if h#data#is_boundary then h else scan t
	    | [] -> raise Not_found
	  in
	  scan ancs
	with Not_found -> 
	  WARN_MSG "boundary not found in [%s] node=\"%s\"" 
	    (Xlist.to_string (fun x -> x#data#to_string) "; " ancs) nd#data#to_string;
	  raise Not_found
	    
	    

    method nearest_common_ancestor ?(closed=false) nds =

      let get_ancestors n =
	let base = self#initial_ancestor_nodes n in
	if closed then
	  base @ [n]
	else
	  base
      in

      let rec intersect a = function
	  h1::t1, h2::t2 -> 
	    if h1 == h2 then 
	      intersect (h1::a) (t1, t2) 
	    else
	      List.rev a
	| _, [] | [], _ -> List.rev a
      in
      let common = ref [] in
      begin
	match nds with
	  h::t -> 
	    common := (get_ancestors h);
	    List.iter
	      (fun nd ->
		common := intersect [] (!common, get_ancestors nd)
	      ) t
	| [] -> ()
      end;
      try
	Xlist.last !common, List.length !common
      with 
	Failure _ ->
	  BEGIN_DEBUG
            let nds_to_str nds = (String.concat ", " (List.map (fun n -> GI.to_string n#gindex) nds)) in
	    DEBUG_MSG "nearest_common_ancestor of [%s]\n" (nds_to_str nds);
	    List.iter
	      (fun n ->
		DEBUG_MSG "  initial ancestor of %a: [%s]\n" 
		  GI.ps n#gindex (nds_to_str (self#initial_ancestor_nodes n))
	      ) nds
	  END_DEBUG;
	  raise (Failure "Sourcecode.Tree.c#nearest_common_ancestor")
	    


    method private _get_origin row revidx =
      let get i =
	try
	  List.nth row i
	with
	  Failure _ ->
	    ERROR_MSG "_get_origin.get: index out of bounds: i=%d row=%s" 
	      i (Xlist.to_string (fun x -> x) ", " row);
	    exit 1
      in
      let origin = get 1 in
      let ending = get 2 in
      if (int_of_string origin) <= revidx && revidx <= (int_of_string ending) then
	let len = List.length row in
	let gid = ref (int_of_string(get 0)) in (* initial gid *)
	let n = len - 2 in
	for i = 10 to n do
	  if i mod 2 = 0 then
	    let ri = int_of_string(get i) in
	    let gi = int_of_string(get (i + 1)) in
	    if revidx >= ri then
	      gid := gi
	done;
	(!gid, origin, ending)
      else
	raise Not_found


    method dump_origin bufsize nctms_file revidx origin_file ending_file =
      DEBUG_MSG "bufsize=%d" bufsize;

      let revidx_s = string_of_int revidx in

      let csv = Csv.load nctms_file in
      let csv = 
	match csv with
	  [] -> []
	| _::tl -> tl
      in
      List.iter
	(fun row ->
	  let row_s = (Xlist.to_string (fun x -> x) ", " row) in
	  DEBUG_MSG "dump_origin: row=(%s)" row_s;
	  try
	    let (gid, origin, ending) = self#_get_origin row revidx in
	    let nd = self#search_node_by_gindex gid in
	    nd#data#set_origin origin;
	    nd#data#set_ending ending
	  with 
	  | Not_found -> ()
	  | Failure _ -> raise (Malformed_row row_s)
	) csv;

      let buf = new Origin.abuf bufsize in
      let buf_ending = new Origin.abuf bufsize in

      let nunknown_origin = ref 0 in
      let nunknown_ending = ref 0 in

      let nds_tbl = Hashtbl.create 0 in
      let nds_tbl_ending = Hashtbl.create 0 in

      self#preorder_scan_whole_initial
	(fun nd ->
	  if nd != self#root then begin

	    let ndat = nd#data in

	    if ndat#origin = unknown_origin then begin
	      incr nunknown_origin;
	      ndat#set_origin revidx_s
	    end;
	    if ndat#ending = unknown_origin then begin
	      incr nunknown_ending;
	      ndat#set_ending revidx_s
	    end;

	    let ogn = ndat#origin in
	    let edg = ndat#ending in

	    let latest_ending = options#latest_target in

	    let ogn_cond = revidx_s = ogn && (ogn <> edg || ogn = latest_ending) in
	    let edg_cond = revidx_s = edg && (ogn <> edg || edg = earliest_origin) in

	    if ogn_cond || edg_cond then begin
	      let ancestors = (* rightmost is the parent *)
		(self#initial_ancestor_nodes nd) 
	      in
	      let ancestors = (* remove the root *)
		if List.length ancestors > 0 then List.tl ancestors else []
	      in
	      if ogn_cond then begin
		try
		  List.iter 
		    (fun a ->
		      if a#data#origin = ogn 
		      then begin
			try
			  let nds = Hashtbl.find nds_tbl a in
			  Hashtbl.replace nds_tbl a (nd::nds);
			  raise Found
			with 
			  Not_found -> 
			    Hashtbl.add nds_tbl a [nd; a];
			    raise Found
		      end
		    ) ancestors;
		  Hashtbl.add nds_tbl nd [nd]
		with
		  Found -> ()
	      end;
	      if edg_cond then begin
		try
		  List.iter 
		    (fun a ->
		      if a#data#ending = edg 
		      then begin
			try
			  let nds = Hashtbl.find nds_tbl_ending a in
			  Hashtbl.replace nds_tbl_ending a (nd::nds);
			  raise Found
			with 
			  Not_found -> 
			    Hashtbl.add nds_tbl_ending a [nd; a];
			    raise Found
		      end
		    ) ancestors;
		  Hashtbl.add nds_tbl_ending nd [nd]
		with
		  Found -> ()
	      end
	    end;
	    
	    let loc = ndat#src_loc in
	    let st = loc.Loc.start_offset in
	    let ed = loc.Loc.end_offset in

	    if st >= 0 && ed >= 0 then begin
	      let range = Origin.create_range (int_of_origin ogn) st ed in
	      buf#put range;
	      let range_ending = Origin.create_range (int_of_origin edg) st ed in
	      buf_ending#put range_ending
	    end
	  end
	);

      let sz = self#size in
      let nknown_origin = sz - !nunknown_origin in
      let cov = float(nknown_origin) *. 100.0 /. float(sz) in
      let nknown_ending = sz - !nunknown_ending in
      let cov_ending = float(nknown_ending) *. 100.0 /. float(sz) in

      Xprint.message "coverage (origin): %d/%d (%f%%)" nknown_origin sz cov;
      Hashtbl.iter
	(fun nd nds -> 
	  Xprint.message "size of fragment: (gid:%a, origin:%s) -> %d" 
	    GI.ps nd#gindex nd#data#origin (List.length nds)
	) nds_tbl;

      buf#dump origin_file;


      Xprint.message "coverage (ending): %d/%d (%f%%)" nknown_ending sz cov_ending;
      Hashtbl.iter
	(fun nd nds -> 
	  Xprint.message "size of fragment: (gid:%a, ending:%s) -> %d" 
	    GI.ps nd#gindex nd#data#ending (List.length nds)
	) nds_tbl_ending;

      buf_ending#dump ending_file;


      (sz, nknown_origin, cov, nds_tbl, nknown_ending, cov_ending, nds_tbl_ending)

      (* end of method dump_origin *)




    method align_fragments (gmap : (int * int) list) (tree : 'self) =

      let gmap_tbl = Hashtbl.create (List.length gmap) in
      List.iter (fun (i, j) -> Hashtbl.replace gmap_tbl i j) gmap;

      let nds_tbl1 = Hashtbl.create 0 in
      let nds_tbl2 = Hashtbl.create 0 in

      let check_mapping nd' a =
	try
	  let ai' = Hashtbl.find gmap_tbl a#gindex in
	  let a' = tree#search_node_by_gindex ai' in

	  let b = List.memq a' (tree#initial_ancestor_nodes nd') in
	  if b then begin
	    try
	      let nds' = Hashtbl.find nds_tbl2 a' in
	      Hashtbl.replace nds_tbl2 a' (nd'::nds')
	    with Not_found -> 
	      Hashtbl.add nds_tbl2 a' [nd'; a']
	  end;
	  b

	with 
          Not_found -> false
      in

      self#preorder_scan_whole_initial
	(fun nd ->
	  if nd != self#root then begin
	    try
	      let ni' = Hashtbl.find gmap_tbl nd#gindex in
	      let nd' = tree#search_node_by_gindex ni' in

	      let ancestors = (* rightmost is the parent *)
		(self#initial_ancestor_nodes nd) 
	      in
	      let ancestors = (* remove the root *)
		if List.length ancestors > 0 then List.tl ancestors else []
	      in
	      begin
		try
		  List.iter 
		    (fun a ->
		      if check_mapping nd' a
		      then begin
			try
			  let nds = Hashtbl.find nds_tbl1 a in
			  Hashtbl.replace nds_tbl1 a (nd::nds);
			  raise Found
			with 
			  Not_found -> 
			    Hashtbl.add nds_tbl1 a [nd; a];
			    raise Found
		      end
		    ) ancestors;
		  Hashtbl.add nds_tbl1 nd [nd];
		  Hashtbl.add nds_tbl2 nd' [nd'];
		with
		  Found -> ()
	      end
	    with Not_found -> ()
	  end
	);

      Hashtbl.iter 
	(fun nd nds -> 
	  try
	    let ni' = Hashtbl.find gmap_tbl nd#gindex in
	    let nd' = tree#search_node_by_gindex ni' in
	    let nds' = Hashtbl.find nds_tbl2 nd' in
	    let loc = nd#data#src_loc in
	    let loc' = nd'#data#src_loc in
	    Printf.printf "%a <%s> (%s) (%d lines %d nodes) --- %a <%s> (%s) (%d lines %d nodes)\n"
	      GI.p nd#gindex nd#data#label (Loc.to_string loc) (Loc.lines loc) (List.length nds)
	      GI.p nd'#gindex nd'#data#label (Loc.to_string loc') (Loc.lines loc') (List.length nds')

	  with Not_found -> ()
	      
	) nds_tbl1

    method find_nodes_by_line_range (start_line, end_line) =
      let res = ref [] in
      self#preorder_scan_whole_initial
	(fun nd ->
	  let loc = nd#data#src_loc in
	  if start_line <= loc.Loc.start_line && loc.Loc.end_line <= end_line then
	    res := nd::!res
	);
      if !res = [] then
	WARN_MSG "no nodes found: %d-%d" start_line end_line;
      
      !res

    method find_nodes_by_line_col_range ((start_line, start_col), (end_line, end_col)) =
      let res = ref [] in
      self#preorder_scan_whole_initial
	(fun nd ->
	  let loc = nd#data#src_loc in
	  if 
	    ( (start_line = loc.Loc.start_line && start_col <= loc.Loc.start_char) 
	    || start_line < loc.Loc.start_line ) 
	      && 
	    ( (loc.Loc.end_line = end_line && loc.Loc.end_char <= end_col)
	    || loc.Loc.end_line < end_line )
	  then
	    res := nd::!res
	);
      if !res = [] then
	WARN_MSG "no nodes found: %d-%d" start_line end_line;
      
      !res

	
    (* for searchast *)

    val mutable token_array = [||]
    val mutable node_array = [||]

    method find_token_node i =
      try
	node_array.(i)
      with 
	Invalid_argument _ -> 
	  WARN_MSG "not found \"%d\"" i;
	  raise Not_found

    method to_token_array =
      if token_array = [||] then
	let l = ref [] in
	let ndl = ref [] in
	self#preorder_scan_whole_initial
	  (fun nd ->
	    if nd#data#not_frommacro then begin
	      l := nd#data#to_short_string :: !l;
	      ndl := nd :: !ndl
	    end
	  );
	let a = Array.of_list(List.rev !l) in
	let nda = Array.of_list(List.rev !ndl) in
	token_array <- a;
	node_array <- nda;
	a
      else
	token_array

    val mutable node_array_pat = [||]

    method find_token_node_pat i =
      try
	node_array_pat.(i)
      with 
	Invalid_argument _ -> 
	  WARN_MSG "not found \"%d\"" i;
	  raise Not_found

    method get_token_array_pat (frag : GIDfragment.c) =
      let l = ref [] in
      let ndl = ref [] in
      self#preorder_scan_whole_initial
	(fun nd ->
	  if frag#contains nd#gindex && nd#data#not_frommacro then begin
	    l := nd#data#to_short_string :: !l;
	    ndl := nd :: !ndl
	  end
	);
      node_array_pat <- Array.of_list(List.rev !ndl);
      Array.of_list(List.rev !l)


    method private compute_continuity matched = (* assumes sorted *)
      let total_gap = 
	let rec scan a = function
	    i0::i1::t -> scan ((i1 - i0 - 1) + a) (i1::t)
	  | [_] -> a
	  | [] -> 0
	in
	scan 0 matched
      in
      let range = 
	match matched with
	  i0::i1::t -> (Xlist.last (i1::t)) - i0 + 1
	| [_] -> 1
	| [] -> 0
      in

      let continuity = (float (range - total_gap)) /. (float range) in

      DEBUG_MSG "continuity=%f (tgap=%d,range=%d)\n" continuity total_gap range;

      continuity


    method match_token_array_pat_ch cache_path frag_src pat gindextok_array ch =
      let re = Str.regexp "^\\([0-9]+\\):\\(.+\\)" in
      let gid_array = Array.make (Array.length gindextok_array) (-1) in
      let tokpat = 
	Array.mapi 
	  (fun i gidtok -> 
	    if Str.string_match re gidtok 0 then
	      let gid_s = Str.matched_group 1 gidtok in
	      let tok = Str.matched_group 2 gidtok in
	      gid_array.(i) <- int_of_string gid_s;
	      tok
	    else begin
	      ERROR_MSG "illegal token cache format: \"%s\"" gidtok;
	      exit 1
	    end

	  ) gindextok_array
      in
      let getgid i = gid_array.(i) in
      let patfrag = new GIDfragment.c in
      let _ = patfrag#set_rep pat in
      let pathash = (encoded_digest_of_file options frag_src) ^ ":" ^ patfrag#hash in
      let gmap_path = mkgmapfilepath ~gmap_ext:options#gmap_ext cache_path patfrag in
      try
	self#_match_token_array_pat_ch tokpat pathash getgid gmap_path ch
      with 
	exn -> 
	  WARN_MSG "caught \"%s\"" (Printexc.to_string exn)


    method show_node_array_pat = (* for debug *)
      Printf.printf "node_array_pat:\n";
      Array.iter
	(fun nd -> 
	  Printf.printf "%a [%s] (%s)\n" GI.p nd#gindex nd#data#label (Loc.to_string nd#data#src_loc)
	) node_array_pat;


    method match_pat_ch cache_path (pat_tree : 'self) tokpat patfrag ch =

      BEGIN_DEBUG
	Printf.printf "PAT:\n";
	pat_tree#show_node_array_pat
      END_DEBUG;

      let getgid i = (pat_tree#find_token_node_pat i)#gindex in
      let gmap_path = mkgmapfilepath ~gmap_ext:options#gmap_ext cache_path patfrag in
      let pathash = pat_tree#encoded_source_digest ^ ":" ^ patfrag#hash in
      try
	self#_match_token_array_pat_ch tokpat pathash getgid gmap_path ch
      with 
	exn -> 
	  WARN_MSG "caught \"%s\"" (Printexc.to_string exn)	  
	  


    method private _match_token_array_pat_ch tokpat pathash getgid gmap_path ch =
      let tokpat_len = Array.length tokpat in
      let a = self#to_token_array in
(*      let n, st_pos, ed_pos, _, _ = LCS.lcs a tokpat in *)
      let exact_matches, relabels, _, _ = Adiff.adiff a tokpat in
      let matches = exact_matches in
(*      let matches = exact_matches @ relabels in *)

      let matched, _ = List.split matches in
      let relabeled, _ = List.split relabels in

      let matched = List.fast_sort Stdlib.compare matched in

      let gaps = 
	let rec loop a = function
	  | i0::i1::t -> loop ((i1 - i0 - 1)::a) (i1::t)
	  | [i] -> List.rev a
	  | [] -> []
	in
	loop [] matched
      in

      let matched_nds = List.map self#find_token_node matched in
      let relabeled_nds = List.map self#find_token_node relabeled in

      let gap_tbl = Hashtbl.create 0 in
      let index_tbl = Hashtbl.create 0 in
      let count = ref 0 in
      List.iter 
	(fun nd -> 
	  try
	    Hashtbl.replace index_tbl nd (List.nth matched !count); 
	    Hashtbl.replace gap_tbl nd (List.nth gaps !count); 
	    incr count
	  with _ -> ()
	) matched_nds;

      BEGIN_DEBUG
	DEBUG_MSG "%d matched nodes (gindex):\n" (List.length matched_nds);
	List.iter 
	  (fun n -> 
	    DEBUG_MSG "%a [%s](%s)\n" GI.ps n#gindex n#data#label (Loc.to_string n#data#src_loc)
	  ) matched_nds
      END_DEBUG;


      let scan_matched m = (* compute size-threshold ratio (STR) *)

	let rec scan segs cur_seg = function
	    n0::(n1::t as rest) -> begin
	      if self#cross_boundary n0 n1 then
		scan ((n0::cur_seg)::segs) [] rest
	      else
		scan segs (n0::cur_seg) rest
	    end
	  | [n] -> (List.rev (List.map List.rev ((n::cur_seg)::segs))) 
	  | [] -> []
	in
	let segs = scan [] [] m in
(*	let nsegs = List.length segs in*)
	let sizes = List.map (fun seg -> List.length seg) segs in
	let locs = List.map merge_locs segs in
(*
  let sum_size =
  List.fold_left
  (fun s sz ->
  s + sz
  ) 0 sizes
  in
 *)
	let rels = List.map (fun seg -> List.filter (fun n -> List.memq n relabeled_nds) seg) segs in
(*
  let ave_size = (float sum_size) /. (float nsegs) in
 *)
	let ranges = 
	  let get_range seg =
	    let last = List.hd (List.rev seg) in
	    let first = List.hd seg in
	    try
	      (Hashtbl.find index_tbl last) - (Hashtbl.find index_tbl first) + 1
	    with 
	      Not_found -> 
		ERROR_MSG "not found: gid:%a or gid:%a" GI.ps first#gindex GI.ps last#gindex;
		exit 1
	  in
	  List.map get_range segs
	in
	let gap_sizes = 
	  List.map 
	    (fun seg -> 
	      List.fold_left 
		(fun s n -> 
		  try
		    s + try Hashtbl.find gap_tbl n with _ -> 0
		  with 
		    Not_found -> 
		      ERROR_MSG "not found: \"%s\"" n#to_string;
		      exit 1
		) 0 (List.rev(List.tl(List.rev seg)))
		
	    ) segs
	in
	let rates = 
	  List.map2 
	    (fun sz (g, r) -> 
	      (float sz) /. (float options#size_threshold) *. (float (r - g)) /. (float r)
	    ) sizes (List.combine gap_sizes ranges) in

	let rec combine4 = function
	    h1::t1, h2::t2, h3::t3, h4::t4 -> (h1, h2, h3, h4)::combine4(t1, t2, t3, t4)
	  | [], [], [], [] -> []
	  | _ -> raise (Invalid_argument "combine4")
	in

	BEGIN_DEBUG
	  for i = 0 to (List.length segs) - 1 do
	    DEBUG_MSG "size=%d(nrels=%d,tgap=%d,range=%d) [%f] %s\n" 
	      (List.nth sizes i) 
	      (List.length (List.nth rels i))
	      (List.nth gap_sizes i)
	      (List.nth ranges i)
	      (List.nth rates i) 
	      (Loc.to_string (List.nth locs i))
	  done
	END_DEBUG;

	let final_rates         = ref [] in
	let final_locs          = ref [] in
	let final_matched_nds   = ref [] in
	let final_relabeled_nds = ref [] in
	List.iter 
	  (fun (seg, rate, loc, rel) -> 
	    if rate >= options#str_threshold then begin
	      final_matched_nds   := !final_matched_nds @ seg;
	      final_rates         := rate::!final_rates;
	      final_locs          := loc::!final_locs;
	      final_relabeled_nds := !final_relabeled_nds @ rel
	    end
	  ) (combine4(segs, rates, locs, rels));

	let nmatches = List.length !final_matched_nds in
	let nrelabels = List.length !final_relabeled_nds in

	let sim = (float nmatches) /. (float tokpat_len) in

	let str = 
	  let sum = List.fold_left (fun s r -> s +. r) 0.0 !final_rates in
	  if sum > 0.0 then
	    sum /. (float (List.length !final_rates)) 
	  else
	    0.0
	in

	nmatches, 
	nrelabels,
	sim, 
	str, 
	(String.concat "|" (List.map Loc.to_string !final_locs)),
	!final_matched_nds,
	!final_relabeled_nds

      in (* end of func scan_matched *)

      let nmats, nrels, sim, str, loc, matched_nodes, relabeled_nodes =
	if (List.length matched) = 0 then 
	  0, 0, 0.0, 0.0, "???", [], []
	else
	  scan_matched matched_nds
      in

      DEBUG_MSG "loc=%s" loc;

      let emr = (float nmats) /. (float (nmats + nrels)) in (* EMR: exact match - match ratio *)

      let renamed_nodes = List.filter (fun nd -> nd#data#is_named) relabeled_nodes in
      let nrenamed = List.length renamed_nodes in
      let wemr = (float nmats) /. (float (nmats + nrenamed)) in (* WEMR: weak EMR *)

(*
  Printf.fprintf ch "similarity: (%d/%d)=%f relabels:%d STR:%f EMR:%f WEMR:%f loc:%s pathash:%s\n" 
  nmats tokpat_len sim nrels str emr wemr loc pathash;
 *)

      (* secondary matching *)
      if nmats > 0 then begin
	let ca, d = self#nearest_common_ancestor matched_nodes in

	let nearest_boundary_opt =
	  try
	    Some (self#get_nearest_boundary ca)
	  with Not_found -> None
	in

	if ca != self#root && nearest_boundary_opt <> None then begin

	  let nearest_boundary = 
	    match nearest_boundary_opt with Some nb -> nb | None -> assert false 
	  in

	  DEBUG_MSG "%s (%s)\n" 
	    nearest_boundary#data#label 
	    (Loc.to_string nearest_boundary#data#src_loc);

	  let src_frag = new GIDfragment.c in
	  let rep = 
	    sprintf "%a-%a" 
	      GI.rs (self#initial_leftmost nearest_boundary)#gindex GI.rs nearest_boundary#gindex
	  in
	  src_frag#set_rep rep;
	  let src_token_array = self#get_token_array_pat src_frag in
	  let exact_matches2, relabels2, _, _ = Adiff.adiff src_token_array tokpat in
	  let matches2 = exact_matches2 @ relabels2 in

	  let nmats2 = List.length matches2 in
	  let sim2 = (float nmats2) /. (float tokpat_len) in
	  let nrels2 = List.length relabels2 in
	  let matched2, _ = List.split matches2 in
	  let matched2 = List.fast_sort Stdlib.compare matched2 in

	  let continuity = self#compute_continuity matched2 in

	  let matched_nds2 = List.map self#find_token_node_pat matched2 in
	  let loc2 = merge_locs matched_nds2 in

	  DEBUG_MSG "loc2=%s" (Loc.to_string loc2);

	  let relabeled2, _ = List.split relabels2 in
	  let relabeled_nds2 = List.map self#find_token_node_pat relabeled2 in
	  let renamed_nds2 = List.filter (fun nd -> nd#data#is_named) relabeled_nds2 in
	  let nrenamed2 = List.length renamed_nds2 in

	  BEGIN_DEBUG
	    DEBUG_MSG "%d matched nodes (2) (gindex):\n" (List.length matched_nds2);
	    List.iter 
	      (fun n -> 
		DEBUG_MSG "%a [%s](%s)\n" GI.ps n#gindex n#data#label (Loc.to_string n#data#src_loc)
	      ) matched_nds2;
	    DEBUG_MSG "%d relabeled nodes (2) (gindex):\n" (List.length relabeled_nds2);
	    List.iter 
	      (fun n -> 
		Printf.printf "%a [%s](%s)\n" GI.p n#gindex n#data#label (Loc.to_string n#data#src_loc)
	      ) relabeled_nds2
	  END_DEBUG;

	  let emr2 = (float nmats2) /. (float (nmats2 + nrels2)) in
	  let wemr2 = (float nmats2) /. (float (nmats2 + nrenamed2)) in
	  let str2 = (float nmats2) /. (float options#size_threshold) in

	  if (List.length exact_matches2) > nmats && continuity >= options#continuity_threshold then begin
	    let _ = 
	      if sim2 >= options#sim_threshold then
		let gmap = 
		  List.map 
		    (fun (i, j) -> 
		      ((self#find_token_node_pat i)#gindex, (getgid j))
		    ) matches2
		in
		dump_gmap gmap gmap_path
	    in
	    Printf.fprintf ch "similarity: (%d/%d)=%f relabels:%d STR:%f EMR:%f WEMR:%f loc:%s pathash:%s\n" 
	      nmats2 tokpat_len sim2 nrels2 str2 emr2 wemr2 (Loc.to_string loc2) pathash;    
	  end
	  else
	    Printf.fprintf ch "similarity: (0/%d)=0.0 relabels:%d STR:0.0 EMR:0.0 WEMR:0.0 loc:WITHDRAWN pathash:%s\n" 
	      tokpat_len nrels2 pathash

	end
	else
	  let matched =
	    List.fast_sort Stdlib.compare (List.map (fun n -> Hashtbl.find index_tbl n) matched_nodes)
	  in
	  let c = self#compute_continuity matched in
	  if c >= options#continuity_threshold then begin
	    let _ = 
	      if sim >= options#sim_threshold then
		let matches = 
		  let ms = List.map (fun n -> Hashtbl.find index_tbl n) matched_nodes in
		  List.filter (fun (i, _) -> List.mem i ms) matches
		in
		let gmap = 
		  List.map 
		    (fun (i, j) -> 
		      ((self#find_token_node i)#gindex, (getgid j))
		    ) matches
		in
		dump_gmap gmap gmap_path
	    in
	    Printf.fprintf ch "similarity: (%d/%d)=%f relabels:%d STR:%f EMR:%f WEMR:%f loc:%s pathash:%s\n" 
	      nmats tokpat_len sim nrels str emr wemr loc pathash
	  end
	  else
	    Printf.fprintf ch "similarity: (0/%d)=0.0 relabels:%d STR:0.0 EMR:0.0 WEMR:0.0 loc:WITHDRAWN pathash:%s\n" 
	      tokpat_len nrels pathash

      end
      else
	let loc = if (List.length matches) > 0 then "CUTOFF" else "-" in
	Printf.fprintf ch "similarity: (0/%d)=0.0 relabels:%d STR:0.0 EMR:0.0 WEMR:0.0 loc:%s pathash:%s\n" 
	  tokpat_len nrels loc pathash
	  (* end of method _match_token_array_pat_ch *)

    method match_pats cache_path ofile pat_tree patfrags =
      Xfile.dump ofile 
	(fun ch -> 
	  List.iter 
	    (fun p -> 
	      let tokpat = pat_tree#get_token_array_pat p in
	      self#match_pat_ch cache_path pat_tree tokpat p ch
	    ) patfrags)

    method find_label (root : 'node) (nd : 'node) =
      let labs = ref [] in
      let lab = get_lab nd in
      self#scan_whole_initial_subtree root
	(fun n ->
	  if lab = get_lab n then
	    labs := n :: !labs
	);
      !labs

    method dump_subtree_for_delta_ch
        (root : node_t) 
        (except : node_t list) 
        (ch : Xchannel.out_channel)
        =
      let fprintf = Xchannel.fprintf in
      let attrs_to_string attrs =
        String.concat "" 
          (List.map 
             (fun (a, v) ->
               sprintf " %s=\"%s\"" a v
             ) attrs)
      in
      let rec doit nd =
        if not (List.memq nd except) then
          let name, attrs, _ = nd#data#orig_to_elem_data_for_delta in
          if nd#is_leaf then begin
	    fprintf ch "<%s%s/>" name (attrs_to_string attrs)
          end
          else begin
	    fprintf ch "<%s%s>" name (attrs_to_string attrs);
            Array.iter doit nd#initial_children;
            fprintf ch "</%s>" name
          end
      in
      doit root


  end (* of class Sourcecode.Tree.c *)


  module PxpD = Pxp_document

  exception Ignore


  let attrs_of_anodes anodes =
    List.fold_left 
      (fun l anode ->
        match anode#node_type with
	| PxpD.T_attribute name -> l @ [name, anode#data]
        | _ -> assert false
      ) [] anodes

  let xnode_to_string = Delta_base.xnode_to_string

  let of_xnode 
      ?(tree_creator=fun options nd -> new c options nd true) 
      (options : #Parser_options.c)
      (xnode : SB.xnode_t) 
      =
    let rec scan_xnode xnode =
      match xnode#node_type with
      | PxpD.T_element name ->
	  if name = Delta_base.text_tag then begin
            failwith (sprintf "illegal node: %s" (xnode_to_string xnode))
          end
	  else begin
	    let children = scan_xnodes xnode#sub_nodes in

	    let anodes = xnode#attributes_as_nodes in
	    let attrs = attrs_of_anodes anodes in
(*
  let nchildren = List.length children in
  if nchildren > 0 then L.add_collapse_target name;
 *)
	    let nd = mknode options (of_elem_data name attrs) children in
	    nd
	  end

      | t -> begin
	  Xprint.warning "ignored: %s" (XML.node_type_to_string xnode#node_type);
	  raise Ignore
      end

    and scan_xnodes xnodes =
      List.fold_right 
        (fun n l -> try (scan_xnode n)::l with Ignore -> l) xnodes []
    in
    let nd = scan_xnode xnode in
    let tree = tree_creator options nd in
    tree
    (* end of func Sourcecode.Tree.node_of_xnode *)



end (* of functor Sourcecode.Tree *)




let find_nearest_p_ancestor_node pred nd =
  let rec scan n =
    try
      let pn = n#initial_parent in
      if pred pn then
	pn
      else
	scan pn
    with
      Otreediff.Otree.Parent_not_found _ -> raise Not_found
  in
  let a = scan nd in
  DEBUG_MSG "%a --> %a" UID.ps nd#uid UID.ps a#uid;
  a

let find_nearest_mapped_ancestor_node is_mapped nd =
  let rec scan n =
    try
      let pn = n#initial_parent in
      if is_mapped pn#uid then
	pn
      else
	scan pn
    with 
      Otreediff.Otree.Parent_not_found _ -> raise Not_found
  in
  let a = scan nd in
  DEBUG_MSG "%a --> %a" UID.ps nd#uid UID.ps a#uid;
  a

let find_nearest_mapped_descendant_nodes is_mapped node =
  let rec get nd =
    List.flatten
      (List.map 
	 (fun n ->
	   if is_mapped n#uid then 
	     [n]
	   else 
	     get n
	 ) (Array.to_list nd#initial_children))
  in
  get node



type frame = 
    { f_scope_node : Spec.node_t;
      f_table      : (name, Spec.node_t) Hashtbl.t;
    }

exception Found of Spec.node_t

class stack = object (self)
  val _global_tbl = Hashtbl.create 0

  val _stack = Stack.create()

  method push nd (* scope creating node *) =
    let frm = { f_scope_node=nd; f_table = Hashtbl.create 0 } in
    Stack.push frm _stack

  method pop = ignore (Stack.pop _stack)

  method register_global name decl_node =
    DEBUG_MSG "registering global \"%s\"" name;
    Hashtbl.replace _global_tbl name decl_node

  method register name decl_node =
    DEBUG_MSG "registering \"%s\"" name;
    let frm = Stack.top _stack in
    Hashtbl.replace frm.f_table name decl_node

  method lookup name =
    try
      Stack.iter
	(fun frm ->
	  if Hashtbl.mem frm.f_table name then
	    raise (Found (Hashtbl.find frm.f_table name))
	) _stack;
      Hashtbl.find _global_tbl name
    with
      Found n -> n
	  
end (* of class Tree.stack *)


class visitor tree = object (self)

  method scanner_body_before_subscan (nd : Spec.node_t) = ()
  method scanner_body_after_subscan (nd : Spec.node_t) = ()

  method scan nd =
    self#scanner_body_before_subscan nd;
    Array.iter self#scan nd#initial_children;
    self#scanner_body_after_subscan nd

  method visit_all =
    self#scan tree#root
end
