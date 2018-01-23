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
(* 
 * AST for the Java Language (for otreediff) 
 *
 * java/tree.ml
 *
 *)


module P   = Printer
module L   = Java_label
module BID = Binding.ID
module FB  = Fact_base.F (L)

let sprintf = Printf.sprintf

let qualifier_of_name n =
  match n.Ast.n_desc with
  | Ast.Nsimple(_, ident) -> None
  | Ast.Nqualified(_, name, _) -> Some name

let conv_loc = L.conv_loc

let loc_of_name n = conv_loc n.Ast.n_loc

let set_loc nd loc = nd#data#set_loc (conv_loc loc)

let rec set_ghost_rec nd =
  nd#data#set_loc Loc.ghost;
  Array.iter set_ghost_rec nd#children

let is_ghost = Triple.is_ghost_ast_node

let getlab = FB.getlab
let get_orig_lab_opt nd =
  match nd#data#orig_lab_opt with
  | Some o -> Some (Obj.obj o : L.t)
  | None -> None

let get_surrounding_classes_or_interfaces nd =
  FB.get_surrounding_xxxs (fun l -> L.is_class l || L.is_interface l || L.is_enum l) nd

let get_fqn package_name nd lab =
  let name =
    Xlist.last (String.split_on_char '.' (L.get_name lab))
  in
  let get_name n = L.get_name (getlab n) in
  let pkg_prefix = 
    if package_name = "" then 
      "" 
    else 
      package_name^"."
  in
  let sep =
    if L.is_class lab || L.is_interface lab || L.is_enum lab then
      "$"
    else
      "."
  in
  let surrounding =
    String.concat "$"
      (List.map get_name (get_surrounding_classes_or_interfaces nd))
  in
  let surrounding_prefix =
    if surrounding = "" then
      ""
    else
      surrounding^sep
  in
  let fqn =
    pkg_prefix^
    surrounding_prefix^
    (*if L.is_ctor lab then
      surrounding
    else*)
      name
  in
  fqn


module Tree = Sourcecode.Tree (L)

open Tree

let get_annotation = get_annotation

class c options root is_whole = object
  inherit Tree.c options root is_whole

  method private create root is_whole = new c options root is_whole

  method unparse_subtree_ch = make_unparser Java_unparsing.unparse

end

let of_xnode options = 
  Tree.of_xnode ~tree_creator:(fun options nd -> new c options nd true) options
    


let of_opt of_x x_opt = match x_opt with None -> [] | Some x -> [of_x x]

let set_nodes_loc nd nodes =
  match nodes with
  | [] -> ()
  | [n] -> nd#data#set_loc n#data#src_loc
  | n::rest ->
      let loc = Loc._merge n#data#src_loc (List.hd(List.rev rest))#data#src_loc in
      nd#data#set_loc loc

let apply_child is_xxx f children =
  Array.iter
    (fun nd ->
      let lab = getlab nd in
      if is_xxx lab then
        f nd
    ) children


let vdid_to_id vdid = 
  try
    String.sub vdid 0 (String.index vdid '[')
  with
    Not_found -> vdid


let set_control_flow body =

  let find_target env x = List.assoc x env in
  let find_break_target = function
    | (_, x) :: _ -> x
    | [] -> raise Not_found
  in
  let rec find_continue_target = function
    | (Some x, _) :: _ -> x
    | (None, _) :: rest -> find_continue_target rest
    | [] -> raise Not_found
  in
  let rec set_succ label_env loop_env nexts nd =
    let ndlab = getlab nd in

    let add_succ1 s =
      DEBUG_MSG "%s[%s] -> %s[%s]"
        (L.to_string ndlab) (Loc.to_string nd#data#src_loc)
        (L.to_string (getlab s)) (Loc.to_string s#data#src_loc);
      nd#data#add_successor s
    in
    let add_succ = List.iter add_succ1 in
  
    let children = nd#initial_children in
    let nchildren = nd#initial_nchildren in

    let handle_block children nchildren =
      DEBUG_MSG "nchidlen=%d" nchildren;
      if nchildren = 0 then
        add_succ nexts
      else if nchildren > 0 then begin
        add_succ1 children.(0);
        let lasti = nchildren - 1 in
        for i = 0 to lasti - 1 do
          DEBUG_MSG "i=%d" i;
          set_succ label_env loop_env [children.(i+1)] children.(i)
        done;
        set_succ label_env loop_env nexts children.(lasti)
      end
    in
    match ndlab with
    | L.Statement stmt -> begin
        match stmt with
        | L.Statement.If -> begin
            let c1 = children.(1) in
            add_succ1 c1;
            set_succ label_env loop_env nexts c1;
            try
              let c2 = children.(2) in
              add_succ1 c2;
              set_succ label_env loop_env nexts c2
            with
            | _ -> add_succ nexts
        end
        | L.Statement.Switch -> begin
            Array.iter
              (fun n ->
                add_succ1 n;
                set_succ label_env ((None, nexts)::loop_env) nexts n
              ) (children.(1))#initial_children
        end
        | L.Statement.For -> begin
            let c3 = (Tree.get_logical_nth_child nd 3).(0) in
            add_succ1 c3;
            set_succ label_env ((Some c3, c3::nexts)::loop_env) nexts c3
        end
        | L.Statement.ForEnhanced -> begin
            let c2 = children.(2) in
            add_succ1 c2;
            set_succ label_env ((Some c2, c2::nexts)::loop_env) nexts c2
        end
        | L.Statement.While -> begin
            let c1 = children.(1) in
            add_succ1 c1;
            set_succ label_env ((Some c1, c1::nexts)::loop_env) nexts c1
        end
        | L.Statement.Do -> begin
            let c0 = children.(0) in
            add_succ1 c0;
            set_succ label_env ((Some c0, c0::nexts)::loop_env) nexts c0
        end
        | L.Statement.Break lab_opt -> begin
            try
              let ns =
                match lab_opt with
                | Some lab -> find_target label_env lab
                | None     -> find_break_target loop_env
              in
              add_succ ns
            with
              _ -> ()
        end
        | L.Statement.Continue lab_opt -> begin
            try
              let ns =
                match lab_opt with
                | Some lab -> find_target label_env lab
                | None     -> [find_continue_target loop_env]
              in
              add_succ ns
            with
              _ -> ()
        end
        | L.Statement.Labeled lab -> begin
            let c0 = children.(0) in
            add_succ1 c0;
            set_succ ((lab, nexts)::label_env) loop_env nexts c0
        end
        | L.Statement.Synchronized -> begin
            let c1 = children.(1) in
            add_succ1 c1;
            set_succ label_env loop_env nexts c1
        end
        | L.Statement.Try -> begin
            apply_child L.is_block
              (fun c ->
                add_succ1 c;
                set_succ label_env loop_env nexts c
              ) children
        end
        | L.Statement.Throw -> begin
        end

        | L.Statement.Return -> begin
        end

        | _ -> begin
            add_succ nexts
        end
    end
    | L.Block -> begin
        handle_block children nchildren
    end
    | L.SwitchBlockStatementGroup -> begin
        let children = Tree.get_logical_nth_child nd 1 in
        let nchildren = Array.length children in
        handle_block children nchildren
    end
    | L.LocalVariableDeclaration _ -> begin
        add_succ nexts
    end

    | _ -> ()
  in
  let children = body#initial_children in
  let nchildren = body#initial_nchildren in
  DEBUG_MSG "* %s[%s]: nchildren=%d"
    (L.to_string (getlab body)) (Loc.to_string body#data#src_loc) nchildren;
  for i = 0 to nchildren - 2 do
    set_succ [] [] [children.(i+1)] children.(i)
  done;
  if nchildren > 0 then
    set_succ [] [] [] children.(nchildren - 1)



class visitor bid_gen tree = object (self)
  inherit Sourcecode.visitor tree

  val stack = new Sourcecode.stack

  method scanner_body_before_subscan nd =
    let lab = getlab nd in
    if L.scope_creating lab then
      stack#push nd

  method scanner_body_after_subscan nd =
    let lab = getlab nd in
    if L.scope_creating lab then
      stack#pop;

    if L.is_parameter lab then begin
      let name = L.get_name lab in
      let bid = bid_gen#gen in
      DEBUG_MSG "DEF(param): %s (bid=%a) %s" name BID.ps bid nd#to_string;
      nd#data#set_binding (Binding.make_unknown_def bid);
      stack#register name nd
    end;

    if L.is_variabledeclarator lab then begin
      if L.is_local_variabledeclarator lab then begin
	let name = L.get_name lab in
	let bid = bid_gen#gen in
	DEBUG_MSG "DEF(decl): %s (bid=%a) %s" name BID.ps bid nd#to_string;
	nd#data#set_binding (Binding.make_unknown_def bid);
	stack#register name nd
      end
    end;

    if L.is_primaryname lab then begin
      let name = L.get_name lab in
      try
	let binder_nd = stack#lookup name in
	let bid = Binding.get_bid binder_nd#data#binding in
	DEBUG_MSG "    USE: %s (bid=%a) %s" name BID.ps bid nd#to_string;
	nd#data#set_binding (Binding.make_use bid)
      with
	Not_found -> ()
    end;

    begin
      match lab with
      | L.MethodBody _ | L.ConstructorBody _ -> set_control_flow nd
      | L.StaticInitializer | L.InstanceInitializer | L.Finally ->
            set_control_flow nd#initial_children.(0)
      | L.CatchClause -> set_control_flow nd#initial_children.(1)
      | _ -> ()
    end

  (* end of method scanner_body_after_subscan *)
    
end (* of class Tree.visitor *)


class translator options = let bid_gen = new BID.generator in object (self)
  inherit node_maker options

  method set_bindings (tree : Spec.tree_t) =

    (* for fields *)
    let fieldtbl = Hashtbl.create 0 (* FQN -> node *) in
    let facctbl = Hashtbl.create 0 (* FQN -> node *) in

    let add tbl nm nd =
      try
	let nds = Hashtbl.find tbl nm in
	Hashtbl.replace tbl nm (nd :: nds)
      with
	Not_found -> Hashtbl.add tbl nm [nd]
    in
    let add_field = add fieldtbl in
    let add_facc = add facctbl in

    let is_self_facc nd =
      match nd#initial_children with
      | [||] -> true
      | [|n|] -> L.is_primarythis (getlab n)
      | _ -> false
    in

    tree#fast_scan_whole_initial
      (fun nd ->
	let lab = getlab nd in
	if L.is_fieldaccess lab then begin
	  if is_self_facc nd then
	    add_facc (get_fqn "" nd lab) nd
	end
	else if L.is_field lab then begin
	  add_field (vdid_to_id (get_fqn "" nd lab)) nd
	end
	else if L.is_variabledeclarator lab then begin
	  if L.is_local_variabledeclarator lab then
	    ()
	  else
	    add_field (get_fqn "" nd lab) nd
	end
      );
    
    Hashtbl.iter 
      (fun nm nds -> 
	let bid = bid_gen#gen in
	let ref_bid = Binding.make_use bid in
	DEBUG_MSG "FQN: %s (bid=%a)" nm BID.ps bid;
	let referred = ref 0 in
	begin
	  try
	    let nds' = Hashtbl.find facctbl nm in
	    List.iter 
	      (fun n -> 
		DEBUG_MSG "    facc: %s" n#to_string;
		n#data#set_binding ref_bid;
		incr referred
	      ) nds'
	  with
	    Not_found ->
	      DEBUG_MSG "    facc: not found"
	end;
	let def_bid = Binding.make_used_def bid !referred in
	List.iter 
	  (fun n -> 
	    DEBUG_MSG "    field: %s" n#to_string;
	    n#data#set_binding def_bid
	  ) nds;
      ) fieldtbl;

    (* for local variables *)
    let visitor = new visitor bid_gen tree in
    visitor#visit_all

    (* end of method set_bindings *)


    
  method mktid nd =
    Lang_base.mktid
      (if options#incomplete_info_flag then 
	"" 
      else 
	Xhash.to_hex (new c options nd false)#digest)
      (if options#incomplete_info_flag then 
	"" 
      else 
	nd#data#anonymized_label)

  method _mktid nd =
    Lang_base.mktid
      (if options#incomplete_info_flag then 
	"" 
      else 
	Xhash.to_hex (new c options nd false)#digest) 
      ""

  val true_parent_tbl = Hashtbl.create 0
  method true_parent_tbl = true_parent_tbl
  method add_true_parent (uid : Otreediff.UID.t) (nd : Spec.node_t) =
    Hashtbl.add true_parent_tbl uid nd

  val true_children_tbl = Hashtbl.create 0
  method true_children_tbl = true_children_tbl
  method add_true_children nd children =
    DEBUG_MSG "%s -> [\n%s\n]"
      nd#to_string (String.concat ";\n" (List.map (fun c -> c#to_string) (Array.to_list children)));
    Hashtbl.add true_children_tbl nd children

  method of_javatype dims ty = 
    let ty =
      if dims > 0 then begin
	let mkty d = { Ast.ty_desc=d; ty_loc=ty.Ast.ty_loc } in
	match ty.Ast.ty_desc with
	| Ast.Tarray(ty', dims') -> mkty (Ast.Tarray(ty', dims' + dims))
	| _                      -> mkty (Ast.Tarray(ty, dims))
      end
      else 
        ty
    in
    let rec get_children desc = 
      match desc with
      | Ast.Tprimitive(al, _) -> (List.map self#of_annotation al), None

      | Ast.TclassOrInterface tss
      | Ast.Tclass tss
      | Ast.Tinterface tss -> begin
          let nds =
            match tss with
            | [] -> []
            | [Ast.TSname(al, _)] -> List.map self#of_annotation al
            | _ -> begin
                List.fold_left
                  (fun l spec ->
                    let al, n, tas_opt =
                      match spec with
                      | Ast.TSname(al, n)       -> al, n, None
                      | Ast.TSapply(al, n, tas) ->
                          if tas.Ast.tas_type_arguments = [] then
                            al, n, None
                          else
                            al, n, Some tas
                    in
                    let id = L.conv_name n in
                    let orig_id = L.conv_name ~resolve:false n in
                    let loc0 =
                      match al with
                      | []   -> n.Ast.n_loc
                      | a::_ -> a.Ast.a_loc
                    in
                    let loc1 =
                      match tas_opt with
                      | Some tas -> tas.Ast.tas_loc
                      | None     -> n.Ast.n_loc
                    in
                    let loc =
                      if loc0 == loc1 then
                        loc0
                      else
                        Ast.Loc.merge loc0 loc1
                    in
                    let tal =
                      match tas_opt with
                      | Some tas -> [self#of_type_arguments id tas]
                      | None -> []
                    in
                    let c = l @ (List.map self#of_annotation al) @ tal in
                    let ordinal_tbl_opt =
                      Some (new ordinal_tbl [List.length l; List.length al; List.length tal])
                    in
                    let orig_lab_opt = Some (L.Type (L.Type.ClassOrInterface orig_id)) in
                    let lab = L.Type (L.Type.ClassOrInterface id) in
                    let nd = self#mknode ~orig_lab_opt ~ordinal_tbl_opt lab c in
                    set_loc nd loc;
                    [nd]
                  ) [] tss
            end
          in
          match nds with
          | [] -> [], None
          | nd :: _ -> (Array.to_list nd#children), get_orig_lab_opt nd
      end
      | Ast.Tarray(t, dims) -> begin
          let children, _lab_opt = get_children t.Ast.ty_desc in
          let lab_opt =
            match _lab_opt with
            | Some (L.Type lab) -> Some (L.Type (L.Type.Array(lab, dims)))
            | Some _ -> assert false
            | None -> None
          in
          children, lab_opt
      end
      | Ast.Tvoid -> [], None
    in
    let children, lab_opt =
      get_children ty.Ast.ty_desc
    in
    let orig_lab_opt =
      match lab_opt with
      | None -> Some (L.of_javatype ~resolve:false ty)
      | Some _ -> lab_opt
    in
    let nd = self#mknode ~orig_lab_opt (L.of_javatype ty) children in
    set_loc nd ty.Ast.ty_loc;
    nd


  method param_to_tystr param = 
    P.type_to_short_string 
      (snd param.Ast.fp_variable_declarator_id) param.Ast.fp_type

  method signature_of_method_header mh =
    let params = mh.Ast.mh_parameters in
    (*sprintf "%s(%s)%s"
      mh.Ast.mh_name
      (Xlist.to_string self#param_to_tystr "" params)
      (P.type_to_short_string 0 mh.Ast.mh_return_type)*)
    sprintf "(%s)%s"
      (Xlist.to_string self#param_to_tystr "" params)
      (P.type_to_short_string 0 mh.Ast.mh_return_type)
      
  method of_parameter param =
    let name, dims = param.Ast.fp_variable_declarator_id in
    let mods = param.Ast.fp_modifiers in
    let mod_nodes = self#of_modifiers_opt L.Kparameter name mods in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length mod_nodes; 1])
    in
    let nd = 
      self#mknode ~ordinal_tbl_opt (L.Parameter(name, dims, param.Ast.fp_variable_arity))
	(mod_nodes @ [self#of_javatype 0 param.Ast.fp_type])
    in
    set_loc nd param.Ast.fp_loc;
    nd

  method of_parameters mname aloc params =
    match params with
    | [] -> []
    | _ ->
        (*let ordinal_tbl_opt = Some (new ordinal_tbl [List.length params]) in*)
	let nd =
	  self#mknode (*~ordinal_tbl_opt*)
            (L.Parameters mname) (List.map self#of_parameter params) 
	in
	set_loc nd aloc;
	[nd]


  method of_type_parameters name tps =
    let tparams = tps.Ast.tps_type_parameters in
    (*let ordinal_tbl_opt = Some (new ordinal_tbl [List.length tparams]) in*)
    let nd = 
      self#mknode (*~ordinal_tbl_opt*) (L.TypeParameters name)
	(List.map self#of_type_parameter tparams)
    in
    set_loc nd tps.Ast.tps_loc;
    nd

  method of_type_parameters_opt name tparams_opt = 
    of_opt (self#of_type_parameters name) tparams_opt

  method of_type_parameter tp =
    let annots = tp.Ast.tp_annotations in
    let tbound = tp.Ast.tp_type_bound in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length annots; if tbound = None then 0 else 1])
    in
    let children =
      (List.map self#of_annotation annots) @
      match tbound with
      | None -> []
      | Some tb -> [self#of_type_bound tb]
    in
    let nd =
      self#mknode ~ordinal_tbl_opt (L.TypeParameter tp.Ast.tp_type_variable) children
    in
    set_loc nd tp.Ast.tp_loc;
    nd

  method of_type_bound tb =
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [1; List.length tb.Ast.tb_additional_bounds])
    in
    let children = 
      (self#of_javatype 0 tb.Ast.tb_reference_type) :: 
      (List.map self#of_additional_bound tb.Ast.tb_additional_bounds)
    in
    let nd = self#mknode ~ordinal_tbl_opt L.TypeBound children in
    set_loc nd tb.Ast.tb_loc;
    nd

  method of_additional_bound ab =
    let nd = self#of_javatype 0 ab.Ast.ab_interface in
    set_loc nd ab.Ast.ab_loc;
    nd

  method of_modifiers kind name ms =
    let children = 
      List.map 
	(fun m ->
          match m.Ast.m_desc with
          | Ast.Mannotation a -> self#of_annotation a
          | _ -> begin
              let lab =
                match m.Ast.m_desc with
                | Ast.Mpublic       -> L.Modifier.Public
                | Ast.Mprotected    -> L.Modifier.Protected
                | Ast.Mprivate      -> L.Modifier.Private
                | Ast.Mstatic       -> L.Modifier.Static
                | Ast.Mabstract     -> L.Modifier.Abstract
                | Ast.Mfinal        -> L.Modifier.Final
                | Ast.Mnative       -> L.Modifier.Native
                | Ast.Msynchronized -> L.Modifier.Synchronized
                | Ast.Mtransient    -> L.Modifier.Transient
                | Ast.Mvolatile     -> L.Modifier.Volatile
                | Ast.Mstrictfp     -> L.Modifier.Strictfp
                | Ast.Mdefault      -> L.Modifier.Default
                | Ast.Mannotation _ -> assert false
              in
	      let nd = self#mkleaf (L.Modifier lab) in
	      set_loc nd m.Ast.m_loc;
	      nd
          end
	) ms.Ast.ms_modifiers
    in
    let children' = List.fast_sort compare_node children in
    let namek = name^(L.kind_to_suffix kind) in
    let nd = self#mklnode (L.Modifiers namek) children' in
    self#add_true_children nd (Array.of_list children);
    set_loc nd ms.Ast.ms_loc;
    nd

  method of_modifiers_opt kind name = function
    | None -> [] 
    | Some ms -> [self#of_modifiers kind name ms]

  method of_throws mname th =
    let leaves = List.map (self#of_javatype 0) th.Ast.th_exceptions in
    let nd = self#mklnode (L.Throws mname) leaves in
    set_loc nd th.Ast.th_loc;
    nd

  method of_throws_opt mname = function
    | None -> []
    | Some throws -> [self#of_throws mname throws]

  method name_of_method_header header = header.Ast.mh_name

  method of_method_header header =
    let ident = header.Ast.mh_name in
    let mods = header.Ast.mh_modifiers in
    let tparams = header.Ast.mh_type_parameters in
    let params = header.Ast.mh_parameters in
    let throws = header.Ast.mh_throws in

    let mod_nodes = self#of_modifiers_opt L.Kmethod ident mods in
    let tp_nodes = self#of_type_parameters_opt ident tparams in
    let rty = self#of_javatype 0 header.Ast.mh_return_type in
    let p_nodes = self#of_parameters ident header.Ast.mh_parameters_loc params in
    let th_nodes = self#of_throws_opt ident throws in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length mod_nodes;
                             List.length tp_nodes;
                             1;
                             List.length p_nodes;
                             List.length th_nodes;
                           ])
    in
    let children = 
      mod_nodes @ tp_nodes @ [rty] @ p_nodes @ th_nodes in
    let msig = self#signature_of_method_header header in
    let nd =
      self#mknode
        ~annot:(L.make_annotation msig)
        ~ordinal_tbl_opt
        (L.Method ident) children
    in
    set_loc nd header.Ast.mh_loc;
    nd


  method of_variable_initializer vi =
    match vi.Ast.vi_desc with
    | Ast.VIexpression e -> self#of_expression e
    | Ast.VIarray ai ->
        let ordinal_tbl_opt = Some (new ordinal_tbl [List.length ai]) in
	let nd = 
	  self#mknode ~ordinal_tbl_opt
            L.ArrayInitializer (List.map self#of_variable_initializer ai) 
	in
	set_loc nd vi.Ast.vi_loc;
	nd

  method of_variable_declarator vd =
    let loc = conv_loc vd.Ast.vd_loc in
    let name, dims = vd.Ast.vd_variable_declarator_id in
    let children =
      match vd.Ast.vd_variable_initializer with 
      | None -> [] 
      | Some init -> [self#of_variable_initializer init]
    in
    let ordinal_tbl_opt = Some (new ordinal_tbl [List.length children]) in
    let nd = 
      self#mknode ~ordinal_tbl_opt
        (L.VariableDeclarator(name, dims, !(vd.Ast.vd_is_local))) children
    in
    nd#data#set_loc loc;
    nd

  method vdids_to_str vdids = String.concat ";" (List.map (fun (id, _) -> id) vdids)

  method of_local_variable_declaration ~is_stmt lvd =
    let mods = lvd.Ast.lvd_modifiers in
    let vdtors = lvd.Ast.lvd_variable_declarators in
    let vdids = 
      List.map (fun vd -> vd.Ast.vd_variable_declarator_id) vdtors
    in
    let vdids_str = self#vdids_to_str vdids in
    let mod_nodes = self#of_modifiers_opt L.Klocal vdids_str mods in

    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length mod_nodes; 1; List.length vdtors])
    in

    let ty_leaf = self#of_javatype 0 lvd.Ast.lvd_type in
    let nd = 
      self#mknode ~ordinal_tbl_opt (L.LocalVariableDeclaration(is_stmt, vdids))
	(mod_nodes @ 
         [ty_leaf] @
	 (List.map self#of_variable_declarator vdtors))
    in
    set_loc nd lvd.Ast.lvd_loc;
    nd


  method of_literal lit = self#mkleaf (L.of_literal lit)

  method is_empty_arguments args = args.Ast.as_arguments = []

  method digest_of_arguments args =
    let t = new c options (self#of_arguments args) false in
    t#digest

  method addhash s h = sprintf "%s:%s" s h


  method of_type_arguments ?(nth=1) name tas =
    let children = List.map self#of_type_argument tas.Ast.tas_type_arguments in
    (*let ordinal_tbl_opt = Some (new ordinal_tbl [List.length children]) in*)
    let nd = self#mknode (*~ordinal_tbl_opt*) (L.TypeArguments(nth, name)) children in
    set_loc nd tas.Ast.tas_loc;
    nd

  method of_type_arguments_opt ?(nth=1) name targs_opt =
    of_opt (self#of_type_arguments ~nth name) targs_opt

  method of_type_argument ta =
    let nd =
      match ta.Ast.ta_desc with
      | Ast.TAreferenceType ty -> self#of_javatype 0 ty
      | Ast.TAwildcard wc      -> self#of_wildcard wc
    in
    set_loc nd ta.Ast.ta_loc;
    nd

  method of_wildcard (al, wb_opt) =
    let a_nodes = self#of_annotations al in
    let wb_nodes =
      match wb_opt with
      | None -> []
      | Some wb -> [self#of_wildcard_bounds wb]
    in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length a_nodes;
                             List.length wb_nodes;
                           ])
    in
    let children = a_nodes @ wb_nodes in
    let nd = self#mknode ~ordinal_tbl_opt L.Wildcard children in
    set_nodes_loc nd children;
    nd

  method of_wildcard_bounds wb =
    let nd = 
      match wb.Ast.wb_desc with
      | Ast.WBextends ty -> self#mknode L.WildcardBoundsExtends [self#of_javatype 0 ty]
      | Ast.WBsuper ty   -> self#mknode L.WildcardBoundsSuper [self#of_javatype 0 ty]
    in
    set_loc nd wb.Ast.wb_loc;
    nd

  method _of_arguments lab args =
    let children = List.map self#of_expression args.Ast.as_arguments in
    (*let ordinal_tbl_opt = Some (new ordinal_tbl [List.length children]) in*)
    let nd = self#mknode (*~ordinal_tbl_opt*) lab children in
    set_loc nd args.Ast.as_loc;
    nd

  method of_arguments args = self#_of_arguments L.Arguments args

  method of_named_arguments name args = 
    self#_of_arguments (L.NamedArguments name) args

  method of_named_arguments_opt name args_opt = 
    of_opt (self#of_named_arguments name) args_opt

  method of_class_instance_creation ?(is_stmt=false) cic =
    let deco id args =
      id^".<init>#"^(string_of_int (List.length args.Ast.as_arguments))
    in
    let create ?(orig_lab_opt=None) plab children otbl =
      let lab = L.mkplab is_stmt plab in
      let orig_lab_opt =
        match orig_lab_opt with
        | Some l -> Some (L.mkplab is_stmt l)
        | None -> None
      in
      let ordinal_tbl_opt = Some (new ordinal_tbl otbl) in
(*
      let children =
	if is_stmt then
	  let n = self#mknode (L.mkplab false plab) children in
	  set_loc n cic.Ast.cic_loc;
	  [n]
	else
	  children
      in
*)
      self#mknode ~orig_lab_opt ~ordinal_tbl_opt lab children
    in
    match cic.Ast.cic_desc with
    | Ast.CICunqualified(targs_opt, ty, args, body_opt) ->
	let name = P.type_to_string ~resolve:true ~show_attr:false ty in
	let args_nd = 
	    [self#of_named_arguments name args]
	in
        let ta_nodes = self#of_type_arguments_opt name targs_opt in
        let cb_nodes = self#of_class_body_opt name body_opt in
        let otbl =
          [List.length ta_nodes;
           1;
           1;
           List.length cb_nodes;
         ]
        in
	let children =
	  ta_nodes @ [self#of_javatype 0 ty] @ args_nd @ cb_nodes
	in
        let orig_lab_opt =
          Some (L.Primary.InstanceCreation (P.type_to_string ~show_attr:false ty))
        in
	let plab = L.Primary.InstanceCreation (deco name args) in
	create ~orig_lab_opt plab children otbl

    | Ast.CICqualified(prim, targs_opt1, ident, targs_opt2, args, body_opt) ->
	let args_nd = [self#of_named_arguments ident args] in
        let ta_nodes1 = self#of_type_arguments_opt ident targs_opt1 in
        let ta_nodes2 = self#of_type_arguments_opt ~nth:2 ident targs_opt2 in
        let cb_nodes = self#of_class_body_opt ident body_opt in
        let otbl =
          [1;
           List.length ta_nodes1;
           List.length ta_nodes2;
           1;
           List.length cb_nodes;
         ]
        in
	let children = 
	  (self#of_primary prim) :: 
	  (ta_nodes1 @ (* ! *)
	   ta_nodes2 @
           args_nd @
	   cb_nodes) (* ! *)
	in
	let plab = L.Primary.QualifiedInstanceCreation (deco ident args) in
	create plab children otbl

    | Ast.CICnameQualified(name, targs_opt1, ident, targs_opt2, args, body_opt) ->
	let n = L.conv_name name in
	let args_nd = [self#of_named_arguments ident args] in
        let ta_nodes1 = self#of_type_arguments_opt n targs_opt1 in
        let ta_nodes2 = self#of_type_arguments_opt ~nth:2 ident targs_opt2 in
        let cb_nodes = self#of_class_body_opt n body_opt in
        let otbl =
          [List.length ta_nodes1;
           List.length ta_nodes2;
           1;
           List.length cb_nodes;
         ]
        in
	let children = ta_nodes1 @ ta_nodes2 @ args_nd @ cb_nodes in
        let orig_lab_opt =
          Some (L.Primary.NameQualifiedInstanceCreation(L.conv_name ~resolve:false name, ident))
        in
	let plab = L.Primary.NameQualifiedInstanceCreation(n, deco ident args) in
        create ~orig_lab_opt plab children otbl
	  

  method of_field_access = function
    | Ast.FAprimary(prim, name) -> 
	self#mknode (L.Primary (L.Primary.FieldAccess name)) [self#of_primary prim]
    | Ast.FAsuper name -> 
	self#mkleaf (L.Primary (L.Primary.SuperFieldAccess name))
    | Ast.FAclassSuper(classname, name) -> 
	self#mknode (L.Primary (L.Primary.ClassSuperFieldAccess name))
	  [self#mkleaf (L.of_classname classname)]
    | Ast.FAimplicit name -> self#mkleaf (L.Primary (L.Primary.FieldAccess name))


  method of_method_invocation ?(is_stmt=false) mi = 

    let deco id args =
      id^"#"^(string_of_int (List.length args.Ast.as_arguments))
    in

    let create ?(orig_lab_opt=None) plab children otbl =
      let ordinal_tbl_opt = Some (new ordinal_tbl otbl) in
      let tid = ref L.null_tid in
(*
      let children =
	if is_stmt then
	  let n = self#mknode (L.mkplab false plab) children in
	  tid := self#_mktid n;
	  set_loc n mi.Ast.mi_loc;
	  [n]
	else
	  children
      in
*)
      let orig_lab_opt =
        match orig_lab_opt with
        | Some l -> Some (L.mkplab is_stmt l)
        | None -> None
      in
      let lab = L.mkplab ~tid:!tid is_stmt plab in
      self#mknode ~orig_lab_opt ~ordinal_tbl_opt lab children
    in
    let nd = 
      match mi.Ast.mi_desc with
      | Ast.MImethodName(name, args) ->

	  let rightmost = Ast.rightmost_identifier name in
(*
  let mname = L.conv_name name in
 *)
	  let q = qualifier_of_name name in
          let a_node = self#of_named_arguments rightmost args in
          let otbl = [if q <> None then 1 else 0; 1] in
	  let children = 
	    match q with 
	    | None -> [a_node]
	    | Some n -> (* !!! *)
                let orig_lab_opt = Some (L.Qualifier (L.conv_name ~resolve:false n)) in
		let qnd = self#mkleaf ~orig_lab_opt (L.Qualifier (L.conv_name n)) in
		qnd#data#set_loc (loc_of_name n);
		[qnd; a_node]
	  in
(*
  let hash = self#digest_of_arguments args in
  let rightmost = self#addhash rightmost hash in
 *)
	  let plab = L.Primary.SimpleMethodInvocation (deco rightmost args) in
	  create plab children otbl

      | Ast.MIprimary(prim, targs_opt, ident, args) ->
(*
  let hash = self#digest_of_arguments args in
  let ident = self#addhash ident hash in
 *)
          let prim_nd = self#of_primary prim in
	  let plab =
            (*if L.is_ambiguous_name (getlab prim_nd) then
              L.Primary.AmbiguousMethodInvocation (deco ident args)
            else*)
              L.Primary.PrimaryMethodInvocation (deco ident args)
          in
          let ta_nodes = self#of_type_arguments_opt ident targs_opt in
          let otbl = [1; List.length ta_nodes; 1] in
	  let children =
	    prim_nd :: (ta_nodes @ [self#of_named_arguments ident args])
	  in
	  create plab children otbl

      | Ast.MItypeName(name, targs_opt, ident, args) ->
	  let n = L.conv_name name in
(*
  let hash = self#digest_of_arguments args in
  let ident = self#addhash ident hash in
 *)
          let orig_lab_opt =
            let n = L.conv_name ~resolve:false name in
            Some (L.Primary.TypeMethodInvocation(n, deco ident args))
          in
	  let plab = L.Primary.TypeMethodInvocation(n, deco ident args) in
          let ta_nodes = self#of_type_arguments_opt ident targs_opt in
          let otbl = [List.length ta_nodes; 1] in
	  let children = ta_nodes @ [self#of_named_arguments ident args] in
	  create ~orig_lab_opt plab children otbl

      | Ast.MIsuper(loc_super, targs_opt, ident, args) ->
	  let snd = self#mkleaf L.Super in
	  set_loc snd loc_super;
(*
  let hash = self#digest_of_arguments args in
  let ident = self#addhash ident hash in
 *)
	  let plab = L.Primary.SuperMethodInvocation (deco ident args) in
          let ta_nodes = self#of_type_arguments_opt ident targs_opt in
          let otbl = [1; List.length ta_nodes; 1] in
	  let children = 
	    snd::(ta_nodes @ [self#of_named_arguments ident args])
	  in
	  create plab children otbl

      | Ast.MIclassSuper(loc_cl, loc_super, classname, targs_opt, ident, args) ->
	  let cnd =
            self#mkleaf
              ~orig_lab_opt:(Some (L.of_classname ~resolve:false classname))
              (L.of_classname classname)
          in
	  set_loc cnd loc_cl;
	  let snd = self#mkleaf L.Super in
	  set_loc snd loc_super;
(*
  let hash = self#digest_of_arguments args in
  let ident = self#addhash ident hash in
 *)
	  let plab = L.Primary.ClassSuperMethodInvocation (deco ident args) in
          let ta_nodes = self#of_type_arguments_opt ident targs_opt in
          let otbl = [1; 1; List.length ta_nodes; 1] in
	  let children =
	    cnd::snd::(ta_nodes @ [self#of_named_arguments ident args])
	  in
	  create plab children otbl
	    
    in
    set_loc nd mi.Ast.mi_loc;
    nd


  method of_array_access aa = 
    let children = 
      match aa.Ast.aa_desc with
      | Ast.AAname(name, expr) ->
	  let pnd =
            let orig_lab_opt =
              Some (L.Primary (L.Primary.Name (L.conv_name ~resolve:false name)))
            in
            self#mkleaf ~orig_lab_opt (L.Primary (L.Primary.Name (L.conv_name name)))
          in
	  set_loc pnd name.Ast.n_loc;
	  [pnd; self#of_expression expr]
      | Ast.AAprimary(prim, expr) -> [self#of_primary prim; self#of_expression expr]
    in
    let nd = self#mknode (L.Primary L.Primary.ArrayAccess) children in
    set_loc nd aa.Ast.aa_loc;
    nd

  method of_dim_expr de = 
    let en = self#of_expression de.Ast.de_desc in
    let nd = self#mknode L.DimExpr [en] in
    set_loc nd de.Ast.de_loc;
    nd

  method of_array_creation_expression = function
    | Ast.ACEtype(ty, exprs, dims) ->
        let ordinal_tbl_opt = Some (new ordinal_tbl [1; List.length exprs]) in
	self#mknode ~ordinal_tbl_opt (L.Primary (L.Primary.ArrayCreationDims dims))
	  ((self#of_javatype 0 ty) :: (List.map self#of_dim_expr exprs))

    | Ast.ACEtypeInit(ty, dims, array_initializer) ->
        let ordinal_tbl_opt = Some (new ordinal_tbl [1; List.length array_initializer]) in
	self#mknode ~ordinal_tbl_opt (L.Primary L.Primary.ArrayCreationInit)
	  ((self#of_javatype dims ty) ::
	   (List.map self#of_variable_initializer array_initializer))

  method of_primary p =
    let loc0 = Ast.Loc.collapse_forward p.Ast.p_loc in
    let name_to_node ?(children=[]) mkplab n =
      let unresolved = L.conv_name ~resolve:false n in
      let orig_lab_opt = Some (L.Primary (mkplab unresolved)) in
      let nd = self#mknode ~orig_lab_opt (L.Primary (mkplab (L.conv_name n))) children in
      let loc = Ast.Loc.widen loc0 (String.length unresolved) in
      set_loc nd loc;
      nd
    in
    let nd =
      match p.Ast.p_desc with
      | Ast.Pname name -> begin
          match qualifier_of_name name with
          | None -> begin
              let mklab =
                if Ast.is_ambiguous_name name then
                  (fun x -> L.Primary.AmbiguousName x)
                else
                  (fun x -> L.Primary.Name x)
              in
              name_to_node mklab name
          end
          | Some q -> begin
              if Ast.is_ambiguous_name q then begin
                let mknd ?(children=[]) =
                  name_to_node ~children (fun x -> L.Primary.AmbiguousName x)
                in
                let rec doit n =
                  try
                    let n0, _ = Ast.decompose_name n in
                    mknd ~children:[doit n0] n
                  with
                    _ -> mknd n
                in
                doit name
              end
              else
                name_to_node (fun x -> L.Primary.Name x) name
          end
      end
      | Ast.Pliteral lit -> self#of_literal lit

      | Ast.PclassLiteral ty ->
	  self#mknode (L.Primary L.Primary.ClassLiteral) [self#of_javatype 0 ty] 

      | Ast.PclassLiteralVoid -> self#mkleaf (L.Primary (L.Primary.ClassLiteralVoid)) 
      | Ast.Pthis -> self#mkleaf (L.Primary L.Primary.This)
      | Ast.PqualifiedThis name ->
          let orig_lab_opt =
            Some (L.Primary (L.Primary.QualifiedThis (L.conv_name ~resolve:false name)))
          in
	  self#mkleaf ~orig_lab_opt (L.Primary (L.Primary.QualifiedThis (L.conv_name name)))

      | Ast.Pparen expr -> 
	  let e_nd = self#of_expression expr in
	  let tid = self#mktid e_nd in
	  self#mknode (L.Primary (L.Primary.Paren tid)) [self#of_expression expr]

      | Ast.PclassInstanceCreation class_instance_creation -> 
	  self#of_class_instance_creation class_instance_creation

      | Ast.PfieldAccess field_acc -> self#of_field_access field_acc
      | Ast.PmethodInvocation meth_inv -> self#of_method_invocation meth_inv
      | Ast.ParrayAccess arr_acc -> self#of_array_access arr_acc
      | Ast.ParrayCreationExpression array_creation ->
	  self#of_array_creation_expression array_creation

      | Ast.PmethodReference method_reference ->
          self#of_method_reference method_reference

      | Ast.Perror -> self#mkleaf L.Error
    in
    set_loc nd p.Ast.p_loc;
    nd

  method of_method_reference mr =
    let mkprim ?(orig_lab_opt=None) ordinal_tbl_opt l c =
      let orig_lab_opt =
        match orig_lab_opt with
        | Some l -> Some (L.Primary l)
        | None -> None
      in
      self#mknode ~orig_lab_opt ~ordinal_tbl_opt (L.Primary l) c
    in
    match mr.Ast.mr_desc with
  | Ast.MRname(n, tas_opt, id) ->
      let ta_nodes = self#of_type_arguments_opt id tas_opt in
      let ordinal_tbl_opt = Some (new ordinal_tbl [List.length ta_nodes]) in
      let orig_lab_opt =
        Some (L.Primary.NameMethodReference(L.conv_name ~resolve:false n, id))
      in
      mkprim ~orig_lab_opt ordinal_tbl_opt
        (L.Primary.NameMethodReference(L.conv_name n, id)) ta_nodes

  | Ast.MRprimary(p, tas_opt, id) ->
      let ta_nodes = self#of_type_arguments_opt id tas_opt in
      let ordinal_tbl_opt = Some (new ordinal_tbl [1; List.length ta_nodes]) in
      mkprim ordinal_tbl_opt
        (L.Primary.PrimaryMethodReference id) ((self#of_primary p) :: ta_nodes)

  | Ast.MRsuper(tas_opt, id) ->
      let ta_nodes = self#of_type_arguments_opt id tas_opt in
      let ordinal_tbl_opt = Some (new ordinal_tbl [List.length ta_nodes]) in
      mkprim ordinal_tbl_opt
        (L.Primary.SuperMethodReference id) ta_nodes

  | Ast.MRtypeSuper(n, tas_opt, id) ->
      let ta_nodes = self#of_type_arguments_opt id tas_opt in
      let ordinal_tbl_opt = Some (new ordinal_tbl [List.length ta_nodes]) in
      let orig_lab_opt =
        Some (L.Primary.TypeSuperMethodReference(L.conv_name ~resolve:false n, id))
      in
      mkprim ~orig_lab_opt ordinal_tbl_opt
        (L.Primary.TypeSuperMethodReference(L.conv_name n, id)) ta_nodes

  | Ast.MRtypeNew(n, tas_opt) ->
      let ta_nodes = self#of_type_arguments_opt "" tas_opt in
      let ordinal_tbl_opt = Some (new ordinal_tbl [List.length ta_nodes]) in
      let orig_lab_opt =
        Some (L.Primary.TypeNewMethodReference(L.conv_name ~resolve:false n))
      in
      mkprim ~orig_lab_opt ordinal_tbl_opt
        (L.Primary.TypeNewMethodReference(L.conv_name n)) ta_nodes

  method of_assignment ?(is_stmt=false) (lhs, ao, expr) = 
   let lab = L.of_assignment_operator ~is_stmt ao in
   let children = [self#of_expression lhs; self#of_expression expr] in
(*
   let children =
     if is_stmt then
       let n = self#mknode (L.of_assignment_operator ~is_stmt:false ao) children in
       set_loc n ao.Ast.ao_loc;
       [n]
     else
       children
   in
*)
   let nd = self#mknode lab children in
   set_loc nd ao.Ast.ao_loc;
   nd

  method of_expression ?(is_stmt=false) e =
    let nd = 
      match e.Ast.e_desc with
      | Ast.Eprimary prim -> self#of_primary prim
      | Ast.Eunary(unary_op, expr) -> 
	  self#mknode (L.of_unary_operator ~is_stmt unary_op) [self#of_expression expr] 

      | Ast.Ebinary(bin_op, expr1, expr2) ->
	  self#mknode (L.of_binary_operator bin_op)
	    [self#of_expression expr1; self#of_expression expr2]

      | Ast.Ecast(ty, expr) -> 
	  self#mknode (L.Expression L.Expression.Cast) 
	    [self#of_javatype 0 ty; self#of_expression expr] 

      | Ast.Einstanceof(expr, ty) -> 
	  self#mknode (L.Expression L.Expression.Instanceof)
	    [self#of_expression expr; self#of_javatype 0 ty]

      | Ast.Econd(expr1, expr2, expr3) ->
	  self#mknode (L.Expression L.Expression.Cond)
	    [self#of_expression expr1; self#of_expression expr2; self#of_expression expr3]

      | Ast.Eassignment assignment -> self#of_assignment ~is_stmt assignment

      | Ast.Elambda(params, body) -> 
          self#mknode (L.Expression L.Expression.Lambda)
            [self#of_lambda_params params; self#of_lambda_body body]

      | Ast.Eerror -> self#mkleaf L.Error
    in
    set_loc nd e.Ast.e_loc;
    nd

  method of_lambda_params params =
    match params.Ast.lp_desc with
    | Ast.LPident id     -> 
        let nd = self#mkleaf (L.InferredParameter id) in
        set_loc nd params.Ast.lp_loc;
        nd

    | Ast.LPformal fps -> 
        let nd = self#mknode (L.Parameters "") (List.map self#of_parameter fps) in
        set_loc nd params.Ast.lp_loc;
        nd

    | Ast.LPinferred ids -> 
        let mkp (loc, id) =
          let n = self#mkleaf (L.InferredParameter id) in
          set_loc n loc;
          n
        in
        let nd = self#mknode (L.InferredParameters) (List.map mkp ids) in
        set_loc nd params.Ast.lp_loc;
        nd

  method of_lambda_body = function
    | Ast.LBexpr expr   -> self#of_expression expr
    | Ast.LBblock block -> self#of_block block


  method of_statement_expression ?(is_stmt=false) se =
    let nd = 
      match se.Ast.se_desc with
      | Ast.SEassignment assign  -> self#of_assignment ~is_stmt assign
      | Ast.SEpreIncrement expr  -> self#of_expression ~is_stmt expr
      | Ast.SEpreDecrement expr  -> self#of_expression ~is_stmt expr
      | Ast.SEpostIncrement expr -> self#of_expression ~is_stmt expr
      | Ast.SEpostDecrement expr -> self#of_expression ~is_stmt expr

      | Ast.SEmethodInvocation method_invocation -> 
	  self#of_method_invocation ~is_stmt method_invocation

      | Ast.SEclassInstanceCreation class_instance_creation ->
	  self#of_class_instance_creation ~is_stmt class_instance_creation

      | Ast.SEerror -> self#mkleaf L.Error
    in
    set_loc nd se.Ast.se_loc;
    nd

  method of_switch_label sl =
    let nd = 
      match sl.Ast.sl_desc with
      | Ast.SLconstant const_expr -> 
	  self#mknode L.SLconstant [self#of_expression const_expr]
      | Ast.SLdefault -> self#mkleaf L.SLdefault
    in
    set_loc nd sl.Ast.sl_loc;
    nd

  method of_switch_block_statement_group (sls, bss) =
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length sls; List.length bss])
    in
    let children =
      (List.map self#of_switch_label sls) @
      (List.map self#of_block_statement bss)
    in
    let nd = self#mknode ~ordinal_tbl_opt L.SwitchBlockStatementGroup children in
    set_nodes_loc nd children;
    nd

  method of_resource r =
    let (vid, dims) = r.Ast.r_variable_declarator_id in
    let mod_nodes = self#of_modifiers_opt L.Klocal vid r.Ast.r_modifiers in
    let ty_leaf = self#of_javatype 0 r.Ast.r_type in
    let expr_node = self#of_expression r.Ast.r_expr in

    let ordinal_tbl_opt = Some (new ordinal_tbl [List.length mod_nodes; 1; 1]) in

    let children = mod_nodes @ [ty_leaf; expr_node] in
    let nd =
      self#mknode ~ordinal_tbl_opt (L.Resource(vid, dims)) children
    in
    set_loc nd r.Ast.r_loc;
    nd

  method of_resource_spec rs =
    let rl = List.map self#of_resource rs.Ast.rs_resources in
    let nd = self#mknode L.ResourceSpec rl in
    set_loc nd rs.Ast.rs_loc;
    nd

  method of_catch_parameter param =
    let name, dims = param.Ast.cfp_variable_declarator_id in
    let mods = param.Ast.cfp_modifiers in
    let mod_nodes = self#of_modifiers_opt L.Kparameter name mods in
    let type_nodes = List.map (self#of_javatype 0) param.Ast.cfp_type_list in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length mod_nodes; List.length type_nodes])
    in
    let nd =
      self#mknode ~ordinal_tbl_opt (L.CatchParameter(name, dims)) (mod_nodes @ type_nodes)
    in
    set_loc nd param.Ast.cfp_loc;
    nd

  method of_catch c =
    let nd =
      self#mknode L.CatchClause 
	[self#of_catch_parameter c.Ast.c_formal_parameter; self#of_block c.Ast.c_block]
    in
    set_loc nd c.Ast.c_loc;
    nd

  method of_finally f =
    let nd =
      self#mknode L.Finally [self#of_block f.Ast.f_block] 
    in
    set_loc nd f.Ast.f_loc;
    nd

  method of_catches catches = 
    let children = List.map self#of_catch catches in
    let len = List.length children in
    let loc =
      if len = 1 then (List.hd children)#data#src_loc
      else if len > 1 then 
	Loc._merge
	  (List.hd children)#data#src_loc 
	  (List.hd(List.rev children))#data#src_loc
      else 
	Loc.dummy
    in
    let nd = self#mklnode L.Catches children in
    nd#data#set_loc loc;
    nd

  method of_for_init fi =
    let nd = 
      match fi.Ast.fi_desc with
      | Ast.FIstatement se_list ->
	  let children = List.map self#of_statement_expression se_list in
	  let tid = 
	    match children with
	    | [] -> L.null_tid
	    | n::_ -> self#mktid n
	  in
	  self#mklnode (L.ForInit tid) children

      | Ast.FIlocal lvd -> 
	  let lvd_nd = self#of_local_variable_declaration ~is_stmt:false lvd in
	  let tid = self#mktid lvd_nd in
	  self#mknode (L.ForInit tid) [lvd_nd]
    in
    set_loc nd fi.Ast.fi_loc;
    nd

  method of_switch_block sb =
    let nd =
      self#mknode (L.SwitchBlock)
	(List.map
           self#of_switch_block_statement_group
           sb.Ast.sb_switch_block_stmt_grps)
    in
    set_loc nd sb.Ast.sb_loc;
    nd

  method of_statement s =
    let nd =
      match s.Ast.s_desc with
      | Ast.Sblock block   -> self#of_block block
      | Ast.Sempty         -> self#mkleaf (L.Statement L.Statement.Empty)
      | Ast.Sexpression se -> self#of_statement_expression ~is_stmt:true se

      | Ast.Sswitch(e, switch_block) -> 
	  self#mknode (L.Statement L.Statement.Switch)
	    [self#of_expression e; self#of_switch_block switch_block]

      | Ast.Sdo(s, e) -> 
	  (self#mknode (L.Statement L.Statement.Do) [self#of_statement s; self#of_expression e])

      | Ast.Sbreak ident_opt    -> self#mkleaf (L.Statement (L.Statement.Break ident_opt))
      | Ast.Scontinue ident_opt -> 
	  self#mkleaf (L.Statement (L.Statement.Continue ident_opt))

      | Ast.Sreturn e_opt ->
	  self#mknode (L.Statement L.Statement.Return) (of_opt self#of_expression e_opt)

      | Ast.Ssynchronized(e, block) -> 
	  self#mknode (L.Statement L.Statement.Synchronized) 
	    [self#of_expression e; self#of_block block]

      | Ast.Sthrow e -> self#mknode (L.Statement L.Statement.Throw) [self#of_expression e]

      | Ast.Stry(rspec_opt, block, catches_opt, finally_opt) ->
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [if rspec_opt = None then 0 else 1;
                                   1;
                                   if catches_opt = None then 0 else 1;
                                   if finally_opt = None then 0 else 1;
                                 ])
          in
	  self#mknode ~ordinal_tbl_opt (L.Statement L.Statement.Try)
            ((of_opt self#of_resource_spec rspec_opt) @
	     ((self#of_block block) :: 
	      ((of_opt self#of_catches catches_opt) @ (of_opt self#of_finally finally_opt))))

      | Ast.Slabeled(name, s) -> 
	  self#mknode (L.Statement (L.Statement.Labeled name)) [self#of_statement s]

      | Ast.SifThen(e, s) -> 
	  self#mknode (L.Statement L.Statement.If) [self#of_expression e; self#of_statement s] (* order sensitive s -> e *)

      | Ast.SifThenElse(e, s1, s2) ->
	  self#mknode (L.Statement L.Statement.If) 
	    [self#of_expression e; self#of_statement s1; self#of_statement s2] (* order sensitive s2 -> s1 -> e *)

      | Ast.Swhile(e, s) -> 
	  self#mknode (L.Statement L.Statement.While) [self#of_expression e; self#of_statement s]

      | Ast.Sfor(init_opt, e_opt, se_list, s) ->
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [if init_opt = None then 0 else 1;
                                   if e_opt = None then 0 else 1;
                                   if se_list = [] then 0 else 1;
                                   1;
                                 ])
          in
	  let children = 
	    (match init_opt with None -> [] | Some init -> [self#of_for_init init]) @
	    (match e_opt with 
	    | None -> [] 
	    | Some e ->
		let e_nd = self#of_expression e in
		let t = self#mktid e_nd in
		let n = self#mknode (L.ForCond t) [e_nd] in
		set_loc n e.Ast.e_loc;
		[n]
	    ) @
	    (if se_list <> [] 
	    then begin
	      let se_nodes = List.map self#of_statement_expression se_list in
	      let t = 
		match se_nodes with
		| [] -> L.null_tid
		| n::_ -> self#mktid n
	      in
	      let n = self#mknode (L.ForUpdate t) se_nodes in
	      set_nodes_loc n se_nodes;
	      [n]
	    end
	    else []) @
	    [self#of_statement s]
	  in
	  self#mknode ~ordinal_tbl_opt (L.Statement L.Statement.For) children

      | Ast.SforEnhanced(param, e, s) ->
	  self#mknode (L.Statement L.Statement.ForEnhanced)
	    [self#of_parameter param; self#of_expression e; self#of_statement s]

      | Ast.Sassert1 e -> 
	  self#mknode (L.Statement L.Statement.Assert) [self#of_expression e]

      | Ast.Sassert2(e1, e2) -> 
	  self#mknode (L.Statement L.Statement.Assert) 
	    [self#of_expression e1; self#of_expression e2]

      | Ast.Serror -> self#mkleaf L.Error
    in
    set_loc nd s.Ast.s_loc;
    nd

  method _of_block lab b =
    let nd =
      self#mklnode lab (List.map self#of_block_statement b.Ast.b_block_statements)
    in
    set_loc nd b.Ast.b_loc;
    nd

  method of_block b = self#_of_block L.Block b

  method of_method_body mname body = self#_of_block (L.MethodBody mname) body

  method of_block_statement bs =
    let nd =
      match bs.Ast.bs_desc with
      | Ast.BSlocal lvd -> self#of_local_variable_declaration ~is_stmt:true lvd
      | Ast.BSclass cd -> self#of_class_declaration false cd
      | Ast.BSstatement s -> self#of_statement s
      | Ast.BSerror -> self#mkleaf L.Error
    in
    set_loc nd bs.Ast.bs_loc;
    nd

  (*method of_field_declaration fd =
    let _mkfdecl ghost vd vdnd =
      let ty_leaf = self#of_javatype 0 fd.Ast.fd_type in
      let vdid = vd.Ast.vd_variable_declarator_id in
      let vdid_str = fst vdid in
      let mods = fd.Ast.fd_modifiers in
      let mod_nodes = self#of_modifiers_opt L.Kfield vdid_str mods in
      let ordinal_tbl_opt =
        Some (new ordinal_tbl [List.length mod_nodes; 1; 1])
      in
      let children = mod_nodes @ [ty_leaf; vdnd] in
      let nd = self#mknode ~ordinal_tbl_opt (L.FieldDeclaration [vdid]) children in
      if ghost then begin
	nd#data#set_loc Loc.ghost;
	List.iter set_ghost_rec mod_nodes;
	set_ghost_rec ty_leaf
      end
      else
	set_loc nd fd.Ast.fd_loc;
      nd
    in
    let mkfdecl ghost vd = 
      _mkfdecl ghost vd (self#of_variable_declarator vd) 
    in
    match fd.Ast.fd_variable_declarators with
    | []       -> []
    | [vd]     -> [mkfdecl false vd]
    | vd::rest -> 
	let fdecl_nd = mkfdecl false vd in
	let rest_vdnds = List.map self#of_variable_declarator rest in
	List.iter (fun vn -> self#add_true_parent vn#uid fdecl_nd) rest_vdnds;
	fdecl_nd :: (List.map2 (fun v vn -> _mkfdecl true v vn) rest rest_vdnds)*)

  method of_field_declaration fd =
    let mods = fd.Ast.fd_modifiers in
    let vdtors = fd.Ast.fd_variable_declarators in
    let vdids =
      List.map (fun vd -> vd.Ast.vd_variable_declarator_id) vdtors
    in
    let vdid_str = self#vdids_to_str vdids in
    let mod_nodes = self#of_modifiers_opt L.Kfield vdid_str mods in

    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length mod_nodes; 1; List.length vdtors])
    in

    let ty_leaf = self#of_javatype 0 fd.Ast.fd_type in

    let nd =
      self#mknode ~ordinal_tbl_opt (L.FieldDeclaration vdids)
        (mod_nodes @
         [ty_leaf] @
         (List.map self#of_variable_declarator vdtors))
    in
    set_loc nd fd.Ast.fd_loc;
    [nd]

  method of_explicit_constructor_invocation eci =
    let nd = 
      match eci.Ast.eci_desc with
      | Ast.ECIthis(targs_opt, args) ->
          let ta_nodes = self#of_type_arguments_opt "" targs_opt in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [List.length ta_nodes; 1])
          in
	  self#mknode ~ordinal_tbl_opt L.ThisInvocation 
	    (ta_nodes @ [self#of_named_arguments "this" args])

      | Ast.ECIsuper(targs_opt, args) ->
          let ta_nodes = self#of_type_arguments_opt "" targs_opt in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [List.length ta_nodes; 1])
          in
	  self#mknode ~ordinal_tbl_opt L.SuperInvocation
	    (ta_nodes @ [self#of_named_arguments "super" args])

      | Ast.ECIprimary(prim, targs_opt, args) ->
          let ta_nodes = self#of_type_arguments_opt "" targs_opt in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [1; List.length ta_nodes; 1])
          in
	  self#mknode ~ordinal_tbl_opt L.PrimaryInvocation
	    ((self#of_primary prim) :: (ta_nodes @ [self#of_arguments args]))

      | Ast.ECIname(name, targs_opt, args) ->
          let ta_nodes = self#of_type_arguments_opt "" targs_opt in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [List.length ta_nodes; 1])
          in
          let orig_lab_opt =
            Some (L.NameInvocation (L.conv_name ~resolve:false name))
          in
	  self#mknode ~orig_lab_opt ~ordinal_tbl_opt
            (L.NameInvocation (L.conv_name name)) (ta_nodes @ [self#of_arguments args])
    in
    set_loc nd eci.Ast.eci_loc;
    nd

  method of_class_body_declaration cbd =
    let loc = cbd.Ast.cbd_loc in
    let nds =
      match cbd.Ast.cbd_desc with
      | Ast.CBDfield fd -> self#of_field_declaration fd
      | Ast.CBDmethod(method_header, block_opt) -> 
	  let nd = self#of_method_header method_header in
	  begin
	    match block_opt with
	    | None -> ()
	    | Some block -> begin
		nd#add_child_rightmost
		  (self#of_method_body 
                     (self#name_of_method_header method_header) block);
                nd#data#add_to_ordinal_list [1]
            end
	  end;
	  [nd]

      | Ast.CBDclass cd      -> [self#of_class_declaration false cd]
      | Ast.CBDinterface ifd -> [self#of_interface_declaration false ifd]

      | Ast.CBDstaticInitializer block -> 
	  [self#mknode L.StaticInitializer [self#of_block block]]

      | Ast.CBDinstanceInitializer block -> 
	  [self#mknode L.InstanceInitializer [self#of_block block]]

      | Ast.CBDconstructor cd ->
          let mods = cd.Ast.cnd_modifiers in
          let tparams = cd.Ast.cnd_type_parameters in
          let params = cd.Ast.cnd_parameters in
          let throws = cd.Ast.cnd_throws in
          let orig_name = cd.Ast.cnd_name in
	  let name = orig_name^".<init>" in
	  let signature =
            Xlist.to_string self#param_to_tystr "" params
	  in
          let mod_nodes = self#of_modifiers_opt L.Kconstructor signature mods in
          let ta_nodes = self#of_type_parameters_opt signature tparams in
          let p_nodes = self#of_parameters signature cd.Ast.cnd_parameters_loc params in
          let th_nodes = self#of_throws_opt name throws in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [List.length mod_nodes;
                                   List.length ta_nodes;
                                   List.length p_nodes;
                                   List.length th_nodes;
                                   1;
                                 ])
          in
	  let children = 
	    mod_nodes @ ta_nodes @ p_nodes @ th_nodes @
	    [self#of_constructor_body name signature cd.Ast.cnd_body]
	  in
          let annot = L.make_annotation (sprintf "(%s)V" signature) in
          let orig_lab_opt = Some (L.Constructor(orig_name, signature)) in
	  let nd =
            self#mknode ~orig_lab_opt ~annot ~ordinal_tbl_opt
              (L.Constructor(name, signature)) children
          in
	  [nd]
      | Ast.CBDempty -> []
      | Ast.CBDerror -> [self#mkleaf L.Error]
    in
    List.iter 
      (fun nd -> 
        if not (is_ghost nd) then 
          set_loc nd loc
      ) nds;
    nds

  method of_constructor_body name signature cnb =
    let ctor_invk = cnb.Ast.cnb_explicit_constructor_invocation in
    let bss = cnb.Ast.cnb_block in
    let ctor_nodes = of_opt self#of_explicit_constructor_invocation ctor_invk in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length ctor_nodes; List.length bss])
    in
    let children = ctor_nodes @ (List.map self#of_block_statement bss) in
    let nd = self#mknode ~ordinal_tbl_opt (L.ConstructorBody(name, signature)) children in
    set_loc nd cnb.Ast.cnb_loc;
    nd

  method of_class_body_opt name cb = of_opt (self#of_class_body name) cb

  method of_class_body cname cb = 
    let body = cb.Ast.cb_class_body_declarations in
    let children = 
      List.flatten (List.map self#of_class_body_declaration body)
    in
    let fields, methods, ctors, classes, enums, ifaces, static_inits, inst_inits
	= ref [], ref [], ref [], ref [], ref [], ref [], ref [], ref []
    in
    let classify nd =
      match (Obj.obj nd#data#_label : L.t) with
      | L.FieldDeclaration _  -> fields := !fields @ [nd]
      | L.Method _            -> methods := !methods @ [nd]
      | L.Constructor _       -> ctors := !ctors @ [nd]
      | L.Class _             -> classes := !classes @ [nd]
      | L.Enum _              -> enums := !enums @ [nd]
      | L.Interface _         -> ifaces := !ifaces @ [nd]
      | L.AnnotationType _    -> ifaces := !ifaces @ [nd]
      | L.StaticInitializer   -> static_inits := !static_inits @ [nd]
      | L.InstanceInitializer -> inst_inits := !inst_inits @ [nd]
      | l -> WARN_MSG "%s" (L.to_string l)
    in
    let _ = List.iter classify children in

    let fields_l =
      if !fields = [] then
        []
      else
        let fields_ = self#mklnode (L.FieldDeclarations cname) !fields in
        fields_#data#set_loc Loc.ghost;
        [fields_]
    in
    let children' = 
      List.flatten 
	(List.map 
	   (* (List.stable_sort compare_node) *) (fun x -> x)
	   [ fields_l;(*[fields_];*)
	     !ctors; 
	     !methods;
	     !classes; 
	     !enums;
	     !ifaces; 
	     !static_inits; 
	     !inst_inits
	  ])
    in
    let nd = self#mklnode (L.ClassBody cname) children' in
    self#add_true_children nd (Array.of_list children);
    set_loc nd cb.Ast.cb_loc;
    nd

  method of_enum_body name eb =
    let econsts = eb.Ast.eb_enum_constants in
    let cbdecls = eb.Ast.eb_class_body_declarations in
    let dnds = List.flatten (List.map self#of_class_body_declaration cbdecls) in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length econsts; List.length dnds])
    in
    let nd = 
      self#mknode ~ordinal_tbl_opt (L.EnumBody name) 
	((List.map self#of_enum_constant econsts) @ dnds)
    in
    set_loc nd eb.Ast.eb_loc;
    nd

  method of_enum_constant ec =
    let al = ec.Ast.ec_annotations in
    let args = ec.Ast.ec_arguments in
    let ident = ec.Ast.ec_identifier in
    let a_nodes = self#of_annotations al in
    let na_nodes = self#of_named_arguments_opt ident args in
    let cb_nodes = self#of_class_body_opt ident ec.Ast.ec_class_body in
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length a_nodes;
                             List.length na_nodes;
                             List.length cb_nodes;
                           ])
    in
    let nd =
      self#mknode ~ordinal_tbl_opt (L.EnumConstant ident)
	(a_nodes @ na_nodes @ cb_nodes)
    in
    set_loc nd ec.Ast.ec_loc;
    nd

  method of_annotations ans =
    let children = List.map self#of_annotation ans in
    if children = [] then
      []
    else
      let nd = self#mklnode L.Annotations children in
      set_nodes_loc nd children;
      [nd]

  method of_annotation a =
    let nd = 
      match a.Ast.a_desc with
      | Ast.Anormal(name, evps) ->
          let orig_lab_opt =
            Some (L.Annotation (L.Annotation.Normal (L.conv_name ~resolve:false name)))
          in
	  self#mknode ~orig_lab_opt
            (L.Annotation (L.Annotation.Normal (L.conv_name name)))
	    (List.map self#of_element_value_pair evps)

      | Ast.Amarker name ->
          let orig_lab_opt =
            Some (L.Annotation (L.Annotation.Marker (L.conv_name ~resolve:false name)))
          in
	  self#mkleaf ~orig_lab_opt (L.Annotation (L.Annotation.Marker (L.conv_name name)))

      | Ast.AsingleElement(name, ev) ->
          let orig_lab_opt =
            Some (L.Annotation (L.Annotation.SingleElement (L.conv_name ~resolve:false name)))
          in
	  self#mknode ~orig_lab_opt
            (L.Annotation (L.Annotation.SingleElement (L.conv_name name)))
	    [self#of_element_value ev]
    in
    set_loc nd a.Ast.a_loc;
    nd

  method of_element_value_pair { Ast.evp_desc=(ident, ev); Ast.evp_loc=loc } =
    let nd = self#mknode (L.ElementValuePair ident) [self#of_element_value ev] in
    set_loc nd loc;
    nd

  method of_element_value ev =
    let nd =
      match ev.Ast.ev_desc with
      | Ast.EVconditional e -> self#mknode L.EVconditional [self#of_expression e]
      | Ast.EVannotation a -> self#mknode L.EVannotation [self#of_annotation a]
      | Ast.EVarrayInit evs -> 
	  self#mknode L.EVarrayInit (List.map self#of_element_value evs)
    in
    set_loc nd ev.Ast.ev_loc;
    nd

  method of_extends_class exc =
    let nd = self#mknode L.Extends [self#of_javatype 0 exc.Ast.exc_class] in
    set_loc nd exc.Ast.exc_loc;
    nd

  method of_extends_class_opt exc = of_opt self#of_extends_class exc

  method of_extends_interfaces exi =
    let nd = 
      self#mklnode L.ExtendsInterfaces 
	(List.map (self#of_javatype 0) exi.Ast.exi_interfaces) 
    in
    set_loc nd exi.Ast.exi_loc;
    nd

  method of_extends_interfaces_opt exi = of_opt self#of_extends_interfaces exi

  method of_implements im =
    let nd = 
      self#mklnode L.Implements (List.map (self#of_javatype 0) im.Ast.im_interfaces) 
    in
    set_loc nd im.Ast.im_loc;
    nd

  method of_implements_opt im = of_opt self#of_implements im

  method make_specifier_node kind children otbl loc =
    if children = [] then
      []
    else
      let ordinal_tbl_opt = Some (new ordinal_tbl otbl) in
      let nd = self#mknode ~ordinal_tbl_opt (L.Specifier kind) children in
      set_loc nd loc;
      [nd]

  method of_class_declaration_head kind otbl h =
    let ident = h.Ast.ch_identifier in
    let mod_nodes = self#of_modifiers_opt kind ident h.Ast.ch_modifiers in
    let ta_nodes = self#of_type_parameters_opt ident h.Ast.ch_type_parameters in
    let ex_nodes = self#of_extends_class_opt h.Ast.ch_extends_class in
    let im_nodes = self#of_implements_opt h.Ast.ch_implements in
    let children = mod_nodes @ ta_nodes @ ex_nodes @ im_nodes in
    self#make_specifier_node kind children otbl h.Ast.ch_loc

  method of_class_declaration is_top cd = 
    let nd =
      match cd.Ast.cd_desc with
      | Ast.CDclass(h, body) ->
          let otbl =
            [if h.Ast.ch_modifiers <> None then 1 else 0;
             if h.Ast.ch_type_parameters <> None then 1 else 0;
             if h.Ast.ch_extends_class <> None then 1 else 0;
             if h.Ast.ch_implements <> None then 1 else 0;
           ]
          in
          let ident = h.Ast.ch_identifier in
	  let specifier_node = self#of_class_declaration_head L.Kclass otbl h in
	  let children = specifier_node @ [self#of_class_body ident body] in
	  self#mknode (L.Class ident) children

      | Ast.CDenum(h, body) ->
          let otbl =
            [if h.Ast.ch_modifiers <> None then 1 else 0;
             if h.Ast.ch_implements <> None then 1 else 0;
           ]
          in
          let ident = h.Ast.ch_identifier in
	  let specifier_node = self#of_class_declaration_head L.Kenum otbl h in
	  let children = specifier_node @ [self#of_enum_body ident body] in
	  self#mknode (L.Enum ident) children
    in
    set_loc nd cd.Ast.cd_loc;
    nd

  method of_abstract_method_declaration amd =
    self#of_method_header amd.Ast.amd_method_header

  method of_interface_member_declaration = function
    | Ast.IMDconstant field_decl      -> self#of_field_declaration field_decl
    | Ast.IMDinterfaceMethod abs_meth -> [self#of_abstract_method_declaration abs_meth]
    | Ast.IMDclass class_decl         -> [self#of_class_declaration false class_decl]
    | Ast.IMDinterface iface_decl     -> [self#of_interface_declaration false iface_decl]
    | Ast.IMDempty -> []

  method of_interface_body iname ib = 
    let mems = ib.Ast.ib_member_declarations in
    let children = 
      (List.fold_left 
	 (fun l d -> l @ (self#of_interface_member_declaration d)) [] mems)
    in
    let fields, methods, classes, enums, ifaces = 
      ref [], ref [], ref [], ref [], ref []
    in
    let classify nd =
      match (Obj.obj nd#data#_label : L.t) with
      | L.FieldDeclaration _ -> fields := nd::!fields
      | L.Method _ -> methods := nd::!methods
      | L.Class _ -> classes := nd::!classes
      | L.Enum _ -> enums := nd::!enums
      | L.Interface _ -> ifaces := nd::!ifaces
      | l -> FATAL_MSG "%s" (L.to_string l); exit 1
    in
    List.iter classify children;
    let children' = 
      List.flatten (List.map (List.fast_sort compare_node)
		      [!fields; 
		       !methods;
		       !classes;
		       !enums; 
		       !ifaces
		     ])
    in
    let nd = self#mklnode (L.InterfaceBody iname) children' in
    self#add_true_children nd (Array.of_list children);
    set_loc nd ib.Ast.ib_loc;
    nd

  method of_annotation_type_body name atb =
    let nd =
      self#mklnode (L.AnnotationTypeBody name) 
	(List.flatten
	   (List.map 
	      self#of_annotation_type_member_declaration
	      atb.Ast.atb_member_declarations))
    in
    set_loc nd atb.Ast.atb_loc;
    nd

  method of_constant_declaration cd = self#of_field_declaration cd

  method of_default_value dv = self#of_element_value dv

  method of_annot_dim adim =
    let nd = self#mknode L.AnnotDim (self#of_annotations adim.Ast.ad_annotations) in
    set_loc nd adim.Ast.ad_loc;
    nd

  method of_annotation_type_member_declaration atmd =
    let nds =
      match atmd.Ast.atmd_desc with
      | Ast.ATMDconstant cd -> self#of_constant_declaration cd
      | Ast.ATMDelement(modifiers_opt, ty, ident, dl, dval_opt) ->
          let mod_nodes = self#of_modifiers_opt L.Kannotation ident modifiers_opt in
          let dval_nodes = of_opt self#of_default_value dval_opt in
          let ordinal_tbl_opt =
            Some (new ordinal_tbl [List.length mod_nodes;
                                   1;
                                   List.length dl;
                                   List.length dval_nodes])
          in
	  [self#mknode ~ordinal_tbl_opt (L.ElementDeclaration ident)
	     (mod_nodes @ [self#of_javatype 0 ty] @
              (List.map self#of_annot_dim dl) @ dval_nodes)]

      | Ast.ATMDclass cd -> [self#of_class_declaration false cd]
      | Ast.ATMDinterface ifd -> [self#of_interface_declaration false ifd]
      | Ast.ATMDempty -> []
    in
    let loc = atmd.Ast.atmd_loc in
    List.iter (fun nd -> set_loc nd loc) nds;
    nds

  method of_interface_declaration_head kind otbl h =
    let ident = h.Ast.ifh_identifier in
    let children =
      (self#of_modifiers_opt kind ident h.Ast.ifh_modifiers) @
      (self#of_type_parameters_opt ident h.Ast.ifh_type_parameters) @
      (self#of_extends_interfaces_opt h.Ast.ifh_extends_interfaces)
    in
    self#make_specifier_node kind children otbl h.Ast.ifh_loc

  method of_interface_declaration is_top ifd =
    let nd =
      match ifd.Ast.ifd_desc with
      | Ast.IFDnormal(h, body) ->
          let otbl =
            [if h.Ast.ifh_modifiers <> None then 1 else 0;
             if h.Ast.ifh_type_parameters <> None then 1 else 0;
             if h.Ast.ifh_extends_interfaces <> None then 1 else 0;
           ]
          in
          let ident = h.Ast.ifh_identifier in
	  let specifier_node = self#of_interface_declaration_head L.Kinterface otbl h in
	  let children = specifier_node @ [self#of_interface_body ident body] in
	  self#mknode (L.Interface ident) children

      | Ast.IFDannotation(h, body) ->
          let otbl =
            [if h.Ast.ifh_modifiers <> None then 1 else 0; 1]
          in
          let ident = h.Ast.ifh_identifier in
          let specifier_node = self#of_interface_declaration_head L.Kannotation otbl h in
          let children = specifier_node @ [self#of_annotation_type_body ident body] in
	  self#mknode (L.AnnotationType ident) children
    in
    set_loc nd ifd.Ast.ifd_loc;
    nd

  method of_type_declaration td =
    let nds =
      match td.Ast.td_desc with
      | Ast.TDclass class_decl -> [self#of_class_declaration true class_decl]
      | Ast.TDinterface iface_decl -> [self#of_interface_declaration true iface_decl]
      | Ast.TDempty -> []
    in
    let loc = td.Ast.td_loc in
    List.iter (fun nd -> set_loc nd loc) nds;
    nds

  method of_package_decl pd =
    let nd =
      self#mknode (L.PackageDeclaration (L.conv_name pd.Ast.pd_name)) 
	(List.map self#of_annotation pd.Ast.pd_annotations) 
    in 
    set_loc nd pd.Ast.pd_loc; 
    nd

  method of_import_decls idecls =
    match idecls with 
    | [] -> []
    | _ ->
	let of_import_decl id =
	  let nd =
	    match id.Ast.id_desc with
	    | Ast.IDsingle name -> self#mkleaf (L.IDsingle (L.conv_name ~resolve:false name))
	    | Ast.IDtypeOnDemand name -> self#mkleaf (L.IDtypeOnDemand (L.conv_name name))
	    | Ast.IDsingleStatic(name, ident) ->
		self#mkleaf (L.IDsingleStatic(L.conv_name ~resolve:false name, ident))

	    | Ast.IDstaticOnDemand name ->
		self#mkleaf (L.IDstaticOnDemand (L.conv_name name)) 
	  in
	  set_loc nd id.Ast.id_loc;
	  nd
	in
	let inodes = List.map of_import_decl idecls in
	let nd = self#mklnode L.ImportDeclarations inodes in
	set_nodes_loc nd inodes;
	[nd]

end (* of class Tree.translator *)


let of_compilation_unit options cu =
  let trans = new translator options in
  let package_decl = cu.Ast.cu_package in
  let import_decls = cu.Ast.cu_imports in
  let type_decls = cu.Ast.cu_tydecls in
  let pdecl_nodes = of_opt trans#of_package_decl package_decl in
  let idecl_nodes = trans#of_import_decls import_decls in
  let tdecl_nodes =
    match type_decls with
    | [] -> []
    | _ ->
	let td_nodes = List.flatten (List.map trans#of_type_declaration type_decls) in
	let nd = mklnode options L.TypeDeclarations td_nodes in
	set_nodes_loc nd td_nodes;
	[nd]
  in
  let compilation_unit_node =
    let ordinal_tbl_opt =
      Some (new ordinal_tbl [List.length pdecl_nodes;
                             List.length idecl_nodes;
                             List.length tdecl_nodes;
                           ])
    in
    let children = pdecl_nodes @ idecl_nodes @ tdecl_nodes in
    let nd = mknode options ~ordinal_tbl_opt L.CompilationUnit children in
    set_nodes_loc nd children;
    nd
  in
  let tree = 
    new c options compilation_unit_node true 
  in
  tree#set_true_parent_tbl trans#true_parent_tbl;
  tree#set_true_children_tbl trans#true_children_tbl;
  tree#collapse;
  trans#set_bindings tree;
  tree

let of_ast options ast =
  let tree = of_compilation_unit options ast#compilation_unit in
  tree#set_misparsed_regions ast#missed_regions;
  tree#set_misparsed_LOC ast#missed_LOC;
  tree#set_total_LOC ast#lines_read;
  tree#set_ignored_regions (ast#comment_regions @ ast#ignored_regions);
  tree
