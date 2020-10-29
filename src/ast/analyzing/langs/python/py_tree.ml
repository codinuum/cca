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
 * AST for the Python programming language
 *
 * python/tree.ml
 *
 *)


module L = Py_label

let sprintf = Printf.sprintf

let first l = 
  try List.hd l with _ -> assert false

let last l = 
  try List.hd(List.rev l) with _ -> assert false

let conv_loc = L.conv_loc

let set_loc nd loc = nd#data#set_loc (conv_loc loc)

let of_opt of_x = function
  | None -> []
  | Some x -> [of_x x]

let loc_of_name (loc, name) = loc

let dottedname_to_string dname =
  Xlist.to_string (fun n -> L.conv_name n) "." dname


module Tree = Sourcecode.Tree(L)
open Tree


class translator options = object (self)
  inherit node_maker options

  method of_opt_lab : 'a . L.t -> ('a -> node_t) -> 'a option -> node_t list = 
    fun lab of_x ->
      function
	| None -> []
	| Some x ->
	    let xnd = of_x x in
	    let nd = self#mknode lab [xnd] in
	    nd#data#set_loc xnd#data#src_loc;
	    [nd]

  method of_opt_lab_l : 'a . L.t -> ('a -> node_t list) -> 'a option -> node_t list = 
    fun lab of_x ->
      function
	| None -> []
	| Some x ->
	    let xnds = of_x x in
	    let nd = self#mknode lab xnds in
	    let loc =
	      match xnds with
	      | [n] -> n#data#src_loc
	      | [] -> assert false
	      | n::_ -> 
		  let last = Xlist.last xnds in 
		  Loc.merge n#data#src_loc last#data#src_loc
	    in
	    nd#data#set_loc loc;
	    [nd]

  method of_name (loc, name) = 
    let nd = self#mkleaf (L.Name name) in
    set_loc nd loc;
    nd

  method of_dottedname dname =
    let loc =
      match dname with
      | [] -> assert false
      | [l, n] -> l
      | (l, n)::t -> Ast.Loc.merge l (loc_of_name (last dname))
    in
    let nd = self#mknode L.DottedName (List.map self#of_name dname) in
    set_loc nd loc;
    nd


  method of_expr_suite lab (expr, suite) =
    let children = (self#of_expr expr)::[self#of_suite suite] in
    let nd = self#mknode lab children in
    nd

  method of_elif (loc, expr, suite) =
    let nd = self#of_expr_suite L.Elif (expr, suite) in
    set_loc nd loc;
    nd

  method of_elifs elifs = List.map self#of_elif elifs

  method of_else (loc, suite) =
    let nd = self#mknode L.Else [self#of_suite suite] in
    set_loc nd loc;
    nd

  method of_finally (loc, suite) =
    let nd = self#mknode L.Finally [self#of_suite suite] in
    set_loc nd loc;
    nd

  method of_statement stmt =
    let stmtd = stmt.Ast.stmt_desc in
    let mkstmtnode = self#mknode (L.of_statement stmtd) in
    let nd =
      match stmtd with
      | Ast.Ssimple sstmts ->
	  (match sstmts with
	    [sstmt] -> self#of_simplestmt sstmt
	  | _ -> mkstmtnode (List.map self#of_simplestmt sstmts))

      | Ast.Sif(expr, suite, elifs, else_opt) ->
       	  mkstmtnode
	    ([self#of_expr expr; self#of_suite suite] 
	     @ (self#of_elifs elifs) @ (of_opt self#of_else else_opt))

      | Ast.Swhile(expr, suite, else_opt) ->
	  mkstmtnode
	    ([self#of_expr expr; self#of_suite suite] @ (of_opt self#of_else else_opt))

      | Ast.Sfor(targs, exprs, suite, else_opt) ->
	  mkstmtnode
	    (
	     (self#of_targs L.Targets targs)::(self#of_exprs L.In exprs)
	     ::(self#of_suite suite)::(of_opt self#of_else else_opt)
	    )

      | Ast.Stry(suite, excepts, else_opt, fin_opt) ->
	  mkstmtnode
	    (((self#of_suite suite)::(self#of_excepts excepts)) 
	     @ (of_opt self#of_else else_opt) @ (of_opt self#of_finally fin_opt))

      | Ast.Stryfin(suite1, fin) ->
	  mkstmtnode [self#of_suite suite1; self#of_finally fin]

      | Ast.Swith(withitems, suite) ->
	  mkstmtnode 
	    ((List.map self#of_withitem withitems) @ [self#of_suite suite])

      | Ast.Sasync_funcdef(decos, name, params, retann_opt, suite) ->
	  mkstmtnode
	    ((self#of_decorators name decos) @ [self#of_name name] @
	     (self#of_named_parameters (L.conv_name name) params) @
             (self#of_opt_lab L.ReturnAnnotation self#of_expr retann_opt) @
	     [self#of_named_suite name suite])

      | Ast.Sfuncdef(decos, name, params, retann_opt, suite) ->
	  mkstmtnode
	    ((self#of_decorators name decos) @ [self#of_name name] @
	     (self#of_named_parameters (L.conv_name name) params) @
             (self#of_opt_lab L.ReturnAnnotation self#of_expr retann_opt) @
	     [self#of_named_suite name suite])

      | Ast.Sclassdef(decos, name, arglist, suite) ->
	  let c = [self#of_named_suite name suite] in
	  let c =
            match arglist with
            | loc, [] when loc = Ast.Loc.dummy -> c
            | loc, [] ->
                let nd = self#mkleaf L.Inheritance in
                set_loc nd loc;
                nd::c
            | loc, _ ->
                let nd = self#mknode L.Inheritance (self#of_named_arglist (L.conv_name name) arglist) in
                set_loc nd loc;
                nd::c
	  in
	  let c = (self#of_decorators name decos) @ ((self#of_name name)::c) in
	  mkstmtnode c

      | Ast.Sasync stmt -> mkstmtnode [self#of_statement stmt]
    in
    set_loc nd stmt.Ast.stmt_loc;
    nd

  method of_withitem (expr, targ_opt) =
    let expr_nd = self#of_expr expr in
    let targ_opt_nd = of_opt self#of_targ targ_opt in
    let nd = 
      self#mknode L.WithItem (expr_nd::targ_opt_nd)
    in
    let eloc = expr_nd#data#src_loc in
    let loc =
      match targ_opt_nd with
      | [] -> eloc
      | [nd] -> Loc.merge eloc nd#data#src_loc
      | _ -> assert false
    in
    nd#data#set_loc loc;
    nd

  method of_targs lab = self#of_exprs lab

  method of_exprs lab exprs =
    if exprs = [] then 
      self#mkleaf lab
    else
      let children = List.map self#of_expr exprs in
      let nd = self#mknode lab children in
      let loc = 
	Loc._merge (first children)#data#src_loc (last children)#data#src_loc 
      in
      nd#data#set_loc loc;
      nd

  method of_except (except_clause, suite) =
    let l, exc = self#of_except_clause except_clause in
    let snd = self#of_suite suite in
    let loc = Loc._merge l snd#data#src_loc in
    let children = exc @ [snd] in
    let nd = self#mknode L.Except children in
    nd#data#set_loc loc;
    nd

  method of_excepts es = List.map self#of_except es

  method of_decorators name decos =
    if decos = [] then []
    else
      let children = List.map self#of_decorator decos in
      let loc = 
	Loc._merge (first children)#data#src_loc (last children)#data#src_loc 
      in
      let nd = self#mknode (L.Decorators (L.conv_name name)) children in
      nd#data#set_loc loc;
      [nd]
	
  method of_simplestmt sstmt =
    let sstmtd = sstmt.Ast.sstmt_desc in
    let lab = L.of_simplestmt sstmtd in
    let mksstmtnode = self#mknode lab in
    let nd = 
      match sstmtd with
      | Ast.SSexpr exprs ->
	  (match exprs with
	  | [expr] -> self#of_expr expr
	  | _ -> self#of_exprs (L.of_simplestmt sstmtd) exprs)
      | Ast.SSassign(testlist_list, testlist) ->
	  let targs_list = List.map (fun tl -> tl.Ast.list) testlist_list in
	  let exprs = testlist.Ast.list in
	  mksstmtnode
	    ((List.map (fun targs -> self#of_targs L.LHS targs) targs_list) @
	     [self#of_exprs L.RHS exprs])

      | Ast.SSaugassign(targs, augop, testlist) ->
	  let exprs = testlist.Ast.list in
	  mksstmtnode
	    [self#of_targs L.LHS targs; self#of_exprs L.RHS exprs]

      | Ast.SSannassign(targs, expr, testlist_opt) ->
          let exprs =
            match testlist_opt with
            | Some testlist -> testlist.Ast.list
            | None -> []
          in
          mksstmtnode [self#of_targs L.LHS targs; self#of_expr expr; self#of_exprs L.RHS exprs]

      | Ast.SSprint exprs -> self#of_exprs lab exprs
      | Ast.SSprintchevron(expr, exprs) ->
	  mksstmtnode
	    ([self#mknode L.Chevron [self#of_expr expr]] @ (List.map self#of_expr exprs))

      | Ast.SSdel targs -> self#of_targs lab targs
      | Ast.SSpass -> self#mkleaf lab
      | Ast.SSbreak -> self#mkleaf lab
      | Ast.SScontinue -> self#mkleaf lab
      | Ast.SSreturn exprs -> self#of_exprs lab exprs
      | Ast.SSraise -> self#mkleaf lab
      | Ast.SSraise1 expr -> self#of_exprs lab [expr]
      | Ast.SSraise2(expr1, expr2) -> self#of_exprs lab [expr1; expr2]
      | Ast.SSraisefrom(expr1, expr2) -> self#of_exprs lab [expr1; expr2]
      | Ast.SSraise3(expr1, expr2, expr3) -> self#of_exprs lab [expr1; expr2; expr3]
      | Ast.SSyield exprs -> self#of_exprs lab exprs
      | Ast.SSimport dottedname_as_names ->
	  mksstmtnode (List.map self#of_dottedname_as_name dottedname_as_names)
	    
      | Ast.SSfrom(dots_opt, dottedname_opt, name_as_names) -> begin
          let from_children, from_loc =
            match dots_opt, dottedname_opt with
            | None,      Some dottedname -> begin
                let dnnd = self#of_dottedname dottedname in
                [dnnd], dnnd#data#src_loc
            end
            | Some dots, Some dottedname -> begin
                let dnd = self#of_dots dots in
                let dnnd = self#of_dottedname dottedname in
                [dnd; dnnd], Loc._merge dnd#data#src_loc dnnd#data#src_loc
            end
            | Some dots, None -> begin
                let dnd = self#of_dots dots in
                [dnd], dnd#data#src_loc
            end
            | _ -> assert false
          in
          let from_nd = self#mknode L.From from_children in
          from_nd#data#set_loc from_loc;
          mksstmtnode (from_nd::(List.map self#of_name_as_name name_as_names))
      end
      | Ast.SSglobal names -> begin
	  mksstmtnode
	    (List.map
	       (fun name ->
		 let n = self#mkleaf (L.Name (L.conv_name name)) in
		 set_loc n (L.loc_of_name name);
		 n
	       )
	       names)
      end
      | Ast.SSnonlocal names -> begin
	  mksstmtnode
	    (List.map
	       (fun name ->
		 let n = self#mkleaf (L.Name (L.conv_name name)) in
		 set_loc n (L.loc_of_name name);
		 n
	       )
	       names)
      end
      | Ast.SSexec expr -> self#of_exprs lab [expr]
      | Ast.SSexec2(expr1, expr2) -> self#of_exprs lab [expr1; expr2]
      | Ast.SSexec3(expr1, expr2, expr3) -> self#of_exprs lab [expr1; expr2; expr3]
      | Ast.SSassert expr -> self#of_exprs lab [expr]
      | Ast.SSassert2(expr1, expr2) -> self#of_exprs lab [expr1; expr2]
    in
    set_loc nd sstmt.Ast.sstmt_loc;
    nd

  method of_dots (loc, ndots) =
    let nd = self#mkleaf (L.Dots ndots) in
    set_loc nd loc;
    nd
      
  method of_dottedname_as_name (dname, name_opt) =
    let dname_nd = self#of_dottedname dname in
    match name_opt with
    | None -> dname_nd
    | Some name -> 
	self#mknode L.As [dname_nd; self#mkleaf (L.Name (L.conv_name name))]

  method of_name_as_name (name, name_opt) =
    let name_nd = self#of_name name in
    match name_opt with
    | None -> name_nd
    | Some name -> 
	self#mknode L.As [name_nd; self#mkleaf (L.Name (L.conv_name name))]

  method of_except_clause = function
    | Ast.EX loc -> conv_loc loc, []
    | Ast.EX1(loc, expr) -> conv_loc loc, [self#of_expr expr]
    | Ast.EX2(loc, expr, targ) -> conv_loc loc, [self#of_expr expr; self#of_targ targ]

  method of_suite (loc, stmts) =
    let nd = self#mknode L.Suite (List.map self#of_statement stmts) in
    set_loc nd loc;
    nd

  method of_named_suite name (loc, stmts) =
    let nd = 
      self#mknode (L.NamedSuite (L.conv_name name)) 
	(List.map self#of_statement stmts) 
    in
    set_loc nd loc;
    nd

  method of_parameters (loc, dparams) =
    match dparams with
    | [] -> []
    | _ -> 
	let children = List.map self#of_vararg dparams in
	let nd = self#mknode L.Parameters children in
	set_loc nd loc;
	[nd]

  method of_vararg = function
    | Ast.VAarg(fpdef, expr_opt) -> begin
        let children = (self#of_fpdef fpdef)::(of_opt self#of_expr expr_opt) in
        if children = [] then self#mkleaf L.DefParameter
        else
          let loc = 
	    Loc._merge (first children)#data#src_loc (last children)#data#src_loc 
          in
          let nd = self#mknode L.DefParameter children in
          nd#data#set_loc loc;
          nd
    end
    | Ast.VAargs(loc, None)     -> let nd = self#mkleaf L.Tuple in set_loc nd loc; nd
    | Ast.VAargs(loc, (Some n)) -> let nd = self#mknode L.Tuple [self#of_name n] in set_loc nd loc; nd
    | Ast.VAkwargs(loc, n)      -> let nd = self#mknode L.Dict [self#of_name n] in set_loc nd loc; nd

  method of_named_parameters name (loc, dparams) =
    match dparams with
    | [] -> []
    | _ -> 
	let children = List.map self#of_vararg dparams in
	let nd = self#mknode (L.NamedParameters name) children in
	set_loc nd loc;
	[nd]

  method of_fpdef = function
    | Ast.Fname name -> self#of_name name
    | Ast.Ftyped(loc, name, expr) -> begin
        let nd = self#mknode L.Typed [self#of_name name; self#of_expr expr] in
        set_loc nd loc;
        nd
    end
    | Ast.Flist(loc, fpdefs) -> begin
	let children = List.map self#of_fpdef fpdefs in
	let nd = self#mknode L.Sublist children in
	set_loc nd loc;
	nd
    end

  method of_decorator (loc, dname, arglist) =
    let dname_str = L.dotted_name_to_string dname in
    let children = (self#of_dottedname dname)::(self#of_named_arglist dname_str arglist) in
    let nd = self#mknode (L.Decorator dname_str) children in
    set_loc nd loc;
    nd


  method of_expr expr =
    let nd = 
      match expr.Ast.expr_desc with
      | Ast.Eprimary prim -> self#of_primary prim
      | Ast.Epower(prim, expr) -> self#mknode L.Power [self#of_primary prim; self#of_expr expr]
      | Ast.Ebop(expr1, bop, expr2) -> self#of_exprs (L.of_bop bop) [expr1; expr2]
      | Ast.Euop(uop, expr) -> self#of_exprs (L.of_uop uop) [expr]
      | Ast.Elambda(params, expr) -> self#mknode L.Lambda ((self#of_parameters params) @ [self#of_expr expr])
      | Ast.Econd(expr1, expr2, expr3) -> self#of_exprs L.Test [expr1; expr2; expr3]
      | Ast.Estar expr -> self#of_exprs L.Star [expr]
      | Ast.Enamed(expr1, expr2) -> self#of_exprs L.Named [expr1; expr2]
      | Ast.Efrom expr -> self#of_exprs L.From [expr]
      | Ast.Earg(expr1, expr2) -> self#of_exprs L.Named [expr1; expr2]
    in
    set_loc nd expr.Ast.expr_loc;
    nd


  method of_primary_desc primd =
    let lab = L.of_primary primd in
    let mkprimnode = self#mknode lab in
    let nd = 
      match primd with
      | Ast.Pname name -> self#mkleaf lab
      | Ast.Pliteral lit ->
	  (match lit with
	    Ast.Lstring pystrs ->
	      mkprimnode 
		(List.map 
		   (fun pystr ->
		     let str, loc = 
		       match pystr with
		       | Ast.PSshort(l, s) -> String.sub s 1 ((String.length s) - 2), l
		       | Ast.PSlong(l, s) -> String.sub s 3 ((String.length s) - 6), l
		     in
		     let n = self#mkleaf (L.StringLiteral str) in
		     set_loc n loc; 
		     n
		   ) pystrs)
	  | _ -> self#mkleaf lab)
	    
      | Ast.Pparen expr -> self#of_exprs lab [expr]
      | Ast.Ptuple exprs -> self#of_exprs lab exprs
      | Ast.Pyield exprs -> self#of_exprs lab exprs
      | Ast.PcompT(expr, compfor) -> mkprimnode [self#of_expr expr; self#of_compfor compfor]
      | Ast.PcompL(expr, compfor) -> mkprimnode [self#of_expr expr; self#of_compfor compfor]
      | Ast.Plist exprs -> self#of_exprs lab exprs
      | Ast.Plistnull -> self#mkleaf lab
      | Ast.Pdictorset dictorsetmaker -> mkprimnode (self#of_dictorsetmaker dictorsetmaker)
      | Ast.Pdictnull -> self#mkleaf lab
      | Ast.Pstrconv exprs -> self#of_exprs lab exprs
      | Ast.Pattrref(prim, name) -> self#mknode lab [self#of_primary prim; self#of_name name]
      | Ast.Psubscript(prim, exprs) -> mkprimnode ((self#of_primary prim)::(List.map self#of_expr exprs))
      | Ast.Pslice(prim, sliceitems) -> mkprimnode ((self#of_primary prim)::(List.map self#of_sliceitem sliceitems))
      | Ast.Pcall(prim, arglist) -> begin
          let tid = L.tid_of_primary prim in
          let lab = L.Primary (L.Primary.Call tid) in
          self#mknode lab ((self#of_primary prim)::(self#of_arglist tid arglist))
      end
      | Ast.Pawait prim -> mkprimnode [self#of_primary prim]
    in
    nd

  method of_primary prim =
    let primd = prim.Ast.prim_desc in
    let nd = self#of_primary_desc primd in
    set_loc nd prim.Ast.prim_loc;
    nd

  method of_targ t = self#of_expr t

  method of_listfor (loc, exprs1, exprs2, listiter_opt) =
    let children =
      let tgnd = self#of_exprs L.Target exprs1 in
      let innd = self#of_exprs L.In exprs2 in
      [tgnd; innd] @ (of_opt self#of_listiter listiter_opt)
    in
    let nd = self#mknode (L.Primary L.Primary.ListFor) children in
    set_loc nd loc;
    nd

  method of_listif (loc, expr, listiter_opt) =
    let children = (self#of_expr expr)::(of_opt self#of_listiter listiter_opt) in
    let nd = self#mknode L.ListIf children in
    set_loc nd loc;
    nd

  method of_listiter = function
    | Ast.LIfor listfor -> self#of_listfor listfor
    | Ast.LIif listif -> self#of_listif listif

  method of_dictelem delem =
    let nd =
      match delem.Ast.delem_desc with
      | DEkeyValue(e1, e2) -> self#of_exprs L.KeyDatum [e1; e2]
      | DEstarStar e -> self#of_exprs L.StarStar [e]
    in
    set_loc nd delem.Ast.delem_loc;
    nd

  method of_dictorsetmaker = function
    | Ast.DSMdict key_datums       -> List.map self#of_dictelem key_datums
    | Ast.DSMdictC(delem, compfor) -> [self#of_dictelem delem; self#of_compfor compfor]
    | Ast.DSMset es                -> List.map self#of_expr es
    | Ast.DSMsetC(e, compfor)      -> [self#of_expr e; self#of_compfor compfor]

  method of_sliceitem = function
    | Ast.SIexpr expr -> self#of_expr expr

    | Ast.SIproper(loc, expr1_opt, expr2_opt, expr3_opt) ->
	let children =
	  (self#of_opt_lab L.Lower self#of_expr expr1_opt) @
	  (self#of_opt_lab L.Upper self#of_expr expr2_opt) @
	  (self#of_opt_lab L.Stride self#of_expr expr3_opt)
	in
	let nd = self#mknode L.SliceItem children in
	set_loc nd loc;
	nd

    | Ast.SIellipsis loc ->
	let nd = self#mkleaf L.SliceItemEllipsis in
	set_loc nd loc;
	nd

  method of_arglist tid (loc, args) =
    match args with
    | [] -> []
    | _ -> begin
        let children = List.map self#of_argument args in
        let nd = self#mknode (L.Arguments tid) children in
        set_loc nd loc;
        [nd]
    end

  method of_named_arglist name (loc, args) =
    match args with
    | [] -> []
    | _ -> begin
	let children = List.map self#of_argument args in
	let nd = self#mknode (L.NamedArguments name) children in
	set_loc nd loc;
	[nd]
    end

  method of_argument = function
    | Aarg(loc, expr, expr_opt) -> begin
        let children =
          match expr_opt with
          | Some e -> [self#of_expr expr; self#of_expr e]
          | None -> [self#of_expr expr]
        in
        let nd = self#mknode L.Argument children in
        set_loc nd loc;
        nd
    end
    | Acomp(loc, expr, compfor) -> begin
        let nd = self#mknode L.CompArgument [self#of_expr expr; self#of_compfor compfor] in
        set_loc nd loc;
        nd
    end
    | Aassign(loc, expr1, expr2) -> begin
        let nd = self#mknode L.AssignArgument [self#of_expr expr1; self#of_expr expr2] in
        set_loc nd loc;
        nd
    end
    | Aargs(loc, expr) -> begin
        let nd = self#mknode L.StarArgument [self#of_expr expr] in
        set_loc nd loc;
        nd
    end
    | Akwargs(loc, expr) -> begin
        let nd = self#mknode L.StarStarArgument [self#of_expr expr] in
        set_loc nd loc;
        nd
    end

  method of_compiter = function
    | Ast.Cfor compfor -> self#of_compfor compfor
    | Ast.Cif compif -> self#of_compif compif

  method of_compif (loc, expr, compiter_opt) =
    let children = (self#of_expr expr)::(of_opt self#of_compiter compiter_opt) in
    let nd = self#mknode L.GenIf children in
    set_loc nd loc;
    nd

  method of_compfor (loc, (exprs, expr, compiter_opt), async) =
    let children = 
      [self#of_exprs L.Target exprs;
       self#of_exprs L.In [expr]] @
      (of_opt self#of_compiter compiter_opt)
    in
    let lab =
      if async then
        L.AsyncGenFor
      else
        L.GenFor
    in
    let nd = self#mknode lab children in
    set_loc nd loc;
    nd

end (* of class Python.Tree.translator *)


let of_fileinput options fname = 
  let trans = new translator options in function (* toplevel convert function *)
    | Ast.Fileinput(loc, stmts) ->
	let children = List.map trans#of_statement stmts in
	let nd = mknode options (L.FileInput "") children in
	set_loc nd loc;
	let tree = new c options nd true in
	tree#collapse;
	tree#init;
	tree

let of_ast options fname ast =
  let tree = of_fileinput options fname ast#fileinput in
  tree#set_misparsed_regions ast#missed_regions;
  tree#set_misparsed_LOC ast#missed_LOC;
  tree#set_total_LOC ast#lines_read;
  tree#set_ignored_regions (ast#comment_regions @ ast#ignored_regions);
  tree
