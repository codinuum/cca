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
 * A pretty printer for the Python programming language 
 *
 * printer.ml
 *
 *)

open Printf
open Ast
open Common

let indent_unit = ref 4

let pr_string s = print_string s
let pr_space() = print_string " "
let pr_comma() = print_string ","
let pr_period() = print_string "."
let pr_colon() = print_string ":"
let pr_semicolon() = print_string ";"
let pr_equal() = print_string "="
let pr_null() = ()
let pr_newline() = print_newline()

let rec pr_list pr sep = function
  | [] -> ()
  | [x] -> pr x
  | h::t -> pr h; sep(); pr_list pr sep t

let pr_opt pr = function None -> () | Some x -> pr x

let pr_indent level = pr_string (String.make (level * !indent_unit) ' ')

let pr_name = function _, n -> pr_string n

let pr_names names = pr_list pr_name pr_comma names

let pr_dottedname dname = pr_list pr_name pr_period dname



let rec pr_fileinput = function
  | Fileinput(_, finput) ->
      pr_list (pr_statement 0) pr_null finput

and pr_statement level stmt =
  match stmt.stmt_desc with
  | Ssimple sstmts ->
      pr_indent level;
      pr_list pr_smallstmt pr_semicolon sstmts; 
      pr_newline()

  | Sif(cnd, thn, elifs, else_opt) ->
      pr_expr_suite level "if" (cnd, thn);
      pr_list (pr_elif level) pr_null elifs;
      pr_else_opt level else_opt

  | Swhile(cnd, suite, else_opt) -> 
      pr_expr_suite level "while" (cnd, suite);
      pr_else_opt level else_opt

  | Sfor(targs, exprs, suite, else_opt) ->
      pr_indent level;
      pr_string "for ";
      pr_list pr_target pr_comma targs;
      pr_string " in ";
      pr_exprs exprs;
      pr_colon();
      pr_suite level suite;
      pr_else_opt level else_opt

  | Stry(suite, except_suites, else_opt, fin_opt) ->
      pr_indent level;
      pr_string "try:";
      pr_suite level suite;
      pr_list
	(function 
	    EX _, suite -> 
	      pr_indent level; pr_string "except:"; pr_suite level suite

	  | EX1(_, expr), suite ->
	      pr_indent level; 
	      pr_string "except "; 
	      pr_expr expr;
	      pr_colon();
	      pr_suite level suite

	  | EX2(_, expr, targ), suite ->
	      pr_indent level;
	      pr_string "except "; 
	      pr_expr expr;
	      pr_comma();
	      pr_target targ;
	      pr_colon();
	      pr_suite level suite

	) pr_null except_suites;
      pr_else_opt level else_opt;
      pr_finally_opt level fin_opt

  | Stryfin(suite1, (_, suite2)) ->
      pr_indent level;
      pr_string "try:";
      pr_suite level suite1;
      pr_indent level;
      pr_string "finally:";
      pr_suite level suite2;

  | Swith(withitems, suite) ->
      pr_indent level;
      pr_string "with ";
      pr_list pr_with_item pr_comma withitems;
      pr_colon();
      pr_suite level suite

  | Sfuncdef(decs, name, params, suite) ->
      pr_indent level;
      pr_list pr_decorator pr_null decs;
      pr_string "def ";
      pr_name name;
      pr_string "(";
      pr_parameters params;
      pr_string "):";
      pr_suite level suite

  | Sclassdef(decs, name, exprs, suite) ->
      pr_indent level;
      pr_list pr_decorator pr_null decs;
      pr_string "class ";
      pr_name name;
      (match exprs with 
	[] -> () 
      | _ -> pr_string "("; pr_exprs exprs; pr_string ")");
      pr_colon();
      pr_suite level suite

and pr_with_item (expr, targ_opt) =
  pr_expr expr;
  begin
    match targ_opt with
    | None -> ()
    | Some targ -> pr_string " as "; pr_target targ
  end


and pr_else_opt level = 
  pr_opt 
    (fun (_, suite) -> 
      pr_indent level; pr_string "else:"; pr_suite level suite)

and pr_finally_opt level = 
  pr_opt 
    (fun (_, suite) -> 
      pr_indent level; pr_string "finally:"; pr_suite level suite)

and pr_elif level (_, expr, suite) = pr_expr_suite level "elif" (expr, suite)

and pr_expr_suite level kw (expr, suite) =
  pr_indent level;
  pr_string kw;
  pr_space();
  pr_expr expr;
  pr_colon();
  pr_suite level suite

and pr_exprs exprs = pr_list pr_expr pr_comma exprs

and pr_testlist testlist = 
  if testlist.yield then pr_string "yield";
  pr_list pr_expr pr_comma testlist.list

and pr_targs ts = pr_exprs ts

and pr_smallstmt sstmt =
  match sstmt.sstmt_desc with
  | SSexpr exprs -> pr_list pr_expr pr_comma exprs

  | SSassign(testlist_list, testlist) ->
      pr_list pr_testlist pr_equal testlist_list;
      pr_equal();
      pr_testlist testlist

  | SSaugassign(targs, augop, testlist) ->
      pr_targs targs;
      pr_space();
      pr_augop augop;
      pr_space();
      pr_testlist testlist

  | SSprint exprs -> pr_string "print "; pr_exprs exprs

  | SSprintchevron(expr, exprs) ->
      pr_string "print>>";
      pr_expr expr;
      pr_comma();
      pr_exprs exprs

  | SSdel(targs) -> pr_string "del "; pr_targs targs

  | SSpass -> pr_string "pass"
  | SSbreak -> pr_string "break"
  | SScontinue -> pr_string "continue"

  | SSreturn exprs -> pr_string "return "; pr_exprs exprs

  | SSraise -> pr_string "raise"

  | SSraise1 expr -> pr_string "raise "; pr_expr expr

  | SSraise2(expr1, expr2) ->
      pr_string "raise ";
      pr_expr expr1;
      pr_comma();
      pr_expr expr2;

  | SSraise3(expr1, expr2, expr3) ->
      pr_string "raise ";
      pr_expr expr1;
      pr_comma();
      pr_expr expr2;
      pr_comma();
      pr_expr expr3;

  | SSyield exprs -> pr_string "yield "; pr_exprs exprs

  | SSimport(dname_as_names) ->
      pr_string "import ";
      pr_list pr_dottedname_as_name pr_comma dname_as_names

  | SSfrom(dname, name_as_names) ->
      pr_string "from ";
      pr_dottedname dname;
      pr_string " import ";
      (match name_as_names with
	[] -> pr_string "*"
      | _ -> pr_list pr_name_as_name pr_comma name_as_names)

  | SSglobal names -> pr_string "global "; pr_names names

  | SSexec expr -> pr_string "exec "; pr_expr expr

  | SSexec2(expr1, expr2) -> 
      pr_string "exec "; 
      pr_expr expr1;
      pr_string " in ";
      pr_expr expr2

  | SSexec3(expr1, expr2, expr3) ->
      pr_string "exec "; 
      pr_expr expr1;
      pr_string " in ";
      pr_expr expr2;
      pr_comma();
      pr_expr expr3

  | SSassert expr -> pr_string "assert "; pr_expr expr

  | SSassert2(expr1, expr2) ->
      pr_string "assert ";
      pr_expr expr1;
      pr_comma();
      pr_expr expr2

and pr_dottedname_as_name (dname, name_opt) =
  pr_dottedname dname;
  pr_opt (fun name -> pr_string " as "; pr_name name) name_opt

and pr_name_as_name (name, name_opt) =
  pr_name name;
  pr_opt (fun name -> pr_string " as "; pr_name name) name_opt

and pr_suite level (_, stmts) =
    match stmts with
      [{stmt_desc=(Ssimple _); stmt_loc=l} as sstmt] -> pr_statement 0 sstmt
    | _ ->
	let level' = level + 1 in
	pr_newline();
	pr_list (pr_statement level') pr_null stmts


and pr_parameters (_, vargs, tini, dini) = 
  pr_vargs vargs;
  (match tini, dini with
  | None, None -> ()
  | Some t, None -> pr_string "*"; pr_name t
  | None, Some d -> pr_string "**"; pr_name d
  | Some t, Some d -> 
      pr_string "*"; pr_name t; pr_string ",**"; pr_name d)

and pr_vargs vargs =
  let pr_varg (fpdef, expr_opt) =
    pr_fpdef fpdef;
    pr_opt (fun expr -> pr_equal(); pr_expr expr) expr_opt
  in
  pr_list pr_varg pr_comma vargs

and pr_fpdef = function 
  | Fname name -> pr_name name
  | Flist(_, fpdefs) -> 
      pr_string "("; pr_list pr_fpdef pr_comma fpdefs; pr_string ")"

and pr_decorator (_, dname, arglist) =
  pr_string "@";
  pr_dottedname dname;
  pr_arglist arglist;
  pr_newline();

and pr_expr expr =
  match expr.expr_desc with
  | Eprimary prim -> pr_primary prim

  | Epower(prim, expr) -> pr_primary prim; pr_string "**"; pr_expr expr

  | Ebop(expr1, bop, expr2) ->
      pr_expr expr1;
      pr_bop bop;
      pr_expr expr2;

  | Euop(uop, expr) -> pr_uop uop; pr_expr expr

  | Elambda(params, expr) ->
      pr_string "lambda";
      (match params with 
	_, [], None, None -> () 
      | _ -> pr_space(); pr_parameters params);
      pr_colon();
      pr_expr expr

  | Econd(expr1, expr2, expr3) ->
      pr_expr expr1;
      pr_string "if";
      pr_expr expr2;
      pr_string "else";
      pr_expr expr3

and pr_primary prim =
  match prim.prim_desc with
  | Pname name -> pr_name name

  | Pliteral lit -> pr_literal lit
	
  | Pparen expr -> pr_string "("; pr_expr expr; pr_string ")"

  | Ptuple exprs -> pr_string "(";  pr_exprs exprs; pr_string ")"

  | Pyield exprs -> pr_string "(yield"; pr_exprs exprs; pr_string ")"

  | Pcomp(expr, compfor) -> 
      pr_string "(";
      pr_expr expr;
      pr_space();
      pr_compfor compfor;
      pr_string ")"

  | Plist listmaker -> 
      pr_string "["; pr_listmaker listmaker; pr_string "]"

  | Plistnull -> pr_string "[]"

  | Pdictorset dictorsetmaker -> 
      pr_string "{"; pr_dictorsetmaker dictorsetmaker; pr_string "}"

  | Pdictnull -> pr_string "{}"

  | Pstrconv exprs -> pr_string "`"; pr_exprs exprs; pr_string "`"

  | Pattrref(prim, name) -> pr_primary prim; pr_period(); pr_name name

  | Psubscript(prim, exprs) ->
      pr_primary prim; pr_string "["; pr_exprs exprs; pr_string "]"

  | Pslice(prim, sliceitems) ->
      pr_primary prim; 
      pr_string "["; 
      pr_list pr_sliceitem pr_comma sliceitems;
      pr_string "]"
	
  | Pcall(prim, arglist) ->
      pr_primary prim; 
      pr_string "(";
      pr_arglist arglist;
      pr_string ")"

and pr_literal = function
  | Linteger str -> pr_string str
  | Llonginteger str -> pr_string str
  | Lfloatnumber str -> pr_string str
  | Limagnumber str -> pr_string str
  | Lstring pystrs -> 
      pr_list 
	(function 
	    PSlong(_, s) -> pr_string s
	  | PSshort(_, s) -> pr_string s
	) pr_space pystrs

and pr_target x = pr_expr x

and pr_listmaker = function
  | LMfor(expr, listfor) -> pr_expr expr; pr_space(); pr_listfor listfor

  | LMtest exprs -> pr_exprs exprs

and pr_listfor (_, exprs1, exprs2, listiter_opt) =
  pr_string "for ";
  pr_exprs exprs1;
  pr_string " in ";
  pr_exprs exprs2;
  pr_opt (fun listiter -> pr_space(); pr_listiter listiter) listiter_opt

and pr_listif (_, expr, listiter_opt) =
  pr_string "if ";
  pr_expr expr;
  pr_opt (fun listiter -> pr_space(); pr_listiter listiter) listiter_opt

and pr_listiter = function 
  | LIfor listfor -> pr_listfor listfor
  | LIif listif -> pr_listif listif

and pr_dictorsetmaker = function
  | DSMdict key_dats ->
      pr_list
	(fun (_, e1, e2) -> pr_expr e1; pr_colon(); pr_expr e2)
	pr_comma key_dats

  | DSMdictC(e1, e2, compfor) ->
      pr_expr e1;
      pr_colon();
      pr_expr e2;
      pr_space();
      pr_compfor compfor

  | DSMset es -> pr_list pr_expr pr_comma es

  | DSMsetC(e, compfor) ->
      pr_expr e;
      pr_space();
      pr_compfor compfor

and pr_expr_opt expr_opt = pr_opt pr_expr expr_opt

and pr_sliceitem = function
  | SIexpr expr -> pr_expr expr
  | SIproper(_, expr_opt1, expr_opt2, expr_opt3) ->
      pr_expr_opt expr_opt1; 
      pr_colon(); 
      pr_expr_opt expr_opt2; 
      pr_colon(); 
      pr_expr_opt expr_opt3

  | SIellipsis _ -> pr_string "..."

and pr_arglist (_, args, tini, dini) =
  pr_list pr_argument pr_comma args;
  (match tini, dini with
  | None, None -> ()
  | Some (t, al), None -> 
      pr_string "*"; 
      pr_expr t; 
      pr_list (fun a -> pr_comma(); pr_argument a) pr_null al
  | None, Some d -> 
      pr_string "**"; 
      pr_expr d
  | Some (t, al), Some d -> 
      pr_string "*"; 
      pr_expr t;
      pr_list (fun a -> pr_comma(); pr_argument a) pr_null al;
      pr_string ",**"; pr_expr d)


and pr_argument (_, expr_opt, expr, compfor_opt) =
  pr_opt (fun expr -> pr_expr expr; pr_equal()) expr_opt;
  pr_expr expr;
  pr_opt (fun compfor -> pr_space(); pr_compfor compfor) compfor_opt

and pr_compiter = function
  | Cfor compfor -> pr_compfor compfor
  | Cif compif -> pr_compif compif

and pr_compif (_, expr, compiter_opt) =
  pr_string "if ";
  pr_expr expr;
  pr_opt (fun compiter -> pr_space(); pr_compiter compiter) compiter_opt

and pr_compfor (_, exprs, expr, compiter_opt) =
  pr_string "for ";
  pr_exprs exprs;
  pr_string " in ";
  pr_expr expr;
  pr_opt (fun compiter -> pr_space(); pr_compiter compiter) compiter_opt

and pr_augop = function
  | AaddEq -> pr_string "+="
  | AsubEq -> pr_string "-="
  | AmulEq -> pr_string "*="
  | AdivEq -> pr_string "/="
  | AmodEq -> pr_string "%="
  | AandEq -> pr_string "&="
  | AorEq -> pr_string "|="
  | AxorEq -> pr_string "^="
  | AshiftLEq -> pr_string "<<="
  | AshiftREq -> pr_string ">>="
  | ApowEq -> pr_string "**="
  | AfdivEq -> pr_string "//="

and pr_bop = function
  | Bmul -> pr_string "*"
  | Bdiv -> pr_string "/"
  | Bfdiv -> pr_string "//"
  | Bmod -> pr_string "%"
  | Badd -> pr_string "+"
  | Bsub -> pr_string "-"
  | BshiftL -> pr_string "<<"
  | BshiftR -> pr_string ">>"
  | Beq -> pr_string "=="
  | Bneq -> pr_string "!="
  | Blt -> pr_string "<"
  | Bgt -> pr_string ">"
  | Ble -> pr_string "<="
  | Bge -> pr_string ">="
  | BbitAnd -> pr_string "&"
  | BbitOr -> pr_string "|"
  | BbitXor -> pr_string "^"
  | Band -> pr_string " and "
  | Bor -> pr_string " or "
  | Bis -> pr_string " is "
  | BisNot -> pr_string " is not "
  | Bin -> pr_string " in "
  | BnotIn -> pr_string " not in "

and pr_uop = function 
  | Upositive -> pr_string "+"
  | Unegative -> pr_string "-"
  | Ucomplement -> pr_string "~"
  | Unot -> pr_string "not "
