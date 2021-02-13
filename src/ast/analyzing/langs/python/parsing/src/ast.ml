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
 * ast.ml
 *
 *)

open Printf

module Loc = struct
  include Astloc
end

type loc = Loc.t


type name = loc * string

type dottedname = name list



type fileinput = Fileinput of loc * statement list

and testlist = { list: expr list; comma: bool; mutable yield: bool }

and statement = { stmt_desc: statement_desc; stmt_loc: loc }

and statement_desc =
  | Ssimple of simplestmt list
  | Sasync of statement
  | Sif of expr * suite * (loc * expr * suite) list * (loc * suite) option
  | Swhile of expr * suite * (loc * suite) option
  | Sfor of target list * expr list * suite * (loc * suite) option
  | Stry of suite * (except * suite) list * (loc * suite) option
	* (loc * suite) option
  | Stryfin of suite * (loc * suite)
  | Swith of (expr * target option) list * suite
  | Sasync_funcdef of decorator list * name * parameters * expr option * suite
  | Sfuncdef of decorator list * name * parameters * expr option * suite
  | Sclassdef of decorator list * name * arglist * suite

and simplestmt = { sstmt_desc: simplestmt_desc; sstmt_loc: loc }

and simplestmt_desc =
  | SSexpr of expr list
  | SSassign of testlist list * testlist
  | SSannassign of target list * expr * testlist option
  | SSaugassign of target list * augop * testlist
  | SSprint of expr list
  | SSprintchevron of expr * expr list
  | SSdel of target list
  | SSpass
  | SSbreak
  | SScontinue
  | SSreturn of expr list
  | SSraise
  | SSraise1 of expr
  | SSraise2 of expr * expr
  | SSraise3 of expr * expr * expr
  | SSraisefrom of expr * expr
  | SSyield of expr list
  | SSimport of dottedname_as_name list
  | SSfrom of dots option * dottedname option * name_as_name list
  | SSglobal of name list
  | SSnonlocal of name list
  | SSexec of expr
  | SSexec2 of expr * expr
  | SSexec3 of expr * expr * expr
  | SSassert of expr
  | SSassert2 of expr * expr


and dottedname_as_name = dottedname * name option

and name_as_name = name * name option

and except = EX of loc | EX1 of loc * expr | EX2 of loc * expr * target

and suite = loc * statement list

and dots = loc * int

and parameters = loc * vararg list

and vararg =
| VAarg of fpdef * expr option
| VAargs of loc * name option
| VAkwargs of loc * name

and fpdef = Fname of name | Flist of loc * fpdef list | Ftyped of loc * name * expr

and decorator = loc * dottedname * arglist

and expr = { expr_desc: expr_desc; expr_loc: loc }

and expr_desc =
  | Eprimary of primary
  | Epower of primary * expr
  | Ebop of expr * bop * expr
  | Euop of uop * expr
  | Elambda of parameters * expr
  | Econd of expr * expr * expr
  | Estar of expr
  | Enamed of expr * expr
  | Efrom of expr

  | Earg of expr * expr

and primary = { prim_desc: primary_desc; prim_loc: loc }

and primary_desc =
  | Pname of name
  | Pliteral of literal
  | Pparen of expr
  | Ptuple of expr list
  | Pyield of expr list
  | PcompT of expr * compfor
  | PcompL of expr * compfor
  | Plist of expr list
  | Plistnull
  | Pdictorset of dictorsetmaker
  | Pdictnull
  | Pstrconv of expr list
  | Pattrref of primary * name
  | Psubscript of primary * expr list
  | Pslice of primary * sliceitem list
  | Pcall of primary * arglist
  | Pawait of primary

and trailer =
  | TRattrref of name
  | TRsubscript of expr list
  | TRslice of sliceitem list
  | TRcall of arglist

and literal =
  | Linteger of string
  | Llonginteger of string
  | Lfloatnumber of string
  | Limagnumber of string
  | Lstring of pystring list

and pystring = PSlong of loc * string | PSshort of loc * string

and target = expr

and listfor = loc * expr list * expr list * listiter option

and listif = loc * expr * listiter option

and listiter = LIfor of listfor | LIif of listif

and dictorsetmaker =
| DSMdict of dictelem list
| DSMdictC of dictelem * compfor
| DSMset of expr list
| DSMsetC of expr * compfor

and dictelem = { delem_desc : dictelem_desc; delem_loc : loc }

and dictelem_desc =
| DEkeyValue of expr * expr
| DEstarStar of expr

and sliceitem =
  | SIexpr of expr
  | SI2 of loc * expr option * expr option
  | SI3 of loc * expr option * expr option * expr option
  | SIellipsis of loc

and arglist = loc * argument list

and argument =
  | Aarg of loc * expr * expr option
  | Acomp of loc * expr * compfor
  | Aassign of loc * expr * expr
  | Aargs of loc * expr
  | Akwargs of loc * expr


and compiter = Cfor of compfor | Cif of compif

and compif = loc * expr * compiter option

and compfor = loc * (expr list * expr * compiter option) * bool

and augop =
  | AaddEq
  | AsubEq
  | AmulEq
  | AdivEq
  | AmodEq
  | AandEq
  | AorEq
  | AxorEq
  | AshiftLEq
  | AshiftREq
  | ApowEq
  | AfdivEq

and bop =
  | Bmul | Bdiv | Bfdiv | Bmod | Badd | Bsub
  | BshiftL | BshiftR
  | Beq | Bneq | Blt | Bgt | Ble | Bge
  | BbitAnd | BbitOr | BbitXor | Band | Bor
  | Bis | BisNot | Bin | BnotIn

and uop = Upositive | Unegative | Ucomplement | Unot


class c (fileinput : fileinput) = object (self)
  inherit Ast_base.c

  method fileinput = fileinput


end (* of class AST.c *)


(* end of AST *)
