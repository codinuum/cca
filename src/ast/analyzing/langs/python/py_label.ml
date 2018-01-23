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
(* python/label.ml *)


type name = string

let lang_prefix = Astml.python_prefix

let operator_attr_name = "operator"


let sprintf = Printf.sprintf

let keyroot_depth_min = 1

type tie_id = Lang_base.tie_id

let null_tid      = Lang_base.null_tid
let mktid         = Lang_base.mktid
let tid_to_string = Lang_base.tid_to_string
let anonymize_tid = Lang_base.anonymize_tid
let mktidattr     = Lang_base.mktidattr


let conv_loc
    { Ast.Loc.start_offset=so;
      Ast.Loc.end_offset=eo;
      Ast.Loc.start_line=sl;
      Ast.Loc.start_char=sc;
      Ast.Loc.end_line=el;
      Ast.Loc.end_char=ec
    } =
  Loc.make so eo sl sc el ec

let conv_name (_, name) = name
let loc_of_name (loc, _) = loc

open Charpool

module Literal =
  struct
    type t = 
      | Integer of string
      |	LongInteger of string
      | FloatNumber of string
      | ImagNumber of string
      | String

    let to_string = function
      | Integer str     -> sprintf "Integer:%s" str
      | LongInteger str -> sprintf "LongInteger:%s" str
      | FloatNumber str -> sprintf "FloatNumber:%s" str
      | ImagNumber str  -> sprintf "ImagNumber:%s" str
      | String          -> "String"

    let to_simple_string = function
      | Integer str     
      | LongInteger str 
      | FloatNumber str 
      | ImagNumber str  -> str
      | String          -> ""

    let anonymize = function
      | Integer str     -> Integer ""
      | LongInteger str -> LongInteger ""
      | FloatNumber str -> FloatNumber ""
      | ImagNumber str  -> ImagNumber ""
      | String          -> String

    let to_short_string ?(ignore_identifiers_flag=false) = 
    let combo = combo ~ignore_identifiers_flag in function
      | Integer str     -> mkstr 0
      | LongInteger str -> combo 1 [str]
      | FloatNumber str -> combo 2 [str]
      | ImagNumber str  -> combo 3 [str]
      | String          -> mkstr 4

    let of_literal = function
      | Ast.Linteger str     -> Integer str
      | Ast.Llonginteger str -> LongInteger str
      | Ast.Lfloatnumber str -> FloatNumber str
      | Ast.Limagnumber str  -> ImagNumber str
      | Ast.Lstring _        -> String

    let to_tag lit =
      let name, attrs =
	match lit with
	| Integer str     -> "IntegerLiteral", ["value",XML.encode_string str]
	| LongInteger str -> "LongIntegerLiteral", ["value",XML.encode_string str]
	| FloatNumber str -> "FloatNumberLiteral", ["value",XML.encode_string str]
	| ImagNumber str  -> "ImagNumberLiteral", ["value",XML.encode_string str]
	| String          -> "StringLiteral", []
      in
      name, attrs

  end (* of module Literal *)

module AssignmentOperator =
  struct
    type t =
      | Eq
      | AddEq
      | SubEq
      | MulEq
      | DivEq
      | ModEq
      | AndEq
      | OrEq
      | XorEq
      | ShiftLEq
      | ShiftREq
      | PowEq
      | FDivEq

    let to_string ao =
      let str =
	match ao with
	| Eq       -> "="
	| AddEq    -> "+="
	| SubEq    -> "-="
	| MulEq    -> "*="
	| DivEq    -> "/="
	| ModEq    -> "%="
	| AndEq    -> "&="
	| OrEq     -> "|="
	| XorEq    -> "^="
	| ShiftLEq -> "<<="
	| ShiftREq -> ">>="
	| PowEq    -> "**="
	| FDivEq   -> "//="
      in sprintf "AO(%s)" str

    let to_short_string = function
      | Eq    -> mkstr 0
      | AddEq -> mkstr 1
      | SubEq -> mkstr 2
      | MulEq -> mkstr 3
      | DivEq -> mkstr 4
      | ModEq -> mkstr 5
      | AndEq -> mkstr 6
      | OrEq  -> mkstr 7
      | XorEq -> mkstr 8
      | ShiftLEq -> mkstr 9
      | ShiftREq -> mkstr 10
      | PowEq  -> mkstr 11
      | FDivEq -> mkstr 12

    let of_aop = function
	Ast.AaddEq -> AddEq
      | Ast.AsubEq -> SubEq
      | Ast.AmulEq -> MulEq
      | Ast.AdivEq -> DivEq
      | Ast.AmodEq -> ModEq
      | Ast.AandEq -> AndEq
      | Ast.AorEq  -> OrEq
      | Ast.AxorEq -> XorEq
      | Ast.AshiftLEq -> ShiftLEq
      | Ast.AshiftREq -> ShiftREq
      | Ast.ApowEq  -> PowEq
      | Ast.AfdivEq -> FDivEq


    let to_tag aop =
      let name = 
	match aop with
	| Eq       -> "Assign"
	| AddEq    -> "AddAssign"
	| SubEq    -> "SubtAssign"
	| MulEq    -> "MultAssign"
	| DivEq    -> "DivAssign"
	| ModEq    -> "ModAssign"
	| AndEq    -> "AndAssign"
	| OrEq     -> "OrAssign"
	| XorEq    -> "XorAssign"
	| ShiftLEq -> "ShiftLAssign"
	| ShiftREq -> "ShiftRAssign"
	| PowEq    -> "PowAssign"
	| FDivEq   -> "FDivAssign"
      in
      name, []
	
  end (* of module AssignmentOperator *)

module UnaryOperator =
  struct
    type t = 
      | Positive 
      | Negative 
      | Complement 
      | Not

    let to_string uo =
      let str =
	match uo with
	| Positive   -> "+"
	| Negative   -> "-"
	| Complement -> "~"
	| Not        -> "not"
      in sprintf "UO(%s)" str

    let to_short_string = function
      | Positive   -> mkstr 0
      | Negative   -> mkstr 1
      | Complement -> mkstr 2
      | Not        -> mkstr 3

    let of_uop = function
	Ast.Upositive   -> Positive
      | Ast.Unegative   -> Negative
      | Ast.Ucomplement -> Complement
      | Ast.Unot        -> Not

    let to_tag uo =
      let name =
	match uo with
	| Positive   -> "Positive"
	| Negative   -> "Negative"
	| Complement -> "Complement"
	| Not        -> "Not"
      in
      name, []

  end (* of module UnaryOperator *)

module BinaryOperator =
  struct
    type t =
      | Mul | Div | FDiv | Mod | Add | Sub 
      | ShiftL | ShiftR
      | Eq | Neq | Lt | Gt | Le | Ge
      | BitAnd | BitOr | BitXor | And | Or
      | Is | IsNot | In | NotIn

    let to_string bo =
      let str =
	match bo with
	| Mul    -> "*"
	| Div    -> "/"
	| FDiv   -> "//"
	| Mod    -> "%"
	| Add    -> "+"
	| Sub    -> "-"
	| ShiftL -> "<<"
	| ShiftR -> ">>"
	| Eq     -> "=="
	| Neq    -> "!="
	| Lt     -> "<"
	| Gt     -> ">"
	| Le     -> "<="
	| Ge     -> ">="
	| BitAnd -> "&"
	| BitOr  -> "|"
	| BitXor -> "^"
	| And    -> "and"
	| Or     -> "or"
	| Is     -> "is"
	| IsNot  -> "is not"
	| In     -> "in"
	| NotIn  -> "not in"
      in sprintf "BO(%s)" str

    let to_short_string = function
      | Mul    -> mkstr 0
      | Div    -> mkstr 1
      | FDiv   -> mkstr 2
      | Mod    -> mkstr 3
      | Add    -> mkstr 4
      | Sub    -> mkstr 5
      | ShiftL -> mkstr 6 
      | ShiftR -> mkstr 7
      | Eq     -> mkstr 8
      | Neq    -> mkstr 9
      | Lt     -> mkstr 10
      | Gt     -> mkstr 11
      | Le     -> mkstr 12
      | Ge     -> mkstr 13
      | BitAnd -> mkstr 14 
      | BitOr  -> mkstr 15
      | BitXor -> mkstr 16
      | And    -> mkstr 17
      | Or     -> mkstr 18
      | Is     -> mkstr 19
      | IsNot  -> mkstr 20
      | In     -> mkstr 21
      | NotIn  -> mkstr 22

    let of_bop = function
      | Ast.Bmul    -> Mul
      | Ast.Bdiv    -> Div
      | Ast.Bfdiv   -> FDiv
      | Ast.Bmod    -> Mod
      | Ast.Badd    -> Add
      | Ast.Bsub    -> Sub
      | Ast.BshiftL -> ShiftL
      | Ast.BshiftR -> ShiftR
      | Ast.Beq     -> Eq
      | Ast.Bneq    -> Neq
      | Ast.Blt     -> Lt
      | Ast.Bgt     -> Gt
      | Ast.Ble     -> Le
      | Ast.Bge     -> Ge
      | Ast.BbitAnd -> BitAnd
      | Ast.BbitOr  -> BitOr
      | Ast.BbitXor -> BitXor
      | Ast.Band    -> And
      | Ast.Bor     -> Or
      | Ast.Bis     -> Is
      | Ast.BisNot  -> IsNot
      | Ast.Bin     -> In
      | Ast.BnotIn  -> NotIn


    let to_tag bo =
      let name =
	match bo with
	| Mul    -> "Mult"
	| Div    -> "Div"
	| FDiv   -> "FDiv"
	| Mod    -> "Mod"
	| Add    -> "Add"
	| Sub    -> "Subt"
	| ShiftL -> "ShiftL"
	| ShiftR -> "ShiftR"
	| Eq     -> "Eq"
	| Neq    -> "NotEq"
	| Lt     -> "Le"
	| Gt     -> "Gt"
	| Le     -> "Le"
	| Ge     -> "Ge"
	| BitAnd -> "BitAnd"
	| BitOr  -> "BitOr"
	| BitXor -> "BitXor"
	| And    -> "And"
	| Or     -> "Or"
	| Is     -> "Is"
	| IsNot  -> "IsNot"
	| In     -> "InOp"
	| NotIn  -> "NotIn"
      in
      name, []

  end (* of module BinaryOperator *)

module Statement =
  struct
    type t =
      | Simple
      | If
      | While
      | For
      | Try
      | With
      | FuncDef of name
      | ClassDef of name


    let to_string = function
      | Simple        -> "Simple"
      | If            -> "If"
      | While         -> "While"
      | For           -> "For"
      | Try           -> "Try"
      | With          -> "With"
      | FuncDef name  -> "FuncDef:" ^ name
      | ClassDef name -> "ClassDef:" ^ name

    let anonymize = function
      | FuncDef name  -> FuncDef ""
      | ClassDef name -> ClassDef ""
      | stmt          -> stmt

    let to_short_string ?(ignore_identifiers_flag=false) = 
    let combo = combo ~ignore_identifiers_flag in function
      | Simple -> mkstr 0
      | If     -> mkstr 1
      | While  -> mkstr 2
      | For    -> mkstr 3
      | Try    -> mkstr 4
      | With   -> mkstr 5
      | FuncDef name  -> combo 6 [name]
      | ClassDef name -> combo 7 [name]

    let to_tag stmt =
      let name, attrs =
	match stmt with
	| Simple        -> "SimpleStmt", []
	| If            -> "IfStmt", []
	| While         -> "WhileStmt", []
	| For           -> "ForStmt", []
	| Try           -> "TryStmt", []
	| With          -> "WithStmt", []
	| FuncDef name  -> "FuncDef", ["name",name]
	| ClassDef name -> "ClassDef", ["name",name]
      in
      name, attrs
      
  end (* of module Statement *)

module SimpleStatement =
  struct
    type t = 
      | Expr
      | Assignment of AssignmentOperator.t
      | Print
      | Del
      | Pass
      | Break
      | Continue
      | Return
      | Raise
      | Yield
      | Import
      | Global
      | Exec
      | Assert

    let to_string = function
      | Expr -> "Expr"
      | Assignment aop -> sprintf "Assignment.%s" (AssignmentOperator.to_string aop)
      | Print          -> "Print"
      | Del            -> "Del"
      | Pass           -> "Pass"
      | Break          -> "Break"
      | Continue       -> "Continue"
      | Return         -> "Return"
      | Raise          -> "Raise"
      | Yield          -> "Yield"
      | Import         -> "Import"
      | Global         -> "Global"
      | Exec           -> "Exec"
      | Assert         -> "Assert"

    let to_short_string = function
      | Expr           -> mkstr 0 
      | Assignment aop -> catstr [mkstr 1; AssignmentOperator.to_short_string aop]
      | Print          -> mkstr 2
      | Del            -> mkstr 3
      | Pass           -> mkstr 4
      | Break          -> mkstr 5
      | Continue       -> mkstr 6
      | Return         -> mkstr 7
      | Raise          -> mkstr 8
      | Yield          -> mkstr 9
      | Import         -> mkstr 10
      | Global         -> mkstr 11
      | Exec           -> mkstr 12
      | Assert         -> mkstr 13

    let to_tag sstmt =
      let name, attrs =
	match sstmt with
	| Expr           -> "ExprStmt", []
	| Assignment aop -> AssignmentOperator.to_tag aop
	| Print          -> "PrintStmt", []
	| Del            -> "DelStmt", []
	| Pass           -> "PassStmt", []
	| Break          -> "BreakStmt", []
	| Continue       -> "ContinueStmt", []
	| Return         -> "ReturnStmt", []
	| Raise          -> "RaiseStmt", []
	| Yield          -> "YieldStmt", []
	| Import         -> "ImportStmt", []
	| Global         -> "GlobalStmt", []
	| Exec           -> "ExecStmt", []
	| Assert         -> "AssertStmt", []
      in
      name, attrs

  end (* of module SimpleStatement *)


module Primary = struct
  type t =
    | Name of name
    | Literal of Literal.t
    | Paren
    | Tuple
    | Yield
    | Test
    | List
    | Dict
    | StringConv
    | AttrRef
    | Subscription
    | Slicing
    | Call of tie_id
	  

  let to_string = function
    | Name name    -> sprintf "Name:%s" name
    | Literal lit  -> Literal.to_string lit
    | Paren        -> "Paren"
    | Tuple        -> "Tuple"
    | Yield        -> "Yield"
    | Test         -> "Test"
    | List         -> "List"
    | Dict         -> "Dict"
    | StringConv   -> "StringConv"
    | AttrRef      -> "AttrRef"
    | Subscription -> "Subscription"
    | Slicing      -> "Slicing"
    | Call tid     -> "Call:" ^ (tid_to_string tid)

  let anonymize ?(more=false) = function
    | Name name   -> Name ""
    | Literal lit -> Literal (Literal.anonymize lit)
    | Call tid    -> Call (anonymize_tid ~more tid)
    | prim        -> prim

  let to_short_string ?(ignore_identifiers_flag=false) = 
    let combo = combo ~ignore_identifiers_flag in function
    | Name name    -> combo 0 [name]
    | Literal lit  -> combo 1 [Literal.to_short_string ~ignore_identifiers_flag lit]
    | Paren        -> mkstr 2
    | Tuple        -> mkstr 3
    | Yield        -> mkstr 4
    | Test         -> mkstr 5
    | List         -> mkstr 6
    | Dict         -> mkstr 7
    | StringConv   -> mkstr 8 
    | AttrRef      -> mkstr 9
    | Subscription -> mkstr 10
    | Slicing      -> mkstr 11
    | Call tid     -> combo 12 [tid_to_string tid]
			      
  let to_tag prim =
    let name, attrs =
      match prim with
      | Name name    -> "NameAtom", ["name",name]
      | Literal lit  -> Literal.to_tag lit
      | Paren        -> "ParenAtom", []
      | Tuple        -> "TupleAtom", []
      | Yield        -> "YieldAtom", []
      | Test         -> "TestAtom", []
      | List         -> "ListAtom", []
      | Dict         -> "DictAtom", []
      | StringConv   -> "StringConvAtom", []
      | AttrRef      -> "AttrRef", []
      | Subscription -> "Subscription", []
      | Slicing      -> "Slicing", []
      | Call tid     -> "Call", mktidattr tid
    in
    name, attrs

end (* of module Primary *)

type annotation = string option

let null_annotation = None

let annotation_to_string = function
  | None   -> "<none>"
  | Some x -> x



type t = (* Label *)
  | Dummy

  | FileInput of name
  | DottedName
  | Name of name
  | Lambda
  | Test
  | Power

  | Primary of Primary.t
  | UnaryOperator of UnaryOperator.t
  | BinaryOperator of BinaryOperator.t
  | Statement of Statement.t
  | SimpleStatement of SimpleStatement.t

  | Elif
  | Else
  | Targets
  | Target
  | Except
  | Suite
  | NamedSuite of name
  | Parameters
  | NamedParameters of name
  | Decorators
  | Decorator
  | Finally
  | In
  | LHS
  | RHS
  | As
  | ListMaker
  | ListIf
  | ListFor
  | DictOrSetMaker
  | KeyDatum
  | SliceItem
  | Lower
  | Upper
  | Stride
  | SliceItemEllipsis
  | Arguments
  | NamedArguments of name
  | Argument
  | GenFor
  | GenIf
  | Inheritance
  | Chevron
  | From
  | Tuple
  | Dict
  | DefParameter
  | Sublist
  | StringLiteral of string
  | WithItem


let opt_to_string to_str = function
  | Some x -> to_str x
  | None   -> ""

let name_to_string (_, n) = n

let literal_to_string = function
  | Ast.Linteger str     -> "Linteger:" ^ str
  | Ast.Llonginteger str -> "Llonginteger:" ^ str
  | Ast.Lfloatnumber str -> "Lfloatnumber:" ^ str
  | Ast.Limagnumber str  -> "Linagnumber:" ^ str
  | Ast.Lstring pystrs   -> 
      "Lstring[" ^
      (Xlist.to_string
	 (function 
	     Ast.PSlong(_, s) -> "PSlong:" ^ s
	   | Ast.PSshort(_, s) -> "PSshort:" ^ s
	 ) ";" pystrs) ^
      "]"

let rec primary_to_string prim = 
  match prim.Ast.prim_desc with
  | Ast.Pname name           -> "Pname(" ^ (name_to_string name) ^ ")"
  | Ast.Pliteral lit         -> "Pliteral(" ^ (literal_to_string lit) ^ ")"
  | Ast.Pparen expr          -> "Pparen(" ^ (expr_to_string expr) ^ ")"
  | Ast.Ptuple exprs         -> "Ptuple" ^ (exprs_to_string exprs)
  | Ast.Pyield exprs         -> "Pyield" ^ (exprs_to_string exprs)

  | Ast.Pcomp(expr, compfor) -> 
      "Pcomp(" ^ (expr_to_string expr) ^ "," ^ 
      (compfor_to_string compfor) ^ ")"

  | Ast.Plist listmaker -> "Plist" ^ (listmaker_to_string listmaker)
  | Ast.Plistnull -> "Plistnull"
  | Ast.Pdictorset dictorsetmaker -> "Pdictorset" ^ (dictorsetmaker_to_string dictorsetmaker)
  | Ast.Pdictnull -> "Pdictnull"
  | Ast.Pstrconv exprs -> "Pstrconv" ^ (exprs_to_string exprs)
  | Ast.Pattrref(prim, name) -> 
      "Pattrref(" ^ (primary_to_string prim) ^ "," ^ (name_to_string name) ^ ")"

  | Ast.Psubscript(prim, exprs) ->
      "Psubscript(" ^ (primary_to_string prim) ^ "," ^ 
      (exprs_to_string exprs) ^ ")"

  | Ast.Pslice(prim, sliceitems) ->
      "Pslice(" ^ (primary_to_string prim) ^ ",[" ^
      (Xlist.to_string sliceitem_to_string ";" sliceitems) ^ "])"
	
  | Ast.Pcall(prim, arglist) ->
      "Pcall(" ^ (primary_to_string prim) ^ "," ^ 
      (arglist_to_string arglist) ^ ")"

and expr_to_string expr = 
  match expr.Ast.expr_desc with
  | Ast.Eprimary prim -> "Eprimary(" ^ (primary_to_string prim) ^ ")"

  | Ast.Epower(prim, expr) -> 
      "Epower(" ^ (primary_to_string prim) ^ "," ^ (expr_to_string expr) ^ ")"

  | Ast.Ebop(expr1, bop, expr2) -> 
      "Ebop(" ^ (expr_to_string expr1) ^ "," ^ (bop_to_string bop) ^ "," ^
      (expr_to_string expr2) ^ ")"

  | Ast.Euop(uop, expr) -> 
      "Euop(" ^ (uop_to_string uop) ^ "," ^ (expr_to_string expr) ^ ")"

  | Ast.Elambda(params, expr) ->
      "Elambda(" ^ 
      (match params with 
	_, [], None, None -> "" 
      | _ -> parameters_to_string params) ^
      (expr_to_string expr)

  | Ast.Econd(expr1, expr2, expr3) ->
      "Econd(" ^ 
      (expr_to_string expr1) ^ "," ^
      (expr_to_string expr2) ^ "," ^
      (expr_to_string expr3) ^ ")"

and bop_to_string = function
  | Ast.Bmul    -> "Bmul"
  | Ast.Bdiv    -> "Bdiv"
  | Ast.Bfdiv   -> "Bfdiv"
  | Ast.Bmod    -> "Bmod"
  | Ast.Badd    -> "Badd"
  | Ast.Bsub    -> "Bsub"
  | Ast.BshiftL -> "BshiftL"
  | Ast.BshiftR -> "BshiftR"
  | Ast.Beq     -> "Beq"
  | Ast.Bneq    -> "Bneq"
  | Ast.Blt     -> "Blt"
  | Ast.Bgt     -> "Bgt"
  | Ast.Ble     -> "Ble"
  | Ast.Bge     -> "Bge"
  | Ast.BbitAnd -> "BbitAnd"
  | Ast.BbitOr  -> "BbitOr"
  | Ast.BbitXor -> "BbitXor"
  | Ast.Band    -> "Band"
  | Ast.Bor     -> "Bor"
  | Ast.Bis     -> "Bis"
  | Ast.BisNot  -> "BisNot"
  | Ast.Bin     -> "Bin"
  | Ast.BnotIn  -> "BnotIn"

and uop_to_string = function
  | Ast.Upositive   -> "Upositive"
  | Ast.Unegative   -> "Unegative"
  | Ast.Ucomplement -> "Ucomplement"
  | Ast.Unot        -> "Unot"

and parameters_to_string (_, vargs, tini, dini) =
  "(" ^ 
  (vargs_to_string vargs) ^ "," ^
  (opt_to_string name_to_string tini) ^ "," ^
  (opt_to_string name_to_string dini) ^ ")"

and vargs_to_string vargs =
  let varg_to_string (fpdef, expr_opt) = 
    (fpdef_to_string fpdef) ^ "," ^ (opt_to_string expr_to_string expr_opt)
  in
  "[" ^ (Xlist.to_string varg_to_string ";" vargs) ^ "]"

and fpdef_to_string = function
  | Ast.Fname n -> "Fname(" ^ (name_to_string n) ^ ")"
  | Ast.Flist(_, fpdefs) ->
      "Flist[" ^ (Xlist.to_string fpdef_to_string ";" fpdefs) ^ "]"

and exprs_to_string exprs =
  "[" ^ (Xlist.to_string expr_to_string ";" exprs) ^ "]"

and compfor_to_string (_, exprs, expr, compiter_opt) =
  "(" ^ 
  (exprs_to_string exprs) ^ "," ^ (expr_to_string expr) ^ "," ^ 
  (opt_to_string compiter_to_string compiter_opt) ^ 
  ")"

and compiter_to_string = function
  | Ast.Cfor gf -> "Cfor(" ^ (compfor_to_string gf) ^ ")"
  | Ast.Cif gi -> "Cif(" ^ (compif_to_string gi) ^ ")"

and compif_to_string (_, expr, compiter_opt) =
  "(" ^ 
  (expr_to_string expr) ^ "," ^ (opt_to_string compiter_to_string compiter_opt) ^ 
  ")"

and listmaker_to_string = function
  | Ast.LMfor (expr, listfor) -> 
      "LMfor(" ^ (expr_to_string expr) ^ "," ^ (listfor_to_string listfor) ^ ")"
  | Ast.LMtest exprs -> "LMtest" ^ (exprs_to_string exprs)

and listfor_to_string (_, exprs1, exprs2, listiter_opt) =
  "(" ^ 
  (exprs_to_string exprs1) ^ "," ^ (exprs_to_string exprs2) ^ "," ^ 
  (opt_to_string listiter_to_string listiter_opt) ^ 
  ")"

and listiter_to_string = function
  | Ast.LIfor lf -> "LIfor(" ^ (listfor_to_string lf) ^ ")"
  | Ast.LIif li -> "LIif(" ^ (listif_to_string li) ^ ")"

and listif_to_string (_, expr, listiter_opt) =
  "(" ^ 
  (expr_to_string expr) ^ "," ^ (opt_to_string listiter_to_string listiter_opt) ^ 
  ")"

and dictorsetmaker_to_string dictorsetmaker = 
  let s =
    match dictorsetmaker with
    | Ast.DSMdict key_dats ->
	(Xlist.to_string
	   (fun (_, e1, e2) -> 
	     (expr_to_string e1)^":"^(expr_to_string e2)) "," key_dats)

    | Ast.DSMdictC(e1, e2, compfor) ->
	(expr_to_string e1)^":"^(expr_to_string e2)^" "^(compfor_to_string compfor)

    | Ast.DSMset es -> Xlist.to_string expr_to_string "," es

    | Ast.DSMsetC(e, compfor) ->
	(expr_to_string e)^" "^(compfor_to_string compfor)
  in
  "{"^s^"}"

and sliceitem_to_string = function
  | Ast.SIexpr expr -> "SIexpr(" ^ (expr_to_string expr) ^ ")"
  | Ast.SIproper(_, expr_opt1, expr_opt2, expr_opt3) ->
      "SIproper(" ^ 
      (opt_to_string expr_to_string expr_opt1) ^ "," ^  
      (opt_to_string expr_to_string expr_opt2) ^ "," ^ 
      (opt_to_string expr_to_string expr_opt3) ^ ")"
  | Ast.SIellipsis _ -> "SIellipsis"

and arglist_to_string (_, args, tini, dini) =
  "(" ^
  (Xlist.to_string argument_to_string "," args) ^ "," ^
  (opt_to_string 
     (fun (e, al) -> 
       (expr_to_string e)^(Xlist.to_string (fun a -> ","^(argument_to_string a)) "" al)
     ) tini
  ) ^ "," ^
  (opt_to_string expr_to_string dini) ^ 
  ")"

and argument_to_string (_, expr_opt, expr, compfor_opt) =
  "(" ^
  (opt_to_string expr_to_string expr_opt) ^ "," ^
  (expr_to_string expr) ^ "," ^
  (opt_to_string compfor_to_string compfor_opt) ^ 
  ")"


let of_statement stmt =
  Statement
    (match stmt with
    | Ast.Ssimple _                -> Statement.Simple
    | Ast.Sif _                    -> Statement.If
    | Ast.Swhile _                 -> Statement.While
    | Ast.Sfor _                   -> Statement.For
    | Ast.Stry _                   -> Statement.Try
    | Ast.Stryfin _                -> Statement.Try
    | Ast.Swith _                  -> Statement.With
    | Ast.Sfuncdef(_, name, _, _)  -> Statement.FuncDef (conv_name name)
    | Ast.Sclassdef(_, name, _, _) -> Statement.ClassDef (conv_name name)
    )

let of_simplestmt sstmt = 
  SimpleStatement
    (match sstmt with
    | Ast.SSexpr _               -> SimpleStatement.Expr
    | Ast.SSassign _             -> SimpleStatement.Assignment AssignmentOperator.Eq
    | Ast.SSaugassign(_, aop, _) -> 
	SimpleStatement.Assignment (AssignmentOperator.of_aop aop)
    | Ast.SSprint _              -> SimpleStatement.Print
    | Ast.SSprintchevron _       -> SimpleStatement.Print
    | Ast.SSdel _                -> SimpleStatement.Del
    | Ast.SSpass                 -> SimpleStatement.Pass
    | Ast.SSbreak                -> SimpleStatement.Break
    | Ast.SScontinue             -> SimpleStatement.Continue
    | Ast.SSreturn _             -> SimpleStatement.Return
    | Ast.SSraise                -> SimpleStatement.Raise
    | Ast.SSraise1 _             -> SimpleStatement.Raise
    | Ast.SSraise2 _             -> SimpleStatement.Raise
    | Ast.SSraise3 _             -> SimpleStatement.Raise
    | Ast.SSyield _              -> SimpleStatement.Yield
    | Ast.SSimport _             -> SimpleStatement.Import
    | Ast.SSfrom _               -> SimpleStatement.Import
    | Ast.SSglobal _             -> SimpleStatement.Global
    | Ast.SSexec _               -> SimpleStatement.Exec
    | Ast.SSexec2 _              -> SimpleStatement.Exec
    | Ast.SSexec3 _              -> SimpleStatement.Exec
    | Ast.SSassert _             -> SimpleStatement.Assert
    | Ast.SSassert2 _            -> SimpleStatement.Assert)

let of_bop bop = BinaryOperator (BinaryOperator.of_bop bop)

let of_uop uop = UnaryOperator (UnaryOperator.of_uop uop)

let of_primary p =
  Primary
    (match p with
    | Ast.Pname name      -> Primary.Name (conv_name name)
    | Ast.Pliteral lit    -> Primary.Literal (Literal.of_literal lit)
    | Ast.Pparen _        -> Primary.Paren
    | Ast.Ptuple _        -> Primary.Tuple
    | Ast.Pyield _        -> Primary.Yield
    | Ast.Pcomp _         -> Primary.Test
    | Ast.Plist _         -> Primary.List
    | Ast.Plistnull       -> Primary.List 
    | Ast.Pdictorset _    -> Primary.Dict
    | Ast.Pdictnull       -> Primary.Dict 
    | Ast.Pstrconv _      -> Primary.StringConv
    | Ast.Pattrref _      -> Primary.AttrRef
    | Ast.Psubscript _    -> Primary.Subscription
    | Ast.Pslice _        -> Primary.Slicing
    | Ast.Pcall (prim, _) -> 
	let tid = 
	  mktid
	    (Digest.to_hex (Digest.string (primary_to_string prim)))
	    ""
	in
	Primary.Call tid
    )


let rec to_string = function
  | Dummy                 -> "Dummy"

  | Primary p             -> Primary.to_string p
  | UnaryOperator uo      -> UnaryOperator.to_string uo
  | BinaryOperator bo     -> BinaryOperator.to_string bo
  | Statement stmt        -> Statement.to_string stmt
  | SimpleStatement sstmt -> SimpleStatement.to_string sstmt

  | FileInput n           -> sprintf "FileInput:%s" n
  | DottedName            -> "DottedName"
  | Name n                -> sprintf "Name:%s" n
  | Lambda                -> "Lambda"
  | Test                  -> "Test"
  | Power                 -> "Power"
  | Elif                  -> "Elif"
  | Else                  -> "Else"
  | Targets               -> "Targets"
  | Target                -> "Target"
  | Except                -> "Except"
  | Suite                 -> "Suite"
  | NamedSuite n          -> sprintf "Suite:%s" n
  | Parameters            -> "Parameters"
  | NamedParameters n     -> sprintf "Parameters:%s" n
  | Decorators            -> "Decorators"
  | Decorator             -> "Decorator"
  | Finally               -> "Finally"
  | In                    -> "In"	
  | LHS                   -> "LHS"
  | RHS                   -> "RHS"
  | As                    -> "As"
  | ListMaker             -> "ListMaker"
  | ListIf                -> "ListIf"
  | ListFor               -> "ListFor"
  | DictOrSetMaker        -> "DictOrSetMaker"
  | KeyDatum              -> "KeyDatum"
  | SliceItem             -> "SliceItem"
  | Lower                 -> "Lower"
  | Upper                 -> "Upper"
  | Stride                -> "Stride"
  | SliceItemEllipsis     -> "SliceItemEllipsis"
  | Arguments             -> "Arguments"
  | NamedArguments n      -> sprintf "Arguments:%s" n
  | Argument              -> "Argument"
  | GenFor                -> "GenFor"
  | GenIf                 -> "GenIf"
  | Inheritance           -> "Inheritance"
  | Chevron               -> "Chevron"
  | From                  -> "From"
  | Tuple                 -> "Tuple"
  | Dict                  -> "Dict"
  | DefParameter          -> "DefParameter"
  | Sublist               -> "Sublist"
  | StringLiteral str     ->
      let re0 = Str.regexp_string "\n" in
      let str0 = Str.global_replace re0 "\\n" str in
      sprintf "StringLiteral:%s" str0
  | WithItem -> "WithItem"

let anonymize ?(more=false) = function
  | Primary p         -> Primary (Primary.anonymize ~more p)
  | Statement stmt    -> Statement (Statement.anonymize stmt)
  | FileInput n       -> FileInput ""
  | Name n            -> Name ""
  | NamedSuite n      -> NamedSuite ""
  | NamedParameters n -> NamedParameters ""
  | NamedArguments n  -> NamedArguments ""
  | StringLiteral str -> StringLiteral ""
  | lab               -> lab

let anonymize2 = anonymize ~more:true

let anonymize3 = anonymize ~more:true

let to_simple_string = to_string (* to be implemented *)


let rec to_short_string ?(ignore_identifiers_flag=false) = 
    let combo = combo ~ignore_identifiers_flag in function
  | Dummy -> mkstr 0

  | Primary p             -> catstr [mkstr 1; Primary.to_short_string ~ignore_identifiers_flag p]
  | UnaryOperator uo      -> catstr [mkstr 2; UnaryOperator.to_short_string uo]
  | BinaryOperator bo     -> catstr [mkstr 3; BinaryOperator.to_short_string bo]
  | Statement stmt        -> catstr [mkstr 4; Statement.to_short_string ~ignore_identifiers_flag stmt]
  | SimpleStatement sstmt -> catstr [mkstr 5; SimpleStatement.to_short_string sstmt]

  | FileInput n -> combo 6 [n]
  | DottedName  -> mkstr 7
  | Name n      -> combo 8 [n]

  | Lambda -> mkstr 9
  | Test   -> mkstr 10
  | Power  -> mkstr 11

  | Elif    -> mkstr 12
  | Else    -> mkstr 13
  | Targets -> mkstr 14
  | Target  -> mkstr 15
  | Except  -> mkstr 16
  | Suite   -> mkstr 17

  | NamedSuite n      -> combo 18 [n]
  | Parameters        -> mkstr 19
  | NamedParameters n -> combo 20 [n]

  | Decorators -> mkstr 21
  | Decorator  -> mkstr 22
  | Finally    -> mkstr 23
  | In  -> mkstr 24
  | LHS -> mkstr 25
  | RHS -> mkstr 26
  | As  -> mkstr 27
  | ListMaker -> mkstr 28
  | ListIf    -> mkstr 29
  | ListFor   -> mkstr 30
  | DictOrSetMaker -> mkstr 31
  | KeyDatum  -> mkstr 32
  | SliceItem -> mkstr 33
  | Lower     -> mkstr 34
  | Upper     -> mkstr 35
  | Stride    -> mkstr 36
  | SliceItemEllipsis -> mkstr 37
  | Arguments -> mkstr 38

  | NamedArguments n -> combo 39 [n]

  | Argument    -> mkstr 40
  | GenFor      -> mkstr 41
  | GenIf       -> mkstr 42
  | Inheritance -> mkstr 43
  | Chevron     -> mkstr 44
  | From        -> mkstr 45
  | Tuple       -> mkstr 46
  | Dict        -> mkstr 47
  | DefParameter -> mkstr 48
  | Sublist      -> mkstr 49
  | StringLiteral str -> 
      if (String.length str) > string_len_threshold then
	catstr [mkstr 50; Digest.to_hex (Digest.string str)]
      else
	catstr [mkstr 51; str]
  | WithItem     -> mkstr 52


let to_tag l =
  let name, attrs =
    match l with
    | Dummy                 -> "Dummy", []

    | Primary p             -> Primary.to_tag p
    | UnaryOperator uo      -> UnaryOperator.to_tag uo
    | BinaryOperator bo     -> BinaryOperator.to_tag bo
    | Statement stmt        -> Statement.to_tag stmt
    | SimpleStatement sstmt -> SimpleStatement.to_tag sstmt

    | DottedName            -> "DottedName", []
    | Name n                -> "name", ["name",n]
    | Lambda                -> "Lambda", []
    | Test                  -> "Test", []
    | Power                 -> "Power", []
    | Elif                  -> "Elif", []
    | Else                  -> "Else", []
    | Targets               -> "Targets", []
    | Target                -> "Target", []
    | Except                -> "Except", []
    | Suite                 -> "Suite", []
    | NamedSuite n          -> "NamedSuite", ["name",n]
    | Parameters            -> "Parameters", []
    | NamedParameters n     -> "NamedParameters", ["name",n]
    | Decorators            -> "Decorators", []
    | Decorator             -> "Decorator", []
    | Finally               -> "Finally", []
    | In                    -> "In", []
    | LHS                   -> "Lhs", []
    | RHS                   -> "Rhs", []
    | As                    -> "As", []
    | ListMaker             -> "ListMaker", []
    | ListIf                -> "ListIf", []
    | ListFor               -> "ListFor", []
    | DictOrSetMaker        -> "DictOrsetMaker", []
    | KeyDatum              -> "KeyDatum", []
    | SliceItem             -> "SliceItem", []
    | Lower                 -> "Lower", []
    | Upper                 -> "Upper", []
    | Stride                -> "Stride", []
    | SliceItemEllipsis     -> "SliceItemEllipsis", []
    | Arguments             -> "Arguments", []
    | NamedArguments n      -> "NamedArguments", ["name",n]
    | Argument              -> "Argument", []
    | GenFor                -> "GenFor", []
    | GenIf                 -> "GenIf", []
    | Inheritance           -> "Inheritance", []
    | Chevron               -> "Chevron", []
    | From                  -> "From", []
    | Tuple                 -> "Tuple", []
    | Dict                  -> "Dict", []
    | DefParameter          -> "DefParameter", []
    | Sublist               -> "Sublist", []
    | StringLiteral str     -> "StringLiteral", ["value",XML.encode_string str]
    | WithItem              -> "WithItem", []

    | FileInput n           -> "FileInput", ["name",n]
  in
  name, attrs


let to_char lab = '0' (* to be implemented *)


let to_elem_data = Astml.to_elem_data lang_prefix to_tag

let of_elem_data name attrs _ = Dummy (* not yet *)


let is_named = function
  | FileInput _ 
  | Name _
  | NamedSuite _
  | NamedParameters _
  | NamedArguments _ 
  | Statement (Statement.FuncDef _ | Statement.ClassDef _)
  | Primary (Primary.Name _) 
    -> true
  | _ -> false

let is_named_orig = function
  | FileInput _ 
  | Name _
  | Statement (Statement.FuncDef _ | Statement.ClassDef _)
  | Primary (Primary.Name _) 
    -> true
  | _ -> false

let relabel_allowed = function (* FIXME: should be tuned! *)
  | Primary _, Primary _
  | UnaryOperator _, UnaryOperator _
  | BinaryOperator _, BinaryOperator _
  | SimpleStatement _, SimpleStatement _ -> true

  | l1, l2 -> anonymize2 l1 = anonymize2 l2


let is_hunk_boundary _ _ = false (* not yet *)

(* These labels are collapsible whether they are leaves or not. *)
let forced_to_be_collapsible lab = 
  false

let is_collapse_target options lab = 
  let res =
    if options#no_collapse_flag then 
      false
    else 
      match lab with
      | Statement _
      | SimpleStatement _
      | Primary _
      | Inheritance
      | Parameters
      | DottedName
      | Lambda
      | Suite
      | NamedSuite _
	-> true
      | _ -> false
  in
(*  DEBUG_MSG "%s -> %B" (to_string lab) res; *)
  res

let is_to_be_notified = function
  | Statement(Statement.FuncDef _|Statement.ClassDef _) -> true
  | _ -> false

let is_boundary = function
  | Statement(Statement.FuncDef _|Statement.ClassDef _) -> true
  | _ -> false

let is_partition = function
  | Statement _ 
  | SimpleStatement _
    -> true
  | _ -> false

let is_sequence = function
  | FileInput _
  | Suite
  | NamedSuite _
      -> true
  | _ -> false


let get_ident_use = function
  | Name n -> n
  | Primary (Primary.Name n) -> n
  | _ -> ""


let is_string_literal = function
  | StringLiteral _ -> true
  | Primary (Primary.Literal Literal.String) -> true
  | _ -> false

let is_int_literal = function
  | Primary (Primary.Literal (Literal.Integer _ | Literal.LongInteger _)) -> true
  | _ -> false

let is_real_literal = function
  | Primary (Primary.Literal (Literal.FloatNumber _)) -> true
  | _ -> false


(* for fact extraction *)

let get_category lab = 
  let name, _ = to_tag lab in
  name

let get_name lab = raise Not_found (* not yet *)

let get_value = function
  | Primary (Primary.Literal lit) -> Literal.to_simple_string lit
  | StringLiteral s -> s
  | _ -> raise Not_found

let cannot_be_keyroot nd = false

let is_phantom = function
  | Decorators
  | Targets
  | Parameters
  | NamedParameters _
  | Arguments
  | NamedArguments _
      -> true
  | _ -> false

let is_special _ = false

