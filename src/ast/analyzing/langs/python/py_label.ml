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
(* py_label.ml *)


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

let dottedname_to_string dname = String.concat "." (List.map conv_name dname)

open Charpool

module Literal =
  struct
    type t = 
      | Integer of string
      |	LongInteger of string
      | FloatNumber of string
      | ImagNumber of string
      | String of string
      | CatString of string

    let to_string = function
      | Integer str     -> sprintf "Integer:%s" str
      | LongInteger str -> sprintf "LongInteger:%s" str
      | FloatNumber str -> sprintf "FloatNumber:%s" str
      | ImagNumber str  -> sprintf "ImagNumber:%s" str
      | String str      -> sprintf "String:%s" str
      | CatString str   -> sprintf "CatString:%s" str

    let to_simple_string = function
      | Integer str     
      | LongInteger str 
      | FloatNumber str 
      | ImagNumber str
      | String str
      | CatString str -> str

    let anonymize = function
      | Integer str     -> Integer ""
      | LongInteger str -> LongInteger ""
      | FloatNumber str -> FloatNumber ""
      | ImagNumber str  -> ImagNumber ""
      | String str      -> String ""
      | CatString str   -> CatString ""

    let to_short_string ?(ignore_identifiers_flag=false) = 
    let combo = combo ~ignore_identifiers_flag in function
      | Integer str     -> mkstr 0
      | LongInteger str -> combo 1 [str]
      | FloatNumber str -> combo 2 [str]
      | ImagNumber str  -> combo 3 [str]
      | String str      -> combo 4 [str]
      | CatString str   -> combo 5 [str]

    let pystr_to_string = function
      | Ast.PSshort(_, s) | Ast.PSlong(_, s) -> s

    let of_literal = function
      | Ast.Linteger str     -> Integer str
      | Ast.Llonginteger str -> LongInteger str
      | Ast.Lfloatnumber str -> FloatNumber str
      | Ast.Limagnumber str  -> ImagNumber str
      | Ast.Lstring []       -> String ""
      | Ast.Lstring [pystr]  -> String (pystr_to_string pystr)
      | Ast.Lstring pystrs -> begin
          let s =
            String.concat ""
	      (List.map
	         (function
		   | Ast.PSshort(_, s) -> String.sub s 1 ((String.length s) - 2)
		   | Ast.PSlong(_, s)  -> String.sub s 3 ((String.length s) - 6)
	         ) pystrs)
          in
          let s_ =
            if true||(String.length s) > string_len_threshold then
	      Digest.to_hex (Digest.string s)
            else
	      s
          in
          CatString s_
      end

    let to_tag lit =
      let name, attrs =
        match lit with
        | Integer str     -> "IntegerLiteral", ["value",XML.encode_string str]
        | LongInteger str -> "LongIntegerLiteral", ["value",XML.encode_string str]
        | FloatNumber str -> "FloatNumberLiteral", ["value",XML.encode_string str]
        | ImagNumber str  -> "ImagNumberLiteral", ["value",XML.encode_string str]
        | String str      -> "StringLiteral", ["value",XML.encode_string str]
        | CatString str   -> "CatStringLiteral", ["value",XML.encode_string str]
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

    let to_simple_string = function
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

    let to_string ao = sprintf "AO(%s)" (to_simple_string ao)

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

    let to_simple_string = function
      | Positive   -> "+"
      | Negative   -> "-"
      | Complement -> "~"
      | Not        -> "not "

    let to_string uo = sprintf "UO(%s)" (to_simple_string uo)

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

    let to_simple_string = function
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
      | And    -> " and "
      | Or     -> " or "
      | Is     -> " is "
      | IsNot  -> " is not "
      | In     -> " in "
      | NotIn  -> " not in "

    let to_string bo = sprintf "BO(%s)" (to_simple_string bo)

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
      | Async
      | AsyncFuncDef of name

    let to_string = function
      | Simple            -> "Simple"
      | If                -> "If"
      | While             -> "While"
      | For               -> "For"
      | Try               -> "Try"
      | With              -> "With"
      | FuncDef name      -> "FuncDef:" ^ name
      | ClassDef name     -> "ClassDef:" ^ name
      | Async             -> "Async"
      | AsyncFuncDef name -> "AsyncFuncDef:" ^ name

    let is_named = function
      | FuncDef _
      | AsyncFuncDef _
      | ClassDef _
          -> true
      | _ -> false

    let is_named_orig = is_named

    let anonymize = function
      | FuncDef name  -> FuncDef ""
      | ClassDef name -> ClassDef ""
      | AsyncFuncDef name -> AsyncFuncDef ""
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
      | Async -> mkstr 8
      | AsyncFuncDef name -> combo 9 [name]

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
        | Async             -> "Async", []
        | AsyncFuncDef name -> "AsyncFuncDef", ["name",name]
      in
      name, attrs
      
  end (* of module Statement *)

module SimpleStatement =
  struct
    type t = 
      | Expr
      | Assign of AssignmentOperator.t
      | Print
      | Del
      | Pass
      | Break
      | Continue
      | Return
      | Raise
      | Yield
      | Import
      | FromImport
      | Global
      | Exec
      | Assert
      | AnnAssign
      | RaiseFrom
      | Nonlocal

    let to_string = function
      | Expr       -> "Expr"
      | Assign aop -> sprintf "Assignment.%s" (AssignmentOperator.to_string aop)
      | Print      -> "Print"
      | Del        -> "Del"
      | Pass       -> "Pass"
      | Break      -> "Break"
      | Continue   -> "Continue"
      | Return     -> "Return"
      | Raise      -> "Raise"
      | Yield      -> "Yield"
      | Import     -> "Import"
      | FromImport -> "FromImport"
      | Global     -> "Global"
      | Exec       -> "Exec"
      | Assert     -> "Assert"
      | AnnAssign  -> "AnnAssign"
      | RaiseFrom  -> "RaiseFrom"
      | Nonlocal   -> "Nonlocal"

    let to_short_string = function
      | Expr       -> mkstr 0
      | Assign aop -> catstr [mkstr 1; AssignmentOperator.to_short_string aop]
      | Print      -> mkstr 2
      | Del        -> mkstr 3
      | Pass       -> mkstr 4
      | Break      -> mkstr 5
      | Continue   -> mkstr 6
      | Return     -> mkstr 7
      | Raise      -> mkstr 8
      | Yield      -> mkstr 9
      | Import     -> mkstr 10
      | FromImport -> mkstr 11
      | Global     -> mkstr 12
      | Exec       -> mkstr 13
      | Assert     -> mkstr 14
      | AnnAssign  -> mkstr 15
      | RaiseFrom  -> mkstr 16
      | Nonlocal   -> mkstr 17

    let anonymize ?(more=false) = function
      | Assign aop -> Assign AssignmentOperator.Eq
      | lab -> lab

    let to_tag sstmt =
      let name, attrs =
	match sstmt with
	| Expr       -> "ExprStmt", []
	| Assign aop -> AssignmentOperator.to_tag aop
	| Print      -> "PrintStmt", []
	| Del        -> "DelStmt", []
	| Pass       -> "PassStmt", []
	| Break      -> "BreakStmt", []
	| Continue   -> "ContinueStmt", []
	| Return     -> "ReturnStmt", []
	| Raise      -> "RaiseStmt", []
	| Yield      -> "YieldStmt", []
	| Import     -> "ImportStmt", []
	| FromImport -> "FromImportStmt", []
	| Global     -> "GlobalStmt", []
	| Exec       -> "ExecStmt", []
	| Assert     -> "AssertStmt", []
        | AnnAssign  -> "AnnAssignStmt", []
        | RaiseFrom  -> "RaiseFromStmt", []
        | Nonlocal   -> "NonlocalStmt", []
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
    | ListFor
    | Dict
    | StringConv
    | AttrRef
    | Subscription
    | Slicing
    | Call of tie_id
    | Await

  let to_string = function
    | Name name    -> sprintf "Name:%s" name
    | Literal lit  -> Literal.to_string lit
    | Paren        -> "Paren"
    | Tuple        -> "Tuple"
    | Yield        -> "Yield"
    | Test         -> "Test"
    | List         -> "List"
    | ListFor      -> "ListFor"
    | Dict         -> "Dict"
    | StringConv   -> "StringConv"
    | AttrRef      -> "AttrRef"
    | Subscription -> "Subscription"
    | Slicing      -> "Slicing"
    | Call tid     -> "Call:" ^ (tid_to_string tid)
    | Await        -> "Await"

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
    | ListFor      -> mkstr 7
    | Dict         -> mkstr 8
    | StringConv   -> mkstr 9 
    | AttrRef      -> mkstr 10
    | Subscription -> mkstr 11
    | Slicing      -> mkstr 12
    | Call tid     -> combo 13 [tid_to_string tid]
    | Await        -> mkstr 14
			      
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
      | ListFor      -> "ListForAtom", []
      | Dict         -> "DictAtom", []
      | StringConv   -> "StringConvAtom", []
      | AttrRef      -> "AttrRef", []
      | Subscription -> "Subscription", []
      | Slicing      -> "Slicing", []
      | Call tid     -> "Call", mktidattr tid
      | Await        -> "Await", []
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
  | DottedName of string
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
  | Decorators of name
  | Decorator of name
  | Finally
  | In
  | Yield
  | LHS
  | RHS
  | As
  | ListIf
  | KeyDatum
  | SliceItem
  | Ellipsis
  | Arguments of tie_id
  | NamedArguments of name
  | Argument
  | CompArgument
  | AssignArgument
  | GenFor
  | AsyncGenFor
  | GenIf
  | Inheritance
  | Chevron
  | From
  | ParamDef
  | ListParamDef
  | TypedParamDef
  | WithItem
  | StarStar
  | Star
  | Named
  | ReturnAnnotation
  | Dots of int
  | Stride

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
           | Ast.PSlong(_, s) -> "PSlong:" ^ s
           | Ast.PSshort(_, s) -> "PSshort:" ^ s
         ) ";" pystrs) ^
      "]"

let rec primary_to_string prim = primary_desc_to_string prim.Ast.prim_desc

and primary_desc_to_string = function
  | Ast.Pname name            -> sprintf "Pname(%s)" (name_to_string name)
  | Ast.Pliteral lit          -> sprintf "Pliteral(%s)" (literal_to_string lit)
  | Ast.Pparen expr           -> sprintf "Pparen(%s)" (expr_to_string expr)
  | Ast.Ptuple exprs          -> "Ptuple" ^ (exprs_to_string exprs)
  | Ast.Pyield exprs          -> "Pyield" ^ (exprs_to_string exprs)
  | Ast.PcompT(expr, compfor) -> sprintf "PcompT(%s,%s)" (expr_to_string expr) (compfor_to_string compfor)
  | Ast.PcompL(expr, compfor) -> sprintf "PcompL(%s,%s)" (expr_to_string expr) (compfor_to_string compfor)
  | Ast.Plist exprs -> "Plist" ^ (exprs_to_string exprs)
  | Ast.Plistnull -> "Plistnull"
  | Ast.Pdictorset dictorsetmaker -> "Pdictorset" ^ (dictorsetmaker_to_string dictorsetmaker)
  | Ast.Pdictnull -> "Pdictnull"
  | Ast.Pstrconv exprs -> "Pstrconv" ^ (exprs_to_string exprs)
  | Ast.Pattrref(prim, name) -> sprintf "Pattrref(%s,%s)" (primary_to_string prim) (name_to_string name)
  | Ast.Psubscript(prim, exprs) -> sprintf "Psubscript(%s,%s)" (primary_to_string prim) (exprs_to_string exprs)
  | Ast.Pslice(prim, sliceitems) ->
      sprintf "Pslice(%s,[%s])" (primary_to_string prim) (Xlist.to_string sliceitem_to_string ";" sliceitems)
  | Ast.Pcall(prim, arglist) -> sprintf "Pcall(%s,%s)" (primary_to_string prim) (arglist_to_string arglist)
  | Ast.Pawait prim -> sprintf "Pawait(%s)" (primary_to_string prim)

and expr_to_string expr = 
  match expr.Ast.expr_desc with
  | Ast.Eprimary prim -> sprintf "Eprimary(%s)" (primary_to_string prim)
  | Ast.Epower(prim, expr) -> sprintf "Epower(%s,%s)" (primary_to_string prim) (expr_to_string expr)
  | Ast.Ebop(expr1, bop, expr2) ->
      sprintf "Ebop(%s,%s,%s)" (expr_to_string expr1) (bop_to_string bop) (expr_to_string expr2)
  | Ast.Euop(uop, expr) -> sprintf "Euop(%s,%s)" (uop_to_string uop) (expr_to_string expr)

  | Ast.Elambda(params, expr) ->
      "Elambda(" ^
      (match params with 
      | _, [] -> "" 
      | _ -> parameters_to_string params) ^
      (expr_to_string expr)

  | Ast.Econd(expr1, expr2, expr3) ->
      sprintf "Econd(%s,%s,%s)" (expr_to_string expr1) (expr_to_string expr2) (expr_to_string expr3)
  | Ast.Estar expr -> sprintf "Estar(%s)" (expr_to_string expr)
  | Ast.Enamed(expr1, expr2) -> sprintf "Enamed(%s,%s)" (expr_to_string expr1) (expr_to_string expr2)
  | Ast.Efrom expr -> sprintf "Efrom(%s)" (expr_to_string expr)
  | Ast.Earg(expr1, expr2) -> sprintf "Earg(%s,%s)" (expr_to_string expr1) (expr_to_string expr2)

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

and parameters_to_string (_, vargs) =
  "(" ^ 
  (vargs_to_string vargs) ^ "," ^
  ")"

and vararg_to_string = function
  | Ast.VAarg(fpdef, expr_opt) -> (fpdef_to_string fpdef) ^ "," ^ (opt_to_string expr_to_string expr_opt)
  | Ast.VAargs(_, None)     -> "*"
  | Ast.VAargs(_, (Some n)) -> "*" ^ (name_to_string n)
  | Ast.VAkwargs(_, n)      -> "**" ^ (name_to_string n)

and vargs_to_string vargs = "[" ^ (Xlist.to_string vararg_to_string ";" vargs) ^ "]"

and fpdef_to_string = function
  | Ast.Fname n -> sprintf "Fname(%s)" (name_to_string n)
  | Ast.Ftyped(_, name, expr) -> sprintf "Ftyped(%s,%s)" (name_to_string name) (expr_to_string expr)
  | Ast.Flist(_, fpdefs) -> sprintf "Flist[%s]" (Xlist.to_string fpdef_to_string ";" fpdefs)

and exprs_to_string exprs = sprintf "[%s]" (Xlist.to_string expr_to_string ";" exprs)

and compfor_to_string (_, (exprs, expr, compiter_opt), async) =
  (if async then "async " else "")^
  "for " ^ 
  (exprs_to_string exprs) ^ " in " ^ (expr_to_string expr) ^ " " ^ 
  (opt_to_string compiter_to_string compiter_opt)

and compiter_to_string = function
  | Ast.Cfor gf -> "Cfor(" ^ (compfor_to_string gf) ^ ")"
  | Ast.Cif gi -> "Cif(" ^ (compif_to_string gi) ^ ")"

and compif_to_string (_, expr, compiter_opt) =
  "(" ^ 
  (expr_to_string expr) ^ "," ^ (opt_to_string compiter_to_string compiter_opt) ^ 
  ")"

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

and dictelem_to_string delem =
  match delem.Ast.delem_desc with
  | DEkeyValue(e1, e2) -> (expr_to_string e1)^":"^(expr_to_string e2)
  | DEstarStar e -> "**"^(expr_to_string e)

and dictorsetmaker_to_string dictorsetmaker = 
  let s =
    match dictorsetmaker with
    | Ast.DSMdict key_dats -> Xlist.to_string dictelem_to_string "," key_dats

    | Ast.DSMdictC(delem, compfor) -> (dictelem_to_string delem)^" "^(compfor_to_string compfor)

    | Ast.DSMset es -> Xlist.to_string expr_to_string "," es

    | Ast.DSMsetC(e, compfor) ->
	(expr_to_string e)^" "^(compfor_to_string compfor)
  in
  "{"^s^"}"

and sliceitem_to_string = function
  | Ast.SIexpr expr -> "SIexpr(" ^ (expr_to_string expr) ^ ")"
  | Ast.SI2(_, expr_opt1, expr_opt2) ->
      "SI2(" ^
      (opt_to_string expr_to_string expr_opt1) ^ "," ^
      (opt_to_string expr_to_string expr_opt2) ^ ")"
  | Ast.SI3(_, expr_opt1, expr_opt2, expr_opt3) ->
      "SI3(" ^
      (opt_to_string expr_to_string expr_opt1) ^ "," ^
      (opt_to_string expr_to_string expr_opt2) ^ "," ^
      (opt_to_string expr_to_string expr_opt3) ^ ")"
  | Ast.SIellipsis _ -> "SIellipsis"

and arglist_to_string (_, args) = sprintf "(%s)" (Xlist.to_string argument_to_string "," args)

and argument_to_string = function
  | Aarg(_, expr, expr_opt) -> begin
      (expr_to_string expr)^
      (match expr_opt with
      | Some e -> "="^(expr_to_string e)
      | _ -> "")
  end
  | Acomp(_, expr, compfor) -> (expr_to_string expr)^" "^(compfor_to_string compfor)
  | Aassign(_, expr1, expr2) -> (expr_to_string expr1)^":="^(expr_to_string expr2)
  | Aargs(_, expr) -> "*"^(expr_to_string expr)
  | Akwargs(_, expr) -> "**"^(expr_to_string expr)

let of_statement stmt =
  Statement
    (match stmt with
    | Ast.Ssimple _                        -> Statement.Simple
    | Ast.Sif _                            -> Statement.If
    | Ast.Swhile _                         -> Statement.While
    | Ast.Sfor _                           -> Statement.For
    | Ast.Stry _                           -> Statement.Try
    | Ast.Stryfin _                        -> Statement.Try
    | Ast.Swith _                          -> Statement.With
    | Ast.Sasync _                         -> Statement.Async
    | Ast.Sasync_funcdef(_, name, _, _, _) -> Statement.AsyncFuncDef (conv_name name)
    | Ast.Sfuncdef(_, name, _, _, _)       -> Statement.FuncDef (conv_name name)
    | Ast.Sclassdef(_, name, _, _)         -> Statement.ClassDef (conv_name name)
    )

let tid_of_import name_as_names =
  let dottedname_as_names_to_string dname_as_names =
    let f = function
      | (dn, Some n) -> (dottedname_to_string dn)^" as "^(conv_name n)
      | (dn, None) -> dottedname_to_string dn
    in
    String.concat "," (List.map f dname_as_names)
  in
  let s = dottedname_as_names_to_string name_as_names in
  mktid
    (Digest.to_hex (Digest.string s))
    ""

let tid_of_from_import (dots_opt, dname_opt, name_as_names) =
  let name_as_names_to_string name_as_names =
    let f = function
      | (n, Some n0) -> (conv_name n)^" as "^(conv_name n0)
      | (n, None) -> conv_name n
    in
    match name_as_names with
    | [] -> "*"
    | _ -> String.concat "," (List.map f name_as_names)
  in
  let s =
    (match dots_opt with
    | Some (_, ndots) -> String.make ndots '.'
    | _ -> "")^
    (match dname_opt with
    | Some dname -> dottedname_to_string dname
    | _ -> "")^" import "^
    (name_as_names_to_string name_as_names)
  in
  mktid
    (Digest.to_hex (Digest.string s))
    ""

let of_simplestmt sstmt = 
  SimpleStatement
    (match sstmt with
    | Ast.SSexpr _               -> SimpleStatement.Expr
    | Ast.SSassign _             -> SimpleStatement.Assign AssignmentOperator.Eq
    | Ast.SSaugassign(_, aop, _) -> SimpleStatement.Assign (AssignmentOperator.of_aop aop)
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
    | Ast.SSfrom _               -> SimpleStatement.FromImport
    | Ast.SSglobal _             -> SimpleStatement.Global
    | Ast.SSexec _               -> SimpleStatement.Exec
    | Ast.SSexec2 _              -> SimpleStatement.Exec
    | Ast.SSexec3 _              -> SimpleStatement.Exec
    | Ast.SSassert _             -> SimpleStatement.Assert
    | Ast.SSassert2 _            -> SimpleStatement.Assert
    | Ast.SSannassign _          -> SimpleStatement.AnnAssign
    | Ast.SSraisefrom _          -> SimpleStatement.RaiseFrom
    | Ast.SSnonlocal _           -> SimpleStatement.Nonlocal
    )

let of_bop bop = BinaryOperator (BinaryOperator.of_bop bop)

let of_uop uop = UnaryOperator (UnaryOperator.of_uop uop)

let tid_of_primary prim =
  mktid
    (Digest.to_hex (Digest.string (primary_to_string prim)))
    ""

let of_primary p =
  Primary
    (match p with
    | Ast.Pname name      -> Primary.Name (conv_name name)
    | Ast.Pliteral lit    -> Primary.Literal (Literal.of_literal lit)
    | Ast.Pparen _        -> Primary.Paren
    | Ast.Ptuple _        -> Primary.Tuple
    | Ast.Pyield _        -> Primary.Yield
    | Ast.PcompT _        -> Primary.Test
    | Ast.PcompL _        -> Primary.ListFor
    | Ast.Plist _         -> Primary.List
    | Ast.Plistnull       -> Primary.List 
    | Ast.Pdictorset _    -> Primary.Dict
    | Ast.Pdictnull       -> Primary.Dict 
    | Ast.Pstrconv _      -> Primary.StringConv
    | Ast.Pattrref _      -> Primary.AttrRef
    | Ast.Psubscript _    -> Primary.Subscription
    | Ast.Pslice _        -> Primary.Slicing
    | Ast.Pcall (prim, _) -> Primary.Call (tid_of_primary prim)
    | Ast.Pawait _        -> Primary.Await
    )


let rec to_string = function
  | Dummy                 -> "Dummy"

  | Primary p             -> Primary.to_string p
  | UnaryOperator uo      -> UnaryOperator.to_string uo
  | BinaryOperator bo     -> BinaryOperator.to_string bo
  | Statement stmt        -> Statement.to_string stmt
  | SimpleStatement sstmt -> SimpleStatement.to_string sstmt

  | FileInput n           -> sprintf "FileInput:%s" n
  | DottedName s          -> sprintf "DottedName:%s" s
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
  | Decorators n          -> sprintf "Decorators:%s" n
  | Decorator n           -> sprintf "Decorator:%s" n
  | Finally               -> "Finally"
  | In                    -> "In"
  | Yield                 -> "Yield"
  | LHS                   -> "LHS"
  | RHS                   -> "RHS"
  | As                    -> "As"
  | ListIf                -> "ListIf"
  | KeyDatum              -> "KeyDatum"
  | SliceItem             -> "SliceItem"
  | Ellipsis              -> "Ellipsis"
  | Arguments tid         -> sprintf "Arguments:%s" (tid_to_string tid)
  | NamedArguments n      -> sprintf "NamedArguments:%s" n
  | Argument              -> "Argument"
  | CompArgument          -> "CompArgument"
  | AssignArgument        -> "AssignArgument"
  | GenFor                -> "GenFor"
  | AsyncGenFor           -> "AsyncGenFor"
  | GenIf                 -> "GenIf"
  | Inheritance           -> "Inheritance"
  | Chevron               -> "Chevron"
  | From                  -> "From"
  | ParamDef              -> "ParamDef"
  | ListParamDef          -> "ListParamDef"
  | TypedParamDef         -> "TypedParamDef"
  | WithItem              -> "WithItem"
  | StarStar              -> "StarStar"
  | Star                  -> "Star"
  | Named                 -> "Named"
  | ReturnAnnotation      -> "ReturnAnnotation"
  | Dots i                -> sprintf "Dots:%d" i
  | Stride                -> "Stride"

let anonymize ?(more=false) = function
  | CompArgument
  | AssignArgument
  | AsyncGenFor when more  -> GenFor
  | Primary p              -> Primary (Primary.anonymize ~more p)
  | Statement stmt         -> Statement (Statement.anonymize stmt)
  | SimpleStatement sstmt  -> SimpleStatement (SimpleStatement.anonymize ~more sstmt)
  | FileInput n            -> FileInput ""
  | Name n                 -> Name ""
  | DottedName n when more -> Name ""
  | DottedName n           -> DottedName ""
  | NamedSuite n           -> NamedSuite ""
  | NamedParameters n      -> NamedParameters ""
  | Arguments tid          -> Arguments (anonymize_tid ~more tid)
  | NamedArguments n       -> NamedArguments ""
  | Decorator n            -> Decorator ""
  | Decorators n           -> Decorators ""
  | Dots i                 -> Dots 0
  | lab                    -> lab

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

  | FileInput n  -> combo 6 [n]

  | DottedName s -> combo 7 [s]
  | Name n       -> combo 8 [n]

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

  | Decorators n -> combo 21 [n]
  | Decorator n  -> combo 22 [n]
  | Finally      -> mkstr 23
  | In  -> mkstr 24
  | LHS -> mkstr 25
  | RHS -> mkstr 26
  | As  -> mkstr 27
  | ListIf    -> mkstr 29

  | KeyDatum  -> mkstr 32
  | SliceItem -> mkstr 33
  | Ellipsis  -> mkstr 37
  | Arguments tid -> combo 38 [tid_to_string tid]
  | NamedArguments n -> combo 39 [n]

  | Argument    -> mkstr 40
  | GenFor      -> mkstr 41
  | GenIf       -> mkstr 42
  | Inheritance -> mkstr 43
  | Chevron     -> mkstr 44
  | From        -> mkstr 45
  | ParamDef    -> mkstr 48
  | ListParamDef -> mkstr 49
  | WithItem     -> mkstr 52
  | StarStar     -> mkstr 53
  | Star         -> mkstr 54
  | Named        -> mkstr 55
  | ReturnAnnotation -> mkstr 56
  | TypedParamDef    -> mkstr 57
  | Dots i           -> combo 58 [string_of_int i]
  | CompArgument          -> mkstr 59
  | AssignArgument        -> mkstr 60
  | AsyncGenFor           -> mkstr 63
  | Yield                 -> mkstr 65
  | Stride                -> mkstr 66

let to_tag l =
  let name, attrs =
    match l with
    | Dummy                 -> "Dummy", []

    | Primary p             -> Primary.to_tag p
    | UnaryOperator uo      -> UnaryOperator.to_tag uo
    | BinaryOperator bo     -> BinaryOperator.to_tag bo
    | Statement stmt        -> Statement.to_tag stmt
    | SimpleStatement sstmt -> SimpleStatement.to_tag sstmt

    | DottedName s          -> "DottedName", ["name",s]
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
    | Decorators n          -> "Decorators", ["name",n]
    | Decorator n           -> "Decorator", ["name",n]
    | Finally               -> "Finally", []
    | In                    -> "In", []
    | Yield                 -> "Yield", []
    | LHS                   -> "Lhs", []
    | RHS                   -> "Rhs", []
    | As                    -> "As", []
    | ListIf                -> "ListIf", []
    | KeyDatum              -> "KeyDatum", []
    | SliceItem             -> "SliceItem", []
    | Ellipsis              -> "Ellipsis", []
    | Arguments tid         -> "Arguments", mktidattr tid
    | NamedArguments n      -> "NamedArguments", ["name",n]
    | Argument              -> "Argument", []
    | CompArgument          -> "CompArgument", []
    | AssignArgument        -> "AssignArgument", []
    | GenFor                -> "GenFor", []
    | AsyncGenFor           -> "AsyncGenFor", []
    | GenIf                 -> "GenIf", []
    | Inheritance           -> "Inheritance", []
    | Chevron               -> "Chevron", []
    | From                  -> "From", []
    | ParamDef              -> "ParamDef", []
    | ListParamDef          -> "ListParamDef", []
    | TypedParamDef         -> "TypedParamDef", []
    | WithItem              -> "WithItem", []
    | StarStar              -> "StarStar", []
    | Star                  -> "Star", []
    | Named                 -> "Named", []
    | ReturnAnnotation      -> "ReturnAnnotation", []
    | Dots i                -> "Dots", ["ndots",string_of_int i]
    | Stride                -> "Stride", []

    | FileInput n           -> "FileInput", ["name",n]
  in
  name, attrs


let to_char lab = '0' (* to be implemented *)


let to_elem_data = Astml.to_elem_data lang_prefix to_tag


let is_common_name =
  let common_name_list = [
    "True"; "False"; "None"; "self";

    "abs"; "all"; "any"; "bin"; "bool"; "bytearray"; "callable"; "chr";
    "classmethod"; "compile"; "complex"; "delattr"; "dict"; "dir"; "divmod";
    "enumerate"; "eval"; "filter"; "float"; "format"; "frozenset";
    "getattr"; "globals"; "hasattr"; "hash"; "help"; "hex"; "id";
    "input"; "int"; "isinstance"; "issubclass"; "iter"; "len";
    "list"; "locals"; "map"; "max"; "memoryview"; "min"; "next";
    "object"; "oct"; "open"; "ord"; "pow"; "property"; "range";
    "repr"; "reversed"; "round"; "set"; "setattr"; "slice";
    "sorted"; "staticmethod"; "str"; "sum"; "super"; "tuple";
    "type"; "vars"; "zip"; "__import__"; "NotImplemented";
    "Ellipsis"; "__debug__";
  ]
  in
  let s = Xset.create (List.length common_name_list) in
  let _ = List.iter (Xset.add s) common_name_list in
  Xset.mem s

let is_named = function
  | FileInput _
  | Name _
  | DottedName _
  | NamedSuite _
  | NamedParameters _
  | NamedArguments _
  | Decorator _
  | Decorators _
  | Primary (Primary.Name _)
    -> true
  | Statement stmt -> Statement.is_named stmt
  | _ -> false

let is_named_orig = function
  | FileInput _
  | Decorator _
  | Name _
  | DottedName _
  | Primary (Primary.Name _)
    -> true
  | Statement stmt -> Statement.is_named_orig stmt
  | _ -> false

let is_compatible _ _ = false

let is_order_insensitive = function
  | _ -> false

let quasi_eq _ _ = false

let relabel_allowed = function (* FIXME: should be tuned! *)
  | Primary _, SimpleStatement _ | SimpleStatement _, Primary _
  | UnaryOperator _, Primary _ | Primary _, UnaryOperator _
  | BinaryOperator _, Primary _ | Primary _, BinaryOperator _
    -> true
  | l1, l2 -> anonymize2 l1 = anonymize2 l2

let move_disallowed = function
  | Primary (Primary.Name n) | Name n when is_common_name n -> true
  | _ -> false

let is_common = function
  | Name n when is_common_name n -> true
  | Else
  | _ -> false

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
      | NamedParameters _
      | Arguments _
      | NamedArguments _
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
  | Primary (Primary.Literal Literal.String _) -> true
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
  | _ -> raise Not_found

let has_value = function
  | Primary (Primary.Literal _) -> true
  | _ -> false

let has_non_trivial_value lab =
  try
    let v = get_value lab in
    v <> "0" && v <> "1" && v <> "True" && v <> "False" && v <> "None"
  with
    Not_found -> false

let cannot_be_keyroot nd = false

let is_phantom = function
  | Decorators _
  | Targets
  | Parameters
  | NamedParameters _
  | Arguments _
  | NamedArguments _
      -> true
  | _ -> false

let is_special _ = false

open Astml.Attr

let of_elem_data =

  let mkprim x = Primary x in
  let mklit x = Primary (Primary.Literal x) in
  let mksstmt x = SimpleStatement x in
  let mkaop x = SimpleStatement (SimpleStatement.Assign x) in
  let mkstmt x = Statement x in
  let mkbop x = BinaryOperator x in
  let mkuop x = UnaryOperator x in

  let tag_list = [
    "Dummy",              (fun a -> Dummy);
    "NameAtom",           (fun a -> Name(find_name a));
    "IntegerLiteral",     (fun a -> mklit(Literal.Integer(find_value a)));
    "LongIntegerLiteral", (fun a -> mklit(Literal.LongInteger(find_value a)));
    "FloatNumberLiteral", (fun a -> mklit(Literal.FloatNumber(find_value a)));
    "ImagNumberLiteral",  (fun a -> mklit(Literal.ImagNumber(find_value a)));
    "StringLiteral",      (fun a -> mklit(Literal.String(find_value a)));
    "CatStringLiteral",   (fun a -> mklit(Literal.CatString(find_value a)));
    "ParenAtom",          (fun a -> mkprim(Primary.Paren));
    "TupleAtom",          (fun a -> mkprim(Primary.Tuple));
    "YieldAtom",          (fun a -> mkprim(Primary.Yield));
    "TestAtom",           (fun a -> mkprim(Primary.Test));
    "ListAtom",           (fun a -> mkprim(Primary.List));
    "ListForAtom",        (fun a -> mkprim(Primary.ListFor));
    "DictAtom",           (fun a -> mkprim(Primary.Dict));
    "StringConvAtom",     (fun a -> mkprim(Primary.StringConv));
    "AttrRef",            (fun a -> mkprim(Primary.AttrRef));
    "Subscription",       (fun a -> mkprim(Primary.Subscription));
    "Slicing",            (fun a -> mkprim(Primary.Slicing));
    "Call",               (fun a -> mkprim(Primary.Call(find_tid a)));
    "Await",              (fun a -> mkprim(Primary.Await));

    "ExprStmt",           (fun a -> mksstmt(SimpleStatement.Expr));

    "Assign",             (fun a -> mkaop(AssignmentOperator.Eq));
    "AddAssign",          (fun a -> mkaop(AssignmentOperator.AddEq));
    "SubtAssign",         (fun a -> mkaop(AssignmentOperator.SubEq));
    "MultAssign",         (fun a -> mkaop(AssignmentOperator.MulEq));
    "DivAssign",          (fun a -> mkaop(AssignmentOperator.DivEq));
    "ModAssign",          (fun a -> mkaop(AssignmentOperator.ModEq));
    "AndAssign",          (fun a -> mkaop(AssignmentOperator.AndEq));
    "OrAssign",           (fun a -> mkaop(AssignmentOperator.OrEq));
    "XorAssign",          (fun a -> mkaop(AssignmentOperator.XorEq));
    "ShiftLAssign",       (fun a -> mkaop(AssignmentOperator.ShiftLEq));
    "ShiftRAssign",       (fun a -> mkaop(AssignmentOperator.ShiftREq));
    "PowAssign",          (fun a -> mkaop(AssignmentOperator.PowEq));
    "FDivAssign",         (fun a -> mkaop(AssignmentOperator.FDivEq));

    "PrintStmt",          (fun a -> mksstmt(SimpleStatement.Print));
    "DelStmt",            (fun a -> mksstmt(SimpleStatement.Del));
    "PassStmt",           (fun a -> mksstmt(SimpleStatement.Pass));
    "BreakStmt",          (fun a -> mksstmt(SimpleStatement.Break));
    "ContinueStmt",       (fun a -> mksstmt(SimpleStatement.Continue));
    "ReturnStmt",         (fun a -> mksstmt(SimpleStatement.Return));
    "RaiseStmt",          (fun a -> mksstmt(SimpleStatement.Raise));
    "YieldStmt",          (fun a -> mksstmt(SimpleStatement.Yield));
    "ImportStmt",         (fun a -> mksstmt(SimpleStatement.Import));
    "FromImportStmt",     (fun a -> mksstmt(SimpleStatement.FromImport));
    "GlobalStmt",         (fun a -> mksstmt(SimpleStatement.Global));
    "ExecStmt",           (fun a -> mksstmt(SimpleStatement.Exec));
    "AssertStmt",         (fun a -> mksstmt(SimpleStatement.Assert));
    "AnnAssignStmt",      (fun a -> mksstmt(SimpleStatement.AnnAssign));
    "RaiseFromStmt",      (fun a -> mksstmt(SimpleStatement.RaiseFrom));
    "NonlocalStmt",       (fun a -> mksstmt(SimpleStatement.Nonlocal));

    "SimpleStmt",         (fun a -> mkstmt(Statement.Simple));
    "IfStmt",             (fun a -> mkstmt(Statement.If));
    "WhileStmt",          (fun a -> mkstmt(Statement.While));
    "ForStmt",            (fun a -> mkstmt(Statement.For));
    "TryStmt",            (fun a -> mkstmt(Statement.Try));
    "WithStmt",           (fun a -> mkstmt(Statement.With));
    "FuncDef",            (fun a -> mkstmt(Statement.FuncDef(find_name a)));
    "AsyncFuncDef",       (fun a -> mkstmt(Statement.AsyncFuncDef(find_name a)));
    "ClassDef",           (fun a -> mkstmt(Statement.ClassDef(find_name a)));
    "Async",              (fun a -> mkstmt(Statement.Async));

    "Mult",               (fun a -> mkbop(BinaryOperator.Mul));
    "Div",                (fun a -> mkbop(BinaryOperator.Div));
    "FDiv",               (fun a -> mkbop(BinaryOperator.FDiv));
    "Mod",                (fun a -> mkbop(BinaryOperator.Mod));
    "Add",                (fun a -> mkbop(BinaryOperator.Add));
    "Subt",               (fun a -> mkbop(BinaryOperator.Sub));
    "ShiftL",             (fun a -> mkbop(BinaryOperator.ShiftL));
    "ShiftR",             (fun a -> mkbop(BinaryOperator.ShiftR));
    "Eq",                 (fun a -> mkbop(BinaryOperator.Eq));
    "NotEq",              (fun a -> mkbop(BinaryOperator.Neq));
    "Le",                 (fun a -> mkbop(BinaryOperator.Lt));
    "Gt",                 (fun a -> mkbop(BinaryOperator.Gt));
    "Le",                 (fun a -> mkbop(BinaryOperator.Le));
    "Ge",                 (fun a -> mkbop(BinaryOperator.Ge));
    "BitAnd",             (fun a -> mkbop(BinaryOperator.BitAnd));
    "BitOr",              (fun a -> mkbop(BinaryOperator.BitOr));
    "BitXor",             (fun a -> mkbop(BinaryOperator.BitXor));
    "And",                (fun a -> mkbop(BinaryOperator.And));
    "Or",                 (fun a -> mkbop(BinaryOperator.Or));
    "Is",                 (fun a -> mkbop(BinaryOperator.Is));
    "IsNot",              (fun a -> mkbop(BinaryOperator.IsNot));
    "InOp",               (fun a -> mkbop(BinaryOperator.In));
    "NotIn",              (fun a -> mkbop(BinaryOperator.NotIn));

    "Positive",           (fun a -> mkuop(UnaryOperator.Positive));
    "Negative",           (fun a -> mkuop(UnaryOperator.Negative));
    "Complement",         (fun a -> mkuop(UnaryOperator.Complement));
    "Not",                (fun a -> mkuop(UnaryOperator.Not));

    "DottedName",         (fun a -> DottedName(find_name a));
    "name",               (fun a -> Name(find_name a));
    "Lambda",             (fun a -> Lambda);
    "Test",               (fun a -> Test);
    "Power",              (fun a -> Power);
    "Elif",               (fun a -> Elif);
    "Else",               (fun a -> Else);
    "Targets",            (fun a -> Targets);
    "Target",             (fun a -> Target);
    "Except",             (fun a -> Except);
    "Suite",              (fun a -> Suite);
    "NamedSuite",         (fun a -> NamedSuite(find_name a));
    "Parameters",         (fun a -> Parameters);
    "NamedParameters",    (fun a -> NamedParameters(find_name a));
    "Decorators",         (fun a -> Decorators(find_name a));
    "Decorator",          (fun a -> Decorator(find_name a));
    "Finally",            (fun a -> Finally);
    "In",                 (fun a -> In);
    "Yield",              (fun a -> Yield);
    "Lhs",                (fun a -> LHS);
    "Rhs",                (fun a -> RHS);
    "As",                 (fun a -> As);
    "ListIf",             (fun a -> ListIf);
    "KeyDatum",           (fun a -> KeyDatum);
    "SliceItem",          (fun a -> SliceItem);
    "Ellipsis",           (fun a -> Ellipsis);
    "Arguments",          (fun a -> Arguments(find_tid a));
    "NamedArguments",     (fun a -> NamedArguments(find_name a));
    "Argument",           (fun a -> Argument);
    "CompArgument",       (fun a -> CompArgument);
    "AssignArgument",     (fun a -> AssignArgument);
    "GenFor",             (fun a -> GenFor);
    "AsyncGenFor",        (fun a -> AsyncGenFor);
    "GenIf",              (fun a -> GenIf);
    "Inheritance",        (fun a -> Inheritance);
    "Chevron",            (fun a -> Chevron);
    "From",               (fun a -> From);
    "ParamDef",           (fun a -> ParamDef);
    "ListParamDef",       (fun a -> ListParamDef);
    "TypedParamDef",      (fun a -> TypedParamDef);
    "WithItem",           (fun a -> WithItem);
    "StarStar",           (fun a -> StarStar);
    "Star",               (fun a -> Star);
    "Named",              (fun a -> Named);
    "ReturnAnnotation",   (fun a -> ReturnAnnotation);
    "Dots",               (fun a -> Dots(find_int a "ndots"));
    "Stride",             (fun a -> Stride);
    "FileInput",          (fun a -> FileInput(find_name a));
  ]
  in
  let tbl = Hashtbl.create (List.length tag_list) in
  let _ =
    List.iter (fun (tname, lab) -> Hashtbl.add tbl tname lab) tag_list
  in
  let of_elem name attrs (_ : string) =
    try
      (Hashtbl.find tbl name) attrs
    with
    | Not_found -> failwith ("Py_label.of_tag: tag not found: "^name)
    | e -> failwith ("Py_label.of_tag: "^(Printexc.to_string e))
  in
  of_elem

