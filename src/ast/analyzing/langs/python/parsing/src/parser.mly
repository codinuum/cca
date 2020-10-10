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
 * A parser for the Python programming language (for v2.7)
 * based on Python-2.7.3/Grammar/Grammar
 * parser.mly
 *
 *)

%{
open Printf
open Common
open Ast

open Parser_aux
module A = Parser_aux.F (Stat)
open A
open Stat
%}

%parameter <Stat : Parser_aux.STATE_T>

%token EOF

%token <string> NAMEx
%token <string> INTEGER
%token <string> LONGINTEGER
%token <string> FLOATNUMBER
%token <string> IMAGNUMBER
%token <string> SHORTSTRING

%token <string> LONGSTRING_BEGIN_S
%token <string> LONGSTRING_BEGIN_D
%token <string> LONGSTRING_REST

%token <int> NEWLINE
%token INDENT DEDENT

(* Operators *)
%token PLUS MINUS STAR STAR_STAR SLASH SLASH_SLASH PERCENT
%token LT_LT GT_GT AMP PIPE HAT TILDE
%token GT LT EQ_EQ LT_EQ GT_EQ EXCLAM_EQ LT_GT

(* Delimiters *)
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE AT
%token COMMA COLON DOT BACKQUOTE EQ SEMICOLON 
%token PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ SLASH_SLASH_EQ PERCENT_EQ 
%token AMP_EQ PIPE_EQ HAT_EQ LT_LT_EQ GT_GT_EQ STAR_STAR_EQ

(* Keywords *)
%token AND AS ASSERT BREAK CLASS CONTINUE DEF DEL
%token ELIF ELSE EXCEPT EXEC
%token FINALLY FOR FROM GLOBAL
%token IF IMPORT IN IS LAMBDA NOT OR
%token PASS PRINT RAISE RETURN TRY
%token WHILE WITHx YIELD

%start main
%type <Ast.fileinput> main

%%
(********** Rules **********)

main:
| file_input EOF { Fileinput(get_loc $startofs $endofs, $1) }
|            EOF { Fileinput(get_loc $startofs $endofs, []) }
;

file_input:
| file_input_ { List.rev $1 }
;
file_input_:
|             NEWLINE { [] }
|             stmt { [$1] }
| file_input_ NEWLINE { $1 }
| file_input_ stmt { $2 :: $1 }
;

decorator:
| AT dotted_name                       NEWLINE { get_loc $startofs $endofs($2), $2, emptyarglist }
| AT dotted_name LPAREN         RPAREN NEWLINE { get_loc $startofs $endofs($4), $2, emptyarglist }
| AT dotted_name LPAREN arglist RPAREN NEWLINE { get_loc $startofs $endofs($5), $2, $4 }
;

decorators:
| decorator            { [$1] }
| decorator decorators { $1 :: $2 }
;

decorated:
| decorators classdef { match $2 with Sclassdef(_, n, t, s) -> Sclassdef($1, n, t, s) | _ -> assert false }
| decorators funcdef  { match $2 with Sfuncdef(_, n, p, s) -> Sfuncdef($1, n, p, s) | _ -> assert false }

funcdef:
| DEF name parameters COLON suite { Sfuncdef([], $2, $3, $5) }
;
name:
| NAMEx { get_loc $startofs $endofs, $1 }
;

parameters:
| LPAREN             RPAREN { Ast.Loc.dummy, [], None, None }
| LPAREN varargslist RPAREN { $2 }
;

varargslist:
| v=vararg_comma_list STAR n0=name                         { get_loc $startofs $endofs, v, Some n0, None }
| v=vararg_comma_list STAR n0=name COMMA STAR_STAR n1=name { get_loc $startofs $endofs, v, Some n0, Some n1 }
| v=vararg_comma_list                    STAR_STAR n1=name { get_loc $startofs $endofs, v, None, Some n1 }
| v=varargs_       { get_loc $startofs $endofs, v, None, None }
| v=varargs_ COMMA { get_loc $startofs $endofs, v, None, None }
;
%inline vararg_comma_list:
| (* *) { [] }
| v=vararg_comma_list_ { v }
;
%inline vararg_comma_list_:
| v=varargs_ COMMA { v }
;
varargs_:
|                vararg { [$1] }
| varargs_ COMMA vararg { $1 @ [$3] }
;
vararg:
| fpdef         { $1, None }
| fpdef EQ test { $1, Some $3 }
;

fpdef:
| name                 { Fname $1 }
| LPAREN fplist RPAREN { Flist(get_loc $startofs $endofs, $2) }
;

fplist:
| fpdefs       { List.rev $1 }
| fpdefs COMMA { List.rev $1 }
;
fpdefs:
|              fpdef { [$1] }
| fpdefs COMMA fpdef { $3 :: $1 }
;

stmt:
| simple_stmt { $1 }
| compound_stmt { $1 }
;

simple_stmt:
| simple_stmt_ NEWLINE { $1 }
;
simple_stmt_:
| small_stmts           { mkstmt $startofs $endofs (Ssimple (List.rev $1)) }
| small_stmts SEMICOLON { mkstmt $startofs $endofs (Ssimple (List.rev $1)) }
;
small_stmts:
|                       small_stmt { [$1] }
| small_stmts SEMICOLON small_stmt { $3 :: $1 }
; 

small_stmt:
| small_stmt_ { mksstmt $startofs $endofs $1 }
;
small_stmt_:
| expr_stmt   { $1 }
| print_stmt  { $1 }
| del_stmt    { $1 }
| pass_stmt   { $1 }
| flow_stmt   { $1 }
| import_stmt { $1 }
| global_stmt { $1 }
| exec_stmt   { $1 }
| assert_stmt { $1 }
;

expr_stmt:
| testlist { SSexpr $1.list }
| testlist augassign testlist_or_yield_expr { SSaugassign($1.list, $2, $3) }
| testlist eq_testlists 
    {
     match $2 with
       last :: a -> SSassign($1 :: (List.rev a), last) 
     | _ -> parse_error $startofs $endofs "syntax error"
    }
;
eq_testlists:
|              EQ testlist_or_yield_expr { [$2] }
| eq_testlists EQ testlist_or_yield_expr { $3 :: $1 }
;
testlist_or_yield_expr:
| testlist { $1 }
| yield_expr { $1 }
;

augassign:
| PLUS_EQ        { AaddEq }
| MINUS_EQ       { AsubEq }
| STAR_EQ        { AmulEq }
| SLASH_EQ       { AdivEq }
| PERCENT_EQ     { AmodEq }
| AMP_EQ         { AandEq }
| PIPE_EQ        { AorEq }
| HAT_EQ         { AxorEq }
| LT_LT_EQ       { AshiftLEq }
| GT_GT_EQ       { AshiftREq }
| STAR_STAR_EQ   { ApowEq }
| SLASH_SLASH_EQ { AfdivEq }
;

print_stmt:
| PRINT                { SSprint [] }
| PRINT       testlist { SSprint $2.list }
| PRINT GT_GT testlist 
    { 
      match $3.list with
	h :: t -> SSprintchevron(h, t) 
      | _ -> parse_error $startofs $endofs "syntax error"
    }
;

del_stmt:
| DEL exprlist { SSdel $2 }
;

pass_stmt:
| PASS { SSpass }
;

flow_stmt:
| break_stmt    { $1 }
| continue_stmt { $1 }
| return_stmt   { $1 }
| raise_stmt    { $1 }
| yield_stmt    { $1 }
;

break_stmt:
| BREAK { SSbreak }
;

continue_stmt:
| CONTINUE { SScontinue }
;

return_stmt:
| RETURN          { SSreturn [] }
| RETURN testlist { SSreturn $2.list }
;

yield_stmt:
| yield_expr { SSyield $1.list }
;

raise_stmt:
| RAISE                            { SSraise }
| RAISE test                       { SSraise1 $2 }
| RAISE test COMMA test            { SSraise2($2, $4) }
| RAISE test COMMA test COMMA test { SSraise3($2, $4, $6) }
;

import_stmt:
| import_name { $1 }
| import_from { $1 }
;

import_name:
| IMPORT dotted_as_names { SSimport $2 }
;

import_from:
| FROM dotted_name IMPORT imports 
    {
     begin
       match $2, $4 with
	 [_, "__future__"], [(_, "with_statement"), None] ->
	   env#enable_with_stmt
       | _ -> ()
     end;
     SSfrom($2, $4) 
   }
;
imports:
| STAR { [] }
| LPAREN import_as_names_list RPAREN { $2 }
| import_as_names_list { $1 }
;
import_as_names_list:
| import_as_names       { List.rev $1 }
| import_as_names COMMA { List.rev $1 }
;

import_as_name:
| name         { $1, None }
| name AS name { $1, Some $3 }
;

dotted_as_name:
| dotted_name           { $1, None }
| dotted_name name name { if (snd $2) = "as" then $1, Some $3 else parse_error $startofs $endofs "syntax error" }
| dotted_name AS name   { $1, Some $3 }
;

import_as_names:
|                       import_as_name { [$1] }
| import_as_names COMMA import_as_name { $3 :: $1 }
;

dotted_as_names:
| dotted_as_name                       { [$1] }
| dotted_as_name COMMA dotted_as_names { $1 :: $3 }
;

dotted_name:
| name                 { [$1] }
| name DOT dotted_name { $1 :: $3 }
;

global_stmt:
| GLOBAL names { SSglobal $2 }
;
names:
| name             { [$1] }
| name COMMA names { $1 :: $3 }
;

exec_stmt: 
| EXEC expr                    { SSexec $2 }
| EXEC expr IN test            { SSexec2($2, $4) }
| EXEC expr IN test COMMA test { SSexec3($2, $4, $6) }
;

assert_stmt:
| ASSERT test            { SSassert $2 }
| ASSERT test COMMA test { SSassert2($2, $4) }
;

compound_stmt:
| compound_stmt_ { mkstmt $startofs $endofs $1 }
;
compound_stmt_:
| if_stmt { $1 }
| while_stmt { $1 }
| for_stmt { $1 }
| try_stmt { $1 }
| with_stmt { $1 }
| funcdef { $1 }
| classdef { $1 }
| decorated { $1 }
;

if_stmt:
| IF test COLON suite           { Sif($2, $4, [], None) }
| IF test COLON suite elifs     { Sif($2, $4, $5, None) }
| IF test COLON suite       els { Sif($2, $4, [], Some $5) }
| IF test COLON suite elifs els { Sif($2, $4, $5, Some $6) }
;
elifs:
| elif       { [$1] }
| elif elifs { $1 :: $2 } 
;
elif:
| ELIF test COLON suite { get_loc $startofs $endofs, $2, $4 }
;
els:
| ELSE COLON suite { get_loc $startofs $endofs, $3 }
;

while_stmt:
| WHILE test COLON suite     { Swhile($2, $4, None) }
| WHILE test COLON suite els { Swhile($2, $4, Some $5) }
;

for_stmt:
| FOR exprlist IN testlist COLON suite     { Sfor($2, $4.list, $6, None) }
| FOR exprlist IN testlist COLON suite els { Sfor($2, $4.list, $6, Some $7) }
;

try_stmt:
| try_except              { let t, e = $1 in Stry(t, e, None, None) }
| try_except els          { let t, e = $1 in Stry(t, e, Some $2, None) }
| try_except     finally  { let t, e = $1 in Stry(t, e, None, Some $2) }
| try_except els finally  { let t, e = $1 in Stry(t, e, Some $2, Some $3) }
| TRY COLON suite finally { Stryfin($3, $4) }
;
try_except:
| TRY COLON suite except_clause_suites { $3, $4 }
;
finally:
| FINALLY COLON suite { get_loc $startofs $endofs, $3 }
;
except_clause_suites:
| except_clause COLON suite                      { [$1, $3] }
| except_clause COLON suite except_clause_suites { ($1, $3) :: $4 }
;

with_stmt:
| WITHx with_item_list COLON suite { Swith(List.rev $2, $4) }
;

with_item:
| test         { $1, None }
| test AS expr { $1, Some $3 }
;

with_item_list:
|                      with_item { [$1] }
| with_item_list COMMA with_item { $3 :: $1 }
;

except_clause:
| EXCEPT                 { EX(get_loc $startofs $endofs) }
| EXCEPT test            { EX1(get_loc $startofs $endofs, $2) }
| EXCEPT test COMMA test { EX2(get_loc $startofs $endofs, $2, $4) }
| EXCEPT test AS    test { EX2(get_loc $startofs $endofs, $2, $4) }
;

suite:
| simple_stmt { get_loc $startofs $endofs, [$1] }
| NEWLINE INDENT stmts DEDENT { get_loc $startofs($3) $endofs($3), $3 }
;
stmts:
| stmt       { [$1] }
| stmt stmts { $1 :: $2 }
;

testlist_safe:
| old_tests       { List.rev $1 }
| old_tests COMMA { List.rev $1 }
;
old_tests:
|                 old_test { [$1] }
| old_tests COMMA old_test { $3 :: $1 }
;

old_test:
| or_test { $1 }
| old_lambdef { mkexpr $startofs $endofs $1 }
;

old_lambdef:
| LAMBDA             COLON old_test { Elambda((Ast.Loc.dummy, [], None, None), $3) }
| LAMBDA varargslist COLON old_test { Elambda($2, $4) }
;

test:
| or_test { $1 }
| or_test IF or_test ELSE test { mkexpr $startofs $endofs(Econd($1, $3, $5)) }
| lambdef { mkexpr $startofs $endofs $1 }
;

or_test:
|            and_test { $1 }
| or_test OR and_test { mkexpr $startofs $endofs (Ebop($1, Bor, $3)) }
;

and_test:
|              not_test { $1 }
| and_test AND not_test { mkexpr $startofs $endofs (Ebop($1, Band, $3)) }
;

not_test:
| comparison { $1 }
| NOT not_test { mkexpr $startofs $endofs (Euop(Unot, $2)) }
;

comparison:
| expr { $1 }
| comparison comp_op expr { mkexpr $startofs $endofs (Ebop($1, $2, $3)) }
;

comp_op:
| LT { Blt }
| GT { Bgt }
| EQ_EQ { Beq }
| GT_EQ { Bge }
| LT_EQ { Ble }
| LT_GT { Bneq }
| EXCLAM_EQ { Bneq }
| IN     { Bin }
| NOT IN { BnotIn }
| IS     { Bis }
| IS NOT { BisNot }
;

expr:
| xor_expr           { $1 }
| xor_expr PIPE expr { mkexpr $startofs $endofs (Ebop($1, BbitOr, $3)) }
;

xor_expr:
|              and_expr { $1 }
| xor_expr HAT and_expr { mkexpr $startofs $endofs (Ebop($1, BbitXor, $3)) }
;

and_expr:
|              shift_expr { $1 }
| and_expr AMP shift_expr { mkexpr $startofs $endofs (Ebop($1, BbitAnd, $3)) }
;

shift_expr:
|                  arith_expr { $1 }
| shift_expr LT_LT arith_expr { mkexpr $startofs $endofs (Ebop($1, BshiftL, $3)) }
| shift_expr GT_GT arith_expr { mkexpr $startofs $endofs (Ebop($1, BshiftR, $3)) }
;

arith_expr:
|                  term { $1 }
| arith_expr PLUS  term { mkexpr $startofs $endofs (Ebop($1, Badd, $3)) }
| arith_expr MINUS term { mkexpr $startofs $endofs (Ebop($1, Bsub, $3)) }
;

term:
|          factor { $1 }
| term mop factor { mkexpr $startofs $endofs (Ebop($1, $2, $3)) }
;
mop:
| STAR        { Bmul }
| SLASH       { Bdiv }
| PERCENT     { Bmod }
| SLASH_SLASH { Bfdiv }
;

factor:
| PLUS  factor { mkexpr $startofs $endofs (Euop(Upositive, $2)) }
| MINUS factor { mkexpr $startofs $endofs (Euop(Unegative, $2)) }
| TILDE factor { mkexpr $startofs $endofs (Euop(Ucomplement, $2)) }
| power { $1 }
;

power:
| primary                  { mkexpr $startofs $endofs (Eprimary $1) }
| primary STAR_STAR factor { mkexpr $startofs $endofs (Epower($1, $3)) }
;
primary:
| atom { mkprim $startofs $endofs $1 }
| primary trailer 
    { 
      let p = 
	match $2 with
	  TRattrref n -> Pattrref($1, n)
	| TRsubscript el -> Psubscript($1, el)
	| TRslice sil -> Pslice($1, sil)
	| TRcall al -> Pcall($1, al)
      in mkprim $startofs $endofs p
    }
;

atom:
| LPAREN               RPAREN { Ptuple [] }
| LPAREN testlist_comp RPAREN { $2 }
| LBRACKET           RBRACKET { Plistnull }
| LBRACKET listmaker RBRACKET { Plist $2 }
| LBRACE                RBRACE { Pdictnull }
| LBRACE dictorsetmaker RBRACE { Pdictorset $2 }
| BACKQUOTE testlist1 BACKQUOTE { Pstrconv $2 }
| name { Pname $1 }
| literal { Pliteral $1 }
;
literal:
| INTEGER     { Linteger $1 }
| LONGINTEGER { Llonginteger $1 }
| FLOATNUMBER { Lfloatnumber $1 }
| IMAGNUMBER  { Limagnumber $1 }
| strings     { Lstring $1 }
;
strings:
| stringliteral { [$1] }
| stringliteral strings { $1 :: $2 }
;
stringliteral:
| SHORTSTRING { PSshort(get_loc $startofs $endofs, $1) }
| LONGSTRING_BEGIN_S LONGSTRING_REST { PSlong(get_loc $startofs $endofs, $1 ^ $2) }
| LONGSTRING_BEGIN_D LONGSTRING_REST { PSlong(get_loc $startofs $endofs, $1 ^ $2) }
;

listmaker:
| test list_for { LMfor($1, $2) }
| testlist { LMtest $1.list }
;

testlist_comp:
| test comp_for { Pcomp($1, $2) }
| testlist 
    { 
      if $1.yield then 
	Pyield $1.list
      else
	if $1.comma then 	  
	  (match $1.list with [t] -> Pparen t | _ -> Ptuple $1.list)
	else 
	  Ptuple $1.list
    }
;

lambdef:
| LAMBDA             COLON test { Elambda((Ast.Loc.dummy, [], None, None), $3) }
| LAMBDA varargslist COLON test { Elambda($2, $4) }
;

trailer:
| LPAREN         RPAREN { TRcall (Ast.Loc.dummy, [], None, None) }
| LPAREN arglist RPAREN { TRcall $2 }
| LBRACKET               RBRACKET { TRsubscript [] }
| LBRACKET subscriptlist RBRACKET 
    { 
      if (List.for_all (function SIexpr _ -> true | _ -> false) $2) then
	TRsubscript 
	  (List.map 
	     (function SIexpr e -> e | _ -> parse_error $startofs $endofs "syntax error") $2)
      else TRslice $2
    }
| DOT name { TRattrref $2 }
;

subscriptlist:
| subscripts       { List.rev $1 }
| subscripts COMMA { List.rev $1 }
;
subscripts:
|                  subscript { [$1] }
| subscripts COMMA subscript { $3 :: $1 }
;

subscript:
| DOT DOT DOT { SIellipsis(get_loc $startofs $endofs) }
| test { SIexpr $1 }

|      COLON      { SIproper(get_loc $startofs $endofs, None, None, None) }
| test COLON      { SIproper(get_loc $startofs $endofs, Some $1, None, None) }
|      COLON test { SIproper(get_loc $startofs $endofs, None, Some $2, None) }
| test COLON test { SIproper(get_loc $startofs $endofs, Some $1, Some $3, None) }

|      COLON      sliceop { SIproper(get_loc $startofs $endofs, None, None, $2) }
| test COLON      sliceop { SIproper(get_loc $startofs $endofs, Some $1, None, $3) }
|      COLON test sliceop { SIproper(get_loc $startofs $endofs, None, Some $2, $3) }
| test COLON test sliceop { SIproper(get_loc $startofs $endofs, Some $1, Some $3, $4) }
;

sliceop:
| COLON      { None }
| COLON test { Some $2 }
;

exprlist:
| exprs       { $1 }
| exprs COMMA { $1 }
;
exprs:
|             expr { [$1] }
| exprs COMMA expr { $1 @ [$3] }
;

testlist:
| testlist1       { mktestlist $1 false false }
| testlist1 COMMA { mktestlist $1 true false }
;

dictorsetmaker:
| test COLON test comp_for { DSMdictC($1, $3, $4) }
| test_colon_tests         { DSMdict(List.rev $1) }
| test_colon_tests COMMA   { DSMdict(List.rev $1) }
| test comp_for   { DSMsetC($1, $2) }
| testlist1       { DSMset $1 }
| testlist1 COMMA { DSMset $1 }
;
test_colon_tests:
|                        test_colon_test { [$1] }
| test_colon_tests COMMA test_colon_test { $3 :: $1 }
;
test_colon_test:
| test COLON test { get_loc $startofs $endofs, $1, $3 }
;

classdef:
| CLASS name                        COLON suite { Sclassdef([], $2, [], $4) }
| CLASS name LPAREN testlist RPAREN COLON suite { Sclassdef([], $2, $4.list, $7) }
;

arglist:
| al=arg_comma_list a=argument                                             { get_loc $startofs $endofs, al @ [a], None, None }
| al=arg_comma_list a=argument COMMA                                       { get_loc $startofs $endofs, al @ [a], None, None }
| al=arg_comma_list STAR t0=test cl=comma_arg_list                         { get_loc $startofs $endofs, al, Some (t0, cl), None }
| al=arg_comma_list STAR t0=test cl=comma_arg_list COMMA STAR_STAR t1=test { get_loc $startofs $endofs, al, Some (t0, cl), Some t1 }
| al=arg_comma_list                                      STAR_STAR t1=test { get_loc $startofs $endofs, al, None, Some t1 }
;

%inline comma_arg_list:
| (* *)             { [] }
| c=comma_arg_list_ { c }
;
comma_arg_list_:
|                 COMMA argument { [$2] }
| comma_arg_list_ COMMA argument { $1 @ [$3] }
;
%inline arg_comma_list:
| (* *)             { [] }
| a=arg_comma_list_ { a }
;
arg_comma_list_:
|                 argument COMMA { [$1] }
| arg_comma_list_ argument COMMA { $1 @ [$2] }

argument:
| test                  { get_loc $startofs $endofs, None, $1, None }
| test EQ test          { get_loc $startofs $endofs, Some $1, $3, None }
| test         comp_for { get_loc $startofs $endofs, None, $1, Some $2 }
| test EQ test comp_for { get_loc $startofs $endofs, Some $1, $3, Some $4 }
;

list_iter:
| list_for { LIfor $1 }
| list_if { LIif $1 }
;

list_for:
| FOR exprlist IN testlist_safe           { get_loc $startofs $endofs, $2, $4, None }
| FOR exprlist IN testlist_safe list_iter { get_loc $startofs $endofs, $2, $4, Some $5 }
;

list_if:
| IF old_test           { get_loc $startofs $endofs, $2, None }
| IF old_test list_iter { get_loc $startofs $endofs, $2, Some $3 }
;

comp_iter:
| comp_for { Cfor $1 }
| comp_if  { Cif $1 }
;

comp_for:
| FOR exprlist IN or_test           { get_loc $startofs $endofs, $2, $4, None }
| FOR exprlist IN or_test comp_iter { get_loc $startofs $endofs, $2, $4, Some $5 }
;

comp_if:
| IF old_test           { get_loc $startofs $endofs, $2, None }
| IF old_test comp_iter { get_loc $startofs $endofs, $2, Some $3 }
;

testlist1:
|                 test { [$1] }
| testlist1 COMMA test { $1 @ [$3] }
;

yield_expr:
| YIELD          { mktestlist [] false true }
| YIELD testlist { $2.yield<-true; $2 }
;

%%
