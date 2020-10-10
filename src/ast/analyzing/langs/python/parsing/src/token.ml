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
(* token.ml *)

open Printf

open Tokens_


let get_rawtoken (rt, _, _) = rt

let rawtoken_to_string = function
  | AND      -> "AND"
  | AS       -> "AS"
  | ASSERT   -> "ASSERT"
  | BREAK    -> "BREAK"
  | CLASS    -> "CLASS"
  | CONTINUE -> "CONTINUE"
  | DEF      -> "DEF"
  | ELIF     -> "ELIF"
  | ELSE     -> "ELSE"
  | EXCEPT   -> "EXCEPT"
  | EXEC     -> "EXEC"
  | FINALLY  -> "FINALLY"
  | FOR      -> "FOR"
  | FROM     -> "FROM"
  | GLOBAL   -> "GLOBAL"
  | IF       -> "IF"
  | IMPORT   -> "IMPORT"
  | IN       -> "IN"
  | IS       -> "IS"
  | LAMBDA   -> "LAMBDA"
  | NOT      -> "NOT"
  | OR       -> "OR"
  | PASS     -> "PASS"
  | PRINT    -> "PRINT"
  | RAISE    -> "RAISE"
  | RETURN   -> "RETURN"
  | TRY      -> "TRY"
  | WHILE    -> "WHILE"
  | WITHx    -> "WITH"
  | YIELD    -> "YIELD"

  | NAMEx s -> sprintf "NAME(%s)" s

  | INDENT    -> "INDENT"
  | DEDENT    -> "DEDENT"
  | NEWLINE n -> sprintf "NEWLINE(%d)" n

  | INTEGER s     -> sprintf "INTEGER(%s)" s
  | LONGINTEGER s -> sprintf "LONGINTEGER(%s)" s
  | FLOATNUMBER s -> sprintf "FLOATNUMBER(%s)" s
  | IMAGNUMBER s  -> sprintf "IMAGNUMBER(%s)" s
  | SHORTSTRING s -> sprintf "SHORTSTRING(%s)" s

  | LONGSTRING_BEGIN_S s -> sprintf "LONGSTRING_BEGIN_S(%s)" s
  | LONGSTRING_BEGIN_D s -> sprintf "LONGSTRING_BEGIN_D(%s)" s
  | LONGSTRING_REST s    -> sprintf "LONGSTRING_REST(%s)" s

  | EQ_EQ     -> "EQ_EQ"
  | LT_EQ     -> "LT_EQ" 
  | GT_EQ     -> "GT_EQ" 
  | EXCLAM_EQ -> "EXCLAM_EQ" 
  | LT_GT     -> "LT_GT" 

  | STAR_STAR   -> "STAR_STAR" 
  | SLASH_SLASH -> "SLASH_SLASH" 
  | LT_LT       -> "LT_LT" 
  | GT_GT       -> "GT_GT" 

  | PLUS_EQ        -> "PLUS_EQ" 
  | MINUS_EQ       -> "MINUS_EQ" 
  | STAR_EQ        -> "STAR_EQ" 
  | SLASH_EQ       -> "SLASH_EQ" 
  | SLASH_SLASH_EQ -> "SLASH_SLASH_EQ" 
  | PERCENT_EQ     -> "PERCENT_EQ" 
  | AMP_EQ         -> "AMP_EQ" 
  | PIPE_EQ        -> "PIPE_EQ" 
  | HAT_EQ         -> "HAT_EQ" 
  | GT_GT_EQ       -> "GT_GT_EQ" 
  | LT_LT_EQ       -> "LT_LT_EQ" 
  | STAR_STAR_EQ   -> "STAR_STAR_EQ" 

  | PLUS    -> "PLUS" 
  | MINUS   -> "MINUS" 
  | STAR    -> "STAR" 
  | SLASH   -> "SLASH" 
  | PERCENT -> "PERCENT" 
  | AMP     -> "AMP" 
  | PIPE    -> "PIPE" 
  | HAT     -> "HAT" 
  | TILDE   -> "TILDE" 
  | GT      -> "GT" 
  | LT      -> "LT" 

  | LPAREN    -> "LPAREN" 
  | RPAREN    -> "RPAREN" 
  | LBRACE    -> "LBRACE" 
  | RBRACE    -> "RBRACE" 
  | LBRACKET  -> "LBRACKET" 
  | RBRACKET  -> "RBRACKET"
  | AT        -> "AT" 
  | COMMA     -> "COMMA" 
  | COLON     -> "COLON" 
  | DOT       -> "DOT" 
  | BACKQUOTE -> "BACKQUOTE" 
  | EQ        -> "EQ" 
  | SEMICOLON -> "SEMICOLON" 

  | EOF -> "EOF"

  | _ -> "???"

let to_string (pos_mgr : Position.manager) (tok, st, ed) =
  let loc = pos_mgr#lexposs_to_loc st ed in
  sprintf "%s[%s]" (rawtoken_to_string tok) (Ast.Loc.to_string loc)
