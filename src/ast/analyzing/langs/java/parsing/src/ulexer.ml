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
 * A lexer (utf-8) for the Java Language (JLS 3rd.)
 *
 * ulexer.ml
 *
 *)

module PB = Parserlib_base

open Tokens_
open Compat

exception EOF_reached


let mklexpos i =
    { Lexing.pos_fname = "";
      Lexing.pos_lnum  = 0;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = i
    }

let mktok rawtok ulexbuf =
  let st_pos = mklexpos (Ulexing.lexeme_start ulexbuf) in
  let ed_pos = mklexpos ((Ulexing.lexeme_end ulexbuf) - 1) in
  rawtok, st_pos, ed_pos

let dollar_pat = Str.regexp_string "$"
let escape_dollar = Str.global_replace dollar_pat "&#36;"

let find_keyword, delete_keyword =
  let keyword_list =
    [
     "abstract",     (fun l -> ABSTRACT l);
     "assert",       (fun l -> ASSERT l);
     "boolean",      (fun l -> BOOLEAN l);
     "break",        (fun l -> BREAK l);
     "byte",         (fun l -> BYTE l);
     "case",         (fun l -> CASE l);
     "catch",        (fun l -> CATCH l);
     "char",         (fun l -> CHAR l);
     "class",        (fun l -> CLASS l);
     "const",        (fun l -> CONST l);
     "continue",     (fun l -> CONTINUE l);
     "default",      (fun l -> DEFAULT l);
     "do",           (fun l -> DO l);
     "double",       (fun l -> DOUBLE l);
     "else",         (fun l -> ELSE l);
     "enum",         (fun l -> ENUM l);
     "extends",      (fun l -> EXTENDS l);
     "final",        (fun l -> FINAL l);
     "finally",      (fun l -> FINALLY l);
     "float",        (fun l -> FLOAT l);
     "for",          (fun l -> FOR l);
     "goto",         (fun l -> GOTO l);
     "if",           (fun l -> IF l);
     "implements",   (fun l -> IMPLEMENTS l);
     "import",       (fun l -> IMPORT l);
     "instanceof",   (fun l -> INSTANCEOF l);
     "int",          (fun l -> INT l);
     "interface",    (fun l -> INTERFACE l);
     "long",         (fun l -> LONG l);
     "native",       (fun l -> NATIVE l);
     "new",          (fun l -> NEW l);
     "package",      (fun l -> PACKAGE l);
     "private",      (fun l -> PRIVATE l);
     "protected",    (fun l -> PROTECTED l);
     "public",       (fun l -> PUBLIC l);
     "return",       (fun l -> RETURN l);
     "short",        (fun l -> SHORT l);
     "static",       (fun l -> STATIC l);
     "strictfp",     (fun l -> STRICTFP l);
     "super",        (fun l -> SUPER l);
     "switch",       (fun l -> SWITCH l);
     "synchronized", (fun l -> SYNCHRONIZED l);
     "this",         (fun l -> THIS l);
     "throw",        (fun l -> THROW l);
     "throws",       (fun l -> THROWS l);
     "transient",    (fun l -> TRANSIENT l);
     "try",          (fun l -> TRY l);
     "void",         (fun l -> VOID l);
     "volatile",     (fun l -> VOLATILE l);
     "while",        (fun l -> WHILE l);

     "exports", (fun l -> EXPORTS l);(* 9 *)
     "module", (fun l -> MODULE l);(* 9 *)
     "open", (fun l -> OPEN l);(* 9 *)
     "opens", (fun l -> OPENS l);(* 9 *)
     "provides", (fun l -> PROVIDES l);(* 9 *)
     "requires", (fun l -> REQUIRES l);(* 9 *)
     "to", (fun l -> TO l);(* 9 *)
     "transitive", (fun l -> TRANSITIVE l);(* 9 *)
     "uses", (fun l -> USES l);(* 9 *)
     "with", (fun l -> WITH_ l);(* 9 *)

     (* "var", (fun l -> VAR l); *)(* 10 *)

     "yield", (fun l -> YIELD l);(* 14 *)

     "record", (fun l -> RECORD l);(* 16 *)

     "sealed", (fun l -> SEALED l);(* 17 *)
     "non-sealed", (fun l -> NON_SEALED l);(* 17 *)
     "permits", (fun l -> PERMITS l);(* 17 *)

     "aspect",       (fun l -> ASPECT l);
     "pointcut",     (fun l -> POINTCUT l);
     "within",       (fun l -> WITHIN l);
     "declare",      (fun l -> DECLARE l);
   ] in
  let keyword_table = Hashtbl.create (List.length keyword_list) in
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      keyword_list
  in
  let find loc s =
    try
      (Hashtbl.find keyword_table s) loc
    with
      Not_found -> IDENTIFIER(loc, escape_dollar s)
  in
  let delete s =
    DEBUG_MSG "deleting keyword \"%s\"" s;
    Hashtbl.remove keyword_table s
  in
  find, delete


module F (Stat : Parser_aux.STATE_T) = struct

  module Loc = Ast.Loc
  module Aux = Parser_aux.F (Stat)

  open Stat


  let offsets_to_loc st ed = env#current_pos_mgr#offsets_to_loc st ed

  let lexing_error lexbuf msg =
    let loc = offsets_to_loc (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf) in
    Common.fail_to_parse ~head:(Loc.to_string ~prefix:"[" ~suffix:"]" loc) msg

  let regexp hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
  let regexp unicode_escape = '\\' 'u'+ hex_digit hex_digit hex_digit hex_digit

  let regexp line_terminator = ['\013' '\010'] | "\013\010"

  let regexp input_character = unicode_escape | [^'\013' '\010']

  let regexp white_space = [' ' '\009' '\012']

  let regexp not_star_not_slash = [^'*' '/'] | unicode_escape | "\013\010"
  let regexp not_star = [^'*'] | unicode_escape | "\013\010"

 (* 880-1023:Greek 2304-2431:Devanagari 4352-4607,43360-43391,44032-55215,55216-55295:Hangul *)
  let regexp java_letter = ['A'-'Z' 'a'-'z' '_' '$' 880-1023 1024-1279 2304-2431 4352-4607 43360-43391 44032-55215 55216-55295]
  let regexp java_letter_or_digit = java_letter | ['0'-'9']
  let regexp identifier_chars = java_letter java_letter_or_digit*
  let regexp identifier_or_keyword = identifier_chars

  let regexp underscores = '_'+
  let regexp non_zero_digit = ['1'-'9']
  let regexp digit = '0' | non_zero_digit
  let regexp digits = digit | digit (digit|'_')* digit
  let regexp decimal_numeral = '0' | non_zero_digit digits? | non_zero_digit underscores digits

  let regexp hex_digits = hex_digit | hex_digit (hex_digit|'_')* hex_digit
  let regexp hex_numeral = ("0x"|"0X") hex_digits

  let regexp octal_digit = ['0'-'7']
  let regexp octal_digits = octal_digit | octal_digit (octal_digit|'_')* octal_digit
  let regexp octal_numeral = '0' underscores? octal_digits

  let regexp binary_digit = ['0' '1']
  let regexp binary_digits = binary_digit | binary_digit (binary_digit|'_')* binary_digit
  let regexp binary_numeral = ("0b"|"0B") binary_digits

  let regexp integer_type_suffix = ['l' 'L']

  let regexp decimal_integer_literal = decimal_numeral integer_type_suffix?
  let regexp hex_integer_literal = hex_numeral integer_type_suffix?
  let regexp octal_integer_literal = octal_numeral integer_type_suffix?
  let regexp binary_integer_literal = binary_numeral integer_type_suffix?

  let regexp integer_literal =
    decimal_integer_literal | hex_integer_literal | octal_integer_literal | binary_integer_literal

  let regexp float_type_suffix = ['f' 'F' 'd' 'D']
  let regexp signed_integer = ['+' '-']? digits
  let regexp exponent_part = ['e' 'E'] signed_integer

  let regexp decimal_floating_point_literal =
    digits '.' digits? exponent_part? float_type_suffix?
  | '.' digits exponent_part? float_type_suffix?
  | digits exponent_part
  | digits float_type_suffix
  | digits exponent_part float_type_suffix

  let regexp hex_significand =
    hex_numeral '.'?
  | ("0x"|"0X") hex_digits? '.' hex_digits

  let regexp binary_exponent = ['p' 'P'] signed_integer

  let regexp hexadecimal_floating_point_literal =
    hex_significand binary_exponent float_type_suffix?

  let regexp floating_point_literal =
    decimal_floating_point_literal | hexadecimal_floating_point_literal

  let regexp boolean_literal = "true" | "false"

  let regexp octal_escape =
    '\\' (['0'-'7'] | ['0'-'7'] ['0'-'7'] | ['0'-'3'] ['0'-'7'] ['0'-'7'])
  let regexp escape_sequence =
    ('\\' ['\'' '"' '\\' 'b' 'f' 'n' 'r' 't']) | octal_escape

  let regexp single_character = unicode_escape | [^'\013' '\010' '\'' '\\']
  let regexp character_literal =
    ('\'' single_character '\'') | ('\'' escape_sequence '\'')

  let regexp string_character =
    unicode_escape | [^'\013' '\010' '"' '\\'] | escape_sequence
  let regexp string_literal = '"' string_character* '"'

  let regexp text_block_quote = "\"\"\""
  let regexp text_block_item = [^'\\']

  let regexp null_literal = "null"

  let regexp literal = integer_literal | floating_point_literal | boolean_literal
  | character_literal | string_literal | null_literal


  let rec token = lexer
  |   white_space -> token lexbuf
  |   line_terminator -> token lexbuf

  |   "//" ->
      line_comment (Ulexing.lexeme_start lexbuf) lexbuf;
      token lexbuf

  |   "/*" not_star ->
      traditional_comment (Ulexing.lexeme_start lexbuf) lexbuf;
      token lexbuf

  |   "/**/" ->
      let st, ed = Ulexing.lexeme_start lexbuf, Ulexing.lexeme_end lexbuf in
      env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ed);
      token lexbuf

  |   "/**" ->
      document_comment (Ulexing.lexeme_start lexbuf) lexbuf;
      token lexbuf

  |   text_block_quote -> text_block (Ulexing.utf8_lexeme lexbuf) lexbuf

  |   "true"  -> mktok TRUE lexbuf
  |   "false" -> mktok FALSE lexbuf
  |   integer_literal        -> mktok (INTEGER_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   floating_point_literal -> mktok (FLOATING_POINT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   character_literal      -> mktok (CHARACTER_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   string_literal         -> mktok (STRING_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   null_literal           -> mktok NULL lexbuf

  |   ">>>>>>>" -> mktok GT_7 lexbuf
  |   "=======" -> marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf
  |   "|||||||" -> marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf
  |   "<<<<<<<" -> marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf

  |   "==" -> mktok EQ_EQ lexbuf
  |   "<=" -> mktok LT_EQ lexbuf
  |   ">=" -> mktok GT_EQ lexbuf
  |   "!=" -> mktok EXCLAM_EQ lexbuf
  |   "&&" -> mktok AND_AND lexbuf
  |   "||" -> mktok OR_OR lexbuf
  |   "++" -> mktok PLUS_PLUS lexbuf
  |   "--" -> mktok MINUS_MINUS lexbuf
  |   "-=" -> mktok MINUS_EQ lexbuf
  |   "->" -> mktok MINUS_GT lexbuf
  |   "<<" -> mktok LT_LT lexbuf
  |   ">>" -> mktok GT_GT lexbuf
  |   ">>>" -> mktok GT_GT_GT lexbuf
  |   "+=" -> mktok PLUS_EQ lexbuf
  |   "*=" -> mktok STAR_EQ lexbuf
  |   "/=" -> mktok SLASH_EQ lexbuf
  |   "&=" -> mktok AND_EQ lexbuf
  |   "|=" -> mktok OR_EQ lexbuf
  |   "^=" -> mktok HAT_EQ lexbuf
  |   "%=" -> mktok PERCENT_EQ lexbuf
  |   "<<=" -> mktok LT_LT_EQ lexbuf
  |   ">>=" -> mktok GT_GT_EQ lexbuf
  |   ">>>=" -> mktok GT_GT_GT_EQ lexbuf
  |   "..." -> mktok ELLIPSIS lexbuf
  |   "::" -> mktok COLON_COLON lexbuf
  |   ".." -> mktok DOT_DOT lexbuf

  |   '(' -> mktok (LPAREN(Aux.get_loc_for_lex lexbuf)) lexbuf
  |   ')' -> mktok (RPAREN(Aux.get_loc_for_lex lexbuf)) lexbuf
  |   '{' -> env#open_lex_brace; mktok LBRACE lexbuf
  |   '}' -> env#close_lex_brace; mktok RBRACE lexbuf
  |   '[' -> mktok LBRACKET lexbuf
  |   ']' -> mktok RBRACKET lexbuf
  |   ';' -> mktok SEMICOLON lexbuf
  |   ',' -> mktok COMMA lexbuf
  |   '.' -> mktok DOT lexbuf

  |   '@' -> mktok (AT(Aux.get_loc_for_lex lexbuf)) lexbuf

  |   '=' -> mktok EQ lexbuf
  |   '>' -> mktok GT lexbuf
  |   '<' -> mktok (LT(Aux.get_loc_for_lex lexbuf)) lexbuf
  |   '!' -> mktok EXCLAM lexbuf
  |   '~' -> mktok TILDE lexbuf
  |   '?' -> mktok QUESTION lexbuf
  |   ':' -> mktok COLON lexbuf
  |   '+' -> mktok PLUS lexbuf
  |   '-' -> mktok MINUS lexbuf
  |   '*' -> mktok STAR lexbuf
  |   '/' -> mktok SLASH lexbuf
  |   '&' -> mktok AND lexbuf
  |   '|' -> mktok OR lexbuf
  |   '^' -> mktok HAT lexbuf
  |   '%' -> mktok PERCENT lexbuf

  |   identifier_or_keyword ->
      mktok (find_keyword (Aux.get_loc_for_lex lexbuf) (Ulexing.utf8_lexeme lexbuf)) lexbuf

  |   eof ->
      if not env#current_source#eof_reached then begin
	env#current_source#set_eof_reached;
	mktok EOF lexbuf
      end
      else
	raise EOF_reached

  |   _ ->
      let s = Ulexing.utf8_lexeme lexbuf in
      if PB.is_bom s then begin
        let loc = offsets_to_loc (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf) in
        Common.warning_loc loc "BOM (0x%s:%s) found" (Xhash.to_hex s) (PB.get_bom_name s);
        token lexbuf
      end
      else
        let mes = Printf.sprintf "invalid symbol: %s(%s)" s
            (Seq.fold_left
               (fun h c ->
                 h^(Printf.sprintf "%02x" (Char.code c))
               ) "0x" (String.to_seq s)
            )
        in
        if env#keep_going_flag then begin
          let loc = offsets_to_loc (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf) in
          Common.warning_loc loc "%s" mes;
          mktok (ERROR mes) lexbuf
        end
        else
          lexing_error lexbuf mes

	

  and text_block s = lexer
  |   text_block_quote -> mktok (TEXT_BLOCK (s^(Ulexing.utf8_lexeme lexbuf))) lexbuf
  |   _ -> text_block (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf
(*  |   text_block_item -> text_block (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   _ ->
      let s0 = Ulexing.utf8_lexeme lexbuf in
      let mes = Printf.sprintf "invalid symbol: %s(%s)" s0
          (Seq.fold_left
             (fun h c ->
               h^(Printf.sprintf "%02x" (Char.code c))
             ) "0x" (String.to_seq s0)
          )
      in
      if env#keep_going_flag then begin
        let loc = offsets_to_loc (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf) in
        Common.warning_loc loc "%s" mes;
        (*mktok (ERROR mes) lexbuf*)
        text_block s lexbuf
      end
      else
        lexing_error lexbuf mes*)

  and traditional_comment st = lexer
  |   "*/" -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st (Ulexing.lexeme_end lexbuf))
  |   _ -> traditional_comment st lexbuf

  and line_comment st = lexer
  |   line_terminator -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1))
  |   _ -> line_comment st lexbuf

  and document_comment st = lexer
  |   "*/" -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st (Ulexing.lexeme_end lexbuf))
  |   _ -> document_comment st lexbuf

  and marker st s = lexer
  |   line_terminator -> (MARKER s), mklexpos st, mklexpos ((Ulexing.lexeme_end lexbuf) - 1)
  |   _ -> marker st (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf


  let set_to_JLS lv loc kw =
    match env#actual_java_lang_spec with
    | Common.JLSnone -> begin
	Common.warning_loc loc "'%s' occurred as an identifier (JLS%d or before)" kw lv;
	env#set_actual_java_lang_spec lv
    end
    | Common.JLS x when lv > x -> begin
	Common.warning_loc loc "'%s' occurred as an identifier (JLS%d or before)" kw lv;
	env#set_actual_java_lang_spec lv
    end
    | _ -> ()


  module P = Parser.Make (Stat)

  let assert_stmt_parser = PB.mkparser P.partial_assert_statement
  let block_stmt_parser  = PB.mkparser P.partial_block_statement

  let string_of_token_queue = Common.token_queue_to_string Token.to_orig
  (*let string_of_token_queue (q : 'a Xqueue.c) =
    let l = q#fold (fun l x -> x::l) [] in
    Xlist.to_string Token.to_orig " " (List.rev l)*)

  let kw_to_ident name t =
    let tok, st, ed = Token.decompose t in
    match tok with
    | ENUM loc
    | ASSERT loc

    | EXPORTS loc | MODULE loc | NON_SEALED loc | OPEN loc | OPENS loc | PERMITS loc
    | PROVIDES loc | RECORD loc | REQUIRES loc | SEALED loc | TO loc | TRANSITIVE loc
    | USES loc | VAR loc | WITH_ loc | YIELD loc

    | ASPECT loc | POINTCUT loc | WITHIN loc | DECLARE loc
      -> Token.create (IDENTIFIER(loc, name)) st ed
    | _ -> t



  exception Modified_token of Token.t

  let peek_nth queue ulexbuf nth =
    let t_opt = ref None in
    let count = ref 0 in
    begin
      try
        queue#iter
          (fun t ->
            if !count = nth then
              raise Exit;

            t_opt := Some t;
            incr count
          );

        for i = 1 to  nth - !count do
          let t = token ulexbuf in
          queue#add t;
          t_opt := Some t
        done
      with
        Exit -> ()
    end;
    match !t_opt with
    | Some t ->
        let tok = Token.to_rawtoken t in
        DEBUG_MSG "nth=%d tok=%s" nth (Token.rawtoken_to_string tok);
        t, tok
    | None -> assert false


  let get_token queue ulexbuf =

    let take() =
      if queue#is_empty then
        token ulexbuf
      else
        queue#take
    in

    let peek_nth = peek_nth queue ulexbuf in

    let discard() =
      let _, _, ed = Token.decompose (take()) in
      ed
    in

    let check_contextual_keywords ?(at_stmt=false) t =
      let tok, st, ed = Token.decompose t in
      match tok with
      | TRANSITIVE loc -> begin
          let conv () =
            DEBUG_MSG "TRANSITIVE --> <identifier>";
            let kw = "transitive" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | (STATIC _ | IDENTIFIER _), (IDENTIFIER _ | DOT | SEMICOLON) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | SEALED loc -> begin
          let conv () =
            DEBUG_MSG "SEALED --> <identifier>";
            let kw = "sealed" in
            (*set_to_JLS 16 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          match tok2 with
          | AT _
          | PUBLIC _ | PROTECTED _ | PRIVATE _ | ABSTRACT _ | STATIC _ | FINAL _ | STRICTFP _
          | NON_SEALED _
          | CLASS _ | INTERFACE _ -> t
          | _ -> conv()
      end
      | NON_SEALED loc -> begin
          let conv () =
            DEBUG_MSG "NON_SEALED --> <identifier>";
            let kw = "non-sealed" in
            (*set_to_JLS 16 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          match tok2 with
          | AT _
          | PUBLIC _ | PROTECTED _ | PRIVATE _ | ABSTRACT _ | STATIC _ | FINAL _ | STRICTFP _
          | SEALED _
          | CLASS _ | INTERFACE _ -> t
          | _ -> conv()
      end
      | YIELD loc -> begin
          let conv () =
            DEBUG_MSG "YIELD --> <identifier>";
            let kw = "yield" in
            (*set_to_JLS 13 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          match tok2 with
          | IDENTIFIER _ | EXCLAM | TILDE | NEW _ | SWITCH _
          | TRUE | FALSE | NULL
          | INTEGER_LITERAL _ | FLOATING_POINT_LITERAL _ | CHARACTER_LITERAL _ | STRING_LITERAL _ -> t
          | LPAREN _ | PLUS | MINUS | PLUS_PLUS | MINUS_MINUS when env#stmt_head_flag -> t
          | _ -> conv()
      end
      | PERMITS loc -> begin
          let conv () =
            DEBUG_MSG "PERMITS --> <identifier>";
            let kw = "permits" in
            (*set_to_JLS 16 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | LBRACE) -> t
          | _ -> conv()
      end
      | TO loc -> begin
          let conv () =
            DEBUG_MSG "TO --> <identifier>";
            let kw = "to" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | COMMA | SEMICOLON) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | WITH_ loc -> begin
          let conv () =
            DEBUG_MSG "WITH_ --> <identifier>";
            let kw = "with" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | COMMA | SEMICOLON) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | OPEN loc -> begin
          let conv () =
            DEBUG_MSG "OPEN --> <identifier>";
            let kw = "open" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | MODULE _, IDENTIFIER _ -> t
          | _ -> conv()
      end
      | MODULE loc -> begin
          let conv () =
            DEBUG_MSG "MODULE --> <identifier>";
            let kw = "module" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | LBRACE) -> t
          | _ -> conv()
      end
      | REQUIRES loc -> begin
          let conv () =
            DEBUG_MSG "REQUIRES --> <identifier>";
            let kw = "module" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | SEMICOLON) -> t
          | (STATIC _ | TRANSITIVE _), IDENTIFIER _ -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | EXPORTS loc -> begin
          let conv () =
            DEBUG_MSG "EXPORTS --> <identifier>";
            let kw = "exports" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | TO _) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | OPENS loc -> begin
          let conv () =
            DEBUG_MSG "OPENS --> <identifier>";
            let kw = "opens" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | TO _) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | USES loc -> begin
          let conv () =
            DEBUG_MSG "USES --> <identifier>";
            let kw = "uses" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | SEMICOLON) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | PROVIDES loc -> begin
          let conv () =
            DEBUG_MSG "PROVIDES --> <identifier>";
            let kw = "provides" in
            (*set_to_JLS 8 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          if at_stmt then
            conv()
          else
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (DOT | WITH_ _) -> t
          | _ when env#in_module -> t
          | _ -> conv()
      end
      | RECORD loc -> begin
          let conv () =
            DEBUG_MSG "RECORD --> <identifier>";
            let kw = "record" in
            (*set_to_JLS 15 loc kw;*)
            (*delete_keyword kw;*)
            kw_to_ident kw t
          in
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (LT _ | LPAREN _) -> t
          | _ -> conv()
      end
      | _ -> t
    in

    let mkscanner q =
      if q#is_empty then begin
        fun () -> Token.create EOP Loc.dummy_lexpos Loc.dummy_lexpos
      end
      else begin
        let last = ref q#peek in
        fun () ->
          try
            let t = q#take in
            let t = check_contextual_keywords t in
            DEBUG_MSG ">>> %s" (Token.to_string env#current_pos_mgr t);
            last := t;
            t
          with
            Queue.Empty ->
              let _, _, ed = Token.decompose !last in
              Token.create EOP ed ed
      end
    in

    let res =
      let t = take() in
      let tok, st, ed = Token.decompose t in
      match tok with
      | ENUM loc -> begin
          let _, tok2 = peek_nth 1 in
          let _, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (IMPLEMENTS _ | LBRACE) -> t
          | _ -> begin
              DEBUG_MSG "ENUM --> <identifier>";
              let kw = "enum" in
              set_to_JLS 2 loc kw;
              delete_keyword kw;
              kw_to_ident kw t
          end
      end
      | DEFAULT loc -> begin
          let _, tok2 = peek_nth 1 in
          match tok2 with
          | COLON -> begin
              DEBUG_MSG "DEFAULT --> DEFAULT__COLON";
              let st, ed = Token.to_lexposs t in
              Token.create (DEFAULT__COLON loc) st ed
          end
          | _ -> t
      end
      | MINUS_GT when env#case_flag -> begin
          DEBUG_MSG "MINUS_GT --> MINUS_GT__CASE";
          let st, ed = Token.to_lexposs t in
          Token.create MINUS_GT__CASE st ed
      end
      | AT loc -> begin
          let _, tok2 = peek_nth 1 in
          match tok2 with
          | INTERFACE _ -> begin
              DEBUG_MSG "AT --> AT__INTERFACE";
              Token.create (AT__INTERFACE loc) st ed
          end
          | _ -> t
      end
      | LPAREN loc when begin
          match Obj.obj env#last_rawtoken with
          | GT | IDENTIFIER _ -> false
          | _ -> true
      end -> begin
          DEBUG_MSG "LPAREN";
          let nth = ref 1 in
          let lv = ref 1 in
          try
            while true do
              let _, tok' = peek_nth !nth in
              DEBUG_MSG "tok' = %s" (Token.rawtoken_to_string tok');
              begin
                match tok' with
                | EOF | SEMICOLON -> raise Exit
                | LPAREN _ -> incr lv
                | RPAREN _ -> begin
                    decr lv;
                    DEBUG_MSG "lv=%d" !lv;
                    if !lv = 0 then begin
                      let _, tok'' = peek_nth (!nth+1) in
                      DEBUG_MSG "tok'' = %s" (Token.rawtoken_to_string tok'');
                      begin
                        match tok'' with
                        | MINUS_GT ->
                            DEBUG_MSG "'(' --> '(':lambda";
                            let m = Token.create (LPAREN__LAMBDA loc) st ed in
                            raise (Modified_token m)
                        | _ -> ()
                      end;
                      raise Exit
                    end
                end
                | _ -> ()
              end;
              incr nth
            done;
            assert false
          with
          | Exit -> t
          | Modified_token m -> m
      end
      | SEMICOLON when env#keep_going_flag -> begin
          match Obj.obj env#last_rawtoken with
          | SEMICOLON -> begin
              let t', tok' = peek_nth 1 in
              DEBUG_MSG "tok' = %s" (Token.rawtoken_to_string tok');
              match tok' with
              | IMPORT _ -> begin
                  let _ = discard() in
                  t'
              end
              | _ -> t
          end
          | _ -> t
      end
      | IDENTIFIER(loc, s) when env#keep_going_flag -> begin
          match Obj.obj env#last_rawtoken with
          | LBRACE | SEMICOLON -> begin
              let _, tok' = peek_nth 1 in
              DEBUG_MSG "tok' = %s" (Token.rawtoken_to_string tok');
              match tok' with
              | IMPORT _ -> begin
                  DEBUG_MSG "IDENTIFIER --> ERROR";
                  Common.warning_loc loc "'%s': invalid import" s;
                  Token.create (ERROR s) st ed
              end
              | PUBLIC _ | PROTECTED _ | PRIVATE _ | STATIC _ | FINAL _ -> begin
                  DEBUG_MSG "IDENTIFIER --> ERROR_MOD";
                  Common.warning_loc loc "'%s': invalid modifier" s;
                  Token.create (ERROR_MOD s) st ed
              end
              | FOR _ | WHILE _ | DO _ | IF _ | SWITCH _ | RETURN _ -> begin
                  DEBUG_MSG "IDENTIFIER --> ERROR_STMT";
                  Common.warning_loc loc "'%s': invalid statement" s;
                  Token.create (ERROR_STMT s) st ed
              end
              | DOT when begin
                  let _, tok'' = peek_nth 2 in
                  DEBUG_MSG "tok'' = %s" (Token.rawtoken_to_string tok'');
                  match tok'' with
                  | RBRACE -> true
                  | _ -> false
              end -> begin
                DEBUG_MSG "IDENTIFIER DOT --> ERROR";
                Common.warning_loc loc "'%s.': syntax error" s;
                let ed_ = discard() in
                Token.create (ERROR (s^".")) st ed_
              end
              | IDENTIFIER(loc', s') -> begin
                  let _, tok'' = peek_nth 2 in
                  DEBUG_MSG "tok'' = %s" (Token.rawtoken_to_string tok'');
                  match tok'' with
                  | IDENTIFIER(loc'', s'') -> begin
                      let buf = Buffer.create 0 in
                      Buffer.add_string buf s;
                      Buffer.add_string buf " ";
                      Buffer.add_string buf s';
                      Buffer.add_string buf " ";
                      Buffer.add_string buf s'';
                      let nth = ref 3 in
                      begin
                        try
                          while true do
                            let _, nth_tok = peek_nth !nth in
                            DEBUG_MSG "%dth tok = %s" !nth (Token.rawtoken_to_string nth_tok);
                            match nth_tok with
                            | IDENTIFIER(_, nth_s) -> begin
                                incr nth;
                                Buffer.add_string buf " ";
                                Buffer.add_string buf nth_s
                            end
                            | EXCLAM -> begin
                                incr nth;
                                Buffer.add_string buf "!"
                            end
                            | _ -> raise Exit
                          done
                        with
                          Exit -> ()
                      end;
                      let ed_ = ref ed in
                      for i = 1 to !nth do
                        ed_ := discard()
                      done;
                      let err = Buffer.contents buf in
                      DEBUG_MSG "IDENTIFIER --> ERROR";
                      Common.warning_loc loc "'%s': syntax error" err;
                      Token.create (ERROR err) st !ed_
                  end
                  | _ -> t
              end
              | _ -> t
          end
          | _ -> t
      end
      | WITHIN _ when not env#in_declare_flag && not env#in_pointcut_flag -> begin
          DEBUG_MSG "WITHIN --> <identifier>";
          kw_to_ident "within" t
      end
      | DECLARE _ when not env#in_aspect_flag -> begin
          DEBUG_MSG "DECLARE --> <identifier>";
          kw_to_ident "declare" t
      end
      | POINTCUT _ when not env#in_aspect_flag -> begin
          DEBUG_MSG "POINTCUT --> <identifier>";
          kw_to_ident "pointcut" t
      end
      | ASPECT _ when begin
          match Obj.obj env#last_rawtoken with
          | PUBLIC _ | PROTECTED _ | PRIVATE _
          | STATIC _ | ABSTRACT _
          | FINAL _ | NATIVE _ | SYNCHRONIZED _ | TRANSIENT _ | VOLATILE _
          | STRICTFP _ | DEFAULT _ -> false
          | LBRACE | SEMICOLON -> begin
              let _, tok2 = peek_nth 1 in
              let _, tok3 = peek_nth 2 in
              match tok2, tok3 with
              | IDENTIFIER _, (EXTENDS _ | IMPLEMENTS _ | LBRACE) -> false
              | _ -> true
          end
          | _ -> true
      end -> begin
          DEBUG_MSG "ASPECT --> <identifier>";
          kw_to_ident "aspect" t
      end
      | _ -> check_contextual_keywords t
    in (* res *)

    let tok, st, ed = Token.decompose res in
    let res' =
      match tok with
      | ASSERT loc -> begin
          match Obj.obj env#last_rawtoken with
          | COLON | RPAREN _ | ELSE _ | DO _ | SEMICOLON | LBRACE | RBRACE | STMT _ -> begin
              let q0 = new Xqueue.c in
              let last = ref res in
              let take() =
                let t =
                  try
                    queue#take
                  with
                    Queue.Empty -> token ulexbuf
                in
                check_contextual_keywords ~at_stmt:true t
              in
              begin
                let blv = ref 0 in
                try
                  while true do
                    let t = take() in
                    last := t;
                    q0#add t;
                    match Token.to_rawtoken t with
                    | SEMICOLON when !blv = 0 -> raise Exit
                    | LBRACE -> incr blv
                    | RBRACE -> decr blv
                    | _ -> ()
                  done
                with
                  Exit -> ()
              end;
              let _, _, ed' = Token.decompose !last in
              begin
                let q = new Xqueue.c in
                q#add res;
                q0#iter q#add;
                let orig_line = string_of_token_queue q in
                DEBUG_MSG "token queue: [%s]" orig_line;
                let scanner = mkscanner q in
                DEBUG_MSG "parsing with assert-stmt parser...";
                try
                  let stmt = assert_stmt_parser scanner in
                  DEBUG_MSG "SUCCESSFULLY PARSED!";
                  Token.create (STMT stmt) st ed'
                with
                  exn -> begin
                    DEBUG_MSG "FAILED TO PARSE! (%s)" (Printexc.to_string exn);
                    DEBUG_MSG "assuming that 'assert' is an identifier";
                    let q = new Xqueue.c in
                    q#add (kw_to_ident "assert" res);
                    q0#iter
                      (fun x ->
                        let x' =
                          match Token.to_rawtoken x with
                          | ASSERT loc' -> kw_to_ident "assert" x
                          | _ -> x
                        in
                        q#add x'
                      );
                    DEBUG_MSG "token queue: [%s]" (string_of_token_queue q);
                    let scanner = mkscanner q in
                    DEBUG_MSG "parsing with block-stmt parser...";
                    try
                      let stmt = block_stmt_parser scanner in
                      DEBUG_MSG "SUCCESSFULLY PARSED!";
                      let tok' =
                        match stmt.Ast.bs_desc with
                        | Ast.BSstatement s -> STMT s
                        | _ -> BLOCK_STMT stmt
                      in
                      Token.create tok' st ed'
                    with
                      _ -> Token.create (ERROR_STMT orig_line) st ed'
                  end
              end
          end
          | _ -> begin
              DEBUG_MSG "ASSERT --> <identifier>";
              set_to_JLS 2 loc "assert";
              kw_to_ident "assert" res
          end
      end
      | _ -> res
    in
    env#set_last_rawtoken (Obj.repr (Token.to_rawtoken res'));
    env#clear_stmt_head_flag;
    res'


end (* of functor Ulexer.F *)
