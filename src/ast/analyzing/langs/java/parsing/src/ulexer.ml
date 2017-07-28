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
 * A lexer (utf-8) for the Java Language (JLS 3rd.)
 *
 * ulexer.ml
 *
 *)


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



let find_keyword =
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
      Not_found -> IDENTIFIER(loc, s)
  in
  find


module F (Stat : Parser_aux.STATE_T) = struct

  module Loc = Ast.Loc
  module Aux = Parser_aux.F (Stat)

  open Stat


  let offsets_to_loc st ed =
    env#current_pos_mgr#offsets_to_loc st ed

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


  let regexp java_letter = ['A'-'Z' 'a'-'z' '_' '$']
  let regexp java_letter_or_digit = java_letter | ['0'-'9']
  let regexp identifier_chars = java_letter java_letter_or_digit*
  let regexp identifier_or_keyword = identifier_chars

  let regexp integer_type_suffix = ['l' 'L']
  let regexp octal_numeral = '0' | ['0'-'7']+
  let regexp octal_integer_literal = octal_numeral integer_type_suffix?
  let regexp hex_numeral = ("0x"|"0X") hex_digit+
  let regexp hex_integer_literal = hex_numeral integer_type_suffix?
  let regexp digits = ['0'-'9']+
  let regexp decimal_numeral = '0' | ['1'-'9'] digits?
  let regexp decimal_integer_literal = decimal_numeral integer_type_suffix?
  let regexp integer_literal = 
    decimal_integer_literal | hex_integer_literal | octal_integer_literal

  let regexp float_type_suffix = ['F' 'f' 'D' 'd']
  let regexp signed_integer = ['+' '-']? digits
  let regexp exponent_part = ['e' 'E'] signed_integer

  let regexp decimal_floating_point_literal =
    (digits '.' digits? exponent_part? float_type_suffix?) 
  | ('.' digits exponent_part? float_type_suffix?) 
  | (digits exponent_part)
  | (digits float_type_suffix)
  | (digits exponent_part float_type_suffix)

  let regexp hex_significand =
    hex_numeral
  | hex_numeral '.'
  | ("0x"|"0X") hex_digit* '.' hex_digit+

  let regexp binary_exponent = ['P' 'p'] signed_integer

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

  |   "true"  -> mktok TRUE lexbuf
  |   "false" -> mktok FALSE lexbuf
  |   integer_literal        -> mktok (INTEGER_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   floating_point_literal -> mktok (FLOATING_POINT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   character_literal      -> mktok (CHARACTER_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   string_literal         -> mktok (STRING_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
  |   null_literal           -> mktok NULL lexbuf

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

  |   '(' -> mktok (LPAREN(Aux.get_loc_for_lex lexbuf)) lexbuf
  |   ')' -> mktok (RPAREN(Aux.get_loc_for_lex lexbuf)) lexbuf
  |   '{' -> mktok LBRACE lexbuf
  |   '}' -> mktok RBRACE lexbuf
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
      lexing_error lexbuf (Printf.sprintf "invalid symbol(%s)" (Ulexing.utf8_lexeme lexbuf))

	

  and traditional_comment st = lexer
  |   "*/" -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st (Ulexing.lexeme_end lexbuf))
  |   _ -> traditional_comment st lexbuf

  and line_comment st = lexer
  |   line_terminator -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1))
  |   _ -> line_comment st lexbuf

  and document_comment st = lexer
  |   "*/" -> env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st (Ulexing.lexeme_end lexbuf))
  |   _ -> document_comment st lexbuf


  let set_to_JLS2 loc kw =
    match env#java_lang_spec with
    | Common.JLSx -> 
	Common.warning_loc loc "'%s' occurred as an identifier, assuming JLS2..." kw;
	env#set_java_lang_spec_JLS2
    | Common.JLS3 -> 
	Aux.parse_error_loc loc (Printf.sprintf "'%s' identifier is not available in JLS3" kw)
    | Common.JLS2 -> ()


  module PB = Parserlib_base
  module P = Parser.Make (Stat)

  let assert_stmt_parser = PB.mkparser P.partial_assert_statement
  let block_stmt_parser  = PB.mkparser P.partial_block_statement

  let mkscanner q =
    if Queue.is_empty q then begin
      fun () -> Token.create EOP Loc.dummy_lexpos Loc.dummy_lexpos
    end
    else begin
      let last = ref (Queue.peek q) in
      fun () ->
        try
          let t = Queue.take q in
          last := t;
          t
        with
          Queue.Empty ->
            let _, _, ed = Token.decompose !last in
            Token.create EOP ed ed
    end

  let string_of_token_queue q =
    let l = Queue.fold (fun l x -> x::l) [] q in
    Xlist.to_string Token.to_orig " " (List.rev l)

  let kw_to_ident name t =
    let tok, st, ed = Token.decompose t in
    match tok with
    | ENUM loc 
    | ASSERT loc -> Token.create (IDENTIFIER(loc, name)) st ed
    | _ -> t
    


  exception Modified_token of Token.t

  let get_token queue ulexbuf =

    let take() =
      if Queue.is_empty queue then
        token ulexbuf
      else
        Queue.take queue
    in

    let peek_nth nth =
      let t_opt = ref None in
      let count = ref 0 in
      begin
        try
          Queue.iter 
            (fun t ->
              if !count = nth then
                raise Exit;

              t_opt := Some t;
              incr count
            ) queue;

          for i = 1 to  nth - !count do
            let t = token ulexbuf in
            Queue.add t queue;
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
    in

    let res =
      let t = take() in
      let tok, st, ed = Token.decompose t in
      match tok with
      | ENUM loc -> begin
          let t2, tok2 = peek_nth 1 in
          let t3, tok3 = peek_nth 2 in
          match tok2, tok3 with
          | IDENTIFIER _, (IMPLEMENTS _ | LBRACE) -> t
          | _ -> begin
              DEBUG_MSG "ENUM --> <identifier>";
              set_to_JLS2 loc "enum";
              kw_to_ident "enum" t
          end
      end
      | DEFAULT loc -> begin
          let t2, tok2 = peek_nth 1 in
          match tok2 with
          | COLON -> begin
              DEBUG_MSG "DEFAULT --> DEFAULT__COLON";
              let st, ed = Token.to_lexposs t in
              Token.create (DEFAULT__COLON loc) st ed
          end
          | _ -> t
      end
      | AT loc -> begin
          let t2, tok2 = peek_nth 1 in
          match tok2 with
          | INTERFACE _ -> begin
              DEBUG_MSG "AT --> AT__INTERFACE";
              Token.create (AT__INTERFACE loc) st ed
          end
          | _ -> t
      end
      | LPAREN loc -> begin
          DEBUG_MSG "LPAREN";
          let nth = ref 1 in
          let lv = ref 1 in
          try
            while true do
              let _, tok' = peek_nth !nth in
              DEBUG_MSG "tok' = %s" (Token.rawtoken_to_string tok');
              begin
                match tok' with
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
      | _ -> t
    in (* res *)
    let tok, st, ed = Token.decompose res in
    let res' =
      match tok with
      | ASSERT loc -> begin
          match Obj.obj env#last_rawtoken with
          | COLON | RPAREN _ | ELSE _ | DO _ | SEMICOLON | LBRACE | RBRACE | STMT _ -> begin
              let q0 = Queue.create() in
              let last = ref res in
              let take() =
                try
                  Queue.take queue
                with
                  Queue.Empty -> token ulexbuf
              in
              begin
                try
                  while true do
                    let t = take() in
                    last := t;
                    Queue.add t q0; 
                    match Token.to_rawtoken t with
                    | SEMICOLON -> raise Exit
                    | _ -> ()
                  done
                with
                  Exit -> ()
              end;
              let _, _, ed' = Token.decompose !last in
              begin
                let q = Queue.create() in
                Queue.add res q;
                Queue.iter (fun x -> Queue.add x q) q0;
                DEBUG_MSG "token queue: [%s]" (string_of_token_queue q);
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
                    let q = Queue.create() in
                    Queue.add (kw_to_ident "assert" res) q;
                    Queue.iter 
                      (fun x -> 
                        let x' =
                          match Token.to_rawtoken x with
                          | ASSERT loc' -> kw_to_ident "assert" x
                          | _ -> x
                        in
                        Queue.add x' q
                      ) q0;
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
                      _ -> Token.create ERROR_STMT st ed'
                  end
              end
          end
          | _ -> begin
              DEBUG_MSG "ASSERT --> <identifier>";
              set_to_JLS2 loc "assert";
              kw_to_ident "assert" res
          end
      end
      | _ -> res
    in
    env#set_last_rawtoken (Obj.repr (Token.to_rawtoken res'));
    res'


end (* of functor Ulexer.F *)

