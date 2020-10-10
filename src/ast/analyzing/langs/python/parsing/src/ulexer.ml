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
 * A lexer (utf-8) for the Python programming language
 *
 * ulexer.ml
 *
 *)

open Printf

open Tokens_
open Common

open Compat

module PB = Parserlib_base

type longstringmode = LSMfalse | LSMsingle | LSMdouble 


exception Illegal_indent



let indent_length ind =
  let c = ref 0 in
  let add_tab() = 
    let n = !c mod 8 in
    c := !c + 8 - n
  in
  let n = String.length ind in
  if n > 0 then begin
    if ind.[0] = '\012' then 
      ()
    else if ind.[0] = '\009' then 
      add_tab()
    else if ind.[0] = ' ' then 
      incr c
    else 
      raise Illegal_indent; 
    if n > 1 then
      for i = 1 to n - 1 do
	if ind.[i] = '\009' then 
	  add_tab()
	else if ind.[i] = ' ' then 
	  incr c
	else 
	  raise Illegal_indent
      done
  end;
  DEBUG_MSG "length=%d" !c;
  !c


let mklexpos i =
    { Lexing.pos_fname = "";
      Lexing.pos_lnum = 0;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = i
    }

let mktok rawtok ulexbuf =
  let st_pos = mklexpos (Ulexing.lexeme_start ulexbuf) in
  let ed_pos = mklexpos ((Ulexing.lexeme_end ulexbuf) - 1) in
  rawtok, st_pos, ed_pos

      

module F (Stat : Parser_aux.STATE_T) = struct

  open Stat

  let offsets_to_loc st ed =
    env#current_pos_mgr#offsets_to_loc st ed

  let lexing_error lexbuf msg = 
    let loc = offsets_to_loc (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf) in
    Common.fail_to_parse ~head:(Astloc.to_string ~prefix:"[" ~suffix:"]" loc) msg


  let find_keyword =
    let keyword_list =
      [ 
	"and",      AND;
	"assert",   ASSERT;
	"break",    BREAK;
	"class",    CLASS;
	"continue", CONTINUE;
	"def",      DEF;
	"del",      DEL;
	"elif",     ELIF;
	"else",     ELSE;
	"except",   EXCEPT;
	"exec",     EXEC;
	"finally",  FINALLY;
	"for",      FOR;
	"from",     FROM;
	"global",   GLOBAL;
	"if",       IF;
	"import",   IMPORT;
	"in",       IN;
	"is",       IS;
	"lambda",   LAMBDA;
	"not",      NOT;
	"or",       OR;
	"pass",     PASS;
	"print",    PRINT;
	"raise",    RAISE;
	"return",   RETURN;
	"try",      TRY;
	"while",    WHILE;
	"yield",    YIELD
      ] in 
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    let _ = 
      List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
	keyword_list 
    in
    let with_keywords = [ "as", AS; "with", WITHx ] in
    let find s = 
      try 
	Hashtbl.find keyword_table s
      with 
	Not_found -> 
	  if env#with_stmt_enabled then
	    try
	      List.assoc s with_keywords
	    with 
	      Not_found -> NAMEx s
	  else 
	    NAMEx s
    in
    find

  type token_t = Tokens_.token * Lexing.position * Lexing.position

  class scanner_env = object (self)

    val mutable longstringmode = LSMfalse

    val indent_stack = Stack.create()
    val token_stack = (Stack.create() : token_t Stack.t)
    val mutable paren_count = 0

    method longstringmode = longstringmode
    method set_longstringmode m = longstringmode <- m

    method push_indent n =
      DEBUG_MSG "pushed %d" n;
      Stack.push n indent_stack 

    method pop_indent =
      let n = Stack.pop indent_stack in
      DEBUG_MSG "poped %d" n;
      n

    method top_of_indent_stack = Stack.top indent_stack
    method indent_stack_iter f = Stack.iter f indent_stack
    method indent_stack_len = Stack.length indent_stack
    method init_indent_stack =
      Stack.clear indent_stack; 
      Stack.push 0 indent_stack

    method push_token t =
      DEBUG_MSG "pushed %s" (Token.to_string env#current_pos_mgr t);
      Stack.push t token_stack 

    method pop_token =
      let t = Stack.pop token_stack in
      DEBUG_MSG "poped %s" (Token.to_string env#current_pos_mgr t);
      t

    method is_token_stack_empty = Stack.is_empty token_stack
    method init_token_stack = 
      Stack.clear token_stack

    method paren_in = paren_count <- paren_count + 1
    method paren_out = paren_count <- paren_count - 1
    method in_paren = paren_count > 0
    method init_paren = paren_count <- 0


    method init =
      self#set_longstringmode LSMfalse;
      self#init_indent_stack;
      self#init_token_stack;
      self#init_paren

    initializer
      self#init

  end (* of Ulexer.scanner_env *)



  let regexp line_terminator = ['\013' '\010'] | "\013\010"

  let regexp white_space = [' ' '\009' '\012']

  let regexp indent = ('\012')? [' ' '\009']*

  let regexp line_comment = '#' [^'\013' '\010']*

  let regexp null_lines = line_terminator+ (indent line_comment? line_terminator+)*

  let regexp line_join = '\\' line_terminator

  let regexp letter = ['A'-'Z' 'a'-'z']
  let regexp digit = ['0'-'9']
  let regexp identifier = (letter | '_') (letter | digit | '_')*

  let regexp escapeseq = '\\' _

  let regexp longstring_single_quote = "'''"
  let regexp longstring_double_quote = "\"\"\""
  let regexp longstringchar = [^'\\']
  let regexp shortstringchar_single = [^'\\' '\013' '\010' '\'']
  let regexp shortstringchar_double = [^'\\' '\013' '\010' '\"']

  let regexp longstringitem = longstringchar | escapeseq
  let regexp shortstringitem_single = shortstringchar_single | escapeseq
  let regexp shortstringitem_double = shortstringchar_double | escapeseq

  let regexp shortstring = '\'' shortstringitem_single* '\'' | '"' shortstringitem_double* '"'
  let regexp stringprefix = "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR"

  let regexp string = stringprefix? shortstring

  let regexp longstring_start_single = stringprefix? longstring_single_quote
  let regexp longstring_start_double = stringprefix? longstring_double_quote

  let regexp hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
  let regexp hexinteger = '0' ['x' 'X'] hexdigit+
  let regexp octinteger = '0' ['o' 'O'] ['0'-'7']+
  let regexp bininteger = '0' ['b' 'B'] ['0' '1']+
  let regexp decimalinteger = '0' | ['1'-'9'] digit*
  let regexp integer = decimalinteger | bininteger | octinteger | hexinteger
  let regexp longinteger = integer ['l' 'L']

  let regexp exponent = ['e' 'E'] ['+' '-']? digit+
  let regexp intpart = digit+
  let regexp pointfloat = intpart? '.' digit+ | intpart '.'
  let regexp exponentfloat = (intpart|pointfloat) exponent
  let regexp floatnumber = pointfloat | exponentfloat
  let regexp imagnumber = (floatnumber | digit+) ['j' 'J']



  let rec token scanner_env = lexer

      line_join -> token scanner_env lexbuf
|   white_space -> token scanner_env lexbuf

|   line_comment ->
    DEBUG_MSG "[COMMENT] \"%s\"" (Ulexing.utf8_lexeme lexbuf);
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    DEBUG_MSG "[COMMENT] region: %d-%d" st ed;
    env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ed);
    token scanner_env lexbuf

|   integer                 -> mktok (INTEGER (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   longinteger             -> mktok (LONGINTEGER (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   floatnumber             -> mktok (FLOATNUMBER (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   imagnumber              -> mktok (IMAGNUMBER (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   longstring_start_single -> mktok (LONGSTRING_BEGIN_S (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   longstring_start_double -> mktok (LONGSTRING_BEGIN_D (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   string                  -> mktok (SHORTSTRING (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   null_lines ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "[NULL_LINES] \"%s\"" s;
    let st, ed = 
      Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1
    in
    DEBUG_MSG "[NULL_LINES]: region: %d-%d" st ed;
    env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ed);
    if scanner_env#in_paren then 
      token scanner_env lexbuf 
    else begin
      DEBUG_MSG " * checking indent";
      let ind = indent lexbuf in
      let len = indent_length ind in
      let top = scanner_env#top_of_indent_stack in
      if len = top then 
	()
      else if len > top then begin
	scanner_env#push_indent len; 
	scanner_env#push_token (mktok INDENT lexbuf)
      end
      else if len < top then begin
	let ok = ref false in
	scanner_env#indent_stack_iter (fun n -> if n = len then ok := true);
	if !ok then begin
	  let c = ref 0 in
	  scanner_env#indent_stack_iter 
	    (fun n -> 
	      if n > len then begin
		let _ = scanner_env#pop_indent in incr c 
	      end);
	  if !c > 0 then begin
	    for i = 1 to !c do
	      scanner_env#push_token (mktok DEDENT lexbuf)
	    done
	  end
	  else 
	    raise Illegal_indent
	end
	else 
	  raise Illegal_indent
      end
      else 
	raise Illegal_indent; (* impossible *)

      mktok (NEWLINE len) lexbuf
    end


|   "==" -> mktok EQ_EQ lexbuf
|   "<=" -> mktok LT_EQ lexbuf
|   ">=" -> mktok GT_EQ lexbuf
|   "!=" -> mktok EXCLAM_EQ lexbuf
|   "<>" -> mktok LT_GT lexbuf

|   "**" -> mktok STAR_STAR lexbuf
|   "//" -> mktok SLASH_SLASH lexbuf
|   "<<" -> mktok LT_LT lexbuf
|   ">>" -> mktok GT_GT lexbuf

|   "+="  -> mktok PLUS_EQ lexbuf
|   "-="  -> mktok MINUS_EQ lexbuf
|   "*="  -> mktok STAR_EQ lexbuf
|   "/="  -> mktok SLASH_EQ lexbuf
|   "//=" -> mktok SLASH_SLASH_EQ lexbuf
|   "%="  -> mktok PERCENT_EQ lexbuf
|   "&="  -> mktok AMP_EQ lexbuf
|   "|="  -> mktok PIPE_EQ lexbuf
|   "^="  -> mktok HAT_EQ lexbuf
|   ">>=" -> mktok GT_GT_EQ lexbuf
|   "<<=" -> mktok LT_LT_EQ lexbuf
|   "**=" -> mktok STAR_STAR_EQ lexbuf

|   '+' -> mktok PLUS lexbuf
|   '-' -> mktok MINUS lexbuf
|   '*' -> mktok STAR lexbuf
|   '/' -> mktok SLASH lexbuf
|   '%' -> mktok PERCENT lexbuf
|   '&' -> mktok AMP lexbuf
|   '|' -> mktok PIPE lexbuf
|   '^' -> mktok HAT lexbuf
|   '~' -> mktok TILDE lexbuf
|   '>' -> mktok GT lexbuf
|   '<' -> mktok LT lexbuf

|   '(' -> scanner_env#paren_in; mktok LPAREN lexbuf
|   ')' -> scanner_env#paren_out; mktok RPAREN lexbuf
|   '{' -> scanner_env#paren_in; mktok LBRACE lexbuf
|   '}' -> scanner_env#paren_out; mktok RBRACE lexbuf
|   '[' -> scanner_env#paren_in; mktok LBRACKET lexbuf
|   ']' -> scanner_env#paren_out; mktok RBRACKET lexbuf
|   '@' -> mktok AT lexbuf
|   ',' -> mktok COMMA lexbuf
|   ':' -> mktok COLON lexbuf
|   '.' -> mktok DOT lexbuf
|   '`' -> mktok BACKQUOTE lexbuf
|   '=' -> mktok EQ lexbuf
|   ';' -> mktok SEMICOLON lexbuf


|   '$' -> mktok DEDENT lexbuf (* illegal in python! *)

|   identifier -> mktok (find_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   eof 
  -> 
    let n = scanner_env#indent_stack_len - 1 in
    if n > 0 then begin
      for i = 1 to n do
	ignore (scanner_env#pop_indent);
	scanner_env#push_token (mktok DEDENT lexbuf)
      done
    end;
    (mktok EOF lexbuf)
      

|   _ -> 
    let sym = Ulexing.utf8_lexeme lexbuf in
    if sym = "\"" then
      lexing_error lexbuf "invalid single-quated string"
    else
      lexing_error lexbuf (sprintf "invalid symbol(%s)" sym)


(*
  and line_comment = lexer
      line_terminator -> ()
| _ -> line_comment lexbuf
*)

  and indent = lexer
      indent -> Ulexing.utf8_lexeme lexbuf

  and longstring_single s = lexer
      longstring_single_quote -> mktok (LONGSTRING_REST (s^(Ulexing.utf8_lexeme lexbuf))) lexbuf
|   longstringitem -> longstring_single (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf

  and longstring_double s = lexer
      longstring_double_quote -> mktok (LONGSTRING_REST (s^(Ulexing.utf8_lexeme lexbuf))) lexbuf
|   longstringitem -> longstring_double (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf





  class scanner = object (self)
    inherit [Tokens_.token] PB.scanner

    val scanner_env = new scanner_env

    val mutable ulexbuf_opt = None


    method init = 
      scanner_env#init

    method enter_source src =
      DEBUG_MSG "source=\"%s\"" src#filename;
      let ulexbuf =
        if src#filename = "<stdin>" then begin
          src#get_ulexbuf_from_stdin
        end
        else begin
          src#get_ulexbuf
        end
      in
      ulexbuf_opt <- Some ulexbuf;
      ulexbuf

    method get_token() =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          let tok =
	    if scanner_env#is_token_stack_empty then 
	      try
	        match scanner_env#longstringmode with
	        | LSMsingle -> begin 
		    scanner_env#set_longstringmode LSMfalse;
		    longstring_single "" ulexbuf
	        end
	        | LSMdouble -> begin 
		    scanner_env#set_longstringmode LSMfalse;
		    longstring_double "" ulexbuf
	        end
	        | LSMfalse -> token scanner_env ulexbuf
	      with 
	        Illegal_indent -> lexing_error ulexbuf "illegal indent"

	    else 
	      scanner_env#pop_token
          in
          DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr tok);
          (match Token.get_rawtoken tok with 
          | LONGSTRING_BEGIN_S _ -> scanner_env#set_longstringmode LSMsingle
          | LONGSTRING_BEGIN_D _ -> scanner_env#set_longstringmode LSMdouble 
          | _ -> ());
          tok
      end
      | None -> failwith "Ulexer.scanner#get_token"

    initializer
      scanner_env#init;
      env#set_enter_source_callback self#enter_source

  end (* of Ulexer.scanner *)


end (* of functor Ulexer.F *)


