(*
   Copyright 2012-2024 Codinuum Software Lab <https://codinuum.com>

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
  DEBUG_MSG "n=%d" n;
  DEBUG_MSG "ind=\"%s\"" (Xstring.encode ind);
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

  let loc_of_poss stp edp =
    let pos_mgr = env#current_pos_mgr in
    let so = stp.Lexing.pos_cnum in
    let sl, sc = pos_mgr#get_position so in
    let eo = edp.Lexing.pos_cnum in
    let el, ec = pos_mgr#get_position eo in
    Astloc.make ~fname:env#current_filename so eo sl sc el ec

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
        (*"exec",     EXEC;*)
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
        (*"print",    PRINT;*)
        "raise",    RAISE;
        "return",   RETURN;
        "try",      TRY;
        "while",    WHILE;
        "yield",    YIELD;

        "await",    AWAIT;
        "async",    ASYNC;
        "nonlocal", NONLOCAL;
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
    val mutable last_indent_len = 0

    val mutable newline_flag = false

    method newline_flag = newline_flag
    method set_newline_flag () = newline_flag <- true
    method clear_newline_flag () = newline_flag <- false

    method last_indent_len = last_indent_len
    method set_last_indent_len n =
      DEBUG_MSG "%d" n;
      last_indent_len <- n

    method longstringmode = longstringmode
    method set_longstringmode m = longstringmode <- m

    method push_indent n =
      DEBUG_MSG "pushed %d" n;
      Stack.push n indent_stack

    method pop_indent () =
      let n = Stack.pop indent_stack in
      DEBUG_MSG "poped %d" n;
      n

    method top_of_indent_stack = Stack.top indent_stack
    method indent_stack_iter f = Stack.iter f indent_stack
    method indent_stack_len = Stack.length indent_stack
    method init_indent_stack () =
      Stack.clear indent_stack;
      Stack.push 0 indent_stack

    method push_token t =
      DEBUG_MSG "pushed %s" (Token.to_string env#current_pos_mgr t);
      Stack.push t token_stack

    method pop_token () =
      let t = Stack.pop token_stack in
      DEBUG_MSG "poped %s" (Token.to_string env#current_pos_mgr t);
      t

    method is_token_stack_empty = Stack.is_empty token_stack
    method init_token_stack () =
      Stack.clear token_stack

    method paren_in () = paren_count <- paren_count + 1
    method paren_out () = paren_count <- paren_count - 1
    method in_paren = paren_count > 0
    method paren_count = paren_count
    method init_paren () = paren_count <- 0


    method init =
      self#set_longstringmode LSMfalse;
      self#init_indent_stack();
      self#init_token_stack();
      self#init_paren()

    initializer
      self#init

  end (* of Ulexer.scanner_env *)



  let regexp line_terminator = ['\013' '\010'] | "\013\010"

  let regexp white_space = [' ' '\009' '\012']

  let regexp indent = ('\012')? [' ' '\009']*

  let regexp line_comment = '#' [^'\013' '\010']*

  let regexp null_lines = line_terminator+ (indent line_comment? line_terminator+)*

  let regexp line_join = '\\' line_terminator

  let regexp letter = ['A'-'Z' 'a'-'z' 128-255 880-1023 1024-1279 2304-2431 4352-4607 43360-43391 44032-55215 55216-55295]
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
  let regexp stringprefix =
    "r" | "u" | "R" | "U" | "f" | "F" | "fr" | "Fr" | "fR" | "FR" |
    "rf" | "rF" | "Rf" | "RF" | "ur" | "UR" | "Ur" | "uR" |
    "b" | "B" | "br" | "Br" | "bR" | "BR" | "rb" | "rB" | "Rb" | "RB"

  let regexp string = stringprefix? shortstring

  let regexp longstring_start_single = stringprefix? longstring_single_quote
  let regexp longstring_start_double = stringprefix? longstring_double_quote

  let regexp hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
  let regexp hexinteger = '0' ['x' 'X'] ('_'? hexdigit)+
  let regexp octinteger = '0' ['o' 'O'] ('_'? ['0'-'7'])+
  let regexp bininteger = '0' ['b' 'B'] ('_'? ['0' '1'])+
  let regexp decimalinteger = '0' ('_'? '0')* | ['1'-'9'] ('_'? digit)*
  let regexp integer = decimalinteger | bininteger | octinteger | hexinteger
  let regexp longinteger = integer ['l' 'L']

  let regexp exponent = ['e' 'E'] ['+' '-']? digit+
  let regexp intpart = digit ('_'? digit)*
  let regexp pointfloat = intpart? '.' digit+ | intpart '.'
  let regexp exponentfloat = (intpart|pointfloat) exponent
  let regexp floatnumber = pointfloat | exponentfloat
  let regexp imagnumber = (floatnumber | digit+) ['j' 'J']

  let regexp conflict_marker = null_lines ("<<<<<<<" | "|||||||" | "=======" | ">>>>>>>")


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

|   conflict_marker ->
    let m = Xstring.lstrip (Ulexing.utf8_lexeme lexbuf) in
    let nl = mktok (NEWLINE scanner_env#top_of_indent_stack) lexbuf in
    marker nl scanner_env (Ulexing.lexeme_start lexbuf) m lexbuf

|   null_lines ->
    scanner_env#set_newline_flag();
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    DEBUG_MSG "[NULL_LINES] \"%s\": region: %d-%d"
      (Xstring.encode (Ulexing.utf8_lexeme lexbuf)) st ed;
    env#comment_regions#add (env#current_pos_mgr#offsets_to_loc st ed);
    if scanner_env#in_paren then begin
      let ind_len = indent_length (indent lexbuf) in
      scanner_env#set_last_indent_len ind_len;
      token scanner_env lexbuf
    end
    else begin
      DEBUG_MSG "checking indent...";
      let ind = indent lexbuf in (* next indent *)
      check_indent scanner_env ind lexbuf
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
|   ":="  -> mktok COLON_EQ lexbuf

|   "->"  -> mktok MINUS_GT lexbuf
|   "..." -> mktok ELLIPSIS lexbuf

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

|   '(' -> scanner_env#paren_in(); mktok LPAREN lexbuf
|   ')' -> scanner_env#paren_out(); mktok RPAREN lexbuf
|   '{' -> scanner_env#paren_in(); mktok LBRACE lexbuf
|   '}' -> scanner_env#paren_out(); mktok RBRACE lexbuf
|   '[' -> scanner_env#paren_in(); mktok LBRACKET lexbuf
|   ']' -> scanner_env#paren_out(); mktok RBRACKET lexbuf
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
        ignore (scanner_env#pop_indent());
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

  and handle_indent ?(eof_flag=false) ?(prepend_opt=None) scanner_env ind_len lexbuf =
    DEBUG_MSG "eof_flag=%B" eof_flag;
    DEBUG_MSG "ind_len=%d" ind_len;
    let tok_consumer =
      match prepend_opt with
      | Some prepend -> prepend
      | None -> scanner_env#push_token
    in
    let top = scanner_env#top_of_indent_stack in
    DEBUG_MSG "top=%d" top;
    if not eof_flag && ind_len = top then
      ()
    else if ind_len > top then begin
      scanner_env#push_indent ind_len;
      tok_consumer (mktok INDENT lexbuf)
    end
    else if eof_flag || ind_len < top then begin
      let ok = ref false in
      scanner_env#indent_stack_iter (fun n -> if n = ind_len then ok := true);
      DEBUG_MSG "ok=%B" !ok;
      if !ok then begin
        let c = ref 0 in
        if eof_flag then
          incr c;
        scanner_env#indent_stack_iter
          (fun n ->
            DEBUG_MSG "n=%d" n;
            if eof_flag || n > ind_len then begin
              let _ = scanner_env#pop_indent() in
              incr c
            end);
        if !c > 0 then begin
          for i = 1 to !c do
            tok_consumer (mktok DEDENT lexbuf)
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

  and check_indent scanner_env ind lexbuf =
    let ind_len = indent_length ind in
    DEBUG_MSG "ind_len=%d" ind_len;
    scanner_env#set_last_indent_len ind_len;
    handle_indent scanner_env ind_len lexbuf;
    mktok (NEWLINE ind_len) lexbuf


  and indent = lexer
      indent -> Ulexing.utf8_lexeme lexbuf

  and longstring_single s = lexer
      longstring_single_quote -> mktok (LONGSTRING_REST (s^(Ulexing.utf8_lexeme lexbuf))) lexbuf
|   longstringitem -> longstring_single (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf

  and longstring_double s = lexer
      longstring_double_quote -> mktok (LONGSTRING_REST (s^(Ulexing.utf8_lexeme lexbuf))) lexbuf
|   longstringitem -> longstring_double (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf

  and marker nl scanner_env st s = lexer
|   null_lines ->
    let ind = indent lexbuf in
    let _ = check_indent scanner_env ind lexbuf in
    let mtok = (MARKER s), mklexpos st, mklexpos ((Ulexing.lexeme_end lexbuf) - 1) in
    scanner_env#push_token mtok;
    nl

|   _ -> marker nl scanner_env st (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf


  let peek_nth queue scanner_env ulexbuf nth =
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
          let t = token scanner_env ulexbuf in
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

  let token_queue_to_string tq =
    let is_alpha_numeric c =
      match c with
      | '0'..'9' | 'a'..'z' | 'A'..'Z' -> true
      | _ -> false
    in
    let buf = Buffer.create 0 in
    tq#iter
      (fun t ->
        let s = Token.to_orig t in
        begin
          match s with
          | "." | ")" | "," | ";" | "}" | "]" -> ()
          | _ -> begin
              let buf_len = Buffer.length buf in
              if buf_len > 0 then
                match Buffer.nth buf (buf_len - 1) with
                | '.' | '(' | '@' | '[' -> ()
                | c when is_alpha_numeric c && s = "(" -> ()
                | _ -> Buffer.add_string buf " "
          end
        end;
        Buffer.add_string buf s
      );
    Buffer.contents buf

  let is_outline_rawtoken = function
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | LBRACKET
    | RBRACKET
    | INDENT
    | DEDENT
      -> true
    | _ -> false

  let outline_queue_to_string q =
    let buf = Buffer.create 0 in
    q#iter
      (fun t ->
        let rt = Token.to_rawtoken t in
        if is_outline_rawtoken rt then
          let s = Token.rawtoken_to_orig rt in
          Buffer.add_string buf s
      );
    Buffer.contents buf

  let is_stmt_head = function
    | PRINT | DEL | PASS | BREAK | CONTINUE | RETURN | RAISE(* | YIELD*)
    | IMPORT | GLOBAL | NONLOCAL | EXEC | ASSERT
    (*| IF | ELSE *)| WHILE(* | FOR*) | TRY | WITHx(* | ASYNC*) | DEF | CLASS | AT
      -> true
    | _ -> false

  class scanner = object (self)
    inherit [Tokens_.token] PB.scanner

    val scanner_env = new scanner_env

    val mutable ulexbuf_opt = None

    val queue = new Xqueue.c

    val shadow_queue = new Xqueue.c
    (*val shadow_q = new Xqueue.c*)

    val mutable last_rawtoken = Tokens_.EOF
    val mutable last_rawtoken2 = Tokens_.EOF

    method init =
      scanner_env#init

    initializer
      scanner_env#init;
      env#set_enter_source_callback self#enter_source

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

    method current_indent = scanner_env#top_of_indent_stack

    method scanner_env = scanner_env

    method reset_paren_level () =
      scanner_env#init_paren();
      env#reset_paren_level()

    method prepend_token tok =
      DEBUG_MSG "%s" (Token.rawtoken_to_string (Token.to_rawtoken tok));
      queue#prepend tok

    method prepend_rawtoken rawtok stp edp =
      let t = Token.create rawtok stp edp in
      self#prepend_token t

    method peek_nth nth =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          let token, rawtok = peek_nth queue scanner_env ulexbuf nth in
          DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr token);
          token, rawtok
      end
      | None -> failwith "Ulexer.F.scanner#peek_nth"

    method peek_up_to check_rawtok =
      let rec peek l nth =
        try
          let tok, rawtok = self#peek_nth nth in
          if check_rawtok rawtok then
            l, nth
          else
            peek ((tok, rawtok)::l) (nth + 1)
        with
          _ -> l, nth
      in
      peek [] 1

    method peek_nth_rawtoken nth =
      let _, rt = self#peek_nth nth in
      rt

    method shadow_queue = shadow_queue
    method reset_shadow_queue = shadow_queue#clear
    method shadow_contents = token_queue_to_string shadow_queue
    method copy_shadow_queue = shadow_queue#copy
    method prepend_shadow_queue q =
      DEBUG_MSG "shadow_queue=%s" self#shadow_contents;
      DEBUG_MSG "q=%s" (outline_queue_to_string q);
      shadow_queue#prepend_from q

    method shadow_outline = outline_queue_to_string shadow_queue
(*
    method shadow_q = shadow_q
    method reset_shadow_q = shadow_q#clear
    method shadow_outline = outline_queue_to_string shadow_q
    method copy_shadow_q = shadow_q#copy
    method prepend_shadow_q q =
      DEBUG_MSG "shadow_q=%s" self#shadow_outline;
      DEBUG_MSG "q=%s" (outline_queue_to_string q);
      shadow_q#prepend_from q
*)

    method has_error () =
      let b =
        try
          shadow_queue#iter
            (fun t ->
              DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr t);
              match Token.decompose t with
              | ERROR _, _, _ -> raise Exit
              | _, stp, edp ->
                  if stp = Lexing.dummy_pos && edp = Lexing.dummy_pos then
                    raise Exit
            );
          false
        with
          Exit -> true
      in
      DEBUG_MSG "%B" b;
      b

    method discard_tokens n =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          for i = 1 to n do
            let tok = self#__get_token ulexbuf in
            DEBUG_MSG ">> %s" (Token.to_string env#current_pos_mgr tok);
            ignore tok
          done
      end
      | _ -> ()

    method __get_token ulexbuf =
      let get_token () =
        let tok =
          if scanner_env#is_token_stack_empty then begin
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
          end
          else
            scanner_env#pop_token()
        in
        begin
          match Token.get_rawtoken tok with
          | LONGSTRING_BEGIN_S _ -> scanner_env#set_longstringmode LSMsingle
          | LONGSTRING_BEGIN_D _ -> scanner_env#set_longstringmode LSMdouble
          | _ -> ()
        end;
        tok
      in
      let t =
        if queue#is_empty then
          get_token()
        else
          queue#take
      in
      env#set_last_token (Obj.repr t);
      t

    method _get_token () =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          let token = self#__get_token ulexbuf in
          (*DEBUG_MSG "--> %s" (Token.to_string env#current_pos_mgr token);*)
          token
      end
      | None -> failwith "Ulexer.scanner#_get_token"

    method get_token () =

      let token = self#_get_token() in

      let rawtok, stp, edp = Token.decompose token in

      let is_print_stmt =
        match rawtok with
        | NAMEx "print" -> begin
            match last_rawtoken with
            | IMPORT -> false
            | _ ->
                match self#peek_nth_rawtoken 1 with
                | LPAREN -> false
                | _ ->
                    DEBUG_MSG "is_print_stmt=true";
                    true
        end
        | _ -> false
      in
      let is_exec_stmt =
        match rawtok with
        | NAMEx "exec" -> begin
            match self#peek_nth_rawtoken 1 with
            | LPAREN | EQ -> false
            | _ ->
                DEBUG_MSG "is_exec_stmt=true";
                true
        end
        | _ -> false
      in

      let token, rawtok =
        if is_print_stmt then begin
          let _, s, e = Token.decompose token in
          let t = Token.create PRINT s e in
          t, PRINT
        end
        else if is_exec_stmt then begin
          let _, s, e = Token.decompose token in
          let t = Token.create EXEC s e in
          t, EXEC
        end
        else if env#keep_going_flag && stp <> Lexing.dummy_pos && edp <> Lexing.dummy_pos then begin
          DEBUG_MSG "block_level=%d" env#block_level;
          DEBUG_MSG "paren_level=%d" env#paren_level;
          match rawtok with
          | RPAREN when env#paren_level = 0 -> begin
              let loc = loc_of_poss stp edp in
              DEBUG_MSG "discarding a redundant closing parenthesis";
              Parser_aux.warning_loc loc "discarding a redundant closing parenthesis";
              self#reset_paren_level();
              let token = self#_get_token() in
              let rawtok = Token.to_rawtoken token in
              token, rawtok
          end
          | INDENT when begin
              match last_rawtoken with
              | NEWLINE _ -> last_rawtoken2 != COLON
              | _ -> false
          end -> begin
            let loc = loc_of_poss stp edp in
            DEBUG_MSG "discarding a redundant indent";
            Parser_aux.warning_loc loc "discarding a redundant indent";
            let token = self#_get_token() in
            let rawtok = Token.to_rawtoken token in
            token, rawtok
          end
          | DEDENT when begin
              match last_rawtoken with
              | NEWLINE _ -> last_rawtoken2 == COLON
              | _ -> false
          end -> begin
            DEBUG_MSG "last_rawtoken2=%s" (Token.rawtoken_to_string last_rawtoken2);
            DEBUG_MSG "last_rawtoken=%s" (Token.rawtoken_to_string last_rawtoken);
            DEBUG_MSG "rawtok=%s" (Token.rawtoken_to_string rawtok);
            DEBUG_MSG "outline=%s" self#shadow_outline;
            let loc = loc_of_poss stp edp in
            DEBUG_MSG "adding indent and dedent";
            Parser_aux.warning_loc loc "adding indent and dedent";
            self#prepend_token token;
            (*let ind_len = self#current_indent + 4 in
            self#prepend_token (Token.create (Tokens_.NEWLINE ind_len) stp stp);
            self#prepend_token (Token.create (Tokens_.PASS) stp stp);*)
            self#prepend_token (Token.create (Tokens_.DEDENT) stp stp);
            let token = Token.create (Tokens_.INDENT) stp stp in
            let rawtok = Token.to_rawtoken token in
            token, rawtok
          end
          | rt when begin
              last_rawtoken != COLON && env#paren_level > 0 &&
              (
               is_stmt_head rt || rt == EOF ||
               match rt with
               | NAMEx _ -> begin
                   match last_rawtoken with
                   | RPAREN | NAMEx _
                     -> true
                   | _ -> false
               end
               | _ -> false
              )
          end -> begin
            DEBUG_MSG "newline_flag=%B" scanner_env#newline_flag;
            DEBUG_MSG "last_rawtoken=%s" (Token.rawtoken_to_string last_rawtoken);
            DEBUG_MSG "rawtok=%s" (Token.rawtoken_to_string rawtok);
            DEBUG_MSG "outline=%s" self#shadow_outline;
            let n = env#paren_level in
            let loc = loc_of_poss stp edp in

            if scanner_env#newline_flag then begin
              DEBUG_MSG "adding %d closing parentheses" n;
              Parser_aux.warning_loc loc "adding %d closing parentheses" n;
              self#prepend_token token;
              let ind_len = scanner_env#last_indent_len in
              begin
                match ulexbuf_opt with
                | Some ulexbuf ->
                    let prepend_opt = Some self#prepend_token in
                    let eof_flag = rawtok == EOF in
                    handle_indent ~eof_flag scanner_env ~prepend_opt ind_len ulexbuf
                | _ -> ()
              end;
              self#prepend_token (Token.create (Tokens_.NEWLINE ind_len) stp stp);
              let t = Token.create Tokens_.RPAREN stp stp in
              for i = 1 to n do
                self#prepend_token t;
              done;
              self#reset_paren_level();
              let token = self#_get_token() in
              let rawtok = Token.to_rawtoken token in
              token, rawtok
            end
            else begin
              DEBUG_MSG "adding comma";
              Parser_aux.warning_loc loc "adding comma";
              self#prepend_token token;
              let token = Token.create (Tokens_.COMMA) stp stp in
              let rawtok = Token.to_rawtoken token in
              token, rawtok
            end

          end
          | _ -> token, rawtok
        end
        else
          token, rawtok
      in

      begin
        match rawtok with
        | LPAREN -> begin
            env#open_paren();
            (*shadow_q#add token*)
        end
        | RPAREN when env#paren_level > 0 -> begin
            env#close_paren();
            (*shadow_q#add token*)
        end
        | LBRACE -> begin
            env#open_brace();
            (*shadow_q#add token*)
        end
        | RBRACE when env#brace_level > 0 -> begin
            env#close_brace();
            (*shadow_q#add token*)
        end
        | LBRACKET -> begin
            env#open_bracket();
            (*shadow_q#add token*)
        end
        | RBRACKET when env#brace_level > 0 -> begin
            env#close_bracket();
            (*shadow_q#add token*)
        end
        | INDENT -> begin
            env#open_block();
            (*shadow_q#add token*)
        end
        | DEDENT when env#block_level > 0 -> begin
            env#close_block();
            (*shadow_q#add token*)
        end
        | _ -> ()
      end;
      shadow_queue#add token;
      DEBUG_MSG "shadow_queue length: %d" shadow_queue#length;
      DEBUG_MSG "---------- %s" (Token.to_string env#current_pos_mgr token);
      env#clear_shift_flag();
      last_rawtoken2 <- last_rawtoken;
      last_rawtoken <- rawtok;
      scanner_env#clear_newline_flag();
      token


  end (* of Ulexer.scanner *)


end (* of functor Ulexer.F *)
