(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>
   Copyright 2020 Chiba Institute of Technology

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

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)

module C = Context
module PB = Parserlib_base
open Tokens_
open Compat


let regexp white_space = [' ' '\009' '\012']
let regexp line_terminator = ['\013' '\010'] | "\013\010"

let regexp h_char = [^'>' '\013' '\010']
let regexp q_char = [^'"' '\013' '\010']

let regexp header_name = '<' h_char+ '>' | '"' q_char+ '"'

let regexp digit = ['0'-'9']

let regexp nondigit = ['a'-'z' 'A'-'Z' '_' '$'] | "##"

let regexp hexadecimal_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let regexp hex_quad = hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit

let regexp universal_character_name = 'u' hex_quad | 'U' hex_quad hex_quad

let regexp shell_var = "${" nondigit (nondigit|digit|[':' '-'])* '}'

let regexp identifier_nondigit = nondigit | universal_character_name | shell_var | '?' '?' '?'+

let regexp identifier = identifier_nondigit (identifier_nondigit|digit)*

let regexp sign = ['+' '-']

let regexp pp_number =
  '.'? digit (digit|identifier_nondigit|'\'' digit|'\'' nondigit| ['e' 'E' 'p' 'P'] sign|'.')*

let regexp unsigned_suffix = ['u' 'U']
let regexp long_suffix = ['l' 'L']
let regexp long_long_suffix = "ll" | "LL"

let regexp integer_suffix =
  unsigned_suffix long_suffix? |
  unsigned_suffix long_long_suffix? |
  long_suffix unsigned_suffix? |
  long_long_suffix unsigned_suffix?

let regexp binary_digit = ['0' '1']
let regexp octal_digit = ['0'-'7']
let regexp nonzero_digit = ['1'-'9']


let regexp hexadecimal_prefix = '0' ['x' 'X']

let regexp hexadecimal_digit_sequence = (hexadecimal_digit+ '\'')* hexadecimal_digit+

let regexp binary_literal = '0' ['b' 'B'] (binary_digit+ '\'')* binary_digit+
let regexp octal_literal = '0' (octal_digit* '\'' octal_digit)* octal_digit*
let regexp decimal_literal = nonzero_digit (digit* '\'' digit)* digit*
let regexp hexadecimal_literal = hexadecimal_prefix hexadecimal_digit_sequence

let regexp integer_literal =
  binary_literal integer_suffix? |
  octal_literal integer_suffix? |
  decimal_literal integer_suffix? |
  hexadecimal_literal integer_suffix?

(*let regexp escape_sequence = '\\'
    (['\'' '"' '?' '\\' 'a' 'b' 'f' 'n' 'r' 't' 'v'] |
     octal_digit (octal_digit octal_digit?)? |
     'x' hexadecimal_digit+)*)

let regexp escape_sequence = '\\'
    (['\'' '"' '?' '\\' 'a'-'z' '%'] | (* !!! to handle "\con" for instance *)
     octal_digit (octal_digit octal_digit?)? |
     ('x'|'U') hexadecimal_digit+) (* to handle non-standard \U *)

let regexp c_char = [^'\'' '\\' '\013' '\010'] | escape_sequence | universal_character_name

let regexp encoding_prefix = 'u' '8' | ['u' 'U' 'L']

let regexp character_literal = encoding_prefix? '\'' c_char+ '\''

let regexp digit_sequence = (digit+ '\'')* digit+

let regexp fractional_constant = digit_sequence? '.' digit_sequence | digit_sequence '.'
let regexp hexadecimal_fractional_constant =
  hexadecimal_digit_sequence? '.' hexadecimal_digit_sequence | hexadecimal_digit_sequence '.'

let regexp exponent_part = ['e' 'E'] sign? digit_sequence
let regexp binary_exponent_part = ['p' 'P'] sign? digit_sequence
let regexp floating_suffix = ['f' 'l' 'F' 'L']

let regexp decimal_floating_literal =
  fractional_constant exponent_part? floating_suffix? |
  digit_sequence exponent_part floating_suffix?

let regexp hexadecimal_floating_literal =
  hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part floating_suffix? |
  hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part floating_suffix?

let regexp floating_literal = decimal_floating_literal | hexadecimal_floating_literal

let regexp s_char = [^'"' '\\' '\013' '\010'] | escape_sequence | universal_character_name
let regexp s_char_no_bq = [^'`' '"' '\\' '\013' '\010']
let regexp d_char = [^' ' '(' ')' '\\' '\009' '\011' '\012' '\013' '\010']
let regexp r_char = '.' (* !!! *)

let regexp s_char_sequence = (s_char|'\\' (identifier|line_terminator|'('|')'|'/'|','|']')|line_terminator [^'#'])+
let regexp s_char_sequence_no_bq = (s_char_no_bq)+
let regexp d_char_sequence = d_char+
let regexp r_char_sequence = r_char+

let regexp raw_string_head = encoding_prefix? 'R' '"' d_char_sequence? '('
let regexp raw_string_tail = ')' d_char_sequence? '"'

let regexp string_literal = encoding_prefix? '"' s_char_sequence? '"' | '@' '"' s_char_sequence? '"' |
'`' s_char_sequence_no_bq '`'

let regexp boolean_literal = "false" | "true"

(*let regexp pointer_literal = "nullptr"*)

let regexp user_defined_integer_literal = integer_literal identifier
let regexp user_defined_floating_literal = floating_literal identifier
let regexp user_defined_string_literal = string_literal identifier
let regexp user_defined_character_literal = character_literal identifier

let regexp user_defined_literal =
  user_defined_integer_literal | user_defined_floating_literal |
  user_defined_string_literal | user_defined_character_literal


let regexp literal =
  integer_literal | character_literal | floating_literal | string_literal |
  boolean_literal (*| pointer_literal*) | user_defined_literal



let regexp pp_keyword = '#' white_space* identifier
let regexp ws_or_lt = white_space | '\\' line_terminator
let regexp ident_or_symbol = identifier | '.' | '=' | ['0'-'9']+identifier?
let regexp pp_concatenated_identifier =
  (identifier ws_or_lt* "##" ws_or_lt* )+ (ident_or_symbol ws_or_lt* "##" ws_or_lt* )* ident_or_symbol |
  (identifier "#")+ identifier?

(* *)

let ws_pat = Str.regexp "[ \009\012\013\010\\]+"

let normalize_pp_keyword k =
  let s = Str.global_replace ws_pat "" k in
  String.lowercase_ascii s

let normalize_pp_concatenated_ident k =
  IDENT (Str.global_replace ws_pat "" k)

let raw_string_head_pat = Str.regexp "^\\(u8\\|u\\|U\\|L\\)?R\"\\([^' ' '(' ')' '\\' '\009' '\011' '\012' '\013' '\010']*\\)($"

let get_marker s =
  let b = Str.string_match raw_string_head_pat s 0 in
  if b then
    Str.matched_group 2 s
  else
    ""

module StringHash = struct
  type t = string
  let equal s0 s1 = String.equal s0 s1
  let hash s = Hashtbl.hash_param 5 8 s
end

module StringHashtbl = Hashtbl.Make (StringHash)

(*module StringMap = Map.Make (String)*)


let _find_objc_keyword =
  let keyword_list =
    [
      "@class",        OBJC_CLASS;
      "@interface",    OBJC_INTERFACE;
      "@end",          OBJC_END;
      "@property",     OBJC_PROPERTY;
      "@private",      OBJC_PRIVATE;
      "@public",       OBJC_PUBLIC;
      "@protected",    OBJC_PROTECTED;
      "@package",      OBJC_PACKAGE;
      "@protocol",     OBJC_PROTOCOL;
      "@encode",       OBJC_ENCODE;
      "@synchronized", OBJC_SYNCHRONIZED;
      "@selector",     OBJC_SELECTOR;
      "@defs",         OBJC_DEFS;
      "@try",          OBJC_TRY;
      "@throw",        OBJC_THROW;
      "@catch",        OBJC_CATCH;
      "@finally",      OBJC_FINALLY;
      "@synthesize",   OBJC_SYNTHESIZE;
      "@dynamic",      OBJC_DYNAMIC;
      "@optional",     OBJC_OPTIONAL;
      "@required",     OBJC_REQUIRED;
      "@autoreleasepool", OBJC_AUTORELEASEPOOL;
      "@available",    OBJC_AVAILABLE;
    ] in
  let keyword_table = StringHashtbl.create (List.length keyword_list) in
  let _ =
    List.iter (fun (kwd, tok) -> StringHashtbl.add keyword_table kwd tok)
      keyword_list
  in
  fun s -> StringHashtbl.find keyword_table s

let find_objc_keyword s =
  try
    _find_objc_keyword s
  with
    Not_found -> (*OBJC_UNKNOWN*)IDENT s


let _find_pp_keyword =
  let keyword_list =
    [
     "#include", (fun () -> PP_INCLUDE);
     "#define",  (fun () -> PP_DEFINE);
     "#undef",   (fun () -> PP_UNDEF);
     "#line",    (fun () -> PP_LINE);
     "#error",   (fun () -> PP_ERROR);
     "#pragma",  (fun () -> PP_PRAGMA);
     "#if",      (fun () -> PP_IF);
     "#ifdef",   (fun () -> PP_IFDEF);
     "#ifndef",  (fun () -> PP_IFNDEF);
     "#elif",    (fun () -> PP_ELIF (ref ""));
     "#else",    (fun () -> PP_ELSE (ref ""));
     "#endif",   (fun () -> PP_ENDIF (ref ""));
     "#import",  (fun () -> PP_IMPORT);
    ] in
  let keyword_table = StringHashtbl.create (List.length keyword_list) in
  let _ =
    List.iter (fun (kwd, tok) -> StringHashtbl.add keyword_table kwd tok)
      keyword_list
  in
  fun s -> (StringHashtbl.find keyword_table (normalize_pp_keyword s))()
  (*let keyword_map =
    List.fold_left (fun m (kwd, tok) -> StringMap.add kwd tok m) StringMap.empty keyword_list
  in
  fun s -> StringMap.find (normalize_pp_keyword s) keyword_map*)

let find_pp_keyword s =
  try
    _find_pp_keyword s
  with
    Not_found -> PP_UNKNOWN s


let _find_keyword =
  let keyword_list =
    [
     "alignas"          , ALIGNAS;
     "alignof"          , ALIGNOF;
     "asm"              , ASM;
     "auto"             , AUTO;
     "bool"             , BOOL;
     "break"            , BREAK;
     "case"             , CASE;
     "catch"            , CATCH;
     "char"             , CHAR;
     "char8_t"          , CHAR8_T;
     "char16_t"         , CHAR16_T;
     "char32_t"         , CHAR32_T;
     "class"            , CLASS;
     "concept"          , CONCEPT;
     "const"            , CONST;
     "consteval"        , CONSTEVAL;
     "constexpr"        , CONSTEXPR;
     "constinit"        , CONSTINIT;
     "const_cast"       , CONST_CAST;
     "continue"         , CONTINUE;
     "decltype"         , DECLTYPE;
     "default"          , DEFAULT;
     "delete"           , DELETE;
     "double"           , DOUBLE;
     "do"               , DO;
     "dynamic_cast"     , DYNAMIC_CAST;
     "else"             , ELSE;
     "enum"             , ENUM;
     "explicit"         , EXPLICIT;
     "export"           , EXPORT;
     "extern"           , EXTERN;
     "false"            , FALSE;
     "float"            , FLOAT;
     "for"              , FOR;
     "friend"           , FRIEND;
     "goto"             , GOTO;
     "if"               , IF;
     "inline"           , INLINE;
     "int"              , INT;
     "long"             , LONG;
     "mutable"          , MUTABLE;
     "namespace"        , NAMESPACE;
     "new"              , NEW;
     "noexcept"         , NOEXCEPT;
     "nullptr"          , NULLPTR;
     "operator"         , OPERATOR;
     "private"          , PRIVATE;
     "protected"        , PROTECTED;
     "public"           , PUBLIC;
     "register"         , REGISTER;
     "reinterpret_cast" , REINTERPRET_CAST;
     "requires"         , REQUIRES;
     "return"           , RETURN;
     "short"            , SHORT;
     "signed"           , SIGNED;
     "sizeof"           , SIZEOF;
     "static"           , STATIC;
     "static_assert"    , STATIC_ASSERT;
     "static_cast"      , STATIC_CAST;
     "struct"           , STRUCT;
     "switch"           , SWITCH;
     "template"         , TEMPLATE;
     "this"             , THIS;
     "thread_local"     , THREAD_LOCAL;
     "throw"            , THROW;
     "true"             , TRUE;
     "try"              , TRY;
     "typedef"          , TYPEDEF;
     "typeid"           , TYPEID;
     "typename"         , TYPENAME;
     "union"            , UNION;
     "unsigned"         , UNSIGNED;
     "using"            , USING;
     "virtual"          , VIRTUAL;
     "void"             , VOID;
     "volatile"         , VOLATILE;
     "wchar_t"          , WCHAR_T;
     "while"            , WHILE;

     "and"              , AMP_AMP "and";
     "or"               , BAR_BAR "or";
     "xor"              , HAT "xor";
     "not"              , EXCLAM "not";
     "bitand"           , AMP "bitand";
     "bitor"            , BAR "bitor";
     "compl"            , TILDE "compl";
     "and_eq"           , AMP_EQ "and_eq";
     "or_eq"            , BAR_EQ "or_eq";
     "xor_eq"           , HAT_EQ "xor_eq";
     "not_eq"           , EXCLAM_EQ "not_eq";

     "audit"            , AUDIT;
     "axiom"            , AXIOM;
     "final"            , FINAL;
     "override"         , OVERRIDE;

     "defined"          , DEFINED;
     "__has_include"    , HAS_INCLUDE;
     "__has_cpp_attribute", HAS_CPP_ATTRIBUTE;

     "restrict"         , RESTRICT "restrict"; (* C99 *)
     "__restrict__"     , RESTRICT "__restrict__";

     "__asm__"          , GNU_ASM "__asm__";
     "__attribute__"    , GNU_ATTR "__attribute__";
     "__attribute"      , GNU_ATTR "__attribute";

     "__stdcall"        , MS_STDCALL "__stdcall";
     "WINAPI"           , MS_STDCALL "WINAPI";
     "APIENTRY"         , MS_STDCALL "APIENTRY";
     "__cdecl"          , MS_CDECL "__cdecl";
     "__asm"            , MS_ASM "__asm";
     "_asm"             , MS_ASM "_asm";
   ] in
  let keyword_table = StringHashtbl.create (List.length keyword_list) in
  let _ =
    List.iter (fun (kwd, tok) -> StringHashtbl.add keyword_table kwd tok)  keyword_list
  in
  StringHashtbl.find keyword_table
  (*let keyword_map =
    List.fold_left (fun m (kwd, tok) -> StringMap.add kwd tok m) StringMap.empty keyword_list
  in
  fun s -> StringMap.find s keyword_map*)

let find_keyword s =
  try
    _find_keyword s
  with
    Not_found -> IDENT s



module F (Stat : Parser_aux.STATE_T) = struct

  open Stat


  let mklexpos i =
    let mgr = env#current_pos_mgr in
    let l, c = mgr#get_position i in
    { Lexing.pos_fname = (*mgr#filename*)"";
      Lexing.pos_lnum  = l;
      Lexing.pos_bol   = i - c;
      Lexing.pos_cnum  = i
    }

  let mktok ?(st_opt=None) rawtok ulexbuf =
    let st_pos =
      match st_opt with
      | Some x -> mklexpos x
      | None -> mklexpos (Ulexing.lexeme_start ulexbuf)
    in
    let _ = DEBUG_MSG "%s" (Token.rawtoken_to_string rawtok) in
    env#clear_lex_line_head_flag();
    let ed_pos = mklexpos ((Ulexing.lexeme_end ulexbuf) - 1) in
    rawtok, st_pos, ed_pos


  let offsets_to_loc st ed = env#current_pos_mgr#offsets_to_loc st ed
  let _offsets_to_loc st ed sl sc el ec = env#current_pos_mgr#_offsets_to_loc st ed sl sc el ec
  let get_position ofs = env#current_pos_mgr#get_position ofs

  let add_comment_region loc =
    ()



  let rec _token = lexer
|  white_space -> _token lexbuf

|  line_terminator ->
    if env#lex_pp_line_flag then begin
      env#exit_lex_pp_line();
      env#exit_lex_ms_asm_line();
      let tok = mktok NEWLINE lexbuf in
      env#set_lex_line_head_flag();
      tok
    end
    else if env#lex_ms_asm_line_flag then begin
      env#exit_lex_ms_asm_line();
      let tok = mktok END_ASM lexbuf in
      env#set_lex_line_head_flag();
      tok
    end
    else begin
      env#set_lex_line_head_flag();
      _token lexbuf
    end

|  '\\' white_space* line_terminator -> _token lexbuf

|  "//" ->
    line_comment (Ulexing.lexeme_start lexbuf) lexbuf

|  "/*" ->
    let st = Ulexing.lexeme_start lexbuf in
    let sl, sc = get_position st in
    begin
      try
        block_comment (st, sl, sc) lexbuf
      with
        e -> begin
          let pos = Ulexing.lexeme_start lexbuf in
          let l, c = env#current_pos_mgr#get_position pos in
          let mes = Printf.sprintf "failed to handle block comment (%dL,%dC)" l c in
          raise (PB.Parse_error("", mes))
        end
    end

|  "/**/" ->
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    _token lexbuf

|  pp_keyword ->
    let kwd = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "kwd=%s" kwd;
    if env#lex_pp_line_flag || env#lex_ms_asm_line_flag || env#lex_line_head_flag then
      let r = find_pp_keyword kwd in
      let r =
        match r with
        | PP_UNKNOWN s when env#asm_flag -> IDENT s
        | _ -> env#enter_lex_pp_line(); r
      in
      mktok r lexbuf
    else
      mktok (IDENT kwd) lexbuf


|  integer_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (INT_LITERAL s) lexbuf

|  character_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (CHAR_LITERAL s) lexbuf

|  floating_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (FLOAT_LITERAL s) lexbuf

|  string_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (STR_LITERAL s) lexbuf

|  boolean_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (BOOL_LITERAL s) lexbuf

(*|  pointer_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (PTR_LITERAL s) lexbuf*)

|  user_defined_integer_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (USER_INT_LITERAL s) lexbuf

|  user_defined_floating_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (USER_FLOAT_LITERAL s) lexbuf

(*|  user_defined_string_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (USER_STR_LITERAL s) lexbuf*)

|  user_defined_character_literal ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (USER_CHAR_LITERAL s) lexbuf

| ">>>>>>>" -> mktok (GT_7 (ref false)) lexbuf
| "=======" -> conflict_marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf
| "|||||||" -> conflict_marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf
| "<<<<<<<" -> conflict_marker (Ulexing.lexeme_start lexbuf) (Ulexing.utf8_lexeme lexbuf) lexbuf

|  "%:%:" -> mktok SHARP_SHARP(*PERC_COLON_PERC_COLON*) lexbuf
|  "..." -> mktok ELLIPSIS lexbuf
|  "->*" -> mktok MINUS_GT_STAR lexbuf
|  "<=>" -> mktok LT_EQ_GT lexbuf
|  "<<=" -> mktok LT_LT_EQ lexbuf
|  ">>=" -> mktok GT_GT_EQ lexbuf
|  "<<<" -> mktok CUDA_LT_LT_LT lexbuf
|  "##" -> mktok SHARP_SHARP lexbuf
(*|  "<:" -> mktok LBRACKET(*LT_COLON*) lexbuf*)
|  ":>" -> mktok RBRACKET(*COLON_GT*) lexbuf
|  "<%" -> mktok LBRACE(*LT_PERC*) lexbuf
|  "%>" -> mktok RBRACE(*PERC_GT*) lexbuf
|  "%:" -> mktok SHARP(*PERC_COLON*) lexbuf
|  "::" -> mktok COLON_COLON lexbuf
|  ".*" -> mktok DOT_STAR lexbuf
|  "->" -> mktok MINUS_GT lexbuf
|  "+=" -> mktok PLUS_EQ lexbuf
|  "-=" -> mktok MINUS_EQ lexbuf
|  "*=" -> mktok STAR_EQ lexbuf
|  "/=" -> mktok SLASH_EQ lexbuf
|  "%=" -> mktok PERC_EQ lexbuf
|  "^=" -> mktok (HAT_EQ "^=") lexbuf
|  "&=" -> mktok (AMP_EQ "&=") lexbuf
|  "|=" -> mktok (BAR_EQ "|=") lexbuf
|  "==" -> mktok EQ_EQ lexbuf
|  "!=" -> mktok (EXCLAM_EQ "!=") lexbuf
|  "<=" -> mktok LT_EQ lexbuf
|  ">=" -> mktok GT_EQ lexbuf
|  "&&" -> mktok PTR_AMP_AMP lexbuf
|  "||" -> mktok (BAR_BAR "||") lexbuf
|  "<<" -> mktok LT_LT lexbuf
|  ">>" -> mktok GT_GT lexbuf
|  "++" -> mktok PLUS_PLUS lexbuf
|  "--" -> mktok MINUS_MINUS lexbuf
|  "{" ->
    if env#lex_ms_asm_line_flag then
      env#exit_lex_ms_asm_line();
    mktok LBRACE lexbuf
|  "}" -> mktok RBRACE lexbuf
|  "[" -> mktok LBRACKET lexbuf
|  "]" -> mktok RBRACKET lexbuf
|  "#" ->
    if env#lex_pp_line_flag || env#asm_flag then
      mktok SHARP lexbuf
    else begin
      env#enter_lex_pp_line();
      mktok PP_ lexbuf
    end
|  "(" -> mktok TY_LPAREN lexbuf
|  ")" -> mktok RPAREN lexbuf
|  ";" -> mktok (SEMICOLON true) lexbuf
|  ":" -> mktok COLON lexbuf
|  "?" -> mktok QUEST lexbuf
|  "." -> mktok DOT lexbuf
|  "~" -> mktok (TILDE "~") lexbuf
|  "!" -> mktok (EXCLAM "!") lexbuf
|  "+" -> mktok PLUS lexbuf
|  "-" -> mktok MINUS lexbuf
|  "*" -> mktok PTR_STAR lexbuf
|  "/" -> mktok SLASH lexbuf
|  "%" -> mktok PERC lexbuf
|  "^" -> mktok (HAT "^") lexbuf
|  "&" -> mktok PTR_AMP lexbuf
|  "|" -> mktok (BAR "|") lexbuf
|  "=" -> mktok EQ lexbuf
|  "<" -> mktok TEMPL_LT lexbuf
|  ">" -> mktok TY_TEMPL_GT lexbuf
|  "," -> mktok COMMA lexbuf


|  pp_concatenated_identifier ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (normalize_pp_concatenated_ident s) lexbuf

|  identifier ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let kw = find_keyword s in
    begin
      match kw with
      | MS_ASM _ -> env#enter_lex_ms_asm_line()
      | _ -> ()
    end;
    mktok kw lexbuf

|  raw_string_head ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "raw_string_head: %s" s;
    let m = get_marker s in
    DEBUG_MSG "marker: %s" m;
    let buf = Buffer.create 0 in
    Buffer.add_string buf s;
    raw_string m (Ulexing.lexeme_start lexbuf) buf lexbuf

|  '\\' identifier ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mktok (BS_IDENT s) lexbuf

|  "@@" ->
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    let l, c = env#current_pos_mgr#get_position st in
    Xprint.warning "unknown token \"@@\" found at (%dL,%dC)" l c;
    _token lexbuf

|  '@' identifier ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let kw = find_objc_keyword s in
    mktok kw lexbuf

|  '@' ->
    if env#asm_flag then
      line_comment (Ulexing.lexeme_start lexbuf) lexbuf
    else
      mktok AT lexbuf

|  '\\' -> mktok BS lexbuf

|  eof ->
    if env#lex_pp_line_flag then begin
      env#exit_lex_pp_line();
      mktok NEWLINE lexbuf
    end
    else
      mktok EOF lexbuf

|  _ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    if PB.is_bom s then
      Xprint.warning "BOM (0x%s:%s) found" (Xhash.to_hex s) (PB.get_bom_name s)
    else begin
      let l, c = env#current_pos_mgr#get_position (Ulexing.lexeme_start lexbuf) in
      Xprint.warning "unknown token \"%s\" found at (%dL,%dC)" s l c
    end;
    _token lexbuf
    (*raise Ulexing.Error*)


  and line_comment st = lexer
|   line_terminator ->
    if env#lex_pp_line_flag then begin
      env#exit_lex_pp_line();
      let tok = mktok NEWLINE lexbuf in
      env#set_lex_line_head_flag();
      tok
    end
    else begin
      let cloc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      add_comment_region cloc;
      env#set_lex_line_head_flag();
      _token lexbuf
    end

|  '\\' line_terminator -> line_comment st lexbuf

| _ -> line_comment st lexbuf

  and block_comment st_sl_sc = lexer
|  "*/" ->
    let st, sl, sc = st_sl_sc in
    let ed = (Ulexing.lexeme_end lexbuf) - 1 in
    let el, ec = get_position ed in
    add_comment_region (_offsets_to_loc st ed sl sc el ec);
    _token lexbuf

|  _ -> block_comment st_sl_sc lexbuf

  and raw_string marker st buf = lexer
| raw_string_tail ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "raw_string_tail: %s" s;
    let m = String.sub s 1 ((String.length s) - 2) in
    DEBUG_MSG "m: %s" m;
    if m = marker then begin
      Buffer.add_string buf s;
      mktok ~st_opt:(Some st) (STR_LITERAL (Buffer.contents buf)) lexbuf
    end
    else begin
      Buffer.add_string buf s;
      raw_string marker st buf lexbuf
    end

| identifier ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "s=\"%s\"" s;
    Buffer.add_string buf s;
    raw_string marker st buf lexbuf

| ' '+ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "s=\"%s\"" s;
    Buffer.add_string buf s;
    raw_string marker st buf lexbuf

| _ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "s=\"%s\"" s;
    Buffer.add_string buf s;
    raw_string marker st buf lexbuf

  and conflict_marker st s = lexer
| line_terminator ->
    (CONFLICT_MARKER(s, ref false)), mklexpos st, mklexpos ((Ulexing.lexeme_end lexbuf) - 1)
| _ -> conflict_marker st (s^(Ulexing.utf8_lexeme lexbuf)) lexbuf


end (* Ulexer.F *)
