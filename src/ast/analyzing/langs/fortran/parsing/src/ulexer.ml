(*
   Copyright 2013-2018 RIKEN
   Copyright 2018-2020 Chiba Institude of Technology

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

(* 
 * A lexer (utf-8) for Fortran language
 *
 * ulexer.ml
 *
 *)

module Loc = Astloc
module PB = Parserlib_base
module SF = Common.SourceForm
module PPD = Labels.PpDirective

open Common
open Tokens_

open Compat

module DL = DirectiveLine

let sep_count_thresh = 2

let ws_pat = Str.regexp "[ \009\012]+"

let normalize_pp_keyword k =
  let s = Str.global_replace ws_pat "" k in
  String.lowercase_ascii s

let _find_pp_keyword =
  let keyword_list =
    [ 
      "#undef",   PP_UNDEF;
      "#if",      PP_IF;
      "#else",    PP_ELSE;
      "#elif",    PP_ELIF;
      "#endif",   PP_ENDIF;
      "#ifdef",   PP_IFDEF;
      "#ifndef",  PP_IFNDEF;
      "#include", PP_INCLUDE;
      "#define",  PP_DEFINE;
      "#error",   PP_ERROR;
      "#warning", PP_WARNING;
    ] in 
  let keyword_table = Hashtbl.create (List.length keyword_list) in
  let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
      keyword_list 
  in
  let _find s = Hashtbl.find keyword_table (normalize_pp_keyword s) in
  _find

let find_pp_keyword s =
  try
    _find_pp_keyword s
  with
    Not_found -> PP_UNKNOWN

let pp_is_QCC_keyword = function
(*  | PP_BRANCH (PPD.Else|PPD.Endif)*)
  | PP_ELSE
  | PP_ENDIF
  | PP_IF
  | PP_ELIF
  | PP_IFDEF -> true
  | _ -> false


let _find_dotted_keyword, find_dotted_keyword =
  let keyword_list =
    [ 
      ".and.",   (fun s -> D_AND);
      ".eq.",    (fun s -> D_EQ);
      ".eqv.",   (fun s -> D_EQV);
      ".ge.",    (fun s -> D_GE);
      ".gt.",    (fun s -> D_GT);
      ".le.",    (fun s -> D_LE);
      ".lt.",    (fun s -> D_LT);
      ".ne.",    (fun s -> D_NE);
      ".neqv.",  (fun s -> D_NEQV);
      ".not.",   (fun s -> D_NOT);
      ".or.",    (fun s -> D_OR);

      ".true.",  (fun s -> LOGICAL_LITERAL s);
      ".false.", (fun s -> LOGICAL_LITERAL s);

    ] in 
  let keyword_table = Hashtbl.create (List.length keyword_list) in
  let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
      keyword_list 
  in
  let _find s = (Hashtbl.find keyword_table (String.lowercase_ascii s)) s in
  let find s = 
    try 
      _find s
    with 
      Not_found -> DEFINED_OP s
  in
  _find, find



let _find_keyword =
  let keyword_list =
    [ 
      (* keywords *)
      "attributes",      (fun s -> PREFIX_SPEC s);  (* PGI CUDA *)
      "abstract",        (fun s -> ABSTRACT s);     (* F2003 *)
      "allocatable",     (fun s -> ALLOCATABLE s);
      "allocate",        (fun s -> ALLOCATE s);
      "assign",          (fun s -> ASSIGN s);       (* F90 *)
      "assignment",      (fun s -> ASSIGNMENT s);
      "associate",       (fun s -> ASSOCIATE s);    (* F2003 *)
      "asynchronous",    (fun s -> ASYNCHRONOUS s); (* F2003 *)
      "backspace",       (fun s -> BACKSPACE s);
      "bind",            (fun s -> BIND s);         (* F2003 *)
      "block",           (fun s -> BLOCK s);        (* F2008 *)
      "blockdata",       (fun s -> BLOCK_DATA s);
      "byte",            (fun s -> BYTE s);
      "call",            (fun s -> CALL s);
      "case",            (fun s -> CASE s);
      "character",       (fun s -> CHARACTER s);
      "class",           (fun s -> CLASS s);        (* F2003 *)
      "close",           (fun s -> CLOSE s);
      "codimension",     (fun s -> CODIMENSION s);  (* F2008 *)
      "common",          (fun s -> COMMON s);
      "complex",         (fun s -> KINDED_TYPE_SPEC s);
      "concurrent",      (fun s -> CONCURRENT s);   (* F2008 *)
      "contains",        (fun s -> CONTAINS s);
      "contiguous",      (fun s -> SIMPLE_ATTR s);  (* F2008 *)
      "continue",        (fun s -> CONTINUE s);
      "critical",        (fun s -> CRITICAL s);     (* F2008 *)
      "cycle",           (fun s -> CYCLE s);
      "data",            (fun s -> DATA s);
      "deallocate",      (fun s -> DEALLOCATE s);
      "default",         (fun s -> DEFAULT s);
      "deferred",        (fun s -> DEFERRED s);     (* F2003 *)
      "dimension",       (fun s -> DIMENSION s);
      "do",              (fun s -> DO s);
      "double",          (fun s -> DOUBLE s);
      "doubleprecision", (fun s -> DOUBLE_PRECISION s);
      "doublecomplex",   (fun s -> DOUBLE_COMPLEX s);
      "else",            (fun s -> ELSE s);
      "elseif",          (fun s -> ELSE_IF s);
      "elsewhere",       (fun s -> ELSEWHERE s);
      "elemental",       (fun s -> PREFIX_SPEC s);
      "end",             (fun s -> END s);
      "endassociate",    (fun s -> END_ASSOCIATE s); (* F2003 *)
      "endblockdata",    (fun s -> END_BLOCK_DATA s);
      "endblock",        (fun s -> END_BLOCK s);   (* F2008 *)
      "endcritical",     (fun s -> END_CRITICAL s); (* F2008 *)
      "enddo",           (fun s -> END_DO s);
      "endenum",         (fun s -> END_ENUM s);    (* F2003 *)
      "endfile",         (fun s -> END_FILE s);
      "endforall",       (fun s -> END_FORALL s);
      "endfunction",     (fun s -> END_FUNCTION s);
      "endif",           (fun s -> END_IF s);
      "endinterface",    (fun s -> END_INTERFACE s);
      "endmodule",       (fun s -> END_MODULE s);
      "endprogram",      (fun s -> END_PROGRAM s);
      "endselect",       (fun s -> END_SELECT s);
      "endsubmodule",    (fun s -> END_SUBMODULE s); (* F2008 *)
      "endsubroutine",   (fun s -> END_SUBROUTINE s);
      "endtype",         (fun s -> END_TYPE s);
      "endwhere",        (fun s -> END_WHERE s);
      "entry",           (fun s -> ENTRY s);
      "enum",            (fun s -> ENUM s);         (* F2003 *)
      "enumerator",      (fun s -> ENUMERATOR s);   (* F2003 *)
(*      "errmsg",          (fun s -> ERRMSG s);       (* F2003 *)*)
      "error",           (fun s -> ERROR s);        (* F2008 *)
      "equivalence",     (fun s -> EQUIVALENCE s);
      "exit",            (fun s -> EXIT s);
      "extends",         (fun s -> EXTENDS s);      (* F2003 *)
      "external",        (fun s -> SIMPLE_ATTR s);
      "final",           (fun s -> FINAL s);        (* F2003 *)
      "flush",           (fun s -> FLUSH s);        (* F2003 *)
      "forall",          (fun s -> FORALL s);
      "format",          (fun s -> FORMAT s);
      "function",        (fun s -> FUNCTION s);
      "generic",         (fun s -> GENERIC s);
      "goto",            (fun s -> GO_TO s);
      "if",              (fun s -> IF s);
      "implicit",        (fun s -> IMPLICIT s);
      "import",          (fun s -> IMPORT s);       (* F2003 *)
      "impure",          (fun s -> PREFIX_SPEC s);  (* F2008 *)
      "in",              (fun s -> INTENT_SPEC s);
(*      "include",         (fun s -> INCLUDE s); *)
      "inout",           (fun s -> INTENT_SPEC s);
      "inquire",         (fun s -> INQUIRE s);
      "integer",         (fun s -> KINDED_TYPE_SPEC s);
      "intent",          (fun s -> INTENT s);
      "interface",       (fun s -> INTERFACE s);
      "intrinsic",       (fun s -> INTRINSIC s);
      "kind",            (fun s -> KIND s);
      "len",             (fun s -> LEN s);
      "lock",            (fun s -> LOCK s); (* F2008 *)
      "logical",         (fun s -> KINDED_TYPE_SPEC s);
      "module",          (fun s -> MODULE s);
      "mold",            (fun s -> ALLOC_OPT_EXPR s);      (* F2008 *)
      "namelist",        (fun s -> NAMELIST s);
      "none",            (fun s -> NONE s);
      "non_intrinsic",   (fun s -> NON_INTRINSIC s); (* F2003 *)
      "non_overridable", (fun s -> NON_OVERRIDABLE s); (* F2003 *)
      "nopass",          (fun s -> NOPASS s); (* F2003 *)
      "null",            (fun s -> NULL s);
      "nullify",         (fun s -> NULLIFY s);
      "only",            (fun s -> ONLY s);
      "open",            (fun s -> OPEN s);
      "operator",        (fun s -> OPERATOR s);
      "optional",        (fun s -> OPTIONAL s);
      "out",             (fun s -> INTENT_SPEC s);
      "pass",            (fun s -> PASS s); (* F2003 *)
      "parameter",       (fun s -> PARAMETER s);
      "pause",           (fun s -> PAUSE s);      (* F90 *)
      "pointer",         (fun s -> POINTER s);
      "precision",       (fun s -> PRECISION s);
      "print",           (fun s -> PRINT s);
      "private",         (fun s -> PRIVATE s);
      "procedure",       (fun s -> PROCEDURE s);
      "program",         (fun s -> PROGRAM s);
      "protected",       (fun s -> SIMPLE_ATTR s);  (* F2003 *)
      "public",          (fun s -> PUBLIC s);
      "pure",            (fun s -> PREFIX_SPEC s);
      "read",            (fun s -> READ s);
      "real",            (fun s -> KINDED_TYPE_SPEC s);
      "recursive",       (fun s -> PREFIX_SPEC s);
      "result",          (fun s -> RESULT s);
      "return",          (fun s -> RETURN s);
      "rewind",          (fun s -> REWIND s);
      "save",            (fun s -> SAVE s);
      "selectcase",      (fun s -> SELECT_CASE s);
      "selecttype",      (fun s -> SELECT_TYPE s); (* F2003 *)
      "sequence",        (fun s -> SEQUENCE s);
      "source",          (fun s -> ALLOC_OPT_EXPR s);      (* F2003 *)
(*      "stat",            (fun s -> STAT s);*)
      "stop",            (fun s -> STOP s);
      "submodule",       (fun s -> SUBMODULE s); (* F2008 *)
      "subroutine",      (fun s -> SUBROUTINE s);
      "sync",            (fun s -> SYNC s);
      "target",          (fun s -> TARGET s);
      "then",            (fun s -> THEN s);
      "to",              (fun s -> TO s);          (* F95 *)
      "type",            (fun s -> TYPE s);
      "use",             (fun s -> USE s);
      "value",           (fun s -> SIMPLE_ATTR s); (* F2003 *)
      "volatile",        (fun s -> SIMPLE_ATTR s); (* F2003 *)
      "wait",            (fun s -> WAIT s);        (* F2003 *)
      "where",           (fun s -> WHERE s);
      "while",           (fun s -> WHILE s);
      "write",           (fun s -> WRITE s);

(* for Compaq/Intel *)
      "automatic",       (fun s -> SIMPLE_ATTR s);
      "static",          (fun s -> SIMPLE_ATTR s);
(*      "options",         (fun s -> OPTIONS s);*)
      "accept",          (fun s -> ACCEPT s);
      "rewrite",         (fun s -> REWRITE s);
      "delete",          (fun s -> DELETE s);
      "unlock",          (fun s -> UNLOCK s);
(*      "definefile",      (fun s -> DEFINE_FILE s);*)
      "encode",          (fun s -> ENCODE s);
      "decode",          (fun s -> DECODE s);
      "find",            (fun s -> FIND s);
      "virtual",         (fun s -> VIRTUAL s);
      "structure",       (fun s -> STRUCTURE s);
      "endstructure",    (fun s -> END_STRUCTURE s);
      "record",          (fun s -> RECORD s);
      "union",           (fun s -> UNION s);
      "endunion",        (fun s -> END_UNION s);
      "map",             (fun s -> MAP s);
      "endmap",          (fun s -> END_MAP s);

(* for PGI CUDA *)
      "constant",        (fun s -> SIMPLE_ATTR s);
      "device",          (fun s -> SIMPLE_ATTR s);
      "managed",         (fun s -> SIMPLE_ATTR s);
      "pinned",          (fun s -> SIMPLE_ATTR s);
      "texture",         (fun s -> SIMPLE_ATTR s);
      "shared",          (fun s -> SIMPLE_ATTR s);
  ] in 
  let keyword_table = Hashtbl.create (List.length keyword_list) in
  let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
      keyword_list 
  in
  let _find s = (Hashtbl.find keyword_table (String.lowercase_ascii s)) s in
  _find

let find_keyword s = 
  try 
    _find_keyword s
  with 
    Not_found -> IDENTIFIER s




module F (Stat : Parser_aux.STATE_T) = struct

  module Loc = Ast.Loc
  module PA = Parser_aux
  module Aux = PA.F (Stat)
  module TokenF = Token.F (Stat)
  module LineStat = Parser_aux.LineStat
  module H = Labels.HeaderFile
  module PPD = Labels.PpDirective

  open Stat



  let is_free_source_form()  = env#current_source#is_free_source_form
  let is_fixed_source_form() = env#current_source#is_fixed_source_form

  let offsets_to_loc st ed = env#current_pos_mgr#offsets_to_loc st ed
      


  let normalize_continued_string =
    let pat = Str.regexp "&[ \009\012]*\n[ \009\012]*&" in
    let norm str =
      Str.global_replace pat "" str
    in
    norm
    
  let startswith_digit str =
    try
      match str.[0] with
      | '0'..'9' -> true
      | _ -> false
    with
      _ -> false

  let is_header_filename str =
    let len = String.length str in
    if len > 2 then
      match str.[0], str.[len-1] with
      | '"', '"' | '\'', '\'' | '<', '>' -> true
      | _ -> false
    else
      false


  let regexp line_terminator = ['\013' '\010'] | "\013\010"

  let regexp white_space = [' ' '\009' '\012']

  let is_white_space c = c = ' ' || c = '\009' || c = '\012'

  let is_white_space_c c = c = 32 || c = 9 || c = 12

  let regexp letter = ['a'-'z''A'-'Z']

  let regexp digit = ['0'-'9']

  let regexp hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

  let regexp alphanumeric_character = letter | digit | '_'

(*
  let regexp special_character = [' ' '=' '+' '-' '*' '/' '(' ')' ',' '.' '\'' ':' '!' '"' '%' '&' ';' '<' '>' '?' '$']
  let regexp character = alphanumeric_character | special_character
*)

  let regexp cont = '&' white_space* line_terminator white_space* '&'

  let regexp name = letter alphanumeric_character*

  let regexp sign = ['+' '-']
  let regexp digit_string = digit+

  let regexp b_d_str = ['0' '1'] ((['0' '1']|white_space)* ['0' '1'])?

  let regexp b_digit_str = cont? (b_d_str cont? b_d_str | ['0' '1']) cont?

  let regexp o_d_str = ['0'-'7'] ((['0'-'7']|white_space)* ['0'-'7'])?

  let regexp o_digit_str = cont? (o_d_str cont? o_d_str | ['0'-'7']) cont?

  let regexp z_d_str = hex_digit ((hex_digit|white_space)* hex_digit)?

  let regexp hex_digit_str = cont? (z_d_str cont? z_d_str | hex_digit) cont?

  let regexp scalar_int_constant_name = name
  let regexp kind_param = digit_string | scalar_int_constant_name
  let regexp signed_digit_string = sign? digit_string

  let regexp old_octal_constant = ['0'-'7']+ ['B' 'b']

  let regexp old_int_constant = old_octal_constant | digit_string? '#' hex_digit+

  let regexp int_literal_constant = digit_string ('_' kind_param)? | old_int_constant

  let regexp signed_int_literal_constant = sign? int_literal_constant


  let regexp b_quoted = '\'' b_digit_str '\'' | '"' b_digit_str '"'
  let regexp o_quoted = '\'' o_digit_str '\'' | '"' o_digit_str '"'
  let regexp hex_quoted = '\'' hex_digit_str '\'' | '"' hex_digit_str '"'

  let regexp binary_constant = ['b' 'B'] b_quoted | b_quoted ['b' 'B'] (* suffix form is non-standard *)
  let regexp octal_constant = ['o' 'O'] o_quoted | o_quoted ['o' 'O'] (* suffix form is non-standard *)
  let regexp hex_constant = ['z' 'Z' 'X' 'x'] hex_quoted | hex_quoted ['z' 'Z' 'X' 'x'] (* X and suffix form are non-standard *)

  let regexp boz_literal_constant = binary_constant | octal_constant | hex_constant


(*  let regexp significand = digit_string '.' digit_string? | '.' digit_string *)
  let regexp significand = digit_string '.' digit_string | '.' digit_string
  let regexp exponent_letter = ['e' 'E' 'd' 'D' 'q' 'Q'] (* Q for REAL(16) constants (Compaq/Intel) *)
  let regexp exponent = signed_digit_string

(*  let regexp real_literal_constant = significand (exponent_letter exponent)? ('_' kind_param)? | digit_string exponent_letter exponent ('_' kind_param)? *)
  let regexp real_literal_constant_no_kind = significand (exponent_letter exponent)? | digit_string ('.')? exponent_letter exponent
  let regexp real_literal_constant = real_literal_constant_no_kind ('_' kind_param)? | digit_string ('.') ('_' kind_param)

  let regexp signed_real_literal_constant = sign? real_literal_constant

  let regexp real_part = signed_int_literal_constant | signed_real_literal_constant
  let regexp imag_part = signed_int_literal_constant | signed_real_literal_constant


  let regexp complex_literal_constant = '(' real_part ',' imag_part ')'

  let regexp rep_char_non_double_quote = [^'"' '\013' '\010'] | "''" | "\"\""
  let regexp rep_char_non_single_quote = [^'\'' '\013' '\010'] | "''" | "\"\""
  let regexp char_literal_constant_no_kind = (('\'' rep_char_non_single_quote* '\'') | ('"' rep_char_non_double_quote* '"'))
  let regexp char_literal_constant = (kind_param '_')? char_literal_constant_no_kind

  let regexp char_start_single = (kind_param '_')? '\''
  let regexp char_start_double = (kind_param '_')? '"'


  let regexp true_constant = '.' ['t' 'T'] ['r' 'R'] ['u' 'U'] ['e' 'E'] '.' ('_' kind_param)?
  let regexp false_constant = '.' ['f' 'F'] ['a' 'A'] ['l' 'L'] ['s' 'S'] ['e' 'E'] '.' ('_' kind_param)? 

  let regexp logical_literal_constant = true_constant | false_constant

(*
  let regexp literal_constant = 
    int_literal_constant | 
    real_literal_constant | 
    complex_literal_constant | 
    logical_literal_constant | 
    char_literal_constant | 
    boz_literal_constant
*)

  let regexp named_constant = name

(*
  let regexp constant = literal_constant | named_constant

  let regexp int_constant = constant
  let regexp char_constant = constant
*)

  let regexp filename_character_dq = [^'\"']
  let regexp filename_character_sq = [^'\'']
  let regexp sys_filename_character = [^'>']



  let regexp line_concat = '\\' line_terminator


  let regexp not_star_not_slash = [^'*' '/'] | "\013\010"
  let regexp not_star = [^'*'] | "\013\010"


  let regexp pp_keyword = '#' white_space* name

  let regexp pp_out = '#' white_space+ digit_string white_space+ char_literal_constant_no_kind (white_space+ digit)?

  let regexp pp_identifier = '_'+ name

  let regexp kw_and = ['A' 'a'] ['N' 'n'] ['D' 'd']
  let regexp kw_or  = ['O' 'o'] ['R' 'r']
  let regexp kw_true  = ['T' 't'] ['R' 'r'] ['U' 'u'] ['E' 'e']
  let regexp kw_false  = ['F' 'f'] ['A' 'a'] ['L' 'l'] ['S' 's'] ['E' 'e']
  let regexp kw_eq_OR_eqv = ['E' 'e'] ['Q' 'q'] (['V' 'v'])?
  let regexp kw_ge_OR_gt = ['G' 'g'] (['E' 'e']|['T' 't'])
  let regexp kw_le_OR_lt = ['L' 'l'] (['E' 'e']|['T' 't'])
  let regexp kw_ne_OR_neqv_OR_not = ['N' 'n'] (['E' 'e'] (['Q' 'q'] ['V' 'v'])? |['O' 'o'] ['T' 't'])

  let regexp dotted_op = '.' (kw_and|kw_or|kw_true|kw_false|kw_eq_OR_eqv|kw_ge_OR_gt|kw_le_OR_lt|kw_ne_OR_neqv_OR_not) '.'

  let regexp dotted_identifier = '.' letter+ '.'


  let regexp iboz = ['I' 'B' 'O' 'Z' 'i' 'b' 'o' 'z']

  let regexp iboz_desc = iboz int_literal_constant ('.' int_literal_constant)?

  let regexp f_d_desc = ['F' 'f' 'D' 'd'] int_literal_constant '.' int_literal_constant

  let regexp e_en_es_g = ['E' 'e'] (['N' 'n'] | ['S' 's'])? | ['G' 'g']

  let regexp e_en_es_g_desc = e_en_es_g int_literal_constant '.' int_literal_constant (['E' 'e'] int_literal_constant)?

  let regexp l_desc = ['L' 'l'] int_literal_constant

  let regexp a_desc = ['A' 'a'] int_literal_constant?

  let regexp data_edit_desc = int_literal_constant? (iboz_desc | f_d_desc | e_en_es_g_desc | l_desc | a_desc)

  let regexp kP_desc = signed_digit_string ['P' 'p']

  let regexp position_edit_desc0 = ['T' 't'] (['L' 'l' 'R' 'r'])? int_literal_constant
  let regexp position_edit_desc1 = int_literal_constant ['X' 'x']

  let regexp cH_desc = digit_string ['H' 'h'] (* deleted in F95 *)


  let regexp keyword_or_name = (letter | '_')+

  let regexp kw_include = ['I' 'i'] ['N' 'n'] ['C' 'c'] ['L' 'l'] ['U' 'u'] ['D' 'd'] ['E' 'e']
(*
  let regexp include_line = kw_include white_space* char_literal_constant_no_kind white_space* line_terminator
*)
  let regexp ocl_head = ['O' 'o'] ['C' 'c'] ['L' 'l']

  let regexp omp_sentinel = ['O' 'o'] ['M' 'm'] ['P' 'p']

  let regexp acc_sentinel = ['A' 'a'] ['C' 'c'] ['C' 'c']

  let regexp xlf_trigger = ['I' 'i'] ['B' 'b'] ['M' 'm'] ['*' 'P' 'p' 'T' 't'] | ['S' 's'] ['M' 'm'] ['P' 'p'] '$' (* IBM *)

  let regexp at_process = '@' ['P' 'p'] ['R' 'r'] ['O' 'o'] ['C' 'c'] ['E' 'e'] ['S' 's'] ['S' 's'] (* IBM *)

  let regexp dec_prefix = ['D' 'd'] (['I' 'i'] ['R' 'r'] | ['E' 'e'] ['C' 'c']) '$' (* Intel *)

  let regexp pp_underscore = '_'+

  let regexp kw_options = ['O' 'o'] ['P' 'p'] ['T' 't'] ['I' 'i'] ['O' 'o'] ['N' 'n'] ['S' 's']

(*
  let get_prev_char ulexbuf =
    let buf = Ulexing.get_buf ulexbuf in
    let cur = Ulexing.get_pos ulexbuf in
    let prev_char = buf.(cur-2) in
    let cur_char = buf.(cur-1) in
    DEBUG_MSG "prev char: %c (cur: %c)" (Char.chr prev_char) (Char.chr cur_char);
    prev_char
*)    


  let mkloc ulexbuf =
    let st = Ulexing.lexeme_start ulexbuf in
    let ed = (Ulexing.lexeme_end ulexbuf) - 1 in
    let bloc = ulexbuf.Ulexing.base_loc in
    if bloc = Loc.dummy then
      offsets_to_loc st ed
    else
      Loc.make ~fname:bloc.Loc.filename
        (bloc.Loc.start_offset+st-1)
        (bloc.Loc.start_offset+ed-1)
        bloc.Loc.start_line
        (bloc.Loc.start_char+st-1)
        bloc.Loc.end_line
        (bloc.Loc.start_char+ed-1)

  let make_qtoken rt st_pos ed_pos =
    let ext = env#current_loc_layers_encoded in
    let qt = PB.make_qtoken ~cache:env#fname_ext_cache ~ext rt st_pos ed_pos in
    DEBUG_MSG "%s" (Token.qtoken_to_string qt);
    qt

  let get_last_char ulexbuf =
    let buf = Ulexing.get_buf ulexbuf in
    let st = Ulexing.get_start ulexbuf in
    Bytes.get buf (st-1)

  let is_letter_or_uscore c =
    match c with
    | 'a'..'z' | 'A'..'Z' | '_' -> true
    | _ -> false

  let get_last_int_literal s =
    let len = String.length s in
    let num = ref "" in
    begin
      try
        for i = len - 1 downto 0 do
          let c = s.[i] in
          match c with
          | '0'..'9' -> num := Printf.sprintf "%c%s" c !num
          | _ -> 
              if is_letter_or_uscore c then
                raise Not_found
              else
                raise Exit
        done
      with
      | Exit -> ()
    end;
    int_of_string !num



  let mklabel lab ulexbuf =
    let loc = mkloc ulexbuf in
    (lab, loc)

  let merge_label (lab0, loc0) (lab1, loc1) =
    (lab0^lab1, PB.merge_locs ~cache:(Some env#fname_ext_cache) loc0 loc1)

  let register_label (lab, loc) =
    if lab <> "" then
      env#register_label 
        loc.Loc.filename
        loc.Loc.start_line 
        (Aux.normalize_label lab, loc)


  type margin_stat = { mutable ms_in_margin  : bool;
                       mutable ms_open_paren : bool;
                       mutable ms_open_char  : PA.char_context;
                     }

  type comment_type =
    | C_fixed (* C, *, or D *)
    | C_free  (* ! *)

  let comment_type_to_string = function
    | C_fixed -> "FIXED_COMMENT"
    | C_free  -> "FREE_COMMENT"


  exception Sep_found of string


  class guess_env = object (self)

    val mutable lnum    = 1
    val mutable pos     = 1
    val mutable max_pos = 1

    val mutable stmt = ""
    val mutable last_stmt = ""

    val mutable second_last_nonblank_char_within_limit = '\000'
    val mutable last_nonblank_char_within_limit = '\000'

    val stmt_sep_count_tbl   = Hashtbl.create 0
    val margin_sep_count_tbl = Hashtbl.create 0

    val mutable effective_line_count    = 0
    val mutable exclam_comment_count    = 0
    val mutable fixed_comment_count     = 0
    val mutable long_line_count         = 0
    val mutable marginal_amp_count      = 0
    val mutable amp_count               = 0
    val mutable noncomment_margin_count = 0
    val mutable letter_cont_field_count = 0
    val mutable free_cont_count    = 0
    val mutable incomplete_line_count   = 0

    val mutable marginal_complete_free_cont_count = 0

    val mutable paren_level = 0
    val mutable _paren_level = ""

    val mutable pp_branch_level = 0
    val pp_branch_stack = Stack.create()

    val mutable blank_line_flag = true

    val mutable char_context = PA.CH_NONE

    val margin_stat = { ms_in_margin  = false; 
                        ms_open_paren = false; 
                        ms_open_char  = PA.CH_NONE;
                      }
    val mutable last_in_margin = false
    val mutable margin_paren_level = 0
    val mutable _margin_paren_level = ""
    val mutable margin_char_context = PA.CH_NONE

    val mutable letter_cont_field_flag = false

    val mutable free_cont_flag = false




    method free_cont_flag = free_cont_flag
    method set_free_cont_flag = free_cont_flag <- true
    method clear_free_cont_flag = free_cont_flag <- false


    method char_context = char_context

    method in_char = 
      match char_context with
      | PA.CH_NONE -> false
      | _ -> true

    method marginal_complete_free_cont_count = marginal_complete_free_cont_count
    method incr_marginal_complete_free_cont_count =
      marginal_complete_free_cont_count <- marginal_complete_free_cont_count + 1

    method free_cont_count = free_cont_count
    method incr_free_cont_count =
      free_cont_count <- free_cont_count + 1


    method letter_cont_field_count = letter_cont_field_count
    method incr_letter_cont_field_count = 
      letter_cont_field_count <- letter_cont_field_count + 1

    method letter_cont_field = letter_cont_field_flag
    method set_letter_cont_field = letter_cont_field_flag <- true
    method clear_letter_cont_field = letter_cont_field_flag <- false


    method noncomment_margin_count = noncomment_margin_count

    method clear_last_in_margin =
      if last_in_margin then begin
        DEBUG_MSG "cleared";
        last_in_margin <- false
      end

    method reset_paren_level =
      DEBUG_MSG "called";
      paren_level <- 0;
      _paren_level <- ""

    method reset_margin_paren_level =
      DEBUG_MSG "called";
      margin_paren_level <- 0;
      _margin_paren_level <- ""

    method incr_paren_level =
      let lv = paren_level + 1 in
      let _lv = _paren_level^"(" in
      DEBUG_MSG "%s:%d -> %s:%d" _paren_level paren_level _lv lv;
      paren_level <- lv;
      _paren_level <- _lv

    method incr_margin_paren_level =
      let lv = margin_paren_level + 1 in
      let _lv = _margin_paren_level^"(" in
      DEBUG_MSG "%s:%d -> %s:%d" _margin_paren_level margin_paren_level _lv lv;
      margin_paren_level <- lv;
      _margin_paren_level <- _lv

    method decr_paren_level =
      let lv = paren_level - 1 in
      let _lv = _paren_level^")" in
      DEBUG_MSG "%s:%d -> %s:%d" _paren_level paren_level _lv lv;
      paren_level <- lv;
      _paren_level <- _lv

    method decr_margin_paren_level =
      let lv = margin_paren_level - 1 in
      let _lv = _margin_paren_level^")" in
      DEBUG_MSG "%s:%d -> %s:%d" _margin_paren_level margin_paren_level _lv lv;
      margin_paren_level <- lv;
      _margin_paren_level <- _lv

    method check_paren_level =
      let len0 = String.length _paren_level in
      let s = _paren_level ^ _margin_paren_level in
      DEBUG_MSG "_paren_level=%s" _paren_level;
      DEBUG_MSG "_margin_paren_level=%s" _margin_paren_level;
      let b =
        if _paren_level = "" then
          true
        else
          let lv = ref 0 in
          try
            String.iteri
              (fun i c ->
                begin
                  match c with
                  | '(' -> incr lv
                  | ')' -> decr lv
                  | _ -> ()
                end;
                if i >= len0 && !lv = 0 then
                  raise Exit
              ) s;
            false
          with
            Exit -> true
      in
      DEBUG_MSG "%B" b;
      b

    method margin_stat = margin_stat

    method in_margin =
      margin_stat.ms_in_margin

    method enter_margin =
      DEBUG_MSG "open_paren=%B" self#in_paren;
      DEBUG_MSG "char_context=%s" (PA.char_context_to_string char_context);
      margin_stat.ms_in_margin <- true;
      margin_stat.ms_open_paren <- self#in_paren;
      margin_stat.ms_open_char <- char_context

    method exit_margin =
      if margin_stat.ms_in_margin then begin
        DEBUG_MSG "exiting";
        if margin_stat.ms_open_paren && (paren_level + margin_paren_level) = 0 then begin
          DEBUG_MSG "paren closed in the margin"
        end;
        if 
          match margin_stat.ms_open_char with
          | PA.CH_NONE -> false
          | PA.CH_SINGLE as cc -> char_context = cc && margin_char_context = cc
          | PA.CH_DOUBLE as cc -> char_context = cc && margin_char_context = cc
        then begin
          DEBUG_MSG "character context closed in the margin"
        end;
        margin_stat.ms_in_margin <- false;
        margin_stat.ms_open_paren <- false;
        margin_stat.ms_open_char <- PA.CH_NONE;
        DEBUG_MSG "margin_stat cleared";
        last_in_margin <- true;
        DEBUG_MSG "last_in_margin -> true";
      end

    method enter_char cc = 
      assert (cc <> PA.CH_NONE);
      if margin_stat.ms_in_margin then begin
        let cc' = 
          if margin_char_context = PA.CH_NONE then
            cc
          else
            PA.CH_NONE
        in
        DEBUG_MSG "char_context (in margin): %s -> %s" 
          (PA.char_context_to_string margin_char_context)
          (PA.char_context_to_string cc');
        margin_char_context <- cc'
      end
      else begin
        let cc' = 
          if char_context = PA.CH_NONE then
            cc
          else
            PA.CH_NONE
        in
        DEBUG_MSG "char_context: %s -> %s"
          (PA.char_context_to_string char_context)
          (PA.char_context_to_string cc');
        char_context <- cc'
      end

    method exit_char cc = 
      if margin_stat.ms_in_margin then begin
        let cc' = 
          if margin_char_context = PA.CH_NONE then
            cc
          else
            PA.CH_NONE
        in
        DEBUG_MSG "char_context (in margin): %s -> %s"
          (PA.char_context_to_string margin_char_context)
          (PA.char_context_to_string cc');
        margin_char_context <- cc'
      end
      else begin
        let cc' = 
          if char_context = PA.CH_NONE then
            cc
          else
            PA.CH_NONE
        in
        DEBUG_MSG "char_context: %s -> %s" 
          (PA.char_context_to_string char_context)
          (PA.char_context_to_string cc');
        char_context <- cc'
      end


    method in_paren = 
      let b = paren_level > 0 in
      DEBUG_MSG "%B (lv=%d)" b paren_level;
      b
      
    method enter_paren =
      if margin_stat.ms_in_margin then
        self#incr_margin_paren_level
      else
        self#incr_paren_level

    method exit_paren =
      if margin_stat.ms_in_margin then
        self#decr_margin_paren_level
      else
        self#decr_paren_level

    method check_cont =
      let may_be_incomplete_line =
        match last_nonblank_char_within_limit with
        | ',' | '*' -> begin
            DEBUG_MSG "possible incomplete line: the previous line ends with %C" 
              last_nonblank_char_within_limit;
            true
        end
        | '/' -> begin
            if second_last_nonblank_char_within_limit = '/' then begin
              DEBUG_MSG "possible incomplete line: the previous line ends with //";
              true
            end
            else
              false
        end
        | _ -> false
      in
      self#reset_last_nonblank_char_within_limit;
      may_be_incomplete_line


    method check_at_initial_line =
      DEBUG_MSG "paren_level=%d last_in_margin=%B margin_paren_level=%d sep_in_margin=%B"
        paren_level last_in_margin margin_paren_level self#sep_in_margin;

      let may_be_incomplete_line = ref self#check_cont in
      let may_be_comment_margin = ref false in

      let check_sep() =
        let is_noncomment_margin, sep =
          try
            self#iter_stmt_sep_count
              (fun sep count -> 
                DEBUG_MSG "sep count: \"%s\" -> %d (thresh=%d)" 
                  sep count sep_count_thresh;
                if count >= sep_count_thresh then begin
                  try
                    let _ = self#get_margin_sep_count sep in
                    raise (Sep_found sep)
                  with
                    Not_found -> ()
                end
              );
            false, ""
          with
            Sep_found s -> true, s
        in
        if is_noncomment_margin then begin
          DEBUG_MSG "non-comment margin found (freq of \"%s\")" sep;
          self#incr_noncomment_margin_count;
          may_be_incomplete_line := false
        end
      in (* check_sep *)

      if paren_level = 0 then begin

        if margin_paren_level = 0 then begin

          if char_context <> PA.CH_NONE then begin
            if margin_char_context = char_context then begin
              DEBUG_MSG "non-comment margin found (char)";
              self#incr_noncomment_margin_count;
              may_be_incomplete_line := false
            end;
            margin_char_context <- PA.CH_NONE;
            char_context <- PA.CH_NONE
          end
          else if margin_char_context <> PA.CH_NONE then begin
            DEBUG_MSG "comment margin?";
            may_be_comment_margin := true;
            margin_char_context <- PA.CH_NONE
          end
          else if self#sep_in_margin then begin
            check_sep()
          end
          else
            ()

        end
        else begin (* margin_paren_level <> 0 *)
          DEBUG_MSG "comment margin?";
          may_be_comment_margin := true;
          (*self#reset_margin_paren_level*)
        end;
        (*self#reset_paren_level*)
      end
      else begin (* paren_level <> 0 *)

        if paren_level + margin_paren_level = 0 then begin
          DEBUG_MSG "non-comment margin found (paren)";
          self#incr_noncomment_margin_count;
          may_be_incomplete_line := false
        end
        else begin
          if self#sep_in_margin then begin
            check_sep()
          end
        end;

        if last_in_margin && not !may_be_comment_margin && not self#check_paren_level then begin
          DEBUG_MSG "margin contains non-comment characters and they never close parentheses";
          self#incr_marginal_complete_free_cont_count
        end;

        (*self#reset_paren_level;
        self#reset_margin_paren_level;*)
        margin_char_context <- PA.CH_NONE;

      end;

      self#reset_sep_count;

      if !may_be_incomplete_line then begin
        DEBUG_MSG "possible incomplete line found";
        if not !may_be_comment_margin then begin
          DEBUG_MSG "possible non-comment margin found";
          self#incr_noncomment_margin_count
        end;
        self#incr_incomplete_line_count
      end


    method pp_branch_level = pp_branch_level

    method current_pp_branch =
      Stack.top pp_branch_stack

    method enter_pp_branch ?(if0=false) () =
      DEBUG_MSG "lv: %d -> %d" pp_branch_level (pp_branch_level+1);
      pp_branch_level <- pp_branch_level + 1;
      let section_stack = Stack.create() in
      if not if0 then
        Stack.push (ref false) section_stack;
      Stack.push section_stack pp_branch_stack

    method exit_pp_branch () = 
      DEBUG_MSG "lv: %d -> %d" pp_branch_level (pp_branch_level-1);
      pp_branch_level <- pp_branch_level - 1;
      let section_stack = Stack.pop pp_branch_stack in
      let is_free_form = ref true in
      if Stack.is_empty section_stack then
        false
      else begin
        Stack.iter 
          (fun b -> is_free_form := !is_free_form && !b) 
          section_stack;
        DEBUG_MSG "is_free_form=%B" !is_free_form;
        !is_free_form
      end

    method in_pp_branch = pp_branch_level > 0

    method enter_pp_section () =
      DEBUG_MSG "lv: %d" pp_branch_level;
      Stack.push (ref false) self#current_pp_branch

    method set_current_pp_section =
      let section_stack = self#current_pp_branch in
      if not (Stack.is_empty section_stack) then
        (Stack.top section_stack) := true


    method lnum = lnum
    method pos = pos
    method max_pos = max_pos

    method stmt = stmt
    method add_to_stmt s = stmt <- stmt^s
    method reset_stmt = 
      last_stmt <- stmt;
      stmt <- "";
      self#clear_free_cont_flag

    method stmt_is_blank =
      try
        String.iter (fun c -> if c <> ' ' && c <> '\t' then raise Exit) stmt;
        true
      with
        Exit -> false

    method last_stmt = last_stmt

    method effective_line_count = effective_line_count
    method exclam_comment_count = exclam_comment_count
    method fixed_comment_count = fixed_comment_count
    method long_line_count = long_line_count
    method incomplete_line_count = incomplete_line_count

    method is_blank_line = blank_line_flag

    method add_to_lnum n = lnum <- lnum + n

    method add_to_pos n = 
      pos <- pos + n;
      if pos > max_pos then
        max_pos <- pos

    method set_pos n = pos <- n
    method reset_pos = pos <- 1



    method last_nonblank_char_within_limit =
      last_nonblank_char_within_limit

    method second_last_nonblank_char_within_limit =
      second_last_nonblank_char_within_limit

    method reset_last_nonblank_char_within_limit =
      last_nonblank_char_within_limit <- '\000';
      second_last_nonblank_char_within_limit <- '\000'

    method set_last_nonblank_char_within_limit s =
      last_nonblank_char_within_limit <- s

    method set_second_last_nonblank_char_within_limit s =
      second_last_nonblank_char_within_limit <- s

    method private sep_in_xxx tbl sep =
      try
        let c = Hashtbl.find tbl sep in
        Hashtbl.replace tbl sep (c+1)
      with
        Not_found -> Hashtbl.add tbl sep 1

    method add_sep sep =
      if self#in_margin then begin
        DEBUG_MSG "%s (in margin)" sep;
        self#sep_in_xxx margin_sep_count_tbl sep
      end
      else begin
        DEBUG_MSG "%s" sep;
        self#sep_in_xxx stmt_sep_count_tbl sep
      end

    method iter_stmt_sep_count f = 
      Hashtbl.iter f stmt_sep_count_tbl

    method get_margin_sep_count = Hashtbl.find margin_sep_count_tbl

    method sep_in_margin = (Hashtbl.length margin_sep_count_tbl) > 0

    method reset_sep_count = 
      Hashtbl.clear stmt_sep_count_tbl;
      Hashtbl.clear margin_sep_count_tbl

    method incr_effective_line_count = 
      effective_line_count <- effective_line_count + 1

    method incr_exclam_comment_count = 
      exclam_comment_count <- exclam_comment_count + 1

    method incr_fixed_comment_count = 
      fixed_comment_count <- fixed_comment_count + 1
    method incr_long_line_count = 
      long_line_count <- long_line_count + 1
    method incr_marginal_amp_count = 
      marginal_amp_count <- marginal_amp_count + 1
    method incr_amp_count = 
      amp_count <- amp_count + 1
    method incr_incomplete_line_count = 
      incomplete_line_count <- incomplete_line_count + 1
    method incr_noncomment_margin_count = 
      noncomment_margin_count <- noncomment_margin_count + 1

    method set_blank_line_flag = blank_line_flag <- true
    method clear_blank_line_flag = blank_line_flag <- false

    method is_false_fixed_source_form =
      let b =
        marginal_complete_free_cont_count > 0 ||
        (fixed_comment_count = 0) && 
        (
         (exclam_comment_count > 0 &&
          amp_count > 0 &&
          (marginal_amp_count = amp_count || marginal_amp_count + free_cont_count = amp_count)
         ) ||
         (letter_cont_field_count > 0)
        )
      in
      DEBUG_MSG "%B" b;
      b

    method to_string =
      Printf.sprintf ("\n"^^
                      "lnum:%d pos:%d\n"^^
                      "marginal complete free conts: %d\n"^^
                      "effective lines      : %d\n"^^
                      "!comments            : %d\n"^^
                      "fixed form comments  : %d\n"^^
                      "long lines           : %d\n"^^
                      "marginal &s          : %d\n"^^
                      "&s                   : %d\n"^^
                      "non-comment margins  : %d\n"^^
                      "letter cont fields   : %d\n"^^
                      "free form conts      : %d\n"^^
                      "incomplete lines     : %d\n"^^
                      "paren level        : %d\n"^^
                      "margin paren level : %d\n"^^
                      "pp branch level    : %d\n"^^
                      "char context       : %s\n"^^
                      "margin char context: %s\n"^^
                      "blank_line_flag    : %B\n"^^
                      "free_cont_flag     : %B"
                     ) 
        lnum pos
        marginal_complete_free_cont_count
        effective_line_count exclam_comment_count fixed_comment_count long_line_count
        marginal_amp_count amp_count noncomment_margin_count
        letter_cont_field_count free_cont_count incomplete_line_count
        paren_level margin_paren_level pp_branch_level
        (PA.char_context_to_string char_context)
        (PA.char_context_to_string margin_char_context)
        blank_line_flag free_cont_flag

  end (* of class guess_env *)


  let check_pos ?(is_blank=false) genv =
    let max_line_length = env#current_source#max_line_length in
    if not genv#in_margin && genv#pos > max_line_length then begin
      DEBUG_MSG "entering margin (pos=%d)" genv#pos;
      let stmt_len = String.length genv#stmt in
      if genv#in_char then begin
        let c = genv#stmt.[stmt_len - 1] in (* should be double or single quote *)
        genv#set_last_nonblank_char_within_limit c;
        if stmt_len - 2 >= 0 then
          genv#set_second_last_nonblank_char_within_limit genv#stmt.[stmt_len - 2]
      end
      else begin (* not in char context *)
        let rec find i =
          if i < 0 then
            (-1, '\000')
          else
            let c = genv#stmt.[i] in
            if is_white_space c then
              find (i - 1)
            else
              (i, c)
        in
        let i1, c1 = find (stmt_len - 1) in
        genv#set_last_nonblank_char_within_limit c1;
        let i2, c2 = find (i1 - 1) in
        if i2 >= 0 then
          genv#set_second_last_nonblank_char_within_limit c2

      end;

      DEBUG_MSG "is_blank=%B" is_blank;
      if not is_blank then
        genv#incr_long_line_count;

      DEBUG_MSG "line reaches the margin: stmt=^%s$ pos=%d" genv#stmt genv#pos;
      DEBUG_MSG "last_nonblank_char_within_limit=%C (max_line_length=%d)"
        genv#last_nonblank_char_within_limit max_line_length;
      DEBUG_MSG "second_last_nonblank_char_within_limit=%C"
        genv#second_last_nonblank_char_within_limit;

      genv#enter_margin

    end

  let head_keywords =
    let l = [
      "function";
      "module";
      "subroutine";
      "integer";
      "real";
      "double";
      "logical";
      "character";
      "complex";
      "type";
      "implicit";
    ] in
    let s = Xset.create (List.length l) in
    List.iter (Xset.add s) l;
    s

  let rec scan_label_field genv form lexbuf = 
    let thresh = env#effective_lines_for_source_form_guess in

    DEBUG_MSG "thresh=%d" thresh;

    if genv#pos = 1 then begin
      DEBUG_MSG "form=%s\n%s" (SF.to_string form) genv#to_string;
      DEBUG_MSG "thresh=%d" thresh
    end;

    if thresh > 0 && genv#effective_line_count >= thresh then
      form
    else
      _scan_label_field genv form lexbuf

  and possibly_free ?(head_symbol="") genv form lexbuf =
    if genv#in_pp_branch then begin
      genv#set_current_pp_section;
      if head_symbol <> "" then begin
        let rest = scan_name genv lexbuf in
        if rest <> "" then begin
          let s = String.lowercase_ascii (head_symbol^rest) in
          DEBUG_MSG "head_name: %s" s;
          if Xset.mem head_keywords s then
            SF.Free
          else
            skip_line genv form lexbuf
        end
        else
          skip_line genv form lexbuf
      end
      else begin
        skip_line genv form lexbuf
      end
    end
    else
      SF.Free
 
  and _scan_label_field (genv : guess_env) form = lexer

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /*";
    scan_block_comment_label genv form lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /**/";
    if genv#pos = 5 then begin
      genv#set_pos 6;
      scan_continuation_field genv form lexbuf
    end
    else begin
      genv#add_to_pos 1;
      scan_label_field genv form lexbuf
    end

|   ['C' 'c' '*' 'D' 'd'] -> (* D and d are non-standard *)
    let s = Ulexing.utf8_lexeme lexbuf in
    if genv#pos = 1 then begin
      DEBUG_MSG "COMMENT (%s) [%dL]" s genv#lnum;
      genv#incr_fixed_comment_count;
      genv#incr_effective_line_count;
      scan_comment C_fixed genv form lexbuf
    end
    else begin
      possibly_free ~head_symbol:s genv form lexbuf
    end

|   white_space -> 
    DEBUG_MSG "WHITE SPACE [%dL]" genv#lnum;

    let s = Ulexing.utf8_lexeme lexbuf in
    if s = "\t" then begin
      DEBUG_MSG "TAB found in label field";
      genv#add_to_pos 1;
      scan_tab_label_field genv form lexbuf
    end
    else
      if genv#pos = 5 then begin
        genv#set_pos 6;
        scan_continuation_field genv form lexbuf
      end
      else begin
        genv#add_to_pos 1;
        scan_label_field genv form lexbuf
      end

|   digit_string -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "DIGIT STRING (%s) [%dL]" s genv#lnum;
    let len = Ulexing.lexeme_length lexbuf in
    let n = len + genv#pos - 1 in
    if n < 5 then begin
      genv#add_to_pos len;
      genv#incr_effective_line_count;
      genv#clear_blank_line_flag;
      scan_label_field genv form lexbuf
    end
    else if n = 5 then begin
      genv#set_pos 6;
      genv#incr_effective_line_count;
      scan_continuation_field genv form lexbuf
    end
    else begin
      possibly_free ~head_symbol:s genv form lexbuf
    end

|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%dL]" genv#lnum;
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#set_blank_line_flag;
    scan_label_field genv form lexbuf

|   eof -> 
    DEBUG_MSG "EOF"; 
    form

|   '!' ->
    DEBUG_MSG "COMMENT (%s) [%dL]" (Ulexing.utf8_lexeme lexbuf) genv#lnum;
    if genv#pos <> 6 then
      genv#incr_exclam_comment_count;
    scan_comment C_free genv form lexbuf

|   '#' ['i' 'I'] ['f' 'F'] white_space+ '0' ->
    DEBUG_MSG "PP BRANCH (%s) [%dL]" (Ulexing.utf8_lexeme lexbuf) genv#lnum;
    genv#enter_pp_branch ~if0:true ();
    scan_pp_directive genv form lexbuf

|   pp_keyword ->
    let kwd = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "PP DIRECTIVE (%s) [%dL]" kwd genv#lnum;
    begin
      match find_pp_keyword kwd with
      | PP_IF | PP_IFDEF | PP_IFNDEF -> begin
          genv#enter_pp_branch();
          scan_pp_directive genv form lexbuf
      end
      | PP_ELSE | PP_ELIF -> begin
          genv#enter_pp_section();
          scan_pp_directive genv form lexbuf
      end
      | PP_ENDIF -> begin
          if genv#exit_pp_branch() then
            possibly_free genv form lexbuf
          else
            scan_pp_directive genv form lexbuf
      end
      | _ -> scan_pp_directive genv form lexbuf
    end

|   '#' | '@' | '%' ->
    DEBUG_MSG "DIRECTIVE (%s) [%dL]" (Ulexing.utf8_lexeme lexbuf) genv#lnum;
    scan_pp_directive genv form lexbuf

|   _ -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "OTHER (%s) [%dL]" s genv#lnum;
    possibly_free ~head_symbol:s genv form lexbuf

  and skip_line genv form = lexer
|   '\\' white_space* line_terminator -> 
    genv#add_to_lnum 1;
    genv#reset_pos;
    scan_label_field genv form lexbuf

|   line_terminator -> 
    genv#add_to_lnum 1;
    genv#reset_pos;
    scan_label_field genv form lexbuf

|   eof -> form

|   _ -> skip_line genv form lexbuf


  and scan_block_comment_label genv form = lexer
|   "*/" -> 
    if genv#pos = 5 then begin
      genv#set_pos 6;
      scan_continuation_field genv form lexbuf
    end
    else begin
      genv#add_to_pos 1;
      scan_label_field genv form lexbuf
    end

|   _ -> scan_block_comment_label genv form lexbuf


  and scan_tab_label_field genv form = lexer
|   ['1'-'9'] -> 
    DEBUG_MSG "CONTINUATION! (%s) [%dL]" (Ulexing.utf8_lexeme lexbuf) genv#lnum;
    genv#reset_last_nonblank_char_within_limit;
    genv#add_to_pos 1;
    begin
      match genv#char_context with
      | PA.CH_NONE -> scan_stmt ~is_head:true genv form lexbuf
      | PA.CH_SINGLE -> scan_char_single genv form lexbuf
      | PA.CH_DOUBLE -> scan_char_double genv form lexbuf
    end

|   line_terminator ->
    DEBUG_MSG "LINE TERMINATOR [%dL]" genv#lnum;
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#set_blank_line_flag;
    scan_label_field genv form lexbuf

|   eof -> form

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/*)";
    scan_block_comment_tab genv form lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/**/)";
    genv#check_at_initial_line;
    genv#add_to_pos 1;
    genv#reset_stmt;
    scan_stmt ~is_head:true genv form lexbuf

|   _ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "INITIAL LINE (%s) [%dL]" s genv#lnum;
    if genv#free_cont_flag then begin
      DEBUG_MSG "last line ends with '&' and no digit after the first tab ('%s')" s;
      SF.Free
    end
    else begin
      genv#check_at_initial_line;
      genv#add_to_pos 1;
      genv#reset_stmt;
      genv#add_to_stmt s;
      scan_stmt ~is_head:true genv form lexbuf
    end

  and scan_block_comment_tab genv form = lexer
|   "*/" -> 
    genv#check_at_initial_line;
    genv#add_to_pos 1;
    genv#reset_stmt;
    scan_stmt ~is_head:true genv form lexbuf

|   _ -> scan_block_comment_tab genv form lexbuf


  and scan_continuation_field genv form = lexer

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/*)";
    scan_block_comment_cont genv form lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/**/)";
    genv#check_at_initial_line;
    genv#add_to_pos 1;
    genv#reset_stmt;
    scan_stmt ~is_head:true genv form lexbuf

|   '0' | white_space -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "INITIAL LINE (%s) [%dL]" s genv#lnum;
    if genv#free_cont_flag then begin
      DEBUG_MSG "last line ends with '&' and continuation field is '%s'" s;
      SF.Free
    end
    else begin
      genv#check_at_initial_line;
      genv#add_to_pos 1;
      genv#reset_stmt;
      scan_stmt ~is_head:true genv form lexbuf
    end

|   eof -> form

|   line_terminator ->
    genv#add_to_lnum 1;
    DEBUG_MSG "CONTINUATION! (LINE TERMINATOR) [%dL]" genv#lnum;
    if genv#free_cont_flag then begin
      DEBUG_MSG "last line ends with '&' and continuation field is LINE_TERMINATOR";
      SF.Free
    end
    else begin
      genv#reset_last_nonblank_char_within_limit;
      genv#add_to_lnum 1;
      genv#reset_pos;
      begin
        match genv#char_context with
        | PA.CH_NONE -> scan_stmt ~is_head:true genv form lexbuf
        | PA.CH_SINGLE -> scan_char_single genv form lexbuf
        | PA.CH_DOUBLE -> scan_char_double genv form lexbuf
      end
    end

|   letter ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "CONTINUATION! (%s) [%dL]" s genv#lnum;
    if genv#free_cont_flag then begin
      DEBUG_MSG "last line ends with '&' and continuation field is '%s'" s;
      SF.Free
    end
    else begin
      genv#reset_last_nonblank_char_within_limit;
      genv#add_to_pos 1;
      genv#set_letter_cont_field;
      begin
        match genv#char_context with
        | PA.CH_NONE -> scan_stmt ~is_head:true genv form lexbuf
        | PA.CH_SINGLE -> scan_char_single genv form lexbuf
        | PA.CH_DOUBLE -> scan_char_double genv form lexbuf
      end
    end

|   _ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "CONTINUATION! (%s) [%dL]" s genv#lnum;
    DEBUG_MSG "stmt=%s" genv#stmt;
    if genv#free_cont_flag && s <> "&" then begin
      DEBUG_MSG "last line ends with '&' and continuation field is '%s'" s;
      SF.Free
    end
    else begin
      genv#clear_free_cont_flag;
      genv#reset_last_nonblank_char_within_limit;
      genv#add_to_pos 1;
      begin
        match genv#char_context with
        | PA.CH_NONE -> scan_stmt ~is_head:true genv form lexbuf
        | PA.CH_SINGLE -> scan_char_single genv form lexbuf
        | PA.CH_DOUBLE -> scan_char_double genv form lexbuf
      end
    end

  and scan_block_comment_cont genv form = lexer
|   "*/" -> 
    genv#check_at_initial_line;
    genv#add_to_pos 1;
    genv#reset_stmt;
    scan_stmt ~is_head:true genv form lexbuf

|   _ -> scan_block_comment_cont genv form lexbuf


  and scan_stmt ?(is_head=false) genv form = lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR";
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_letter_cont_field;
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#set_blank_line_flag;
    genv#exit_margin;
    scan_label_field genv form lexbuf

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /*";
    scan_block_comment_stmt genv form lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /**/";
    genv#clear_letter_cont_field;
    genv#add_to_pos 1;
    genv#add_to_stmt " ";
    scan_stmt genv form lexbuf

|   white_space ->
    genv#clear_letter_cont_field;
    check_pos ~is_blank:true genv;
    genv#add_to_pos 1;
    genv#add_to_stmt (Ulexing.utf8_lexeme lexbuf);
    scan_stmt genv form lexbuf

|   ';' ->
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    check_pos genv;
    if genv#stmt_is_blank then begin (* *)
      DEBUG_MSG "stmt begins with ';'";
      SF.Free
    end
    else begin
      genv#add_to_stmt ";";
      genv#add_to_pos 1;
      scan_stmt genv form lexbuf
    end

|   '!' ->
    genv#clear_letter_cont_field;
    DEBUG_MSG "COMMENT (%s) [%dL]" (Ulexing.utf8_lexeme lexbuf) genv#lnum;
    genv#incr_exclam_comment_count;
    scan_comment C_free genv form lexbuf

|   '&' ->
    genv#clear_letter_cont_field;
    let max_line_length = env#current_source#max_line_length in
    if genv#pos <= max_line_length then begin
      DEBUG_MSG "'&' found at pos %d (<= max_line_length(%d))" genv#pos max_line_length;
      genv#incr_free_cont_count;
      genv#set_free_cont_flag
    end
    else begin
      DEBUG_MSG "'&' found at pos %d (> max_line_length(%d))" genv#pos max_line_length;
      genv#incr_marginal_amp_count
    end;
    genv#incr_amp_count;
    genv#add_to_pos 1;
    scan_stmt genv form lexbuf

|   char_start_single -> 
    DEBUG_MSG "CHAR_START(SINGLE QUOTE) [%dL] pos=%d" genv#lnum genv#pos;
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    genv#add_to_pos (Ulexing.lexeme_length lexbuf);
    genv#add_to_stmt (Ulexing.utf8_lexeme lexbuf);
    genv#enter_char PA.CH_SINGLE;
    scan_char_single genv form lexbuf

|   char_start_double -> 
    DEBUG_MSG "CHAR_START(DOUBLE QUOTE) [%dL] pos=%d" genv#lnum genv#pos;
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    genv#add_to_pos (Ulexing.lexeme_length lexbuf);
    genv#add_to_stmt (Ulexing.utf8_lexeme lexbuf);
    genv#enter_char PA.CH_DOUBLE;
    scan_char_double genv form lexbuf

|   cH_desc -> 
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    let cH = Ulexing.utf8_lexeme lexbuf in
    let len = Ulexing.lexeme_length lexbuf in
    check_pos genv;
    genv#add_to_stmt cH;
    let is_hollerith =
      try
        let c = get_last_char lexbuf in
        DEBUG_MSG "last_char: '%c'" c;
        not (is_letter_or_uscore c)
      with
        _ -> true
    in
    if is_hollerith then begin
      DEBUG_MSG "H_DESC(%s)" cH;
      let n_str = Xstring.rstrip ~strs:["H"; "h"] cH in
      try
        let n = int_of_string n_str in
        DEBUG_MSG "n=%d" n;
        if n < 1 then
	  invalid_arg "cH_desc"
        else
	  scan_hollerith genv form n 1 lexbuf
      with
      | Failure _ | Invalid_argument _ ->
          genv#add_to_pos len;
          scan_stmt genv form lexbuf
    end
    else begin
      genv#add_to_pos len;
      scan_stmt genv form lexbuf
    end

|   eof -> form

|   letter ->
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    let s = Ulexing.utf8_lexeme lexbuf in
    let hollerith_num =
      if (s = "h" || s = "H") && genv#stmt <> "" && is_head then begin
        let stmt = genv#stmt in
        DEBUG_MSG "stmt: '%s'" stmt;
        try
          let n = get_last_int_literal stmt in
          DEBUG_MSG "last_int_literal: %d" n;
          if n > 0 then
            Some n
          else
            None
        with
          _ -> None
      end
      else
        None
    in
    check_pos genv;
    genv#add_to_stmt s;
    begin 
      match hollerith_num with
      | Some n -> begin
          DEBUG_MSG "hollerith: %dH" n;
          scan_hollerith genv form n 1 lexbuf
      end
      | None -> begin
          if genv#letter_cont_field then begin
            DEBUG_MSG "a word starting from continuation field found";
            genv#clear_letter_cont_field;
            genv#incr_letter_cont_field_count
          end;
          genv#add_to_pos 1;
          scan_stmt genv form lexbuf
      end
    end

|   digit -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    if genv#pos > 6 && genv#stmt_is_blank then begin
      DEBUG_MSG "stmt starting with digit (\"%s\") at pos=%d found" s genv#pos;
      possibly_free genv form lexbuf
    end
    else begin
      if is_head then begin
        genv#reset_paren_level;
        genv#reset_margin_paren_level;
      end;
      genv#clear_last_in_margin;
      genv#clear_letter_cont_field;
      check_pos genv;
      genv#add_to_stmt s;
      genv#add_to_pos 1;
      scan_stmt genv form lexbuf
    end

|   ',' | '+' | '-' | '*' | '/' ->
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    let s = Ulexing.utf8_lexeme lexbuf in
    check_pos genv;
    genv#add_to_stmt s;

    genv#add_sep s;

    genv#add_to_pos 1;
    scan_stmt genv form lexbuf

|   "//" ->
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    let s = Ulexing.utf8_lexeme lexbuf in
    check_pos genv;
    genv#add_to_stmt s;

    genv#add_sep s;

    genv#add_to_pos 2;
    scan_stmt genv form lexbuf

|   _ -> 
    if is_head then begin
      genv#reset_paren_level;
      genv#reset_margin_paren_level;
    end;
    genv#clear_last_in_margin;
    genv#clear_letter_cont_field;
    let s = Ulexing.utf8_lexeme lexbuf in
    check_pos genv;
    genv#add_to_stmt s;

    if s = "(" then begin
      DEBUG_MSG "pos=%d" genv#pos;
      genv#enter_paren
    end
    else if s = ")" then begin
      DEBUG_MSG "pos=%d" genv#pos;
      genv#exit_paren
    end;

    genv#add_to_pos 1;
    scan_stmt genv form lexbuf


  and scan_hollerith genv form n i = lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR";
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#exit_margin;
    scan_hollerith_continuation genv form n i 1 lexbuf

|   _ ->
    DEBUG_MSG "\"%s\"" (Ulexing.utf8_lexeme lexbuf);
    let len = Ulexing.lexeme_length lexbuf in
    if n = i then begin
      check_pos genv;
      genv#add_to_pos len;
      scan_stmt genv form lexbuf
    end
    else
      scan_hollerith genv form n (i+len) lexbuf


  and scan_hollerith_continuation genv form n i pos = lexer
|   ['C' 'c' '*' 'D' 'd'] -> 
    if pos = 1 then
      scan_hollerith_skip_line genv form n i lexbuf
    else if pos = 6 then
      scan_hollerith genv form n i lexbuf
    else
      scan_hollerith_continuation genv form n i (pos+1) lexbuf

|   '0' | white_space ->
    if pos = 6 then
      scan_hollerith genv form n i lexbuf
    else
      scan_hollerith_continuation genv form n i (pos+1) lexbuf

|   _ ->
    if pos = 6 then
      scan_hollerith genv form n i lexbuf
    else
      scan_hollerith_continuation genv form n i (pos+1) lexbuf


  and scan_hollerith_skip_line genv form n i = lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR";
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#exit_margin;
    scan_hollerith_continuation genv form n i 1 lexbuf

|   _ -> scan_hollerith_skip_line genv form n i lexbuf


  and scan_block_comment_stmt genv form = lexer
|   "*/" -> 
    genv#clear_letter_cont_field;
    genv#add_to_pos 1;
    genv#add_to_stmt " ";
    scan_stmt genv form lexbuf

|   _ -> scan_block_comment_stmt genv form lexbuf


  and scan_char_single genv form = lexer
|   "''" -> 
    genv#add_to_pos 2;
    scan_char_single genv form lexbuf

|   "\"\"" -> 
    genv#add_to_pos 2;
    scan_char_single genv form lexbuf

|   '&' white_space* line_terminator ->
    if genv#pos > env#current_source#max_line_length then begin
      DEBUG_MSG "CHARACTER CONTEXT CONTINUATION?";
      genv#add_to_lnum 1;
      genv#reset_pos;
      genv#exit_margin;
      scan_label_field genv form lexbuf
    end
    else begin
      DEBUG_MSG "CHARACTER CONTEXT CONTINUATION!";
      possibly_free genv form lexbuf
    end

|   line_terminator ->
    DEBUG_MSG "LINE TERMINATOR";
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#exit_margin;
    scan_label_field genv form lexbuf

|   '\'' -> 
    DEBUG_MSG "CHAR END (SINGLE QUOTE) [%dL] pos=%d" genv#lnum genv#pos;
    check_pos genv;
    genv#add_to_pos 1;
    genv#add_to_stmt "'";
    genv#exit_char PA.CH_SINGLE;
    scan_stmt genv form lexbuf

|   [^'\''] ->
    (*DEBUG_MSG "[%s] pos=%d" (Ulexing.utf8_lexeme lexbuf) genv#pos;*)
    check_pos genv;
    genv#add_to_pos 1;
    scan_char_single genv form lexbuf


  and scan_char_double genv form = lexer
|   "''" -> 
    genv#add_to_pos 2;
    scan_char_double genv form lexbuf

|   "\"\"" -> 
    genv#add_to_pos 2; 
    scan_char_double genv form lexbuf

|   '&' white_space* line_terminator ->
    if genv#pos > env#current_source#max_line_length then begin
      DEBUG_MSG "CHARACTER CONTEXT CONTINUATION?";
      genv#add_to_lnum 1;
      genv#reset_pos;
      genv#exit_margin;
      scan_label_field genv form lexbuf
    end
    else begin
      DEBUG_MSG "CHARACTER CONTEXT CONTINUATION!";
      possibly_free genv form lexbuf
    end

|   line_terminator -> 
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#exit_margin;
    scan_label_field genv form lexbuf

|   '\"' -> 
    DEBUG_MSG "CHAR END (DOUBLE QUOTE) [%dL] pos=%d" genv#lnum genv#pos;
    check_pos genv;
    genv#add_to_pos 1;
    genv#add_to_stmt "\"";
    genv#exit_char PA.CH_DOUBLE;
    scan_stmt genv form lexbuf

|   [^'"'] ->
    (*DEBUG_MSG "[%s] pos=%d" (Ulexing.utf8_lexeme lexbuf) genv#pos;*)
    check_pos genv;
    genv#add_to_pos 1;
    scan_char_double genv form lexbuf


  and scan_comment ctype ?(is_blank=true) genv form = lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR (%s)" (comment_type_to_string ctype);
    if is_blank then begin
      match ctype with
      | C_fixed -> DEBUG_MSG "blank fixed comment"
      | _ -> ()
    end;
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#exit_margin;
    genv#set_blank_line_flag;
    scan_label_field genv form lexbuf

|   eof -> form

|   white_space ->
    scan_comment ctype ~is_blank genv form lexbuf

|   _ -> 
(*
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "\"%s\" (form=%s)" s (SF.to_string form);
*)
    scan_comment ctype ~is_blank:false genv form lexbuf

  and scan_pp_directive genv form = lexer
|   '\\' white_space* line_terminator -> 
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#set_blank_line_flag;
    scan_pp_directive genv form lexbuf

|   line_terminator -> 
    genv#add_to_lnum 1;
    genv#reset_pos;
    genv#set_blank_line_flag;
    scan_label_field genv form lexbuf

|   eof -> form

|   _ -> scan_pp_directive genv form lexbuf

  and scan_name genv = lexer
|   name -> Ulexing.utf8_lexeme lexbuf
|   _ -> ""


  let _guess_source_form ulexbuf =
    let genv = new guess_env in
    let form = scan_label_field genv SF.Fixed ulexbuf in
    DEBUG_MSG "form=%s" (SF.to_string form);
    DEBUG_MSG "genv:%s" genv#to_string;
    let final_form =
      if form = SF.Fixed then begin
        if genv#is_false_fixed_source_form then
          SF.Free
        else
          SF.Fixed
      end
      else
        form
    in
    let src = env#current_source in
    if genv#noncomment_margin_count > 0 && src#max_line_length < genv#max_pos then begin
      if final_form = SF.Fixed then
        src#set_max_line_length_fixed genv#max_pos
      else
        src#set_max_line_length_free genv#max_pos
    end;
    final_form


  let guess_source_form file =
    let src = new Source.c file in
    let ulexbuf = src#get_ulexbuf in
    let form =
      try
        let f = _guess_source_form ulexbuf in
        src#close;
        f
      with
        exn ->
          DEBUG_MSG "raised exception: %s" (Printexc.to_string exn);
	  src#close;
	  env#verbose_msg "guessing from file extension";
          try
            let ext = String.lowercase_ascii file#get_extension in
	    if List.mem ext [".f";".for"] then
	      SF.Fixed
	    else
	      SF.Free
          with
            Xfile.No_extension _ -> SF.Free
    in
    env#add_source_form file#path form;
    DEBUG_MSG "%s --> %s" file#path (SF.to_string form);
    form


  let rec name rest = 
    let _ = DEBUG_MSG "rest=%d" rest in
    lexer (* keyword that identifier or another keyword may follow *)
|   "allocatable"      -> (fun s -> ALLOCATABLE s), 11
|   "allocate"         -> (fun s -> ALLOCATE s), 8
|   "assign"           -> (fun s -> ASSIGN s), 6
|   "assignment"       -> (fun s -> ASSIGNMENT s), 10
|   "backspace"        -> (fun s -> BACKSPACE s), 9
|   "blockdata"        -> (fun s -> BLOCK_DATA s), 9
|   "call"             -> (fun s -> CALL s), 4
|   "case"             -> (fun s -> CASE s), 4
|   "character"        -> (fun s -> CHARACTER s), 9
|   "close"            -> (fun s -> CLOSE s), 5
|   "common"           -> (fun s -> COMMON s), 6
|   "complex"          -> (fun s -> KINDED_TYPE_SPEC s), 7 (* FUNCTION, SUBROUTINE, and name may follow *)
|   "contains"         -> (fun s -> CONTAINS s), 8
|   "contiguous"       -> (fun s -> SIMPLE_ATTR s), 10
|   "continue"         -> (fun s -> CONTINUE s), 8
|   "cycle"            -> (fun s -> CYCLE s), 5
|   "data"             -> (fun s -> DATA s), 4
|   "deallocate"       -> (fun s -> DEALLOCATE s), 10
|   "default"          -> (fun s -> DEFAULT s), 7
|   "dimension"        -> (fun s -> DIMENSION s), 9
|   "do"               -> (fun s -> DO s), 2
|   "doubleprecision"  -> (fun s -> DOUBLE_PRECISION s), 15
|   "doublecomplex"    -> (fun s -> DOUBLE_COMPLEX s), 13
|   "double"           -> (fun s -> DOUBLE s), 6
|   "precision"        -> (fun s -> PRECISION s), 9
|   "else"             -> (fun s -> ELSE s), 4
|   "elseif"           -> (fun s -> ELSE_IF s), 6
|   "elsewhere"        -> (fun s -> ELSEWHERE s), 9
|   "elemental"        -> (fun s -> PREFIX_SPEC s), 9
(*
|   "end"              -> (fun s -> END s), 3
*)
|   "endassociate"     -> (fun s -> END_ASSOCIATE s), 12
|   "endblock"         -> (fun s -> END_BLOCK s), 12
|   "endblockdata"     -> (fun s -> END_BLOCK_DATA s), 12
|   "endcritical"      -> (fun s -> END_CRITICAL s), 11
|   "enddo"            -> (fun s -> END_DO s), 5
|   "endfile"          -> (fun s -> END_FILE s), 7
|   "endforall"        -> (fun s -> END_FORALL s), 9
|   "endfunction"      -> (fun s -> END_FUNCTION s), 11
|   "endif"            -> (fun s -> END_IF s), 5
|   "endinterface"     -> (fun s -> END_INTERFACE s), 12
|   "endmodule"        -> (fun s -> END_MODULE s), 9
|   "endprogram"       -> (fun s -> END_PROGRAM s), 10
|   "endselect"        -> (fun s -> END_SELECT s), 9
|   "endsubmodule"     -> (fun s -> END_SUBMODULE s), 11
|   "endsubroutine"    -> (fun s -> END_SUBROUTINE s), 13
|   "endtype"          -> (fun s -> END_TYPE s), 7
|   "endwhere"         -> (fun s -> END_WHERE s), 8
|   "entry"            -> (fun s -> ENTRY s), 5
|   "enumerator"       -> (fun s -> ENUMERATOR s), 10
|   "equivalence"      -> (fun s -> EQUIVALENCE s), 11
|   "exit"             -> (fun s -> EXIT s), 4
|   "external"         -> (fun s -> SIMPLE_ATTR s), 8
|   "flush"            -> (fun s -> FLUSH s), 5
|   "forall"           -> (fun s -> FORALL s), 6
|   "format"           -> (fun s -> FORMAT s), 6
|   "function"         -> (fun s -> FUNCTION s), 8
|   "goto"             -> (fun s -> GO_TO s), 4
(*
|   "if"               -> (fun s -> IF s), 2
*)
|   "implicit"         -> (fun s -> IMPLICIT s), 8
|   "impure"           -> (fun s -> PREFIX_SPEC s), 6
(*
|   "in"               -> (fun s -> IN s), 2
|   "include"          -> (fun s -> INCLUDE s), 7
|   "inout"            -> (fun s -> IN_OUT s), 5
*)
|   "inquire"          -> (fun s -> INQUIRE s), 7
|   "integer"          -> (fun s -> KINDED_TYPE_SPEC s), 7 (* FUNCTION, SUBROUTINE, and name may follow *)
|   "intent"           -> (fun s -> INTENT s), 6
|   "interface"        -> (fun s -> INTERFACE s), 9
|   "intrinsic"        -> (fun s -> INTRINSIC s), 9
|   "logical"          -> (fun s -> KINDED_TYPE_SPEC s), 7 (* FUNCTION, SUBROUTINE, and name may follow *)
|   "module"           -> (fun s -> MODULE s), 6
|   "namelist"         -> (fun s -> NAMELIST s), 8
|   "none"             -> (fun s -> NONE s), 4
|   "null"             -> (fun s -> NULL s), 4
|   "nullify"          -> (fun s -> NULLIFY s), 7
|   "only"             -> (fun s -> ONLY s), 4
|   "open"             -> (fun s -> OPEN s), 4
|   "operator"         -> (fun s -> OPERATOR s), 8
|   "optional"         -> (fun s -> OPTIONAL s), 8
(*
|   "out"              -> (fun s -> OUT s), 3
*)
|   "parameter"        -> (fun s -> PARAMETER s), 9
|   "pause"            -> (fun s -> PAUSE s), 5
|   "pointer"          -> (fun s -> POINTER s), 7
|   "print"            -> (fun s -> PRINT s), 5
|   "private"          -> (fun s -> PRIVATE s), 7
|   "procedure"        -> (fun s -> PROCEDURE s), 9
|   "program"          -> (fun s -> PROGRAM s), 7
|   "protected"        -> (fun s -> SIMPLE_ATTR s), 9
|   "public"           -> (fun s -> PUBLIC s), 6
|   "pure"             -> (fun s -> PREFIX_SPEC s), 4
|   "read"             -> (fun s -> READ s), 4
|   "real"             -> (fun s -> KINDED_TYPE_SPEC s), 4 (* FUNCTION, SUBROUTINE, and name may follow *)
|   "recursive"        -> (fun s -> PREFIX_SPEC s), 9
|   "result"           -> (fun s -> RESULT s), 6
|   "return"           -> (fun s -> RETURN s), 6
|   "rewind"           -> (fun s -> REWIND s), 6
|   "save"             -> (fun s -> SAVE s), 4
|   "selectcase"       -> (fun s -> SELECT_CASE s), 10
|   "selecttype"       -> (fun s -> SELECT_TYPE s), 10
|   "sequence"         -> (fun s -> SEQUENCE s), 8
(*
|   "stat"             -> (fun s -> STAT s), 4
|   "stop"             -> (fun s -> STOP s), 4
*)
|   "submodule"        -> (fun s -> SUBMODULE s), 9
|   "subroutine"       -> (fun s -> SUBROUTINE s), 10
|   "target"           -> (fun s -> TARGET s), 6
|   "then"             -> (fun s -> THEN s), 4
|   "type"             -> (fun s -> TYPE s), 4
|   "use"              -> (fun s -> USE s), 3
|   "value"            -> (fun s -> SIMPLE_ATTR s), 5
|   "volatile"         -> (fun s -> SIMPLE_ATTR s), 8
|   "where"            -> (fun s -> WHERE s), 5
|   "while"            -> (fun s -> WHILE s), 5
|   "write"            -> (fun s -> WRITE s), 5

|   letter ->
    let str = Ulexing.utf8_lexeme lexbuf in
    let f = fun s -> IDENTIFIER s in
    if rest = 1 then
      f, 1
    else
      name_sub f (String.length str) lexbuf   

|   digit_string -> 
    let str = Ulexing.utf8_lexeme lexbuf in
    let f = fun s -> INT_LITERAL s in
    f, String.length str


   and name_sub f n = lexer
|   keyword_or_name -> f, (String.length (Ulexing.utf8_lexeme lexbuf)) + n
|   digit_string -> f, (String.length (Ulexing.utf8_lexeme lexbuf)) + n


  let tokenize_name _str =
    DEBUG_MSG "tokenizing \"%s\"..." _str;
    let size = String.length _str in
    let str = String.lowercase_ascii _str in

    let _str =
      if env#ignore_case then
        str
      else
        _str
    in

    let cur = ref 0 in
    let ulexbuf = Ulexing.from_utf8_string str in
    let l = ref [] in
    let keyword_found = ref false in
    begin
      try
	while true do
	  let tok_f, sz = name (size - !cur) ulexbuf in
	  let tok = tok_f (String.sub _str !cur sz) in
	  DEBUG_MSG "tok=%s" (Token.rawtoken_to_string tok);
	  begin
	    match tok with
	    | IDENTIFIER _ | INT_LITERAL _ -> ()
	    | _ -> keyword_found := true
	  end;
	  l := !l @ [tok];
	  cur := !cur + sz
	done
      with
      | Ulexing.Error -> ()
      | e -> WARN_MSG "exception caught: %s" (Printexc.to_string e)
    end;
    if !cur <> size then begin
      DEBUG_MSG "failed: size=%d cur=%d" size !cur;
      IDENTIFIER _str
    end
    else begin
      DEBUG_MSG "toks=[%s]" (Xlist.to_string Token.rawtoken_to_string ";" !l);
      match !l with
      | [] -> assert false
      | [x] -> x
      | xs -> 
	  if !keyword_found (* && config#is_fixed_source_form *) then
	    COMPOSITE_IDENTIFIER(false, _str, List.map Obj.repr xs)
	  else
	    IDENTIFIER _str
    end

  exception Invalid_ocl
      

  let ofs_to_pos ofs =
    let pos_mgr = env#current_pos_mgr in
    let l, c = pos_mgr#get_position ofs in
    Loc.mklexpos ~fname:pos_mgr#filename ~lnum:l ~bol:(ofs-c) ofs


  let mkt ofs rt lexbuf =
    let st = Ulexing.lexeme_start lexbuf + ofs in
    let ed = (Ulexing.lexeme_end lexbuf) - 1 + ofs in
    let st_pos = ofs_to_pos st in
    let ed_pos = ofs_to_pos ed in
    make_qtoken rt st_pos ed_pos


  let find_ocl_keyword =
    let keyword_list =
      [ 
        "aligned",                OCL_ALIGNED;
        "array_fusion",           OCL_ARRAY_FUSION;
        "array_merge",            OCL_ARRAY_MERGE;
        "array_private",          OCL_ARRAY_PRIVATE;
        "array_subscript",        OCL_ARRAY_SUBSCRIPT;
        "auto",                   OCL_AUTO;
        "cache_sector_size",      OCL_CACHE_SECTOR_SIZE;
        "cache_subsector_assign", OCL_CACHE_SUBSECTOR_ASSIGN;
        "end_array_fusion",       OCL_END_ARRAY_FUSION;
        "end_cache_sector_size",  OCL_END_CACHE_SECTOR_SIZE;
        "end_cache_subsector",    OCL_END_CACHE_SUBSECTOR;
        "eval",                   OCL_EVAL;
        "fission_point",          OCL_FISSION_POINT;
        "fltld",                  OCL_FLTLD;
        "fp_contract",            OCL_FP_CONTRACT;
        "fp_relaxed",             OCL_FP_RELAXED;
        "independent",            OCL_INDEPENDENT;
        "level",                  OCL_LEVEL;
        "loop_blocking",          OCL_LOOP_BLOCKING;
        "loop_interchange",       OCL_LOOP_INTERCHANGE;
        "loop_noblocking",        OCL_LOOP_NOBLOCKING;
        "loop_nofission",         OCL_LOOP_NOFISSION;
        "loop_nofusion",          OCL_LOOP_NOFUSION;
        "loop_nointerchange",     OCL_LOOP_NOINTERCHANGE;
        "mfunc",                  OCL_MFUNC;
        "noalias",                OCL_NOALIAS;
        "noarray_private",        OCL_NOARRAY_PRIVATE;
        "noarraypad",             OCL_NOARRAYPAD;
        "noeval",                 OCL_NOEVAL;
        "nofltld",                OCL_NOFLTLD;
        "nofp_contract",          OCL_NOFP_CONTRACT;
        "nofp_relaxed",           OCL_NOFP_RELAXED;
        "nomfunc",                OCL_NOMFUNC;
        "nopreex",                OCL_NOPREEX;
        "noprefetch",             OCL_NOPREFETCH;
        "norecurrence",           OCL_NORECURRENCE;
        "noreduction",            OCL_NOREDUCTION;
        "nosimd",                 OCL_NOSIMD;
        "nostriping",             OCL_NOSTRIPING;
        "noswp",                  OCL_NOSWP;
        "nounroll",               OCL_NOUNROLL;
        "nouxsimd",               OCL_NOUXSIMD;
        "novrec",                 OCL_NOVREC;
        "noxfill",                OCL_NOXFILL;
        "parallel",               OCL_PARALLEL;
        "parallel_strong",        OCL_PARALLEL_STRONG;
        "preex",                  OCL_PREEX;
        "prefetch",               OCL_PREFETCH;
        "prefetch_cache_level",   OCL_PREFETCH_CACHE_LEVEL;
        "prefetch_infer",         OCL_PREFETCH_INFER;
        "prefetch_iteration",     OCL_PREFETCH_ITERATION;
        "prefetch_iteration_l2",  OCL_PREFETCH_ITERATION_L2;
        "prefetch_noinfer",       OCL_PREFETCH_NOINFER;
        "prefetch_nostrong",      OCL_PREFETCH_NOSTRONG;
        "prefetch_nostrong_l2",   OCL_PREFETCH_NOSTRONG_L2;
        "prefetch_read",          OCL_PREFETCH_READ;
        "prefetch_sequential",    OCL_PREFETCH_SEQUENTIAL;
        "prefetch_strong",        OCL_PREFETCH_STRONG;
        "prefetch_strong_l2",     OCL_PREFETCH_STRONG_L2;
        "prefetch_write",         OCL_PREFETCH_WRITE;
        "reduction",              OCL_REDUCTION;
        "serial",                 OCL_SERIAL;
        "simd",                   OCL_SIMD;
        "soft",                   OCL_SOFT;
        "striping",               OCL_STRIPING;
        "strong",                 OCL_STRONG;
        "swp",                    OCL_SWP;
        "temp",                   OCL_TEMP;
        "unaligned",              OCL_UNALIGNED;
        "unroll",                 OCL_UNROLL;
        "uxsimd",                 OCL_UXSIMD;
        "xfill",                  OCL_XFILL;
        "loop_part_parallel",     OCL_LOOP_PART_PARALLEL;
        "loop_nopart_parallel",   OCL_LOOP_NOPART_PARALLEL;
        "first_private",          OCL_FIRST_PRIVATE;
        "last_private",           OCL_LAST_PRIVATE;
        "temp_private",           OCL_TEMP_PRIVATE;
        "parallel_cyclic",        OCL_PARALLEL_CYCLIC;
      ] in 
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    let _ = 
      List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
        keyword_list 
    in
    let find s = 
      try
        Hashtbl.find keyword_table (String.lowercase_ascii s)
      with
        Not_found -> IDENTIFIER s
    in
    find

  let rec scan_ocl ofs = lexer
|   white_space -> scan_ocl ofs lexbuf

|   line_terminator -> mkt ofs EOL lexbuf

|   name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    mkt ofs (find_ocl_keyword s) lexbuf

|   digit_string -> mkt ofs (INT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   dotted_op -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   dotted_identifier -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   char_literal_constant_no_kind -> mkt ofs (CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   "**"  -> mkt ofs STAR_STAR lexbuf
|   "=="  -> mkt ofs EQ_EQ lexbuf
|   "/="  -> mkt ofs SLASH_EQ lexbuf
|   ">="  -> mkt ofs GT_EQ lexbuf
|   "<="  -> mkt ofs LT_EQ lexbuf

|   "<" -> mkt ofs LT lexbuf
|   "=" -> mkt ofs EQ lexbuf
|   ">" -> mkt ofs GT lexbuf

|   "+" -> mkt ofs PLUS lexbuf
|   "-" -> mkt ofs MINUS lexbuf
|   "*" -> mkt ofs STAR lexbuf
|   "/" -> mkt ofs SLASH lexbuf

|   "(" -> mkt ofs LPAREN lexbuf
|   ")" -> mkt ofs RPAREN lexbuf
|   "," -> mkt ofs COMMA lexbuf
|   ":" -> mkt ofs COLON lexbuf




  let find_omp_keyword, get_omp_keyword_string =
    let keyword_list =
      [ 
(* clause keywords *)
        "auto",          OMP_AUTO;
        "capture",       OMP_CAPTURE;
        "collapse",      OMP_COLLAPSE;
        "copyin",        OMP_COPYIN;
        "copyprivate",   OMP_COPYPRIVATE;
        "default",       OMP_DEFAULT;
        "dynamic",       OMP_DYNAMIC;
        "end",           OMP_END;
        "final",         OMP_FINAL;
        "firstprivate",  OMP_FIRSTPRIVATE;
        "guided",        OMP_GUIDED;
        "if",            OMP_IF;
        "lastprivate",   OMP_LASTPRIVATE;
        "mergeable",     OMP_MERGEABLE;
        "none",          OMP_NONE;
        "nowait",        OMP_NOWAIT;
        "num_threads",   OMP_NUM_THREADS;
        "private",       OMP_PRIVATE;
        "read",          OMP_READ;
        "reduction",     OMP_REDUCTION;
        "runtime",       OMP_RUNTIME;
        "schedule",      OMP_SCHEDULE;
        "section",       OMP_SECTION;
        "shared",        OMP_SHARED;
        "static",        OMP_STATIC;
        "untied",        OMP_UNTIED;
        "update",        OMP_UPDATE;
        "write",         OMP_WRITE;

(* directive keywords *)
        "parallel",      OMP_PARALLEL;
        "do",            OMP_DO;
        "sections",      OMP_SECTIONS;
        "single",        OMP_SINGLE;
        "workshare",     OMP_WORKSHARE;
        "task",          OMP_TASK;
        "taskyield",     OMP_TASKYIELD;
        "master",        OMP_MASTER;
        "critical",      OMP_CRITICAL;
        "barrier",       OMP_BARRIER;
        "taskwait",      OMP_TASKWAIT;
        "atomic",        OMP_ATOMIC;
        "flush",         OMP_FLUSH;
        "ordered",       OMP_ORDERED;
        "threadprivate", OMP_THREADPRIVATE;

        "endatomic",            OMP_END_ATOMIC;
        "endcritical",          OMP_END_CRITICAL;
        "enddo",                OMP_END_DO;
        "endmaster",            OMP_END_MASTER;
        "endordered",           OMP_END_ORDERED;
        "endparallel",          OMP_END_PARALLEL;
        "endsections",          OMP_END_SECTIONS;
        "endsingle",            OMP_END_SINGLE;
        "endtask",              OMP_END_TASK;
        "endworkshare",         OMP_END_WORKSHARE;
        "paralleldo",           OMP_PARALLEL_DO;
        "parallelsections",     OMP_PARALLEL_SECTIONS;
        "parallelworkshare",    OMP_PARALLEL_WORKSHARE;
        "endparalleldo",        OMP_END_PARALLEL_DO;
        "endparallelsections",  OMP_END_PARALLEL_SECTIONS;
        "endparallelworkshare", OMP_END_PARALLEL_WORKSHARE;

(* 4.0 clause keywords *)
        "linear",        OMP_LINEAR;
        "map",           OMP_MAP;
        "alloc",         OMP_ALLOC;
        "to",            OMP_TO;
        "from",          OMP_FROM;
        "tofrom",        OMP_TOFROM;
        "safelen",       OMP_SAFELEN;
        "simdlen",       OMP_SIMDLEN;
        "aligned",       OMP_ALIGNED;
        "uniform",       OMP_UNIFORM;
        "inbranch",      OMP_INBRANCH;
        "notinbranch",   OMP_NOTINBRANCH;
        "device",        OMP_DEVICE;
        "seq_cst",       OMP_SEQ_CST;
        "dist_schedule", OMP_DIST_SCHEDULE;
        "in",            OMP_IN;
        "inout",         OMP_INOUT;
        "out",           OMP_OUT;
        "initializer",   OMP_INITIALIZER;
        "proc_bind",     OMP_PROC_BIND;
        "close",         OMP_CLOSE;
        "spread",        OMP_SPREAD;
        "depend",        OMP_DEPEND;
        "num_teams",     OMP_NUM_TEAMS;
        "thread_limit",  OMP_THREAD_LIMIT;

(* 4.0 directive keywords *)
        "cancel",                                 OMP_CANCEL;
        "cancellationpoint",                      OMP_CANCELLATION_POINT;
        "declarereduction",                       OMP_DECLARE_REDUCTION;
        "declaresimd",                            OMP_DECLARE_SIMD;
        "declaretarget",                          OMP_DECLARE_TARGET;
        "distribute",                             OMP_DISTRIBUTE;
        "distributeparalleldosimd",               OMP_DISTRIBUTE_PARALLEL_DO_SIMD;
        "distributeparalleldo",                   OMP_DISTRIBUTE_PARALLEL_DO;
        "distributesimd",                         OMP_DISTRIBUTE_SIMD;
        "dosimd",                                 OMP_DO_SIMD;
        "enddistributeparalleldo",                OMP_END_DISTRIBUTE_PARALLEL_DO;
        "enddistributeparalleldosimd",            OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD;
        "enddistributesimd",                      OMP_END_DISTRIBUTE_SIMD;
        "enddistribute",                          OMP_END_DISTRIBUTE;
        "enddosimd",                              OMP_END_DO_SIMD;
        "endparalleldosimd",                      OMP_END_PARALLEL_DO_SIMD;
        "endsimd",                                OMP_END_SIMD;
        "endtarget",                              OMP_END_TARGET;
        "endtargetdata",                          OMP_END_TARGET_DATA;
        "endtargetteams",                         OMP_END_TARGET_TEAMS;
        "endtargetteamsdistribute",               OMP_END_TARGET_TEAMS_DISTRIBUTE;
        "endtargetteamsdistributeparalleldo",     OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO;
        "endtargetteamsdistributeparalleldosimd", OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
        "endtargetteamsdistributesimd",           OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD;
        "endtaskgroup",                           OMP_END_TASKGROUP;
        "endteams",                               OMP_END_TEAMS;
        "endteamsdistribute",                     OMP_END_TEAMS_DISTRIBUTE;
        "endteamsdistributeparalleldo",           OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO;
        "endteamsdistributeparalleldosimd",       OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
        "endteamsdistributesimd",                 OMP_END_TEAMS_DISTRIBUTE_SIMD;
        "paralleldosimd",                         OMP_PARALLEL_DO_SIMD;
        "simd",                                   OMP_SIMD;
        "target",                                 OMP_TARGET;
        "targetdata",                             OMP_TARGET_DATA;
        "targetteams",                            OMP_TARGET_TEAMS;
        "targetteamsdistribute",                  OMP_TARGET_TEAMS_DISTRIBUTE;
        "targetteamsdistributeparalleldo",        OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO;
        "targetteamsdistributeparalleldosimd",    OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
        "targetteamsdistributesimd",              OMP_TARGET_TEAMS_DISTRIBUTE_SIMD;
        "targetupdate",                           OMP_TARGET_UPDATE;
        "taskgroup",                              OMP_TASKGROUP;
        "teams",                                  OMP_TEAMS;
        "teamsdistribute",                        OMP_TEAMS_DISTRIBUTE;
        "teamsdistributeparalleldo",              OMP_TEAMS_DISTRIBUTE_PARALLEL_DO;
        "teamsdistributeparalleldosimd",          OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
        "teamsdistributesimd",                    OMP_TEAMS_DISTRIBUTE_SIMD;

(* type-spec keywords required for DECLARE REDUCTION *)
        "integer",         KINDED_TYPE_SPEC "integer";
        "real",            KINDED_TYPE_SPEC "real";
        "double",          DOUBLE "double";
        "precision",       PRECISION "precision";
        "doubleprecision", DOUBLE_PRECISION "doubleprecision";
        "doublecomplex",   DOUBLE_COMPLEX "doublecomplex";
        "complex",         KINDED_TYPE_SPEC "complex";
        "character",       CHARACTER "character";
        "logical",         KINDED_TYPE_SPEC "logical";
        "kind",            KIND "kind";
        "len",             LEN "len";

      ] in 
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) keyword_list;
    let find ?(no_ident=false) s = 
      try
        Hashtbl.find keyword_table (String.lowercase_ascii s)
      with
        Not_found -> 
          if no_ident then
            raise Not_found
          else
            IDENTIFIER s
    in
    let inv_keyword_table = Hashtbl.create (List.length keyword_list) in
    List.iter (fun (kwd, tok) -> Hashtbl.add inv_keyword_table tok kwd) keyword_list;
    let get tok = Hashtbl.find inv_keyword_table tok in
    find, get


  let blank_pat = Str.regexp "[ \t]+"

  let get_omp_continuable_keyword = function
    | IDENTIFIER _ident -> begin
        let ident = String.lowercase_ascii _ident in
        match ident with
        | "cancellation"
        | "declare"
        | "distributeparallel"
        | "enddistributeparallel" 
        | "endtargetteamsdistributeparallel"
        | "endteamsdistributeparallel"
        | "targetteamsdistributeparallel"
        | "teamsdistributeparallel"
        | "thread"
          -> ident
        | _ -> raise Not_found
    end
    | OMP_DISTRIBUTE                              -> "distribute"
    | OMP_DISTRIBUTE_PARALLEL_DO                  -> "distributeparalleldo"
    | OMP_DO                                      -> "do"
    | OMP_END                                     -> "end"
    | OMP_END_DISTRIBUTE                          -> "enddistribute"
    | OMP_END_DISTRIBUTE_PARALLEL_DO              -> "enddistributeparalleldo"
    | OMP_END_DO                                  -> "enddo"
    | OMP_END_PARALLEL                            -> "endparallel"
    | OMP_END_PARALLEL_DO                         -> "endparalleldo"
    | OMP_END_TARGET                              -> "endtarget"
    | OMP_END_TARGET_TEAMS                        -> "endtargetteams"
    | OMP_END_TARGET_TEAMS_DISTRIBUTE             -> "endtargetteamsdistribute"
    | OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO -> "endtargetteamsdistributeparalleldo"
    | OMP_END_TEAMS                               -> "endteams"
    | OMP_END_TEAMS_DISTRIBUTE                    -> "endteamsdistribute"
    | OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO        -> "endteamsdistributeparalleldo"
    | OMP_PARALLEL                                -> "parallel"
    | OMP_PARALLEL_DO                             -> "paralleldo"
    | OMP_TARGET                                  -> "target"
    | OMP_TARGET_TEAMS                            -> "targetteams"
    | OMP_TARGET_TEAMS_DISTRIBUTE                 -> "targetteamsdistribute"
    | OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO     -> "targetteamsdistributeparalleldo"
    | OMP_TEAMS                                   -> "teams"
    | OMP_TEAMS_DISTRIBUTE                        -> "teamsdistribute"
    | OMP_TEAMS_DISTRIBUTE_PARALLEL_DO            -> "teamsdistributeparalleldo"

    | DOUBLE s -> s

    | _ -> raise Not_found


  let get_omp_following_keyword = function
    | IDENTIFIER _ident -> begin
        let ident = String.lowercase_ascii _ident in
        match ident with
        | "data"
        | "distributeparallel"
        | "targetteamsdistributeparallel"
        | "teamsdistributeparallel"
          -> ident
        | _ -> raise Not_found
    end
    | OMP_REDUCTION                                -> "reduction"
    | OMP_SIMD                                     -> "simd"
    | OMP_TARGET                                   -> "target"
    | OMP_PARALLEL                                 -> "parallel"
    | OMP_PARALLEL_DO                              -> "paralleldo"
    | OMP_PARALLEL_DO_SIMD                         -> "paralleldosimd"
    | OMP_ATOMIC                                   -> "atomic"
    | OMP_CRITICAL                                 -> "critical"
    | OMP_DISTRIBUTE                               -> "distribute"
    | OMP_DISTRIBUTE_PARALLEL_DO                   -> "distributeparalleldo"
    | OMP_DISTRIBUTE_PARALLEL_DO_SIMD              -> "distributeparalleldosimd"
    | OMP_DISTRIBUTE_SIMD                          -> "distributesimd"
    | OMP_DO                                       -> "do"
    | OMP_DO_SIMD                                  -> "dosimd"
    | OMP_MASTER                                   -> "master"
    | OMP_ORDERED                                  -> "ordered"
    | OMP_PARALLEL_SECTIONS                        -> "parallelsections"
    | OMP_PARALLEL_WORKSHARE                       -> "parallelworkshare"
    | OMP_SECTIONS                                 -> "sections"
    | OMP_SINGLE                                   -> "single"
    | OMP_TARGET_DATA                              -> "targetdata"
    | OMP_TARGET_TEAMS                             -> "targetteams"
    | OMP_TARGET_TEAMS_DISTRIBUTE                  -> "targetteamsdistribute"
    | OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO      -> "targetteamsdistributeparalleldo"
    | OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD -> "targetteamsdistributeparalleldosimd"
    | OMP_TARGET_TEAMS_DISTRIBUTE_SIMD             -> "targetteamsdistributesimd"
    | OMP_TASK                                     -> "task"
    | OMP_TASKGROUP                                -> "taskgroup"
    | OMP_TEAMS                                    -> "teams"
    | OMP_TEAMS_DISTRIBUTE                         -> "teamsdistribute"
    | OMP_TEAMS_DISTRIBUTE_PARALLEL_DO             -> "teamsdistributeparalleldo"
    | OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD        -> "teamsdistributeparalleldosimd"
    | OMP_TEAMS_DISTRIBUTE_SIMD                    -> "teamsdistributesimd"
    | OMP_WORKSHARE                                -> "workshare"
    | OMP_UPDATE                                   -> "update"
    | OMP_PRIVATE                                  -> "private"

    | PRECISION s 
(*    | COMPLEX s*)
      -> s

    | KINDED_TYPE_SPEC s when (String.lowercase_ascii s) = "complex" -> "complex"

    | _ -> raise Not_found


  let rec scan_omp ofs = lexer
|   white_space -> scan_omp ofs lexbuf

|   line_terminator -> mkt ofs EOL lexbuf

|   name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    mkt ofs (find_omp_keyword s) lexbuf

|   int_literal_constant     -> mkt ofs (INT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   real_literal_constant    -> mkt ofs (REAL_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   logical_literal_constant -> mkt ofs (LOGICAL_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   boz_literal_constant     -> mkt ofs (BOZ_LITERAL (normalize_continued_string (Ulexing.utf8_lexeme lexbuf))) lexbuf

|   dotted_op -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   dotted_identifier -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   char_literal_constant_no_kind -> mkt ofs (CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   "**"  -> mkt ofs STAR_STAR lexbuf
|   "=="  -> mkt ofs EQ_EQ lexbuf
|   "/="  -> mkt ofs SLASH_EQ lexbuf
|   ">="  -> mkt ofs GT_EQ lexbuf
|   "<="  -> mkt ofs LT_EQ lexbuf
|   "//"  -> mkt ofs SLASH_SLASH lexbuf

|   "<" -> mkt ofs LT lexbuf
|   "=" -> mkt ofs EQ lexbuf
|   ">" -> mkt ofs GT lexbuf

|   "+" -> mkt ofs PLUS lexbuf
|   "-" -> mkt ofs MINUS lexbuf
|   "*" -> mkt ofs STAR lexbuf
|   "/" -> mkt ofs SLASH lexbuf

|   "(" -> mkt ofs LPAREN lexbuf
|   ")" -> mkt ofs RPAREN lexbuf
|   "," -> mkt ofs COMMA lexbuf
|   ":" -> mkt ofs COLON lexbuf

|   "%" -> mkt ofs PERCENT lexbuf

|   "&" -> scan_omp ofs lexbuf


  let find_acc_keyword, get_acc_keyword_string =
    let keyword_list =
      [ 
        "parallel",           ACC_PARALLEL;
        "kernels",            ACC_KERNELS;
        "data",               ACC_DATA;
        "enter",              ACC_ENTER;
        "exit",               ACC_EXIT;
        "host_data",          ACC_HOST_DATA;
        "loop",               ACC_LOOP;
        "cache",              ACC_CACHE;
        "atomic",             ACC_ATOMIC;
        "update",             ACC_UPDATE;
        "wait",               ACC_WAIT;
        "routine",            ACC_ROUTINE;
        "declare",            ACC_DECLARE;
        "end",                ACC_END;
        "if",                 ACC_IF;
        "reduction",          ACC_REDUCTION;
        "private",            ACC_PRIVATE;
        "firstprivate",       ACC_FIRSTPRIVATE;
        "default",            ACC_DEFAULT;
        "none",               ACC_NONE;
        "device_type",        ACC_DEVICE_TYPE;
        "dtype",              ACC_DTYPE;
        "async",              ACC_ASYNC;
        "num_gangs",          ACC_NUM_GANGS;
        "num_workers",        ACC_NUM_WORKERS;
        "vector_length",      ACC_VECTOR_LENGTH;
        "copyin",             ACC_COPYIN;
        "create",             ACC_CREATE;
        "present_or_copy",    ACC_PRESENT_OR_COPY;
        "pcopy",              ACC_PCOPY;
        "present_or_copyin",  ACC_PRESENT_OR_COPYIN;
        "pcopyin",            ACC_PCOPYIN;
        "present_or_copyout", ACC_PRESENT_OR_COPYOUT;
        "pcopyout",           ACC_PCOPYOUT;
        "use_device",         ACC_USE_DEVICE;
        "present_or_create",  ACC_PRESENT_OR_CREATE;
        "pcreate",            ACC_PCREATE;
        "copyout",            ACC_COPYOUT;
        "delete",             ACC_DELETE;
        "copy",               ACC_COPY;
        "present",            ACC_PRESENT;
        "deviceptr",          ACC_DEVICEPTR;
        "collapse",           ACC_COLLAPSE;
        "seq",                ACC_SEQ;
        "auto",               ACC_AUTO;
        "tile",               ACC_TILE;
        "gang",               ACC_GANG;
        "worker",             ACC_WORKER;
        "vector",             ACC_VECTOR;
        "independent",        ACC_INDEPENDENT;
        "read",               ACC_READ;
        "write",              ACC_WRITE;
        "capture",            ACC_CAPTURE;
        "self",               ACC_SELF;
        "host",               ACC_HOST;
        "bind",               ACC_BIND;
        "nohost",             ACC_NOHOST;
        "device_resident",    ACC_DEVICE_RESIDENT;
        "link",               ACC_LINK;
        "device",             ACC_DEVICE;
      ] in 
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) keyword_list;
    let find ?(no_ident=false) s = 
      try
        Hashtbl.find keyword_table (String.lowercase_ascii s)
      with
        Not_found -> 
          if no_ident then
            raise Not_found
          else
            IDENTIFIER s
    in
    let inv_keyword_table = Hashtbl.create (List.length keyword_list) in
    List.iter (fun (kwd, tok) -> Hashtbl.add inv_keyword_table tok kwd) keyword_list;
    let get tok = Hashtbl.find inv_keyword_table tok in
    find, get


  let blank_pat = Str.regexp "[ \t]+"


  let rec scan_acc ofs = lexer
|   white_space -> scan_acc ofs lexbuf

|   line_terminator -> mkt ofs EOL lexbuf

|   name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    mkt ofs (find_acc_keyword s) lexbuf

|   int_literal_constant     -> mkt ofs (INT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   real_literal_constant    -> mkt ofs (REAL_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   logical_literal_constant -> mkt ofs (LOGICAL_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   boz_literal_constant     -> mkt ofs (BOZ_LITERAL (normalize_continued_string (Ulexing.utf8_lexeme lexbuf))) lexbuf

|   dotted_op -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   dotted_identifier -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   char_literal_constant_no_kind -> mkt ofs (CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   "**"  -> mkt ofs STAR_STAR lexbuf
|   "=="  -> mkt ofs EQ_EQ lexbuf
|   "/="  -> mkt ofs SLASH_EQ lexbuf
|   ">="  -> mkt ofs GT_EQ lexbuf
|   "<="  -> mkt ofs LT_EQ lexbuf
|   "//"  -> mkt ofs SLASH_SLASH lexbuf

|   "<" -> mkt ofs LT lexbuf
|   "=" -> mkt ofs EQ lexbuf
|   ">" -> mkt ofs GT lexbuf

|   "+" -> mkt ofs PLUS lexbuf
|   "-" -> mkt ofs MINUS lexbuf
|   "*" -> mkt ofs STAR lexbuf
|   "/" -> mkt ofs SLASH lexbuf

|   "(" -> mkt ofs LPAREN lexbuf
|   ")" -> mkt ofs RPAREN lexbuf
|   "," -> mkt ofs COMMA lexbuf
|   ":" -> mkt ofs COLON lexbuf

|   "%" -> mkt ofs PERCENT lexbuf

|   "&" -> scan_acc ofs lexbuf



  let find_xlf_keyword =
    let keyword_list =
      [ 
        "align",               XLF_ALIGN;
        "assert",              XLF_ASSERT;
        "block_loop",          XLF_BLOCK_LOOP;
        "cncall",              XLF_CNCALL;
        "collapse",            XLF_COLLAPSE;
        "eject",               XLF_EJECT;
        "execution_frequency", XLF_EXECUTION_FREQUENCY;
        "expected_value",      XLF_EXPECTED_VALUE;
        "functrace_xlf_catch", XLF_FUNCTRACE_XLF_CATCH;
        "functrace_xlf_exter", XLF_FUNCTRACE_XLF_ENTER;
        "functrace_xlf_exit",  XLF_FUNCTRACE_XLF_EXIT;
        "ignore_tkr",          XLF_IGNORE_TKR;
        "independent",         XLF_INDEPENDENT;
        "loopid",              XLF_LOOPID;
        "mem_delay",           XLF_MEM_DELAY;
        "new",                 XLF_NEW;
        "nofunctrace",         XLF_NOFUNCTRACE;
        "nosimd",              XLF_NOSIMD;
        "novector",            XLF_NOVECTOR;
        "permutation",         XLF_PERMUTATION;
        "snapshot",            XLF_SNAPSHOT;
        "sourceform",          XLF_SOURCEFORM;
        "stream_unroll",       XLF_STREAM_UNROLL;
        "subscriptorder",      XLF_SUBSCRIPTORDER;
        "unroll",              XLF_UNROLL;
        "unroll_and_fuse",     XLF_UNROLL_AND_FUSE;

        "itercnt",    XLF_ITERCNT;
        "minitercnt", XLF_MINITERCNT;
        "maxitercnt", XLF_MAXITERCNT;
        "nodeps",     XLF_NODEPS;
        "reduction",  XLF_REDUCTION;
(*
        "max",       XLF_MAX;
        "min",       XLF_MIN;
        "iand",      XLF_IAND;
        "ieor",      XLF_IEOR;
*)
        "fixed",     XLF_FIXED;
        "free",      XLF_FREE;
        "f90",       XLF_F90;
        "ibm",       XLF_IBM;
        "very_high", XLF_VERY_HIGH;
        "very_low",  XLF_VERY_LOW;
      ] in 
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    let _ = 
      List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
        keyword_list 
    in
    let find s = 
      try
        Hashtbl.find keyword_table (String.lowercase_ascii s)
      with
        Not_found -> IDENTIFIER s
    in
    find

  let rec scan_xlf ofs = lexer
|   white_space -> scan_xlf ofs lexbuf

|   line_terminator -> mkt ofs EOL lexbuf

|   at_process -> mkt ofs XLF_PROCESS lexbuf

|   name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    mkt ofs (find_xlf_keyword s) lexbuf

|   digit_string -> mkt ofs (INT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   dotted_op -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   dotted_identifier -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   char_literal_constant_no_kind -> mkt ofs (CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   "**"  -> mkt ofs STAR_STAR lexbuf
|   "=="  -> mkt ofs EQ_EQ lexbuf
|   "/="  -> mkt ofs SLASH_EQ lexbuf
|   ">="  -> mkt ofs GT_EQ lexbuf
|   "<="  -> mkt ofs LT_EQ lexbuf

|   "<" -> mkt ofs LT lexbuf
|   "=" -> mkt ofs EQ lexbuf
|   ">" -> mkt ofs GT lexbuf

|   "+" -> mkt ofs PLUS lexbuf
|   "-" -> mkt ofs MINUS lexbuf
|   "*" -> mkt ofs STAR lexbuf
|   "/" -> mkt ofs SLASH lexbuf

|   "(" -> mkt ofs LPAREN lexbuf
|   ")" -> mkt ofs RPAREN lexbuf
|   "," -> mkt ofs COMMA lexbuf
|   ":" -> mkt ofs COLON lexbuf

|   "&" -> scan_xlf ofs lexbuf



  let find_dec_keyword =
    let keyword_list =
      [
       "alias",             DEC_ALIAS;
       "assume",            DEC_ASSUME;
       "assume_aligned",    DEC_ASSUME_ALIGNED;
       "attributes",        DEC_ATTRIBUTES;
       "block_loop",        DEC_BLOCK_LOOP;
       "noblock_loop",      DEC_NOBLOCK_LOOP;
       "code_align",        DEC_CODE_ALIGN;
       "declare",           DEC_DECLARE;
       "nodeclare",         DEC_NODECLARE;
       "define",            DEC_DEFINE;
       "undefine",          DEC_UNDEFINE;
       "distribute",        DEC_DISTRIBUTE;
       "point",             DEC_POINT;
       "fixedformlinesize", DEC_FIXEDFORMLINESIZE;
       "fma",               DEC_FMA;
       "nofma",             DEC_NOFMA;
       "freeform",          DEC_FREEFORM;
       "nofreeform",        DEC_NOFREEFORM;
       "ident",             DEC_IDENT;
       "if",                DEC_IF;
       "defined",           DEC_DEFINED;
       "inline",            DEC_INLINE;
       "forceinline",       DEC_FORCEINLINE;
       "noinline",          DEC_NOINLINE;
       "integer",           DEC_INTEGER;
       "ivdep",             DEC_IVDEP;
       "loop",              DEC_LOOP;
       "count",             DEC_COUNT;
       "message",           DEC_MESSAGE;
       "nofusion",          DEC_NOFUSION;
       "objcomment",        DEC_OBJCOMMENT;
       "optimize",          DEC_OPTIMIZE;
       "nooptimize",        DEC_NOOPTIMIZE;
       "options",           DEC_OPTIONS;
       "pack",              DEC_PACK;
       "parallel",          DEC_PARALLEL;
       "noparallel",        DEC_NOPARALLEL;
       "prefetch",          DEC_PREFETCH;
       "noprefetch",        DEC_NOPREFETCH;
       "psect",             DEC_PSECT;
       "real",              DEC_REAL;
       "simd",              DEC_SIMD;
       "strict",            DEC_STRICT;
       "nostrict",          DEC_NOSTRICT;
       "unroll",            DEC_UNROLL;
       "nounroll",          DEC_NOUNROLL;
       "unroll_and_jam",    DEC_UNROLL_AND_JAM;
       "nounroll_and_jam",  DEC_NOUNROLL_AND_JAM;
       "vector",            DEC_VECTOR;
       "novector",          DEC_NOVECTOR;

       "distributepoint",   DEC_DISTRIBUTEPOINT;
       "loopcount",         DEC_LOOPCOUNT;
       "ifdefined",         DEC_IFDEFINED;
       "elseif",            DEC_ELSEIF;
       "else",              DEC_ELSE;
       "endif",             DEC_ENDIF;
       "endoptions",        DEC_ENDOPTIONS;

       "always",            DEC_ALWAYS;
       "assert",            DEC_ASSERT;
       "aligned",           DEC_ALIGNED;
       "unaligned",         DEC_UNALIGNED;
       "temporal",          DEC_TEMPORAL;
       "nontemporal",       DEC_NONTEMPORAL;
       "vecremainder",      DEC_VECREMAINDER;
       "novecremainder",    DEC_NOVECREMAINDER;
       "noassert",          DEC_NOASSERT;
       "firstprivate",      DEC_FIRSTPRIVATE;
       "lastprivate",       DEC_LASTPRIVATE;
       "linear",            DEC_LINEAR;
       "private",           DEC_PRIVATE;
       "reduction",         DEC_REDUCTION;
       "vectorlength",      DEC_VECTORLENGTH;
       "vectorlengthfor",   DEC_VECTORLENGTHFOR;
       "num_threads",       DEC_NUM_THREADS;
       "endoptions",        DEC_ENDOPTIONS;

       "end",     DEC_END;
       "align",   DEC_ALIGN;
       "noalign", DEC_NOALIGN;
       "wrt",     DEC_WRT;
       "nowrt",   DEC_NOWRT;
       "warn",    DEC_WARN;
       "offload_attribute_target", DEC_OFFLOAD_ATTRIBUTE_TARGET;
       "alignment",   DEC_ALIGNMENT;
       "noalignment", DEC_NOALIGNMENT;
       "factor", DEC_FACTOR;
       "level", DEC_LEVEL;

       "integer",         KINDED_TYPE_SPEC "integer";
       "real",            KINDED_TYPE_SPEC "real";
       "complex",         KINDED_TYPE_SPEC "complex";

      ] in
    let keyword_table = Hashtbl.create (List.length keyword_list) in
    let _ =
      List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        keyword_list
    in
    let find s =
      try
        Hashtbl.find keyword_table (String.lowercase_ascii s)
      with
        Not_found -> IDENTIFIER s
    in
    find

  let rec scan_dec ofs = lexer
|   white_space -> scan_dec ofs lexbuf

|   line_terminator -> mkt ofs EOL lexbuf

|   name ->
    let s = Ulexing.utf8_lexeme lexbuf in
    mkt ofs (find_dec_keyword s) lexbuf

|   digit_string -> mkt ofs (INT_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   dotted_op -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf
|   dotted_identifier -> mkt ofs (find_dotted_keyword (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   char_literal_constant_no_kind -> mkt ofs (CHAR_LITERAL (Ulexing.utf8_lexeme lexbuf)) lexbuf

|   "**"  -> mkt ofs STAR_STAR lexbuf
|   "=="  -> mkt ofs EQ_EQ lexbuf
|   "/="  -> mkt ofs SLASH_EQ lexbuf
|   ">="  -> mkt ofs GT_EQ lexbuf
|   "<="  -> mkt ofs LT_EQ lexbuf
|   "::"  -> mkt ofs COLON_COLON lexbuf

|   "<" -> mkt ofs LT lexbuf
|   "=" -> mkt ofs EQ lexbuf
|   ">" -> mkt ofs GT lexbuf

|   "+" -> mkt ofs PLUS lexbuf
|   "-" -> mkt ofs MINUS lexbuf
|   "*" -> mkt ofs STAR lexbuf
|   "/" -> mkt ofs SLASH lexbuf

|   "(" -> mkt ofs LPAREN lexbuf
|   ")" -> mkt ofs RPAREN lexbuf
|   "," -> mkt ofs COMMA lexbuf
|   ":" -> mkt ofs COLON lexbuf

|   "&" -> scan_dec ofs lexbuf


  type pp_define_stat = D_id | D_params | D_body | D_finished


  let add_comment_region loc =
    if env#loc_stack_level = 0 then
      env#comment_regions#add loc



  let rec mktok
      ?(force_max_line_length=None)
      ?(set_token_feeded=true)
      ?(pending=false)
      ?(start_opt=None)
      ?(end_opt=None)
      rawtok ulexbuf
      =
    let pos_mgr = env#current_pos_mgr in
    let st = 
      match start_opt with
      | None -> Ulexing.lexeme_start ulexbuf 
      | Some x -> x
    in
    let ed = 
      match end_opt with
      | None -> (Ulexing.lexeme_end ulexbuf) - 1 
      | Some x -> x
    in
    let sl, sc = pos_mgr#get_position st in
    let src = env#current_source in
    let max_line_length =
      match force_max_line_length with
      | Some n -> begin
          DEBUG_MSG "max_line_length: %d (forced)" n;
          n
      end
      | None -> begin
          DEBUG_MSG "max_line_length: %d" src#max_line_length;
          src#max_line_length
      end
    in

    if src#is_fixed_source_form && sc >= max_line_length && rawtok <> EOL then begin
      DEBUG_MSG "discarding %s (beyond source line (length=%d))" 
	(Token.rawtoken_to_string rawtok) max_line_length;
      _token ulexbuf
    end
    else begin
      let st_pos = Loc.mklexpos ~fname:pos_mgr#filename ~lnum:sl ~bol:(st-sc) st in
      let ed_pos = ofs_to_pos ed in

      begin
	match rawtok with
	| EOL | RAW _ -> ()
	| _ -> 
            env#set_line_stat_nonblank;
            if set_token_feeded then
              env#set_token_feeded
      end;

      let t = make_qtoken rawtok st_pos ed_pos in

      if not pending then begin
        DEBUG_MSG "%s" (Token.qtoken_to_string t);
        env#set_last_lex_qtoken_obj (Obj.repr t)
      end;

      t

    end

  and mklab lab_opt lexbuf =
    match lab_opt with
    | Some l -> l
    | _ -> mklabel "" lexbuf


  and feed_pending_EOL pending_EOL lexbuf = 
    match pending_EOL with
    | Some t -> 
        DEBUG_MSG "pending_EOL: %s" (Token.qtoken_to_string t);
        env#set_last_lex_qtoken_obj (Obj.repr t);
        t
    | _ -> 
        DEBUG_MSG "pending_EOL: None";
        _token lexbuf

  and discard_pending_RAWOMP() =
    DEBUG_MSG "queue length: %d" env#pending_RAWOMP_obj_queue_length;
    try
      while true do
        let tok, loc = Obj.obj env#take_pending_RAWOMP_obj in
        let line = 
          match tok with
          | RAW {DL.tag=DL.OMP; DL.line=l} -> l
          | _ -> assert false
        in
        parse_warning_loc loc "ignoring OpenMP directive in continued line \"%s\"" line
      done
    with
      Queue.Empty -> ()

  and queue_pending_RAWOMP() =
    DEBUG_MSG "queue length: %d" env#pending_RAWOMP_obj_queue_length;
    try
      while true do
        env#add_pending_token_obj env#take_pending_RAWOMP_obj
      done
    with
      Queue.Empty -> ()

  and rollback lexbuf =
    DEBUG_MSG "ROLLBACK!";
    Ulexing.rollback lexbuf

  and token ?(pending_EOL=None) lexbuf = (* entry point *)

    let pending_EOL =
      match pending_EOL with
      | Some _ -> pending_EOL
      | None ->
          try
            let t = Obj.obj env#take_pending_EOL_obj in
            Some t
          with
            Not_found -> None
    in

    DEBUG_MSG "BOL=%B token_feeded=%B BOS=%B line_stat=%s continued=%B%s" 
      env#at_BOL env#token_feeded env#at_BOS 
      (LineStat.to_string env#line_stat) env#continued 
      (opt_to_string Token.qtoken_to_string ~prefix:" pending_EOL:" pending_EOL);

    DEBUG_MSG "env#in_name_context=%B in_type_spec_context=%B" 
      env#in_name_context env#in_type_spec_context;

    begin
      match pending_EOL with
      | None -> queue_pending_RAWOMP()
      | Some _ -> ()
    end;

    let normal() =
      env#reset_lex_mode;

      if env#token_feeded then begin
        env#clear_BOL;
        if env#at_BOCL then
          env#clear_BOCL
      end;

      if is_fixed_source_form() then begin (* fixed source form *)
        DEBUG_MSG "fixed source form";
        if env#at_BOL then begin
          if env#at_BOS then begin
            env#clear_BOS;
            _token lexbuf
          end
          else
	    label_field ~pending_EOL None 1 lexbuf
        end
        else begin
	  if env#at_BOS then begin
	    env#clear_BOS
	  end;
          env#set_line_stat_nonblank;
	  _token lexbuf
        end
      end
      else begin (* free source form *)
        DEBUG_MSG "free source form";
        if env#continued then begin
	  _token lexbuf
        end
        else begin (* fixed form src may include free form src*)
	  if env#at_BOL then begin
            env#enter_free_line;
	    label ~pp_pending_EOL:pending_EOL lexbuf
          end
	  else
	    _token lexbuf
        end
      end
    in (* normal *)

    match env#lex_mode with
    | PA.LEX_NORMAL -> begin
        DEBUG_MSG "LEX MODE: NORMAL";
        normal()
    end
    | PA.LEX_QUEUE -> begin
        DEBUG_MSG "LEX MODE: QUEUE";
        if env#pending_token_obj_queue_length > 0 then begin
          let t = Obj.obj env#take_pending_token_obj in
          let t' = conv_pp_token t in
          DEBUG_MSG "taking from queue: %s" (Token.qtoken_to_string t');
          env#set_last_lex_qtoken_obj (Obj.repr t');
          t'
        end
        else
          normal()
    end
    | PA.LEX_QUEUE_THEN_DO f -> begin
        DEBUG_MSG "LEX MODE: QUEUE_THEN_DO";
        if env#pending_token_obj_queue_length > 0 then begin
          let t = Obj.obj env#take_pending_token_obj in
          let t' = conv_pp_token t in
          DEBUG_MSG "taking from queue: %s" (Token.qtoken_to_string t');
          env#set_last_lex_qtoken_obj (Obj.repr t');
          t'
        end
        else begin
          env#reset_lex_mode;
          Obj.obj (f())
        end
    end

(*
  and _token lexbuf = 
    DEBUG_MSG "line_stat=%s so=%d" 
      (LineStat.to_string env#line_stat) ((mkloc lexbuf).Loc.start_offset);
    inst0#start;
    let res = __token lexbuf in
    inst0#stop;
    res
*)

  and _token
      ?(pp_pending_EOL=None)
      ?(identifier_may_continue=false)
      ?(hollerith_may_continue=false)
      = lexer

|   white_space -> _token lexbuf

|   line_terminator ->
    DEBUG_MSG "LINE_TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    
    if is_fixed_source_form() then begin (* fixed source form *)
      env#set_BOL;
      env#set_continuable;
      token ~pending_EOL:(Some (mktok ~pending:true EOL lexbuf)) lexbuf
    end
    else begin (* free source form *)
      DEBUG_MSG "amp_line=%B" env#amp_line;
      DEBUG_MSG "continued=%B" env#continued;
      if env#amp_line then begin
        env#set_BOL;
	env#clear_amp_line;
        env#set_BOCL;
	_token ~pp_pending_EOL lexbuf
      end
      else begin
        env#clear_continued;
        match env#line_stat with
        | LineStat.AssumedBlank
        | LineStat.PureComment -> 
            env#set_BOL; 
            _token ~pp_pending_EOL lexbuf

        | _ -> 
            env#set_BOL; 
            mktok EOL lexbuf
      end
    end

|   '!' ->
    DEBUG_MSG "COMMENT (!) [%s] (BOL=%B, BOS=%B)" (Loc.to_string (mkloc lexbuf)) env#at_BOL env#at_BOS;
    if is_fixed_source_form() then
      env#set_continuable;
    line_comment "!" (env#at_BOL || env#at_BOS) (Ulexing.lexeme_start lexbuf) lexbuf

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /*";
    block_comment ~pp_pending_EOL ~identifier_may_continue (Ulexing.lexeme_start lexbuf) lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /**/";
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    _token ~pp_pending_EOL ~identifier_may_continue lexbuf 


|   pp_identifier -> 
    begin
      let s = Ulexing.utf8_lexeme lexbuf in
      DEBUG_MSG "PP_IDENTIFIER(%s)" s;
      env#clear_BOS;
      try
        let body = env#lex_find_macro s in
        match body with
        | Macro.Object line -> mktok (PP_MACRO_ID(Macro.K_GENERAL, s)) lexbuf
        | Macro.Function(params, line) ->
            let start_opt = Some (Ulexing.lexeme_start lexbuf) in
            let args, ed = pp_macro_arguments 0 [] "" lexbuf in
            mktok ~start_opt ~end_opt:(Some ed) (PP_MACRO_APPL(s, args)) lexbuf
      with
        Not_found ->
          if identifier_may_continue then
            mktok (CONTINUED_IDENTIFIER s) lexbuf
          else
            mktok (PP_IDENTIFIER (Ulexing.utf8_lexeme lexbuf)) lexbuf
    end

|   data_edit_desc ->
    env#clear_BOS;
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "DATA_EDIT_DESC(%s)" s;
    if env#in_format_context || startswith_digit s || String.contains s '.' then
      mktok (DATA_EDIT_DESC s) lexbuf
    else
      mktok (find_keyword s) lexbuf

|   kP_desc -> 
    env#clear_BOS;
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "KP_DESC(%s)" s;
    mktok (KP_DESC s) lexbuf

|   position_edit_desc0 -> 
    env#clear_BOS;
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "POSITION_EDIT_DESC0(%s)" s;
    if env#in_format_context then
      mktok (POSITION_EDIT_DESC s) lexbuf
    else
      mktok (find_keyword s) lexbuf

|   position_edit_desc1 -> 
    env#clear_BOS;
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "POSITION_EDIT_DESC1(%s)" s;
    mktok (POSITION_EDIT_DESC s) lexbuf

|   cH_desc -> 
    env#clear_BOS;
    begin
      env#current_source#set_spec_F90;
      let cH = Ulexing.utf8_lexeme lexbuf in
      DEBUG_MSG "H_DESC(%s)" cH;
      let n_str = Xstring.rstrip ~strs:["H"; "h"] cH in
      try
	let n = int_of_string n_str in
	if n < 1 then
	  invalid_arg "cH_desc"
	else
          let chlen = String.length cH in
	  hollerith (Ulexing.lexeme_start lexbuf) chlen n 1 "" lexbuf
      with
      | Failure _ | Invalid_argument _ ->
          mktok (CONTINUED_IDENTIFIER cH) lexbuf
    end

|   '_' int_literal_constant -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "_INT_LITERAL_CONSTANT(%s)" s;
    mktok (CONTINUED_IDENTIFIER s) lexbuf

|   int_literal_constant     -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "INT_LITERAL_CONSTANT(%s)" s;
    if identifier_may_continue then
      mktok (CONTINUED_IDENTIFIER s) lexbuf
    else
      mktok (INT_LITERAL s) lexbuf

|   real_literal_constant    -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "REAL_LITERAL_CONSTANT(%s)" s;
    mktok (REAL_LITERAL s) lexbuf

|   logical_literal_constant -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "LOGICAL_LITERAL_CONSTANT(%s)" s;
    mktok (LOGICAL_LITERAL s) lexbuf

|   boz_literal_constant     -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "BOZ_LITERAL_CONSTANT(%s)" s;
    mktok (BOZ_LITERAL (normalize_continued_string s)) lexbuf

|   char_start_single ->
    env#enter_char_single;
    char_single (Ulexing.lexeme_start lexbuf) "" lexbuf

|   char_start_double ->
    env#enter_char_double;
    char_double (Ulexing.lexeme_start lexbuf) "" lexbuf


|   "**"  -> mktok STAR_STAR lexbuf
|   "=="  -> mktok EQ_EQ lexbuf
|   "/="  -> mktok SLASH_EQ lexbuf
|   ">="  -> mktok GT_EQ lexbuf
|   "<="  -> mktok LT_EQ lexbuf

|   "//"  -> mktok SLASH_SLASH lexbuf

|   "=>"  -> mktok EQ_GT lexbuf
|   "::"  -> mktok COLON_COLON lexbuf
(*
|   "(/"  -> mktok LPAREN_SLASH lexbuf
*)
|   "/)"  -> env#lex_exit_paren_context; mktok SLASH_RPAREN lexbuf

|   "&&"  -> mktok PP_AND lexbuf
|   "||"  -> mktok PP_OR lexbuf

|   "##"  -> mktok PP_CONCAT lexbuf

|   dotted_op -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "DOTTED_OP(%s)" s;
    mktok (find_dotted_keyword s) lexbuf

|   "$" -> mktok DOLLAR lexbuf
|   "%" -> mktok PERCENT lexbuf
|   "\\" -> mktok BACKSLASH lexbuf

|   "&" -> 
    DEBUG_MSG "&";
    if is_free_source_form() then begin (* free source form *)
      if env#at_BOL then begin
	env#clear_BOL;
        env#clear_amp_line
      end
      else begin
        env#set_amp_line;
	env#set_continued
      end;
      _token ~pp_pending_EOL lexbuf
    end
    else (* fixed source form *)
      mktok ~force_max_line_length:(Some 72) AMP lexbuf

|   "(" -> env#lex_enter_paren_context; mktok LPAREN lexbuf
|   ")" -> env#lex_exit_paren_context; mktok RPAREN lexbuf

|   "[" -> mktok LBRACKET lexbuf
|   "]" -> mktok RBRACKET lexbuf

|   "*" -> mktok STAR lexbuf
|   "/" -> mktok SLASH lexbuf
|   "+" -> mktok PLUS lexbuf
|   "-" -> mktok MINUS lexbuf

|   "," -> mktok COMMA lexbuf
|   "." -> mktok DOT lexbuf
|   ":" -> mktok COLON lexbuf

|   ";" -> 
    if is_fixed_source_form() then
      env#set_BOS; 
    mktok SEMICOLON lexbuf

|   "<" -> mktok LT lexbuf
|   "=" -> mktok EQ lexbuf
|   ">" -> mktok GT lexbuf
|   "?" -> mktok QUESTION lexbuf

|   pp_keyword -> 
    let kwd = Ulexing.utf8_lexeme lexbuf in
    let loc = mkloc lexbuf in
    DEBUG_MSG "DIRECTIVE (%s) [%s]" kwd (Loc.to_string loc);
    begin
      try
        let tok = find_pp_keyword kwd in

        if pp_is_QCC_keyword tok then
          env#current_source#omp_cc_lines#add_QCC loc.Loc.start_line;

        let get_st_pos() =
          let st = Ulexing.lexeme_start lexbuf in
          ofs_to_pos st
        in
        begin
          match tok with
          | PP_INCLUDE -> 
              env#clear_BOS;
              pp_include_filename_start pp_pending_EOL (get_st_pos()) lexbuf

          | PP_DEFINE -> pp_define (get_st_pos()) "" None "" (Ulexing.lexeme_start lexbuf) D_id lexbuf

          | PP_UNDEF -> pp_undef (get_st_pos()) "" D_id lexbuf

          | PP_IF -> pp_if (get_st_pos()) "" lexbuf

          | PP_ELIF -> pp_if ~elif:true (get_st_pos()) "" lexbuf

          | PP_IFDEF -> pp_ifdef (get_st_pos()) "" lexbuf

          | PP_IFNDEF -> pp_ifdef ~ndef:true (get_st_pos()) "" lexbuf

          | PP_WARNING -> pp_line (fun m -> PP_ISSUE__MESG (PPD.Warning m)) (get_st_pos()) "" lexbuf

          | PP_ERROR -> pp_line (fun m -> PP_ISSUE__MESG (PPD.Error m)) (get_st_pos()) "" lexbuf

          | PP_UNKNOWN -> pp_line (fun r -> PP_UNKNOWN__REST(kwd, r)) (get_st_pos()) "" lexbuf

          | PP_ELSE -> pp_else (get_st_pos()) lexbuf

          | PP_ENDIF -> pp_endif (get_st_pos()) lexbuf

          | _ -> assert false
        end
      with
        Not_found ->
          WARN_MSG "unknown directive: %s" kwd;
          pp_skip lexbuf
    end

|   pp_out ->
    BEGIN_DEBUG
      let line = Ulexing.utf8_lexeme lexbuf in
      let loc = mkloc lexbuf in
      DEBUG_MSG "PP OUTPUT LINE (%s) (BOL=%B) [%s]" line env#at_BOL (Loc.to_string loc);
    END_DEBUG;
(*    parse_warning_loc loc "ignoring pp output line: %s" line; *)
    pp_skip lexbuf


|   at_process -> 
    let d = Ulexing.utf8_lexeme lexbuf in
    xlf "" env#at_BOL (Ulexing.lexeme_start lexbuf) d lexbuf

|   '%' kw_include white_space+ ->
    let kwd = Ulexing.utf8_lexeme lexbuf in
    let st = Ulexing.lexeme_start lexbuf in
    let tok =
      let _id = Xstring.rstrip kwd in
      let _id_len = String.length _id in
      let id_len = _id_len - 1 in
      let id = String.sub _id 1 id_len in
      let perc_pos = ofs_to_pos st in
      let st_pos = ofs_to_pos (st+1) in
      let ed_pos = ofs_to_pos (st+id_len) in
      let ptok = make_qtoken PERCENT perc_pos perc_pos in
      let itok = make_qtoken (IDENTIFIER id) st_pos ed_pos in
      COMPOSITE_IDENTIFIER(true, kwd, [Obj.repr ptok;Obj.repr itok])
    in
    if env#at_BOL then begin
      DEBUG_MSG "INCLUDE LINE (%s) [%s]" kwd (Loc.to_string (mkloc lexbuf));
      env#clear_BOS;
      let compo_token = mktok ~set_token_feeded:false tok lexbuf in
      try
        include_filename_start (ofs_to_pos st) lexbuf
      with
        Ulexing.Error ->
          rollback lexbuf;
          env#set_token_feeded;
          compo_token
    end
    else begin
      env#clear_BOL;
      env#clear_BOS;
      mktok tok lexbuf
    end

|   kw_include ->
    let kwd = Ulexing.utf8_lexeme lexbuf in
    if env#at_BOL then begin
      DEBUG_MSG "INCLUDE LINE (%s) [%s]" kwd (Loc.to_string (mkloc lexbuf));
      let st = Ulexing.lexeme_start lexbuf in
      env#clear_BOS;
      let ident_token = mktok ~set_token_feeded:false (IDENTIFIER kwd) lexbuf in
      try
        include_filename_start (ofs_to_pos st) lexbuf
      with
        Ulexing.Error ->
          rollback lexbuf;
          env#set_token_feeded;
          ident_token
    end
    else begin
      env#clear_BOL;
      env#clear_BOS;
      mktok (IDENTIFIER kwd) lexbuf
    end

|   kw_options ->
    let kwd = Ulexing.utf8_lexeme lexbuf in
    if env#at_BOL then begin
      DEBUG_MSG "OPTIONS LINE (%s) [%s]" kwd (Loc.to_string (mkloc lexbuf));
      let st = Ulexing.lexeme_start lexbuf in
      env#clear_BOS;
      let ident_token = mktok ~set_token_feeded:false (IDENTIFIER kwd) lexbuf in
      try
        options_line (ofs_to_pos st) "" lexbuf
      with
        Ulexing.Error ->
          rollback lexbuf;
          env#set_token_feeded;
          ident_token
    end
    else begin
      env#clear_BOL;
      env#clear_BOS;
      mktok (IDENTIFIER kwd) lexbuf
    end

|   name -> 
    begin
      let s = Ulexing.utf8_lexeme lexbuf in
      DEBUG_MSG "NAME(%s)" s;

      let hollerith_num =
        if
          (s.[0] = 'h' || s.[0] = 'H') &&
          is_fixed_source_form() &&
          hollerith_may_continue
        then begin
          let last_tok, _ = Obj.obj env#get_last_lex_qtoken_obj in
          match last_tok with
          | INT_LITERAL i -> begin
              try
                let n = int_of_string i in
                if n > 0 then
                  Some n
                else
                  None
              with
                _ -> None
          end
          | _ -> None
        end
        else
          None
      in
      begin
        match hollerith_num with
        | Some n -> begin
            let chlen = String.length s in
            let len = chlen - 1 in
            let ini = String.sub s 1 len in
            DEBUG_MSG "hollerith: %dH%s" n ini;
            hollerith ~partial:true (Ulexing.lexeme_start lexbuf) chlen n chlen ini lexbuf
        end
        | None -> begin
            env#clear_BOS;

            let start_opt = Some (Ulexing.lexeme_start lexbuf) in

            try
              let body = env#lex_find_macro s in

              DEBUG_MSG "macro found \"%s\" --> %s" s (Macro.body_to_string body);

              match body with
              | Macro.Object line -> begin
                  try
                    mktok (Macro.tok_of_line line) lexbuf
                  with
                    Not_found -> mktok (PP_MACRO_ID(Macro.K_GENERAL, s)) lexbuf
              end
              | Macro.Function(params, line) ->
                  let is_app = pre_pp_macro_arguments lexbuf in
                  if is_app then
                    let args, ed = pp_macro_arguments 0 [] "" lexbuf in
                    let rt = PP_MACRO_APPL(s, args) in
                    mktok ~start_opt ~end_opt:(Some ed) rt lexbuf
                  else
                    raise Not_found
            with
              Not_found -> begin

                if identifier_may_continue then begin
                  mktok ~start_opt (CONTINUED_IDENTIFIER s) lexbuf
                end
                else begin
                  let last_qtoken = Obj.obj env#get_last_lex_qtoken_obj in
                  DEBUG_MSG "last_qtoken=%s" (Token.qtoken_to_string last_qtoken);

                  let last_tok = Token.qtoken_to_rawtoken last_qtoken in

                  try
                    match last_tok with
                    | PERCENT -> mktok ~start_opt (IDENTIFIER s) lexbuf

                    | LPAREN | INTENT_SPEC _ -> begin
                        let rawtok = _find_keyword s in
                        mktok ~start_opt rawtok lexbuf
                    end
                    | _ -> begin
                        let rawtok = _find_keyword s in
                        match rawtok with
                        | INTENT_SPEC _ -> mktok ~start_opt (IDENTIFIER s) lexbuf
                        | _ -> mktok ~start_opt rawtok lexbuf
                    end
                  with
                    Not_found -> begin

                      let is_typeof =
                        match last_tok with
                        | QUESTION -> (String.lowercase_ascii s) = "typeof"
                        | _ -> false
                      in

                      if is_typeof then begin
                        mktok ~start_opt (LINDA_TYPEOF s) lexbuf
                      end
                      else if is_fixed_source_form() then begin
	                if env#lex_in_paren_context then
	                  mktok ~start_opt (IDENTIFIER s) lexbuf
                        else
                          mktok ~start_opt (tokenize_name s) lexbuf
                      end
	              else
                        mktok ~start_opt (IDENTIFIER s) lexbuf
                    end

                end (* else *)

              end (* Not_found -> *)

        end

      end

    end (* name -> *)


|   eof -> 
    begin
      if is_free_source_form() then begin
        if env#at_BOS then begin
          let last_tok, _ = Obj.obj env#get_last_lex_qtoken_obj in
          match last_tok with
          | EOL -> env#clear_BOS
          | _ -> ()
        end
      end;
      try
        let eol_qtoken = Obj.obj env#take_pending_EOL_obj in
        env#set_lex_mode_queue;
        eol_qtoken
      with
        Not_found ->
          let ofs = (Ulexing.lexeme_end lexbuf) - 1 in
          let loc = env#current_pos_mgr#offsets_to_loc ofs ofs in
          EOF None, loc
    end

|   pp_underscore ->
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "pp underscore \"%s\"" s;
    mktok (PP_UNDERSCORE s) lexbuf

|   _ -> 
    DEBUG_MSG "invalid symbol \"%s\"" (Ulexing.utf8_lexeme lexbuf);
    parse_warning_loc (mkloc lexbuf) "ignoring invalid symbol \"%s\"" (Ulexing.utf8_lexeme lexbuf);
    _token ~pp_pending_EOL lexbuf


  and options_line st_pos str = lexer
|   line_terminator ->
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let tok = make_qtoken (OPTIONS__OPTS str) st_pos ed_pos in
    tok

|   '/' name ('=' name)? ->
    let opt = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "OPTION (%s) [%s]" opt (Loc.to_string (mkloc lexbuf));
    options_line st_pos (str^opt) lexbuf

|   white_space+ ->
    options_line st_pos str lexbuf

  and char_single st str = lexer
|   "''" -> char_single st (str^"'") lexbuf
|   "\"\"" -> char_single st (str^"\"") lexbuf

|   '\'' '&' white_space* line_terminator white_space* '&' '\'' -> 
    char_single st (str^"'") lexbuf
|   '"' '&' white_space* line_terminator white_space* '&' '"' -> 
    char_single st (str^"\"") lexbuf

|   rep_char_non_single_quote -> 
    char_single st (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '&' white_space* line_terminator -> 
    DEBUG_MSG "CHARACTER CONTEXT CONTINUATION!";
    let is_CC =
      let ln, _ = env#current_pos_mgr#get_position st in
      env#current_source#omp_cc_lines#is_CC_line ln
    in
    DEBUG_MSG "is_CC: %B" is_CC;
    skip_char_single ~is_CC st str lexbuf
(*
|   line_terminator [^'C' 'c' '*' 'D' 'd'] _ _ _ _ [^' ' '\009' '\012' '0'] ->
    char_single st str lexbuf
*)
|   line_terminator ->
    env#set_BOL;
    mktok ~set_token_feeded:false ~start_opt:(Some st) (CHAR_LITERAL str) lexbuf

|   '\'' 'C'? -> 
    env#exit_char;
    mktok ~start_opt:(Some st) (CHAR_LITERAL str) lexbuf

  and skip_char_single ?(is_CC=false) st str = lexer
|   white_space -> skip_char_single ~is_CC st str lexbuf
|   '&' -> char_single st str lexbuf
|   "!$" -> 
    if is_CC then begin
      let loc = mkloc lexbuf in
      env#current_source#omp_cc_lines#add loc.Loc.start_line;
      skip_char_single st str lexbuf
    end
    else begin
      rollback lexbuf;
      char_single st str lexbuf
    end
|   _ ->
    (*parse_warning_loc (mkloc lexbuf) "missing '&' in continued character constant";*)
    (* Intel? *)
    rollback lexbuf;
    char_single st str lexbuf


  and char_double st str = lexer
|   "''" -> char_double st (str^"'") lexbuf
|   "\"\"" -> char_double st (str^"\"") lexbuf

|   '\'' '&' white_space* line_terminator white_space* '&' '\'' -> char_double st (str^"'") lexbuf
|   '"' '&' white_space* line_terminator white_space* '&' '"' -> char_double st (str^"\"") lexbuf

|   rep_char_non_double_quote -> char_double st (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '&' white_space* line_terminator -> 
    DEBUG_MSG "CHARACTER CONTEXT CONTINUATION!";
    let is_CC =
      let ln, _ = env#current_pos_mgr#get_position st in
      env#current_source#omp_cc_lines#is_CC_line ln
    in
    DEBUG_MSG "is_CC: %B" is_CC;
    skip_char_double ~is_CC st str lexbuf
(*
|   line_terminator [^'C' 'c' '*' 'D' 'd'] _ _ _ _ [^' ' '\009' '\012' '0'] ->
    char_double st str lexbuf
*)
|   line_terminator ->
    env#set_BOL;
    mktok ~set_token_feeded:false ~start_opt:(Some st) (CHAR_LITERAL str) lexbuf

|   '"' 'C'? -> 
    env#exit_char;
    mktok ~start_opt:(Some st) (CHAR_LITERAL str) lexbuf

  and skip_char_double ?(is_CC=false) st str = lexer
|   white_space -> skip_char_double ~is_CC st str lexbuf
|   '&' -> char_double st str lexbuf
|   "!$" -> 
    if is_CC then begin
      let loc = mkloc lexbuf in
      env#current_source#omp_cc_lines#add loc.Loc.start_line;
      skip_char_double st str lexbuf
    end
    else begin
      rollback lexbuf;
      char_double st str lexbuf
    end
|   _ ->
    (*parse_warning_loc (mkloc lexbuf) "missing '&' in continued character constant";*)
    (* Intel? *)
    rollback lexbuf;
    char_double st str lexbuf


  and label ?(pp_pending_EOL=None) lexbuf =
    DEBUG_MSG "pp_pending_EOL: %s" 
      (match pp_pending_EOL with Some e -> Token.qtoken_to_string e | None -> "");
    assert (env#at_BOL);
    let res = _label ~pp_pending_EOL lexbuf in
    res

  and _label ?(pp_pending_EOL=None) = lexer
|   white_space -> label ~pp_pending_EOL lexbuf
    
|   digit_string? -> 
    let lab = (Ulexing.utf8_lexeme lexbuf) in
    DEBUG_MSG "%sDIGIT STRING (%s)" (if lab = "" then "EMPTY " else "") lab;
    if lab <> "" then begin
      let label = mklabel lab lexbuf in
      register_label label;
      env#clear_BOL;
    end;
    env#set_BOS;
    begin
      match pp_pending_EOL with
      | Some t -> begin
          DEBUG_MSG "pp_pending_EOL=%s" (Token.qtoken_to_string t);
          let middle_of_free_form_src =
            let _, last_loc = Obj.obj env#get_last_lex_qtoken_obj in
            let last_path = Fname.strip last_loc.Loc.filename in
            let last_form = env#get_source_form last_path in
            last_form = SF.Free && env#current_source#is_free_source_form
          in
          DEBUG_MSG "middle_of_free_form_src=%B" middle_of_free_form_src;
          if middle_of_free_form_src then begin
            env#set_pending_EOL_obj (Obj.repr t);
            _token ~pp_pending_EOL lexbuf
          end
          else begin
            env#set_lex_mode_queue;
            feed_pending_EOL pp_pending_EOL lexbuf
          end
      end
      | None -> _token ~pp_pending_EOL lexbuf
    end

|   '!' -> 
    DEBUG_MSG "COMMENT (!) [%s] (BOL=%B)" (Loc.to_string (mkloc lexbuf)) env#at_BOL;
    line_comment "!" ~pending_EOL:pp_pending_EOL env#at_BOL (Ulexing.lexeme_start lexbuf) lexbuf

|   ';' -> (* F2008: a line is permitted to begin with a semicolon *)
    DEBUG_MSG "SEMICOLON";
    label ~pp_pending_EOL lexbuf

|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    env#set_BOL;
    label ~pp_pending_EOL lexbuf


  and label_field ?(pending_EOL=None) lab_opt pos = 
    let _ = env#enter_fixed_line in
    let _ = DEBUG_MSG "pos=%d%s" pos (opt_to_string Token.qtoken_to_string ~prefix:" pending_EOL:" pending_EOL) in
    lexer

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/*)";
    block_comment_label ~pending_EOL lab_opt pos (Ulexing.lexeme_start lexbuf) lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/**/)";
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    if pos = 5 then
      continuation_field ~pending_EOL (mklab lab_opt lexbuf) lexbuf
    else
      label_field ~pending_EOL lab_opt (pos+1) lexbuf

|   ['C' 'c' '*' 'D' 'd'] -> (* d and D are non-standard *)
    let sym = Ulexing.utf8_lexeme lexbuf in

    if pos = 1 then begin
      if (sym = "D" || sym = "d") && env#current_source#parse_d_lines then begin
        DEBUG_MSG "DEBUG LINE (%s)" sym;
        label_field ~pending_EOL lab_opt (pos+1) lexbuf        
      end
      else begin
        DEBUG_MSG "COMMENT (%s)" sym;
        env#set_line_stat_pure_comment;
        line_comment sym ~pending_EOL true (Ulexing.lexeme_start lexbuf) lexbuf
      end
    end
    else begin
      DEBUG_MSG "invalid symbol \"%s\"" sym;
      parse_warning_loc (mkloc lexbuf) 
        "invalid symbol \"%s\" in label field: the rest of the line is ignored" sym;
      line_comment sym ~pending_EOL false (Ulexing.lexeme_start lexbuf) lexbuf
    end

|   '!' ->
    DEBUG_MSG "COMMENT (!) (BOL=%B) [%s]" env#at_BOL (Loc.to_string (mkloc lexbuf));
    let pure_comment = env#at_BOL in
    if pure_comment then
      env#set_line_stat_pure_comment
    else
      env#set_line_stat_mixed_comment;
    line_comment "!" ~pending_EOL pure_comment (Ulexing.lexeme_start lexbuf) lexbuf

|   '%'? kw_include ->
    DEBUG_MSG "INCLUDE LINE (%s) [%s]" 
      (Ulexing.utf8_lexeme lexbuf) (Loc.to_string (mkloc lexbuf));
    rollback lexbuf;
    if env#continuable then begin
      env#set_lex_mode_queue;
      feed_pending_EOL pending_EOL lexbuf
    end
    else begin
      _token lexbuf
    end

|   pp_keyword ->
    let d = Ulexing.utf8_lexeme lexbuf in
    let loc = mkloc lexbuf in    
    DEBUG_MSG "DIRECTIVE (%s) (BOL=%B) [%s]" d env#at_BOL (Loc.to_string loc);
    rollback lexbuf;
    begin
      let tok = find_pp_keyword d in

      if pp_is_QCC_keyword tok then
        env#current_source#omp_cc_lines#add_QCC loc.Loc.start_line;

      match tok with
      | PP_INCLUDE ->
          (*if env#continuable then begin
            DEBUG_MSG "continuable";
            env#set_lex_mode_queue;
            feed_pending_EOL pending_EOL lexbuf
          end
          else*) begin
            _token ~pp_pending_EOL:pending_EOL lexbuf
          end

      | _ -> begin
          begin
            match pending_EOL with
            | Some t -> 
                DEBUG_MSG "pending_EOL=%s" (Token.qtoken_to_string t);
                env#set_pending_EOL_obj (Obj.repr t)
            | _ -> ()
          end;
          _token lexbuf
      end
    end

|   pp_out ->
    BEGIN_DEBUG
      let line = Ulexing.utf8_lexeme lexbuf in
      let loc = mkloc lexbuf in
      DEBUG_MSG "PP OUTPUT LINE (%s) (BOL=%B) [%s]" line env#at_BOL (Loc.to_string loc);
    END_DEBUG;
(*    parse_warning_loc loc "ignoring pp output line: %s" line; *)
    pp_skip ~pending_EOL lexbuf

|   white_space -> 
    DEBUG_MSG "WHITE SPACE";
    
    let s = Ulexing.utf8_lexeme lexbuf in
    if s = "\t" then begin
      DEBUG_MSG "TAB found in label field";
      tab_label_field ~pending_EOL (mklab lab_opt lexbuf) lexbuf
    end
    else
      if pos = 5 then
        continuation_field ~pending_EOL (mklab lab_opt lexbuf) lexbuf
      else
        label_field ~pending_EOL lab_opt (pos+1) lexbuf

|   digit_string -> 
    DEBUG_MSG "DIGIT STRING (%s)" (Ulexing.utf8_lexeme lexbuf);
    let len = Ulexing.lexeme_length lexbuf in
    let lab = Ulexing.utf8_lexeme lexbuf in
    let label1 = mklabel lab lexbuf in
    let label = 
      match lab_opt with
      | Some label0 -> merge_label label0 label1
      | None -> label1
    in
    if len + pos < 5 then
      label_field ~pending_EOL (Some label) (pos+len) lexbuf
    else
      continuation_field ~pending_EOL label lexbuf

|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    if pos = 1 then
      env#set_line_stat_pure_comment;
    env#set_BOL; 
    token ~pending_EOL lexbuf

|   eof ->
    DEBUG_MSG "EOF[%s]" (Loc.to_string (mkloc lexbuf));
    begin
      match pending_EOL with
      | Some t -> begin
          if env#in_included_file then begin
            mktok (EOF (Some (Obj.repr t))) lexbuf
          end
          else begin
            env#add_pending_token_obj (Obj.repr (mktok (EOF None) lexbuf));
            env#set_lex_mode_queue;
            t
          end
      end
      | _ -> mktok (EOF None) lexbuf
    end

|   _ -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "invalid symbol \"%s\"" s;
    let loc = mkloc lexbuf in
    parse_warning_loc loc "invalid symbol \"%s\" in label field: the rest of the line is ignored" s;
    env#current_source#omp_cc_lines#remove loc.Loc.start_line;
    line_comment s ~pending_EOL false (Ulexing.lexeme_start lexbuf) lexbuf


  and check_continuation pending_EOL last_tok last_loc =
    let identifier_may_continue, hollerith_may_continue =
      match last_tok with
      | IDENTIFIER _ -> begin
          match pending_EOL with
          | Some (_, loc) ->
              loc.Loc.start_offset = last_loc.Loc.end_offset + 1, false
          | None -> false, false
      end
      | INT_LITERAL _ -> begin
          match pending_EOL with
          | Some (_, loc) ->
              false, loc.Loc.start_offset = last_loc.Loc.end_offset + 1
          | _ -> false, false
      end
      | _ -> false, false
    in
    identifier_may_continue, hollerith_may_continue


  and continuation_field ?(pending_EOL=None) ((lab, loc) as label) = 
    let label_empty = lab = "" in
    let _ = 
      DEBUG_MSG "entering continuation_field: continuable=%B line_stat=%s label=%s [%s]%s" 
	env#continuable (LineStat.to_string env#line_stat)
	(if label_empty then "<none>" else lab) (Loc.to_string loc)
        (opt_to_string Token.qtoken_to_string ~prefix:" pending_EOL:" pending_EOL);
    in
    lexer

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /*";
    block_comment_cont ~pending_EOL label (Ulexing.lexeme_start lexbuf) lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /**/";
    let st, ed = Ulexing.lexeme_start lexbuf, (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    env#set_BOS;

    if not label_empty then begin
      env#set_line_stat_nonblank;
      register_label label
    end;

    if label_empty then
      continuation_field_sub ~pending_EOL lexbuf
    else begin
      if env#continuable then begin
        env#set_lex_mode_queue;
        feed_pending_EOL pending_EOL lexbuf
      end
      else begin
        _token lexbuf
      end
    end

|   '0' | white_space -> 
    let str = Ulexing.utf8_lexeme lexbuf in

    let is_ws = str <> "0" in

    if is_ws then
      DEBUG_MSG "WHITE SPACE"
    else begin
      DEBUG_MSG "0"; 
      env#clear_BOL
    end;

    env#set_BOS;

    if not label_empty then begin
      env#set_line_stat_nonblank;
      register_label label
    end;

    if label_empty && is_ws then
      continuation_field_sub ~pending_EOL lexbuf
    else begin
      if env#continuable then begin
        env#set_lex_mode_queue;
        feed_pending_EOL pending_EOL lexbuf
      end
      else begin
        _token lexbuf
      end
    end

|   line_terminator ->
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    env#set_line_stat_pure_comment;
    env#set_BOL;
    token ~pending_EOL lexbuf

|   _ -> 
    DEBUG_MSG "CONTINUATION! (%s)" (Ulexing.utf8_lexeme lexbuf);
    env#set_line_stat_continued;

    env#clear_BOL;

    begin
      match env#char_context with
      | PA.CH_SINGLE -> char_single (Ulexing.lexeme_start lexbuf) "" lexbuf
      | PA.CH_DOUBLE -> char_double (Ulexing.lexeme_start lexbuf) "" lexbuf
      | PA.CH_NONE -> begin

          let last_qtoken = Obj.obj env#get_last_lex_qtoken_obj in
          DEBUG_MSG "last_qtoken=%s" (Token.qtoken_to_string last_qtoken);

          let last_tok, last_loc = last_qtoken in

          let identifier_may_continue, hollerith_may_continue =
            check_continuation pending_EOL last_tok last_loc
          in
          DEBUG_MSG "identifier_may_continue:%B" identifier_may_continue;
          DEBUG_MSG "hollerith_may_continue:%B" hollerith_may_continue;

          let gen_id, id =
            match last_tok with
            | PP_INCLUDE__FILE h -> begin
                let last_path = Fname.strip last_loc.Loc.filename in
                let cur_path = env#current_source#path in
                let b = last_path = cur_path in
                if b then
                  true, Printf.sprintf "generated for %s" (H.to_rep h)
                else
                  false, ""
            end
            | _ -> false, ""
          in
          DEBUG_MSG "gen_id=%B" gen_id;
          if gen_id then
            PP_IDENTIFIER id, last_loc
          else begin
            discard_pending_RAWOMP();
            env#clear_pending_EOL_obj;
            env#set_lex_mode_queue_then_do 
              (fun () ->
                Obj.repr
                  (_token ~identifier_may_continue ~hollerith_may_continue lexbuf)
              );
            token lexbuf
          end

      end
    end

  and continuation_field_sub ?(pending_EOL=None) = 
    let _ = DEBUG_MSG "continuable:%B" env#continuable in
    lexer
|   white_space -> continuation_field_sub ~pending_EOL lexbuf

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /*";
    block_comment_cont_sub ~pending_EOL (Ulexing.lexeme_start lexbuf) lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT: /**/";
    continuation_field_sub ~pending_EOL lexbuf

|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    env#set_line_stat_pure_comment;
    env#set_BOL;
    env#clear_BOS;
    token ~pending_EOL lexbuf

|   '!' ->
    DEBUG_MSG "COMMENT (!) (BOL=%B) [%s]" env#at_BOL (Loc.to_string (mkloc lexbuf));
    env#set_line_stat_pure_comment;
    env#clear_BOS;
    line_comment "!" ~pending_EOL true (Ulexing.lexeme_start lexbuf) lexbuf

|   _ -> 
    DEBUG_MSG "OTHER (%s)" (Ulexing.utf8_lexeme lexbuf);
    rollback lexbuf;
    if env#continuable then begin
      env#set_lex_mode_queue;
      feed_pending_EOL pending_EOL lexbuf
    end
    else begin
      env#set_continuable;
      _token lexbuf
    end

(* from F77 Reference from Oracle:
   The characters can continue over to a continuation line, but that gets tricky. 
   Short standard fixed format lines are padded on the right with blanks up to 72
   columns, but short tab-format lines stop at the newline.
*)
  and hollerith ?(partial=false) st chlen n i str = lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR";
    if is_fixed_source_form() && env#in_fixed_line then begin
      let src = env#current_source in
      let max_line_length = src#max_line_length in
      let _, sc = env#current_pos_mgr#get_position st in
      let pos = sc + i + chlen in
      DEBUG_MSG "n=%d i=%d sc=%d pos=%d" n i sc pos;
      if pos < max_line_length then begin
        let rest = n - i in
        let a = max_line_length - pos in
        if rest <= a then begin
          let str' = str^(String.make rest ' ') in
          let rt = HOLLERITH(str', partial) in
          let qtok = mktok ~start_opt:(Some st) rt lexbuf in
          rollback lexbuf;
          qtok
        end
        else begin
          let str' = str^(String.make a ' ') in
          hollerith_continuation ~partial st chlen n (i+a) str' 1 lexbuf
        end
      end
      else
        hollerith_continuation ~partial st chlen n i str 1 lexbuf
    end
    else
      hollerith_continuation ~partial st chlen n i str 1 lexbuf
    
|   _ ->
    let s0 = Ulexing.utf8_lexeme lexbuf in
    let len = Ulexing.lexeme_length lexbuf in
    let str' = str^s0 in
    if n = i then
      mktok ~start_opt:(Some st) (HOLLERITH(str', partial)) lexbuf
    else
      hollerith ~partial st chlen n (i+len) str' lexbuf


  and hollerith_continuation ?(partial=false) st chlen n i str pos = 
    let _ = DEBUG_MSG "n=%d i=%d str=\"%s\" pos=%d" n i str pos in
    lexer
|   ['C' 'c' '*' 'D' 'd'] -> 
    if pos = 1 then
      hollerith_skip_line ~partial st chlen n i str lexbuf
    else if pos = 6 then
      hollerith ~partial st chlen n i str lexbuf
    else begin
      let s = Ulexing.utf8_lexeme lexbuf in
      DEBUG_MSG "invalid symbol \"%s\"" s;
      parse_warning_loc (mkloc lexbuf) "ignoring invalid symbol \"%s\" in label field after \"%s\"" s str;
      hollerith_continuation ~partial st chlen n i str (pos+1) lexbuf
    end

|   '0' | white_space ->
    if pos = 6 then begin
      DEBUG_MSG "invalid symbol \"%s\"" (Ulexing.utf8_lexeme lexbuf);
      parse_warning_loc (mkloc lexbuf) "ignoring unexpected white space in continuation field after \"%s\"" str;
      hollerith ~partial st chlen n i str lexbuf
    end
    else
      hollerith_continuation ~partial st chlen n i str (pos+1) lexbuf
|   _ ->
    if pos = 6 then
      hollerith ~partial st chlen n i str lexbuf
    else begin
      let s = Ulexing.utf8_lexeme lexbuf in
      DEBUG_MSG "invalid symbol \"%s\"" s;
      parse_warning_loc (mkloc lexbuf) "ignoring invalid symbol \"%s\" in label field after \"%s\"" s str;
      hollerith_continuation ~partial st chlen n i str (pos+1) lexbuf
    end


  and hollerith_skip_line ?(partial=false) st chlen n i str = lexer
|   line_terminator -> hollerith_continuation ~partial st chlen n i str 1 lexbuf
|   _ -> hollerith_skip_line ~partial st chlen n i str lexbuf
      

  and block_comment ?(pp_pending_EOL=None) ?(identifier_may_continue=false) st = lexer
|   "*/" -> 
    add_comment_region (offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1)); 
    _token ~pp_pending_EOL ~identifier_may_continue lexbuf

|   _ -> block_comment ~pp_pending_EOL ~identifier_may_continue st lexbuf


  and block_comment_label ?(pending_EOL=None) lab_opt pos st = lexer
|   "*/" -> 
    add_comment_region (offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1)); 
    if pos = 5 then
      continuation_field ~pending_EOL (mklab lab_opt lexbuf) lexbuf
    else
      label_field ~pending_EOL lab_opt (pos+1) lexbuf

|   _ -> block_comment_label ~pending_EOL lab_opt pos st lexbuf


  and block_comment_cont ?(pending_EOL=None) ((lab, loc) as label) st = 
    let label_empty = lab = "" in
    lexer
|   "*/" -> 
    add_comment_region (offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1)); 
    env#set_BOS;

    if not label_empty then begin
      env#set_line_stat_nonblank;
      register_label label
    end;

    if label_empty then
      continuation_field_sub ~pending_EOL lexbuf
    else begin
      if env#continuable then begin
        env#set_lex_mode_queue;
        feed_pending_EOL pending_EOL lexbuf
      end
      else begin
        _token lexbuf
      end
    end

|   _ -> block_comment_cont ~pending_EOL label st lexbuf

  and block_comment_cont_sub ?(pending_EOL=None) st = lexer
|   "*/" -> 
    let ed = (Ulexing.lexeme_end lexbuf) - 1 in
    add_comment_region (offsets_to_loc st ed);
    continuation_field_sub ~pending_EOL lexbuf

|   _ -> block_comment_cont_sub ~pending_EOL st lexbuf


  and tab_label_field ?(pending_EOL=None) ((lab, loc) as label) = 
    let label_empty = lab = "" in
    let _ = env#enter_tab_line in
    let _ = 
      DEBUG_MSG "entering tab_label_field: continuable=%B line_stat=%s label=%s [%s]" 
	env#continuable (LineStat.to_string env#line_stat)
	(if label_empty then "<none>" else lab) (Loc.to_string loc)
    in
    lexer

|   ['1'-'9'] -> 
    DEBUG_MSG "CONTINUATION! (%s)" (Ulexing.utf8_lexeme lexbuf);
    env#set_line_stat_continued;

    begin
      match env#char_context with
      | PA.CH_SINGLE -> char_single (Ulexing.lexeme_start lexbuf) "" lexbuf
      | PA.CH_DOUBLE -> char_double (Ulexing.lexeme_start lexbuf) "" lexbuf
      | PA.CH_NONE -> begin

          let last_tok, last_loc = Obj.obj env#get_last_lex_qtoken_obj in

          let identifier_may_continue, hollerith_may_continue =
            check_continuation pending_EOL last_tok last_loc
          in
          DEBUG_MSG "identifier_may_continue:%B" identifier_may_continue;
          DEBUG_MSG "hollerith_may_continue:%B" hollerith_may_continue;

          discard_pending_RAWOMP();
          env#clear_pending_EOL_obj;
          env#set_lex_mode_queue_then_do 
            (fun () ->
              Obj.repr
                (_token ~identifier_may_continue ~hollerith_may_continue lexbuf));
          token lexbuf

      end
    end

|   white_space -> tab_label_field ~pending_EOL label lexbuf

|   "/*" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/*)";
    block_comment_tab ~pending_EOL label (Ulexing.lexeme_start lexbuf) lexbuf

|   "/**/" ->
    DEBUG_MSG "C-STYLE BLOCK COMMENT(/**/)";
    tab_label_field ~pending_EOL label lexbuf

|   '!' ->
    DEBUG_MSG "COMMENT (!) (BOL=%B) [%s]" env#at_BOL (Loc.to_string (mkloc lexbuf));
    env#set_line_stat_pure_comment;
    line_comment "!" ~pending_EOL true (Ulexing.lexeme_start lexbuf) lexbuf

|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));
    if label_empty then
      env#set_line_stat_pure_comment;
    env#set_BOL;
    token ~pending_EOL lexbuf

|   _ -> 
    DEBUG_MSG "OTHER (%s)" (Ulexing.utf8_lexeme lexbuf);
    env#set_BOS;

    if not label_empty then
      register_label label;

    rollback lexbuf;

    if env#continuable then begin
      env#set_lex_mode_queue;
      feed_pending_EOL pending_EOL lexbuf
    end
    else begin
      if not label_empty then
        env#set_line_stat_nonblank;
      _token lexbuf
    end


  and block_comment_tab ?(pending_EOL=None) label st = lexer
|   "*/" -> 
    add_comment_region (offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1)); 
    tab_label_field ~pending_EOL label lexbuf

|   _ -> block_comment_tab ~pending_EOL label st lexbuf


  and line_comment head ?(pending_EOL=None) pure_comment st = 
(*    let _ = DEBUG_MSG "pure_comment=%B st=%d" pure_comment st in*)
    lexer
|   line_terminator -> 
    DEBUG_MSG "LINE TERMINATOR [%s]" (Loc.to_string (mkloc lexbuf));

    let cloc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
    DEBUG_MSG "comment loc: [%s]" (Loc.to_string cloc);

    add_comment_region cloc; 

    env#set_BOL;
    if pure_comment then begin
      token ~pending_EOL lexbuf
    end
    else begin
      if is_free_source_form() then begin
        if env#amp_line then begin
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          env#clear_continued;
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
	token ~pending_EOL:(Some (mktok ~pending:true EOL lexbuf)) lexbuf
      end
    end

|   ocl_head -> 
    let st' = Ulexing.lexeme_start lexbuf in
    DEBUG_MSG "head=%s st=%d st'=%d pure_comment=%B" head st st' pure_comment;
    if head = "!" && st' = st + 1 && pure_comment then begin
      if env#at_BOCL then begin
        let loc = mkloc lexbuf in
        parse_warning_loc loc "ignoring OCL in continued line";
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
      else begin
        DEBUG_MSG "OCL [%s]" (Loc.to_string (mkloc lexbuf));
        ocl ~pending_EOL pure_comment st "" lexbuf
      end
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   xlf_trigger -> 
    let trigger = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "trigger_constant: \"%s\"" trigger;
    let st' = Ulexing.lexeme_start lexbuf in
    DEBUG_MSG "st=%d st'=%d pure_comment=%B" st st' pure_comment;
    if st' = st + 1 && pure_comment then begin
      if env#at_BOCL then begin
        let loc = mkloc lexbuf in
        parse_warning_loc loc "ignoring XLF directive in continued line";
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
      else begin
        DEBUG_MSG "XLF [%s]" (Loc.to_string (mkloc lexbuf));
        xlf trigger ~pending_EOL pure_comment st "" lexbuf
      end
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   dec_prefix -> 
    let prefix = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "directive prefix: \"%s\"" prefix;
    let st' = Ulexing.lexeme_start lexbuf in
    DEBUG_MSG "st=%d st'=%d pure_comment=%B" st st' pure_comment;
    if st' = st + 1 && pure_comment then begin
      if env#at_BOCL then begin
        let loc = mkloc lexbuf in
        parse_warning_loc loc "ignoring DEC directive in continued line";
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
      else begin
        DEBUG_MSG "DEC [%s]" (Loc.to_string (mkloc lexbuf));
        dec prefix ~pending_EOL pure_comment st "" lexbuf
      end
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   '$' -> 
    let st' = Ulexing.lexeme_start lexbuf in
    DEBUG_MSG "checking if OMP or ACC line";
    DEBUG_MSG "st=%d st'=%d pure_comment=%B" st st' pure_comment;
    if st' = st + 1 && pure_comment then begin
      pre_omp head ~pending_EOL pure_comment st lexbuf
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   _ -> line_comment head ~pending_EOL pure_comment st lexbuf



  and pre_omp head ?(pending_EOL=None) pure_comment st = 
    let _ = DEBUG_MSG "pure_comment=%B st=%d" pure_comment st in
    lexer

|   line_terminator ->
    let loc = mkloc lexbuf in
    DEBUG_MSG "OMP Conditional Compilation [%s]" (Loc.to_string loc);
    env#current_source#omp_cc_lines#add loc.Loc.start_line;
    env#set_BOL;
    if is_fixed_source_form() then begin
      token ~pending_EOL lexbuf
    end
    else begin (* free source form *)
      label lexbuf
    end

|   white_space ->
    let last_tok, _ = Obj.obj env#get_last_lex_qtoken_obj in
    let loc = mkloc lexbuf in
    begin
      match last_tok with
      | RAW {DL.tag=DL.OMP; DL.fixed_cont=fixed_cont; DL.free_cont=free_cont} when free_cont -> begin
          DEBUG_MSG "OMP Continued Line [%s]" (Loc.to_string loc);
          env#current_source#omp_cc_lines#add_QCC loc.Loc.start_line;
          omp ~pending_EOL ~offset:0 pure_comment st "" lexbuf
      end
      | _ -> begin
          DEBUG_MSG "OMP Conditional Compilation [%s]" (Loc.to_string loc);
          env#current_source#omp_cc_lines#add loc.Loc.start_line;
          if is_fixed_source_form() then begin
            label_field ~pending_EOL None 4 lexbuf
          end
          else begin (* free source form *)
            label lexbuf
          end
      end
    end

|   digit ->
    let loc = mkloc lexbuf in
    if is_fixed_source_form() then begin
      DEBUG_MSG "OMP Conditional Compilation [%s]" (Loc.to_string loc);
      env#current_source#omp_cc_lines#add loc.Loc.start_line;
      rollback lexbuf;
      label_field ~pending_EOL None 3 lexbuf
    end
    else begin (* free source form *)
      line_comment head ~pending_EOL pure_comment st lexbuf
    end

|   '&' ->
    let loc = mkloc lexbuf in
    if is_fixed_source_form() then begin
      line_comment head ~pending_EOL pure_comment st lexbuf
    end
    else begin (* free source form *)
      if env#at_BOCL then begin
        DEBUG_MSG "OMP Conditional Compilation [%s]" (Loc.to_string loc);
        env#current_source#omp_cc_lines#add loc.Loc.start_line;
        if env#at_BOL then begin
	  env#clear_BOL;
	  env#clear_amp_line
        end;
        _token lexbuf
      end
      else begin
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
    end

|   omp_sentinel -> 
    if (Ulexing.lexeme_start lexbuf) = st + 2 then begin
      if env#at_BOCL then begin
        let loc = mkloc lexbuf in
        parse_warning_loc loc "ignoring OpenMP directive in continued line";
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
      else begin
        let loc = mkloc lexbuf in
        DEBUG_MSG "OMP Directive [%s]" (Loc.to_string loc);
        env#current_source#omp_cc_lines#add_QCC loc.Loc.start_line;
        omp ~pending_EOL pure_comment st "" lexbuf
      end
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   acc_sentinel -> 
    if (Ulexing.lexeme_start lexbuf) = st + 2 then begin
      if env#at_BOCL then begin
        let loc = mkloc lexbuf in
        parse_warning_loc loc "ignoring OpenACC directive in continued line";
        line_comment head ~pending_EOL pure_comment st lexbuf
      end
      else begin
        let loc = mkloc lexbuf in
        DEBUG_MSG "ACC Directive [%s]" (Loc.to_string loc);
        acc ~pending_EOL pure_comment st "" lexbuf
      end
    end
    else
      line_comment head ~pending_EOL pure_comment st lexbuf

|   _ -> line_comment head ~pending_EOL pure_comment st lexbuf


  and ocl ?(pending_EOL=None) pure_comment st line = lexer
|   line_terminator -> 
    BEGIN_DEBUG
      DEBUG_MSG "LINE TERMINATOR [%s] pure_comment=%B" (Loc.to_string (mkloc lexbuf)) pure_comment;
      let loc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      DEBUG_MSG "line: [%s][%s]" line (Loc.to_string loc);
    END_DEBUG;
    env#set_BOL;

    let ocl_qtoken = mktok ~start_opt:(Some st) (RAW (DL.mkocl line)) lexbuf in

    if pure_comment then begin
      begin
        match pending_EOL with
        | Some _ -> 
            env#add_pending_token_obj (Obj.repr ocl_qtoken);
            token ~pending_EOL lexbuf
              
        | _ -> ocl_qtoken
      end
    end
    else
      if is_free_source_form() then begin
        if env#amp_line then begin (* impossible? *)
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          DEBUG_MSG "feeding EOL";
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
        token ~pending_EOL lexbuf
      end

|   _ -> ocl ~pending_EOL pure_comment st (line^(Ulexing.utf8_lexeme lexbuf)) lexbuf

  and dec prefix ?(pending_EOL=None) pure_comment st line = lexer
|   line_terminator -> 
    BEGIN_DEBUG
      DEBUG_MSG "LINE TERMINATOR [%s] pure_comment=%B" (Loc.to_string (mkloc lexbuf)) pure_comment;
      let loc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      DEBUG_MSG "line: [%s][%s]" line (Loc.to_string loc);
    END_DEBUG;
    env#set_BOL;

    let dec_qtoken = mktok ~start_opt:(Some st) (RAW (DL.mkdec prefix line)) lexbuf in

    if pure_comment then begin
      begin
        match pending_EOL with
        | Some _ ->
            if env#pending_RAWOMP_obj_queue_length > 0 then begin
              queue_pending_RAWOMP();
            end;
            env#add_pending_token_obj (Obj.repr dec_qtoken);
            token ~pending_EOL lexbuf

        | _ -> dec_qtoken
      end
    end
    else begin
      if is_free_source_form() then begin
        if env#amp_line then begin (* impossible? *)
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          DEBUG_MSG "feeding EOL";
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
        token ~pending_EOL lexbuf
      end
    end

|   _ -> dec prefix ~pending_EOL pure_comment st (line^(Ulexing.utf8_lexeme lexbuf)) lexbuf

  and is_fixed_cont_line ?(assume_fixed_source_form=true) line =
    (not assume_fixed_source_form || is_fixed_source_form()) &&
    not (Xstring.startswith line " " || Xstring.startswith line "0")

  and is_free_cont_line ?(assume_free_source_form=true) line =
    (not assume_free_source_form || is_free_source_form()) &&
    Xstring.endswith (Xstring.rstrip line) "&"

  and xlf trigger ?(pending_EOL=None) pure_comment st line = lexer
|   line_terminator -> 
    BEGIN_DEBUG
      DEBUG_MSG "LINE TERMINATOR [%s] pure_comment=%B" (Loc.to_string (mkloc lexbuf)) pure_comment;
      let loc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      DEBUG_MSG "line: [%s][%s]" line (Loc.to_string loc);
    END_DEBUG;
    env#set_BOL;

    let fixed_cont =  trigger <> "" && is_fixed_cont_line line in
    let free_cont = is_free_cont_line line in

    let line =
      if fixed_cont then begin
        let b = Bytes.of_string line in
        Bytes.set b 0 ' ';
        Bytes.to_string b
      end
      else
        line
    in

    let xlf_qtoken = 
      mktok ~start_opt:(Some st) (RAW (DL.mkxlf trigger line fixed_cont free_cont)) lexbuf
    in

    if pure_comment then begin
      begin
        match pending_EOL with
        | Some _ -> 
            env#add_pending_token_obj (Obj.repr xlf_qtoken);
            token ~pending_EOL lexbuf

        | _ -> xlf_qtoken
      end
    end
    else
      if is_free_source_form() then begin
        if env#amp_line then begin (* impossible? *)
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          DEBUG_MSG "feeding EOL";
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
        token ~pending_EOL lexbuf
      end

|   _ -> xlf trigger ~pending_EOL pure_comment st (line^(Ulexing.utf8_lexeme lexbuf)) lexbuf


  and check_omp_separated_keyword queue_add =
    (fun tmp_kw_t_opt t ->
      let tok, loc = t in
      match tmp_kw_t_opt with
      | Some (tmp_kw, tmp_t) -> begin
          try
            let kw = get_omp_following_keyword tok in

            let tok' = find_omp_keyword ~no_ident:true (tmp_kw^kw) in
            let _, tmp_loc = tmp_t in
            let tmp_st, _ = Loc.to_lexposs tmp_loc in
            let _, ed = Loc.to_lexposs loc in
            let t' = make_qtoken tok' tmp_st ed in
            try
              let kw' = get_omp_continuable_keyword tok' in
              Some (kw', t')
            with
              Not_found -> queue_add t'; None

          with
            Not_found -> 
              queue_add tmp_t;
              try
                let kw = get_omp_continuable_keyword tok in
                Some (kw, t)
              with
                Not_found -> queue_add t; None
      end
      | None -> begin
          try
            let kw = get_omp_continuable_keyword tok in
            Some (kw, t)
          with
            Not_found -> queue_add t; None
      end
    )

  and get_omp_token_queue ofs line =
    DEBUG_MSG "line=^%s$" line;
    let ulbuf = Ulexing.from_utf8_string line in
    let scanner() = scan_omp ofs ulbuf in
    let qtoken_list = ref [] in
    begin
      try
        while true do
          qtoken_list := (scanner()) :: !qtoken_list
        done
      with
        Ulexing.Error -> ()
    end;
    qtoken_list := List.rev !qtoken_list;

    BEGIN_DEBUG
      DEBUG_MSG "token_list:";
      List.iter (fun t -> DEBUG_MSG " %s" (Token.qtoken_to_string t)) !qtoken_list
    END_DEBUG;

    let queue = new Xqueue.c in
    let queue_add t = queue#add (Obj.repr t) in

    let last_opt = 
      List.fold_left (check_omp_separated_keyword queue_add) None !qtoken_list
    in
    begin
      match last_opt with
      | Some (_, t) -> queue_add t
      | None -> ()
    end;

    queue


  and omp
      ?(in_comment=false) 
      ?(pending_EOL=None) 
      ?(offset=5(* length of '!$omp' *))
      pure_comment st line = lexer
|   line_terminator -> 
    BEGIN_DEBUG
      DEBUG_MSG "LINE TERMINATOR [%s] pure_comment=%B" 
        (Loc.to_string (mkloc lexbuf)) pure_comment;
      let loc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      DEBUG_MSG "line: [%s][%s]" line (Loc.to_string loc);
    END_DEBUG;
    env#set_BOL;

    let fixed_cont = is_fixed_cont_line (*~assume_fixed_source_form:false*) line in
    let free_cont = is_free_cont_line (*~assume_free_source_form:false*) line in
    DEBUG_MSG "fixed_cont:%B free_cont:%B" fixed_cont free_cont;
    let line =
      if fixed_cont && offset > 0 then begin
        let b = Bytes.of_string line in
        Bytes.set b 0 ' ';
        Bytes.to_string b
      end
      else
        line
    in
    let q = get_omp_token_queue (st+offset) line in
    let omp_qtoken =
      mktok ~start_opt:(Some st) (RAW (DL.mkomp line q fixed_cont free_cont)) lexbuf
    in

    if pure_comment then begin
      if is_free_source_form() then begin
        begin
          match pending_EOL with
          | Some t -> 
              DEBUG_MSG "pending_EOL: Some"; 
              env#set_pending_EOL_obj (Obj.repr t)

          | None -> 
              DEBUG_MSG "pending_EOL: None"; 
              ()
        end;
        omp_qtoken
      end
      else begin (* fixed source form *)
        match pending_EOL with
        | Some _ -> 
            DEBUG_MSG "pending_EOL: Some";
            env#add_pending_RAWOMP_obj (Obj.repr omp_qtoken);
            token ~pending_EOL lexbuf

        | None -> 
            DEBUG_MSG "pending_EOL: None"; 
            omp_qtoken
      end
    end
    else begin
      if is_free_source_form() then begin
        if env#amp_line then begin (* impossible? *)
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          DEBUG_MSG "feeding EOL";
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
	token ~pending_EOL:(Some (mktok ~pending:true EOL lexbuf)) lexbuf
      end
    end

|   '!' -> 
    DEBUG_MSG "COMMENT (!) [%s]" (Loc.to_string (mkloc lexbuf));
    omp ~in_comment:true ~pending_EOL ~offset pure_comment st line lexbuf

|   _ -> 
    let line' = 
      if in_comment then
        line
      else
        line^(Ulexing.utf8_lexeme lexbuf)
    in
    omp ~in_comment ~pending_EOL ~offset pure_comment st line' lexbuf


  and get_acc_token_queue ofs line =
    DEBUG_MSG "line=^%s$" line;
    let ulbuf = Ulexing.from_utf8_string line in
    let scanner() = scan_acc ofs ulbuf in
    let qtoken_list = ref [] in
    begin
      try
        while true do
          qtoken_list := (scanner()) :: !qtoken_list
        done
      with
        Ulexing.Error -> ()
    end;
    qtoken_list := List.rev !qtoken_list;

    BEGIN_DEBUG
      DEBUG_MSG "token_list:";
      List.iter (fun t -> DEBUG_MSG " %s" (Token.qtoken_to_string t)) !qtoken_list
    END_DEBUG;

    let queue = new Xqueue.c in
    let queue_add t = queue#add (Obj.repr t) in

    List.iter queue_add !qtoken_list;

    queue


  and acc ?(pending_EOL=None) pure_comment st line = lexer
|   line_terminator -> 
    BEGIN_DEBUG
      DEBUG_MSG "LINE TERMINATOR [%s] pure_comment=%B" (Loc.to_string (mkloc lexbuf)) pure_comment;
      let loc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
      DEBUG_MSG "line: [%s][%s]" line (Loc.to_string loc);
    END_DEBUG;
    env#set_BOL;

    let fixed_cont = is_fixed_cont_line line in
    let free_cont = is_free_cont_line line in

    let line =
      if fixed_cont then begin
        let b = Bytes.of_string line in
        Bytes.set b 0 ' ';
        Bytes.to_string b
      end
      else
        line
    in

    let acc_qtoken =
      mktok ~start_opt:(Some st) (RAW (DL.mkacc line fixed_cont free_cont)) lexbuf
    in

    if pure_comment then begin
      begin
        match pending_EOL with
        | Some _ -> 
            if env#pending_RAWOMP_obj_queue_length > 0 then begin
              queue_pending_RAWOMP();
            end;
            env#add_pending_token_obj (Obj.repr acc_qtoken);
            token ~pending_EOL lexbuf

        | _ -> acc_qtoken
      end
    end
    else
      if is_free_source_form() then begin
        if env#amp_line then begin (* impossible? *)
          env#clear_amp_line;
          _token lexbuf
        end
        else begin
          DEBUG_MSG "feeding EOL";
	  mktok EOL lexbuf
        end
      end
      else begin (* fixed source form *)
        token ~pending_EOL lexbuf
      end

|   _ -> acc ~pending_EOL pure_comment st (line^(Ulexing.utf8_lexeme lexbuf)) lexbuf


  and conv_pp_token qtoken =
    let tok, loc = qtoken in
    let tok' =
      match tok with
      | PP_BRANCH br -> begin
          match br with
          | PPD.If _ | PPD.Ifdef _ | PPD.Ifndef _ -> 
              env#lex_enter_pp_branch br;
              tok
          | PPD.Endif _ -> begin
              try
                let br', plv = env#lex_exit_pp_branch in
                PP_BRANCH (PPD.Endif(br', plv))
              with
                Failure _ -> 
                  (*parse_warning_loc loc "dangling #endif";*)
                  tok
          end
          | _ -> tok
      end
      | _ -> tok
    in
    if tok = tok' then
      qtoken
    else
      tok', loc


  and output_pp_qtoken pp_qtoken lexbuf =
    try
      let _ = env#get_pending_EOL_obj in

      if env#pending_RAWOMP_obj_queue_length > 0 then begin
        queue_pending_RAWOMP();
      end;

      env#add_pending_token_obj (Obj.repr pp_qtoken);
      token lexbuf
    with
      Not_found -> begin
        let pp_qtoken' = conv_pp_token pp_qtoken in
        env#set_last_lex_qtoken_obj (Obj.repr pp_qtoken');
        pp_qtoken'
      end


  and pre_pp_macro_arguments = lexer
|   '(' -> 
    rollback lexbuf; 
    true
|   _ -> 
    rollback lexbuf;
    false

  and pp_macro_arguments paren_lv args arg = 
    let _ = 
      DEBUG_MSG "paren_lv:%d args=[%s] arg={%s}" 
        paren_lv (String.concat "," args) arg 
    in
    lexer
|   '(' white_space* -> 
    if paren_lv = 0 then
      pp_macro_arguments (paren_lv+1) args arg lexbuf
    else
      pp_macro_arguments (paren_lv+1) args (arg^"(") lexbuf

|   ')' ->
    if paren_lv = 1 then
      let args' =
        if arg <> "" then
          arg::args
        else
          args
      in
      List.rev args', ((Ulexing.lexeme_end lexbuf) - 1)
    else
      pp_macro_arguments (paren_lv-1) args (arg^")") lexbuf

|   ',' white_space* -> 
    if paren_lv = 1 then
      pp_macro_arguments paren_lv (arg::args) " " lexbuf
    else
      pp_macro_arguments paren_lv args (arg^",") lexbuf

|   char_start_single -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    pp_char_single paren_lv args (arg^s) lexbuf

|   char_start_double -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    pp_char_double paren_lv args (arg^s) lexbuf

|   _ ->
    let s = Ulexing.utf8_lexeme lexbuf in
    pp_macro_arguments paren_lv args (arg^s) lexbuf

  and pp_char_single paren_lv args arg = lexer
|   "''" -> pp_char_single paren_lv args (arg^"'") lexbuf
|   "\"\"" -> pp_char_single paren_lv args (arg^"\"") lexbuf
|   rep_char_non_single_quote -> 
    pp_char_single paren_lv args (arg^(Ulexing.utf8_lexeme lexbuf)) lexbuf
|   '\'' -> pp_macro_arguments paren_lv args (arg^"'") lexbuf

  and pp_char_double paren_lv args arg = lexer
|   "''" -> pp_char_double paren_lv args (arg^"'") lexbuf
|   "\"\"" -> pp_char_double paren_lv args (arg^"\"") lexbuf
|   rep_char_non_double_quote -> 
    pp_char_double paren_lv args (arg^(Ulexing.utf8_lexeme lexbuf)) lexbuf
|   '"' -> pp_macro_arguments paren_lv args (arg^"\"") lexbuf


  and pp_define st_pos id params_opt body body_st stat = 
    let mem_param p =
      match params_opt with
      | Some params -> List.mem p params
      | None -> false
    in
    lexer

|   white_space* -> 
(*    let s = Ulexing.utf8_lexeme lexbuf in *)
    begin
      match stat with
      | D_id -> 
          if id <> "" then
            let body_st' = Ulexing.lexeme_end lexbuf in
            pp_define st_pos id params_opt body body_st' D_body lexbuf
          else
            pp_define st_pos id params_opt body body_st stat lexbuf

      | D_body -> pp_define st_pos id params_opt (body^" ") body_st stat lexbuf
      | _ -> pp_define st_pos id params_opt body body_st stat lexbuf
    end

|   line_concat -> pp_define st_pos id params_opt body body_st stat lexbuf

|   line_terminator -> 
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let body_ =
      let bloc = offsets_to_loc body_st ed in
      match params_opt with
      | Some params -> Macro.mk_fun_body ~loc:bloc (List.rev params) body
      | None        -> Macro.mk_obj_body ~loc:bloc body
    in

    begin
      try
        let b = env#find_macro body in
        let ln = Macro.line_of_body b in
        begin
          try
            match Macro.tok_of_line ln with
            | PP_MACRO_CONST _      -> Macro.resolve_body (PP_MACRO_CONST id) body_
            | PP_MACRO_CONST_CHAR _ -> Macro.resolve_body (PP_MACRO_CONST_CHAR id) body_
            | PP_MACRO_CONST_INT _  -> Macro.resolve_body (PP_MACRO_CONST_INT id) body_
            | PP_MACRO_NAME _       -> Macro.resolve_body (PP_MACRO_NAME(id, "")) body_
            | PP_MACRO_EXPR _       -> Macro.resolve_body (PP_MACRO_EXPR id) body_
            | PP_MACRO_STMT _       -> Macro.resolve_body (PP_MACRO_STMT id) body_
            | PP_MACRO_TYPE_SPEC _  -> Macro.resolve_body (PP_MACRO_TYPE_SPEC id) body_
            | PP_MACRO_WRITE _      -> Macro.resolve_body (PP_MACRO_WRITE id) body_
            | PP_MACRO_READ_WRITE _ -> Macro.resolve_body (PP_MACRO_READ_WRITE id) body_
            | _ -> ()
          with
            Not_found -> ()
        end
      with
        Not_found -> DEBUG_MSG "not found: %s" body
    end;

    let pp_qtoken = make_qtoken (PP_DEFINE__IDENT__BODY(id, body_)) st_pos ed_pos in
    DEBUG_MSG "pp_qtoken: %s" (Token.qtoken_to_string pp_qtoken);
    env#lex_define_macro id body_;
    output_pp_qtoken pp_qtoken lexbuf

|   pp_identifier | name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    begin
      match stat with
      | D_id -> pp_define st_pos s params_opt body body_st stat lexbuf
      | D_params -> 
          let params_opt' =
            match params_opt with
            | None -> assert false
            | Some params -> Some (s :: params)
          in
          pp_define st_pos id params_opt' body body_st stat lexbuf

      | D_body -> 
          let body' =
            if mem_param s then
              String.concat "" [body;"{";s;"}"]
            else
              body^s
          in
          pp_define st_pos id params_opt body' body_st stat lexbuf

      | _ -> pp_define st_pos id params_opt (body^s) body_st stat lexbuf
    end

|   '(' -> 
    begin
      match stat with
      | D_id -> pp_define st_pos id (Some []) body body_st D_params lexbuf
      | D_body -> pp_define st_pos id params_opt (body^"(") body_st stat lexbuf
      | _ -> pp_define st_pos id params_opt body body_st stat lexbuf
    end

|   ',' -> 
    begin
      match stat with
      | D_body -> pp_define st_pos id params_opt (body^",") body_st stat lexbuf
      | _ -> pp_define st_pos id params_opt body body_st stat lexbuf
    end

|   ')' ->
    begin
      match stat with
      | D_params -> pp_define st_pos id params_opt body body_st D_id lexbuf
      | D_body -> pp_define st_pos id params_opt (body^")") body_st stat lexbuf
      | _ -> pp_define st_pos id params_opt body body_st stat lexbuf
    end

|   _ -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    begin
      match stat with
      | D_id -> pp_define st_pos id params_opt body body_st stat lexbuf
      | _ -> pp_define st_pos id params_opt (body^s) body_st stat lexbuf
    end


  and pp_undef st_pos id stat = lexer
|   line_concat -> pp_undef st_pos id stat lexbuf

|   line_terminator -> 
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let pp_qtoken = make_qtoken (PP_UNDEF__IDENT id) st_pos ed_pos in
    DEBUG_MSG "pp_tok: %s" (Token.qtoken_to_string pp_qtoken);
    env#lex_undefine_macro id;
    output_pp_qtoken pp_qtoken lexbuf

|   pp_identifier | name -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    begin
      match stat with
      | D_id -> pp_undef st_pos s D_finished lexbuf
      | _ -> pp_undef st_pos id stat lexbuf
    end

|   _ -> 
(*    let s = Ulexing.utf8_lexeme lexbuf in *)
    begin
      match stat with
      | D_id -> pp_undef st_pos id stat lexbuf
      | _ -> pp_undef st_pos id stat lexbuf
    end


  and pp_if ?(elif=false) st_pos cond = lexer
|   line_concat -> pp_if ~elif st_pos cond lexbuf

|   line_terminator -> 
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let br =
      if elif then
        PPD.Elif cond
      else
        PPD.If cond
    in
    let pp_rawtok = PP_BRANCH br in
    let pp_qtoken = make_qtoken pp_rawtok st_pos ed_pos in
    output_pp_qtoken pp_qtoken lexbuf

|   white_space+ -> pp_if ~elif st_pos cond lexbuf

|   _ -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    pp_if ~elif st_pos (cond^s) lexbuf


  and pp_ifdef ?(ndef=false) st_pos id = lexer
|   line_concat -> pp_ifdef ~ndef st_pos id lexbuf

|   white_space -> pp_ifdef ~ndef st_pos id lexbuf

|   line_terminator -> 
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let pp_rawtok = 
      PP_BRANCH
        (if ndef then
          PPD.Ifndef id
        else
          PPD.Ifdef id)
    in
    let pp_qtoken = make_qtoken pp_rawtok st_pos ed_pos in
    output_pp_qtoken pp_qtoken lexbuf

|   name -> 
    let n = Ulexing.utf8_lexeme lexbuf in
    pp_ifdef ~ndef st_pos (if id = "" then n else id) lexbuf

|   pp_identifier -> 
    let n = Ulexing.utf8_lexeme lexbuf in
    pp_ifdef ~ndef st_pos (if id = "" then n else id) lexbuf

|   _ -> pp_ifdef ~ndef st_pos id lexbuf


  and pp_else st_pos = lexer
|   line_terminator ->
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let pp_qtoken = make_qtoken (PP_BRANCH PPD.Else) st_pos ed_pos in
    output_pp_qtoken pp_qtoken lexbuf

|   _ -> pp_else st_pos lexbuf


  and pp_endif st_pos = lexer
|   line_terminator ->
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let pp_qtoken =
      make_qtoken (PP_BRANCH (PPD.Endif(PPD.If "???", 0))) st_pos ed_pos
    in
    output_pp_qtoken pp_qtoken lexbuf

|   _ -> pp_endif st_pos lexbuf


  and pp_line mktok st_pos mesg = lexer
|   line_concat -> pp_line mktok st_pos mesg lexbuf

|   line_terminator -> 
    env#set_BOL;
    let ed = (Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_length lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    let stripped = Xstring.strip mesg in
    let pp_qtoken = make_qtoken (mktok stripped) st_pos ed_pos in
    output_pp_qtoken pp_qtoken lexbuf

|   _ -> 
    let s = Ulexing.utf8_lexeme lexbuf in
    pp_line mktok st_pos (mesg^s) lexbuf

(*
  and pp_other pp_qtoken = lexer
|   line_terminator ->
    env#set_BOL;
    output_pp_qtoken pp_qtoken lexbuf

|   _ -> pp_other pp_qtoken lexbuf
*)

  and pp_skip ?(pending_EOL=None) = lexer
|   line_terminator ->
    env#set_BOL;
    token ~pending_EOL lexbuf

|   _ -> pp_skip lexbuf


  and pp_include_filename_start pp_pending_EOL st_pos = lexer
|   white_space -> pp_include_filename_start pp_pending_EOL st_pos lexbuf
|   '\"' -> pp_include_filename_dq pp_pending_EOL st_pos "\"" lexbuf
|   '\'' -> pp_include_filename_sq pp_pending_EOL st_pos "\'" lexbuf
|   '<' -> pp_include_sys_filename pp_pending_EOL st_pos "<" lexbuf

|   pp_identifier | name -> (* filename by macro *)
    let id = Ulexing.utf8_lexeme lexbuf in
    DEBUG_MSG "id=%s" id;
    let handler quoted =
      handle_include pp_pending_EOL
        quoted 
        (fun () -> 
          make_qtoken
            (PP_INCLUDE__FILE (H.mkmacro ~content:(Some quoted) id)) 
            st_pos 
            (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
        )
        lexbuf
    in
    begin
      try
        match env#lex_find_macro id with
        | Macro.Object line -> begin
            let s = Xstring.strip line.Macro.ln_raw in
            DEBUG_MSG "s=%s" s;
            if is_header_filename s then
              handler s
            else
              handler ""
        end
        | _ -> handler ""
      with
        Not_found -> handler ""
    end

  and feed_pp_pending_EOL pending_EOL t lexbuf =
    match pending_EOL with
    | None -> begin
        env#set_last_lex_qtoken_obj (Obj.repr t); 
        t
    end
    | Some _ -> begin
        env#add_pending_token_obj (Obj.repr t);
        env#set_lex_mode_queue;
        feed_pending_EOL pending_EOL lexbuf
    end
    

  and pp_include_filename_dq pp_pending_EOL st_pos str = 
    lexer
|   line_terminator -> 
    env#set_BOL;
    let pp_tok = mkbadincltok st_pos (PP_INCLUDE__FILE (H.mkuser str)) lexbuf in
    feed_pp_pending_EOL pp_pending_EOL pp_tok lexbuf

|   filename_character_dq -> 
    pp_include_filename_dq pp_pending_EOL st_pos (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '\"' -> 
    let quoted = str^"\"" in
    handle_include pp_pending_EOL
      quoted 
      (fun () -> 
        make_qtoken
          (PP_INCLUDE__FILE (H.mkuser quoted)) 
          st_pos (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
      )
      lexbuf

  and pp_include_filename_sq pp_pending_EOL st_pos str = 
    lexer
|   line_terminator -> 
    env#set_BOL;
    let pp_tok = mkbadincltok st_pos (PP_INCLUDE__FILE (H.mkuser str)) lexbuf in
    feed_pp_pending_EOL pp_pending_EOL pp_tok lexbuf

|   filename_character_sq -> 
    pp_include_filename_sq pp_pending_EOL st_pos (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '\'' -> 
    let quoted = str^"\'" in
    handle_include pp_pending_EOL
      quoted 
      (fun () -> 
        make_qtoken
          (PP_INCLUDE__FILE (H.mkuser quoted)) 
          st_pos (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
      )
      lexbuf

  and pp_include_sys_filename pp_pending_EOL st_pos str = 
    lexer
|   line_terminator -> 
    env#set_BOL;
    let pp_tok = mkbadincltok st_pos (PP_INCLUDE__FILE (H.mksystem str)) lexbuf in
    feed_pp_pending_EOL pp_pending_EOL pp_tok lexbuf

|   sys_filename_character -> 
    pp_include_sys_filename pp_pending_EOL st_pos (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '>' -> 
    let quoted = str^">" in
    handle_include pp_pending_EOL
      quoted 
      (fun () -> 
        make_qtoken
          (PP_INCLUDE__FILE (H.mksystem quoted)) 
          st_pos (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
      )
      lexbuf

  and include_filename_start st_pos = lexer
|   white_space -> include_filename_start st_pos lexbuf
|   '\"' -> include_filename_dq st_pos "\"" lexbuf
|   '\'' -> include_filename_sq st_pos "\'" lexbuf

  and include_filename_dq st_pos str = 
    lexer
|   line_terminator -> 
    env#set_BOL;
    let pp_tok = mkbadincltok st_pos (INCLUDE__FILE str) lexbuf in
    pp_tok

|   filename_character_dq -> 
    include_filename_dq st_pos (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '\"' ->
    let quoted = str^"\"" in
    handle_include None
      quoted 
      (fun () -> 
        make_qtoken 
          (INCLUDE__FILE quoted) st_pos (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
      )
      lexbuf

  and include_filename_sq st_pos str = 
    lexer
|   line_terminator -> 
    env#set_BOL;
    let pp_tok = mkbadincltok st_pos (INCLUDE__FILE str) lexbuf in
    pp_tok

|   filename_character_sq -> 
    include_filename_sq st_pos (str^(Ulexing.utf8_lexeme lexbuf)) lexbuf

|   '\'' -> 
    let quoted = str^"\'" in
    handle_include None
      quoted 
      (fun () -> 
        make_qtoken 
          (INCLUDE__FILE quoted) st_pos (ofs_to_pos ((Ulexing.lexeme_end lexbuf) - 1))
      )
      lexbuf


  and mkbadincltok st_pos rawtok lexbuf =
    let ed = (Ulexing.lexeme_end lexbuf) - 1 in
    let ed_pos = ofs_to_pos ed in
    parse_warning st_pos ed_pos "open quotation";
    make_qtoken rawtok st_pos ed_pos


  and handle_include pp_pending_EOL quoted mkincltok ?(trailing_comment=None) = lexer
|   line_terminator -> 
    DEBUG_MSG "quoted=%s" quoted;
    begin
      match trailing_comment with
      | Some st -> begin
          let cloc = offsets_to_loc st ((Ulexing.lexeme_end lexbuf) - 1) in
          DEBUG_MSG "comment loc: [%s]" (Loc.to_string cloc);
          add_comment_region cloc
      end
      | None -> ()
    end;

    let feed_incltok() =
      let pp_tok = mkincltok() in
      feed_pp_pending_EOL pp_pending_EOL pp_tok lexbuf
    in

    if quoted = "" then begin
      env#set_BOL;
      feed_incltok()
    end
    else begin
      try
        let unquoted = H.get_unquoted quoted in
        let files = env#find_path ~ignore_case:true unquoted in

        match files with
        | [file] -> begin
            DEBUG_MSG "checking %s" file#path;
            let is_extra_source_file =
              try
                let ext = String.lowercase_ascii file#get_extension in
                not (List.mem ext extensions)
              with
                Xfile.No_extension _ -> true
            in
            if is_extra_source_file then begin
              env#add_extra_source_file file
            end;

            let src = new Source.c file in
            if src#exists then begin
              DEBUG_MSG "source exists";
              (*env#verbose_msg "found: %s" src#path;*)
              if env#source_entered src then begin
                warning_msg "cyclic include: %s" quoted;
                env#set_BOL;
                feed_incltok()
              end
              else begin
                if env#ignore_include_flag then begin
                  env#set_BOL;
                  feed_incltok()
                end
                else begin
                  let cur_src = env#current_source in
                  let config = src#lang_config in
                  config#_set_parse_d_lines_flag cur_src#parse_d_lines;
                  env#push_loc (Token.qtoken_to_loc (mkincltok()));
                  token ~pending_EOL:pp_pending_EOL (env#enter_source src)
                end
              end
            end
            else begin
              DEBUG_MSG "source does not exist";
              warning_msg "not found: %s" quoted;
              env#set_BOL;
              feed_incltok()
            end
        end
        | [] -> begin
            warning_msg "not found: %s" quoted;
            env#set_BOL;
            feed_incltok()
        end
        | _ -> begin
            let cur_path = env#current_source#path in
            warning_msg "\"%s\": multiple files found for %s:" cur_path quoted;
            List.iter
              (fun f ->
                warning_msg "  \"%s\"" f#path
              ) files;
            env#set_BOL;
            feed_incltok()
        end
      with
        Invalid_argument _ -> 
          warning_msg "invalid quoted file name: %s" quoted;
          env#set_BOL;
          feed_incltok()
    end

|   '!' -> 
    handle_include pp_pending_EOL quoted mkincltok ~trailing_comment:(Some (Ulexing.lexeme_start lexbuf)) lexbuf

|   _ -> handle_include pp_pending_EOL quoted mkincltok ~trailing_comment lexbuf




end (* of functor Ulexer.F *)
