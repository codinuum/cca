(*
   Copyright 2012-2022 Codinuum Software Lab <https://codinuum.com>

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

module Aux = Parser_aux
module PB  = Parserlib_base


let token_queue_to_string = Common.token_queue_to_string Token.to_orig

let outline_queue_to_string oq =
  let buf = Buffer.create 0 in
  oq#iter
    (fun t ->
      let s = Token.to_orig t in
      Buffer.add_string buf s
    );
  Buffer.contents buf


module F (Stat : Aux.STATE_T) = struct

  module U = Ulexer.F (Stat)

  open Stat

  let loc_of_poss stp edp =
    let pos_mgr = env#current_pos_mgr in
    let so = stp.Lexing.pos_cnum in
    let sl, sc = pos_mgr#get_position so in
    let eo = edp.Lexing.pos_cnum in
    let el, ec = pos_mgr#get_position eo in
    Astloc.make ~fname:env#current_filename so eo sl sc el ec


  class c = object (self)
    inherit [Tokens_.token] PB.scanner

    val mutable ulexbuf_opt = None

    val queue = new Xqueue.c

    val shadow_queue = new Xqueue.c
    val shadow_q = new Xqueue.c

    val mutable last_rawtoken = Tokens_.EOF

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

    method prepend_token tok = queue#prepend tok

    method prepend_rawtoken rawtok stp edp =
      let t = Token.create rawtok stp edp in
      self#prepend_token t

    method peek_nth nth =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          let token, rawtok = U.peek_nth queue ulexbuf nth in
          DEBUG_MSG "%s" (Token.to_string env#current_pos_mgr token);
          token, rawtok
      end
      | None -> failwith "Scanner.F.c#peek_nth"

    method shadow_queue = shadow_queue
    method reset_shadow_queue = shadow_queue#clear
    method shadow_contents = token_queue_to_string shadow_queue
    method copy_shadow_queue = shadow_queue#copy
    method prepend_shadow_queue q =
      DEBUG_MSG "shadow_queue=%s" self#shadow_contents;
      DEBUG_MSG "q=%s" (outline_queue_to_string q);
      shadow_queue#prepend_from q

    method shadow_q = shadow_q
    method reset_shadow_q = shadow_q#clear
    method shadow_outline = outline_queue_to_string shadow_q
    method copy_shadow_q = shadow_q#copy
    method prepend_shadow_q q =
      DEBUG_MSG "shadow_q=%s" self#shadow_outline;
      DEBUG_MSG "q=%s" (outline_queue_to_string q);
      shadow_q#prepend_from q


    method has_error =
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

    method method_follows =
      DEBUG_MSG "checking...";
      let b = ref true in
      let count = ref 1 in
      let prev = ref Tokens_.EOF in
      try
        while true do
          let t, rt = self#peek_nth !count in
          begin
            match rt with
            | CLASS _ | ENUM _ | INTERFACE _
            | SEMICOLON | EQ | EOF
              -> begin
                b := false;
                raise Exit
              end
            | LBRACE when match !prev with LPAREN _ -> false | _ -> true -> raise Exit
            | _ -> ()
          end;
          incr count;
          prev := rt
        done;
        false
      with
        Exit ->
          DEBUG_MSG "%B" !b;
          !b

    method discard_tokens n =
      match ulexbuf_opt with
      | Some ulexbuf -> begin
          for i = 1 to n do
            let token = U.get_token queue ulexbuf in
            DEBUG_MSG ">> %s" (Token.to_string env#current_pos_mgr token);
            ignore token
          done
      end
      | _ -> ()

    method get_token () =
      let token =
        match ulexbuf_opt with
        | Some ulexbuf -> begin
            let token = U.get_token queue ulexbuf in
            DEBUG_MSG "------> %s" (Token.to_string env#current_pos_mgr token);
            token
        end
        | None -> failwith "Scanner.F.c#get_token"
      in
      let has_error = self#has_error in
      let rawtok, stp, edp = Token.decompose token in
      if env#keep_going_flag && stp <> Lexing.dummy_pos && edp <> Lexing.dummy_pos then begin
        let add_braces ?(global=false) () =
          let blv = env#block_level in
          DEBUG_MSG "blv=%d" blv;
          match rawtok with
          | RBRACE when global -> begin
              let gblv = env#g_brace_level in
              DEBUG_MSG "g_brace_level=%d" gblv;
              DEBUG_MSG "context_stack: %s" (env#context_stack_rep);
              let n = gblv - 1 in
              DEBUG_MSG "n=%d" n;
              let loc = loc_of_poss stp edp in
              DEBUG_MSG "adding %d closing braces" n;
              if n > 0 then begin
                Common.warning_loc loc "adding %d closing braces" n;
                let t = Token.create Tokens_.RBRACE Lexing.dummy_pos Lexing.dummy_pos in
                for i = 1 to n do
                  self#prepend_token t;
                  (*env#close_block*)
                done
              end
          end
          | RBRACE when blv = 1 -> ()
          | RBRACE when blv = 0 && begin match env#context_stack_as_list with
            | C_method _ :: C_class _ :: C_method _ :: _ -> true
            | _ -> false
          end -> begin
            let loc = loc_of_poss stp edp in
            DEBUG_MSG "adding a closing brace";
            Common.warning_loc loc "adding a closing brace";
            let t = Token.create Tokens_.RBRACE Lexing.dummy_pos Lexing.dummy_pos in
            self#prepend_token t;
          end
          | RBRACE when blv = 0 -> ()
          | _ -> begin
              DEBUG_MSG "block_level=%d" blv;
              DEBUG_MSG "context_stack: %s" (env#context_stack_rep);
              let n =
                match rawtok with
                | RBRACE -> blv - 1
                | LBRACE -> blv + 1
                | _ -> blv
              in
              DEBUG_MSG "n=%d" n;
              let loc = loc_of_poss stp edp in
              DEBUG_MSG "adding %d closing braces" n;
              if n > 0 then begin
                Common.warning_loc loc "adding %d closing braces" n;
                let t = Token.create Tokens_.RBRACE Lexing.dummy_pos Lexing.dummy_pos in
                for i = 1 to n do
                  self#prepend_token t;
                  (*env#close_block*)
                done
              end
          end
        in
        match rawtok with
        | RBRACE | SEMICOLON | LBRACE when env#in_method -> begin
            match rawtok with
            | LBRACE when env#in_new -> ()
            | LBRACE when env#class_flag -> env#clear_class_flag
            | _ -> begin
                DEBUG_MSG "@";
                let t1, rt1 = self#peek_nth 1 in
                match rt1 with
                | EOF when env#g_brace_level > 1 -> add_braces ~global:true ()
                | EOF -> ()
                | NATIVE _ when not has_error -> add_braces()
                | PUBLIC _ | PROTECTED _ | PRIVATE _ | ABSTRACT _ | STATIC _
                | STRICTFP _ when not has_error && self#method_follows -> add_braces()
                | SYNCHRONIZED _ when
                    let _, rt2 = self#peek_nth 2 in
                    match rt2 with LPAREN _ -> false | _ -> not has_error -> add_braces()
                | SEMICOLON when begin
                    rawtok == RBRACE && env#block_level = 1 && not env#at_array && not env#at_lambda &&
                    match env#current_context with
                    | C_method mstat -> begin
                        match Aux.stack_to_list mstat.m_stack with
                        | SC_new :: SC_array :: _ -> false
                        | _ -> true
                    end
                    | _ -> true
                end -> begin
                  DEBUG_MSG "";
                  let t2, rt2 = self#peek_nth 2 in
                  match rt2 with
                  | RBRACE -> ()
                  | IDENTIFIER _ when not env#at_array && not has_error -> begin
                      match env#context_stack_as_list with
                      | C_method _ :: C_class _ :: C_method _ :: _ -> begin
                          let loc = loc_of_poss stp edp in
                          DEBUG_MSG "adding a closing brace";
                          Common.warning_loc loc "adding a closing brace";
                          let t = Token.create Tokens_.RBRACE Lexing.dummy_pos Lexing.dummy_pos in
                          self#prepend_token t;
                      end
                      | _ -> ()
                  end
                  | _ -> ()
                end
                | AT _ when not has_error && self#method_follows -> add_braces()
                | RPAREN _ when not has_error && begin
                    match env#current_context with
                    | C_method mstat -> begin
                        match Aux.stack_to_list mstat.m_stack with
                        | SC_block :: SC_block :: SC_lambda :: SC_ivk :: _ -> true
                        | _ -> false
                    end
                    | _ -> false
                end -> begin
                  let loc = loc_of_poss stp edp in
                  DEBUG_MSG "adding a closing brace";
                  Common.warning_loc loc "adding a closing brace";
                  let t = Token.create Tokens_.RBRACE Lexing.dummy_pos Lexing.dummy_pos in
                  self#prepend_token t;
                end
                | FOR _ | WHILE _ | DO _ | IF _ | SWITCH _ | RETURN _ when begin
                    rawtok == RBRACE && env#block_level = 1
                end -> begin
                  let loc = loc_of_poss stp edp in
                  DEBUG_MSG "adding an opening brace";
                  Common.warning_loc loc "adding an opening brace";
                  let p = Astloc.incr_lexpos edp in
                  let t = Token.create Tokens_.LBRACE p p in
                  self#prepend_token t;
                end
                | _ -> ()
            end
        end
        | RPAREN _ when
            env#in_method &&
            env#paren_level > 1 &&
            not env#at_for &&
            not env#at_lambda &&
            not env#at_res &&
            not has_error -> begin

            let t1, rt1 = self#peek_nth 1 in
            match rt1 with
            | SEMICOLON -> begin
                DEBUG_MSG "q=%s" self#shadow_outline;
                let n = env#paren_level - 1 in
                DEBUG_MSG "n=%d" n;
                let loc = loc_of_poss stp edp in
                DEBUG_MSG "adding %d closing parentheses" n;
                if n > 0 then begin
                  Common.warning_loc loc "adding %d closing parentheses" n;
                  let dummy_loc = loc_of_poss Lexing.dummy_pos Lexing.dummy_pos in
                  let t =
                    Token.create (Tokens_.RPAREN dummy_loc) Lexing.dummy_pos Lexing.dummy_pos
                  in
                  for i = 1 to n do
                    self#prepend_token t;
                  done
                end
            end
            | _ -> ()
        end
        | RPAREN _ when env#in_method && env#paren_level = 1 && not env#at_array -> begin
            DEBUG_MSG "";
            let t1, rt1 = self#peek_nth 1 in
            match rt1 with
            | RBRACE -> begin
                let loc = loc_of_poss stp edp in
                DEBUG_MSG "adding SEMICOLON";
                Common.warning_loc loc "adding SEMICOLON";
                let t = Token.create Tokens_.SEMICOLON Lexing.dummy_pos Lexing.dummy_pos in
                self#prepend_token t
            end
            | _ -> ()
        end
        | _ -> ()
      end;
      DEBUG_MSG "@";
      let token, rawtok =
        if not env#keep_going_flag || stp = Lexing.dummy_pos || edp = Lexing.dummy_pos || has_error then
          match rawtok with
          | GT_7 -> begin
              let _, stp, edp = Token.decompose token in
              self#prepend_token (Token.create Tokens_.GT edp edp);
              self#prepend_token
                (Token.create Tokens_.GT_GT_GT (Astloc.incr_n_lexpos 3 stp) (Astloc.decr_n_lexpos 1 edp));
              let t = Token.create Tokens_.GT_GT_GT stp (Astloc.incr_n_lexpos 2 stp) in
              t, Token.to_rawtoken t
          end
          | _ -> token, rawtok
        else
          match rawtok with
          | SEMICOLON
            when env#in_method && env#paren_level > 0 && not env#at_for && not env#at_lambda && not env#at_res -> begin
              let loc = loc_of_poss stp edp in
              DEBUG_MSG "adding a closing parentheses";
              Common.warning_loc loc "adding a closing parentheses";
              let dummy_loc = loc_of_poss Lexing.dummy_pos Lexing.dummy_pos in
              let t = Token.create (Tokens_.RPAREN dummy_loc) Lexing.dummy_pos Lexing.dummy_pos in
              DEBUG_MSG "---> %s" (Token.to_string env#current_pos_mgr t);
              self#prepend_token token;
              t, Token.to_rawtoken t
            end
          | RBRACE when env#in_method && not env#at_array && begin
              match last_rawtoken with
              | ELSE _ -> true
              | _ -> false
          end -> begin
            DEBUG_MSG "discarding a closing brace";
            let loc = loc_of_poss stp edp in
            Common.warning_loc loc "discarding a closing brace";
            let t1_rt1 = self#peek_nth 1 in
            self#discard_tokens 1;
            t1_rt1
          end
          | RBRACE when env#in_method && env#block_level = 1 && not env#at_array -> begin
              DEBUG_MSG "@";
              match env#context_stack_as_list with
              | C_method _ :: C_class _ :: C_class _ :: _ -> token, rawtok
              | C_method _ :: C_class _ :: C_method _ :: C_class _ :: _ -> token, rawtok
              | C_method mstat :: _ when mstat.m_block_level = 1 && begin
                  let t1, rt1 = self#peek_nth 1 in
                  match rt1 with
                  | RETURN _ | ELSE _ -> true
                  | _ -> false
              end -> begin
                DEBUG_MSG "discarding a closing brace";
                let loc = loc_of_poss stp edp in
                Common.warning_loc loc "discarding a closing brace";
                let t1_rt1 = self#peek_nth 1 in
                self#discard_tokens 1;
                t1_rt1
              end
              | _ -> begin
                  DEBUG_MSG "@";
                  let t1, rt1 = self#peek_nth 1 in
                  match rt1 with
                  | RBRACE -> begin
                      let t2, rt2 = self#peek_nth 2 in
                      match rt2 with
                      | AT _ -> begin
                          let t3, rt3 = self#peek_nth 3 in
                          match rt3 with
                          | IDENTIFIER(_, s) when s = "Override" -> begin
                              let loc = loc_of_poss stp edp in
                              DEBUG_MSG "discarding a redundant closing brace";
                              Common.warning_loc loc "discarding a redundant closing brace";
                              self#discard_tokens 1;
                              token, rawtok
                          end
                          | _ -> token, rawtok
                      end
                      | EOF -> token, rawtok
                      | _ when self#method_follows -> begin
                          let loc = loc_of_poss stp edp in
                          DEBUG_MSG "discarding a redundant closing brace";
                          Common.warning_loc loc "discarding a redundant closing brace";
                          self#discard_tokens 1;
                          token, rawtok
                      end
                      | _ -> token, rawtok
                  end
                  | _ -> token, rawtok
              end
          end
          | LPAREN _ | LPAREN__LAMBDA _ when env#in_method -> token, rawtok
          | RPAREN _
            when env#in_method && env#paren_level = 0 -> begin
              DEBUG_MSG "shadow_queue=%s" self#shadow_contents;
              let loc = loc_of_poss stp edp in
              DEBUG_MSG "discarding a redundant closing parenthesis";
              Common.warning_loc loc "discarding a redundant closing parenthesis";
              match ulexbuf_opt with
              | Some ulexbuf -> begin
                  shadow_queue#add token;
                  let token = U.get_token queue ulexbuf in
                  DEBUG_MSG "---> %s" (Token.to_string env#current_pos_mgr token);
                  token, Token.to_rawtoken token
              end
              | None -> failwith "Scanner.F.c#get_token"
            end
          | EQ_EQ -> begin
              let t1, rt1 = self#peek_nth 1 in
              match rt1 with
              | EQ_EQ -> begin
                  let t2, rt2 = self#peek_nth 2 in
                  match rt2 with
                  | EQ_EQ -> begin
                      let t3, rt3 = self#peek_nth 3 in
                      match rt3 with
                      | EQ -> begin
                          self#discard_tokens 3;
                          let _, stp, _ = Token.decompose token in
                          let _, _, edp = Token.decompose t3 in
                          let t = Token.create (Tokens_.ERROR "=======") stp edp in
                          DEBUG_MSG "------> %s" (Token.to_string env#current_pos_mgr t);
                          let loc = loc_of_poss stp edp in
                          Common.warning_loc loc "syntax error: marker \"=======\" found";
                          t, Token.to_rawtoken t
                      end
                      | _ -> token, rawtok
                  end
                  | _ -> token, rawtok
              end
              | _ -> token, rawtok
          end
          | OR_OR -> begin
              let t1, rt1 = self#peek_nth 1 in
              match rt1 with
              | OR_OR -> begin
                  let t2, rt2 = self#peek_nth 2 in
                  match rt2 with
                  | OR_OR -> begin
                      let t3, rt3 = self#peek_nth 3 in
                      match rt3 with
                      | OR -> begin
                          self#discard_tokens 3;
                          let _, stp, _ = Token.decompose token in
                          let _, _, edp = Token.decompose t3 in
                          let t = Token.create (Tokens_.ERROR "|||||||") stp edp in
                          DEBUG_MSG "------> %s" (Token.to_string env#current_pos_mgr t);
                          let loc = loc_of_poss stp edp in
                          Common.warning_loc loc "syntax error: marker \"|||||||\" found";
                          t, Token.to_rawtoken t
                      end
                      | _ -> token, rawtok
                  end
                  | _ -> token, rawtok
              end
              | _ -> token, rawtok
          end
          | GT_7 when env#tap_level < 7 -> begin
              let _, stp, edp = Token.decompose token in
              let loc = env#current_pos_mgr#lexposs_to_loc stp edp in
              let el = loc.Astloc.end_line in
              DEBUG_MSG "el=%d" el;
              let buf = Buffer.create 7 in
              Buffer.add_string buf ">>>>>>>";
              let last_cnum = ref edp.Lexing.pos_cnum in
              let last_pos = ref edp in
              let count = ref 1 in
              begin
                try
                  while true do
                    let t, rt = self#peek_nth !count in
                    DEBUG_MSG "%d: %s" !count (Token.to_string env#current_pos_mgr t);
                    let _, sp, ep = Token.decompose t in
                    let l = env#current_pos_mgr#lexposs_to_loc sp ep in
                    if l.Astloc.start_line = el then begin
                      for i = 2 to sp.Lexing.pos_cnum - !last_cnum do
                        Buffer.add_string buf " "
                      done;
                      Buffer.add_string buf (Token.rawtoken_to_orig rt);
                      last_pos := ep;
                      last_cnum := ep.Lexing.pos_cnum;
                      incr count
                    end
                    else
                      raise Exit
                  done
                with
                  Exit -> ()
              end;
              if !count > 1 then
                self#discard_tokens (!count - 1);
              let marker = Buffer.contents buf in
              if env#keep_going_flag then begin
                let loc = loc_of_poss stp !last_pos in
                Common.warning_loc loc "marker \"%s\" found" marker;
                let t = Token.create (Tokens_.MARKER marker) stp !last_pos in
                t, Token.to_rawtoken t
              end
              else begin
                let t = Token.create (Tokens_.ERROR marker) stp !last_pos in
                t, Token.to_rawtoken t
              end
          end
          | GT_7 -> begin
              let _, stp, edp = Token.decompose token in
              self#prepend_token (Token.create Tokens_.GT edp edp);
              self#prepend_token
                (Token.create Tokens_.GT_GT_GT (Astloc.incr_n_lexpos 3 stp) (Astloc.decr_n_lexpos 1 edp));
              let t = Token.create Tokens_.GT_GT_GT stp (Astloc.incr_n_lexpos 2 stp) in
              t, Token.to_rawtoken t
          end
          | MARKER marker when env#keep_going_flag -> begin
              let loc = loc_of_poss stp edp in
              Common.warning_loc loc "marker \"%s\" found" marker;
              token, rawtok
          end
          | MARKER marker when not env#keep_going_flag -> begin
              let _, stp, edp = Token.decompose token in
              let loc = loc_of_poss stp edp in
              Common.warning_loc loc "syntax error: marker \"%s\" found" marker;
              let t = Token.create (Tokens_.ERROR marker) stp edp in
              t, Token.to_rawtoken t
          end
          | GT_GT_GT when env#tap_level <> 7 -> begin
              let t1, rt1 = self#peek_nth 1 in
              match rt1 with
              | GT_GT_GT -> begin
                  let t2, rt2 = self#peek_nth 2 in
                  match rt2 with
                  | GT -> begin
                      self#discard_tokens 2;
                      let _, stp, _ = Token.decompose token in
                      let _, _, edp = Token.decompose t2 in
                      let t = Token.create (Tokens_.ERROR ">>>>>>>") stp edp in
                      DEBUG_MSG "------> %s" (Token.to_string env#current_pos_mgr t);
                      let loc = loc_of_poss stp edp in
                      Common.warning_loc loc "syntax error: marker \">>>>>>>\" found";
                      t, Token.to_rawtoken t
                  end
                  | _ -> token, rawtok
              end
              | _ -> token, rawtok
          end
          | LT_LT -> begin
              let t1, rt1 = self#peek_nth 1 in
              match rt1 with
              | LT_LT -> begin
                  let t2, rt2 = self#peek_nth 2 in
                  match rt2 with
                  | LT_LT -> begin
                      let t3, rt3 = self#peek_nth 3 in
                      match rt3 with
                      | LT _ -> begin
                          self#discard_tokens 3;
                          let _, stp, _ = Token.decompose token in
                          let _, _, edp = Token.decompose t3 in
                          let t = Token.create (Tokens_.ERROR "<<<<<<<") stp edp in
                          DEBUG_MSG "------> %s" (Token.to_string env#current_pos_mgr t);
                          let loc = loc_of_poss stp edp in
                          Common.warning_loc loc "syntax error: marker \"<<<<<<<\" found";
                          t, Token.to_rawtoken t
                      end
                      | _ -> token, rawtok
                  end
                  | _ -> token, rawtok
              end
              | _ -> token, rawtok
          end
          | _ -> token, rawtok
      in
      begin
        match rawtok with
        | EOP (*| ERROR _ | ERROR_STMT _*) -> ()

        | LPAREN _ | LPAREN__LAMBDA _ -> begin
            env#open_paren;
            shadow_queue#add token;
            shadow_q#add token
        end
        | RPAREN _ when env#paren_level > 0 -> begin
            env#close_paren;
            shadow_queue#add token;
            shadow_q#add token
        end
        | LBRACE -> begin
            env#g_open_brace;
            shadow_queue#add token;
            shadow_q#add token
        end
        | RBRACE -> begin
            env#g_close_brace;
            shadow_queue#add token;
            shadow_q#add token
        end
        | _ -> shadow_queue#add token
      end;
      DEBUG_MSG "---------- %s" (Token.to_string env#current_pos_mgr token);
      env#clear_shift_flag;
      last_rawtoken <- rawtok;
      token

    initializer
      env#set_enter_source_callback self#enter_source

  end (* of class Scanner.F.c *)


end (* of functor Scanner.F *)
