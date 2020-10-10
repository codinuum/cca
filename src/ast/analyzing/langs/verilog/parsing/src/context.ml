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
(* context.ml *)

module Loc = Astloc

type tag =
  | Cunknown
  | Ctoplevel
  | Cmodule_item_list
  | Cgenerate_item_list
  | Cblock_decl_stmt_list
  | Ccase_item_list
  | Ccase_inside_item_list
  | Ccellpin_list
  | Clist_of_ports
  | Cpev_expr
  | Cev_expr
  | Cexpr

let tag_to_string = function
  | Cunknown               -> "unknown"
  | Ctoplevel              -> "toplevel"
  | Cmodule_item_list      -> "module_item_list"
  | Cgenerate_item_list    -> "generate_item_list"
  | Cblock_decl_stmt_list  -> "block_decl_stmt_list"
  | Ccase_item_list        -> "case_item_list"
  | Ccase_inside_item_list -> "case_inside_item_list"
  | Ccellpin_list          -> "cellpin_list"
  | Clist_of_ports         -> "list_of_ports"
  | Cpev_expr              -> "pev_expr"
  | Cev_expr               -> "ev_expr"
  | Cexpr                  -> "expr"

type t = { tag               : tag; 
	   mutable is_active : bool; 
	 }

let copy_context c = { tag = c.tag; is_active = c.is_active }

let deactivate_context c = c.is_active <- false

let to_string { tag=tag; is_active=is_active } =
  Printf.sprintf "%s[%sACTIVE]" (tag_to_string tag) (if is_active then "" else "NOT ")

let unknown() = { tag=Cunknown; is_active=false; }

let toplevel() = { tag=Ctoplevel; is_active=true; }

let module_item_list() = { tag=Cmodule_item_list; is_active=true; }

let generate_item_list() = { tag=Cgenerate_item_list; is_active=true; }

let block_decl_stmt_list() = { tag=Cblock_decl_stmt_list; is_active=true; }

let case_item_list() = { tag=Ccase_item_list; is_active=true; }

let case_inside_item_list() = { tag=Ccase_inside_item_list; is_active=true; }

let cellpin_list() = { tag=Ccellpin_list; is_active=true; }

let list_of_ports() = { tag=Clist_of_ports; is_active=true; }

let pev_expr() = { tag=Cpev_expr; is_active=true; }

let ev_expr() = { tag=Cev_expr; is_active=true; }

let expr() = { tag=Cexpr; is_active=true; }


let get_tag { tag=tag; is_active=_; } = tag

let is_unknown c                = c.tag = Cunknown
let is_toplevel c               = c.tag = Ctoplevel
let is_module_item_list c       = c.tag = Cmodule_item_list
let is_generate_item_list c     = c.tag = Cgenerate_item_list
let is_block_decl_stmt_list c   = c.tag = Cblock_decl_stmt_list
let is_case_item_list c         = c.tag = Ccase_item_list
let is_case_inside_item_list c  = c.tag = Ccase_inside_item_list
let is_cellpin_list c           = c.tag = Ccellpin_list
let is_list_of_ports c          = c.tag = Clist_of_ports
let is_pev_expr c               = c.tag = Cpev_expr
let is_ev_expr c                = c.tag = Cev_expr
let is_expr c                   = c.tag = Cexpr


class stack env = object (self)
  val checkpoint_tbl = Hashtbl.create 0 (* Loc.t -> t Stack.t *)

  val mutable stack : t Stack.t = Stack.create()
  val mutable suspended = false
  val mutable push_callback       = fun c -> ()
  val mutable pop_callback        = fun poped c -> ()
  val mutable activate_callback   = fun c -> ()
  val mutable deactivate_callback = fun c -> ()


  method size = Stack.length stack

  method register_push_callback f = push_callback <- f
  method register_pop_callback f = pop_callback <- f
  method register_activate_callback f = activate_callback <- f
  method register_deactivate_callback f = deactivate_callback <- f

  method clear = Stack.clear stack

  method top = Stack.top stack


  method suspended = suspended

  method suspend = 
    DEBUG_MSG "called";
    suspended <- true;

  method resume = 
    DEBUG_MSG "called";
    suspended <- false;

(*
  method _force_pop n stack =
  for i = 1 to n do
  ignore (Stack.pop stack)
  done
 *)

  method checkpoint key = 
    BEGIN_DEBUG
      DEBUG_MSG "key=%s" (Loc.to_string key);
    Stack.iter (fun c -> DEBUG_MSG "stack: %s" (to_string c)) stack;
    END_DEBUG;
(*
  if Hashtbl.mem checkpoint_tbl key then
  WARN_MSG "already checkpointed: key=%s" (Loc.to_string key);
 *)
    let copy = self#_copy_stack stack in
    Hashtbl.replace checkpoint_tbl key copy;

  method recover key = 
    try
      stack <- self#_copy_stack (Hashtbl.find checkpoint_tbl key);

      BEGIN_DEBUG
	DEBUG_MSG "key=%s" (Loc.to_string key);
      Stack.iter (fun c -> DEBUG_MSG "stack: %s" (to_string c)) stack;
      END_DEBUG
    with 
      Not_found -> FATAL_MSG "stack not found: key=%s" (Loc.to_string key); exit 1


  method _copy_stack s =
    let copy = Stack.create() in
    let cs = ref [] in
    Stack.iter
      (fun c ->
	cs := (copy_context c) :: !cs
      ) s;
    List.iter
      (fun c ->
	Stack.push c copy
      ) !cs;
    copy


  method push c =
    if not suspended then begin

      BEGIN_DEBUG
	DEBUG_MSG "%s" (to_string c);
      Stack.iter (fun _c -> DEBUG_MSG "stack: %s" (to_string _c)) stack;
      END_DEBUG;

      Stack.push c stack;
      push_callback c
    end
    else
      DEBUG_MSG "suspended";

    env#set_context_enter_flag

  method pop = self#_pop false
  method pop_and_activate = self#_pop true

  method _pop terminates_surrounding_construct =
    if not suspended then begin

      BEGIN_DEBUG
	Stack.iter 
	(fun _c -> 
	  DEBUG_MSG "stack: %s" (to_string _c)
	) stack;
      END_DEBUG;

      ignore (Stack.pop stack);

      let new_top = 
	try 
	  Stack.top stack 
	with Stack.Empty -> 
	  assert false
      in
      DEBUG_MSG "(new top: %s)" (to_string  new_top);
      
      pop_callback terminates_surrounding_construct new_top
    end
    else
      DEBUG_MSG "suspended"

  method activate_top : unit =
    if not suspended then begin
      let c = self#top in

      if not c.is_active then
	DEBUG_MSG "%s" (to_string c);

      c.is_active <- true
    end;
    env#set_context_activate_flag

  method activate_top_no_delay =
    if not suspended then begin
      let c = self#top in

      if not c.is_active then
	DEBUG_MSG "%s" (to_string c);

      c.is_active <- true;
      activate_callback c
    end

  method deactivate_top =
    if not suspended then begin
      let c = self#top in

      if c.is_active then
	DEBUG_MSG "%s" (to_string c);

      c.is_active <- false
    end

  method deactivate_top_no_delay =
    if not suspended then begin
      let c = self#top in

      if c.is_active then
	DEBUG_MSG "%s" (to_string c);

      c.is_active <- false;
      deactivate_callback c
    end

  method top_is_active =
    let c = self#top in
    c.is_active

  method top_is_unknown =
    let c = self#top in
    is_unknown c

  method reset =
    self#clear;
    Hashtbl.clear checkpoint_tbl;
    self#push (toplevel())


  initializer
    self#reset

end (* of class Context.stack *)
