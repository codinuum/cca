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
 * AST for the Java Language 
 *
 * ast.ml
 *
 *)

open Printf


type identifier = string

type simple_name = identifier

type dims = int (* dimension *)

module Loc = Astloc

type loc = Loc.t



type identifier_attribute = 
  | IApackage
  | IAclass of string
  | IAinterface of string
  | IAtypename of string
  | IAmethod
  | IAfield
  | IAconstructor
  | IAparameter
  | IAvariable
  | IAlabel
  | IAstatic of string
  | IAtypeparameter
  | IAexpression
  | IAarray

let iattr_to_str = function 
  | IApackage     -> "package"
  | IAclass s     -> "class"^(if s = "" then "" else ":"^s)
  | IAinterface s -> "interface"^(if s = "" then "" else ":"^s)
  | IAtypename s  -> "typename"^(if s = "" then "" else ":"^s)
  | IAmethod      -> "method"
  | IAconstructor -> "constructor"
  | IAlabel       -> "label"
  | IAfield       -> "field"
  | IAparameter   -> "parameter"
  | IAvariable    -> "variable"
  | IAstatic s    -> "static member"^(if s = "" then "" else ":"^s)
  | IAtypeparameter -> "type parameter"
  | IAexpression -> "expression"
  | IAarray -> "array"


type frame_kind = FKclass of string | FKtypeparameter | FKother

let frame_kind_to_string = function
  | FKclass s -> "class:"^s
  | FKtypeparameter -> "typeparameter"
  | FKother -> "other"

class frame kind = object (self)
  val tbl = (Hashtbl.create 0 : (string, identifier_attribute) Hashtbl.t)

  val qtbl = (Hashtbl.create 0 : (string, identifier_attribute) Hashtbl.t)

  method is_typeparameter_frame = kind = FKtypeparameter

  method get_class_name =
    match kind with
    | FKclass n -> n
    | _ -> raise Not_found

  method private _add t id attr =
    if not (List.mem attr (Hashtbl.find_all t id)) then
      Hashtbl.add t id attr

  method find id =
    Hashtbl.find tbl id

  method add id attr =
    self#_add tbl id attr

  method find_all id =
    Hashtbl.find_all tbl id


  method qfind qn =
    Hashtbl.find qtbl qn

  method qadd qn attr =
    self#_add qtbl qn attr

  method qfind_all qn =
    Hashtbl.find_all qtbl qn


  method iter f =
    Hashtbl.iter f tbl

  method to_string =
    sprintf "FRAME[%s]:\n{%s}\n{%s}\n"
      (frame_kind_to_string kind)
      (Hashtbl.fold (fun id _ s -> id^";"^s) tbl "")
      (Hashtbl.fold (fun qn _ s -> qn^";"^s) qtbl "")

end

type resolve_result =
  | R_resolved of string
  | R_deferred of string * frame Stack.t * string

let resolve_result_to_str = function
  | R_resolved s -> s
  | R_deferred(id, frames, s) -> sprintf "{DEFERRED:%s?%s}" id s

let mkresolved s = R_resolved s

let is_resolved = function
  | R_resolved _ -> true
  | _ -> false

let is_inner lname =
  String.contains lname '$' 

let split_inner lname = (* parent * rest *)
  let idx = String.index lname '$' in
  String.sub lname 0 idx, String.sub lname idx ((String.length lname) - idx)

type name_attribute = 
  | NApackage
  | NAtype of resolve_result
  | NAexpression
  | NAmethod
  | NApackageOrType
  | NAstatic of resolve_result
  | NAambiguous
  | NAunknown

let iattr_to_nattr = function
  | IApackage     -> NApackage
  | IAmethod      -> NAmethod
  | IAexpression  -> NAexpression
  | _ -> failwith "Ast.iattr_to_nattr"

let get_resolved_name = function
  | NAtype (R_resolved s)
  | NAstatic (R_resolved s) ->
      if s = "" then
        raise Not_found
      else
        s
  | _ -> raise Not_found

type name = { n_desc : name_desc; n_loc : loc; }
and name_desc = 
  | Nsimple of name_attribute ref * identifier 
  | Nqualified of name_attribute ref * name * identifier

let set1 orig a =
  match !orig with
  | NApackageOrType -> begin
      match a with
      | NApackage | NAtype _ -> orig := a
      | _ -> ()
  end
  | NAexpression -> begin
      match a with
      | NAstatic _ -> orig := a
      | _ -> ()
  end
  | NAambiguous -> begin
      match a with
      | NAunknown | NAambiguous -> ()
      | _ -> orig := a
  end
  | NAunknown -> orig := a
  | _ -> ()

let set_name_attribute a n =
  match n.n_desc with
  | Nsimple(at, _) -> set1 at a
  | Nqualified(at, _, _) -> set1 at a

let set_attribute lattr attr name =
  let rec set_attr a n =
    match n.n_desc with
    | Nsimple(at, _) -> set1 at a
    | Nqualified(at, n, _) -> set1 at a; set_attr a n
  in
  match name.n_desc with
  | Nsimple _ -> set_attr attr name
  | Nqualified(at, n, _) -> set_attr lattr n; set1 at attr

let set_attribute_PT_T fqn = set_attribute NApackageOrType (NAtype fqn)
let set_attribute_P_T fqn  = set_attribute NApackage (NAtype fqn)
let set_attribute_PT_PT    = set_attribute NApackageOrType NApackageOrType
let set_attribute_P_P      = set_attribute NApackage NApackage
let set_attribute_A_M      = set_attribute NAambiguous NAmethod
let set_attribute_A_E      = set_attribute NAambiguous NAexpression


let get_name_attribute name =
  match name.n_desc with
  | Nsimple(attr, _)
  | Nqualified(attr, _, _) -> !attr

let decompose_name name =
  match name.n_desc with
  | Nsimple _ -> failwith "Ast.decompose_name"
  | Nqualified(_, n, id) -> n, id

let rec leftmost_of_name n =
  match n.n_desc with
  | Nsimple(attr, id) -> attr, id
  | Nqualified(_, n, _) -> leftmost_of_name n

let rightmost_identifier n =
  match n.n_desc with
  | Nsimple(_, id) -> id
  | Nqualified(_, _, id) -> id

let get_qualifier name =
  match name.n_desc with
  | Nsimple _ -> raise Not_found
  | Nqualified(_, n, _) -> n

let is_simple n =
  match n.n_desc with
  | Nsimple _ -> true
  | _ -> false

let is_qualified n =
  match n.n_desc with
  | Nqualified _ -> true
  | _ -> false

let is_ambiguous_name name =
  (get_name_attribute name) = NAambiguous

let is_unknown_name name =
  (get_name_attribute name) = NAunknown

let dummy_name = { n_desc=Nsimple(ref NAunknown, ""); n_loc=Loc.dummy; }

type literal =
  | Linteger of string
  | LfloatingPoint of string
  | Ltrue
  | Lfalse
  | Lcharacter of string
  | Lstring of string
  | Lnull

type assignment_operator = { ao_desc : assignment_operator_desc; ao_loc : loc; }
and assignment_operator_desc =
  | AOeq
  | AOmulEq
  | AOdivEq
  | AOmodEq
  | AOaddEq
  | AOsubEq
  | AOshiftLEq
  | AOshiftREq
  | AOshiftRUEq
  | AOandEq
  | AOxorEq
  | AOorEq

type unary_operator = 
  | UOpostIncrement 
  | UOpostDecrement 
  | UOpreIncrement 
  | UOpreDecrement
  | UOpositive 
  | UOnegative 
  | UOcomplement 
  | UOnot

type binary_operator =
  | BOmul | BOdiv | BOmod | BOadd | BOsub | BOshiftL | BOshiftR | BOshiftRU
  | BOeq | BOneq | BOlt | BOgt | BOle | BOge
  | BObitAnd | BObitOr | BObitXor | BOand | BOor

type variable_declarator_id = identifier * dims

type javatype = { ty_desc : javatype_desc; ty_loc : loc; }
and primitive_type =
  | PTbyte | PTshort | PTint | PTlong
  | PTchar
  | PTfloat | PTdouble
  | PTboolean

and javatype_desc =
  | Tprimitive of annotation list * primitive_type

  | TclassOrInterface of type_spec list
  | Tclass of type_spec list
  | Tinterface of type_spec list

  | Tarray of javatype (* other than array *) * dims

  | Tvoid (* not a type (only for convenience) *)

and type_spec = 
  | TSname of annotation list * name
  | TSapply of annotation list * name * type_arguments

and type_argument = { ta_desc : type_argument_desc; ta_loc : loc; }
and type_argument_desc =
  | TAreferenceType of javatype
  | TAwildcard of wildcard

and wildcard_bounds = { wb_desc : wildcard_bounds_desc; wb_loc : loc; }
and wildcard_bounds_desc =
  | WBextends of javatype
  | WBsuper of javatype

and wildcard = annotation list * wildcard_bounds option

and type_arguments = { tas_type_arguments : type_argument list; tas_loc : loc; }

and throws = { th_exceptions : javatype list; 
		th_loc        : loc;
	      }

and extends_class = { exc_class : javatype;
		       exc_loc   : loc 
		     }
and extends_interfaces = { exi_interfaces : javatype list; 
			    exi_loc        : loc;
			  }

and implements = { im_interfaces : javatype list; 
		    im_loc        : loc;
		  }

and formal_parameter = 
    { fp_modifiers              : modifiers option;
      fp_type                   : javatype;
      fp_variable_declarator_id : variable_declarator_id;
      fp_variable_arity         : bool;
      fp_loc                    : loc;
    }

and modifiers = { ms_modifiers : modifier list; ms_loc : loc; }

and modifier = { m_desc : modifier_desc; m_loc : loc; }
and modifier_desc = 
  | Mpublic
  | Mprotected
  | Mprivate
  | Mstatic
  | Mabstract
  | Mfinal
  | Mnative
  | Msynchronized
  | Mtransient
  | Mvolatile
  | Mstrictfp
  | Mannotation of annotation
  | Mdefault

and variable_initializer = { vi_desc : variable_initializer_desc; vi_loc : loc; }
and variable_initializer_desc = 
  | VIexpression of expression 
  | VIarray of array_initializer

and array_initializer = variable_initializer list

and variable_declarator = 
    { vd_variable_declarator_id : variable_declarator_id;
      vd_variable_initializer   : variable_initializer option;
      vd_is_local               : bool ref;
      vd_loc                    : loc;
    }

and variable_declarators = variable_declarator list

and class_declaration_head = {
    ch_modifiers       : modifiers option;
    ch_identifier      : identifier;
    ch_type_parameters : type_parameters option;
    ch_extends_class   : extends_class option;
    ch_implements      : implements option;
    ch_loc             : loc;
  }

and class_declaration = { cd_desc : class_declaration_desc; cd_loc : loc; }

and class_declaration_desc =
  | CDclass of class_declaration_head * class_body
  | CDenum  of class_declaration_head * enum_body

and type_parameters = { tps_type_parameters : type_parameter list;
			tps_loc             : loc;
		      }

and type_variable = identifier

and type_parameter = { tp_type_variable : type_variable; 
                       tp_annotations   : annotation list;
		       tp_type_bound    : type_bound option;
		       tp_loc           : loc;
		     }

and type_bound = { tb_reference_type    : javatype; 
		   tb_additional_bounds : additional_bound list;
		   tb_loc               : loc;
		 }
and additional_bound = { ab_interface : javatype; ab_loc : loc; }

and enum_body = { eb_enum_constants          : enum_constant list;
		  eb_class_body_declarations : class_body_declaration list;
		  eb_loc                     : loc;
		}

and enum_constant = { ec_annotations : annotations;
		      ec_identifier  : identifier;
		      ec_arguments   : arguments option;
		      ec_class_body  : class_body option;
		      ec_loc         : loc;
		    }

and class_body = 
    { cb_class_body_declarations : class_body_declaration list;
      cb_loc                     : loc;
    }

and class_body_declaration = { cbd_desc : class_body_declaration_desc; 
			       cbd_loc  : loc;
			     }

and class_body_declaration_desc = 
  | CBDfield of field_declaration
  | CBDmethod of method_header * block option
  | CBDclass of class_declaration
  | CBDinterface of interface_declaration
  | CBDstaticInitializer of block
  | CBDinstanceInitializer of block
  | CBDconstructor of constructor_declaration
  | CBDempty
  | CBDerror

and field_declaration = { fd_modifiers            : modifiers option;
			  fd_type                 : javatype;
			  fd_variable_declarators : variable_declarators;
			  fd_loc                  : loc;
			} 

and method_header = { mh_modifiers       : modifiers option;
		      mh_type_parameters : type_parameters option;
		      mh_return_type     : javatype;
		      mh_name            : identifier;
		      mh_parameters_loc  : loc;
		      mh_parameters      : formal_parameter list;
		      mh_dims            : dims;
		      mh_throws          : throws option;
		      mh_loc             : loc;
		    }

and constructor_declaration = { cnd_modifiers       : modifiers option;
				cnd_type_parameters : type_parameters option;
				cnd_name            : simple_name;
				cnd_parameters_loc  : loc;
				cnd_parameters      : formal_parameter list;
				cnd_throws          : throws option;
				cnd_body            : constructor_body;
				cnd_loc             : loc;
			      }

and constructor_body = 
    { cnb_explicit_constructor_invocation : explicit_constructor_invocation option;
      cnb_block                           : block_statement list;
      cnb_loc                             : loc;
    }

and arguments = { as_arguments : argument list; as_loc : loc; }

and explicit_constructor_invocation = 
    { eci_desc : explicit_constructor_invocation_desc;
      eci_loc  : loc
    }
and explicit_constructor_invocation_desc =
  | ECIthis of type_arguments option * arguments
  | ECIsuper of type_arguments option * arguments
  | ECIprimary of primary * type_arguments option * arguments
  | ECIname of name * type_arguments option * arguments

and interface_declaration_head = {
    ifh_modifiers          : modifiers option;
    ifh_identifier         : identifier;
    ifh_type_parameters    : type_parameters option;
    ifh_extends_interfaces : extends_interfaces option;
    ifh_loc                : loc;
  }

and interface_declaration = { ifd_desc : interface_declaration_desc; 
			      ifd_loc  : loc; }

and interface_declaration_desc =
  | IFDnormal of interface_declaration_head * interface_body
  | IFDannotation of interface_declaration_head	* annotation_type_body

and annotation_type_body =
    { atb_member_declarations : annotation_type_member_declaration list;
      atb_loc                 : loc;
    }

and constant_declaration = field_declaration

and default_value = element_value

and annotation_type_member_declaration =
    { atmd_desc : annotation_type_member_declaration_desc;
      atmd_loc  : loc;
    }
and annotation_type_member_declaration_desc =
  | ATMDconstant of constant_declaration
  | ATMDelement of
      modifiers option *
	javatype *
	identifier *
        annot_dim list *
	default_value option

  | ATMDclass of class_declaration
  | ATMDinterface of interface_declaration
  | ATMDempty

and annot_dim = { ad_annotations : annotations;
                  ad_loc : loc;
                }

and interface_body = { ib_member_declarations : interface_member_declaration list;
		       ib_loc                 : loc;
		     }

and interface_method_declaration = { amd_method_header : method_header;
                                     amd_body          : block option;
				     amd_loc           : loc;
				  }

and interface_member_declaration =
  | IMDconstant of field_declaration
  | IMDinterfaceMethod of interface_method_declaration
  | IMDclass of class_declaration
  | IMDinterface of interface_declaration
  | IMDempty

and block_statement = { bs_desc  : block_statement_desc; 
			bs_loc   : loc; 
		      }
and block_statement_desc =
  | BSlocal of local_variable_declaration
  | BSclass of class_declaration
  | BSstatement of statement
  | BSerror

and local_variable_declaration = 
    { lvd_modifiers            : modifiers option;
      lvd_type                 : javatype;
      lvd_variable_declarators : variable_declarators;
      lvd_loc                  : loc;
    }


and block = { b_block_statements : block_statement list; b_loc : loc; }

and statement = { s_desc  : statement_desc; 
		  s_loc   : loc; 
		}
and statement_desc =
  | Sblock of block
  | Sempty
  | Sexpression of statement_expression
  | Sswitch of expression * switch_block
  | Sdo of statement * expression
  | Sbreak of identifier option
  | Scontinue of identifier option
  | Sreturn of expression option
  | Ssynchronized of expression * block
  | Sthrow of expression
  | Stry of resource_spec option * block * catches option * finally option

  | Slabeled of identifier * statement
  | SifThen of expression * statement
  | SifThenElse of expression * statement * statement
  | Swhile of expression * statement
  | Sfor of 
      for_init option * expression option * statement_expression list 
	* statement
  | SforEnhanced of formal_parameter * expression * statement
  | Sassert1 of expression
  | Sassert2 of expression * expression
  | Serror

and statement_expression = { se_desc : statement_expression_desc; se_loc : loc; }
and statement_expression_desc = 
  | SEassignment of assignment
  | SEpreIncrement of expression
  | SEpreDecrement of expression
  | SEpostIncrement of expression
  | SEpostDecrement of expression
  | SEmethodInvocation of method_invocation
  | SEclassInstanceCreation of class_instance_creation
  | SEerror

and for_init = { fi_desc : for_init_desc; fi_loc : loc; }
and for_init_desc = 
  | FIstatement of statement_expression list
  | FIlocal of local_variable_declaration

and primary = { p_desc : primary_desc; p_loc : loc; }
and primary_desc = 
  | Pname of name
  | Pliteral of literal
  | PclassLiteral of javatype
  | PclassLiteralVoid
  | Pthis
  | PqualifiedThis of name (* of type *)
  | Pparen of expression
  | PclassInstanceCreation of class_instance_creation
  | PfieldAccess of field_access
  | PmethodInvocation of method_invocation
  | ParrayAccess of array_access
  | ParrayCreationExpression of array_creation_expression
  | PmethodReference of method_reference
  | Perror

and method_reference = { mr_desc : method_reference_desc; mr_loc : loc; }
and method_reference_desc =
  | MRname of name * type_arguments option * identifier
  | MRprimary of primary * type_arguments option * identifier
  | MRsuper of type_arguments option * identifier
  | MRtypeSuper of name * type_arguments option * identifier
  | MRtypeNew of name * type_arguments option

and array_creation_expression = 
  | ACEtype of javatype * dim_expr list * dims
  | ACEtypeInit of javatype * dims * array_initializer

and dim_expr = { de_desc : expression; de_loc : loc; }

and class_instance_creation = { cic_desc : class_instance_creation_desc; 
				cic_loc  : loc;
			      }
and class_instance_creation_desc =
  | CICunqualified of 
      type_arguments option * 
	javatype * 
	arguments * 
	class_body option
  | CICqualified of 
      primary * 
	type_arguments option * 
	identifier * 
	type_arguments option * 
	arguments * 
	class_body option
  | CICnameQualified of 
      name * 
	type_arguments option * 
	identifier * 
	type_arguments option * 
	arguments * 
	class_body option


and field_access = 
  | FAprimary of primary * identifier
  | FAsuper of identifier
  | FAclassSuper of name (* of type *) * identifier
  | FAimplicit of identifier

and method_invocation = { mi_desc : method_invocation_desc; mi_loc : loc; }
and method_invocation_desc =
  | MImethodName of name (* of method *) * arguments
  | MIprimary of primary * type_arguments option * identifier * arguments
  | MItypeName of name * type_arguments option * identifier * arguments
  | MIsuper of 
      loc (* of super *) * type_arguments option * identifier * arguments
  | MIclassSuper of 
      loc (* of class *) * loc (* of super *) *
      name (* of type *) * type_arguments option * identifier * arguments

and array_access = { aa_desc : array_access_desc; aa_loc : loc; }
and array_access_desc = 
  | AAname of name (* of expression *) * expression
  | AAprimary of primary * expression

and argument = expression

and expression = { e_desc : expression_desc; e_loc : loc; }
and expression_desc = 
  | Eprimary of primary 
  | Eunary of unary_operator * expression
  | Ebinary of binary_operator * expression * expression
  | Ecast of javatype * expression
  | Einstanceof of expression * javatype
  | Econd of expression * expression * expression
  | Eassignment of assignment
  | Elambda of lambda_params * lambda_body
  | Eerror

and lambda_params = { lp_desc : lambda_params_desc; lp_loc : loc; }
and lambda_params_desc = 
  | LPident of identifier
  | LPformal of formal_parameter list 
  | LPinferred of (loc * identifier) list

and lambda_body = LBexpr of expression | LBblock of block

and annotation = { a_desc : annotation_desc; a_loc : loc; }
and annotation_desc =
  | Anormal of name * element_value_pair list
  | Amarker of name
  | AsingleElement of name * element_value

and annotations = annotation list

and element_value = { ev_desc : element_value_desc; ev_loc : loc; }
and element_value_desc =
  | EVconditional of expression
  | EVannotation of annotation
  | EVarrayInit of element_value list

and element_value_pair = { evp_desc : element_value_pair_desc; evp_loc : loc; }
and element_value_pair_desc = identifier * element_value

and assignment = left_hand_side * assignment_operator * expression

and left_hand_side = expression

and constant_expression = expression (* where ... *)

and switch_label_desc = 
  | SLconstant of constant_expression 
  | SLdefault

and switch_label = { sl_desc : switch_label_desc; sl_loc : loc; }

and switch_block_stmt_grp = (switch_label list * block_statement list)

and switch_block = { sb_switch_block_stmt_grps : switch_block_stmt_grp list;
                     sb_loc                    : loc;
                   }

and catch = { c_formal_parameter : catch_formal_parameter;
	      c_block            : block;
	      c_loc              : loc;
	    }

and catches = catch list

and finally = { f_block : block;
		f_loc   : loc;
	      }

and catch_formal_parameter =
        { cfp_modifiers              : modifiers option;
          cfp_type_list              : javatype list;
          cfp_variable_declarator_id : variable_declarator_id;
          cfp_loc                    : loc;
        }

and resource_spec = { rs_resources : resource list;
                      rs_loc       : loc;
                    }

and resource =
    { r_modifiers              : modifiers option;
      r_type                   : javatype;
      r_variable_declarator_id : variable_declarator_id;
      r_expr                   : expression;
      r_loc                    : loc;
    }

type package_declaration = { pd_annotations : annotations; 
			     pd_name        : name; 
			     pd_loc         : loc;
			   }

type import_declaration = { id_desc : import_declaration_desc; id_loc : loc; }
and import_declaration_desc = 
  | IDsingle of name (* of type *) 
  | IDtypeOnDemand of name (* of package or type *)
  | IDsingleStatic of name (* of type *) * identifier
  | IDstaticOnDemand of name (* of package or type *)

type type_declaration = { td_desc : type_declaration_desc; td_loc : loc; }
and type_declaration_desc = 
  | TDclass of class_declaration 
  | TDinterface of interface_declaration
  | TDempty


type compilation_unit =
    { cu_package   : package_declaration option;
      cu_imports   : import_declaration list;
      cu_tydecls   : type_declaration list;
    }

let mh_is_generic mh = mh.mh_type_parameters <> None



let proc_op proc f op =
  match op with
  | Some x -> proc f x
  | None -> ()

let rec proc_element_value f ev =
  match ev.ev_desc with
  | EVconditional e -> proc_expression f e
  | EVannotation a -> proc_annotation f a
  | EVarrayInit evs -> List.iter (proc_element_value f) evs

and proc_annotation f a =
  match a.a_desc with
  | Anormal(n, evps) ->
      f n;
      List.iter (fun evp -> let _, ev = evp.evp_desc in proc_element_value f ev) evps
  | Amarker n -> f n
  | AsingleElement(n, ev) ->
      f n;
      proc_element_value f ev

and proc_type f ty =
  match ty.ty_desc with
  | TclassOrInterface tss
  | Tclass tss
  | Tinterface tss -> List.iter (proc_type_spec f) tss
  | Tarray(ty0, _) -> proc_type f ty0
  | _ -> ()

and proc_type_spec f ts =
  match ts with
  | TSname(al, n) -> List.iter (proc_annotation f) al; f n
  | TSapply(al, n, tas) ->
      List.iter (proc_annotation f) al;
      f n;
      proc_type_arguments f tas

and proc_expression f e =
  match e.e_desc with
  | Eprimary p -> proc_primary f p
  | Eunary(_, e0) -> proc_expression f e0
  | Ebinary(_, e0, e1) -> List.iter (proc_expression f) [e0; e1]
  | Ecast(ty, e0) -> proc_type f ty; proc_expression f e0
  | Einstanceof(e0, ty) -> proc_expression f e0; proc_type f ty
  | Econd(e0, e1, e2) -> List.iter (proc_expression f) [e0; e1; e2]
  | Eassignment(lhs, _, rhs) -> List.iter (proc_expression f) [lhs; rhs]
  | _ -> ()

and proc_primary f p =
  match p.p_desc with
  | Pname n -> f n
  | PclassLiteral ty -> proc_type f ty
  | PqualifiedThis n -> f n
  | Pparen e -> proc_expression f e
  | PclassInstanceCreation cic -> proc_class_instance_creation f cic
  | PfieldAccess fa -> proc_field_access f fa
  | PmethodInvocation mi -> proc_method_invocation f mi
  | ParrayAccess aa -> proc_array_access f aa
  | ParrayCreationExpression ace -> proc_array_creation_expression f ace
  | PmethodReference mr -> proc_method_reference f mr
  | _ -> ()

and proc_class_instance_creation f cic =
  match cic.cic_desc with
  | CICunqualified(tas_op, ty, args, cb_op) ->
      proc_op proc_type_arguments f tas_op;
      proc_type f ty;
      proc_arguments f args;
      proc_op proc_class_body f cb_op
  | CICqualified(p, tas0_op, _, tas1_op, args, cb_op) ->
      proc_primary f p;
      proc_op proc_type_arguments f tas0_op;
      proc_op proc_type_arguments f tas1_op;
      proc_arguments f args;
      proc_op proc_class_body f cb_op
  | CICnameQualified(n, tas0_op, _, tas1_op, args, cb_op) ->
      f n;
      proc_op proc_type_arguments f tas0_op;
      proc_op proc_type_arguments f tas1_op;
      proc_arguments f args;
      proc_op proc_class_body f cb_op

and proc_class_body_declaration f cbd =
  match cbd.cbd_desc with
  | CBDfield fd -> proc_field_declaration f fd
  | CBDmethod(mh, b_op) -> 
      proc_method_header f mh; 
      proc_op proc_block f b_op
  | CBDclass cd -> proc_class_declaration f cd
  | CBDinterface id -> proc_interface_declaration f id
  | CBDstaticInitializer b -> proc_block f b
  | CBDinstanceInitializer b -> proc_block f b
  | CBDconstructor cd -> proc_constructor_declaration f cd
  | _ -> ()

and proc_type_bound f tb =
  proc_type f tb.tb_reference_type;
  List.iter (fun ab -> proc_type f ab.ab_interface) tb.tb_additional_bounds

and proc_type_parameter f tp =
  proc_op proc_type_bound f tp.tp_type_bound

and proc_type_parameters f tps =
  List.iter (proc_type_parameter f) tps.tps_type_parameters

and proc_method_header f mh =
  proc_op proc_type_parameters f mh.mh_type_parameters;
  proc_type f mh.mh_return_type;
  List.iter (proc_formal_parameter f) mh.mh_parameters;
  proc_op proc_throws f mh.mh_throws

and proc_formal_parameter f fp =
  proc_type f fp.fp_type

and proc_throws f th =
  List.iter (proc_type f) th.th_exceptions

and proc_array_initializer f ai =
  List.iter (proc_variable_initializer f) ai

and proc_variable_initializer f vi =
  match vi.vi_desc with
  | VIexpression e -> proc_expression f e
  | VIarray ai -> proc_array_initializer f ai

and proc_variable_declarator f vd =
  proc_op proc_variable_initializer f vd.vd_variable_initializer

and proc_local_variable_declaration f lvd =
  proc_type f lvd.lvd_type;
  List.iter (proc_variable_declarator f) lvd.lvd_variable_declarators

and proc_statement_expression f se =
  match se.se_desc with
  | SEassignment(lhs, _, rhs) -> List.iter (proc_expression f) [lhs; rhs]
  | SEpreIncrement e
  | SEpreDecrement e
  | SEpostIncrement e
  | SEpostDecrement e -> proc_expression f e
  | SEmethodInvocation mi -> proc_method_invocation f mi
  | SEclassInstanceCreation cic -> proc_class_instance_creation f cic
  | _ -> ()

and proc_switch_label f sl =
  match sl.sl_desc with
  | SLconstant e -> proc_expression f e
  | SLdefault -> ()

and proc_statement f s =
  match s.s_desc with
  | Sblock b -> proc_block f b
  | Sexpression se -> proc_statement_expression f se
  | Sswitch(e, swb) ->
      proc_expression f e;
      List.iter
        (fun (sls, bss) ->
          List.iter (proc_switch_label f) sls;
          List.iter (proc_block_statement f) bss)
        swb.sb_switch_block_stmt_grps
  | Sdo(s0, e) ->
      proc_statement f s0;
      proc_expression f e
  | Sreturn e_op -> proc_op proc_expression f e_op
  | Ssynchronized(e, b) ->
      proc_expression f e;
      proc_block f b
  | Stry(rs_op, b, cts_op, fin_op) ->
      proc_op proc_resource_spec f rs_op;
      proc_block f b;
      proc_op (fun f catches -> List.iter (proc_catch f) catches) f cts_op;
      proc_op (fun f fin -> proc_block f fin.f_block) f fin_op
  | Slabeled(_, s0) -> proc_statement f s0
  | SifThen(e, s0)
  | Swhile(e, s0) ->
      proc_expression f e;
      proc_statement f s0
  | SifThenElse(e, s0, s1) ->
      proc_expression f e;
      List.iter (proc_statement f) [s0; s1]
  | Sfor(fi_op, e_op, ses, s0) ->
      proc_op proc_for_init f fi_op;
      proc_op proc_expression f e_op;
      List.iter (proc_statement_expression f) ses;
      proc_statement f s0
  | SforEnhanced(fp, e, s0) -> 
      proc_formal_parameter f fp;
      proc_expression f e;
      proc_statement f s0
  | Sthrow e
  | Sassert1 e -> proc_expression f e
  | Sassert2(e0, e1) -> List.iter (proc_expression f) [e0; e1]
  | _ -> ()

and proc_resource_spec f rs = List.iter (proc_resource f) rs.rs_resources

and proc_resource f r =
  proc_type f r.r_type;
  proc_expression f r.r_expr

and proc_catch f c =
  proc_catch_formal_parameter f c.c_formal_parameter;
  proc_block f c.c_block

and proc_catch_formal_parameter f cfp =
  List.iter (proc_type f) cfp.cfp_type_list

and proc_for_init f fi =
  match fi.fi_desc with
  | FIstatement ses -> List.iter (proc_statement_expression f) ses
  | FIlocal lvd -> proc_local_variable_declaration f lvd

and proc_block_statement f bs =
  match bs.bs_desc with
  | BSlocal lvd -> proc_local_variable_declaration f lvd
  | BSclass cd -> proc_class_declaration f cd
  | BSstatement s -> proc_statement f s
  | _ -> ()

and proc_block f b =
  List.iter (proc_block_statement f) b.b_block_statements

and proc_extends_class f exc =
  proc_type f exc.exc_class

and proc_implements f im =
  List.iter (proc_type f) im.im_interfaces

and proc_class_declaration_head f ch =
  proc_op proc_type_parameters f ch.ch_type_parameters;
  proc_op proc_extends_class f ch.ch_extends_class;
  proc_op proc_implements f ch.ch_implements

and proc_class_declaration f cd =
  match cd.cd_desc with
  | CDclass(ch, cb) ->
      proc_class_declaration_head f ch;
      proc_class_body f cb
  | CDenum(eh, eb) ->
      proc_class_declaration_head f eh;
      proc_enum_body f eb

and proc_enum_body f eb =
  List.iter (proc_enum_constant f) eb.eb_enum_constants;
  List.iter (proc_class_body_declaration f) eb.eb_class_body_declarations

and proc_enum_constant f ec =
  List.iter (proc_annotation f) ec.ec_annotations;
  proc_op proc_arguments f ec.ec_arguments;
  proc_op proc_class_body f ec.ec_class_body

and proc_interface_declaration_head f ifh =
  proc_op proc_type_parameters f ifh.ifh_type_parameters;
  proc_op (fun f ei -> List.iter (proc_type f) ei.exi_interfaces) f ifh.ifh_extends_interfaces

and proc_interface_declaration f ifd =
  match ifd.ifd_desc with
  | IFDnormal(ih, ib) ->
      proc_interface_declaration_head f ih;
      proc_interface_body f ib
  | IFDannotation(ih, atb) ->
      List.iter (proc_annotation_type_member_declaration f) atb.atb_member_declarations

and proc_annotation_type_member_declaration f atmd =
  match atmd.atmd_desc with
  | ATMDconstant fd -> proc_field_declaration f fd
  | ATMDelement(_, ty, _, dl, dv_op) ->
      proc_type f ty;
      List.iter (fun d -> List.iter (proc_annotation f) d.ad_annotations) dl;
      proc_op proc_element_value f dv_op
  | ATMDclass cd -> proc_class_declaration f cd
  | ATMDinterface id -> proc_interface_declaration f id
  | _ -> ()

and proc_interface_body f ib =
  List.iter (proc_interface_member_declaration f) ib.ib_member_declarations

and proc_interface_member_declaration f imd =
  match imd with
  | IMDconstant fd         -> proc_field_declaration f fd
  | IMDinterfaceMethod amd -> proc_interface_method_declaration f amd
  | IMDclass cd            -> proc_class_declaration f cd
  | IMDinterface ifd       -> proc_interface_declaration f ifd
  | _ -> ()

and proc_field_declaration f fd =
  proc_type f fd.fd_type;
  List.iter (proc_variable_declarator f) fd.fd_variable_declarators

and proc_interface_method_declaration f amd =
  proc_method_header f amd.amd_method_header;
  proc_op proc_block f amd.amd_body

and proc_constructor_declaration f cnd =
  proc_op proc_type_parameters f cnd.cnd_type_parameters;
  List.iter (proc_formal_parameter f) cnd.cnd_parameters;
  proc_op proc_throws f cnd.cnd_throws;
  proc_constructor_body f cnd.cnd_body

and proc_constructor_body f cnb =
  proc_op proc_explicit_constructor_invocation f cnb.cnb_explicit_constructor_invocation;
  List.iter (proc_block_statement f) cnb.cnb_block

and proc_explicit_constructor_invocation f eci =
  match eci.eci_desc with
  | ECIthis(tas_op, args)
  | ECIsuper(tas_op, args) ->
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args
  | ECIprimary(p, tas_op, args) ->
      proc_primary f p;
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args
  | ECIname(n, tas_op, args) ->
      f n;
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args

and proc_method_reference f mr =
  match mr.mr_desc with
  | MRname(n, tas_opt, _)
  | MRtypeSuper(n, tas_opt, _)
  | MRtypeNew(n, tas_opt) ->
      f n;
      proc_op proc_type_arguments f tas_opt

  | MRprimary(p, tas_opt, _) ->
      proc_primary f p;
      proc_op proc_type_arguments f tas_opt

  | MRsuper(tas_opt, _) ->
      proc_op proc_type_arguments f tas_opt

and proc_class_body f cb =
  List.iter (proc_class_body_declaration f) cb.cb_class_body_declarations

and proc_arguments f args =
  List.iter (proc_expression f) args.as_arguments

and proc_type_argument f ta =
  match ta.ta_desc with
  | TAreferenceType ty -> proc_type f ty
  | TAwildcard wc -> proc_wildcard f wc

and proc_wildcard f (al, wb) =
  List.iter (proc_annotation f) al;
  proc_op proc_wildcard_bounds f wb

and proc_wildcard_bounds f wb =
  match wb.wb_desc with
  | WBextends ty 
  | WBsuper ty -> proc_type f ty

and proc_type_arguments f targs =
  List.iter (proc_type_argument f) targs.tas_type_arguments

and proc_field_access f fa =
  match fa with
  | FAprimary(p, _) -> proc_primary f p
  | FAclassSuper(n, _) -> f n
  | _ -> ()

and proc_method_invocation f mi =
  match mi.mi_desc with
  | MImethodName(n, args) ->
      f n;
      proc_arguments f args
  | MIprimary(p, tas_op, _, args) ->
      proc_primary f p;
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args
  | MIsuper(_, tas_op, _, args) ->
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args
  | MItypeName(n, tas_op, _, args)
  | MIclassSuper(_, _, n, tas_op, _, args) ->
      f n;
      proc_op proc_type_arguments f tas_op;
      proc_arguments f args

and proc_array_access f aa =
  match aa.aa_desc with
  | AAname(n, e) ->
      f n;
      proc_expression f e
  | AAprimary(p, e) ->
      proc_primary f p;
      proc_expression f e
and proc_array_creation_expression f ace =
  match ace with
  | ACEtype(ty, des, _) ->
      proc_type f ty;
      List.iter (fun de -> proc_expression f de.de_desc) des
  | ACEtypeInit(ty, _, ai) ->
      proc_type f ty;
      proc_array_initializer f ai
and proc_type_declaration f td =
  match td.td_desc with
  | TDclass cd -> proc_class_declaration f cd
  | TDinterface id -> proc_interface_declaration f id
  | _ -> ()

let proc_import_declaration f id =
  match id.id_desc with
  | IDsingle n 
  | IDtypeOnDemand n
  | IDsingleStatic(n, _)
  | IDstaticOnDemand n -> f n

class c (compilation_unit : compilation_unit) = object (self)
  inherit Ast_base.c

  method compilation_unit = compilation_unit

  method iter_name f =
    begin
      match compilation_unit.cu_package with
      | Some pdecl ->
	  List.iter (proc_annotation f) pdecl.pd_annotations;
	  f pdecl.pd_name
      | None -> ()
    end;
    List.iter (proc_import_declaration f) compilation_unit.cu_imports;
    List.iter (proc_type_declaration f) compilation_unit.cu_tydecls

end (* of class Ast.c *)

let dummy_type = { ty_desc=Tvoid; ty_loc=Loc.dummy; }

(* end of Ast *)
