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
 * An unparser for the Java Language 
 *
 * java_unparsing.ml
 *
 *)


module L = Java_label

open Unparsing_base

let pb = new ppbox

let getlab nd =
  match nd#data#orig_lab_opt with
  | Some o -> (Obj.obj o : L.t)
  | None -> (Obj.obj nd#data#_label : L.t)

let has_orig_lab nd =
  match nd#data#orig_lab_opt with
  | Some o -> (Obj.obj o : L.t) <> (Obj.obj nd#data#_label : L.t)
  | None -> false


(* precedence of operators
 *
 * 15: []  .  (params)  expr++  expr--
 * 14: ++expr  --expr  +expr  -expr  ~  !
 * 13: new  (type)expr
 * 12: *  /  %
 * 11: +  - 
 * 10: <<  >>  >>>
 *  9: <  >  >=  <=  instanceof
 *  8: ==  !=
 *  7: &
 *  6: ^
 *  5: |
 *  4: &&
 *  3: ||
 *  2: ?:
 *  1: =  +=  -=  *=  /=  %=  >>=  <<=  >>>=  &=  ^=  |=
 * 
 *)

let get_prec_of_sym = function
  | "." | "[]" -> 15
  | "::" -> 15 (* ??? *)
  | _ -> 0


let prec_of_assignment_operators = 1



let get_prec_of_primary = function
  | L.Primary.Name _ | L.Primary.AmbiguousName _ -> 15
  | L.Primary.InstanceCreation _
  | L.Primary.ArrayCreationDims _
  | L.Primary.ArrayCreationInit -> 13
  | _ -> 0

let get_prec_of_expression = function
  | L.Expression.UnaryOperator uop -> begin
      match uop with
      | L.UnaryOperator.PostIncrement
      | L.UnaryOperator.PostDecrement -> 15
      | L.UnaryOperator.PreIncrement
      | L.UnaryOperator.PreDecrement
      | L.UnaryOperator.Positive
      | L.UnaryOperator.Negative
      | L.UnaryOperator.Complement
      | L.UnaryOperator.Not -> 14
  end
  | L.Expression.BinaryOperator bop -> begin
      match bop with
      | L.BinaryOperator.Mul
      | L.BinaryOperator.Div
      | L.BinaryOperator.Mod -> 12
      | L.BinaryOperator.Add
      | L.BinaryOperator.Sub -> 11
      | L.BinaryOperator.ShiftL
      | L.BinaryOperator.ShiftR
      | L.BinaryOperator.ShiftRU -> 10
      | L.BinaryOperator.Lt
      | L.BinaryOperator.Gt
      | L.BinaryOperator.Le
      | L.BinaryOperator.Ge -> 9
      | L.BinaryOperator.Eq
      | L.BinaryOperator.Neq -> 8
      | L.BinaryOperator.BitAnd -> 7
      | L.BinaryOperator.BitXor -> 6
      | L.BinaryOperator.BitOr -> 5
      | L.BinaryOperator.And -> 4
      | L.BinaryOperator.Or -> 3
  end
  | L.Expression.AssignmentOperator _ -> 1
  | L.Expression.Primary p -> get_prec_of_primary p
  | L.Expression.Cast -> 13
  | L.Expression.Instanceof -> 9
  | L.Expression.Cond -> 2
  | L.Expression.Lambda -> 0

let get_prec = function
  | L.Primary p    -> get_prec_of_primary p
  | L.Expression e -> get_prec_of_expression e
  | _ -> 0

let undeco_pat = Str.regexp "#[0-9]+"
let undeco = Str.global_replace undeco_pat ""

let find_nodes filt a = 
  Array.of_list 
    (List.filter (fun nd -> filt (getlab nd)) (Array.to_list a))

let dollar_pat = Str.regexp_string "$"
let norm_fqn = Str.global_replace dollar_pat "."

let rec pr_node ?(blk_style=BSshort) ?(prec=0) node =

(*pr_string (L.to_string (getlab node));*)

(*let pr_node_ = pr_node ~blk_style ~prec in*)

  let children = node#initial_children in
  let nchildren = Array.length children in

  (*let pr_nth_child nth = apply_nth pr_node children nth in*)
  let pr_nth_child nth = pr_node children.(nth) in

  match getlab node with
  | L.Error -> ()

  | L.CatchParameter(n, dims) ->
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers children;
      pr_selected ~sep:pr_bor ~tail:pr_space L.is_type children;
      pr_id n; pr_dims dims;
      pb#close_box()

  | L.ResourceSpec ->
      pr_lparen();
      pb#pr_hova pr_semicolon pr_node children;
      pr_rparen()

  | L.Resource(n, dims) ->
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers children;
      pr_selected ~tail:pr_space L.is_type children;
      pr_id n; pr_dims dims; pad 1; pr_string "="; pr_space();
      pr_selected L.is_expression children;
      pb#close_box()

  | L.InferredParameter n -> pr_string n
  | L.InferredParameters ->
      pr_lparen(); pb#pr_a pr_comma pr_node children; pr_rparen()

  | L.CompilationUnit
  | L.ImportDeclarations 
  | L.TypeDeclarations
    -> 
      pb#pr_a force_newline pr_node children

  | L.PackageDeclaration n -> pr_string "package "; pr_name n; pr_semicolon()
  | L.IDsingle n           -> pr_string "import "; pr_name n; pr_semicolon()
  | L.IDtypeOnDemand n     -> pr_string "import "; pr_name n; pr_string ".*;"
  | L.IDsingleStatic(n, i) -> 
      pr_string "import static "; pr_name n; pr_dot(); pr_id i; pr_semicolon()

  | L.IDstaticOnDemand n   -> pr_string "import static "; pr_name n; pr_string ".*;"

  | L.Annotations -> pb#pr_a pr_space pr_node children

  | L.Annotation a -> begin
      match a with
      | L.Annotation.Normal n ->
          pr_string "@"; pr_name n; pr_lparen(); 
          pb#pr_va pr_comma pr_node children;
          pr_rparen()

      | L.Annotation.Marker n -> pr_string "@"; pr_name n

      | L.Annotation.SingleElement n -> 
          pr_string "@"; pr_name n; pr_lparen();
          pr_node children.(0);
          pr_rparen()
  end

  | L.Modifiers _ -> pb#open_hbox(); pb#pr_a pr_space pr_node children; pb#close_box()

  | L.Modifier m -> begin
      match m with
      | L.Modifier.Public       -> pr_string "public"
      | L.Modifier.Protected    -> pr_string "protected"
      | L.Modifier.Private      -> pr_string "private"
      | L.Modifier.Static       -> pr_string "static"
      | L.Modifier.Abstract     -> pr_string "abstract"
      | L.Modifier.Final        -> pr_string "final"
      | L.Modifier.Native       -> pr_string "native"
      | L.Modifier.Synchronized -> pr_string "synchronized"
      | L.Modifier.Transient    -> pr_string "transient"
      | L.Modifier.Volatile     -> pr_string "volatile"
      | L.Modifier.Strictfp     -> pr_string "strictfp"
(*      | L.Modifier.Annotation   -> pr_node children.(0)*)
      | L.Modifier.Default      -> pr_string "default"
  end

  | L.ElementValuePair i -> pr_id i; pr_string "="; pr_node children.(0)
  | L.EVconditional      -> pr_node children.(0) 
  | L.EVannotation       -> pr_node children.(0)
  | L.EVarrayInit        -> pr_string "{"; pb#pr_a pr_comma pr_node children; pr_string "}"

  | L.Specifier _ -> ()

  | L.Class i ->
      let specs = get_specs children in
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers specs;
      pr_string "class "; pr_id i; 
      pr_selected L.is_typeparameters specs;
      pr_selected L.is_extends specs;
      pr_selected L.is_implements specs;
      pb#close_box(); 
      pr_selected ~blk_style L.is_classbody children;
      pb#close_box()

  | L.Enum i ->
      let specs = get_specs children in
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers specs;
      pr_string "enum "; pr_id i; 
      pr_selected L.is_implements specs;
      pb#close_box();
      pr_selected ~blk_style L.is_enumbody children;
      pb#close_box()

  | L.Interface i ->
      let specs = get_specs children in
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers specs;
      pr_string "interface "; pr_id i;
      pr_typeparameters specs;
      pr_selected L.is_extendsinterfaces specs;
      pb#close_box(); 
      pr_selected ~blk_style L.is_interfacebody children; 
      pb#close_box()

  | L.AnnotationType i ->
      let specs = get_specs children in
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers specs;
      pr_string "@interface "; pr_id i;
      pb#close_box(); 
      pr_selected ~blk_style L.is_annotationtypebody children; 
      pb#close_box()

  | L.ClassBody _ ->
        if nchildren = 0 then
          pr_string "{}"
        else begin
          pb#pr_block_begin_tall();
          pb#pr_a pr_cut pr_node children;
          pb#pr_block_end()
        end

  | L.EnumBody _ ->
        if nchildren = 0 then
          pr_string "{}"
        else begin
          pb#pr_block_begin_tall(); 
          pr_selected ~pra:pb#pr_hova ~sep:pr_comma L.is_enumconstant children;
          pr_selected ~blk_style ~head:pr_semicolon ~sep:pr_space L.is_classbodydecl children;
          pb#pr_block_end()
        end

  | L.InterfaceBody _ ->
        if nchildren = 0 then
          pr_string "{}"
        else begin
          pb#pr_block_begin_tall();
          pb#pr_a pr_space pr_node children;
          pb#pr_block_end()
        end

  | L.AnnotationTypeBody _ ->
        if nchildren = 0 then
          pr_string "{}"
        else begin
          pb#pr_block_begin_tall();
          pb#pr_a pr_space pr_node children;
          pb#pr_block_end()
        end

  | L.EnumConstant i ->
      pr_selected ~blk_style ~tail:pr_space L.is_annotations children;
      pr_id i;
      pr_selected ~head:pr_lparen ~tail:pr_rparen L.is_arguments children;
      pr_selected ~blk_style L.is_classbody children

  | L.Super -> ()

  | L.ThisInvocation ->
      pr_typearguments children; 
      pr_string "this"; pr_arguments children; pr_semicolon()

  | L.SuperInvocation ->
      pr_typearguments children; 
      pr_string "super"; pr_arguments children; pr_semicolon()

  | L.PrimaryInvocation ->
      pr_node ~blk_style ~prec:(get_prec_of_sym ".") children.(0);
      pr_dot();
      pr_typearguments children;
      pr_string "super"; pr_arguments children; pr_semicolon()

  | L.NameInvocation n ->
      pr_name n; pr_dot();
      pr_typearguments children;
      pr_string "super"; pr_arguments children; pr_semicolon()

  | L.Constructor(name, signature) ->
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pad1 L.is_modifiers children;
      pr_typeparameters children;
      pr_id name;
      pr_parameters children; pad 1;
      pr_selected L.is_throws children;
      pb#close_box();
      pr_selected ~blk_style L.is_ctorbody children;
      pb#close_box()

  | L.ConstructorBody _ ->
      pb#pr_block_begin_short();
      pr_selected ~sep:pr_space
        (fun lab -> L.is_explicitctorinvok lab || L.is_blockstatement lab)
        children;
      pb#pr_block_end()

  | L.StaticInitializer -> 
      pr_string "static "; pr_node ~blk_style:BStall ~prec children.(0)

  | L.InstanceInitializer -> 
      pr_node ~blk_style:BStall ~prec children.(0)

  | L.Block -> 
      if nchildren = 0 then
        pr_string "{}"
      else begin
        pb#pr_block_begin blk_style; 
        pb#pr_a pr_space pr_node children; 
        pb#pr_block_end()
      end

  | L.Wildcard ->
      pr_selected ~sep:pr_space ~tail:pr_space L.is_annotations children;
      pr_string "?";
      pr_selected L.is_wildcard_bounds children

  | L.WildcardBoundsExtends -> pr_string " extends "; pr_node children.(0)
  | L.WildcardBoundsSuper -> pr_string " super "; pr_node children.(0)

  | L.Extends ->
      pr_break 1 pb#indent; 
      pr_string "extends "; 
      pr_node children.(0)

  | L.Implements ->
      pr_break 1 pb#indent; 
      pr_string "implements ";
      pb#pr_hova pr_comma pr_node children

  | L.ExtendsInterfaces ->
      pr_break 1 pb#indent; 
      pr_string "extends "; 
      pb#pr_hova pr_comma pr_node children

  | L.ElementDeclaration i ->
      pb#open_box 0;
      pr_selected ~tail:pad1 L.is_modifiers children;
      pr_selected L.is_type children; pad 1; pr_id i; pr_string "()";
      pr_selected L.is_annot_dim children;
      pr_selected ~head:(fun _ -> pr_string " default ") L.is_elementvalue children;
      pr_semicolon();
      pb#close_box()

  | L.AnnotDim ->
      pr_selected ~sep:pr_space ~tail:pr_space L.is_annotations children;
      pr_string "[]";

  | L.FieldDeclarations _ -> pb#pr_a pr_space pr_node children

  | L.FieldDeclaration _ ->
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers children;
      pr_selected ~tail:pr_space L.is_type children;
      pr_selected ~sep:pr_comma ~tail:pr_semicolon L.is_variabledeclarator children;
      pb#close_box()

  | L.Primary p -> pr_primary ~prec p children

  | L.Expression e -> pr_expression ~prec e children

  | L.TypeArguments _ ->
      pr_string "<"; pb#pr_a _pr_comma pr_node children; pr_string ">";

  | L.NamedArguments _ | L.Arguments -> pb#pr_a pr_comma pr_node children

  | L.TypeParameters _ ->
      pr_string "<"; pb#pr_a _pr_comma pr_node children; pr_string ">"

  | L.TypeParameter i ->
      pr_selected ~sep:pr_space ~tail:pr_space L.is_annotation children;
      pr_id i;
      pr_selected ~head:(fun () -> pr_string " extends ") L.is_type_bound children;

  | L.TypeBound -> pb#pr_a pr_amp pr_node children

  | L.Type ty ->
      if nchildren = 0 then begin
        pr_string (type_to_string ty)
      end
      else begin
        let rec pr_ty = function
          | L.Type.Byte
          | L.Type.Short
          | L.Type.Int
          | L.Type.Long
          | L.Type.Char
          | L.Type.Float
          | L.Type.Double
          | L.Type.Boolean -> begin
              pr_selected ~sep:pr_space ~tail:pr_space L.is_annotation children;
              pr_string (type_to_string ty)
          end
          | L.Type.ClassOrInterface n
          | L.Type.Class n
          | L.Type.Interface n -> begin
              pr_selected ~tail:pr_dot L.is_type children;
              pr_selected ~sep:pr_space ~tail:pr_space L.is_annotation children;
              pr_name n;
              pr_selected L.is_typearguments children
          end
          | L.Type.Array(ty, dims)    -> pr_ty ty; pr_string (dims_to_string dims)

          | L.Type.Void               -> pr_string "void"
        in
        pr_ty ty
      end

  | L.Qualifier n -> pr_name n; pr_dot()

  | L.DimExpr -> pr_node children.(0)

  | L.ArrayInitializer ->
        pr_string "{"; pr_space(); pb#pr_a pr_comma pr_node children; pr_space(); pr_string "}"

  | L.Throws _ -> pr_break 1 pb#indent; pr_string "throws "; pr_types children; pr_space()

  | L.Parameters _ -> pb#pr_hova pr_comma pr_node children

  | L.Parameter(i, dims, va) ->
      pr_selected ~tail:pr_space L.is_modifiers children;
      pr_selected L.is_type children;
      if va then pr_string "...";
      pad 1; 
      pr_id i; pr_dims dims

  | L.Method i ->
      pb#open_vbox 0;
      pb#open_box 0;
      pr_selected ~tail:pad1 L.is_modifiers children;
      pr_typeparameters children;
      pr_selected L.is_type children; pad 1;
      pr_id i; 
      pr_parameters children;
      pr_selected ~head:pad1 L.is_throws children;
      pb#close_box();
      pr_selected ~blk_style ~head:pad1 ~otherwise:pr_semicolon L.is_methodbody children;
      pb#close_box()

  | L.MethodBody _ ->
      if nchildren = 0 then
        pr_string "{}"
      else begin
        pb#pr_block_begin_short(); 
        pb#pr_a pr_space pr_node children;
        pb#pr_block_end()
      end

  | L.ForInit _ -> pb#pr_a pr_comma pr_node children
  | L.ForCond _ -> pr_node children.(0)
  | L.ForUpdate _ -> pb#pr_a pr_comma pr_node children

  | L.Finally -> pr_string "finally"; pr_node children.(0)
  | L.Catches -> pb#pr_a pr_space pr_node children
  | L.CatchClause ->
      pr_string "catch ("; 
      pr_node children.(0);
      pr_rparen(); pr_space();
      pr_node children.(1)

  | L.SLconstant -> pr_string "case "; pr_node children.(0); pr_string ":"
  | L.SLdefault -> pr_string "default:"

  | L.SwitchBlock ->
      pb#pr_block_begin_short();
      pr_selected ~blk_style ~sep:pr_space L.is_switchblockstmtgroup children;
      pb#pr_block_end()

  | L.SwitchBlockStatementGroup ->
      pb#open_vbox 0;
      pr_selected ~sep:pr_space L.is_switchlabel children; pr_break 1 pb#indent;
      pb#open_vbox 0;
      pr_selected ~sep:pr_space
        (fun lab -> L.is_block lab || L.is_blockstatement lab)
        children;
      pb#close_box();
      pb#close_box()

  | L.Statement stmt -> begin
      match stmt with
      | L.Statement.Empty -> pr_semicolon()

      | L.Statement.Assert ->
          pr_string "assert "; pr_node children.(0);
          if nchildren > 1 then begin
            pr_string ":"; pr_node children.(1)
          end; 
          pr_semicolon()

      | L.Statement.If ->
          pr_string "if ("; pr_node children.(0); pr_rparen();
          begin
            try
              if L.is_block (getlab (children.(1))) then
                pad 1
              else
                pr_break 1 pb#indent;
            with
              _ -> pad 1
          end;
          if nchildren < 2 then
            pr_string "{}"
          else begin
            pr_node children.(1);
            if nchildren > 2 then begin
              let else_part = children.(2) in
              pr_space(); pr_string "else";
              let else_lab = getlab else_part in
              if L.is_block else_lab || L.is_if else_lab then
                pad 1
              else
                pr_break 1 pb#indent;
              pr_node else_part
            end
          end

      | L.Statement.For ->
          pr_string "for ("; 
          pr_selected L.is_forinit children; pr_semicolon();
          pr_selected L.is_forcond children; pr_semicolon();
          pr_selected L.is_forupdate children; pr_rparen(); pr_space();
          pr_selected ~blk_style L.is_statement_or_block children

      | L.Statement.ForEnhanced ->
          pr_string "for (";
          pr_node children.(0);
          pr_string " : ";
          pr_node children.(1);
          pr_rparen();
          pr_space();
          pr_node children.(2)

      | L.Statement.While ->
          pr_string "while ("; pr_nth_child 0; pr_rparen(); pr_space();
          pr_nth_child 1

      | L.Statement.Do ->
          pr_string "do "; pr_node children.(0); 
          pr_string " while ("; pr_node children.(1); pr_rparen(); pr_semicolon()

      | L.Statement.Try ->
          pr_string "try ";
          pr_selected ~tail:pad1 L.is_resource_spec children;
          pr_selected ~tail:pad1 L.is_block children;
          pr_selected ~tail:pad1 L.is_catches children;
          pr_selected ~tail:pad1 L.is_finally children

      | L.Statement.Switch ->
          pr_string "switch ("; pr_node children.(0); pr_rparen(); pr_space();
          pr_node children.(1)

      | L.Statement.Synchronized ->
          pr_string "synchronized ("; pr_node children.(0); pr_rparen(); pr_space();
          pr_node children.(1)

      | L.Statement.Return ->
          if nchildren = 0 then
            pr_string "return;"
          else begin
            pr_string "return "; pr_node children.(0); pr_semicolon()
          end

      | L.Statement.Throw -> pr_string "throw "; pr_node children.(0); pr_semicolon()

      | L.Statement.Break i_opt -> begin
          match i_opt with
	  | None -> pr_string "break;"
          | Some i -> pr_string "break "; pr_id i; pr_semicolon()
      end
      | L.Statement.Continue i_opt -> begin
          match i_opt with
	  | None -> pr_string "continue;"
          | Some i -> pr_string "continue "; pr_id i; pr_semicolon()
      end
      | L.Statement.Labeled i -> pr_id i; pr_string ": "; pr_node children.(0)

      | L.Statement.Expression(e, _) -> 
          pr_expression ~prec e children; pr_semicolon()
  end

  | L.LocalVariableDeclaration(isstmt, _) ->
      pb#open_box 0;
      pr_selected ~tail:pr_space L.is_modifiers children;
      pr_selected ~tail:pr_space L.is_type children;
      pr_selected ~sep:pr_comma L.is_variabledeclarator children;
      if isstmt then begin
        pr_semicolon()
      end;
      pb#close_box()

  | L.VariableDeclarator(i, dims, islocal) ->
      pr_id i; pr_dims dims;
      if nchildren > 0 then begin
        pr_string " ="; pr_break 1 pb#indent; 
        pr_selected L.is_expression children;
        pr_selected L.is_arrayinitializer children
      end


and pr_selected 
    ?(pra=pb#pr_a) 
    ?(sep=pr_none) 
    ?(head=pr_none)
    ?(tail=pr_none) 
    ?(otherwise=pr_none)
    ?(blk_style=BSshort) 
    ?(prec=0) 
    filt a 
    = 
  let a' = find_nodes filt a in
  let present = (Array.length a') > 0 in
  if present then begin
    head();
    pra sep (pr_node ~blk_style ~prec) a';
    tail()
  end
  else
    otherwise()

and pr_types children = pb#pr_hova pr_comma pr_node children

and pr_dims dims = pr_string (dims_to_string dims)

and pr_expressions pr_sep children = pb#pr_hova pr_sep pr_node children 

and pr_arguments children = 
  pr_lparen(); pr_selected L.is_arguments children; pr_rparen() 

and pr_parameters children =
  pr_lparen(); pr_selected L.is_parameters children; pr_rparen() 

and pr_typeparameters children = 
  pr_selected ~tail:pr_space L.is_typeparameters children 

and pr_typearguments ?(nth=1) children = 
  pr_selected (L.is_typearguments ~nth) children 


and dims_to_string dims = if dims = 0 then "" else "[]"^(dims_to_string (dims - 1))

and get_specs children =
  let a = find_nodes L.is_specifier children in
  if Array.length a = 0 then
    [||]
  else
    (a.(0))#initial_children

and type_to_string = function
  | L.Type.Byte               -> "byte"
  | L.Type.Short              -> "short"
  | L.Type.Int                -> "int"
  | L.Type.Long               -> "long"
  | L.Type.Char               -> "char"
  | L.Type.Float              -> "float"
  | L.Type.Double             -> "double"
  | L.Type.Boolean            -> "boolean"
  | L.Type.ClassOrInterface n
  | L.Type.Class n           
  | L.Type.Interface n        -> norm_fqn n
  | L.Type.Array(ty, dims)    -> (type_to_string ty)^(dims_to_string dims)
  | L.Type.Void               -> "void"

and pr_primary ?(prec=0) p children =
  let nchildren = Array.length children in

  let pr_cc() = pr_string "::" in

  match p with
  | L.Primary.Name n -> pr_name (norm_fqn n)
  | L.Primary.This   -> pr_string "this"

  | L.Primary.Literal lit -> begin
      let s =
        match lit with
        | L.Literal.Integer i       -> i
        | L.Literal.FloatingPoint f -> f
        | L.Literal.True            -> "true"
        | L.Literal.False           -> "false"
        | L.Literal.Character c     -> c
        | L.Literal.String s        -> Printf.sprintf "\"%s\"" s
        | L.Literal.Null            -> "null"
      in
      pr_string s
  end

  | L.Primary.ClassLiteral     -> pr_node children.(0); pr_string ".class"
  | L.Primary.ClassLiteralVoid -> pr_string "void.class"
  | L.Primary.QualifiedThis n  -> pr_name n; pr_string ".this"

  | L.Primary.InstanceCreation n ->
      pb#open_vbox 0;
      pb#open_box 0;
      pr_string "new "; 
      pr_typearguments children;
      pr_selected L.is_type children;
      pr_arguments children;
      pb#close_box();
      pr_selected L.is_classbody children;
      pb#close_box()

  | L.Primary.QualifiedInstanceCreation i ->
      pr_selected ~prec:(get_prec_of_sym ".") L.is_primary children; 
      pr_string ".new ";
      pr_typearguments children;
      pr_id i;
      pr_typearguments ~nth:2 children;
      pr_arguments children;
      pr_selected L.is_classbody children

  | L.Primary.NameQualifiedInstanceCreation(n, i) ->
      pr_name n; 
      pr_string ".new "; 
      pr_typearguments children;
      pr_id i;
      pr_typearguments ~nth:2 children;
      pr_arguments children;
      pr_selected L.is_classbody children

  | L.Primary.FieldAccess i ->
      if nchildren > 0 then begin
        pr_node ~prec:(get_prec_of_sym ".") children.(0); pr_dot()
      end; 
      pr_id i

  | L.Primary.SuperFieldAccess i -> pr_string "super."; pr_id i

  | L.Primary.ClassSuperFieldAccess i ->
      pr_node children.(0); pr_string ".super."; pr_id i

  | L.Primary.SimpleMethodInvocation i ->
      pb#open_box 0; 
      pr_selected L.is_qualifier children; pr_id (undeco i);
      pr_arguments children;
      pb#close_box()

  | L.Primary.PrimaryMethodInvocation i ->
      pb#open_box 0; 
      pr_selected ~prec:(get_prec_of_sym ".") L.is_primary children; pr_dot(); 
      pr_typearguments children;
      pr_id (undeco i); pr_arguments children;
      pb#close_box()

  | L.Primary.AmbiguousMethodInvocation i ->
      pb#open_box 0;
      pr_selected ~prec:(get_prec_of_sym ".") L.is_primary children; pr_dot();
      pr_typearguments children;
      pr_id (undeco i); pr_arguments children;
      pb#close_box()

  | L.Primary.TypeMethodInvocation(n, i) ->
      pb#open_box 0; 
      pr_name n; pr_dot(); pr_typearguments children;
      pr_id (undeco i); pr_arguments children;
      pb#close_box()

  | L.Primary.SuperMethodInvocation i ->
      pb#open_box 0; 
      pr_string "super."; pr_typearguments children; 
      pr_id (undeco i); pr_arguments children;
      pb#close_box()

  | L.Primary.ClassSuperMethodInvocation i ->
      pb#open_box 0; 
      pr_selected L.is_type children; pr_string ".super."; pr_typearguments children; 
      pr_id (undeco i); pr_arguments children;
      pb#close_box()

  | L.Primary.ArrayAccess ->
      pr_node ~prec:(get_prec_of_sym "[]") children.(0);
      pr_string "["; pr_node children.(1); pr_string "]"

  | L.Primary.ArrayCreationDims dims ->
      pr_string "new "; pr_node children.(0); pr_string "["; 
      let rest = Array.sub children 1 (nchildren-1) in
      pb#pr_hova (fun () -> pr_string "][") pr_node rest;
      pr_string "]";
      pr_dims dims

  | L.Primary.ArrayCreationInit ->
      pr_string "new "; pr_node children.(0);
      if nchildren > 1 then begin
        pr_node children.(1)
      end

  | L.Primary.Paren _ ->
      let e = children.(0) in
      (*if (get_prec (getlab e)) >= prec then
        pr_node e
      else *)begin
        pr_lparen(); pr_node e; pr_rparen()
      end

  | L.Primary.NameMethodReference(n, i) ->
      pb#open_box 0;
      pr_name n; pr_cc(); pr_typearguments children; pr_id i;
      pb#close_box()

  | L.Primary.PrimaryMethodReference i ->
      pb#open_box 0;
      pr_selected ~prec:(get_prec_of_sym "::") L.is_primary children; pr_cc();
      pr_typearguments children; pr_id i;
      pb#close_box()

  | L.Primary.SuperMethodReference i ->
      pb#open_box 0;
      pr_string "super"; pr_cc(); pr_typearguments children; pr_id i;
      pb#close_box()

  | L.Primary.TypeSuperMethodReference(n, i) ->
      pb#open_box 0;
      pr_name n; pr_string ".super"; pr_cc(); pr_typearguments children; pr_id i;
      pb#close_box()

  | L.Primary.TypeNewMethodReference n ->
      pb#open_box 0;
      pr_name n; pr_cc(); pr_typearguments children; pr_string "new";
      pb#close_box()

  | L.Primary.AmbiguousName n -> pr_name (norm_fqn n)

and pr_expression ?(prec=0) e children =
  let prec' = get_prec_of_expression e in
  match e with
  | L.Expression.Primary p -> pr_primary ~prec p children

  | L.Expression.UnaryOperator uop -> begin
      match uop with
      | L.UnaryOperator.PostIncrement -> pr_node ~prec:prec' children.(0); pr_string "++"
      | L.UnaryOperator.PostDecrement -> pr_node ~prec:prec' children.(0); pr_string "--"
      | L.UnaryOperator.PreIncrement  -> pr_string "++"; pr_node ~prec:prec' children.(0)
      | L.UnaryOperator.PreDecrement  -> pr_string "--"; pr_node ~prec:prec' children.(0)
      | L.UnaryOperator.Positive      -> pr_string "+"; pr_node ~prec:prec' children.(0)
      | L.UnaryOperator.Negative      -> pr_string "-"; pr_node ~prec:prec' children.(0)
      | L.UnaryOperator.Complement    -> pr_string "~"; pr_node ~prec:prec' children.(0)
      | L.UnaryOperator.Not           -> pr_string "!"; pr_node ~prec:prec' children.(0)
  end

  | L.Expression.BinaryOperator bop -> begin
      let s =
        match bop with
        | L.BinaryOperator.Mul     -> "*"
        | L.BinaryOperator.Div     -> "/"
        | L.BinaryOperator.Mod     -> "%"
        | L.BinaryOperator.Add     -> "+"
        | L.BinaryOperator.Sub     -> "-"
        | L.BinaryOperator.ShiftL  -> "<<"
        | L.BinaryOperator.ShiftR  -> ">>"
        | L.BinaryOperator.ShiftRU -> ">>>"
        | L.BinaryOperator.Eq      -> "=="
        | L.BinaryOperator.Neq     -> "!="
        | L.BinaryOperator.Lt      -> "<"
        | L.BinaryOperator.Gt      -> ">"
        | L.BinaryOperator.Le      -> "<="
        | L.BinaryOperator.Ge      -> ">="
        | L.BinaryOperator.BitAnd  -> "&"
        | L.BinaryOperator.BitOr   -> "|"
        | L.BinaryOperator.BitXor  -> "^"
        | L.BinaryOperator.And     -> "&&"
        | L.BinaryOperator.Or      -> "||"
      in
      pr_node ~prec:prec' children.(0);
      pad 1; pr_string s; pad 1;
      pr_node ~prec:prec' children.(1)
  end

  | L.Expression.AssignmentOperator aop -> begin
      let s = 
        match aop with
        | L.AssignmentOperator.Eq        -> "="
        | L.AssignmentOperator.MulEq     -> "*="
        | L.AssignmentOperator.DivEq     -> "/="
        | L.AssignmentOperator.ModEq     -> "%="
        | L.AssignmentOperator.AddEq     -> "+="
        | L.AssignmentOperator.SubEq     -> "-="
        | L.AssignmentOperator.ShiftLEq  -> "<<="
        | L.AssignmentOperator.ShiftREq  -> ">>="
        | L.AssignmentOperator.ShiftRUEq -> ">>>="
        | L.AssignmentOperator.AndEq     -> "&="
        | L.AssignmentOperator.XorEq     -> "^="
        | L.AssignmentOperator.OrEq      -> "|="
      in
      pb#open_box 0;
      pr_node children.(0); pad 1;
      pr_string s; pr_space();
      pr_node ~prec:prec_of_assignment_operators children.(1);
      pb#close_box()
  end

  | L.Expression.Cast ->
      pr_lparen(); pr_node children.(0); pr_rparen(); pr_node ~prec:prec' children.(1)

  | L.Expression.Instanceof ->
      pr_node ~prec:prec' children.(0); pr_string " instanceof "; pr_node children.(1)

  | L.Expression.Cond ->
      pr_node ~prec:prec' children.(0); pr_string " ? ";
      pr_node ~prec:prec' children.(1); pr_string " : "; 
      pr_node ~prec:prec' children.(2)

  | L.Expression.Lambda ->
      pr_node children.(0); pr_string " -> "; pr_node children.(1)

let unparse t = 
  pb#open_box 0;
  pr_string "// generated by Diff/AST Java Unparser"; force_newline();
  pr_node t;
  pb#close_box();
  pr_flush()
