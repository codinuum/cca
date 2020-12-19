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

(* cpp/cpp_label.ml *)

open Printf


let keyroot_depth_min = 4

type tie_id = Lang_base.tie_id

let null_tid      = Lang_base.null_tid
let mktid         = Lang_base.mktid
let tid_to_string = Lang_base.tid_to_string
let anonymize_tid = Lang_base.anonymize_tid
let mktidattr     = Lang_base.mktidattr



module type T = sig
  include Spec.LABEL_T

  val lang_prefix               : string

  val is_pp_directive     : t -> bool
  val is_pp_if_section    : t -> bool
  val is_pp_if_section_broken : t -> bool
  val is_pp_group         : t -> bool
  val is_pp_if_group      : t -> bool
  val is_pp_elif_group    : t -> bool
  val is_pp_else_group    : t -> bool

  val is_translation_unit : t -> bool
  val is_container_unit   : t -> bool
  val is_decl             : t -> bool
  val is_simple_decl      : t -> bool
  val is_func             : t -> bool
  val is_stmt             : t -> bool
  val is_sel_stmt         : t -> bool
  val is_jump_stmt        : t -> bool
  val is_iter_stmt        : t -> bool
  val is_decl_stmt        : t -> bool
  val is_comp_stmt        : t -> bool
  val is_try_block        : t -> bool
  val is_if               : t -> bool
  val is_switch           : t -> bool
  val is_while            : t -> bool
  val is_do               : t -> bool
  val is_for              : t -> bool
  val is_ranged_for       : t -> bool
  val is_return           : t -> bool
  val is_goto             : t -> bool

  val is_literal          : t -> bool
  val is_int_literal      : t -> bool
  val is_real_literal     : t -> bool
  val is_string_literal   : t -> bool
  val is_decl_spec        : t -> bool
  val is_static           : t -> bool
  val is_typedef          : t -> bool
  val is_extern           : t -> bool
  val is_expr_list        : t -> bool
  val is_ident            : t -> bool
  val is_unary_expr       : t -> bool
  val is_funcall          : t -> bool
  val is_array_acc        : t -> bool
  val is_memb_acc         : t -> bool
  val is_memb_ptr_acc     : t -> bool
  val is_param_decl_clause : t -> bool
  val is_param_decl       : t -> bool
  val is_desig            : t -> bool
  val is_desig_old        : t -> bool
  val is_dtor             : t -> bool
  val is_abst_dtor        : t -> bool
  val is_initializer      : t -> bool
  val is_body             : t -> bool
  val is_class_spec       : t -> bool
  val is_enum_spec        : t -> bool
  val is_class            : t -> bool
  val is_struct           : t -> bool
  val is_union            : t -> bool
  val is_namespace_head   : t -> bool
  val is_expr_macro_ivk   : t -> bool
  val is_stmt_macro_ivk   : t -> bool
  val is_type_macro_ivk   : t -> bool
  val is_type_id          : t -> bool
  val is_capture          : t -> bool
  val is_braced_init_list : t -> bool
  val is_args_macro       : t -> bool
  val is_args_macro_ivk   : t -> bool

  val getlab                    : Spec.node_t -> t

  val get_pp_include_path       : t -> string

end


let conv_loc 
    { Ast.Loc.filename     = fn;
      Ast.Loc.start_offset = so;
      Ast.Loc.end_offset   = eo;
      Ast.Loc.start_line   = sl;
      Ast.Loc.start_char   = sc;
      Ast.Loc.end_line     = el;
      Ast.Loc.end_char     = ec;
    } = 
  Loc.make ~fname:fn so eo sl sc el ec

open Charpool


include Label


let to_short_string ?(ignore_identifiers_flag=false) = 
  let combo2 = combo2 ~ignore_identifiers_flag in function 
  | DUMMY               -> mkstr2 0
  | EMPTY               -> mkstr2 1
  | AMBIGUOUS_CONSTRUCT -> mkstr2 2
  | PARTIAL_CONSTRUCT   -> mkstr2 608
  | DECLS               -> mkstr2 3
  | MEM_DECLS           -> mkstr2 4
  | STMTS               -> mkstr2 5
  | INITS               -> mkstr2 609
  | LABELS              -> mkstr2 298
  | SPECS               -> mkstr2 470
  | ETORS               -> mkstr2 478

  | TranslationUnit     -> mkstr2 6

  | PpInclude s   -> combo2 7 [s]
  | PpDefine i    -> combo2 8 [i]
  | PpUndef i     -> combo2 9 [i]
  | PpLine (j, s) -> combo2 10 [string_of_int j;s]
  | PpError s     -> combo2 11 [s]
  | PpPragma s    -> combo2 12 [s]
  | PpNull        -> mkstr2 13
  | PpIf          -> mkstr2 14
  | PpIfdef i     -> combo2 15 [i]
  | PpIfndef i    -> combo2 16 [i]
  | PpElif        -> mkstr2 17
  | PpElse        -> mkstr2 18
  | PpEndif       -> mkstr2 19
  | PpUnknown s   -> combo2 20 [s]
  | PpIfSection j -> combo2 21 [string_of_int j]
  | PpIfSectionFuncDef i     -> combo2 466 [i]
  | PpIfSectionBrokenIf      -> mkstr2 467
  | PpIfSectionBrokenFuncDef -> mkstr2 468
  | PpIfGroup     -> mkstr2 22
  | PpElifGroup   -> mkstr2 23
  | PpElseGroup   -> mkstr2 24
  | PpStringized s -> combo2 25 [s]
  | PpMacroParam i -> combo2 26 [i]
  | SimpleDeclaration             -> mkstr2 27
  | AsmDefinition s               -> combo2 28 [s]
  | NamespaceAliasDefinition i    -> combo2 29 [i]
  | UsingDeclaration              -> mkstr2 30
  | UsingDirective i              -> combo2 31 [i]
  | Static_assertDeclaration      -> mkstr2 32
  | AliasDeclaration i            -> combo2 33 [i]
  | OpaqueEnumDeclaration         -> mkstr2 34
  | OpaqueEnumDeclarationClass    -> mkstr2 35
  | OpaqueEnumDeclarationStruct   -> mkstr2 36
  | NodeclspecFunctionDeclaration -> mkstr2 37
  | FunctionDefinition            -> mkstr2 38
  | TemplateDeclaration           -> mkstr2 39
  | DeductionGuide n              -> combo2 40 [n]
  | ExplicitInstantiation         -> mkstr2 41
  | ExplicitSpecialization        -> mkstr2 42
  | LinkageSpecification s        -> combo2 43 [s]
  | NamedNamespaceDefinition i    -> combo2 44 [i]
  | UnnamedNamespaceDefinition    -> mkstr2 45
  | NestedNamespaceDefinition i   -> combo2 46 [i]
  | EmptyDeclaration              -> mkstr2 47
  | AttributeDeclaration          -> mkstr2 48
  | DeclarationMacro i            -> combo2 49 [i]
  | DeclarationMacroInvocation i  -> combo2 50 [i]
  | ExpressionStatement        -> mkstr2 51
  | DeclarationStatement       -> mkstr2 52
  | TryBlock                   -> mkstr2 53
  | LabeledStatement           -> mkstr2 54
  | SelectionStatement         -> mkstr2 55
  | IfStatement                -> mkstr2 56
  | ElseStatement              -> mkstr2 57
  | SwitchStatement            -> mkstr2 58
  (*| EmptyStatement             -> mkstr2 59*)
  | DummyDecl                  -> mkstr2 59
  | CompoundStatement          -> mkstr2 60
  | IterationStatement         -> mkstr2 61
  | WhileStatement             -> mkstr2 62
  | DoStatement                -> mkstr2 63
  | ForStatement               -> mkstr2 64
  | RangeBasedForStatement     -> mkstr2 65
  | JumpStatement              -> mkstr2 66
  | BreakStatement             -> mkstr2 67
  | ContinueStatement          -> mkstr2 68
  | ReturnStatement            -> mkstr2 69
  | GotoStatement i            -> combo2 70 [i]
  | CoroutineReturnStatement   -> mkstr2 456
  | StatementMacroInvocation i -> combo2 71 [i]
  | StatementMacro i           -> combo2 72 [i]
  | IterationMacroInvocation i -> combo2 73 [i]
  | IterationMacro i           -> combo2 74 [i]
  | This                              -> mkstr2 75
  | ParenthesizedExpression           -> mkstr2 76
  | RequiresExpression                -> mkstr2 77
  | FoldExpression                    -> mkstr2 78
  | LambdaExpression                  -> mkstr2 79
  | LogicalOrExpression i             -> combo2 80 [i]
  | LogicalAndExpression i            -> combo2 81 [i]
  | InclusiveOrExpression i           -> combo2 82 [i]
  | ExclusiveOrExpression i           -> combo2 83 [i]
  | AndExpression i                   -> combo2 84 [i]
  | EqualityExpression                -> mkstr2 85
  | EqualityExpressionEq              -> mkstr2 86
  | EqualityExpressionNeq i           -> combo2 87 [i]
  | RelationalExpression              -> mkstr2 88
  | RelationalExpressionLt            -> mkstr2 89
  | RelationalExpressionGt            -> mkstr2 90
  | RelationalExpressionLe            -> mkstr2 91
  | RelationalExpressionGe            -> mkstr2 92
  | CompareExpression                 -> mkstr2 93
  | ShiftExpression                   -> mkstr2 94
  | ShiftExpressionLeft               -> mkstr2 95
  | ShiftExpressionRight              -> mkstr2 96
  | AdditiveExpression                -> mkstr2 97
  | AdditiveExpressionAdd             -> mkstr2 98
  | AdditiveExpressionSubt            -> mkstr2 99
  | MultiplicativeExpression          -> mkstr2 100
  | MultiplicativeExpressionMult      -> mkstr2 101
  | MultiplicativeExpressionDiv       -> mkstr2 102
  | MultiplicativeExpressionMod       -> mkstr2 103
  | PmExpression                      -> mkstr2 104
  | PmExpressionClass                 -> mkstr2 105
  | PmExpressionPtr                   -> mkstr2 106
  | CastExpression                    -> mkstr2 107
  | UnaryExpression                   -> mkstr2 108
  | UnaryExpressionIncr               -> mkstr2 109
  | UnaryExpressionDecr               -> mkstr2 110
  | UnaryExpressionInd                -> mkstr2 111
  | UnaryExpressionAddr               -> mkstr2 112
  | UnaryExpressionLabelAddr          -> mkstr2 479
  | UnaryExpressionPlus               -> mkstr2 113
  | UnaryExpressionMinus              -> mkstr2 114
  | UnaryExpressionNeg i              -> combo2 115 [i]
  | UnaryExpressionCompl i            -> combo2 116 [i]
  | UnaryExpressionSizeof             -> mkstr2 117
  | UnaryExpressionSizeofPack i       -> combo2 118 [i]
  | UnaryExpressionAlignof            -> mkstr2 119
  | NoexceptExpression                -> mkstr2 120
  | PostfixExpression                 -> mkstr2 121
  | PostfixExpressionSubscr           -> mkstr2 122
  | PostfixExpressionFunCall          -> mkstr2 123
  | PostfixExpressionExplicitTypeConv -> mkstr2 124
  | PostfixExpressionExplicitTypeConvExpr   -> mkstr2 454
  | PostfixExpressionExplicitTypeConvBraced -> mkstr2 455
  | PostfixExpressionDot              -> mkstr2 125
  | PostfixExpressionArrow            -> mkstr2 126
  | PostfixExpressionIncr             -> mkstr2 127
  | PostfixExpressionDecr             -> mkstr2 128
  | PostfixExpressionTypeid           -> mkstr2 129
  | PostfixExpressionTypeidExpr       -> mkstr2 459
  | PostfixExpressionTypeidTy         -> mkstr2 460
  | PostfixExpressionDynamic_cast     -> mkstr2 130
  | PostfixExpressionStatic_cast      -> mkstr2 131
  | PostfixExpressionReinterpret_cast -> mkstr2 132
  | PostfixExpressionConst_cast       -> mkstr2 133
  | AssignmentExpression              -> mkstr2 134
  | AssignmentExpressionEq            -> mkstr2 135
  | AssignmentExpressionPlus          -> mkstr2 136
  | AssignmentExpressionMinus         -> mkstr2 137
  | AssignmentExpressionMult          -> mkstr2 138
  | AssignmentExpressionDiv           -> mkstr2 139
  | AssignmentExpressionMod           -> mkstr2 140
  | AssignmentExpressionShiftLeft     -> mkstr2 141
  | AssignmentExpressionShiftRight    -> mkstr2 142
  | AssignmentExpressionAnd i         -> combo2 143 [i]
  | AssignmentExpressionXor i         -> combo2 144 [i]
  | AssignmentExpressionOr i          -> combo2 145 [i]
  | AssignmentExpressionOverloaded i  -> combo2 472 [i]
  | ThrowExpression                   -> mkstr2 146
  | ExpressionPair                    -> mkstr2 147
  | ConditionalExpression             -> mkstr2 148
  | NewExpression                     -> mkstr2 149
  | DeleteExpression                  -> mkstr2 150
  | DeleteExpressionBracket           -> mkstr2 151
  | AwaitExpression                   -> mkstr2 457
  | YieldExpression                   -> mkstr2 458
  | ExpressionMacroInvocation i       -> combo2 152 [i]
  | DefinedMacroExpression i          -> combo2 153 [i]
  | Literal                       -> mkstr2 154
  | IntegerLiteral v              -> combo2 155 [v]
  | CharacterLiteral v            -> combo2 156 [v]
  | FloatingLiteral v             -> combo2 157 [v]
  | StringLiteral v               -> combo2 158 [v]
  | StringMacro i                 -> combo2 159 [i]
  | BooleanLiteral v              -> combo2 160 [v]
  | Nullptr                       -> mkstr2 161
  | ConcatenatedString            -> mkstr2 162
  | UserDefinedCharacterLiteral v -> combo2 163 [v]
  | UserDefinedStringLiteral v    -> combo2 164 [v]
  | UserDefinedFloatingLiteral v  -> combo2 165 [v]
  | UserDefinedIntegerLiteral v   -> combo2 166 [v]
  | UnqualifiedId        -> mkstr2 167
  | OperatorFunctionId   -> mkstr2 168
  | ConversionFunctionId -> mkstr2 169
  | LiteralOperatorId s  -> combo2 170 [s]
  | Destructor           -> mkstr2 171
  | TemplateId           -> mkstr2 172
  | TemplateIdOp         -> mkstr2 448
  | TemplateIdLit        -> mkstr2 449
  | Operator      -> mkstr2 173
  | New           -> mkstr2 174
  | Delete        -> mkstr2 175
  | NewBracket    -> mkstr2 176
  | DeleteBracket -> mkstr2 177
  | Parentheses   -> mkstr2 178
  | Brackets      -> mkstr2 179
  | MinusGt       -> mkstr2 180
  | MinusGtStar   -> mkstr2 181
  | Tilde i       -> combo2 182 [i]
  | Exclam i      -> combo2 183 [i]
  | Plus          -> mkstr2 184
  | Minus         -> mkstr2 185
  | Star          -> mkstr2 186
  | Slash         -> mkstr2 187
  | Perc          -> mkstr2 188
  | Hat i         -> combo2 189 [i]
  | Amp i         -> combo2 190 [i]
  | Bar i         -> combo2 191 [i]
  | Eq            -> mkstr2 192
  | PlusEq        -> mkstr2 193
  | MinusEq       -> mkstr2 194
  | StarEq        -> mkstr2 195
  | SlashEq       -> mkstr2 196
  | PercEq        -> mkstr2 197
  | HatEq i       -> combo2 198 [i]
  | AmpEq i       -> combo2 199 [i]
  | BarEq i       -> combo2 200 [i]
  | EqEq          -> mkstr2 201
  | ExclamEq i    -> combo2 202 [i]
  | Lt            -> mkstr2 203
  | Gt            -> mkstr2 204
  | LtEq          -> mkstr2 205
  | GtEq          -> mkstr2 206
  | LtEqGt        -> mkstr2 207
  | AmpAmp i      -> combo2 208 [i]
  | BarBar i      -> combo2 209 [i]
  | LtLt          -> mkstr2 210
  | GtGt          -> mkstr2 211
  | LtLtEq        -> mkstr2 212
  | GtGtEq        -> mkstr2 213
  | PlusPlus      -> mkstr2 214
  | MinusMinus    -> mkstr2 215
  | Comma         -> mkstr2 216
  | DotStar       -> mkstr2 217
  | DefiningTypeSpecifier           -> mkstr2 218
  | SimpleTypeSpecifier i           -> combo2 219 [i]
  | ElaboratedTypeSpecifier         -> mkstr2 220
  | ElaboratedTypeSpecifierClass i  -> combo2 221 [i]
  | ElaboratedTypeSpecifierStruct i -> combo2 222 [i]
  | ElaboratedTypeSpecifierUnion i  -> combo2 223 [i]
  | ElaboratedTypeSpecifierEnum i   -> combo2 224 [i]
  | TypenameSpecifier i             -> combo2 225 [i]
  | CvQualifier                     -> mkstr2 226
  | Const                           -> mkstr2 227
  | Volatile                        -> mkstr2 228
  | Restrict i                      -> combo2 229 [i]
  | MsAsmBlock(i, s)                -> combo2 465 [i;s]
  | MsCdecl i                       -> combo2 230 [i]
  | MsStdcall i                     -> combo2 231 [i]
  | CallingConvention i             -> combo2 463 [i]
  | GnuAsmBlock(i, s)               -> combo2 469 [i;s]
  | ClassSpecifier                  -> mkstr2 232
  | EnumSpecifier                   -> mkstr2 233
  | BasicType -> mkstr2 234
  | Char      -> mkstr2 235
  | Char8_t   -> mkstr2 236
  | Char16_t  -> mkstr2 237
  | Char32_t  -> mkstr2 238
  | Wchar_t   -> mkstr2 239
  | Bool      -> mkstr2 240
  | Short     -> mkstr2 241
  | Int       -> mkstr2 242
  | Long      -> mkstr2 243
  | Signed    -> mkstr2 244
  | Unsigned  -> mkstr2 245
  | Float     -> mkstr2 246
  | Double    -> mkstr2 247
  | Void      -> mkstr2 248
  | AccessSpecifier -> mkstr2 249
  | Private         -> mkstr2 250
  | Protected       -> mkstr2 251
  | Public          -> mkstr2 252
  | AttributeSpecifier                  -> mkstr2 253
  | StandardAttributeSpecifier          -> mkstr2 254
  | ContractAttributeSpecifier          -> mkstr2 255
  | ContractAttributeSpecifierExpects   -> mkstr2 256
  | ContractAttributeSpecifierEnsures i -> combo2 257 [i]
  | ContractAttributeSpecifierAssert    -> mkstr2 258
  | AlignmentAttributeSpecifier b       -> combo2 259 [string_of_bool b]
  | AttributeMacro i                    -> combo2 260 [i]
  | RefQualifier       -> mkstr2 261
  | RefQualifierAmp    -> mkstr2 262
  | RefQualifierAmpAmp -> mkstr2 263
  | PlaceholderTypeSpecifier         -> mkstr2 264
  | PlaceholderTypeSpecifierAuto     -> mkstr2 265
  | PlaceholderTypeSpecifierDecltype -> mkstr2 266
  | PtrOperator       -> mkstr2 267
  | PtrOperatorStar   -> mkstr2 268
  | PtrOperatorAmp    -> mkstr2 269
  | PtrOperatorAmpAmp -> mkstr2 270
  | Declarator           -> mkstr2 271
  | DeclaratorFunc       -> mkstr2 272
  | PtrDeclarator        -> mkstr2 273
  | PtrDeclaratorPtr     -> mkstr2 447
  | NoptrDeclarator      -> mkstr2 274
  | NoptrDeclaratorId    -> mkstr2 275
  | NoptrDeclaratorParen -> mkstr2 276
  | NoptrDeclaratorFunc  -> mkstr2 277
  | NoptrDeclaratorArray -> mkstr2 278
  | NoexceptSpecifier      -> mkstr2 279
  | NoexceptSpecifierThrow -> mkstr2 280
  | VirtSpecifier         -> mkstr2 281
  | VirtSpecifierFinal    -> mkstr2 282
  | VirtSpecifierOverride -> mkstr2 283
  | VirtSpecifierMacro i  -> combo2 284 [i]
  | StorageClassSpecifier             -> mkstr2 285
  | StorageClassSpecifierStatic       -> mkstr2 286
  | StorageClassSpecifierThread_local -> mkstr2 287
  | StorageClassSpecifierExtern       -> mkstr2 288
  | StorageClassSpecifierMutable      -> mkstr2 289
  | StorageClassSpecifierRegister     -> mkstr2 471
  | StorageClassSpecifierVaxGlobaldef -> mkstr2 646
  | FunctionSpecifier        -> mkstr2 290
  | FunctionSpecifierVirtual -> mkstr2 291
  | ExplicitSpecifier        -> mkstr2 292
  | ClassHead       -> mkstr2 293
  | ClassHeadClass  -> mkstr2 294
  | ClassHeadStruct -> mkstr2 295
  | ClassHeadUnion  -> mkstr2 296
  | EnumHead           -> mkstr2 300
  | EnumHeadEnum       -> mkstr2 301
  | EnumHeadEnumClass  -> mkstr2 302
  | EnumHeadEnumStruct -> mkstr2 303
  | TypeParameterKey         -> mkstr2 304
  | TypeParameterKeyClass    -> mkstr2 305
  | TypeParameterKeyTypename -> mkstr2 306
  | FunctionBody        -> mkstr2 307
  | FunctionBodyDefault -> mkstr2 308
  | FunctionBodyDelete  -> mkstr2 309
  | FunctionTryBlock    -> mkstr2 310
  | DeclSpecifier          -> mkstr2 311
  | DeclSpecifierInline    -> mkstr2 312
  | DeclSpecifierConstexpr -> mkstr2 313
  | DeclSpecifierConsteval -> mkstr2 314
  | DeclSpecifierTypedef   -> mkstr2 315
  | DeclSpecifierFriend    -> mkstr2 316
  | DeclSpecifierMacro i   -> combo2 473 [i]
  | Requirement           -> mkstr2 317
  | SimpleRequirement     -> mkstr2 318
  | TypeRequirement       -> mkstr2 319
  | CompoundRequirement   -> mkstr2 320
  | ReturnTypeRequirement -> mkstr2 321
  | NestedRequirement     -> mkstr2 322
  | AbstractDeclarator           -> mkstr2 323
  | AbstractDeclaratorFunc       -> mkstr2 324
  | PtrAbstractDeclarator        -> mkstr2 325
  | PtrAbstractDeclaratorPtr     -> mkstr2 446
  | NoptrAbstractDeclarator      -> mkstr2 326
  | NoptrAbstractDeclaratorFunc  -> mkstr2 327
  | NoptrAbstractDeclaratorArray -> mkstr2 328
  | NoptrAbstractDeclaratorParen -> mkstr2 329
  | NoptrAbstractPackDeclaratorFunc  -> mkstr2 330
  | NoptrAbstractPackDeclaratorArray -> mkstr2 498
  | SimpleCapture i       -> combo2 331 [i]
  | SimpleCaptureAmp i    -> combo2 332 [i]
  | SimpleCaptureThis     -> mkstr2 333
  | SimpleCaptureStarThis -> mkstr2 334
  | InitCapture i    -> combo2 335 [i]
  | InitCaptureAmp i -> combo2 336 [i]
  | LambdaCapture           -> mkstr2 337
  | LambdaCaptureDefaultEq  -> mkstr2 338
  | LambdaCaptureDefaultAmp -> mkstr2 339
  | MemberDeclarator           -> mkstr2 340
  | MemberDeclaratorDecl           -> mkstr2 297
  | MemberDeclaratorBitField i -> combo2 341 [i]
  | Label i                        -> combo2 342 [i]
  | CaseLabel                      -> mkstr2 343
  | DefaultLabel                   -> mkstr2 344
  | ContractLevel                  -> mkstr2 345
  | ContractLevelDefault           -> mkstr2 346
  | ContractLevelAudit             -> mkstr2 347
  | ContractLevelAxiom             -> mkstr2 348
  | MemberSpecification            -> mkstr2 349
  | MemberDeclarationDecl          -> mkstr2 350
  | Explicit                       -> mkstr2 351
  | Virtual                        -> mkstr2 352
  | Template                       -> mkstr2 353
  | Noexcept                       -> mkstr2 354
  | Extern                         -> mkstr2 355
  | Inline                         -> mkstr2 356
  | Default                        -> mkstr2 357
  | ColonColon                     -> mkstr2 358
  | Ellipsis                       -> mkstr2 359
  | PureSpecifier                  -> mkstr2 360
  | BaseSpecifier                  -> mkstr2 361
  | BaseClause                     -> mkstr2 362
  | ClassVirtSpecifierFinal        -> mkstr2 363
  | ClassName i                    -> combo2 364 [i]
  | ClassHeadName n                -> combo2 365 [n]
  | MacroArgument                  -> mkstr2 366
  | NoptrNewDeclarator             -> mkstr2 367
  | NewInitializer                 -> mkstr2 368
  | NewInitializerMacro i          -> combo2 369 [i]
  | ArgumentsMacro i               -> combo2 370 [i]
  | NewDeclaratorPtr               -> mkstr2 371
  | NewDeclarator                  -> mkstr2 299
  | NewTypeId                      -> mkstr2 372
  | NewPlacement                   -> mkstr2 373
  | LambdaDeclarator               -> mkstr2 374
  | ParenthesizedInitList          -> mkstr2 375
  | LambdaIntroducer               -> mkstr2 376
  | AbstractPackDeclarator         -> mkstr2 377
  | AbstractPack                   -> mkstr2 378
  | RequirementBody                -> mkstr2 379
  | RequirementParameterList       -> mkstr2 380
  | MemInitializer                 -> mkstr2 381
  | QualifiedTypeName              -> mkstr2 382
  | InitDeclarator                 -> mkstr2 383
  | ConceptDefinition n            -> combo2 384 [n]
  | CtorInitializer                -> mkstr2 385
  | ConstraintLogicalOrExpression i  -> combo2 386 [i]
  | ConstraintLogicalAndExpression i -> combo2 387 [i]
  | RequiresClause                 -> mkstr2 388
  | TypeParameter i                -> combo2 389 [i]
  | TemplateHead                   -> mkstr2 390
  | EnclosingNamespaceSpecifier i  -> combo2 391 [i]
  | Enumerator i                   -> combo2 392 [i]
  | EnumeratorDefinition           -> mkstr2 393
  | TypeMacroInvocation i          -> combo2 394 [i]
  | Condition                      -> mkstr2 395
  | ParameterDeclaration           -> mkstr2 396
  | ParameterDeclarationClause b   -> combo2 397 [string_of_bool b]
  | ParametersAndQualifiers        -> mkstr2 398
  | ParametersMacro i              -> combo2 399 [i]
  | Handler                        -> mkstr2 400
  | ExceptionDeclaration           -> mkstr2 401
  | ExpressionList                 -> mkstr2 402
  | EqualInitializer               -> mkstr2 403
  | DesignatedInitializerClause    -> mkstr2 404
  | DesignatorField i              -> combo2 405 [i]
  | DesignatorFieldOld i           -> combo2 495 [i]
  | DesignatorIndex                -> mkstr2 480
  | TrailingReturnType             -> mkstr2 406
  | BracedInitList                 -> mkstr2 407
  | ForRangeDeclaration            -> mkstr2 408
  | Constexpr                      -> mkstr2 409
  | DefiningTypeId                 -> mkstr2 410
  | EnumHeadName i                 -> combo2 411 [i]
  | EnumBase                       -> mkstr2 412
  | QualifiedId                    -> mkstr2 413
  | QualifiedNamespaceSpecifier n  -> combo2 414 [n]
  | TypeName n                     -> combo2 415 [n]
  | ConversionDeclarator           -> mkstr2 416
  | ConversionTypeId               -> mkstr2 417
  | Typename                       -> mkstr2 418
  | UsingDeclarator                -> mkstr2 419
  | TypeConstraint n               -> combo2 420 [n]
  | TypeId                         -> mkstr2 421
  | DecltypeSpecifier              -> mkstr2 422
  | SimpleTemplateId n             -> combo2 423 [n]
  | Identifier i                   -> combo2 424 [i]
  | IdentifierMacroInvocation i    -> combo2 425 [i]
  | NestedNameSpecifier            -> mkstr2 426
  | NestedNameSpecifierHead        -> mkstr2 450
  | NestedNameSpecifierIdent i     -> combo2 451 [i]
  | NestedNameSpecifierTempl i     -> combo2 452 [i]
  | NestedNameSpecifierDeclty      -> mkstr2 453
  | PackExpansion                  -> mkstr2 427
  | AttributeUsingPrefix           -> mkstr2 428
  | Attribute                      -> mkstr2 429
  | AttributeToken i               -> combo2 430 [i]
  | AttributeScopedToken i         -> combo2 431 [i]
  | AttributeNamespace i           -> combo2 432 [i]
  | AttributeArgumentClause        -> mkstr2 433
  | BalancedToken                  -> mkstr2 434
  | BalancedTokenParen             -> mkstr2 435
  | BalancedTokenBracket           -> mkstr2 436
  | BalancedTokenBrace             -> mkstr2 437
  | BalancedTokenSingle s          -> combo2 438 [s]
  | TokenSeq s                     -> combo2 439 [s]
  | ObjectLikeMacro                -> mkstr2 440
  | FunctionLikeMacro mk           -> combo2 441 [macro_kind_to_string mk]
  | OperatorMacro i                -> combo2 442 [i]
  | DefiningTypeSpecifierSeq       -> mkstr2 443
  | DeclSpecifierSeq               -> mkstr2 444
  | TypeSpecifierSeq               -> mkstr2 445
  | Co_await                       -> mkstr2 461
  | FunctionHead                   -> mkstr2 462
  | AccessSpecAnnot i              -> combo2 464 [i]
  | ParametersMacroInvocation i    -> combo2 474 [i]
  | EnumeratorMacroInvocation i    -> combo2 475 [i]
  | AccessSpecMacro i              -> combo2 476 [i]
  | CvMacro i                      -> combo2 477 [i]

  | Statement                 -> mkstr2 481
  | ComputedGotoStatement     -> mkstr2 482
  | CompoundLiteralExpression -> mkstr2 483
  | BlockLiteralExpression    -> mkstr2 484
  | GnuStatementExpression    -> mkstr2 485
  | PtrOperatorHat            -> mkstr2 486
  | NoptrDeclaratorOldFunc    -> mkstr2 487
  | RangedCaseLabel           -> mkstr2 488
  | ClosingBrace              -> mkstr2 489
  | DummyStmt                 -> mkstr2 490
  | PpMarker(j, s, jl)        ->
      combo2 491 [string_of_int j; s; String.concat ":" (List.map string_of_int jl)]
  | LiteralMacroInvocation i  -> combo2 492 [i]
  | GnuAttribute i            -> combo2 493 [i]
  | PpIfSectionCondExpr       -> mkstr2 494
  | GnuAsmBlockFragmented i   -> combo2 496 [i]
  | GnuAsmFragment s          -> combo2 497 [s]
  | DoxygenLine s                      -> combo2 499 [s]
  | NoexceptSpecifierMacro i           -> combo2 500 [i]
  | AttributeMacroInvocation i         -> combo2 501 [i]
  | CudaExecutionConfiguration         -> mkstr2 502
  | CudaKernelCall                     -> mkstr2 502
  | SimpleTemplateIdM                  -> mkstr2 503
  | NamespaceHead i                    -> combo2 504 [i]
  | InitializerClause                  -> mkstr2 505
  | HasIncludeExpression s             -> combo2 506 [s]
  | HasAttributeExpression             -> mkstr2 507
  | TemplParamMacroInvocation i        -> combo2 508 [i]
  | PostfixExpressionFunCallGuarded i  -> combo2 509 [i]
  | PpIfSectionTemplDecl               -> mkstr2 510
  | PpIfSectionTryBlock                -> mkstr2 511
  | Try                                -> mkstr2 512
  | PpIfSectionHandler                 -> mkstr2 513
  | DELIM_MACRO i                      -> combo2 514 [i]
  | Q_PROPERTY                         -> mkstr2 515
  | PpIfSectionBroken                  -> mkstr2 516
  | PpIfSectionBrokenDtorFunc          -> mkstr2 517
  | DeclSpecifierConstinit             -> mkstr2 518
  | NoexceptSpecifierThrowAny          -> mkstr2 519
  | NamespaceDefinitionMacro i         -> combo2 520 [i]

  | TEMPL_PARAMS                       -> mkstr2 521
  | TEMPL_ARGS                         -> mkstr2 522
  | PpIfSectionAltFuncDef                -> mkstr2 523
  | PpIfSectionLogicalAnd                -> mkstr2 524
  | PpIfSectionLogicalOr                 -> mkstr2 525
  | DeclarationMacroInvocationInvocation -> mkstr2 526
  | ForInStatement                       -> mkstr2 527
  | EqualityExpressionStrictEq           -> mkstr2 528
  | ShiftExpressionRightU                -> mkstr2 529
  | PpConcatenatedIdentifier             -> mkstr2 530
  | DummyExpr                                -> mkstr2 531
  | DeclStmtBlock                            -> mkstr2 532
  | At                                       -> mkstr2 533
  | ClassBody                                -> mkstr2 534
  | ObjcThrow                                -> mkstr2 535
  | ObjcSynchronized                         -> mkstr2 536
  | ObjcClassDeclarationList                 -> mkstr2 537
  | ObjcProtocolDeclarationList              -> mkstr2 538
  | ObjcProtocolReferenceList                -> mkstr2 539
  | ObjcInstanceVariables                    -> mkstr2 540
  | ObjcInstanceVariableDeclaration          -> mkstr2 541
  | ObjcInterfaceDeclaration                 -> mkstr2 542
  | ObjcPrivate                              -> mkstr2 543
  | ObjcPublic                               -> mkstr2 544
  | ObjcPackage                              -> mkstr2 545
  | ObjcProtected                            -> mkstr2 546
  | ObjcStructDeclaration                    -> mkstr2 547
  | ObjcStructDeclarator                     -> mkstr2 548
  | ObjcPropertyDeclaration                  -> mkstr2 549
  | ObjcPropertyAttributesDeclaration        -> mkstr2 550
  | ObjcClassMethodDeclaration               -> mkstr2 551
  | ObjcInstanceMethodDeclaration            -> mkstr2 552
  | ObjcMethodType                           -> mkstr2 553
  | ObjcMethodSelector                       -> mkstr2 554
  | ObjcMethodSelectorPack                   -> mkstr2 555
  | ObjcKeywordSelector                      -> mkstr2 556
  | ObjcMessageExpression                    -> mkstr2 557
  | ObjcMessageSelector                      -> mkstr2 558
  | ObjcProtocolInterfaceDeclarationOptional -> mkstr2 559
  | ObjcProtocolInterfaceDeclarationRequired -> mkstr2 560
  | ObjcAutoreleasepool                      -> mkstr2 561
  | ObjcEncodeExpression                     -> mkstr2 562
  | ObjcTryBlock                             -> mkstr2 563
  | ObjcTry                                  -> mkstr2 564
  | ObjcCatchClause                          -> mkstr2 565
  | ObjcFinally                              -> mkstr2 566
  | ObjcAvailable                            -> mkstr2 631
  | PpImport s                               -> combo2 567 [s]
  | PostfixExpressionFunCallMacro i          -> combo2 568 [i]
  | LogicalOrMacroInvocation i               -> combo2 569 [i]
  | TypeMacro i                              -> combo2 570 [i]
  | MsPragma i                               -> combo2 571 [i]
  | MsWarningSpecifier i                     -> combo2 572 [i]
  | PtrOperatorMacro i                       -> combo2 573 [i]
  | ClassHeadMacroInvocation i               -> combo2 574 [i]
  | FunctionBodyMacro i                      -> combo2 575 [i]
  | DeclSpecifierMacroInvocation i           -> combo2 576 [i]
  | LabelMacroInvocation i                   -> combo2 577 [i]
  | BaseMacro i                              -> combo2 578 [i]
  | SuffixMacro i                            -> combo2 579 [i]
  | ArgumentsMacroInvocation i               -> combo2 580 [i]
  | MemInitMacroInvocation i                 -> combo2 581 [i]
  | EnumeratorDefinitionMacro i              -> combo2 582 [i]
  | ParamDeclMacro i                         -> combo2 583 [i]
  | ParamDeclMacroInvocation i               -> combo2 584 [i]
  | AttributeArgumentClauseMacro i           -> combo2 585 [i]
  | OperatorMacroInvocation i                -> combo2 586 [i]
  | CvMacroInvocation i                      -> combo2 587 [i]
  | NamedNamespaceDefinitionHead i           -> combo2 588 [i]
  | BlockHeadMacro i                         -> combo2 589 [i]
  | BlockEndMacro i                          -> combo2 590 [i]
  | AsmShader s                              -> combo2 591 [s]
  | AsmName i                                -> combo2 592 [i]
  | AsmDirective i                           -> combo2 593 [i]
  | VaArgs s                                 -> combo2 594 [s]
  | PtrMacro i                               -> combo2 595 [i]
  | ObjcProtocolDeclaration i                -> combo2 596 [i]
  | ObjcClassInterface i                     -> combo2 597 [i]
  | ObjcCategoryInterface (i0, i1)           -> combo2 598 [i0; i1]
  | ObjcSuperclass i                         -> combo2 599 [i]
  | ObjcSelector i                           -> combo2 600 [i]
  | ObjcKeywordDeclarator i                  -> combo2 601 [i]
  | ObjcSpecifierQualifier i                 -> combo2 602 [i]
  | ObjcProtocolName i                       -> combo2 603 [i]
  | ObjcClassName i                          -> combo2 604 [i]
  | ObjcPropertyAttribute i                  -> combo2 605 [i]
  | ObjcKeywordArgument i                    -> combo2 606 [i]
  | ObjcSelectorExpression i                 -> combo2 607 [i]

  | DeclarationMacroInvocationArrow -> mkstr2 610
  | DeclarationMacroInvocationDot   -> mkstr2 611
  | RefNewExpression|Semicolon      -> mkstr2 612
  | Dot                             -> mkstr2 613
  | OpeningBrace                    -> mkstr2 614
  | OpeningBracket                  -> mkstr2 615
  | ClosingBracket                  -> mkstr2 616
  | DummyDtor                       -> mkstr2 617
  | Lparen                          -> mkstr2 618
  | Rparen                          -> mkstr2 619
  | Asm                             -> mkstr2 620
  | OpaqueEnumDeclarationMacro i    -> combo2 621 [i]
  | ImportDeclaration s             -> combo2 622 [s]
  | LiteralMacro i                  -> combo2 623 [i]
  | DtorMacro i                     -> combo2 624 [i]
  | VirtSpecifierMacroInvocation i  -> combo2 625 [i]
  | ClassHeadMacro i                -> combo2 626 [i]
  | EnumHeadEnumMacro i             -> combo2 627 [i]
  | FunctionBodyMacroInvocation i   -> combo2 628 [i]
  | LambdaCaptureMacroInvocation i  -> combo2 629 [i]
  | BaseSpecMacro i                 -> combo2 630 [i]

  | HugeArray(sz, c) ->
      let h = Xhash.digest_hex_of_string Xhash.MD5 c in
      combo2 632 [string_of_int sz; h]

  | SwiftArg i   -> combo2 633 [i]
  | SwiftFunCall -> mkstr2 634
  | TemplateArguments -> mkstr2 635
  | MsProperty i -> combo2 636 [i]
  | ObjcMethodMacroInvocation i -> combo2 637 [i]
  | SuffixMacroInvocation i -> combo2 638 [i]
  | ClassHeadMsRefClass -> mkstr2 639
  | ClassVirtSpecifierMsSealed -> mkstr2 640
  | OmpDirective s -> combo2 641 [s]
  | AccDirective s -> combo2 642 [s]
  | MsAttributeSpecifier -> mkstr2 643
  | UnsignedInt  -> mkstr2 644
  | UnsignedLong -> mkstr2 645
  | DummyBody -> mkstr2 647

let _anonymize ?(more=false) ?(most=false) = function
  | SimpleTypeSpecifier _            when most -> DefiningTypeSpecifier
  | ElaboratedTypeSpecifier          when most -> DefiningTypeSpecifier
  | ElaboratedTypeSpecifierClass _   when most -> DefiningTypeSpecifier
  | ElaboratedTypeSpecifierStruct _  when most -> DefiningTypeSpecifier
  | ElaboratedTypeSpecifierUnion _   when most -> DefiningTypeSpecifier
  | ElaboratedTypeSpecifierEnum _    when most -> DefiningTypeSpecifier
  | TypenameSpecifier _              when most -> DefiningTypeSpecifier
  | CvQualifier                      when most -> DefiningTypeSpecifier
  | Const                            when most -> DefiningTypeSpecifier
  | Char                             when most -> DefiningTypeSpecifier
  | Char8_t                          when most -> DefiningTypeSpecifier
  | Char16_t                         when most -> DefiningTypeSpecifier
  | Char32_t                         when most -> DefiningTypeSpecifier
  | Wchar_t                          when most -> DefiningTypeSpecifier
  | Bool                             when most -> DefiningTypeSpecifier
  | Short                            when most -> DefiningTypeSpecifier
  | Int                              when most -> DefiningTypeSpecifier
  | Long                             when most -> DefiningTypeSpecifier
  | Signed                           when most -> DefiningTypeSpecifier
  | Unsigned                         when most -> DefiningTypeSpecifier
  | UnsignedInt                      when most -> DefiningTypeSpecifier
  | UnsignedLong                     when most -> DefiningTypeSpecifier
  | Float                            when most -> DefiningTypeSpecifier
  | Double                           when most -> DefiningTypeSpecifier
  | Void                             when most -> DefiningTypeSpecifier
  | Volatile                         when most -> DefiningTypeSpecifier
  | Restrict _                       when most -> DefiningTypeSpecifier
(*
  | MsCdecl _                        when most -> DefiningTypeSpecifier
  | MsStdcall _                      when most -> DefiningTypeSpecifier
*)
  | ClassSpecifier                   when most -> DefiningTypeSpecifier
  | EnumSpecifier                    when most -> DefiningTypeSpecifier
  | TypeMacroInvocation _            when most -> DefiningTypeSpecifier
  | PlaceholderTypeSpecifierAuto     when most -> DefiningTypeSpecifier
  | PlaceholderTypeSpecifierDecltype when most -> DefiningTypeSpecifier

  | DeclaratorFunc                  when most -> Declarator
  | NoptrDeclaratorId               when most -> Declarator
  | NoptrDeclaratorParen            when most -> Declarator
  | NoptrDeclaratorFunc             when most -> Declarator
  | NoptrDeclaratorArray            when most -> Declarator

  | AbstractDeclaratorFunc          when most -> AbstractDeclarator
  | PtrAbstractDeclarator           when most -> AbstractDeclarator
  | NoptrAbstractDeclaratorParen    when most -> AbstractDeclarator
  | NoptrAbstractDeclaratorFunc     when most -> AbstractDeclarator
  | NoptrAbstractDeclaratorArray    when most -> AbstractDeclarator

  | SimpleTemplateId _              when most -> TemplateId
  | SimpleTemplateIdM               when most -> TemplateId


  | ElaboratedTypeSpecifierStruct _     when more -> ElaboratedTypeSpecifierClass ""
  | ElaboratedTypeSpecifierUnion _      when more -> ElaboratedTypeSpecifierClass ""
  | ElaboratedTypeSpecifierEnum _       when more -> ElaboratedTypeSpecifierClass ""
  | SimpleCaptureAmp _                  when more -> SimpleCapture ""
  | SimpleCaptureThis                   when more -> SimpleCapture ""
  | SimpleCaptureStarThis               when more -> SimpleCapture ""
  | InitCaptureAmp _                    when more -> InitCapture ""
  | MemberDeclaratorDecl                when more -> MemberDeclarator
  | MemberDeclaratorBitField _          when more -> MemberDeclarator
  | LambdaCaptureDefaultEq              when more -> LambdaCapture
  | LambdaCaptureDefaultAmp             when more -> LambdaCapture
  | SimpleRequirement                   when more -> Requirement
  | TypeRequirement                     when more -> Requirement
  | CompoundRequirement                 when more -> Requirement
  | ReturnTypeRequirement               when more -> Requirement
  | NestedRequirement                   when more -> Requirement
  | DeclSpecifierInline                 when more -> DeclSpecifier
  | DeclSpecifierConsteval              when more -> DeclSpecifier
  | DeclSpecifierConstexpr              when more -> DeclSpecifier
  | DeclSpecifierConstinit              when more -> DeclSpecifier
  | DeclSpecifierTypedef                when more -> DeclSpecifier
  | DeclSpecifierFriend                 when more -> DeclSpecifier
  | DeclSpecifierMacro _                when more -> DeclSpecifier
  | FunctionBodyDefault                 when more -> FunctionBody
  | FunctionBodyDelete                  when more -> FunctionBody
  | FunctionTryBlock                    when more -> FunctionBody
  | TypeParameterKeyClass               when more -> TypeParameterKey
  | TypeParameterKeyTypename            when more -> TypeParameterKey
  | EnumHeadEnum                        when more -> EnumHead
  | EnumHeadEnumClass                   when more -> EnumHead
  | EnumHeadEnumStruct                  when more -> EnumHead
  | ClassHeadClass                      when more -> ClassHead
  | ClassHeadStruct                     when more -> ClassHead
  | ClassHeadUnion                      when more -> ClassHead
  | FunctionSpecifierVirtual            when more -> FunctionSpecifier
  | ExplicitSpecifier                   when more -> FunctionSpecifier
  | StorageClassSpecifierStatic         when more -> StorageClassSpecifier
  | StorageClassSpecifierThread_local   when more -> StorageClassSpecifier
  | StorageClassSpecifierExtern         when more -> StorageClassSpecifier
  | StorageClassSpecifierMutable        when more -> StorageClassSpecifier
  | StorageClassSpecifierRegister       when more -> StorageClassSpecifier
  | StorageClassSpecifierVaxGlobaldef   when more -> StorageClassSpecifier
  | VirtSpecifierFinal                  when more -> VirtSpecifier
  | VirtSpecifierOverride               when more -> VirtSpecifier
  | VirtSpecifierMacro _                when more -> VirtSpecifier
  | NoexceptSpecifierThrow              when more -> NoexceptSpecifier
  | NoexceptSpecifierThrowAny           when more -> NoexceptSpecifier
  | PtrOperatorStar                     when more -> PtrOperator
  | PtrOperatorAmp                      when more -> PtrOperator
  | PtrOperatorAmpAmp                   when more -> PtrOperator
  | PlaceholderTypeSpecifierAuto        when more -> PlaceholderTypeSpecifier
  | PlaceholderTypeSpecifierDecltype    when more -> PlaceholderTypeSpecifier
  | RefQualifierAmp                     when more -> RefQualifier
  | RefQualifierAmpAmp                  when more -> RefQualifier
  | StandardAttributeSpecifier          when more -> AttributeSpecifier
  | ContractAttributeSpecifier          when more -> AttributeSpecifier
  | ContractAttributeSpecifierExpects   when more -> AttributeSpecifier
  | ContractAttributeSpecifierEnsures _ when more -> AttributeSpecifier
  | ContractAttributeSpecifierAssert    when more -> AttributeSpecifier
  | AlignmentAttributeSpecifier _       when more -> AttributeSpecifier
  | AttributeMacro _                    when more -> AttributeSpecifier
  | Private                             when more -> AccessSpecifier
  | Protected                           when more -> AccessSpecifier
  | Public                              when more -> AccessSpecifier
  | AccessSpecMacro _                   when more -> AccessSpecifier
  | Char                                when more -> BasicType
  | Char8_t                             when more -> BasicType
  | Char16_t                            when more -> BasicType
  | Char32_t                            when more -> BasicType
  | Wchar_t                             when more -> BasicType
  | Bool                                when more -> BasicType
  | Short                               when more -> BasicType
  | Int                                 when more -> BasicType
  | Long                                when more -> BasicType
  | Signed                              when more -> BasicType
  | Unsigned                            when more -> BasicType
  | UnsignedInt                         when more -> BasicType
  | UnsignedLong                        when more -> BasicType
  | Float                               when more -> BasicType
  | Double                              when more -> BasicType
  | Void                                when more -> BasicType
  | Const                               when more -> CvQualifier
  | Volatile                            when more -> CvQualifier
  | Restrict _                          when more -> CvQualifier
  | CvMacro _                           when more -> CvQualifier
  | MsCdecl _                           when more -> CallingConvention ""
  | MsStdcall _                         when more -> CallingConvention ""
  | ElaboratedTypeSpecifierClass _      when more -> ElaboratedTypeSpecifier
  | ElaboratedTypeSpecifierStruct _     when more -> ElaboratedTypeSpecifier
  | ElaboratedTypeSpecifierUnion _      when more -> ElaboratedTypeSpecifier
  | ElaboratedTypeSpecifierEnum _       when more -> ElaboratedTypeSpecifier
  | New                                 when more -> Operator
  | Delete                              when more -> Operator
  | NewBracket                          when more -> Operator
  | DeleteBracket                       when more -> Operator
  | Parentheses                         when more -> Operator
  | Brackets                            when more -> Operator
  | MinusGt                             when more -> Operator
  | MinusGtStar                         when more -> Operator
  | Tilde _                             when more -> Operator
  | Exclam _                            when more -> Operator
  | Plus                                when more -> Operator
  | Minus                               when more -> Operator
  | Star                                when more -> Operator
  | Slash                               when more -> Operator
  | Perc                                when more -> Operator
  | Hat _                               when more -> Operator
  | Amp _                               when more -> Operator
  | Bar _                               when more -> Operator
  | Eq                                  when more -> Operator
  | PlusEq                              when more -> Operator
  | MinusEq                             when more -> Operator
  | StarEq                              when more -> Operator
  | SlashEq                             when more -> Operator
  | PercEq                              when more -> Operator
  | HatEq _                             when more -> Operator
  | AmpEq _                             when more -> Operator
  | BarEq _                             when more -> Operator
  | EqEq                                when more -> Operator
  | ExclamEq _                          when more -> Operator
  | Lt                                  when more -> Operator
  | Gt                                  when more -> Operator
  | LtEq                                when more -> Operator
  | GtEq                                when more -> Operator
  | LtEqGt                              when more -> Operator
  | AmpAmp _                            when more -> Operator
  | BarBar _                            when more -> Operator
  | LtLt                                when more -> Operator
  | GtGt                                when more -> Operator
  | LtLtEq                              when more -> Operator
  | GtGtEq                              when more -> Operator
  | PlusPlus                            when more -> Operator
  | MinusMinus                          when more -> Operator
  | Comma                               when more -> Operator
  | DotStar                             when more -> Operator

  | OperatorFunctionId                  when more -> UnqualifiedId
  | ConversionFunctionId                when more -> UnqualifiedId
  | LiteralOperatorId _                 when more -> UnqualifiedId
  | Destructor                          when more -> UnqualifiedId
  | TemplateId                          when more -> UnqualifiedId
  | IterationMacroInvocation _          when more -> UnqualifiedId

  | IntegerLiteral _                    when more -> Literal
  | CharacterLiteral _                  when more -> Literal
  | FloatingLiteral _                   when more -> Literal
  | StringLiteral _                     when more -> Literal
  | StringMacro _                       when more -> Literal
  | BooleanLiteral _                    when more -> Literal
  | Nullptr                             when more -> Literal
  | ConcatenatedString                  when more -> Literal
  | UserDefinedCharacterLiteral _       when more -> Literal
  | UserDefinedStringLiteral _          when more -> Literal
  | UserDefinedFloatingLiteral _        when more -> Literal
  | UserDefinedIntegerLiteral _         when more -> Literal

  | AssignmentExpressionPlus            when more -> AssignmentExpression
  | AssignmentExpressionMinus           when more -> AssignmentExpression
  | AssignmentExpressionMult            when more -> AssignmentExpression
  | AssignmentExpressionDiv             when more -> AssignmentExpression
  | AssignmentExpressionMod             when more -> AssignmentExpression
  | AssignmentExpressionShiftLeft       when more -> AssignmentExpression
  | AssignmentExpressionShiftRight      when more -> AssignmentExpression
  | AssignmentExpressionAnd _           when more -> AssignmentExpression
  | AssignmentExpressionXor _           when more -> AssignmentExpression
  | AssignmentExpressionOr _            when more -> AssignmentExpression
  | AssignmentExpressionOverloaded _    when more -> AssignmentExpression
  | YieldExpression                     when more -> AssignmentExpression

  | PostfixExpressionSubscr             when more -> PostfixExpression
  | PostfixExpressionFunCall            when more -> PostfixExpression
  | PostfixExpressionExplicitTypeConv   when more -> PostfixExpression
  | PostfixExpressionDot                when more -> PostfixExpression
  | PostfixExpressionArrow              when more -> PostfixExpression
  | PostfixExpressionIncr               when more -> PostfixExpression
  | PostfixExpressionDecr               when more -> PostfixExpression
  | PostfixExpressionTypeid             when more -> PostfixExpression
  | PostfixExpressionDynamic_cast       when more -> PostfixExpression
  | PostfixExpressionStatic_cast        when more -> PostfixExpression
  | PostfixExpressionReinterpret_cast   when more -> PostfixExpression
  | PostfixExpressionConst_cast         when more -> PostfixExpression
  | PostfixExpressionExplicitTypeConv       when more -> PostfixExpression
  | PostfixExpressionExplicitTypeConvExpr   when more -> PostfixExpression
  | PostfixExpressionExplicitTypeConvBraced when more -> PostfixExpression

  | UnaryExpressionIncr                 when more -> UnaryExpression
  | UnaryExpressionDecr                 when more -> UnaryExpression
  | UnaryExpressionInd                  when more -> UnaryExpression
  | UnaryExpressionAddr                 when more -> UnaryExpression
  | UnaryExpressionLabelAddr            when more -> UnaryExpression
  | UnaryExpressionPlus                 when more -> UnaryExpression
  | UnaryExpressionMinus                when more -> UnaryExpression
  | UnaryExpressionNeg _                when more -> UnaryExpression
  | UnaryExpressionCompl _              when more -> UnaryExpression
  | UnaryExpressionSizeof               when more -> UnaryExpression
  | UnaryExpressionSizeofPack _         when more -> UnaryExpression
  | UnaryExpressionAlignof              when more -> UnaryExpression
  | AwaitExpression                     when more -> UnaryExpression
  | PmExpressionClass                   when more -> PmExpression
  | PmExpressionPtr                     when more -> PmExpression
  | MultiplicativeExpressionMult        when more -> MultiplicativeExpression
  | MultiplicativeExpressionDiv         when more -> MultiplicativeExpression
  | MultiplicativeExpressionMod         when more -> MultiplicativeExpression
  | ShiftExpressionLeft                 when more -> ShiftExpression
  | ShiftExpressionRight                when more -> ShiftExpression
  | RelationalExpressionLt              when more -> RelationalExpression
  | RelationalExpressionGt              when more -> RelationalExpression
  | RelationalExpressionLe              when more -> RelationalExpression
  | RelationalExpressionGe              when more -> RelationalExpression
  | EqualityExpressionEq                when more -> EqualityExpression
  | EqualityExpressionNeq _             when more -> EqualityExpression
  | PpIfSectionCondExpr                 when more -> ConditionalExpression
  | PpIfSectionLogicalAnd               when more -> LogicalAndExpression ""
  | PpIfSectionLogicalOr                when more -> LogicalOrExpression ""
  (*| CaseLabel                           when more -> Label ""
  | DefaultLabel                        when more -> Label ""*)
  | BreakStatement                      when more -> JumpStatement
  | ContinueStatement                   when more -> JumpStatement
  | ReturnStatement                     when more -> JumpStatement
  | GotoStatement _                     when more -> JumpStatement
  | ComputedGotoStatement               when more -> JumpStatement
  | CoroutineReturnStatement            when more -> JumpStatement
  | WhileStatement                      when more -> IterationStatement
  | DoStatement                         when more -> IterationStatement
  | ForStatement                        when more -> IterationStatement
  | RangeBasedForStatement              when more -> IterationStatement
  | IfStatement                         when more -> SelectionStatement
  | ElseStatement                       when more -> SelectionStatement
  | SwitchStatement                     when more -> SelectionStatement
  | ContractLevelDefault                when more -> ContractLevel
  | ContractLevelAudit                  when more -> ContractLevel
  | ContractLevelAxiom                  when more -> ContractLevel
  | BalancedTokenSingle _               when more -> BalancedToken
  | BalancedTokenParen                  when more -> BalancedToken
  | BalancedTokenBracket                when more -> BalancedToken
  | BalancedTokenBrace                  when more -> BalancedToken
  | NoptrDeclaratorId                   when more -> NoptrDeclarator
  | NoptrDeclaratorParen                when more -> NoptrDeclarator
  | NoptrDeclaratorFunc                 when more -> NoptrDeclarator
  | NoptrDeclaratorOldFunc              when more -> NoptrDeclarator
  | NoptrDeclaratorArray                when more -> NoptrDeclarator
  | NoptrAbstractDeclaratorParen        when more -> NoptrAbstractDeclarator
  | NoptrAbstractDeclaratorFunc         when more -> NoptrAbstractDeclarator
  | NoptrAbstractDeclaratorArray        when more -> NoptrAbstractDeclarator
  | NoptrNewDeclarator                  when more -> NewDeclarator
  | NewDeclaratorPtr                    when more -> NewDeclarator
  | TemplateIdOp                        when more -> TemplateId
  | TemplateIdLit                       when more -> TemplateId
  | NestedNameSpecifierHead             when more -> NestedNameSpecifier
  | NestedNameSpecifierIdent _          when more -> NestedNameSpecifier
  | NestedNameSpecifierTempl _          when more -> NestedNameSpecifier
  | NestedNameSpecifierDeclty           when more -> NestedNameSpecifier

  | PpIfSectionFuncDef i                when more -> PpIfSection 0
  | PpIfSectionBrokenIf                 when more -> PpIfSection 0
  | PpIfSectionBrokenFuncDef            when more -> PpIfSection 0
  | PpIfSectionAltFuncDef               when more -> PpIfSection 0

  | EnumeratorMacroInvocation _         when more -> Enumerator ""
  | ParametersMacroInvocation _         when more -> ParametersAndQualifiers
  | MsAsmBlock _                        when more -> AsmDefinition ""
  | GnuAsmBlock _                       when more -> AsmDefinition ""
  | GnuAsmBlockFragmented _             when more -> AsmDefinition ""
  | IdentifierMacroInvocation _         when more -> Identifier ""

  | PpInclude _                         -> PpInclude ""
  | PpDefine _                          -> PpDefine ""
  | PpUndef _                           -> PpUndef ""
  | PpLine _                            -> PpLine(0, "")
  | PpError _                           -> PpError ""
  | PpPragma _                          -> PpPragma ""
  | PpIfdef _                           -> PpIfdef ""
  | PpIfndef _                          -> PpIfndef ""
  | PpUnknown _                         -> PpUnknown ""
  | PpIfSection _                       -> PpIfSection 0
  | PpMarker _                          -> PpMarker(0, "", [])
  | PpImport _                          -> PpImport ""
  | OmpDirective _                      -> OmpDirective ""
  | AccDirective _                      -> AccDirective ""
  | AsmDefinition _                     -> AsmDefinition ""
  | UsingDirective _                    -> UsingDirective ""
  | NamespaceAliasDefinition _          -> NamespaceAliasDefinition ""
  | AliasDeclaration _                  -> AliasDeclaration ""
  | DeductionGuide _                    -> DeductionGuide ""
  | LinkageSpecification _              -> LinkageSpecification ""
  | NamedNamespaceDefinition _          -> NamedNamespaceDefinition ""
  | NestedNamespaceDefinition _         -> NestedNamespaceDefinition ""
  | NamespaceDefinitionMacro _          -> NamespaceDefinitionMacro ""
  | DeclarationMacro _                  -> DeclarationMacro ""
  | DeclarationMacroInvocation _        -> DeclarationMacroInvocation ""
  | GotoStatement _                     -> GotoStatement ""
  | StatementMacroInvocation _          -> StatementMacroInvocation ""
  | StatementMacro _                    -> StatementMacro ""
  | IterationMacroInvocation _          -> IterationMacroInvocation ""
  | IterationMacro _                    -> IterationMacro ""
  | UnaryExpressionSizeofPack _         -> UnaryExpressionSizeofPack ""
  | ExpressionMacroInvocation _         -> ExpressionMacroInvocation ""
  | DefinedMacroExpression _            -> DefinedMacroExpression ""
  | HasIncludeExpression _              -> HasIncludeExpression ""
  | LiteralOperatorId _                 -> LiteralOperatorId ""
  | IntegerLiteral _                    -> IntegerLiteral ""
  | CharacterLiteral _                  -> CharacterLiteral ""
  | FloatingLiteral _                   -> FloatingLiteral ""
  | StringLiteral _                     -> StringLiteral ""
  | StringMacro _                       -> StringMacro ""
  | LiteralMacroInvocation _            -> LiteralMacroInvocation ""
  | BooleanLiteral _                    -> BooleanLiteral ""
  | UserDefinedCharacterLiteral _       -> UserDefinedCharacterLiteral ""
  | UserDefinedStringLiteral _          -> UserDefinedStringLiteral ""
  | UserDefinedFloatingLiteral _        -> UserDefinedFloatingLiteral ""
  | UserDefinedIntegerLiteral _         -> UserDefinedIntegerLiteral ""
  | SimpleTypeSpecifier _               -> SimpleTypeSpecifier ""
  | ElaboratedTypeSpecifierClass _      -> ElaboratedTypeSpecifierClass ""
  | ElaboratedTypeSpecifierStruct _     -> ElaboratedTypeSpecifierStruct ""
  | ElaboratedTypeSpecifierUnion _      -> ElaboratedTypeSpecifierUnion ""
  | ElaboratedTypeSpecifierEnum _       -> ElaboratedTypeSpecifierEnum ""
  | TypenameSpecifier _                 -> TypenameSpecifier ""
  | ContractAttributeSpecifierEnsures _ -> ContractAttributeSpecifierEnsures ""
  | AlignmentAttributeSpecifier _       -> AlignmentAttributeSpecifier false
  | VirtSpecifierMacro _                -> VirtSpecifierMacro ""
  | SimpleCapture _                     -> SimpleCapture ""
  | SimpleCaptureAmp _                  -> SimpleCaptureAmp ""
  | InitCapture _                       -> InitCapture ""
  | InitCaptureAmp _                    -> InitCaptureAmp ""
  | MemberDeclaratorBitField _          -> MemberDeclaratorBitField ""
  | Label _                             -> Label ""
  | ClassName _                         -> ClassName ""
  | ClassHeadName _                     -> ClassHeadName ""
  | ConceptDefinition _                 -> ConceptDefinition ""
  | TypeParameter _                     -> TypeParameter ""
  | EnclosingNamespaceSpecifier _       -> EnclosingNamespaceSpecifier ""
  | Enumerator _                        -> Enumerator ""
  | TypeMacroInvocation _               -> TypeMacroInvocation ""
  | DesignatorField _                   -> DesignatorField ""
  | DesignatorFieldOld _                -> DesignatorFieldOld ""
  | EnumHeadName _                      -> EnumHeadName ""
  | QualifiedNamespaceSpecifier _       -> QualifiedNamespaceSpecifier ""
  | TypeName _                          -> TypeName ""
  | TypeConstraint _                    -> TypeConstraint ""
  | SimpleTemplateId _                  -> SimpleTemplateId ""
  | Identifier _                        -> Identifier ""
  | AttributeToken _                    -> AttributeToken ""
  | AttributeScopedToken _              -> AttributeScopedToken ""
  | AttributeNamespace _                -> AttributeNamespace ""
  | BalancedTokenSingle _               -> BalancedTokenSingle ""(*(Obj.repr ())*)
  | NestedNameSpecifierIdent _          -> NestedNameSpecifierIdent ""
  | NestedNameSpecifierTempl _          -> NestedNameSpecifierTempl ""
  | PostfixExpressionExplicitTypeConvExpr   -> PostfixExpressionExplicitTypeConv
  | PostfixExpressionExplicitTypeConvBraced -> PostfixExpressionExplicitTypeConv
  | PostfixExpressionTypeidExpr             -> PostfixExpressionTypeid
  | PostfixExpressionTypeidTy               -> PostfixExpressionTypeid
  | AccessSpecAnnot _                   -> AccessSpecAnnot ""
  | Restrict _                          -> Restrict ""
  | AssignmentExpressionOverloaded _    -> AssignmentExpressionOverloaded ""
  | MsAsmBlock _                        -> MsAsmBlock("", "")
  | MsCdecl _                           -> MsCdecl ""
  | MsStdcall _                         -> MsStdcall ""
  | GnuAsmBlock _                       -> GnuAsmBlock("", "")
  | GnuAsmBlockFragmented _             -> GnuAsmBlockFragmented ""
  | GnuAsmFragment _                    -> GnuAsmFragment ""
  | CallingConvention _                 -> CallingConvention ""
  | DeclSpecifierMacro _                -> DeclSpecifierMacro ""
  | ParametersMacroInvocation _         -> ParametersMacroInvocation ""
  | EnumeratorMacroInvocation _         -> EnumeratorMacroInvocation ""
  | AccessSpecMacro _                   -> AccessSpecMacro ""
  | CvMacro _                           -> CvMacro ""
  | AttributeMacro _                    -> AttributeMacro ""
  | IdentifierMacroInvocation _         -> IdentifierMacroInvocation ""
  | GnuAttribute _                      -> GnuAttribute ""
  | DoxygenLine _                      -> DoxygenLine ""
  | NoexceptSpecifierMacro _           -> NoexceptSpecifierMacro ""
  | AttributeMacroInvocation _         -> AttributeMacroInvocation ""
  | NamespaceHead _                    -> NamespaceHead ""
  | TemplParamMacroInvocation _        -> TemplParamMacroInvocation ""
  | PostfixExpressionFunCallGuarded _  -> PostfixExpressionFunCallGuarded ""
  | PostfixExpressionFunCallMacro _          -> PostfixExpressionFunCallMacro ""
  | LogicalOrMacroInvocation _               -> LogicalOrMacroInvocation ""
  | TypeMacro _                              -> TypeMacro ""
  | MsPragma _                               -> MsPragma ""
  | MsWarningSpecifier _                     -> MsWarningSpecifier ""
  | MsProperty _                             -> MsProperty ""
  | PtrOperatorMacro _                       -> PtrOperatorMacro ""
  | ClassHeadMacroInvocation _               -> ClassHeadMacroInvocation ""
  | FunctionBodyMacro _                      -> FunctionBodyMacro ""
  | DeclSpecifierMacroInvocation _           -> DeclSpecifierMacroInvocation ""
  | LabelMacroInvocation _                   -> LabelMacroInvocation ""
  | BaseMacro _                              -> BaseMacro ""
  | SuffixMacro _                            -> SuffixMacro ""
  | ArgumentsMacroInvocation _               -> ArgumentsMacroInvocation ""
  | MemInitMacroInvocation _                 -> MemInitMacroInvocation ""
  | EnumeratorDefinitionMacro _              -> EnumeratorDefinitionMacro ""
  | ParamDeclMacro _                         -> ParamDeclMacro ""
  | ParamDeclMacroInvocation _               -> ParamDeclMacroInvocation ""
  | AttributeArgumentClauseMacro _           -> AttributeArgumentClauseMacro ""
  | OperatorMacroInvocation _                -> OperatorMacroInvocation ""
  | CvMacroInvocation _                      -> CvMacroInvocation ""
  | NamedNamespaceDefinitionHead _           -> NamedNamespaceDefinitionHead ""
  | BlockHeadMacro _                         -> BlockHeadMacro ""
  | BlockEndMacro _                          -> BlockEndMacro ""
  | AsmShader _                              -> AsmShader ""
  | AsmName _                                -> AsmName ""
  | AsmDirective _                           -> AsmDirective ""
  | VaArgs _                                 -> VaArgs ""
  | PtrMacro _                               -> PtrMacro ""
  | ObjcProtocolDeclaration _                -> ObjcProtocolDeclaration ""
  | ObjcClassInterface _                     -> ObjcClassInterface ""
  | ObjcCategoryInterface _                  -> ObjcCategoryInterface("", "")
  | ObjcSuperclass _                         -> ObjcSuperclass ""
  | ObjcSelector _                           -> ObjcSelector ""
  | ObjcKeywordDeclarator _                  -> ObjcKeywordDeclarator ""
  | ObjcSpecifierQualifier _                 -> ObjcSpecifierQualifier ""
  | ObjcProtocolName _                       -> ObjcProtocolName ""
  | ObjcClassName _                          -> ObjcClassName ""
  | ObjcPropertyAttribute _                  -> ObjcPropertyAttribute ""
  | ObjcKeywordArgument _                    -> ObjcKeywordArgument ""
  | ObjcSelectorExpression _                 -> ObjcSelectorExpression ""
  | ObjcMethodMacroInvocation _              -> ObjcMethodMacroInvocation ""

  | SuffixMacroInvocation _ -> SuffixMacroInvocation ""

  | SwiftArg _ -> SwiftArg ""

  | HugeArray _ -> HugeArray(0, "")

  | lab -> lab

let anonymize = _anonymize ~most:false
let anonymize2 = _anonymize ~more:true ~most:false
let anonymize3 = _anonymize ~more:true ~most:true

(*
type annotation = string option

let null_annotation = None

let annotation_to_string = function
  | None -> "<none>"
  | Some x -> x
*)

module Annotation = struct
  type spec =
    | Require of string list
    | Provide of string list
    | Type of string

  type t = spec list

  let null = ([] : t)

  let mkrequire ns = Require ns
  let mkprovide ns = Provide ns
  let mktype s = Type s

  let spec_to_string = function
    | Require ns -> Printf.sprintf "require %s" (Xlist.to_string (fun x -> x) ", " ns)
    | Provide ns -> Printf.sprintf "provide %s" (Xlist.to_string (fun x -> x) ", " ns)
    | Type s -> Printf.sprintf "type %s" s

  let to_string l = Xlist.to_string spec_to_string "\n" l

  let from_specs (l : spec list) = (l : t)

  let add_spec (a : t) (s : spec) = (s :: a : t)

  let iter f (a : t) =
    List.iter f a

end (* of module Annotation *)

type annotation = Annotation.t
let null_annotation = Annotation.null
let annotation_to_string = Annotation.to_string


let is_hunk_boundary _ _ = false (* not yet *)

(* These labels are collapsible whether they are leaves or not. *)
let forced_to_be_collapsible lab = 
  false


let is_collapse_target options lab = 
  if options#no_collapse_flag then 
    false
  else 
    match lab with
    | SimpleDeclaration
    | AsmDefinition _
    | NamespaceAliasDefinition _
    | UsingDeclaration
    | UsingDirective _
    | Static_assertDeclaration
    | AliasDeclaration _
    | OpaqueEnumDeclaration
    | OpaqueEnumDeclarationClass
    | OpaqueEnumDeclarationStruct
    | NodeclspecFunctionDeclaration
    | FunctionDefinition
    | TemplateDeclaration
    | DeductionGuide _
    | ExplicitInstantiation
    | ExplicitSpecialization
    | LinkageSpecification _
    | NamedNamespaceDefinition _
    | UnnamedNamespaceDefinition
    | NestedNamespaceDefinition _
    | NamespaceDefinitionMacro _
    | AttributeDeclaration

    | MemberDeclarationDecl

    | ExpressionStatement
    | DeclarationStatement
    | TryBlock
    | LabeledStatement
    | SelectionStatement
    | IfStatement
    | ElseStatement
    | SwitchStatement
    | CompoundStatement
    | IterationStatement
    | WhileStatement
    | DoStatement
    | ForStatement
    | RangeBasedForStatement
    | ReturnStatement

    | ParenthesizedExpression
    | RequiresExpression
    | FoldExpression
    | LambdaExpression
    | LogicalOrExpression _
    | LogicalAndExpression _
    | InclusiveOrExpression _
    | ExclusiveOrExpression _
    | AndExpression _
    | EqualityExpression
    | EqualityExpressionEq
    | EqualityExpressionNeq _
    | RelationalExpression
    | RelationalExpressionLt
    | RelationalExpressionGt
    | RelationalExpressionLe
    | RelationalExpressionGe
    | CompareExpression
    | ShiftExpression
    | ShiftExpressionLeft
    | ShiftExpressionRight
    | AdditiveExpression
    | AdditiveExpressionAdd
    | AdditiveExpressionSubt
    | MultiplicativeExpression
    | MultiplicativeExpressionMult
    | MultiplicativeExpressionDiv
    | MultiplicativeExpressionMod
    | PmExpression
    | PmExpressionClass
    | PmExpressionPtr
    | CastExpression
    | CompoundLiteralExpression
    | UnaryExpression
    | UnaryExpressionIncr
    | UnaryExpressionDecr
    | UnaryExpressionInd
    | UnaryExpressionAddr
    | UnaryExpressionLabelAddr
    | UnaryExpressionPlus
    | UnaryExpressionMinus
    | UnaryExpressionNeg _
    | UnaryExpressionCompl _
    | UnaryExpressionSizeof
    | UnaryExpressionSizeofPack _
    | UnaryExpressionAlignof
    | BlockLiteralExpression
    | GnuStatementExpression
    | NoexceptExpression
    | PostfixExpression
    | PostfixExpressionSubscr
    | PostfixExpressionFunCall
    | PostfixExpressionFunCallGuarded _
    | PostfixExpressionExplicitTypeConv
    | PostfixExpressionExplicitTypeConvExpr
    | PostfixExpressionExplicitTypeConvBraced
    | PostfixExpressionDot
    | PostfixExpressionArrow
    | PostfixExpressionIncr
    | PostfixExpressionDecr
    | PostfixExpressionTypeid
    | PostfixExpressionDynamic_cast
    | PostfixExpressionStatic_cast
    | PostfixExpressionReinterpret_cast
    | PostfixExpressionConst_cast
    | AssignmentExpression
    | AssignmentExpressionPlus
    | AssignmentExpressionMinus
    | AssignmentExpressionMult
    | AssignmentExpressionDiv
    | AssignmentExpressionMod
    | AssignmentExpressionShiftLeft
    | AssignmentExpressionShiftRight
    | AssignmentExpressionAnd _
    | AssignmentExpressionXor _
    | AssignmentExpressionOr _
    | ThrowExpression
    | ExpressionPair
    | ConditionalExpression
    | NewExpression
    | DeleteExpression
    | DeleteExpressionBracket

    | QualifiedId

      -> true

    | _ -> false      


let is_to_be_notified = function
  | TemplateDeclaration
  | ClassSpecifier
  | EnumSpecifier
  | FunctionDefinition
    -> true
  | _ -> false

let is_partition = function
  | TranslationUnit
    -> true
  | _ -> false

let is_sequence = function
  | DECLS
  | MEM_DECLS
  | STMTS
  | LABELS
  | TranslationUnit
    -> true
  | _ -> false


let is_boundary = function
  | TranslationUnit
  | TemplateDeclaration
  | SimpleDeclaration
  | FunctionDefinition
    -> true
  | _ -> false

let is_literal = function
  | IntegerLiteral _
  | CharacterLiteral _
  | FloatingLiteral _
  | StringLiteral _
  | StringMacro _
  | BooleanLiteral _
  | Nullptr
  | ConcatenatedString
  | UserDefinedCharacterLiteral _
  | UserDefinedStringLiteral _
  | UserDefinedFloatingLiteral _
  | UserDefinedIntegerLiteral _
      -> true
  | _ -> false

let is_primary = function
  | IntegerLiteral _
  | CharacterLiteral _
  | FloatingLiteral _
  | StringLiteral _
  | StringMacro _
  | BooleanLiteral _
  | Nullptr
  | ConcatenatedString
  | UserDefinedCharacterLiteral _
  | UserDefinedStringLiteral _
  | UserDefinedFloatingLiteral _
  | UserDefinedIntegerLiteral _
  | This
  | ParenthesizedExpression
  | UnqualifiedId
  | OperatorFunctionId
  | ConversionFunctionId
  | LiteralOperatorId _
  | Destructor
  | TemplateId
  | QualifiedId
  | LambdaExpression
  | FoldExpression
  | RequiresExpression
    -> true
  | _ -> false

let is_expr = function
  | LogicalOrExpression _
  | LogicalAndExpression _
  | InclusiveOrExpression _
  | ExclusiveOrExpression _
  | AndExpression _
  | EqualityExpressionEq
  | EqualityExpressionNeq _
  | RelationalExpressionLt
  | RelationalExpressionGt
  | RelationalExpressionLe
  | RelationalExpressionGe
  | CompareExpression
  | ShiftExpression
  | ShiftExpressionLeft
  | ShiftExpressionRight
  | AdditiveExpression
  | AdditiveExpressionAdd
  | AdditiveExpressionSubt
  | MultiplicativeExpressionMult
  | MultiplicativeExpressionDiv
  | MultiplicativeExpressionMod
  | PmExpressionClass
  | PmExpressionPtr
  | CastExpression
  | UnaryExpressionIncr
  | UnaryExpressionDecr
  | UnaryExpressionInd
  | UnaryExpressionAddr
  | UnaryExpressionLabelAddr
  | UnaryExpressionPlus
  | UnaryExpressionMinus
  | UnaryExpressionNeg _
  | UnaryExpressionCompl _
  | UnaryExpressionSizeof
  | UnaryExpressionSizeofPack _
  | UnaryExpressionAlignof
  | NoexceptExpression
  | PostfixExpressionSubscr
  | PostfixExpressionFunCall
  | PostfixExpressionExplicitTypeConv
  | PostfixExpressionDot
  | PostfixExpressionArrow
  | PostfixExpressionIncr
  | PostfixExpressionDecr
  | PostfixExpressionTypeid
  | PostfixExpressionDynamic_cast
  | PostfixExpressionStatic_cast
  | PostfixExpressionReinterpret_cast
  | PostfixExpressionConst_cast
  | AssignmentExpression
  | AssignmentExpressionOverloaded _
  | AssignmentExpressionEq
  | AssignmentExpressionPlus
  | AssignmentExpressionMinus
  | AssignmentExpressionMult
  | AssignmentExpressionDiv
  | AssignmentExpressionMod
  | AssignmentExpressionShiftLeft
  | AssignmentExpressionShiftRight
  | AssignmentExpressionAnd _
  | AssignmentExpressionXor _
  | AssignmentExpressionOr _
  | ThrowExpression
  | ExpressionPair
  | ConditionalExpression
  | NewExpression
  | DeleteExpression
  | DeleteExpressionBracket
  | ExpressionMacroInvocation _
  | DefinedMacroExpression _
  | CudaKernelCall
  | SwiftFunCall
    -> true
  | lab -> is_primary lab

let is_pp_branch = function
  | PpIf
  | PpIfdef _
  | PpIfndef _
  | PpElif
  | PpElse
    -> true
  | _ -> false

let is_stmt = function
  | ExpressionStatement
  | DeclarationStatement
  (*| EmptyStatement*)
  | CompoundStatement
  | TryBlock
  | LabeledStatement
  | SelectionStatement
  | IfStatement
  | ElseStatement
  | SwitchStatement
  | IterationStatement
  | WhileStatement
  | DoStatement
  | ForStatement
  | RangeBasedForStatement
  | JumpStatement
  | BreakStatement
  | ContinueStatement
  | ReturnStatement
  | GotoStatement _
  | StatementMacroInvocation _
  | StatementMacro _
  | IterationMacroInvocation _
  | IterationMacro _
    -> true
  | _ -> false


let is_compatible _ _ = false

let is_order_insensitive = function
  | _ -> false

let quasi_eq _ _ = false

let relabel_allowed = function
  | Identifier _, Identifier _ -> true
  | Identifier _, l | l, Identifier _ -> is_literal l
  | l1, l2 -> 
      (is_expr l1 && is_expr l2) ||
      (*(is_stmt l1 && is_stmt l2) ||*)
      (is_pp_branch l1 && is_pp_branch l2) ||
      (anonymize3 l1 = anonymize3 l2)

let move_disallowed _ = false

let is_common _ = false

let get_ident_use = function
  | _ -> ""

let to_char lab = '0'

let has_names lab =
  try
    ignore (get_names lab);
    true
  with 
    Not_found -> false

let has_a_name lab =
  try
    ignore (get_name lab);
    true
  with 
    Not_found -> false

let is_named lab =
  has_a_name lab || has_names lab

let is_named_orig lab =
  match lab with
  | _ -> is_named lab


let to_elem_data = Astml.to_elem_data lang_prefix to_tag



let getlab nd = (Obj.obj nd#data#_label : t)



let cannot_be_keyroot nd = 
  match getlab nd with
  | TranslationUnit
  | TemplateDeclaration
  | FunctionDefinition
    -> true
  | _ -> false


let is_literal = function
  | IntegerLiteral _ | UserDefinedIntegerLiteral _
  | FloatingLiteral _ | UserDefinedFloatingLiteral _
  | CharacterLiteral _
  | StringLiteral _ | UserDefinedStringLiteral _
  | StringMacro _
  | BooleanLiteral _
  | ConcatenatedString
    -> true
  | _ -> false

let is_int_literal = function
  | IntegerLiteral _ | UserDefinedIntegerLiteral _ -> true
  | _ -> false

let is_real_literal = function
  | FloatingLiteral _ | UserDefinedFloatingLiteral _ -> true
  | _ -> false

let is_string_literal = function
  | StringLiteral _ | UserDefinedStringLiteral _ -> true
  | ConcatenatedString -> true
  | _ -> false

let is_phantom = function

  | _ -> false

let is_special _ = false

let is_pp_directive = function
  | PpInclude _
  | PpDefine _
  | PpUndef _
  | PpLine _
  | PpError _
  | PpPragma _
  | PpNull
  | PpIf
  | PpIfdef _
  | PpIfndef _
  | PpElif
  | PpElse
  | PpEndif
  | PpUnknown _
    -> true
  | _ -> false

let is_pp_define = function
  | PpDefine _ -> true
  | _ -> false

let is_pp_include = function
  | PpInclude _ -> true
  | _ -> false

let get_pp_include_path = function
  | PpInclude h -> h
  | _ -> raise Not_found

let is_container_unit = function
  | PpIfSection _
  | PpIfSectionFuncDef _
  | PpIfSectionCondExpr
  | PpIfSectionHandler
  | PpIfSectionTemplDecl
  | PpIfSectionTryBlock
  | PpIfSectionBroken
  | PpIfSectionBrokenIf
  | PpIfSectionBrokenFuncDef
  | PpIfSectionBrokenDtorFunc
  | PpIfGroup
  | PpElifGroup
  | PpElseGroup
  | CompoundStatement
  | TryBlock
  | IfStatement
  | ElseStatement
  | SwitchStatement
  | WhileStatement
  | DoStatement
  | ForStatement
  | RangeBasedForStatement
    -> true

  | _ -> false

let is_pp_if_section = function
  | PpIfSection _
  | PpIfSectionFuncDef _
  | PpIfSectionCondExpr
  | PpIfSectionHandler
  | PpIfSectionTemplDecl
  | PpIfSectionTryBlock
  | PpIfSectionBroken
  | PpIfSectionBrokenIf
  | PpIfSectionBrokenFuncDef
  | PpIfSectionBrokenDtorFunc
    -> true
  | _ -> false

let is_pp_if_section_broken = function
  | PpIfSectionBroken
  | PpIfSectionBrokenIf
  | PpIfSectionBrokenFuncDef
  | PpIfSectionBrokenDtorFunc
    -> true
  | _ -> false

let is_pp_group = function
  | PpIfGroup
  | PpElifGroup
  | PpElseGroup
    -> true
  | _ -> false

let is_pp_if_group = function
  | PpIfGroup -> true
  | _ -> false

let is_pp_elif_group = function
  | PpElifGroup -> true
  | _ -> false

let is_pp_else_group = function
  | PpElseGroup -> true
  | _ -> false

let is_translation_unit = function
  | TranslationUnit -> true
  | _ -> false

let is_decl = function
  | DummyDecl
  | SimpleDeclaration
  | AsmDefinition _
  | NamespaceAliasDefinition _
  | UsingDeclaration
  | UsingDirective _
  | Static_assertDeclaration
  | AliasDeclaration _
  | OpaqueEnumDeclaration
  | OpaqueEnumDeclarationClass
  | OpaqueEnumDeclarationStruct
  | NodeclspecFunctionDeclaration
  | FunctionDefinition
  | TemplateDeclaration
  | DeductionGuide _
  | ExplicitInstantiation
  | ExplicitSpecialization
  | LinkageSpecification _
  | NamedNamespaceDefinition _
  | UnnamedNamespaceDefinition
  | NestedNamespaceDefinition _
  | NamespaceDefinitionMacro _
  | EmptyDeclaration
  | AttributeDeclaration
  | DeclarationMacro _
  | DeclarationMacroInvocation _
        -> true
  | _ -> false

let is_simple_decl = function
  | SimpleDeclaration -> true
  | _ -> false

let is_func = function
  | FunctionDefinition -> true
  | _ -> false

let is_jump_stmt = function
  | JumpStatement
  | BreakStatement
  | ContinueStatement
  | ReturnStatement
  | GotoStatement _
    -> true
  | _ -> false

let is_sel_stmt = function
  | SelectionStatement
  | IfStatement
  | ElseStatement
  | SwitchStatement
    -> true
  | _ -> false

let is_iter_stmt = function
  | IterationStatement
  | WhileStatement
  | DoStatement
  | ForStatement
  | RangeBasedForStatement
  | IterationMacroInvocation _
  | IterationMacro _
    -> true
  | _ -> false

let is_decl_spec = function
  | StorageClassSpecifierStatic
  | StorageClassSpecifierThread_local
  | StorageClassSpecifierExtern
  | StorageClassSpecifierMutable
  | StorageClassSpecifierRegister
  | StorageClassSpecifierVaxGlobaldef

  | SimpleTypeSpecifier _
  | DecltypeSpecifier
  | PlaceholderTypeSpecifierAuto
  | PlaceholderTypeSpecifierDecltype
  | Char | Char8_t | Char16_t | Char32_t | Wchar_t | Bool | Short | Int | Long | Signed | Unsigned | Float | Double
  | Void
  | UnsignedInt | UnsignedLong
  | ElaboratedTypeSpecifier
  | TypenameSpecifier _

  | Const
  | Volatile
  | Restrict _
  | CvMacro _
  | MsStdcall _
  | MsCdecl _
  | TypeMacroInvocation _

  | FunctionSpecifierVirtual
  | ExplicitSpecifier

  | ClassSpecifier
  | EnumSpecifier

  | DeclSpecifierFriend
  | DeclSpecifierTypedef
  | DeclSpecifierConstexpr
  | DeclSpecifierConsteval
  | DeclSpecifierConstinit
  | DeclSpecifierInline
  | DeclSpecifierMacro _
    -> true
  | _ -> false

let is_decl_stmt = function
  | DeclarationStatement -> true
  | _ -> false

let is_comp_stmt = function
  | CompoundStatement -> true
  | _ -> false

let is_try_block = function
  | TryBlock -> true
  | _ -> false

let is_if = function
  | IfStatement -> true
  | _ -> false

let is_switch = function
  | SwitchStatement -> true
  | _ -> false

let is_while = function
  | WhileStatement -> true
  | _ -> false

let is_do = function
  | DoStatement -> true
  | _ -> false

let is_for = function
  | ForStatement -> true
  | _ -> false

let is_ranged_for = function
  | RangeBasedForStatement -> true
  | _ -> false

let is_return = function
  | ReturnStatement -> true
  | _ -> false

let is_goto = function
  | GotoStatement _ -> true
  | _ -> false

let is_static = function
  | StorageClassSpecifierStatic -> true
  | _ -> false

let is_typedef = function
  | DeclSpecifierTypedef -> true
  | _ -> false

let is_extern = function
  | StorageClassSpecifierExtern -> true
  | _ -> false

let is_expr_list = function
  | ExpressionList -> true
  | _ -> false

let is_ident = function
  | Identifier _ -> true
  | _ -> false

let is_unary_expr = function
  | UnaryExpression
  | UnaryExpressionIncr
  | UnaryExpressionDecr
  | UnaryExpressionInd
  | UnaryExpressionAddr
  | UnaryExpressionLabelAddr
  | UnaryExpressionPlus
  | UnaryExpressionMinus
  | UnaryExpressionNeg _
  | UnaryExpressionCompl _
  | UnaryExpressionSizeof
  | UnaryExpressionSizeofPack _
  | UnaryExpressionAlignof -> true
  | _ -> false

let is_funcall = function
  | PostfixExpressionFunCall -> true
  | _ -> false

let is_array_acc = function
  | PostfixExpressionSubscr -> true
  | _ -> false

let is_memb_acc = function
  | PostfixExpressionDot -> true
  | _ -> false

let is_memb_ptr_acc = function
  | PostfixExpressionArrow -> true
  | _ -> false

let is_param_decl_clause = function
  | ParameterDeclarationClause _ -> true
  | _ -> false

let is_param_decl = function
  | ParameterDeclaration -> true
  | _ -> false

let is_desig = function
  | DesignatorField _
  | DesignatorFieldOld _
  | DesignatorIndex
      -> true
  | _ -> false

let is_dtor = function
  | DeclaratorFunc
  | NoptrDeclaratorId
  | NoptrDeclaratorParen
  | NoptrDeclaratorFunc
  | NoptrDeclaratorArray
    -> true
  | _ -> false

let is_abst_dtor = function
  | AbstractDeclaratorFunc
  | PtrAbstractDeclarator
  | NoptrAbstractDeclaratorParen
  | NoptrAbstractDeclaratorFunc
  | NoptrAbstractDeclaratorArray
      -> true
  | _ -> false

let is_initializer = function
  | EqualInitializer
  | BracedInitList
  | ParenthesizedInitList
    -> true
  | _ -> false

let is_body = function
  | DummyBody
  | FunctionBody
  | FunctionBodyDefault
  | FunctionBodyDelete
  | FunctionTryBlock
    -> true
  | _ -> false

let is_class_spec = function
  | ClassSpecifier -> true
  | _ -> false

let is_enum_spec = function
  | EnumSpecifier -> true
  | _ -> false

let is_class = function
  | ClassHeadClass -> true
  | _ -> false

let is_struct = function
  | ClassHeadStruct -> true
  | _ -> false

let is_union = function
  | ClassHeadUnion -> true
  | _ -> false

let is_namespace_head = function
  | NamespaceHead _ -> true
  | _ -> false

let is_expr_macro_ivk = function
  | ExpressionMacroInvocation _ -> true
  | _ -> false

let is_stmt_macro_ivk = function
  | StatementMacroInvocation _ -> true
  | _ -> false

let is_type_macro_ivk = function
  | TypeMacroInvocation _ -> true
  | _ -> false

let is_type_id = function
  | TypeId -> true
  | _ -> false

let is_capture = function
  | SimpleCapture _
  | SimpleCaptureAmp _
  | SimpleCaptureThis
  | PackExpansion
  | _ -> false

let is_braced_init_list = function
  | BracedInitList -> true
  | _ -> false

let is_args_macro = function
  | ArgumentsMacro _ -> true
  | _ -> false

let is_args_macro_ivk = function
  | ArgumentsMacroInvocation _ -> true
  | _ -> false

let is_desig_old = function
  | DesignatorFieldOld _ -> true
  | _ -> false


open Astml.Attr

let of_elem_data =

  let find_asm attrs       = find_attr attrs "asm" in
  let find_block attrs     = find_attr attrs "block" in
  let find_category attrs  = find_attr attrs "category" in
  let find_code attrs      = find_attr attrs "code" in
  let find_line attrs      = find_attr attrs "line" in
  let find_linkage attrs   = find_attr attrs "linkage" in
  let find_file_name attrs = find_attr attrs "file_name" in
  let find_include attrs   = find_attr attrs "include" in
  let find_operator attrs  = find_attr attrs "operator" in

  let comma_pat = Str.regexp_string "," in
  let find_macro_kind attrs =
    match attrs with
    | [] -> ObjectLike
    | _ -> begin
        let params = Str.split comma_pat (find_attr attrs "params") in
        let va_args = find_attr attrs "va_args" in
        FunctionLike(params, va_args)
    end
  in

  let find_pack attrs  = bool_of_string (find_attr ~default:"false" attrs "pack") in
  let find_is_va attrs = bool_of_string (find_attr ~default:"false" attrs "is_va") in

  let find_line_number attrs = int_of_string (find_attr ~default:"0" attrs "line_number") in
  let find_level attrs       = int_of_string (find_attr ~default:"0" attrs "level") in
  let find_size attrs        = int_of_string (find_attr ~default:"0" attrs "size") in

  let colon_pat = Str.regexp_string ":" in
  let find_flags attrs = List.map int_of_string (Str.split colon_pat (find_attr attrs "flags")) in


  let tag_list = [
    "DUMMY",               (fun a -> DUMMY);
    "EMPTY",               (fun a -> EMPTY);
    "AMBIGUOUS_CONSTRUCT", (fun a -> AMBIGUOUS_CONSTRUCT);
    "PARTIAL_CONSTRUCT",   (fun a -> PARTIAL_CONSTRUCT);
    "DECLS",               (fun a -> DECLS);
    "MEM_DECLS",           (fun a -> MEM_DECLS);
    "STMTS",               (fun a -> STMTS);
    "INITS",               (fun a -> INITS);
    "LABELS",              (fun a -> LABELS);
    "SPECS",               (fun a -> SPECS);
    "ETORS",               (fun a -> ETORS);
    "TEMPL_PARAMS",        (fun a -> TEMPL_PARAMS);
    "TEMPL_ARGS",          (fun a -> TEMPL_ARGS);
    "DELIM_MACRO",         (fun a -> DELIM_MACRO(find_ident a));
    "Q_PROPERTY",          (fun a -> Q_PROPERTY);

    "TranslationUnit", (fun a -> TranslationUnit);

(* PpDirective *)
    "PpInclude",                 (fun a -> PpInclude(find_line a));
    "PpDefine",                  (fun a -> PpDefine(find_ident a));
    "PpUndef",                   (fun a -> PpUndef(find_ident a));
    "PpLine",                    (fun a -> PpLine(find_line_number a, find_file_name a));
    "PpError",                   (fun a -> PpError(find_line a));
    "PpPragma",                  (fun a -> PpPragma(find_line a));
    "PpNull",                    (fun a -> PpNull);
    "PpMarker",                  (fun a -> PpMarker(find_line_number a, find_file_name a, find_flags a));
    "PpIf",                      (fun a -> PpIf);
    "PpIfdef",                   (fun a -> PpIfdef(find_ident a));
    "PpIfndef",                  (fun a -> PpIfndef(find_ident a));
    "PpElif",                    (fun a -> PpElif);
    "PpElse",                    (fun a -> PpElse);
    "PpEndif",                   (fun a -> PpEndif);
    "PpUnknown",                 (fun a -> PpUnknown(find_line a));
    "PpImport",                  (fun a -> PpImport(find_line a));
    "OmpDirective",              (fun a -> OmpDirective(find_line a));
    "AccDirective",              (fun a -> AccDirective(find_line a));
    "PpMacroParam",              (fun a -> PpMacroParam(find_ident a));
    "PpStringized",              (fun a -> PpStringized(find_ident a));
    "PpConcatenatedIdentifier",  (fun a -> PpConcatenatedIdentifier);
    "PpIfSection",               (fun a -> PpIfSection(find_level a));
    "PpIfSectionFuncDef",        (fun a -> PpIfSectionFuncDef(find_ident a));
    "PpIfSectionAltFuncDef",     (fun a -> PpIfSectionAltFuncDef);
    "PpIfSectionBroken",         (fun a -> PpIfSectionBroken);
    "PpIfSectionBrokenIf",       (fun a -> PpIfSectionBrokenIf);
    "PpIfSectionBrokenFuncDef",  (fun a -> PpIfSectionBrokenFuncDef);
    "PpIfSectionBrokenDtorFunc", (fun a -> PpIfSectionBrokenDtorFunc);
    "PpIfSectionCondExpr",       (fun a -> PpIfSectionCondExpr);
    "PpIfSectionLogicalAnd",     (fun a -> PpIfSectionLogicalAnd);
    "PpIfSectionLogicalOr",      (fun a -> PpIfSectionLogicalOr);
    "PpIfSectionTemplDecl",      (fun a -> PpIfSectionTemplDecl);
    "PpIfSectionTryBlock",       (fun a -> PpIfSectionTryBlock);
    "PpIfGroup",                 (fun a -> PpIfGroup);
    "PpElifGroup",               (fun a -> PpElifGroup);
    "PpElseGroup",               (fun a -> PpElseGroup);

(* Declaration *)
    "SimpleDeclaration",                    (fun a -> SimpleDeclaration);
    "AsmDefinition",                        (fun a -> AsmDefinition(find_asm a));
    "NamespaceAliasDefinition",             (fun a -> NamespaceAliasDefinition(find_ident a));
    "UsingDeclaration",                     (fun a -> UsingDeclaration);
    "UsingDirective",                       (fun a -> UsingDirective(find_ident a));
    "Static_assertDeclaration",             (fun a -> Static_assertDeclaration);
    "AliasDeclaration",                     (fun a -> AliasDeclaration(find_ident a));
    "OpaqueEnumDeclaration",                (fun a -> OpaqueEnumDeclaration);
    "OpaqueEnumDeclarationClass",           (fun a -> OpaqueEnumDeclarationClass);
    "OpaqueEnumDeclarationStruct",          (fun a -> OpaqueEnumDeclarationStruct);
    "OpaqueEnumDeclarationMacro",           (fun a -> OpaqueEnumDeclarationMacro(find_ident a));
    "NodeclspecFunctionDeclaration",        (fun a -> NodeclspecFunctionDeclaration);
    "FunctionDefinition",                   (fun a -> FunctionDefinition);
    "TemplateDeclaration",                  (fun a -> TemplateDeclaration);
    "DeductionGuide",                       (fun a -> DeductionGuide(find_name a));
    "ExplicitInstantiation",                (fun a -> ExplicitInstantiation);
    "ExplicitSpecialization",               (fun a -> ExplicitSpecialization);
    "LinkageSpecification",                 (fun a -> LinkageSpecification(find_linkage a));
    "NamedNamespaceDefinition",             (fun a -> NamedNamespaceDefinition(find_ident a));
    "UnnamedNamespaceDefinition",           (fun a -> UnnamedNamespaceDefinition);
    "NestedNamespaceDefinition",            (fun a -> NestedNamespaceDefinition(find_ident a));
    "NamespaceDefinitionMacro",             (fun a -> NamespaceDefinitionMacro(find_ident a));
    "EmptyDeclaration",                     (fun a -> EmptyDeclaration);
    "AttributeDeclaration",                 (fun a -> AttributeDeclaration);
    "DeclarationMacro",                     (fun a -> DeclarationMacro(find_ident a));
    "DeclarationMacroInvocation",           (fun a -> DeclarationMacroInvocation(find_ident a));
    "DeclarationMacroInvocationInvocation", (fun a -> DeclarationMacroInvocationInvocation);
    "DeclarationMacroInvocationArrow",      (fun a -> DeclarationMacroInvocationArrow);
    "DeclarationMacroInvocationDot",        (fun a -> DeclarationMacroInvocationDot);
    "ImportDeclaration",                    (fun a -> ImportDeclaration(find_line a));

(* Statement *)
    "Statement",                (fun a -> Statement);
    "ExpressionStatement",      (fun a -> ExpressionStatement);
    "DeclarationStatement",     (fun a -> DeclarationStatement);
    "TryBlock",                 (fun a -> TryBlock);
    "LabeledStatement",         (fun a -> LabeledStatement);
    "SelectionStatement",       (fun a -> SelectionStatement);
    "IfStatement",              (fun a -> IfStatement);
    "ElseStatement",            (fun a -> ElseStatement);
    "SwitchStatement",          (fun a -> SwitchStatement);
    "CompoundStatement",        (fun a -> CompoundStatement);
    "IterationStatement",       (fun a -> IterationStatement);
    "WhileStatement",           (fun a -> WhileStatement);
    "DoStatement",              (fun a -> DoStatement);
    "ForStatement",             (fun a -> ForStatement);
    "ForInStatement",           (fun a -> ForInStatement);
    "RangeBasedForStatement",   (fun a -> RangeBasedForStatement);
    "JumpStatement",            (fun a -> JumpStatement);
    "BreakStatement",           (fun a -> BreakStatement);
    "ContinueStatement",        (fun a -> ContinueStatement);
    "ReturnStatement",          (fun a -> ReturnStatement);
    "GotoStatement",            (fun a-> GotoStatement(find_ident a));
    "ComputedGotoStatement",    (fun a -> ComputedGotoStatement);
    "CoroutineReturnStatement", (fun a -> CoroutineReturnStatement);
    "StatementMacroInvocation", (fun a -> StatementMacroInvocation(find_ident a));
    "StatementMacro",           (fun a -> StatementMacro(find_ident a));
    "IterationMacroInvocation", (fun a -> IterationMacroInvocation(find_ident a));
    "IterationMacro",           (fun a -> IterationMacro(find_ident a));

(* Expression *)
    "This",                                    (fun a -> This);
    "ParenthesizedExpression",                 (fun a -> ParenthesizedExpression);
    "RequiresExpression",                      (fun a -> RequiresExpression);
    "FoldExpression",                          (fun a -> FoldExpression);
    "LambdaExpression",                        (fun a -> LambdaExpression);
    "LogicalOrExpression",                     (fun a -> LogicalOrExpression(find_ident ~default:"||" a));
    "LogicalAndExpression",                    (fun a -> LogicalAndExpression(find_ident ~default:"&&" a));
    "InclusiveOrExpression",                   (fun a -> InclusiveOrExpression(find_ident ~default:"|" a));
    "ExclusiveOrExpression",                   (fun a -> ExclusiveOrExpression(find_ident ~default:"^" a));
    "AndExpression",                           (fun a -> AndExpression (find_ident ~default:"&" a));
    "EqualityExpression",                      (fun a -> EqualityExpression);
    "EqualityExpressionEq",                    (fun a -> EqualityExpressionEq);
    "EqualityExpressionStrictEq",              (fun a -> EqualityExpressionStrictEq);
    "EqualityExpressionNeq",                   (fun a -> EqualityExpressionNeq(find_ident ~default:"!=" a));
    "RelationalExpression",                    (fun a -> RelationalExpression);
    "RelationalExpressionLt",                  (fun a -> RelationalExpressionLt);
    "RelationalExpressionGt",                  (fun a -> RelationalExpressionGt);
    "RelationalExpressionLe",                  (fun a -> RelationalExpressionLe);
    "RelationalExpressionGe",                  (fun a -> RelationalExpressionGe);
    "CompareExpression",                       (fun a -> CompareExpression);
    "ShiftExpression",                         (fun a -> ShiftExpression);
    "ShiftExpressionLeft",                     (fun a -> ShiftExpressionLeft);
    "ShiftExpressionRight",                    (fun a -> ShiftExpressionRight);
    "ShiftExpressionRightU",                   (fun a -> ShiftExpressionRightU);
    "AdditiveExpression",                      (fun a -> AdditiveExpression);
    "AdditiveExpressionAdd",                   (fun a -> AdditiveExpressionAdd);
    "AdditiveExpressionSubt",                  (fun a -> AdditiveExpressionSubt);
    "MultiplicativeExpression",                (fun a -> MultiplicativeExpression);
    "MultiplicativeExpressionMult",            (fun a -> MultiplicativeExpressionMult);
    "MultiplicativeExpressionDiv",             (fun a -> MultiplicativeExpressionDiv);
    "MultiplicativeExpressionMod",             (fun a -> MultiplicativeExpressionMod);
    "PmExpression",                            (fun a -> PmExpression);
    "PmExpressionClass",                       (fun a -> PmExpressionClass);
    "PmExpressionPtr",                         (fun a -> PmExpressionPtr);
    "CastExpression",                          (fun a -> CastExpression);
    "CompoundLiteralExpression",               (fun a -> CompoundLiteralExpression);
    "UnaryExpression",                         (fun a -> UnaryExpression);
    "UnaryExpressionIncr",                     (fun a -> UnaryExpressionIncr);
    "UnaryExpressionDecr",                     (fun a -> UnaryExpressionDecr);
    "UnaryExpressionInd",                      (fun a -> UnaryExpressionInd);
    "UnaryExpressionAddr",                     (fun a -> UnaryExpressionAddr);
    "UnaryExpressionLabelAddr",                (fun a -> UnaryExpressionLabelAddr);
    "UnaryExpressionPlus",                     (fun a -> UnaryExpressionPlus);
    "UnaryExpressionMinus",                    (fun a -> UnaryExpressionMinus);
    "UnaryExpressionNeg",                      (fun a -> UnaryExpressionNeg(find_ident ~default:"!" a));
    "UnaryExpressionCompl",                    (fun a -> UnaryExpressionCompl(find_ident ~default:"~" a));
    "UnaryExpressionSizeof",                   (fun a -> UnaryExpressionSizeof);
    "UnaryExpressionSizeofPack",               (fun a -> UnaryExpressionSizeofPack(find_ident a));
    "UnaryExpressionAlignof",                  (fun a -> UnaryExpressionAlignof);
    "NoexceptExpression",                      (fun a -> NoexceptExpression);
    "PostfixExpression",                       (fun a -> PostfixExpression);
    "PostfixExpressionSubscr",                 (fun a -> PostfixExpressionSubscr);
    "PostfixExpressionFunCall",                (fun a -> PostfixExpressionFunCall);
    "PostfixExpressionFunCallGuarded",         (fun a -> PostfixExpressionFunCallGuarded(find_ident a));
    "PostfixExpressionFunCallMacro",           (fun a -> PostfixExpressionFunCallMacro(find_ident a));
    "PostfixExpressionExplicitTypeConv",       (fun a -> PostfixExpressionExplicitTypeConv);
    "PostfixExpressionExplicitTypeConvExpr",   (fun a -> PostfixExpressionExplicitTypeConvExpr);
    "PostfixExpressionExplicitTypeConvBraced", (fun a -> PostfixExpressionExplicitTypeConvBraced);
    "PostfixExpressionDot",                    (fun a -> PostfixExpressionDot);
    "PostfixExpressionArrow",                  (fun a -> PostfixExpressionArrow);
    "PostfixExpressionIncr",                   (fun a -> PostfixExpressionIncr);
    "PostfixExpressionDecr",                   (fun a -> PostfixExpressionDecr);
    "PostfixExpressionTypeid",                 (fun a -> PostfixExpressionTypeid);
    "PostfixExpressionTypeidExpr",             (fun a -> PostfixExpressionTypeidExpr);
    "PostfixExpressionTypeidTy",               (fun a -> PostfixExpressionTypeidTy);
    "PostfixExpressionDynamic_cast",           (fun a -> PostfixExpressionDynamic_cast);
    "PostfixExpressionStatic_cast",            (fun a -> PostfixExpressionStatic_cast);
    "PostfixExpressionReinterpret_cast",       (fun a -> PostfixExpressionReinterpret_cast);
    "PostfixExpressionConst_cast",             (fun a -> PostfixExpressionConst_cast);
    "AssignmentExpression",                    (fun a -> AssignmentExpression);
    "AssignmentExpressionOverloaded",          (fun a -> AssignmentExpressionOverloaded(find_operator a));
    "AssignmentExpressionEq",                  (fun a -> AssignmentExpressionEq);
    "AssignmentExpressionPlus",                (fun a -> AssignmentExpressionPlus);
    "AssignmentExpressionMinus",               (fun a -> AssignmentExpressionMinus);
    "AssignmentExpressionMult",                (fun a -> AssignmentExpressionMult);
    "AssignmentExpressionDiv",                 (fun a -> AssignmentExpressionDiv);
    "AssignmentExpressionMod",                 (fun a -> AssignmentExpressionMod);
    "AssignmentExpressionShiftLeft",           (fun a -> AssignmentExpressionShiftLeft);
    "AssignmentExpressionShiftRight",          (fun a -> AssignmentExpressionShiftRight);
    "AssignmentExpressionAnd",                 (fun a -> AssignmentExpressionAnd(find_ident ~default:"&=" a));
    "AssignmentExpressionXor",                 (fun a -> AssignmentExpressionXor(find_ident ~default:"^=" a));
    "AssignmentExpressionOr",                  (fun a -> AssignmentExpressionOr(find_ident ~default:"|=" a));
    "ThrowExpression",                         (fun a -> ThrowExpression);
    "ExpressionPair",                          (fun a -> ExpressionPair);
    "ConditionalExpression",                   (fun a -> ConditionalExpression);
    "NewExpression",                           (fun a -> NewExpression);
    "RefNewExpression",                        (fun a -> RefNewExpression);
    "DeleteExpression",                        (fun a -> DeleteExpression);
    "DeleteExpressionBracket",                 (fun a -> DeleteExpressionBracket);
    "YieldExpression",                         (fun a -> YieldExpression);
    "AwaitExpression",                         (fun a -> AwaitExpression);
    "BlockLiteralExpression",                  (fun a -> BlockLiteralExpression);
    "ExpressionMacroInvocation",               (fun a -> ExpressionMacroInvocation(find_ident a));
    "LogicalOrMacroInvocation",                (fun a -> LogicalOrMacroInvocation(find_ident a));
    "DefinedMacroExpression",                  (fun a -> DefinedMacroExpression(find_ident a));
    "HasIncludeExpression",                    (fun a -> HasIncludeExpression(find_include a));
    "HasAttributeExpression",                  (fun a -> HasAttributeExpression);
    "ConstraintLogicalOrExpression",           (fun a -> ConstraintLogicalOrExpression(find_ident ~default:"||" a));
    "ConstraintLogicalAndExpression",          (fun a -> ConstraintLogicalAndExpression(find_ident ~default:"&&" a));

(* Literal *)
    "Literal",                     (fun a -> Literal);
    "IntegerLiteral",              (fun a -> IntegerLiteral(find_value a));
    "CharacterLiteral",            (fun a -> CharacterLiteral(find_value a));
    "FloatingLiteral",             (fun a -> FloatingLiteral(find_value a));
    "StringLiteral",               (fun a -> StringLiteral(find_value a));
    "StringMacro",                 (fun a -> StringMacro(find_ident a));
    "BooleanLiteral",              (fun a -> BooleanLiteral(find_value a));
    "Nullptr",                     (fun a -> Nullptr);
    "ConcatenatedString",          (fun a -> ConcatenatedString);
    "UserDefinedCharacterLiteral", (fun a -> UserDefinedCharacterLiteral(find_value a));
    "UserDefinedStringLiteral",    (fun a -> UserDefinedStringLiteral(find_value a));
    "UserDefinedFloatingLiteral",  (fun a -> UserDefinedFloatingLiteral(find_value a));
    "UserDefinedIntegerLiteral",   (fun a -> UserDefinedIntegerLiteral(find_value a));
    "LiteralMacro",                (fun a -> LiteralMacro(find_ident a));
    "LiteralMacroInvocation",      (fun a -> LiteralMacroInvocation(find_ident a));

(* UnqualifiedId *)
    "UnqualifiedId",        (fun a -> UnqualifiedId);
    "OperatorFunctionId",   (fun a -> OperatorFunctionId);
    "ConversionFunctionId", (fun a -> ConversionFunctionId);
    "LiteralOperatorId",    (fun a -> LiteralOperatorId(find_ident a));
    "Destructor",           (fun a -> Destructor);
    "TemplateId",           (fun a -> TemplateId);
    "TemplateIdOp",         (fun a -> TemplateIdOp);
    "TemplateIdLit",        (fun a -> TemplateIdLit);

(* Operator *)
    "Operator",      (fun a -> Operator);
    "New",           (fun a -> New);
    "Delete",        (fun a -> Delete);
    "NewBracket",    (fun a -> NewBracket);
    "DeleteBracket", (fun a -> DeleteBracket);
    "Parentheses",   (fun a -> Parentheses);
    "Brackets",      (fun a -> Brackets);
    "MinusGt",       (fun a -> MinusGt);
    "MinusGtStar",   (fun a -> MinusGtStar);
    "Tilde",         (fun a -> Tilde(find_ident ~default:"~" a));
    "Exclam",        (fun a -> Exclam(find_ident ~default:"!" a));
    "Plus",          (fun a -> Plus);
    "Minus",         (fun a -> Minus);
    "Star",          (fun a -> Star);
    "Slash",         (fun a -> Slash);
    "Perc",          (fun a -> Perc);
    "Hat",           (fun a -> Hat(find_ident ~default:"^" a));
    "Amp",           (fun a -> Amp(find_ident ~default:"&" a));
    "Bar",           (fun a -> Bar(find_ident ~default:"|" a));
    "Eq",            (fun a -> Eq);
    "PlusEq",        (fun a -> PlusEq);
    "MinusEq",       (fun a -> MinusEq);
    "StarEq",        (fun a -> StarEq);
    "SlashEq",       (fun a -> SlashEq);
    "PercEq",        (fun a -> PercEq);
    "HatEq",         (fun a -> HatEq(find_ident ~default:"^=" a));
    "AmpEq",         (fun a -> AmpEq(find_ident ~default:"&=" a));
    "BarEq",         (fun a -> BarEq(find_ident ~default:"|=" a));
    "EqEq",          (fun a -> EqEq);
    "ExclamEq",      (fun a -> ExclamEq(find_ident ~default:"!=" a));
    "Lt",            (fun a -> Lt);
    "Gt",            (fun a -> Gt);
    "LtEq",          (fun a -> LtEq);
    "GtEq",          (fun a -> GtEq);
    "LtEqGt",        (fun a -> LtEqGt);
    "AmpAmp",        (fun a -> AmpAmp(find_ident ~default:"&&" a));
    "BarBar",        (fun a -> BarBar(find_ident ~default:"||" a));
    "LtLt",          (fun a -> LtLt);
    "GtGt",          (fun a -> GtGt);
    "LtLtEq",        (fun a -> LtLtEq);
    "GtGtEq",        (fun a -> GtGtEq);
    "PlusPlus",      (fun a -> PlusPlus);
    "MinusMinus",    (fun a -> MinusMinus);
    "Comma",         (fun a -> Comma);
    "Semicolon",     (fun a -> Semicolon);
    "Co_await",      (fun a -> Co_await);
    "DotStar",       (fun a -> DotStar);
    "Dot",           (fun a -> Dot);

(* DefiningTypeSpecifier *)
    "DefiningTypeSpecifier",         (fun a -> DefiningTypeSpecifier);
    "SimpleTypeSpecifier",           (fun a -> SimpleTypeSpecifier(find_ident a));
    "ElaboratedTypeSpecifier",       (fun a -> ElaboratedTypeSpecifier);
    "ElaboratedTypeSpecifierClass",  (fun a -> ElaboratedTypeSpecifierClass(find_ident a));
    "ElaboratedTypeSpecifierStruct", (fun a -> ElaboratedTypeSpecifierStruct(find_ident a));
    "ElaboratedTypeSpecifierUnion",  (fun a -> ElaboratedTypeSpecifierUnion(find_ident a));
    "ElaboratedTypeSpecifierEnum",   (fun a -> ElaboratedTypeSpecifierEnum(find_ident a));
    "TypenameSpecifier",             (fun a -> TypenameSpecifier(find_ident a));
    "CvQualifier",                   (fun a -> CvQualifier);
    "CvMacro",                       (fun a -> CvMacro(find_ident a));
    "CvMacroInvocation",             (fun a -> CvMacroInvocation(find_ident a));
    "TypeMacro",                     (fun a -> TypeMacro(find_ident a));
    "Const",                         (fun a -> Const);
    "Volatile",                      (fun a -> Volatile);
    "Restrict",                      (fun a -> Restrict(find_ident a));
    "MsAsmBlock",                    (fun a -> MsAsmBlock(find_ident a, find_block a));
    "MsCdecl",                       (fun a -> MsCdecl(find_ident a));
    "MsStdcall",                     (fun a -> MsStdcall(find_ident a));
    "MsPragma",                      (fun a -> MsPragma(find_ident a));
    "MsWarningSpecifier",            (fun a -> MsWarningSpecifier(find_ident a));
    "MsProperty",                    (fun a -> MsProperty(find_ident a));
    "MsAttributeSpecifier",          (fun a -> MsAttributeSpecifier);
    "CallingConvention",             (fun a -> CallingConvention(find_ident a));
    "GnuAsmBlock",                   (fun a -> GnuAsmBlock(find_ident a, find_block a));
    "GnuAttribute",                  (fun a -> GnuAttribute(find_ident ~default:"__attribute__" a));
    "GnuStatementExpression",        (fun a -> GnuStatementExpression);
    "ClassSpecifier",                (fun a -> ClassSpecifier);
    "EnumSpecifier",                 (fun a -> EnumSpecifier);

(* BasicType *)
    "BasicType", (fun a -> BasicType);
    "Char",      (fun a -> Char);
    "Char8_t",   (fun a -> Char8_t);
    "Char16_t",  (fun a -> Char16_t);
    "Char32_t",  (fun a -> Char32_t);
    "Wchar_t",   (fun a -> Wchar_t);
    "Bool",      (fun a -> Bool);
    "Short",     (fun a -> Short);
    "Int",       (fun a -> Int);
    "Long",      (fun a -> Long);
    "Signed",    (fun a -> Signed);
    "Unsigned",  (fun a -> Unsigned);
    "Float",     (fun a -> Float);
    "Double",    (fun a -> Double);
    "Void",      (fun a -> Void);
    "UnsignedInt",  (fun a -> UnsignedInt);
    "UnsignedLong", (fun a -> UnsignedLong);

(* AccessSpecifier *)
    "AccessSpecifier", (fun a -> AccessSpecifier);
    "Private",         (fun a -> Private);
    "Protected",       (fun a -> Protected);
    "Public",          (fun a -> Public);

(* AttributeSpecifier *)
    "AttributeSpecifier",                (fun a -> AttributeSpecifier);
    "StandardAttributeSpecifier",        (fun a -> StandardAttributeSpecifier);
    "ContractAttributeSpecifier",        (fun a -> ContractAttributeSpecifier);
    "ContractAttributeSpecifierExpects", (fun a -> ContractAttributeSpecifierExpects);
    "ContractAttributeSpecifierEnsures", (fun a -> ContractAttributeSpecifierEnsures(find_ident a));
    "ContractAttributeSpecifierAssert",  (fun a -> ContractAttributeSpecifierAssert);
    "AlignmentAttributeSpecifier",       (fun a -> AlignmentAttributeSpecifier(find_pack a));
    "AttributeMacro",                    (fun a -> AttributeMacro(find_ident a));

(* RefQualifier *)
    "RefQualifier",       (fun a -> RefQualifier);
    "RefQualifierAmp",    (fun a -> RefQualifierAmp);
    "RefQualifierAmpAmp", (fun a -> RefQualifierAmpAmp);

(* PlaceholderTypeSpecifier *)
    "PlaceholderTypeSpecifier",         (fun a -> PlaceholderTypeSpecifier);
    "PlaceholderTypeSpecifierAuto",     (fun a -> PlaceholderTypeSpecifierAuto);
    "PlaceholderTypeSpecifierDecltype", (fun a -> PlaceholderTypeSpecifierDecltype);

(* PtrOperator *)
    "PtrOperator",       (fun a -> PtrOperator);
    "PtrOperatorStar",   (fun a -> PtrOperatorStar);
    "PtrOperatorAmp",    (fun a -> PtrOperatorAmp);
    "PtrOperatorAmpAmp", (fun a -> PtrOperatorAmpAmp);
    "PtrOperatorHat",    (fun a -> PtrOperatorHat);
    "PtrOperatorMacro",  (fun a -> PtrOperatorMacro(find_ident a));

(* Declarator *)
    "Declarator",             (fun a -> Declarator);
    "DeclaratorFunc",         (fun a -> DeclaratorFunc);
    "PtrDeclarator",          (fun a -> PtrDeclarator);
    "PtrDeclaratorPtr",       (fun a -> PtrDeclaratorPtr);
    "NoptrDeclarator",        (fun a -> NoptrDeclarator);
    "NoptrDeclaratorId",      (fun a -> NoptrDeclaratorId);
    "NoptrDeclaratorParen",   (fun a -> NoptrDeclaratorParen);
    "NoptrDeclaratorFunc",    (fun a -> NoptrDeclaratorFunc);
    "NoptrDeclaratorOldFunc", (fun a -> NoptrDeclaratorOldFunc);
    "NoptrDeclaratorArray",   (fun a-> NoptrDeclaratorArray);
    "DtorMacro",              (fun a -> DtorMacro(find_ident a));

(* NoexceptSpecifier *)
    "NoexceptSpecifier",         (fun a -> NoexceptSpecifier);
    "NoexceptSpecifierThrow",    (fun a -> NoexceptSpecifierThrow);
    "NoexceptSpecifierThrowAny", (fun a -> NoexceptSpecifierThrowAny);

(* VirtSpecifier *)
    "VirtSpecifier",                (fun a -> VirtSpecifier);
    "VirtSpecifierFinal",           (fun a -> VirtSpecifierFinal);
    "VirtSpecifierOverride",        (fun a -> VirtSpecifierOverride);
    "VirtSpecifierMacro",           (fun a -> VirtSpecifierMacro(find_ident a));
    "VirtSpecifierMacroInvocation", (fun a -> VirtSpecifierMacroInvocation(find_ident a));

(* StorageClassSpecifier *)
    "StorageClassSpecifier",             (fun a -> StorageClassSpecifier);
    "StorageClassSpecifierStatic",       (fun a -> StorageClassSpecifierStatic);
    "StorageClassSpecifierThread_local", (fun a -> StorageClassSpecifierThread_local);
    "StorageClassSpecifierExtern",       (fun a -> StorageClassSpecifierExtern);
    "StorageClassSpecifierMutable",      (fun a -> StorageClassSpecifierMutable);
    "StorageClassSpecifierRegister",     (fun a -> StorageClassSpecifierRegister);
    "StorageClassSpecifierVaxGlobaldef", (fun a -> StorageClassSpecifierVaxGlobaldef);

(* FunctionSpecifier *)
    "FunctionSpecifier",        (fun a -> FunctionSpecifier);
    "FunctionSpecifierVirtual", (fun a -> FunctionSpecifierVirtual);
    "ExplicitSpecifier",        (fun a -> ExplicitSpecifier);

(* ClassHead *)
    "ClassHead",                (fun a -> ClassHead);
    "ClassHeadClass",           (fun a -> ClassHeadClass);
    "ClassHeadStruct",          (fun a -> ClassHeadStruct);
    "ClassHeadUnion",           (fun a -> ClassHeadUnion);
    "ClassHeadMacro",           (fun a -> ClassHeadMacro(find_ident a));
    "ClassHeadMacroInvocation", (fun a -> ClassHeadMacroInvocation(find_ident a));

(* EnumHead *)
    "EnumHead",           (fun a -> EnumHead);
    "EnumHeadEnum",       (fun a -> EnumHeadEnum);
    "EnumHeadEnumClass",  (fun a -> EnumHeadEnumClass);
    "EnumHeadEnumStruct", (fun a -> EnumHeadEnumStruct);
    "EnumHeadEnumMacro",  (fun a -> EnumHeadEnumMacro(find_ident a));

(* TypeParameterKey *)
    "TypeParameterKey",         (fun a -> TypeParameterKey);
    "TypeParameterKeyClass",    (fun a -> TypeParameterKeyClass);
    "TypeParameterKeyTypename", (fun a -> TypeParameterKeyTypename);

(* FunctionBody *)
    "FunctionBody",                (fun a -> FunctionBody);
    "FunctionBodyDefault",         (fun a -> FunctionBodyDefault);
    "FunctionBodyDelete",          (fun a -> FunctionBodyDelete);
    "FunctionTryBlock",            (fun a -> FunctionTryBlock);
    "FunctionBodyMacro",           (fun a -> FunctionBodyMacro(find_ident a));
    "FunctionBodyMacroInvocation", (fun a-> FunctionBodyMacroInvocation(find_ident a));

(* DeclSpecifier *)
    "DeclSpecifier",                (fun a -> DeclSpecifier);
    "DeclSpecifierInline",          (fun a -> DeclSpecifierInline);
    "DeclSpecifierConstexpr",       (fun a -> DeclSpecifierConstexpr);
    "DeclSpecifierConsteval",       (fun a -> DeclSpecifierConsteval);
    "DeclSpecifierConstinit",       (fun a -> DeclSpecifierConstinit);
    "DeclSpecifierTypedef",         (fun a -> DeclSpecifierTypedef);
    "DeclSpecifierFriend",          (fun a -> DeclSpecifierFriend);
    "DeclSpecifierMacro",           (fun a -> DeclSpecifierMacro(find_ident a));
    "DeclSpecifierMacroInvocation", (fun a -> DeclSpecifierMacroInvocation(find_ident a));

(* Requirement *)
    "Requirement",           (fun a -> Requirement);
    "SimpleRequirement",     (fun a -> SimpleRequirement);
    "TypeRequirement",       (fun a -> TypeRequirement);
    "CompoundRequirement",   (fun a -> CompoundRequirement);
    "ReturnTypeRequirement", (fun a -> ReturnTypeRequirement);
    "NestedRequirement",     (fun a -> NestedRequirement);

(* AbstractDeclarator *)
    "AbstractDeclarator",           (fun a -> AbstractDeclarator);
    "AbstractDeclaratorFunc",       (fun a -> AbstractDeclaratorFunc);
    "PtrAbstractDeclarator",        (fun a -> PtrAbstractDeclarator);
    "PtrAbstractDeclaratorPtr",     (fun a -> PtrAbstractDeclaratorPtr);
    "NoptrAbstractDeclarator",      (fun a -> NoptrAbstractDeclarator);
    "NoptrAbstractDeclaratorFunc",  (fun a -> NoptrAbstractDeclaratorFunc);
    "NoptrAbstractDeclaratorArray", (fun a -> NoptrAbstractDeclaratorArray);
    "NoptrAbstractDeclaratorParen", (fun a -> NoptrAbstractDeclaratorParen);

(* NoptrAbstractPackDeclarator *)
    "NoptrAbstractPackDeclaratorFunc",  (fun a -> NoptrAbstractPackDeclaratorFunc);
    "NoptrAbstractPackDeclaratorArray", (fun a -> NoptrAbstractPackDeclaratorArray);

(* SimpleCapture *)
    "SimpleCapture",         (fun a -> SimpleCapture(find_ident a));
    "SimpleCaptureAmp",      (fun a -> SimpleCaptureAmp(find_ident a));
    "SimpleCaptureThis",     (fun a -> SimpleCaptureThis);
    "SimpleCaptureStarThis", (fun a -> SimpleCaptureStarThis);

(* InitCapture *)
    "InitCapture",    (fun a-> InitCapture(find_ident a));
    "InitCaptureAmp", (fun a -> InitCaptureAmp(find_ident a));

(* LambdaCapture *)
    "LambdaCapture",                (fun a -> LambdaCapture);
    "LambdaCaptureDefaultEq",       (fun a -> LambdaCaptureDefaultEq);
    "LambdaCaptureDefaultAmp",      (fun a -> LambdaCaptureDefaultAmp);
    "LambdaCaptureMacroInvocation", (fun a -> LambdaCaptureMacroInvocation(find_ident a));

(* MemberDeclarator *)
    "MemberDeclarator",         (fun a -> MemberDeclarator);
    "MemberDeclaratorDecl",     (fun a -> MemberDeclaratorDecl);
    "MemberDeclaratorBitField", (fun a-> MemberDeclaratorBitField(find_ident a));

(* Label *)
    "Label",                (fun a -> Label(find_ident a));
    "CaseLabel",            (fun a -> CaseLabel);
    "RangedCaseLabel",      (fun a -> RangedCaseLabel);
    "DefaultLabel",         (fun a -> DefaultLabel);
    "LabelMacroInvocation", (fun a -> LabelMacroInvocation(find_ident a));

(* ContractLevel *)
    "ContractLevel",         (fun a -> ContractLevel);
    "ContractLevelDefault",  (fun a -> ContractLevelDefault);
    "ContractLevelAudit",    (fun a -> ContractLevelAudit);
    "ContractLevelAxiom",    (fun a -> ContractLevelAxiom);
    "MemberSpecification",   (fun a -> MemberSpecification);
    "MemberDeclarationDecl", (fun a -> MemberDeclarationDecl);

(* *)
    "Asm",            (fun a -> Asm);
    "Default",        (fun a -> Default);
    "Explicit",       (fun a -> Explicit);
    "Extern",         (fun a -> Extern);
    "Inline",         (fun a -> Inline);
    "Noexcept",       (fun a -> Noexcept);
    "Template",       (fun a -> Template);
    "Try",            (fun a -> Try);
    "Virtual",        (fun a -> Virtual);
    "ColonColon",     (fun a -> ColonColon);
    "Ellipsis",       (fun a -> Ellipsis);
    "At",             (fun a -> At);
    "Lparen",         (fun a -> Lparen);
    "Rparen",         (fun a -> Rparen);
    "ClosingBrace",   (fun a -> ClosingBrace);
    "OpeningBracket", (fun a -> OpeningBracket);
    "ClosingBracket", (fun a -> ClosingBracket);

    "DummyBody", (fun a -> DummyBody);
    "DummyDecl", (fun a -> DummyDecl);
    "DummyStmt", (fun a -> DummyStmt);
    "DummyExpr", (fun a -> DummyExpr);
    "DummyDtor", (fun a -> DummyDtor);

    "PureSpecifier",                (fun a -> PureSpecifier);
    "BaseSpecifier",                (fun a -> BaseSpecifier);
    "BaseClause",                   (fun a -> BaseClause);
    "BaseMacro",                    (fun a -> BaseMacro(find_ident a));
    "BaseSpecMacro",                (fun a -> BaseSpecMacro(find_ident a));
    "SuffixMacro",                  (fun a -> SuffixMacro(find_ident a));
    "ClassVirtSpecifierFinal",      (fun a -> ClassVirtSpecifierFinal);
    "ClassName",                    (fun a -> ClassName(find_ident a));
    "ClassHeadName",                (fun a -> ClassHeadName(find_name a));
    "MacroArgument",                (fun a -> MacroArgument);
    "NoptrNewDeclarator",           (fun a -> NoptrNewDeclarator);
    "NewInitializer",               (fun a -> NewInitializer);
    "NewInitializerMacro",          (fun a -> NewInitializerMacro(find_ident a));
    "ArgumentsMacro",               (fun a -> ArgumentsMacro(find_ident a));
    "ArgumentsMacroInvocation",     (fun a -> ArgumentsMacroInvocation(find_ident a));
    "NewDeclaratorPtr",             (fun a -> NewDeclaratorPtr);
    "NewDeclarator",                (fun a -> NewDeclarator);
    "NewTypeId",                    (fun a -> NewTypeId);
    "NewPlacement",                 (fun a -> NewPlacement);
    "LambdaDeclarator",             (fun a -> LambdaDeclarator);
    "ParenthesizedInitList",        (fun a -> ParenthesizedInitList);
    "LambdaIntroducer",             (fun a -> LambdaIntroducer);
    "AbstractPackDeclarator",       (fun a -> AbstractPackDeclarator);
    "AbstractPack",                 (fun a -> AbstractPack);
    "RequirementBody",              (fun a -> RequirementBody);
    "RequirementParameterList",     (fun a -> RequirementParameterList);
    "MemInitializer",               (fun a -> MemInitializer);
    "MemInitMacroInvocation",       (fun a -> MemInitMacroInvocation(find_ident a));
    "QualifiedTypeName",            (fun a -> QualifiedTypeName);
    "InitDeclarator",               (fun a -> InitDeclarator);
    "ConceptDefinition",            (fun a -> ConceptDefinition(find_name a));
    "CtorInitializer",              (fun a -> CtorInitializer);
    "RequiresClause",               (fun a -> RequiresClause);
    "TypeParameter",                (fun a -> TypeParameter(find_ident a));
    "TemplateHead",                 (fun a -> TemplateHead);
    "EnclosingNamespaceSpecifier",  (fun a -> EnclosingNamespaceSpecifier(find_ident a));
    "Enumerator",                   (fun a -> Enumerator(find_ident a));
    "EnumeratorDefinition",         (fun a -> EnumeratorDefinition);
    "EnumeratorDefinitionMacro",    (fun a -> EnumeratorDefinitionMacro(find_ident a));
    "TypeMacroInvocation",          (fun a -> TypeMacroInvocation(find_ident a));
    "Condition",                    (fun a -> Condition);
    "ParameterDeclaration",         (fun a -> ParameterDeclaration);
    "ParameterDeclarationClause",   (fun a -> ParameterDeclarationClause(find_is_va a));
    "ParametersAndQualifiers",      (fun a -> ParametersAndQualifiers);
    "ParamDeclMacro",               (fun a -> ParamDeclMacro(find_ident a));
    "ParamDeclMacroInvocation",     (fun a -> ParamDeclMacroInvocation(find_ident a));
    "ParametersMacro",              (fun a -> ParametersMacro(find_ident a));
    "ParametersMacroInvocation",    (fun a -> ParametersMacroInvocation(find_ident a));
    "Handler",                      (fun a -> Handler);
    "ExceptionDeclaration",         (fun a -> ExceptionDeclaration);
    "ExpressionList",               (fun a -> ExpressionList);
    "EqualInitializer",             (fun a -> EqualInitializer);
    "DesignatedInitializerClause",  (fun a -> DesignatedInitializerClause);
    "DesignatorField",              (fun a -> DesignatorField(find_ident a));
    "DesignatorIndex",              (fun a -> DesignatorIndex);
    "TrailingReturnType",           (fun a -> TrailingReturnType);
    "BracedInitList",               (fun a -> BracedInitList);
    "ForRangeDeclaration",          (fun a -> ForRangeDeclaration);
    "Constexpr",                    (fun a -> Constexpr);
    "DefiningTypeId",               (fun a -> DefiningTypeId);
    "EnumHeadName",                 (fun a -> EnumHeadName(find_ident a));
    "EnumBase",                     (fun a -> EnumBase);
    "QualifiedId",                  (fun a -> QualifiedId);
    "QualifiedNamespaceSpecifier",  (fun a -> QualifiedNamespaceSpecifier(find_name a));
    "TypeName",                     (fun a -> TypeName(find_name a));
    "ConversionDeclarator",         (fun a -> ConversionDeclarator);
    "ConversionTypeId",             (fun a -> ConversionTypeId);
    "Typename",                     (fun a -> Typename);
    "UsingDeclarator",              (fun a -> UsingDeclarator);
    "TypeConstraint",               (fun a -> TypeConstraint(find_name a));
    "TypeId",                       (fun a -> TypeId);
    "DecltypeSpecifier",            (fun a -> DecltypeSpecifier);
    "SimpleTemplateId",             (fun a -> SimpleTemplateId(find_name a));
    "Identifier",                   (fun a -> Identifier(find_ident a));
    "IdentifierMacroInvocation",    (fun a -> IdentifierMacroInvocation(find_ident a));
    "NestedNameSpecifier",          (fun a -> NestedNameSpecifier);
    "NestedNameSpecifierHead",      (fun a -> NestedNameSpecifierHead);
    "NestedNameSpecifierIdent",     (fun a -> NestedNameSpecifierIdent(find_ident a));
    "NestedNameSpecifierTempl",     (fun a -> NestedNameSpecifierTempl(find_ident a));
    "NestedNameSpecifierDeclty",    (fun a -> NestedNameSpecifierDeclty);
    "PackExpansion",                (fun a -> PackExpansion);
    "AttributeUsingPrefix",         (fun a -> AttributeUsingPrefix);
    "Attribute",                    (fun a -> Attribute);
    "AttributeToken",               (fun a -> AttributeToken(find_ident a));
    "AttributeScopedToken",         (fun a -> AttributeScopedToken(find_ident a));
    "AttributeNamespace",           (fun a -> AttributeNamespace(find_ident a));
    "AttributeArgumentClause",      (fun a -> AttributeArgumentClause);
    "AttributeArgumentClauseMacro", (fun a -> AttributeArgumentClauseMacro(find_ident a));
    "AttributeMacroInvocation",     (fun a -> AttributeMacroInvocation(find_ident a));
    "BalancedToken",                (fun a -> BalancedToken);
    "BalancedTokenParen",           (fun a -> BalancedTokenParen);
    "BalancedTokenBracket",         (fun a -> BalancedTokenBracket);
    "BalancedTokenBrace",           (fun a -> BalancedTokenBrace);
    "BalancedTokenSingle",          (fun a -> BalancedTokenSingle(find_code a));
    "TokenSeq",                     (fun a -> TokenSeq(find_code a));
    "ObjectLikeMacro",              (fun a -> ObjectLikeMacro);
    "FunctionLikeMacro",            (fun a -> FunctionLikeMacro(find_macro_kind a));
    "OperatorMacro",                (fun a -> OperatorMacro(find_ident a));
    "OperatorMacroInvocation",      (fun a -> OperatorMacroInvocation(find_ident a));
    "DefiningTypeSpecifierSeq",     (fun a -> DefiningTypeSpecifierSeq);
    "DeclSpecifierSeq",             (fun a -> DeclSpecifierSeq);
    "TypeSpecifierSeq",             (fun a -> TypeSpecifierSeq);
    "FunctionHead",                 (fun a -> FunctionHead);
    "AccessSpecAnnot",              (fun a -> AccessSpecAnnot(find_ident a));
    "EnumeratorMacroInvocation",    (fun a -> EnumeratorMacroInvocation(find_ident a));
    "AccessSpecMacro",              (fun a -> AccessSpecMacro(find_ident a));
    "OpeningBrace",                 (fun a -> OpeningBrace);
    "GnuAsmBlockFragmented",        (fun a -> GnuAsmBlockFragmented(find_ident a));
    "GnuAsmFragment",               (fun a -> GnuAsmFragment(find_block a));
    "DesignatorFieldOld",           (fun a -> DesignatorFieldOld(find_ident a));
    "DoxygenLine",                  (fun a -> DoxygenLine(find_line a));
    "NoexceptSpecifierMacro",       (fun a -> NoexceptSpecifierMacro(find_ident a));
    "CudaExecutionConfiguration",   (fun a -> CudaExecutionConfiguration);
    "CudaKernelCall",               (fun a -> CudaKernelCall);
    "SimpleTemplateIdM",            (fun a -> SimpleTemplateIdM);
    "NamespaceHead",                (fun a -> NamespaceHead(find_ident a));
    "NamedNamespaceDefinitionHead", (fun a -> NamedNamespaceDefinitionHead(find_ident a));
    "InitializerClause",            (fun a -> InitializerClause);
    "TemplParamMacroInvocation",    (fun a -> TemplParamMacroInvocation(find_ident a));
    "PpIfSectionHandler",           (fun a -> PpIfSectionHandler);
    "BlockHeadMacro",               (fun a -> BlockHeadMacro(find_ident a));
    "BlockEndMacro",                (fun a -> BlockEndMacro(find_ident a));
    "DeclStmtBlock",                (fun a -> DeclStmtBlock);
    "AsmShader",                    (fun a -> AsmShader(find_code a));
    "AsmName",                      (fun a -> AsmName(find_ident a));
    "AsmDirective",                 (fun a -> AsmDirective(find_ident a));
    "VaArgs",                       (fun a -> VaArgs(find_code a));
    "PtrMacro",                     (fun a -> PtrMacro(find_ident a));
    "ClassBody",                    (fun a -> ClassBody);
    "HugeArray",                    (fun a -> HugeArray(find_size a, find_code a));
    "TemplateArguments",            (fun a -> TemplateArguments);
    "SuffixMacroInvocation",        (fun a -> SuffixMacroInvocation(find_ident a));

(* Objective-C *)
    "ObjcAutoreleasepool",                      (fun a -> ObjcAutoreleasepool);
    "ObjcAvailable",                            (fun a -> ObjcAvailable);
    "ObjcCatchClause",                          (fun a -> ObjcCatchClause);
    "ObjcCategoryInterface",                    (fun a -> ObjcCategoryInterface(find_ident a, find_category a));
    "ObjcClassDeclarationList",                 (fun a -> ObjcClassDeclarationList);
    "ObjcClassInterface",                       (fun a -> ObjcClassInterface(find_ident a));
    "ObjcClassMethodDeclaration",               (fun a -> ObjcClassMethodDeclaration);
    "ObjcClassName",                            (fun a -> ObjcClassName(find_ident a));
    "ObjcEncodeExpression",                     (fun a -> ObjcEncodeExpression);
    "ObjcFinally",                              (fun a -> ObjcFinally);
    "ObjcInstanceMethodDeclaration",            (fun a -> ObjcInstanceMethodDeclaration);
    "ObjcInstanceVariableDeclaration",          (fun a -> ObjcInstanceVariableDeclaration);
    "ObjcInstanceVariables",                    (fun a -> ObjcInstanceVariables);
    "ObjcInterfaceDeclaration",                 (fun a -> ObjcInterfaceDeclaration);
    "ObjcKeywordArgument",                      (fun a -> ObjcKeywordArgument(find_ident a));
    "ObjcKeywordDeclarator",                    (fun a -> ObjcKeywordDeclarator(find_ident a));
    "ObjcKeywordSelector",                      (fun a -> ObjcKeywordSelector);
    "ObjcMessageExpression",                    (fun a -> ObjcMessageExpression);
    "ObjcMessageSelector",                      (fun a -> ObjcMessageSelector);
    "ObjcMethodMacroInvocation",                (fun a -> ObjcMethodMacroInvocation(find_ident a));
    "ObjcMethodSelector",                       (fun a -> ObjcMethodSelector);
    "ObjcMethodSelectorPack",                   (fun a -> ObjcMethodSelectorPack);
    "ObjcMethodType",                           (fun a -> ObjcMethodType);
    "ObjcPackage",                              (fun a -> ObjcPackage);
    "ObjcPrivate",                              (fun a -> ObjcPrivate);
    "ObjcPropertyAttribute",                    (fun a -> ObjcPropertyAttribute(find_ident a));
    "ObjcPropertyAttributesDeclaration",        (fun a -> ObjcPropertyAttributesDeclaration);
    "ObjcPropertyDeclaration",                  (fun a -> ObjcPropertyDeclaration);
    "ObjcProtected",                            (fun a -> ObjcProtected);
    "ObjcProtocolDeclaration",                  (fun a -> ObjcProtocolDeclaration(find_ident a));
    "ObjcProtocolDeclarationList",              (fun a -> ObjcProtocolDeclarationList);
    "ObjcProtocolInterfaceDeclarationOptional", (fun a -> ObjcProtocolInterfaceDeclarationOptional);
    "ObjcProtocolInterfaceDeclarationRequired", (fun a -> ObjcProtocolInterfaceDeclarationRequired);
    "ObjcProtocolName",                         (fun a -> ObjcProtocolName(find_ident a));
    "ObjcProtocolReferenceList",                (fun a -> ObjcProtocolReferenceList);
    "ObjcPublic",                               (fun a -> ObjcPublic);
    "ObjcSelector",                             (fun a -> ObjcSelector(find_ident a));
    "ObjcSelectorExpression",                   (fun a -> ObjcSelectorExpression(find_ident a));
    "ObjcSpecifierQualifier",                   (fun a -> ObjcSpecifierQualifier(find_ident a));
    "ObjcStructDeclaration",                    (fun a -> ObjcStructDeclaration);
    "ObjcStructDeclarator",                     (fun a -> ObjcStructDeclarator);
    "ObjcSuperclass",                           (fun a -> ObjcSuperclass(find_ident a));
    "ObjcSynchronized",                         (fun a -> ObjcSynchronized);
    "ObjcThrow",                                (fun a -> ObjcThrow);
    "ObjcTry",                                  (fun a -> ObjcTry);
    "ObjcTryBlock",                             (fun a -> ObjcTryBlock);

    "SwiftArg",     (fun a -> SwiftArg(find_ident a));
    "SwiftFunCall", (fun a -> SwiftFunCall)
  ]
  in
  let tbl = Hashtbl.create (List.length tag_list) in
  let _ =
    List.iter (fun (tname, lab) -> Hashtbl.add tbl tname lab) tag_list
  in
  let of_elem name attrs (_ : string) =
    try
      (Hashtbl.find tbl name) attrs
    with
    | Not_found -> failwith ("Cpp_label.of_tag: tag not found: "^name)
    | e -> failwith ("Cpp_label.of_tag: "^(Printexc.to_string e))
  in
  of_elem
