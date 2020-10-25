#

SRC_DIR = ../../../../../..

TESTPROG = parser
PARSECMD = parsef
PARENT_LIB_NAME = Mfortran
LIB_NAME = $(PARENT_LIB_NAME)Parser

LIB = $(LIB_NAME).cma

LINKS = ../$(LIB)

UTIL_DIR = $(SRC_DIR)/util
LANGS_COMMON_DIR = ../../../common
COMMON_DIR = ../../../../common
LABELS_DIR = labels

INCLS =  -I $(LABELS_DIR) -I $(UTIL_DIR) -I $(LANGS_COMMON_DIR) -I $(COMMON_DIR)
DEP_INCLS = $(INCLS)

COMPFLAGS = $(INCLS)
COMPFLAGS_OPT = $(INCLS)
LINKFLAGS = $(INCLS)
LINKFLAGS_OPT = $(INCLS)

MLY = parser.mly
MLY_MLI = $(MLY:.mly=.mli)
MLY_ML = $(MLY:.mly=.ml)
MLY_OBJ = $(MLY:.mly=.cmo)
MLY_OBJ_OPT = $(MLY:.mly=.cmx)

ULEXER = ulexer.ml
ULEXER_OBJ = $(ULEXER:.ml=.cmo)

TOKENS = tokens
TOKENS_ML = $(TOKENS).ml
TOKENS_MLI = $(TOKENS).mli
TOKENS_OBJ = $(TOKENS).cmo
#TOKENS_OBJ_OPT = $(TOKENS_OBJ:.cmo=.cmx)

LABEL_OBJS = f_header_file.cmo \
	f_access_spec.cmo f_ambiguous.cmo f_intent_spec.cmo f_attr_spec.cmo \
	f_case_selector.cmo f_case_value_range.cmo f_close_spec.cmo \
	f_connect_spec.cmo f_constant.cmo f_control_edit_desc.cmo \
	f_defined_operator.cmo f_format.cmo f_format_item.cmo \
	f_intrinsic_operator.cmo f_generic_spec.cmo f_inquire_spec.cmo \
	f_internal_subprogram.cmo f_io_control_spec.cmo f_linda_call.cmo \
	f_module_subprogram.cmo f_ocl_directive.cmo f_omp_clause.cmo \
	f_omp_directive.cmo f_omp_construct.cmo f_acc_clause.cmo \
	f_acc_directive.cmo f_acc_construct.cmo f_position_spec.cmo \
	f_pp_directive.cmo f_type_spec.cmo f_prefix_spec.cmo f_program_unit.cmo \
	f_stmt.cmo f_type_attr_spec.cmo f_xlf.cmo f_dec.cmo f_binding_attr.cmo \
	f_proc_component_attr_spec.cmo f_proc_attr_spec.cmo f_wait_spec.cmo \
	f_flush_spec.cmo

LIB_OBJS = common.cmo context.cmo label_common.cmo \
	$(foreach l,$(LABEL_OBJS),$(LABELS_DIR)/$(l)) labels.cmo label.cmo \
	pinfo.cmo ast.cmo \
	printer.cmo source.cmo macro.cmo parser_aux.cmo disambg.cmo \
	$(TOKENS_OBJ) tokens_.cmo token.cmo \
	$(MLY_OBJ) $(ULEXER_OBJ) keyword.cmo \
	tokensource.cmo branch.cmo tokenbuffer.cmo trunk.cmo tokenpp.cmo \
	scanner.cmo elaborate.cmo lib.cmo 

OBJS = $(LIB_OBJS) main.cmo

_PARSECMD_OBJS = astf.cmo parsef.cmo

PARSECMD_OBJS = $(LIB_OBJS) $(_PARSECMD_OBJS)

LANGS_COMMON_OBJS = fname.cmo astloc.cmo layeredloc.cmo position.cmo ranges.cmo \
	regions.cmo compat.cmo ast_base.cmo source_base.cmo env_base.cmo \
	parserlib_base.cmo macro_base.cmo
LANGS_COMMON_OBJS_DEP = $(foreach f,$(LANGS_COMMON_OBJS),$(LANGS_COMMON_DIR)/$(f))
COMMON_OBJS = loc.cmo astml.cmo binding.cmo \
	base_options.cmo fs_options.cmo hash_options.cmo basic_options.cmo \
	storage.cmo fs.cmo
COMMON_OBJS_DEP = $(foreach f,$(COMMON_OBJS),$(COMMON_DIR)/$(f))
UTIL_OBJS = xset.cmo xstring.cmo xlist.cmo xqueue.cmo xoption.cmo xprint.cmo xfile.cmo \
	xhash.cmo compression.cmo xchannel.cmo XML.cmo inst.cmo xarray.cmo
UTIL_OBJS_DEP = $(foreach f,$(UTIL_OBJS),$(UTIL_DIR)/$(f))

OTHER_OBJS = $(UTIL_OBJS) $(COMMON_OBJS) $(LANGS_COMMON_OBJS)
OTHER_OBJS_DEP = $(UTIL_OBJS_DEP) $(COMMON_OBJS_DEP) $(LANGS_COMMON_OBJS_DEP)

#OBJS_ = $(filter-out $(ULEXER_OBJ), $(OBJS))
OBJS_ = $(OBJS) $(_PARSECMD_OBJS)
OBJS__ = $(filter-out $(MLY_OBJ), $(OBJS))

LIB_CMI = $(LIB:.cmo=.cmi)

TESTPROG_OPT = $(TESTPROG).opt
PARSECMD_OPT = $(PARSECMD).opt
LIB_OPT = $(LIB:.cma=.cmxa)
OBJS_OPT = $(OBJS:.cmo=.cmx)
PARSECMD_OBJS_OPT = $(PARSECMD_OBJS:.cmo=.cmx)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)
OTHER_OBJS_OPT = $(OTHER_OBJS:.cmo=.cmx)
LIB_OBJS_OPT = $(LIB_OBJS:.cmo=.cmx)
LINKS_OPT = ../$(LIB_OPT)

PACKAGES_NO_ULEX = -package str,menhirLib,dynlink,volt
PACKAGES = -package pxp,ulex,str,menhirLib,camlzip,cryptokit,netstring,netzip,dynlink,volt
SYNTAX = -syntax camlp4o

USE_MENHIR = true

#MENHIR_TRACE = --trace

MENHIRFLAGS = $(MENHIR_TRACE) --table --external-tokens Tokens_ --infer --ocamlc "ocamlc $(INCLS)" #--error-recovery


include $(SRC_DIR)/rules.mk

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: MENHIR_TRACE := 
production: opt

opt: ../$(LIB_OPT) $(TESTPROG_OPT) $(PARSECMD_OPT)

debug: ../$(LIB) $(TESTPROG) $(PARSECMD)

$(TOKENS_ML): $(MLY) 
	$(MENHIR) --base $(TOKENS) --only-tokens $(MLY)

$(DEP)/tokens_.d: $(TOKENS_ML)

$(MLY_OBJ): $(MLY_ML) 
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES_NO_ULEX) $(SYNTAX) $(PROFFRLGS) $(COMPFLAGS) -c $<

$(MLY_OBJ_OPT): $(MLY_ML)
	$(OCAMLFIND) ocamlopt $(PACKAGES_NO_ULEX) $(SYNTAX) $(PROFFLAGS) $(COMPFLAGS_OPT) -c $<


$(TESTPROG): $(OTHER_OBJS_DEP) $(OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(OBJS) -linkpkg

$(TESTPROG_OPT): $(OTHER_OBJS_DEP_OPT) $(OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(OBJS_OPT) -linkpkg

$(PARSECMD): $(OTHER_OBJS_DEP) $(PARSECMD_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(PARSECMD_OBJS) -linkpkg

$(PARSECMD_OPT): $(OTHER_OBJS_DEP_OPT) $(PARSECMD_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(PARSECMD_OBJS_OPT) -linkpkg

../$(LIB): $(LIB_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -a -o ../$(LIB) $(LIB_OBJS)

../$(LIB_OPT): $(LIB_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -a \
	-o ../$(LIB_OPT) $(LIB_OBJS_OPT)

tokenbuffer.cmo: parser.cmo
tokenbuffer.cmx: parser.cmx


clean:
	$(RM) *.cm* *.o *.a *~ 
	$(RM) $(LABELS_DIR)/*.cm* $(LABELS_DIR)/*.o $(LABELS_DIR)/*~ 
	$(RM) $(TESTPROG) $(TESTPROG_OPT)
	$(RM) $(PARSECMD) $(PARSECMD_OPT)
	$(RM) -r $(DEP)

distclean: clean
	$(RM) $(MLY_ML) $(MLY_MLI) $(MLY:.mly=.output) $(MLY:.mly=.automaton) $(MLY:.mly=.conflicts) 
	$(RM) $(TOKENS_ML) $(TOKENS_MLI)


ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS_:%.cmo=$(DEP)/%.d) $(OBJS__:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
