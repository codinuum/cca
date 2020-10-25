#

SRC_DIR = ../../../../../..

TESTPROG = parser
PARENT_LIB_NAME = Mcpp
LIB_NAME = $(PARENT_LIB_NAME)Parser

LIB = $(LIB_NAME).cma

LINKS = ../$(LIB)

UTIL_DIR = $(SRC_DIR)/util
LANGS_COMMON_DIR = ../../../common
COMMON_DIR = ../../../../common

INCLS = -I $(UTIL_DIR) -I $(LANGS_COMMON_DIR) -I $(COMMON_DIR)
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


LIB_OBJS = common.cmo context.cmo label.cmo pinfo.cmo ast.cmo source.cmo parser_aux.cmo \
	$(TOKENS_OBJ) tokens_.cmo token.cmo \
	$(MLY_OBJ) $(ULEXER_OBJ) \
	scanner.cmo lib.cmo 

OBJS = $(LIB_OBJS) main.cmo

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

OBJS__ = $(filter-out $(MLY_OBJ), $(OBJS))


TESTPROG_OPT = $(TESTPROG).opt
LIB_OPT = $(LIB:.cma=.cmxa)
OBJS_OPT = $(OBJS:.cmo=.cmx)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)
OTHER_OBJS_OPT = $(OTHER_OBJS:.cmo=.cmx)
LIB_OBJS_OPT = $(LIB_OBJS:.cmo=.cmx)
LINKS_OPT = ../$(LIB_OPT)

PACKAGES_NO_ULEX = -package str,menhirLib,dynlink,volt
PACKAGES = -package pxp,ulex,str,menhirLib,camlzip,cryptokit,netstring,netzip,dynlink,volt
SYNTAX = -syntax camlp4o

USE_MENHIR = true

#MENHIR_TRACE = --trace

MENHIRFLAGS = $(MENHIR_TRACE) --table --external-tokens Tokens_ --infer --ocamlc "ocamlc -I . $(INCLS)" --inspection #--error-recovery



include $(SRC_DIR)/rules.mk

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: MENHIR_TRACE := 
production: opt

opt: ../$(LIB_OPT) $(TESTPROG_OPT)

debug: ../$(LIB) $(TESTPROG)


$(TOKENS_ML): $(MLY) parser_aux.ml
	$(MENHIR) --base $(TOKENS) --only-tokens $(MLY) --table --inspection

$(DEP)/tokens_.d: $(TOKENS_ML)


$(MLY_OBJ): $(MLY_ML)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES_NO_ULEX) $(SYNTAX) $(PROFFRLGS) $(COMPFLAGS) -c $<

$(MLY_OBJ_OPT): $(MLY_ML) parser.cmi
	$(OCAMLFIND) ocamlopt $(PACKAGES_NO_ULEX) $(SYNTAX) $(PROFFLAGS) $(COMPFLAGS_OPT) -c $<


$(TESTPROG): $(OTHER_OBJS_DEP) $(OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(OBJS) -linkpkg

$(TESTPROG_OPT): $(OTHER_OBJS_DEP_OPT) $(OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(OBJS_OPT) -linkpkg

../$(LIB): $(LIB_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -a -o ../$(LIB) $(LIB_OBJS)

../$(LIB_OPT): $(LIB_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -a \
	-o ../$(LIB_OPT) $(LIB_OBJS_OPT)

clean:
	$(RM) *.cm* *.o *.a *~ 
	$(RM) $(TESTPROG) $(TESTPROG_OPT)
	$(RM) -r $(DEP)

distclean: clean
	$(RM) $(MLY_ML) $(MLY_MLI)
	$(RM)  $(MLY:.mly=.output) $(MLY:.mly=.automaton) $(MLY:.mly=.conflicts) 
	$(RM) $(TOKENS_ML) $(TOKENS_MLI)

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(DEP)/parser_aux.di $(DEP)/ulexer.di $(DEP)/scanner.di \
	$(DEP)/lib.di $(DEP)/$(TOKENS).di #$(MLY_MLI:%.mli=$(DEP)/%.di)
endif
endif

