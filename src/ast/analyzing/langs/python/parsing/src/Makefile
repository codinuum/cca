#

SRC_DIR = ../../../../../..

PROG = parser
PARENT_LIB_NAME = Mpython
LIB_NAME = $(PARENT_LIB_NAME)Parser

LIB = $(LIB_NAME).cma

LINKS = ../$(LIB)

UTIL_DIR = $(SRC_DIR)/util
COMMON_DIR = $(SRC_DIR)/ast/analyzing/common
LANGS_COMMON_DIR = ../../../common

INCLS =  -I $(UTIL_DIR) -I $(COMMON_DIR) -I $(LANGS_COMMON_DIR)
DEP_INCLS = $(INCLS)

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)
LINKFLAGS = -g $(INCLS) 
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

LIB_OBJS = common.cmo ast.cmo printer.cmo parser_aux.cmo \
	$(TOKENS_OBJ) tokens_.cmo token.cmo \
	$(MLY:.mly=.cmo) $(ULEXER_OBJ) lib.cmo 

OBJS = $(LIB_OBJS) main.cmo

LANGS_COMMON_OBJS = fname.cmo astloc.cmo position.cmo ranges.cmo regions.cmo \
	compat.cmo ast_base.cmo source_base.cmo env_base.cmo layeredloc.cmo \
	parserlib_base.cmo
LANGS_COMMON_OBJS_DEP = $(foreach f,$(LANGS_COMMON_OBJS),$(LANGS_COMMON_DIR)/$(f))

COMMON_OBJS = base_options.cmo fs_options.cmo hash_options.cmo basic_options.cmo \
	storage.cmo fs.cmo
COMMON_OBJS_DEP = $(foreach f,$(COMMON_OBJS),$(COMMON_DIR)/$(f))

UTIL_OBJS = xprint.cmo xstring.cmo xlist.cmo xfile.cmo xset.cmo xhash.cmo xarray.cmo
UTIL_OBJS_DEP = $(foreach f,$(UTIL_OBJS),$(UTIL_DIR)/$(f))

OTHER_OBJS = $(UTIL_OBJS) $(COMMON_OBJS) $(LANGS_COMMON_OBJS)
OTHER_OBJS_DEP = $(UTIL_OBJS_DEP) $(COMMON_OBJS_DEP) $(LANGS_COMMON_OBJS_DEP)

PROG_OPT = $(PROG).opt
LIB_OPT = $(LIB:.cma=.cmxa)
OBJS_OPT = $(OBJS:.cmo=.cmx)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)
OTHER_OBJS_OPT = $(OTHER_OBJS:.cmo=.cmx)
LIB_OBJS_OPT = $(LIB_OBJS:.cmo=.cmx)
LINKS_OPT = ../$(LIB_OPT)

PACKAGES_NO_ULEX = -package menhirLib,dynlink,volt
PACKAGES = -package str,ulex,menhirLib,cryptokit,netstring,dynlink,volt
SYNTAX = -syntax camlp4o

USE_MENHIR = true
MENHIRFLAGS = --table --external-tokens Tokens_ --infer #--error-recovery

include $(SRC_DIR)/rules.mk

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: opt

all: opt

opt: ../$(LIB_OPT) $(PROG_OPT)

debug: ../$(LIB) $(PROG)

$(DEP)/tokens_.d: tokens.ml

$(TOKENS_ML): $(MLY)
	$(MENHIR) --base $(TOKENS) --only-tokens $(MLY)


$(MLY_OBJ): $(MLY_ML)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES_NO_ULEX) $(SYNTAX) $(COMPFLAGS) -c $<

$(MLY_OBJ_OPT): $(MLY_ML)
	$(OCAMLFIND) ocamlopt $(PACKAGES_NO_ULEX) $(SYNTAX) $(COMPFLAGS_OPT) -c $<

$(PROG): $(OTHER_OBJS_DEP) $(OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) -linkpkg $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(OBJS)


$(PROG_OPT): $(OTHER_OBJS_DEP_OPT) $(OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) -linkpkg $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(OBJS_OPT)

../$(LIB): $(LIB_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -a -o ../$(LIB) $(LIB_OBJS)

../$(LIB_OPT): $(LIB_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -a \
	-o ../$(LIB_OPT) $(LIB_OBJS_OPT) \


clean:
	$(RM) *.cm* *.o *.a *~ $(PROG) $(PROG_OPT)
	$(RM) -r $(DEP)

distclean: clean
	$(RM) $(MLY_ML) $(MLY_MLI) $(MLY:.mly=.output) $(MLY:.mly=.automaton) $(MLY:.mly=.conflicts)
	$(RM) $(TOKENS_ML) $(TOKENS_MLI) 

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
