#

SRC_DIR = ../../../../../..

PROG = parser
PARSECMD = parsev
PARENT_LIB_NAME = Mverilog
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

LIB_OBJS = common.cmo label_common.cmo labels.cmo label.cmo ast.cmo printer.cmo \
	context.cmo source.cmo macro.cmo parser_aux.cmo \
	$(TOKENS_OBJ) token.cmo tokens_.cmo \
	$(MLY_OBJ) $(ULEXER_OBJ) \
	tokenbuffer.cmo scanner.cmo lib.cmo 

OBJS = $(LIB_OBJS) main.cmo

_PARSECMD_OBJS = astv.cmo parsev.cmo

PARSECMD_OBJS = $(LIB_OBJS) $(_PARSECMD_OBJS)

LANGS_COMMON_OBJS = fname.cmo astloc.cmo position.cmo ranges.cmo regions.cmo \
	compat.cmo ast_base.cmo source_base.cmo env_base.cmo layeredloc.cmo \
	parserlib_base.cmo macro_base.cmo
LANGS_COMMON_OBJS_DEP = $(foreach f,$(LANGS_COMMON_OBJS),$(LANGS_COMMON_DIR)/$(f))
COMMON_OBJS = loc.cmo astml.cmo base_options.cmo fs_options.cmo hash_options.cmo \
	 basic_options.cmo storage.cmo fs.cmo
COMMON_OBJS_DEP = $(foreach f,$(COMMON_OBJS),$(COMMON_DIR)/$(f))
UTIL_OBJS = xprint.cmo xstring.cmo xlist.cmo xfile.cmo xset.cmo xhash.cmo \
	compression.cmo xchannel.cmo XML.cmo xarray.cmo xqueue.cmo
UTIL_OBJS_DEP = $(foreach f,$(UTIL_OBJS),$(UTIL_DIR)/$(f))
OTHER_OBJS = $(UTIL_OBJS) $(COMMON_OBJS) $(LANGS_COMMON_OBJS)
OTHER_OBJS_DEP = $(UTIL_OBJS_DEP) $(COMMON_OBJS_DEP) $(LANGS_COMMON_OBJS_DEP)

#OBJS_ = $(filter-out $(ULEXER_OBJ), $(OBJS))
OBJS_ = $(OBJS) $(_PARSECMD_OBJS)
OBJS__ = $(filter-out $(MLY_OBJ), $(OBJS))

PROG_OPT = $(PROG).opt
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

MENHIRFLAGS = $(MENHIR_TRACE) --table --external-tokens Tokens_ --infer --ocamlc "ocamlc $(INCLS)" \
--strategy legacy #--error-recovery


include $(SRC_DIR)/rules.mk

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: MENHIR_TRACE := 
production: opt

opt: ../$(LIB_OPT) $(PROG_OPT) $(PARSECMD_OPT)

debug: ../$(LIB) $(PROG) $(PARSECMD)


$(TOKENS_ML): $(MLY)
	$(MENHIR) --base $(TOKENS) --only-tokens $(MLY)


$(MLY_OBJ): $(MLY_ML)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES_NO_ULEX) $(SYNTAX) $(COMPFLAGS) -c $<

$(MLY_OBJ_OPT): $(MLY_ML)
	$(OCAMLFIND) ocamlopt $(PACKAGES_NO_ULEX) $(SYNTAX) $(COMPFLAGS_OPT) -c $<


$(PROG): $(OTHER_OBJS_DEP) $(OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(OBJS) -linkpkg

$(PROG_OPT): $(OTHER_OBJS_DEP_OPT) $(OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(OBJS_OPT) -linkpkg

$(PARSECMD): $(OTHER_OBJS_DEP) $(PARSECMD_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(LIBS) $(PARSECMD_OBJS) -linkpkg

$(PARSECMD_OPT): $(OTHER_OBJS_DEP_OPT) $(PARSECMD_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(LIBS_OPT) $(PARSECMD_OBJS_OPT) -linkpkg

../$(LIB): $(LIB_OBJS) $(OTHER_OBJS_DEP)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -a -o ../$(LIB) $(LIB_OBJS)

../$(LIB_OPT): $(LIB_OBJS_OPT) $(OTHER_OBJS_DEP_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -a \
	-o ../$(LIB_OPT) $(LIB_OBJS_OPT)



clean:
	$(RM) *.cm* *.o *.a *~ $(PROG) $(PROG_OPT)
	$(RM) $(PARSECMD) $(PARSECMD_OPT)
	$(RM) -r $(DEP)

distclean: clean
	$(RM) $(MLY_ML) $(MLY_MLI) $(MLY:.mly=.output) $(MLY:.mly=.automaton) $(MLY:.mly=.conflicts) 
	$(RM) $(TOKENS_ML) $(TOKENS_MLI)


ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS_:%.cmo=$(DEP)/%.d) $(OBJS__:%.cmo=$(DEP)/%.di) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
