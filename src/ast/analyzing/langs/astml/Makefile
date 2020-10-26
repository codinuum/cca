#

SRC_DIR = ../../../..
ANALYZER_DIR = $(SRC_DIR)/ast/analyzing

PARSER_NAME = astml

LIB_NAME = M$(PARSER_NAME)

LIB = $(LIB_NAME).cmo
LIB_P = $(LIB_NAME)_p.cmo


COMMON_DIR    = $(ANALYZER_DIR)/common
OTREEDIFF_DIR = $(SRC_DIR)/otreediff
ENGINE_DIR    = $(ANALYZER_DIR)/engine
UTIL_DIR      = $(SRC_DIR)/util

INCLS = -I $(UTIL_DIR) -I $(COMMON_DIR) -I $(OTREEDIFF_DIR) -I $(ENGINE_DIR)

include $(ANALYZER_DIR)/subs.mk

OTHER_OBJS_DEP = $(COMMON_OBJS_DEP_P) $(OTREEDIFF_OBJS_DEP) $(ENGINE_OBJS_DEP)

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)
LINKFLAGS =  -g $(INCLS)
LINKFLAGS_OPT = $(INCLS)

__OBJS = common.cmo conf.cmo label.cmo tree.cmo fact.cmo lib_base.cmo
_OBJS = $(__OBJS) change.cmo

OBJS = $(_OBJS) lib.cmo

OBJS_P = $(__OBJS) lib_p.cmo

SPEC_OBJ = S$(PARSER_NAME).cmo

LIB_OPT = $(LIB:.cmo=.cmxs)
LIB_P_OPT = $(LIB_P:.cmo=.cmxs)
OBJS_OPT = $(OBJS:.cmo=.cmx)
OBJS_P_OPT = $(OBJS_P:.cmo=.cmx)

SPEC_OBJ_OPT = $(SPEC_OBJ:.cmo=.cmx)

OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)
COMMON_OBJS_DEP_OPT = $(COMMON_OBJS_DEP:.cmo=.cmx)
COMMON_OBJS_DEP_OPT_P = $(COMMON_OBJS_DEP_P:.cmo=.cmx)

PACKAGES = -package pxp,volt

DEP_INCLS = -I $(COMMON_DIR) -I $(ENGINE_DIR)

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: opt

all: opt

opt: $(SPEC_OBJ_OPT) $(LIB_OPT) $(LIB_P_OPT)

$(SPEC_OBJ_OPT): $(COMMON_OBJS_DEP_OPT_P)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(SYNTAX) $(LINKFLAGS_OPT) $(PROFFLAGS) -c $(SPEC_OBJ:.cmo=.ml)

$(LIB_OPT): $(OBJS_OPT) $(OTHER_OBJS_DEP_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -shared -o $(LIB_OPT) \
	$(OBJS_OPT)

$(LIB_P_OPT): $(OBJS_P_OPT) $(OTHER_OBJS_DEP_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -shared -o $(LIB_P_OPT) \
	$(OBJS_P_OPT)

debug: $(SPEC_OBJ) $(LIB) $(LIB_P)

$(SPEC_OBJ): $(COMMON_OBJS_DEP_P)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(SYNTAX) $(LINKFLAGS) -c $(SPEC_OBJ:.cmo=.ml) 

$(LIB): $(OBJS) $(OTHER_OBJS_DEP)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -shared -o $(LIB) \
	$(OBJS)

$(LIB_P): $(OBJS_P) $(OTHER_OBJS_DEP)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -shared -o $(LIB_P) \
	$(OBJS_P)

clean: 
	$(RM) *~ *.cm* *.o *.a
	$(RM) -r $(DEP)

distclean: clean

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(OBJS_P:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
