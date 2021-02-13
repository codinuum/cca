#

LIB_NAME = M$(PARSER_NAME)

LIB = $(LIB_NAME).cmo
LIB_P = $(LIB_NAME)_p.cmo

UTIL_DIR         = $(SRC_DIR)/util
OTREEDIFF_DIR    = $(SRC_DIR)/otreediff
COMMON_DIR       = $(ANALYZER_DIR)/common
ENGINE_DIR       = $(ANALYZER_DIR)/engine
LANGS_COMMON_DIR = $(ANALYZER_DIR)/langs/common


PARSER_DIR = parsing

_PARSER_OBJS = lib.cmo

PARSER_OBJS = $(foreach o,$(_PARSER_OBJS),$(PARSER_DIR)/src/$(o))
PARSER_OBJS_OPT = $(PARSER_OBJS:.cmo=.cmx)

INCLS = -I $(UTIL_DIR) -I $(COMMON_DIR) -I $(PARSER_DIR) -I $(OTREEDIFF_DIR) -I $(ENGINE_DIR) \
	$(EXTRA_INCLS) -I $(LANGS_COMMON_DIR)

include $(ANALYZER_DIR)/subs.mk

OTHER_LIBS_DEP = $(COMMON_OBJS_DEP) $(OTREEDIFF_OBJS_DEP) $(ENGINE_OBJS_DEP)

LIBS = $(LIB_NAME)Parser.cma

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)
LINKFLAGS =  -g $(INCLS)
LINKFLAGS_OPT = $(INCLS)

__OBJS = label.cmo unparsing.cmo tree.cmo fact.cmo lib_base.cmo
_OBJS = $(__OBJS) change.cmo lib.cmo
_OBJS_P = $(__OBJS) lib_p.cmo
OBJS = $(foreach o,$(_OBJS),$(PREFIX)$(o))
OBJS_P = $(foreach o,$(_OBJS_P),$(PREFIX)$(o))
OBJS_MT = $(OBJS) mt.cmo
OBJS_P_MT = $(OBJS_P) mt.cmo

SPEC_OBJ = S$(PARSER_NAME).cmo

LIB_OPT = $(LIB:.cmo=.cmxs)
LIB_P_OPT = $(LIB_P:.cmo=.cmxs)
OBJS_OPT = $(OBJS:.cmo=.cmx)
OBJS_P_OPT = $(OBJS_P:.cmo=.cmx)
OBJS_MT_OPT = $(OBJS_MT:.cmo=.cmx)
OBJS_P_MT_OPT = $(OBJS_P_MT:.cmo=.cmx)
LIBS_OPT = $(LIBS:.cma=.cmxa)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)

SPEC_OBJ_OPT = $(SPEC_OBJ:.cmo=.cmx)

DEP_INCLS = -I $(COMMON_DIR) -I $(LANGS_COMMON_DIR) -I $(PARSER_DIR) -I $(ENGINE_DIR)

#EXTRA_OBJS := $(LANGS_COMMON_DIR)/unparsing_base.cmo $(EXTRA_OBJS)
#EXTRA_OBJS_OPT = $(EXTRA_OBJS:.cmo=.cmx)

PACKAGES = -package volt

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk

ifdef MULTI_THREAD
	OBJS = $(OBJS_MT)
	OBJS_OPT = $(OBJS_MT_OPT)
	OBJS_P = $(OBJS_P_MT)
	OBJS_P_OPT = $(OBJS_P_MT_OPT)
endif

.PHONY: parser-objs-production

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: parser-objs-production opt

opt: $(SPEC_OBJ_OPT) $(LIB_OPT) $(LIB_P_OPT)

$(SPEC_OBJ_OPT):
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -c $(SPEC_OBJ:.cmo=.ml)

$(LIB_OPT): $(LIBS_OPT) $(OBJS_OPT) $(OTHER_OBJS_DEP_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -shared -o $(LIB_OPT) \
	$(EXTRA_OBJS_OPT) $(LIBS_OPT) $(OBJS_OPT)

$(LIB_P_OPT): $(LIBS_OPT) $(OBJS_P_OPT) $(OTHER_OBJS_DEP_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -shared -o $(LIB_P_OPT) \
	$(EXTRA_OBJS_OPT) $(LIBS_OPT) $(OBJS_P_OPT)

debug: $(SPEC_OBJ) $(LIB) $(LIB_P)

$(SPEC_OBJ):
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -c $(SPEC_OBJ:.cmo=.ml)

$(LIB): $(LIBS) $(OBJS) $(OTHER_OBJS_DEP)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -shared -o $(LIB) \
	$(EXTRA_OBJS) $(LIBS) $(OBJS)

$(LIB_P): $(LIBS) $(OBJS_P) $(OTHER_OBJS_DEP)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -shared -o $(LIB_P) \
	$(EXTRA_OBJS) $(LIBS) $(OBJS_P)

$(LIBS): $(PARSER_OBJS)

$(LIBS_OPT): $(PARSER_OBJS_OPT)


$(PARSER_OBJS):
	$(MAKE) -C $(PARSER_DIR) debug

$(PARSER_OBJS_OPT):
	$(MAKE) -C $(PARSER_DIR) opt

parser-objs-production:
	$(MAKE) -C $(PARSER_DIR) production

clean:
	$(MAKE) -C $(PARSER_DIR) clean
	$(RM) *~ *.cm* *.o *.a
	$(RM) -r $(DEP)

distclean: clean
	$(MAKE) -C $(PARSER_DIR) distclean

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(SPEC_OBJ:%.cmo=$(DEP)/%.d) $(OBJS:%.cmo=$(DEP)/%.d) $(OBJS_P:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
