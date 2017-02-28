#

SRC_DIR = ../..

UTIL_DIR = $(SRC_DIR)/util
UTIL_OBJS = xlist.cmo xarray.cmo weight.cmo compression.cmo
UTIL_OBJS_DEP = $(foreach f,$(UTIL_OBJS),$(UTIL_DIR)/$(f))

OTHER_OBJS = $(UTIL_OBJS_DEP)
OTHER_OBJS_OPT = $(OTHER_OBJS:.cmo=.cmx)

LIB_NAME = Otreediff


LIB = $(LIB_NAME).cmo

LINKS = ../$(LIB)

INCLS = -I $(UTIL_DIR)

COMPFLAGS = -g -for-pack $(LIB_NAME) $(INCLS)
COMPFLAGS_OPT = -for-pack $(LIB_NAME) $(INCLS)
LINKFLAGS =  -g $(INCLS)
LINKFLAGS_OPT = $(INCLS)

MLI =

LIB_OBJS = common.cmo UID.cmo GIndex.cmo path.cmo otree.cmo edit.cmo mapping.cmo \
	clustering.cmo LACS.cmo ZS.cmo lib.cmo

PACKAGES = -package unix,bigarray,volt

LIB_OBJS_OPT = $(LIB_OBJS:.cmo=.cmx)
OBJS_OPT = $(OBJS:.cmo=.cmx) 

LIB_OPT = $(LIB:.cmo=.cmx)
LINKS_OPT = ../$(LIB_OPT)

DEP_INCLS = $(INCLS)

SYNTAX = -syntax camlp4o

STRIP_INFO_LOGGERS = true

include $(SRC_DIR)/rules.mk

all: opt

debug: $(LINKS)

$(LIB): $(LIB_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(LINKFLAGS) -pack -o $(LIB) $^

$(LINKS): $(LIB)
	$(LN) src/$(LIB) ..
	$(LN) src/$(LIB:.cmo=.cmi) ..

opt: $(LINKS_OPT) 

$(LIB_OPT): $(LIB_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) $(PROFFLAGS) -pack -o $(LIB_OPT) $^

$(LINKS_OPT): $(LIB_OPT)
	$(LN) src/$(LIB_OPT) ..
	$(LN) src/$(LIB_OPT:.cmx=.cmi) ..
	$(LN) src/$(LIB_OPT:.cmx=.o) ..

clean:
	$(RM) *.cm* *.o *.a *~
	$(RM) -r $(DEP) 

ifneq ($(MAKECMDGOALS), clean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
