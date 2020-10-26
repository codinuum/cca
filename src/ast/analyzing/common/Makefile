#

SRC_DIR = ../../..

OTREEDIFF_DIR = $(SRC_DIR)/otreediff
UTIL_DIR      = $(SRC_DIR)/util
MLDIFF_DIR    = $(SRC_DIR)/mldiff

INCLS = -I $(OTREEDIFF_DIR) -I $(UTIL_DIR) -I $(MLDIFF_DIR)

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)

PACKAGES = -package csv,uuidm,netstring,pxp,volt

DEP_INCLS = -I $(OTREEDIFF_DIR) -I $(UTIL_DIR)

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk
include ../subs.mk


OBJS = $(COMMON_OBJS)
OBJS_OPT = $(OBJS:.cmo=.cmx)

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: opt

opt: $(OBJS_OPT)

debug: $(OBJS)

clean:
	$(RM) *~ *.o *.cm* 
	$(RM) -r $(DEP)

ifneq ($(MAKECMDGOALS), clean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
