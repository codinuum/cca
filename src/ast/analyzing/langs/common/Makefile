#

SRC_DIR = ../../../..

OBJS = fname.cmo astloc.cmo layeredloc.cmo position.cmo ranges.cmo \
	regions.cmo compat.cmo ast_base.cmo source_base.cmo env_base.cmo \
	parserlib_base.cmo unparsing_base.cmo macro_base.cmo

OBJS_OPT = $(OBJS:.cmo=.cmx)

UTIL_DIR      = $(SRC_DIR)/util
COMMON_DIR    = $(SRC_DIR)/ast/analyzing/common

INCLS = -I $(UTIL_DIR) -I $(COMMON_DIR)

DEP_INCLS = $(INCLS)

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)

PACKAGES = -package ulex,str,unix,menhirLib,netstring,volt
SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: MENHIR_TRACE := 
production: opt

opt: $(OBJS_OPT)

debug: $(OBJS)


clean:
	$(RM) *.cm* *.o *.a *~
	$(RM) -r $(DEP)

ifneq ($(MAKECMDGOALS), clean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif

