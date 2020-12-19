#

SRC_DIR = ../../..

OTREEDIFF_DIR = $(SRC_DIR)/otreediff
UTIL_DIR      = $(SRC_DIR)/util
MLDIFF_DIR    = $(SRC_DIR)/mldiff

COMMON_DIR = ../common

INCLS = -I $(COMMON_DIR) -I $(OTREEDIFF_DIR) -I $(MLDIFF_DIR) -I $(UTIL_DIR)

COMPFLAGS = -g $(INCLS)
COMPFLAGS_OPT = $(INCLS)

include ../subs.mk

OBJS = $(ENGINE_OBJS)

OBJS_OPT = $(OBJS:.cmo=.cmx)

PACKAGES = -package netstring,csv,volt

DEP_INCLS = -I $(COMMON_DIR) -I $(OTREEDIFF_DIR) -I $(MLDIFF_DIR) -I $(UTIL_DIR)

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: opt

opt: $(OBJS_OPT)

debug: $(OBJS)

clean :
	$(RM) *.cm* *.o *.a *~ 
	$(RM) -r $(DEP)

ifneq ($(MAKECMDGOALS), clean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
