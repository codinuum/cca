#

SRC_DIR = ../..

BIN_DIR=bin

PARSESRC_CMD  = $(BIN_DIR)/parsesrc.byte

LANGS_DIR = langs
LANGS = astml python java verilog

LANG_DIRS     = $(foreach lang,$(LANGS),$(LANGS_DIR)/$(lang))
LANG_INCLS    = $(foreach langdir,$(LANG_DIRS),-I $(langdir))
LANG_OBJS     = $(foreach lang,$(LANGS),S$(lang).cmo)
LANG_OBJS_DEP = $(foreach lang,$(LANGS),$(LANGS_DIR)/$(lang)/S$(lang).cmo)


COMMON_DIR = common
ENGINE_DIR = engine
LANGS_COMMON_DIR = langs/common

SUBS = $(COMMON_DIR) $(ENGINE_DIR) $(LANGS_COMMON_DIR) $(LANG_DIRS)

OTREEDIFF_DIR = $(SRC_DIR)/otreediff
MLDIFF_DIR    = $(SRC_DIR)/mldiff
UTIL_DIR      = $(SRC_DIR)/util


include subs.mk

_INCLS = -I $(UTIL_DIR) -I $(COMMON_DIR) -I $(OTREEDIFF_DIR) \
	-I $(MLDIFF_DIR) -I $(ENGINE_DIR) -I $(LANGS_COMMON_DIR)

INCLS = $(_INCLS) $(LANG_INCLS)

COMPFLAGS     = -g $(INCLS)
COMPFLAGS_OPT = -g $(INCLS)

LINKFLAGS     = -g $(INCLS)
LINKFLAGS_OPT = -g $(INCLS)

_PARSESRC_OBJS    = parsesrc.cmo

PARSESRC_OBJS    = version.cmo astcore.cmo $(_PARSESRC_OBJS)


OTHER_OBJS_P = $(UTIL_OBJS) $(OTREEDIFF_LIB) $(MLDIFF_OBJS) \
		$(COMMON_OBJS_P) $(ENGINE_OBJS_P) $(LANGS_COMMON_OBJS) $(LANG_OBJS)

OTHER_OBJS_DEP = $(UTIL_OBJS_DEP) \
		$(OTREEDIFF_OBJS_DEP) \
		$(COMMON_OBJS_DEP) \
		$(ENGINE_OBJS_DEP) \
		$(LANG_COMMON_OBJS_DEP) \
		$(LANG_OBJS_DEP) 

PARSESRC_CMD_OPT  = $(PARSESRC_CMD:.byte=.opt)

PARSESRC_OBJS_OPT    = $(PARSESRC_OBJS:.cmo=.cmx)

OTHER_OBJS_OPT     = $(OTHER_OBJS:.cmo=.cmx)
OTHER_OBJS_P_OPT   = $(OTHER_OBJS_P:.cmo=.cmx)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)

PACKAGES = -package pxp,ulex,str,csv,bigarray,num,zip,menhirLib
PACKAGES += -package cryptokit,netzip,uuidm,volt

DEP_INCLS = -I $(COMMON_DIR) -I $(ENGINE_DIR) -I .

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk


.PHONY: subs-production subs-opt debug subs-debug

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: subs-production $(PARSESRC_CMD_OPT)

subs-production:
	for d in $(SUBS); do \
	  if ! $(MAKE) -C $$d production; then \
	    break; \
	  fi \
	done

opt: subs-opt $(PARSESRC_CMD_OPT)

subs-opt:
	for d in $(SUBS); do \
	  if ! $(MAKE) -C $$d opt; then \
	    break; \
	  fi \
	done

version.ml:
	./genver.sh

version.cmo: version.ml
version.cmx: version.ml

# parsesrc command line driver
$(PARSESRC_CMD): $(OTHER_OBJS_DEP) $(PARSESRC_OBJS) 
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS_P) $(PARSESRC_OBJS) -linkpkg

$(PARSESRC_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(PARSESRC_OBJS_OPT) 
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_P_OPT) $(PARSESRC_OBJS_OPT) -linkpkg

debug: subs-debug $(PARSESRC_CMD)

subs-debug: 
	for d in $(SUBS); do \
	  if ! $(MAKE) -C $$d debug; then \
	    break; \
	  fi \
	done

langs-clean:
	for d in $(LANG_DIRS); do \
	  $(MAKE) -C $$d clean; \
	done

clean:
	for d in $(SUBS); do \
	  $(MAKE) -C $$d clean; \
	done
	$(RM) version.ml
	$(RM) *.cm* *.o *.a *~
	$(RM) $(PARSESRC_CMD) $(PARSESRC_CMD_OPT) 
	$(RM) -r $(DEP)

distclean: clean
	for d in $(LANG_DIRS); do \
	  $(MAKE) -C $$d distclean; \
	done


ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OTHER_OBJS_DEP:%.cmo=$(DEP)/%.d) $(PARSESRC_OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
