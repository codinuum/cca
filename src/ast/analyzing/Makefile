#

SRC_DIR = ../..

BIN_DIR=bin

#DIFFAST_LIB = libdiffast.cma

DIFFAST_CMD   = $(BIN_DIR)/diffast.byte
DIFFAST_WS    = $(BIN_DIR)/diffast_ws.byte
PATCHAST_CMD  = $(BIN_DIR)/patchast.byte
PARSESRC_CMD  = $(BIN_DIR)/parsesrc.byte
SIMAST_CMD    = $(BIN_DIR)/simast.byte

GIT_DIFFAST_CMD = $(BIN_DIR)/ogit.byte

LANGS_DIR = langs
#LANGS = $(shell ls $(LANGS_DIR))
LANGS = astml python java verilog fortran cpp
#LANGS = astml java cpp

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


_DIFFAST_CMD_OBJS = diffast.cmo
_DIFFAST_WS_OBJS  = diffast_ws.cmo netplex.cmo
_PATCHAST_OBJS    = patchast.cmo
_PARSESRC_OBJS    = parsesrc.cmo
_SIMAST_CMD_OBJS  = simast.cmo

_GIT_DIFFAST_OBJS  = git_storage.cmo git_diffast.cmo

DIFFAST_CMD_OBJS = version.cmo astcore.cmo diffastcore.cmo $(_DIFFAST_CMD_OBJS)
DIFFAST_WS_OBJS  = version.cmo astcore.cmo diffastcore.cmo $(_DIFFAST_WS_OBJS)
PATCHAST_OBJS    = version.cmo astcore.cmo diffastcore.cmo $(_PATCHAST_OBJS)
PARSESRC_OBJS    = version.cmo astcore.cmo $(_PARSESRC_OBJS)
SIMAST_CMD_OBJS  = version.cmo astcore.cmo diffastcore.cmo $(_SIMAST_CMD_OBJS)

GIT_DIFFAST_OBJS = version.cmo astcore.cmo diffastcore.cmo $(_GIT_DIFFAST_OBJS)



OBJS_DEP = version.cmo astcore.cmo diffastcore.cmo \
		$(_DIFFAST_CMD_OBJS) $(_DIFFAST_WS_OBJS) \
		$(_PATCHAST_OBJS) $(_GIT_DIFFAST_OBJS) $(_PARSESRC_OBJS) $(_SIMAST_OBJS)

OTHER_OBJS = $(UTIL_OBJS) $(OTREEDIFF_LIB) $(MLDIFF_OBJS) \
		$(COMMON_OBJS) $(ENGINE_OBJS) $(LANGS_COMMON_OBJS) $(LANG_OBJS)

OTHER_OBJS_P = $(UTIL_OBJS) $(OTREEDIFF_LIB) $(MLDIFF_OBJS) \
		$(COMMON_OBJS_P) $(ENGINE_OBJS_P) $(LANGS_COMMON_OBJS) $(LANG_OBJS)

OTHER_OBJS_DEP = $(UTIL_OBJS_DEP) \
		$(OTREEDIFF_OBJS_DEP) \
		$(COMMON_OBJS_DEP) \
		$(ENGINE_OBJS_DEP) \
		$(LANG_COMMON_OBJS_DEP) \
		$(LANG_OBJS_DEP) 

#DIFFAST_LIB_OPT = $(DIFFAST_LIB:.cma=.cmxa)

DIFFAST_CMD_OPT   = $(DIFFAST_CMD:.byte=.opt)
DIFFAST_WS_OPT    = $(DIFFAST_WS:.byte=.opt)
PATCHAST_CMD_OPT  = $(PATCHAST_CMD:.byte=.opt)
PARSESRC_CMD_OPT  = $(PARSESRC_CMD:.byte=.opt)
SIMAST_CMD_OPT    = $(SIMAST_CMD:.byte=.opt)

GIT_DIFFAST_CMD_OPT = $(GIT_DIFFAST_CMD:.byte=.opt)

DIFFAST_CMD_OBJS_OPT = $(DIFFAST_CMD_OBJS:.cmo=.cmx)
DIFFAST_WS_OBJS_OPT  = $(DIFFAST_WS_OBJS:.cmo=.cmx)
PATCHAST_OBJS_OPT    = $(PATCHAST_OBJS:.cmo=.cmx)
PARSESRC_OBJS_OPT    = $(PARSESRC_OBJS:.cmo=.cmx)
SIMAST_CMD_OBJS_OPT  = $(SIMAST_CMD_OBJS:.cmo=.cmx)

GIT_DIFFAST_OBJS_OPT   = $(GIT_DIFFAST_OBJS:.cmo=.cmx)

OTHER_OBJS_OPT     = $(OTHER_OBJS:.cmo=.cmx)
OTHER_OBJS_P_OPT   = $(OTHER_OBJS_P:.cmo=.cmx)
OTHER_OBJS_DEP_OPT = $(OTHER_OBJS_DEP:.cmo=.cmx)

PACKAGES = -package pxp,ulex,str,csv,bigarray,num,zip,menhirLib
PACKAGES += -package cryptokit,netclient
PACKAGES += -package netzip,uuidm,dynlink,volt

PACKAGES_WS = $(PACKAGES) -package pcre,sqlite3,netcgi2,netclient,nethttpd

PACKAGES_GIT_DIFFAST = $(PACKAGES) -package lwt.unix,cmdliner,mtime.clock.os,logs.cli,logs.fmt,fmt.cli,fmt.tty,checkseum.ocaml,git,digestif.ocaml,git-unix

DEP_INCLS = -I $(COMMON_DIR) -I $(ENGINE_DIR) -I .

SYNTAX = -syntax camlp4o

include $(SRC_DIR)/rules.mk


.PHONY: subs-production subs-opt debug subs-debug

all: opt

production: SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
production: subs-production $(DIFFAST_CMD_OPT) git-production $(PATCHAST_CMD_OPT) $(PARSESRC_CMD_OPT) $(SIMAST_CMD_OPT)

subs-production:
	for d in $(SUBS); do \
	  if ! $(MAKE) -C $$d production; then \
	    break; \
	  fi \
	done

git-production: subs-production
git-production: PACKAGES := $(PACKAGES_GIT_DIFFAST) 
git-production: $(GIT_DIFFAST_CMD_OPT) 


opt: subs-opt $(DIFFAST_CMD_OPT) git-opt $(PATCHAST_CMD_OPT) $(PARSESRC_CMD_OPT) $(SIMAST_CMD_OPT)

ws-opt: subs-opt
ws-opt: PACKAGES := $(PACKAGES_WS)
ws-opt: $(DIFFAST_WS_OPT)

git-opt: subs-opt
git-opt: PACKAGES := $(PACKAGES_GIT_DIFFAST) 
git-opt: $(GIT_DIFFAST_CMD_OPT) 


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

# diffast command line driver
$(DIFFAST_CMD): $(OTHER_OBJS_DEP) $(DIFFAST_CMD_OBJS) version.cmo
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(DIFFAST_CMD_OBJS) -linkpkg

$(DIFFAST_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(DIFFAST_CMD_OBJS_OPT) version.cmx
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(DIFFAST_CMD_OBJS_OPT) -linkpkg -thread

# diffast web service driver
$(DIFFAST_WS): $(OTHER_OBJS_DEP) $(DIFFAST_WS_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(DIFFAST_WS_OBJS) -linkpkg

$(DIFFAST_WS_OPT): $(OTHER_OBJS_DEP_OPT) $(DIFFAST_WS_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(DIFFAST_WS_OBJS_OPT) -linkpkg

# patchast command line driver
$(PATCHAST_CMD): $(OTHER_OBJS_DEP) $(PATCHAST_OBJS) version.cmo
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(PATCHAST_OBJS) -linkpkg

$(PATCHAST_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(PATCHAST_OBJS_OPT) version.cmx
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(PATCHAST_OBJS_OPT) -linkpkg -thread

# parsesrc command line driver
$(PARSESRC_CMD): $(OTHER_OBJS_DEP) $(PARSESRC_OBJS) version.cmo
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS_P) $(PARSESRC_OBJS) -linkpkg

$(PARSESRC_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(PARSESRC_OBJS_OPT) version.cmx
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_P_OPT) $(PARSESRC_OBJS_OPT) -linkpkg

# simast command line driver
$(SIMAST_CMD): $(OTHER_OBJS_DEP) $(SIMAST_CMD_OBJS) version.cmo
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(SIMAST_CMD_OBJS) -linkpkg

$(SIMAST_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(SIMAST_CMD_OBJS_OPT) version.cmx
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(SIMAST_CMD_OBJS_OPT) -linkpkg


# git_diffast command line driver
$(GIT_DIFFAST_CMD): $(OTHER_OBJS_DEP) $(GIT_DIFFAST_OBJS)
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(LINKFLAGS) -o $@ \
	$(OTHER_OBJS) $(GIT_DIFFAST_OBJS) -linkpkg

$(GIT_DIFFAST_CMD_OPT): $(OTHER_OBJS_DEP_OPT) $(GIT_DIFFAST_OBJS_OPT)
	$(OCAMLFIND) ocamlopt $(PACKAGES) $(LINKFLAGS_OPT) $(PROFFLAGS) -o $@ \
	$(OTHER_OBJS_OPT) $(GIT_DIFFAST_OBJS_OPT) -linkpkg -thread



#lib: $(DIFFAST_LIB_OPT)

#$(DIFFAST_LIB_OPT):
#	$(OCAMLFIND) ocamlopt $(LINKFLAGS_OPT) -a -o $(DIFFAST_LIB_OPT) \
#	$(OTHER_OBJS_OPT) version.cmo astcore.cmo

debug: subs-debug $(DIFFAST_CMD) $(GIT_DIFFAST_CMD) $(PATCHAST_CMD) $(PARSESRC_CMD) $(SIMAST_CMD) #$(DIFFAST_WS)

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
	$(RM) $(DIFFAST_CMD) $(DIFFAST_CMD_OPT)
	$(RM) $(DIFFAST_WS) $(DIFFAST_WS_OPT)
	$(RM) $(PATCHAST_CMD) $(PATCHAST_CMD_OPT)
	$(RM) $(PARSESRC_CMD) $(PARSESRC_CMD_OPT)
	$(RM) $(SIMAST_CMD) $(SIMAST_CMD_OPT)
	$(RM) $(GIT_DIFFAST_CMD) $(GIT_DIFFAST_CMD_OPT)
	$(RM) -r $(DEP)

distclean: clean
	for d in $(LANG_DIRS); do \
	  $(MAKE) -C $$d distclean; \
	done


ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(OBJS_DEP:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di)
endif
endif
