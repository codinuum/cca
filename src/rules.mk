#

#STRIP_INFO_LOGGERS = true
#MULTI_THREAD = true
#NO_DYNLINK = true

###

LN = ln -fs

#CAML_PREFIX = /usr/bin/
CAML_PREFIX =

OCAMLFIND = $(CAML_PREFIX)ocamlfind
OCAMLC    = $(CAML_PREFIX)ocamlc.opt
OCAMLCP   = $(CAML_PREFIX)ocamlcp
OCAMLYACC = $(CAML_PREFIX)ocamlyacc
OCAMLLEX  = $(CAML_PREFIX)ocamllex.opt
OCAMLDEP  = $(CAML_PREFIX)ocamldep.opt
OCAMLOPT  = $(CAML_PREFIX)ocamlopt.opt
MENHIR    = $(CAML_PREFIX)menhir

ifdef USE_MENHIR
PARSER_GENERATOR = $(MENHIR) $(MENHIRFLAGS)
PARSER_DEP = $(PARSER_GENERATOR) --depend
else
PARSER_GENERATOR = $(OCAMLYACC)
endif

ifdef STRIP_INFO_LOGGERS
ifneq (,$(findstring volt,$(PACKAGES)))
SYNTAX := $(SYNTAX) -ppopt -level -ppopt WARN
endif
endif

ifdef STRIP_DEBUG_LOGGERS
ifneq (,$(findstring volt,$(PACKAGES)))
SYNTAX := $(SYNTAX) -ppopt -level -ppopt INFO
endif
endif

ifdef STRIP_ALL_LOGGERS
ifneq (,$(findstring volt,$(PACKAGES)))
SYNTAX := $(SYNTAX) -ppopt -level -ppopt NONE
endif
endif

ifdef MULTI_THREAD
COMPFLAGS := $(COMPFLAGS) -thread
COMPFLAGS_OPT := $(COMPFLAGS_OPT) -thread
LINKFLAGS := $(LINKFLAGS) -thread
LINKFLAGS_OPT := $(LINKFLAGS_OPT) -thread
endif

COMPFLAGS_OPT := $(COMPFLAGS_OPT) -O3

# ifdef NO_DYNLINK
# COMPFLAGS_OPT := $(COMPFLAGS_OPT) -nodynlink
# LINKFLAGS_OPT := $(LINKFLAGS_OPT) -nodynlink
# endif

PROFFLAGS = -g
#PROFFLAGS = -p -g # uncomment this to build for profiling

OCAMLC_MODE = ocamlc
#OCAMLC_MODE = ocamlcp # does not work yet

OCAMLOPT_MODE = ocamlopt
#OCAMLOPT_MODE = ocamloptp

.PHONY: production all opt debug clean distclean

.SECONDARY: $(MLL:.mll=.ml) $(MLY:.mly=.ml) $(MLY:.mly=.mli)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .d .di

DEP = .dep

ifdef USE_MENHIR
$(DEP)/%.d: %.mly
	@[ -d $(@D) ] || mkdir -p $(@D)
	$(PARSER_DEP) $< > $@
endif

$(DEP)/%.d: %.ml
	@[ -d $(@D) ] || mkdir -p $(@D)
	$(OCAMLFIND) ocamldep $(DEP_INCLS) $(PACKAGES) $(SYNTAX) $< > $@

$(DEP)/%.di: %.mli
	@[ -d $(@D) ] || mkdir -p $(@D)
	$(OCAMLFIND) ocamldep $(DEP_INCLS) $(PACKAGES) $(SYNTAX) -native $< > $@


%.ml %.mli: %.mly
	$(PARSER_GENERATOR) -v $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC_MODE) $(PACKAGES) $(SYNTAX) $(COMPFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT_MODE) $(PACKAGES) $(SYNTAX) $(COMPFLAGS_OPT) $(PROFFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLOPT_MODE) $(PACKAGES) $(COMPFLAGS_OPT) $<
