
SRC_DIR = ..

PACKAGES = -package unix,num,camlzip,cryptokit,netzip,netstring,pxp,volt

SYNTAX = -syntax camlp4o

STRIP_INFO_LOGGERS = true

include $(SRC_DIR)/rules.mk

OBJS = xthread.cmo xset.cmo xstring.cmo xlist.cmo xarray.cmo xqueue.cmo xoption.cmo \
	xprint.cmo xfile.cmo xhash.cmo LCS.cmo weight.cmo HCS.cmo HIS.cmo SMP.cmo \
	compression.cmo xchannel.cmo XML.cmo inst.cmo LLL.cmo

OBJS_OPT = $(OBJS:.cmo=.cmx)

OBJS_TEST = test_SMP.cmo test_HCS.cmo test_HIS.cmo test_XML.cmo

OBJS_TEST_OPT = $(OBJS_TEST:.cmo=.cmx)

TESTS = test_SMP test_HCS test_HIS test_XML

all: opt

opt: $(OBJS_OPT)

tests: $(TESTS)

test_SMP: xlist.cmx xprint.cmx weight.cmx SMP.cmx test_SMP.cmx
	$(OCAMLFIND) ocamlopt -o $@ $^ -package volt -linkpkg

test_HCS: xlist.cmx weight.cmx HCS.cmx test_HCS.cmx
	$(OCAMLFIND) ocamlopt -o $@ $^ -package volt -linkpkg

test_HIS: xlist.cmx weight.cmx HIS.cmx test_HIS.cmx
	$(OCAMLFIND) ocamlopt -o $@ $^ -package volt -linkpkg

test_XML: xprint.cmx xfile.cmx compression.cmx xchannel.cmx XML.cmx test_XML.cmx
	$(OCAMLFIND) ocamlopt -o $@ $^ -package pxp,volt,netzip -linkpkg

debug: $(OBJS)

clean:
	$(RM) *~ *.cm* *.o 
	$(RM) -r $(DEP)
	$(RM) $(TESTS)

ifneq ($(MAKECMDGOALS), clean)
-include $(OBJS:%.cmo=$(DEP)/%.d) $(MLI:%.mli=$(DEP)/%.di) $(OBJS_TEST:%.cmo=$(DEP)/%.d)
endif
