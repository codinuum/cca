
include ../rules.mk

OBJS = mldiff.cmo
OBJS_OPT = $(OBJS:.cmo=.cmx)

all: opt

opt: $(OBJS_OPT)

debug: $(OBJS)

clean:
	$(RM) *~ *.cm* *.o