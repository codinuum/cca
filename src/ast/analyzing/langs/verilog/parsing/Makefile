#

all: opt

production:
	$(MAKE) -C src production

opt:
	$(MAKE) -C src opt

debug:
	$(MAKE) -C src debug

clean:
	$(RM) *~ *.cm* *.o *.a
	$(MAKE) -C src clean

distclean: clean
	$(MAKE) -C src distclean
