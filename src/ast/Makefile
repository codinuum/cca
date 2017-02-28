#

SUBS = analyzing

.PHONY: subs debug clean distclean

all: subs

subs:
	for d in $(SUBS); do \
	  if ! $(MAKE) -k -C $$d; then \
	    break; \
	  fi \
	done

debug:
	for d in $(SUBS); do \
	  if ! $(MAKE) -k -C $$d debug; then \
	    break; \
	  fi \
	done

clean:
	$(RM) *~
	for d in $(SUBS); do \
	  $(MAKE) -k -C $$d clean; \
	done

distclean: clean
	$(MAKE) -k -C analyzing distclean
