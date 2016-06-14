# Use $EMACS environment variable if present, so that all of these are
# equivalent:
#
# 1.  export EMACS=/path/to/emacs && make
# 2.  EMACS=/path/to/emacs make
# 3.  make EMACS=/path/to/emacs
#
# This is particularly useful when EMACS is set in ~/.bash_profile
#
EMACS := $(shell which "$${EMACS}" 2> /dev/null || which "emacs")
EFLAGS =-q -no-site-file -no-init-file -batch -f batch-byte-compile
BATCH = $(EMACS) $(EFLAGS)

SOURCE=yasnippet.el
COMPILED=yasnippet.elc

all: $(COMPILED)

.el.elc:
	$(BATCH) $<

.PHONY: clean

clean:
	rm -f $(COMPILED)
