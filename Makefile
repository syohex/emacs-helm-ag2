.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test-util.el \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

.PHONY: clean
clean:
	-rm -f *.elc *~
