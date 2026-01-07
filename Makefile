.PHONY: test test-capture install clean setup

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . -l scientific-mapping --eval '(message "scientific-mapping loaded successfully")'

test-capture:
	$(EMACS) -Q -batch -L . -l scientific-mapping -l scientific-mapping/tests/test-doc-engine-create.el -f ert-run-tests-batch-and-exit

install:
	$(EMACS) -Q -batch -l package -f package-initialize -f package-refresh-contents

setup:
	mkdir -p ~/.emacs.d/scientific-mapping
	cp -r * ~/.emacs.d/scientific-mapping/

clean:
	rm -rf *.elc
	rm -rf test-data/
	find . -name "*.elc" -delete

all: clean test

