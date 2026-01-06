.PHONY: test install clean setup

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -l ert -l tests/scientific-mapping-tests.el -f ert-run-tests-batch-and-exit

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