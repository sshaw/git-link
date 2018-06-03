.PHONEY: all build test clean

emacs ?= emacs

all: test

build: clean
	$(emacs) -Q --batch -L . -f batch-byte-compile git-link.el

test: build
	$(emacs) -Q --batch -L . -l ert -l git-link-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f git-link.elc
