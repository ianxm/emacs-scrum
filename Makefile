emacs ?= emacs
# EMACS = emacs-24.3

LOAD = -l elpa.el -l org-scrum.el
RM ?= rm -f

.PHONY: all clean checkdoc

all: compile checkdoc

deps:
	$(emacs) -batch -l targets/install-deps.el

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -L . -f batch-byte-compile org-scrum.el

run:
	$(emacs) -Q -l targets/scrum-init.el

clean:
	$(RM) *.elc
