EMACSBIN ?= emacs

all: compile

compile: gitter.elc

gitter.elc: gitter.el
#       Check if let-alist.el is exists or is part of this version of Emacs, if not, download let-alist.el
	@[ -f let-alist.el ] || ${EMACSBIN} -Q --batch --eval '(princ emacs-version)' | grep '^25' || curl http://elpa.gnu.org/packages/let-alist-1.0.4.el -o let-alist.el
	${EMACSBIN} -Q --batch -L . -f batch-byte-compile gitter.el

clean:
	rm -f gitter.elc
