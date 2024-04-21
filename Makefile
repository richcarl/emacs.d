all:
	emacs -batch -f batch-byte-compile *.el lisp/*.el site-lisp/*.el

clean:
	rm *.elc  lisp/*.elc site-lisp/*.elc
