.PHONY: all clean

all: cl-forms.info cl-forms.html index.html cl-forms.pdf ../README.md

cl-forms.info: cl-forms.texi
	makeinfo --no-validate cl-forms.texi

cl-forms.pdf: cl-forms.texi
	texi2any --force --no-validate --pdf cl-forms.texi -o cl-forms.pdf

cl-forms.html: cl-forms.texi
	texi2any --no-validate --html cl-forms.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o cl-forms.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp cl-forms.html

index.html: cl-forms.texi
	texi2any --no-validate --html cl-forms.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o index.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp index.html

cl-forms.texi:
	sbcl --eval '(ql:quickload :cl-forms)' --eval '(ql:quickload :msgraph)' --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-forms.source.texi" "cl-forms.texi" :docsystem :texinfo)' --quit

cl-forms-internals.info: cl-forms-internals.weaved.texi
	makeinfo --no-validate cl-forms-internals.weaved.texi

cl-forms-internals.pdf: cl-forms-internals.weaved.texi
	texi2any --force --no-validate --pdf cl-forms-internals.weaved.texi -o cl-forms-internals.pdf

cl-forms-internals.html: cl-forms-internals.weaved.texi
	texi2any --no-validate --html cl-forms-internals.weaved.texi --css-ref texinfo-klare.css --no-split -o cl-forms-internals.html

cl-forms-internals.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-forms-internals.texi" "cl-forms-internals.weaved.texi" :docsystem :texinfo)' --quit

clean:
	rm -f *.pdf
	rm -f *.html
	rm cl-forms.texi
