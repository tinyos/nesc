AUTOMAKE_OPTIONS = foreign

dist_man_MANS = nescc.1 nescc-mig.1 nescc-ncg.1

EXTRA_DIST =					\
	COPYING-DOC				\
	COPYRIGHT				\
	debugging.html				\
	$(wildcard debugging_files/*.jpg)	\
	$(wildcard dump/*.dsd)			\
	envtarget.html				\
	nesc-debugging.html			\
	ref.pdf					\
	$(wildcard user/*.txt)			\

ref.pdf: ref.tex
	pdflatex ref.tex
	bibtex ref
	pdflatex ref.tex
	pdflatex ref.tex
