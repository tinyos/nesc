AUTOMAKE_OPTIONS = foreign

EXTRA_DIST =					\
	debugging.html				\
	debugging_files/complete-setup.jpg	\
	debugging_files/jtagpod.jpg		\
	debugging_files/micamod.jpg		\
	debugging_files/progboard.jpg		\
	envtarget.html				\
	mig.html				\
	ncc.html                                \
	ncg.html                                \
	nesdoc.html                             \
	ref.pdf					\
	user/generics-1.2.txt

ref.pdf: ref.tex
	pdflatex ref.tex
	bibtex ref
	pdflatex ref.tex
	pdflatex ref.tex
