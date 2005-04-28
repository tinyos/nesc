AUTOMAKE_OPTIONS = foreign

dist_man_MANS = nescc.1 nescc-mig.1 nescc-ncg.1

EXTRA_DIST =					\
	debugging.html				\
	debugging_files/complete-setup.jpg	\
	debugging_files/jtagpod.jpg		\
	debugging_files/micamod.jpg		\
	debugging_files/progboard.jpg		\
	envtarget.html				\
	user/binary-components.txt		\
	user/generics-1.2.txt			\
	user/network-types.txt			\
	ref.pdf

ref.pdf: ref.tex
	pdflatex ref.tex
	bibtex ref
	pdflatex ref.tex
	pdflatex ref.tex
