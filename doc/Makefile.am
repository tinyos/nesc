AUTOMAKE_OPTIONS = foreign

nccdatadir=$(datadir)/ncc
nccdocdir=$(nccdatadir)/doc
userdocdir=$(nccdocdir)/user
dumpdocdir=$(nccdocdir)/dump
dbgdocdir=$(nccdocdir)/debugging_files


dist_man_MANS = nescc.1 nescc-mig.1 nescc-ncg.1 nescc-wiring.1

nccdoc_DATA = 					\
	COPYING-DOC				\
	COPYRIGHT				\
	INDEX					\
	OVERVIEW				\
	../README				\
	mica-debugging.html			\
	envtarget.html				\
	nesc-debugging.html			\
	ref.pdf

dbgdoc_DATA =					\
	$(wildcard debugging_files/*.jpg)

dumpdoc_DATA =					\
	$(wildcard dump/*.dsd)

userdoc_DATA=					\
	$(wildcard user/*.txt)

EXTRA_DIST = 					\
	ref.tex					\
	$(nccdoc_DATA)				\
	$(userdoc_DATA)				\
	$(dumpdoc_DATA)				\
	$(dbgdoc_DATA)

update-ref: 
	pdflatex ref.tex
	bibtex ref
	pdflatex ref.tex
	pdflatex ref.tex
