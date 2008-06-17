## Process this file with automake to produce Makefile.in    -*- makefile -*-

# This file is part of the nesC compiler.
# 
# This file is derived from the RC Compiler. It is thus
#    Copyright (C) 2000-2001 The Regents of the University of California.
# Changes for nesC are
#    Copyright (C) 2002 Intel Corporation
# 
# The attached "nesC" software is provided to you under the terms and
# conditions of the GNU General Public License Version 2 as published by the
# Free Software Foundation.
# 
# nesC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with nesC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.


AUTOMAKE_OPTIONS = foreign

if USING_RCC
regiondir = lib
else
SUBDIRS = libcompat
regiondir = libcompat
endif

extra_inc :=

if NEED_REGEX
extra_inc += -Ilibcompat/regex
endif

if NEED_FNMATCH
extra_inc += -Ilibcompat/fnmatch
endif

ncclibdir=$(libdir)/ncc

INCLUDES = $(regionflags) -I$(srcdir)/$(regiondir) -I$(srcdir)/../libcpp/include -I$(srcdir)/../libcpp -I$(srcdir)/../include $(extra_inc)

noinst_LIBRARIES = libparser.a
ncclib_PROGRAMS = nesc1
ncclib_DATA = tdspecs
ncclib_SCRIPTS = nesc-compile

libregions = $(regiondir)/libregions.a

libparser_a_SOURCES =				\
	AST.c					\
	AST.h					\
	AST_list_node.h				\
	AST_types.h				\
	AST_utils.c				\
	AST_utils.h				\
	AST_walk.c				\
	AST_walk.h				\
	ND_list_nd_arg.h			\
	ND_types.h				\
	array.h					\
	array.c					\
	attributes.h				\
	attributes.c				\
	c-gperf.h				\
	c-lex.c					\
	c-lex.h					\
	c-lex-int.h				\
	c-lex-state.h				\
	c-parse.h				\
	c-parse.tab.h				\
	c-parse.tab.c				\
	constants.c				\
	constants.h				\
	cstring.h				\
	cval.h					\
	cval.c					\
	decls.h					\
	dd_list.c				\
	dd_list.h				\
	dhash.c					\
	dhash.h					\
	edit.c					\
	edit.h					\
	env.c					\
	env.h					\
	errors.c				\
	errors.h				\
	expr.c					\
	expr.h					\
	flags.c					\
	flags.h					\
	gcc-cpp.h				\
	graph.c					\
	graph.h					\
	init.c					\
	init.h					\
	lex.nd.c				\
	machine.c				\
	machine.h				\
	nconfig.h				\
	nesc-abstract.c				\
	nesc-abstract.h				\
	nesc-atomic.c				\
	nesc-atomic.h				\
	nesc-attributes.c			\
	nesc-attributes.h			\
	nesc-c.c				\
	nesc-c.h				\
	nesc-cg.c				\
	nesc-cg.h				\
	nesc-component.c			\
	nesc-component.h			\
	nesc-concurrency.c			\
	nesc-concurrency.h			\
	nesc-configuration.c			\
	nesc-configuration.h			\
	nesc-constants.c			\
	nesc-constants.h			\
	nesc-cpp.c				\
	nesc-cpp.h				\
	nesc-decls.h				\
	nesc-deputy.c				\
	nesc-deputy.h				\
	nesc-dfilter.c				\
	nesc-dfilter.h				\
	nesc-doc.c				\
	nesc-doc.h				\
	nesc-dspec.h				\
	nesc-dspec-int.h			\
	nesc-dspec.tab.c			\
	nesc-dspec.tab.h			\
	nesc-dump.c				\
	nesc-dump.h				\
	nesc-env.c				\
	nesc-env.h				\
	nesc-gcc.c				\
	nesc-gcc.h				\
	nesc-generate.c				\
	nesc-generate.h				\
	nesc-inline.c				\
	nesc-inline.h				\
	nesc-interface.c			\
	nesc-interface.h			\
	nesc-keywords.h				\
	nesc-magic.c				\
	nesc-magic.h				\
	nesc-main.c				\
	nesc-main.h				\
	nesc-module.c				\
	nesc-module.h				\
	nesc-msg.c				\
	nesc-msg.h				\
	nesc-ndoc.c				\
	nesc-ndoc.h				\
	nesc-network.c				\
	nesc-network.h				\
	nesc-paths.c				\
	nesc-paths.h				\
	nesc-semantics.c			\
	nesc-semantics.h			\
	nesc-task.c				\
	nesc-task.h				\
	nesc-uses.c				\
	nesc-uses.h				\
	nesc-xml.c				\
	nesc-xml.h				\
	parser.h				\
	qualifiers.h				\
	sd_list.c				\
	sd_list.h				\
	semantics.c				\
	semantics.h				\
	stmt.c					\
	stmt.h					\
	types.c					\
	types.h					\
	unparse.c				\
	unparse.h				\
	utils.c					\
	utils.h					\
	machine/keil-gperf.h			\
	machine/sdcc-gperf.h

# Character encoding conversion library.
LIBICONV = @LIBICONV@
LIBICONV_DEP = @LIBICONV_DEP@

nesc1_SOURCES = toplev.c
nesc1_LDADD = 	libparser.a 			\
		$(libregions)			\
		../libcpp/libcpp.a		\
		../libiberty/libiberty.a	\
		-lm				\
		$(LIBICONV)

BUILT_SOURCES =					\
	AST_defs.h				\
	AST_list_node.c				\
	AST_list_node.h				\
	AST_parent.c				\
	AST_print.c				\
	AST_types.c				\
	AST_types.h				\
	AST_walk_children.c			\
	ND_defs.h				\
	ND_list_nd_arg.c			\
	ND_list_nd_arg.h			\
	ND_types.c				\
	ND_types.h				\
	c-gperf.h				\
	c-parse.tab.c				\
	c-parse.tab.h				\
	lex.nd.c				\
	nesc-dspec.tab.c			\
	nesc-dspec.tab.h			\
	machine/keil-gperf.h			\
	machine/sdcc-gperf.h

EXTRA_DIST = 					\
	build-basics.el				\
	build-list.el				\
	build-parent.el				\
	build-print.el				\
	build-types.el				\
	build-walk.el				\
	c-parse.gperf				\
	c-parse.y				\
	cg_help.c				\
	cg_help.fig				\
	nodetypes.def				\
	AST_defs.h				\
	AST_list_node.c				\
	AST_parent.c				\
	AST_print.c				\
	AST_types.c				\
	AST_walk_children.c			\
	ND_defs.h				\
	ND_list_nd_arg.c			\
	ND_types.c				\
	machine/self.c				\
	machine/avr.c				\
	machine/env_machine.c			\
	machine/msp430.c			\
	machine/keil.c				\
	machine/keil.gperf			\
	machine/sdcc.c				\
	machine/sdcc.gperf			\
	nesc-dspec.def				\
	nesc-dspec.l				\
	nesc-dspec.y				\
	tdspecs					\
	nesc-compile				\
	COPYING					\
	COPYRIGHT				\
	.gdbinit

BISON = bison
FLEX = flex
EMACS = emacs
M4 = m4
GPERF = gperf

$(srcdir)/c-parse.tab.c $(srcdir)/c-parse.tab.h: c-parse.y
	$(BISON) $(BISONFLAGS) -dv $<

$(srcdir)/nesc-dspec.tab.c $(srcdir)/nesc-dspec.tab.h: nesc-dspec.y
	$(BISON) $(BISONFLAGS) -p nd -dv $<

$(srcdir)/lex.nd.c: nesc-dspec.l
	$(FLEX) -Pnd -s $<

$(srcdir)/ND_types.c $(srcdir)/ND_types.h $(srcdir)/ND_defs.h: build-types.el nesc-dspec.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-types.el ND nesc-dspec.def 10000

$(srcdir)/ND_list_nd_arg.c $(srcdir)/ND_list_nd_arg.h: build-list.el nesc-dspec.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-list.el ND nesc-dspec.def nd_arg

$(srcdir)/AST_types.c $(srcdir)/AST_types.h $(srcdir)/AST_defs.h: build-types.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-types.el AST nodetypes.def 42

$(srcdir)/AST_list_node.c $(srcdir)/AST_list_node.h: build-list.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-list.el AST nodetypes.def node

$(srcdir)/AST_parent.c: build-parent.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-parent.el AST nodetypes.def

$(srcdir)/AST_print.c: build-print.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-print.el AST nodetypes.def

$(srcdir)/AST_walk_children.c: build-walk.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-walk.el AST nodetypes.def

$(srcdir)/c-gperf.h: c-parse.gperf qualifiers.h nesc-keywords.h
	cd $(srcdir) && $(M4) $(M4FLAGS) c-parse.gperf | grep -v '^$$' | \
	$(GPERF) $(GPERFFLAGS) -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,8,$$ >$@-
	mv $@- $@

$(srcdir)/machine/sdcc-gperf.h: machine/sdcc.gperf
	$(GPERF) $(GPERFFLAGS) -j1 -t -E -H sdcc_hash -N is_sdcc_word -k1,3 machine/sdcc.gperf >$@-
	mv $@- $@

$(srcdir)/machine/keil-gperf.h: machine/keil.gperf
	$(GPERF) $(GPERFFLAGS) -j1 -t -E -H keil_hash -N is_keil_word -k1,3,$$ machine/keil.gperf >$@-
	mv $@- $@

# Regression testing stuff

testdoc: nesc-ndoc.c nesc-ndoc.h dd_list.c $(libregions)
	 $(COMPILE) -o $@ -DTESTING nesc-ndoc.c dd_list.c $(libregions)

regress: nesc1 testdoc
	 (cd ../nregress && ./runtest)
