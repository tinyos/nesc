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

INCLUDES = $(regionflags) -I$(srcdir)/$(regiondir)

noinst_LIBRARIES = libparser.a
noinst_PROGRAMS = nesc1

libregions = $(regiondir)/libregions.a

libparser_a_SOURCES =				\
	AST.c					\
	AST.h					\
	AST_defs.h				\
	AST_types.h				\
	AST_utils.c				\
	AST_utils.h				\
	array.h					\
	array.c					\
	builtins.h				\
	builtins.c				\
	c-gperf.h				\
	c-lex.c					\
	c-lex.h					\
	c-parse.h				\
	c-parse.tab.h				\
	c-parse.tab.c				\
	config.h				\
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
	graph.c					\
	graph.h					\
	input.c					\
	input.h					\
	machine.c				\
	machine.h				\
	nesc-c.c				\
	nesc-c.h				\
	nesc-cg.c				\
	nesc-cg.h				\
	nesc-component.c			\
	nesc-component.h			\
	nesc-configuration.c			\
	nesc-configuration.h			\
	nesc-cpp.c				\
	nesc-cpp.h				\
	nesc-decls.h				\
	nesc-env.c				\
	nesc-env.h				\
	nesc-generate.c				\
	nesc-generate.h				\
	nesc-interface.c			\
	nesc-interface.h			\
	nesc-keywords.h				\
	nesc-main.c				\
	nesc-main.h				\
	nesc-module.c				\
	nesc-module.h				\
	nesc-msg.c				\
	nesc-msg.h				\
	nesc-paths.c				\
	nesc-paths.h				\
	nesc-semantics.c			\
	nesc-semantics.h			\
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
	utils.h

nesc1_SOURCES = toplev.c
nesc1_LDADD = libparser.a $(libregions) -lm

BUILT_SOURCES =					\
	AST_defs.c				\
	AST_defs.h				\
	AST_parent.c				\
	AST_print.c				\
	AST_types.h				\
	c-gperf.h				\
	c-parse.tab.c				\
	c-parse.tab.h

EXTRA_DIST = 					\
	build-basics.el				\
	build-parent.el				\
	build-print.el				\
	build-types.el				\
	c-parse.gperf				\
	c-parse.y				\
	nodetypes.def				\
	AST_defs.c				\
	AST_parent.c				\
	AST_print.c				\
	machine/self.c				\
	machine/avr.c				\
	tdspecs					\
	nesc-compile				\
	.gdbinit

BISON = bison
EMACS = emacs
M4 = m4
GPERF = gperf

$(srcdir)/c-parse.tab.c $(srcdir)/c-parse.tab.h: c-parse.y
	$(BISON) $(BISONFLAGS) -dv $<

$(srcdir)/AST_defs.c $(srcdir)/AST_defs.h $(srcdir)/AST_types.h: build-types.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-types.el AST nodetypes.def

$(srcdir)/AST_parent.c: build-parent.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-parent.el AST nodetypes.def

$(srcdir)/AST_print.c: build-print.el nodetypes.def build-basics.el
	cd $(srcdir) && $(EMACS) $(EMACSFLAGS) -batch -load build-print.el AST nodetypes.def

$(srcdir)/c-gperf.h: c-parse.gperf qualifiers.h
	cd $(srcdir) && $(M4) $(M4FLAGS) c-parse.gperf | grep -v '^$$' | \
	$(GPERF) $(GPERFFLAGS) -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$$ >$@-
	mv $@- $@

