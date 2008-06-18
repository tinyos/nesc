# This file is part of the nesC compiler.
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

SUBDIRS = java

ncclibdir=$(libdir)/ncc
nccdatadir=$(datadir)/ncc
editordir=$(nccdatadir)/editor-modes
emacsdir=$(editordir)/emacs
vimdir=$(editordir)/vim
kdedir=$(editordir)/kde
geditdir=$(editordir)/gedit

bin_SCRIPTS = nescc-mig nescc-ncg nescc nescc-wiring

ncclib_DATA = deputy_nodeputy.h			\
	      deputy_stage1.h			\
	      deputy_stage2.h			\
	      genjava.pm 			\
	      gencstjava.pm 			\
	      gencsharp.pm			\
	      genc.pm				\
	      gencstc.pm			\
	      genpython.pm			\
	      gencstpython.pm			\
	      migdecode.pm			\
	      nesc_nx.h

nodist_ncclib_DATA = nesc.jar

emacs_DATA =					\
	editor-modes/emacs/nesc.el 		\
	editor-modes/emacs/new-nesc.el		\
	editor-modes/emacs/old-nesc.el		\
	editor-modes/emacs/readme.txt
vim_DATA =					\
	editor-modes/vim/nesc.vim		\
	editor-modes/vim/filetype.vim		\
        editor-modes/emacs/readme.txt
kde_DATA = 					\
	editor-modes/kde/nesc.xml		\
	editor-modes/kde/readme.txt
gedit_DATA = 					\
	editor-modes/gedit/ncc.lang		\
	editor-modes/gedit/readme.txt

EXTRA_DIST =					\
	$(ncclib_DATA)				\
	$(emacs_DATA)				\
	$(vim_DATA)				\
	$(kde_DATA)				\
	$(gedit_DATA)

NESC_JAR_DEPS = $(shell find java -name '*.java')

nesc.jar: $(NESC_JAR_DEPS)
	(cd java && $(MAKE))
	(cd java && jar cf ../$@ `find . -name '*.class'`)

FORCE:

.phony: FORCE
