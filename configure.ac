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
# the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
# Boston, MA 02110-1301 USA.

dnl -*- m4 -*-

# force autoconf 2.5 on Debian systems
AC_PREREQ(2.50)

AC_INIT(nesc, 1.4.0)
AC_CONFIG_AUX_DIR(config-aux)

AM_INIT_AUTOMAKE

AC_CONFIG_SUBDIRS(src libiberty libcpp)

AC_PROG_CC

AC_PATH_PROG(pathperl, perl)
if test -z "$pathperl" ; then
  AC_MSG_ERROR(I can't find perl);
fi

# Only check if emacs, flex, bison and gperf are installed if
# there is a Bootstrap file present.
AC_CHECK_FILE([Bootstrap])
if test "$ac_cv_file_Bootstrap" == yes ; then
  AM_PATH_LISPDIR
  AS_IF([test "$EMACS" = no], [AC_MSG_ERROR([cannot find emacs])])

  AC_PATH_PROG(pathgperf, gperf, [no])
  AS_IF([test "$pathgperf" = no], [AC_MSG_ERROR([cannot find gperf])])

  AC_PATH_PROG(pathbison, bison, [no])
  AS_IF([test "$pathbison" = no], [AC_MSG_ERROR([cannot find bison])])

  AC_PATH_PROG(pathflex, flex, [no])
  AS_IF([test "$pathflex" = no], [AC_MSG_ERROR([cannot find flex])])
fi

AC_OUTPUT(
	Makefile
	doc/Makefile
	tools/Makefile
	tools/nescc-mig
	tools/nescc-ncg
	tools/nescc
	tools/nescc-diff
	tools/nescc-wiring
	tools/java/Makefile
	tools/java/net/Makefile
	tools/java/net/tinyos/Makefile
	tools/java/net/tinyos/nesc/Makefile
	tools/java/net/tinyos/nesc/wiring/Makefile
	tools/java/net/tinyos/nesc/dump/Makefile
	tools/java/net/tinyos/nesc/dump/xml/Makefile
)
