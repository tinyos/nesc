#
#	avarice - The "avarice" program.
#	Copyright (C) 2001 Scott Finneran
#      Copyright (C) 2002 Intel Corporation
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License Version 2
#       as published by the Free Software Foundation.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.
#
#
AUTOMAKE_OPTIONS = foreign

bin_PROGRAMS = avarice

bin_SCRIPTS = start-avarice ice-gdb

pkgdata_DATA = gdb-avarice-script

avarice_SOURCES =	\
	avarice.h	\
	jtag.h		\
	jtagbp.cc	\
	jtagio.cc	\
	jtagmisc.cc	\
	jtagprog.cc	\
	jtagrun.cc	\
	jtagrw.cc	\
	main.cc		\
	remote.cc	\
	remote.h	\
	utils.cc

EXTRA_DIST =		\
	COPYING		\
	gdb-avarice-script \
	start-avarice
