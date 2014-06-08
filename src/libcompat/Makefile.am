## Process this file with automake to produce Makefile.in	-*- makefile -*-

#    This file is part of the RC compiler.
#    Copyright (C) 2000-2001 The Regents of the University of California.
# 
# RC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# RC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with nesC; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
# Boston, MA 02110-1301 USA.

AM_CPPFLAGS = -DNMEMDEBUG -DNDEBUG -O2

noinst_LIBRARIES = libregions.a

noinst_HEADERS =				\
	alloc.c					\
	pages.c					\
	stats.c

if NEED_FNMATCH
AM_CPPFLAGS += -Ifnmatch
FNMATCH = fnmatch.c fnmatch/fnmatch.h
endif

if NEED_REGEX
AM_CPPFLAGS += -Iregex
REGEX = regex.c regex/regex.h
endif

libregions_a_SOURCES =				\
    regions.c					\
    regions.h					\
    $(FNMATCH)					\
    $(REGEX)

