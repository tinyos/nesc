/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
   Copyright (C) 2002 Intel Corporation

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#ifndef NCONFIG_H
#define NCONFIG_H

/* autoconf/configure options */
#include "autoconf.h"
#undef PACKAGE
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_BUGREPORT
#undef PACKAGE_VERSION

/* Target type structure information.
   This setup assumes that we are compiling on self, with gcc 
*/

#include <limits.h>
#define BITSPERBYTE CHAR_BIT

/* Largest signed and unsigned int types (for constants and constant folding) */
typedef long long largest_int;
typedef unsigned long long largest_uint;

#define LARGEST_UINTBITS (SIZEOF_LONG_LONG * BITSPERBYTE)

/* Miscellaneous config */

typedef unsigned char bool;

#include "cstring.h"

#ifndef NULL
#define NULL ((void *)0)
#endif

#define TRUE 1
#define FALSE 0

#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifndef HAVE_ALLOCA
#ifdef __GNUC__
#define alloca __builtin_alloca
void *alloca(size_t n);
#else
#error need alloca
#endif
#endif
#endif

#if __GNUC__ >= 3 && __GNUC_MINOR__ >= 1
#define USE_UNNAMED_UNION
#endif

/* The number of non-significant low-order bits in pointer addresses
   (typically log2 of the alignment of allocations) */
#define ALIGNMENT_BITS 3

#ifdef WIN32
#define DIR_SEPARATOR '\\'
#endif

#endif
