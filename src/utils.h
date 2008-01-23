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

#ifndef UTILS_H
#define UTILS_H

#include "array.h"
#include "cstring.h"

void renew_region(region *r);
/* Effects: Delete region *r (if not null), allocate a new region in *r */

void *xmalloc(size_t size);
void *xrealloc(void *p, size_t newsize);

unsigned long align_to(unsigned long n, unsigned long alignment);

int ilog2(largest_uint x);

/* least common multiple */
unsigned long lcm(unsigned long x, unsigned long y);

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

DECLARE_ARRAY(wchar_array, wchar_t)
DECLARE_ARRAY(char_array, char)

int wcs_mb_size(const wchar_t *wstr);
/* Returns: number of bytes to be allocated for a C string buffer
     that will successfully hold the result of wcstombs(buffer, wstr, ?),
     or -1 if wstr cannot be converted
*/

/* Some basic hash functions, that are re-used in a number of places */
unsigned long hash_ptr(void *p);
int compare_ptr(void *entry1, void *entry2);

unsigned long hash_str(const char *);

#if !HAVE_REALPATH
char *realpath(const char *path, char *resolved_path);
#endif

/* From libiberty */
extern const char *lbasename (const char *);

/* On machines with DIR_SEPARATOR defined, replace all DIR_SEPARATOR's
   by / */
void unixify_path(char *path);

/* TRUE if path is absolute, false otherwise */
bool absolute_path(char *path);

#ifdef WIN32
#define DEVNULL "nul:"
#else
#define DEVNULL "/dev/null"
#endif

#endif
