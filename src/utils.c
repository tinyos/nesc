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

#include "parser.h"
#include "utils.h"

void *xmalloc(size_t size)
{
#ifdef BWGC
  void *x = GC_malloc(size);
#else
  void *x = malloc(size);
#endif

  if (!x) abort();

  return x;
}

void *xrealloc(void *p, size_t newsize)
{
#ifdef BWGC
  void *x = GC_realloc(p, newsize);
#else
  void *x = realloc(p, newsize);
#endif

  if (!x) abort();

  return x;
}

/* Make a new cstring with a copy of s, length l */
cstring make_cstring(region r, const char *s, int l)
{
  cstring cs;

  cs.data = rstralloc(r, l + 1);
  memcpy(cs.data, s, l);
  cs.data[l] = '\0';
  cs.length = l;

  return cs;
}

/* Make a new cstring with a copy of regular C string s */
cstring str2cstring(region r, const char *s)
{
  return make_cstring(r, s, strlen(s));
}

unsigned long align_to(unsigned long n, unsigned long alignment)
{
  int count = (n + alignment - 1) / alignment;

  return count * alignment;
}

unsigned long gcd(unsigned long x, unsigned long y)
{
  unsigned long z;

  for (;;)
    {
      if (y == 0)
	return x;
      
      z = x % y; x = y; y = z;
    }
}

unsigned long lcm(unsigned long x, unsigned long y)
{
  /* ignoring risk of overflow (used for alignments which are typically <= 16) */
  return (x * y) / gcd(x, y); 
}

DEFINE_ARRAY(wchar_array, wchar_t)
