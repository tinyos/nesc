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

#include <regions.h>
#include <assert.h>
#include <limits.h>
#include "array.h"

struct array {
  region sameregion r;
  void *sameregion data;
  size_t elemsize;
  type_t elemtype;
  size_t nelems, nalloc;
};

struct array *new_array(region r, size_t initialsize,
			size_t typesize, type_t typeinfo)
{
  struct array *a = ralloc(r, struct array);

  a->r = r;
  a->data = typed_rarrayalloc(r, initialsize, typesize, typeinfo);
  a->elemsize = typesize;
  a->elemtype = typeinfo;
  a->nelems = 0;
  a->nalloc = initialsize;

  return a;
}

void *array_extend(struct array *a, int by)
{
  size_t oldelems = a->nelems;

  if (by < 0)
    assert(-by <= a->nelems && by != INT_MIN);
  else if (a->nelems + by > a->nalloc)
    {
      size_t newsize = a->nalloc * 2 + by;
      void *newdata = typed_rarrayalloc(a->r, newsize, a->elemsize, a->elemtype);

      /* XXX: could work harder to support really large array sizes
	 (this code will fail for a->nalloc >= (max(size_t)-by)/2) */
      assert(newsize > a->nalloc); /* die when we get really big */
      typed_rarraycopy(newdata, a->data, a->nelems, a->elemsize, a->elemtype);
      a->data = newdata;
      a->nalloc = newsize;
    }
  a->nelems += by;

  return (char *)a->data + a->elemsize * oldelems;
}

void array_reset(struct array *a)
{
  a->nelems = 0;
}

size_t array_length(struct array *a)
{
  return a->nelems;
}

void *array_data(struct array *a)
{
  return a->data;
}

