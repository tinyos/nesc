/*
 * Copyright (c) 1999-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */
/* A (growable) hash table */

#include <regions.h>
#include "dhash.h"

struct dhash_table
{
  region sameregion r;
  void **sameregion elements;
  unsigned long size, used;
  int (*traditional compare)(void *key, void *y);
  unsigned long (*traditional hash)(void *x);
};

dhash_table new_dhash_table(region r, unsigned long initial_size,
			    int (*compare)(void *key, void *y),
			    unsigned long (*hash)(void *x))
{
  dhash_table h = ralloc(r, struct dhash_table);

  h->r = r;
  h->elements = rarrayalloc(r, initial_size, void *);
  h->size = initial_size;
  h->used = 0;
  h->compare = compare;
  h->hash = hash;

  return h;
}

unsigned long dhash_used(dhash_table h)
{
  return h->used;
}

void *dhlookup(dhash_table h, void *x)
{
  unsigned long hash = h->hash(x);
  unsigned long i = hash & (h->size - 1);

  for (;;)
    {
      void *bucket = h->elements[i];

      if (!bucket)
	return NULL;
      if (h->compare(x, bucket))
	return bucket;

      if (++i >= h->size)
	i = 0;
    }
}

void dhadd(dhash_table h, void *x)
{
  unsigned long hash = h->hash(x);
  unsigned long i;

  h->used++;
  if (h->used > 3 * h->size / 4)
    {
      void **oldelements = h->elements;
      unsigned long j, oldsize = h->size;

      /* Grow hashtable */
      h->size *= 2;
      h->elements = rarrayalloc(h->r, h->size, void *);

      /* Rehash old entries */
      for (j = 0; j < oldsize; j++)
	if (oldelements[j])
	  {
	    unsigned long newhash = h->hash(oldelements[j]);
	    unsigned long newi = newhash & (h->size - 1);
		    
	    while (h->elements[newi])
	      {
		newi++;
		if (newi >= h->size)
		  newi = 0;
	      }
	    h->elements[newi] = oldelements[j];
	    if (j == i)
	      i = newi;
	  }
    }

  i = hash & (h->size - 1);

  for (;;)
    {
      if (!h->elements[i])
	{
	  h->elements[i] = x;
	  return;
	}

      if (++i >= h->size)
	i = 0;
    }
}

dhash_scan dhscan(dhash_table h)
{
  dhash_scan iterator;

  iterator.h = h;
  iterator.index = 0;

  return iterator;
}

void *dhnext(dhash_scan *iterator)
{
  dhash_table h = iterator->h;

  for (;;)
    {
      void *x;

      if (iterator->index >= h->size)
	return NULL;
      x = h->elements[iterator->index++];
      if (x)
	return x;
    }
}
