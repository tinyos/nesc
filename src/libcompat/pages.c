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
#include <limits.h>

typedef __rcintptr pageid;

static size_t total_page_count; /* total pages allocated */
#define PAGE_GROUP_SIZE (1 + (total_page_count >> 7))

#if 0
#define FREEPAGE ((region)-1) /* Id of a free page */
#else
#define FREEPAGE (&zeroregion)
#endif
#ifdef NMEMDEBUG
#define ASSERT_FREE(p) 
#define ASSERT_INUSE(p, r) 
#else
#define ASSERT_FREE(p) assert(regionof(p) == FREEPAGE)
#ifdef DUPLICATES
#define ASSERT_INUSE(p, r) assert(regionof(p) == r->base)
#else
#define ASSERT_INUSE(p, r) assert(regionof(p) == r)
#endif
#endif

/* Page allocator for region-based memory management */
/* TBD: special free list for size == K ?? */

#define PAGECOUNTBITS (CHAR_BIT * sizeof(pageid) - 1)

struct page
{
  /* Next page in region or in free list */
  struct page *next;

  /* Doubly linked list of pages sorted by address */
  struct page *next_address, *prev_address;

  /* number of pages in this allocation unit. Negative for free pages. */
  pageid pagecount : PAGECOUNTBITS;

  unsigned int free : 1;

  /* Only in free pages not in the single_pages list */
  struct page *previous;
};

/* The pages are kept in a single list sorted by address via the
   next_address and prev_address fields. The first page's prev_address and
   the last page's next_address fields points to pages_byaddress.
   page_byaddress.next_address is the first page
   page_byaddress.prev_address is the last page

   This list is used for coalescing operations.
*/
static struct page pages_byaddress;

struct page *alloc_single_page(struct page *next);
void free_single_page(region r, struct page *p);

struct page *alloc_pages(int n, struct page *next);
void free_pages(region r, struct page *p);


/* a list of free individual pages */
struct page *single_pages;

/* free pages (not including those in single_pages) */
struct page *unused_pages;

static void init_pages(void)
{
  pages_byaddress.next_address = &pages_byaddress;
  pages_byaddress.prev_address = &pages_byaddress;
}

static void insertbefore_address(struct page *p, struct page *before)
{
  p->prev_address = before->prev_address;
  p->next_address = before;
  before->prev_address = p;
  p->prev_address->next_address = p;
}

static void insertafter_address(struct page *p, struct page *after)
{
  insertbefore_address(p, after->next_address);
}

static void unlink_address(struct page *p)
{
  p->prev_address->next_address = p->next_address;
  p->next_address->prev_address = p->prev_address;
}

static void addbyaddress(struct page *p)
{
  struct page *address_scan;
  static struct page *last_add;

  /* Weird performance hack: we try and guess what direction the 
     underlying malloc gets memory by comparing the address of p
     with the previous address passed to addbyaddress. If it's greater,
     we try and add from the end of the list, walking backwards. Otherwise 
     we try and add from the start of the list, walking forwards. */

  if (p > last_add)
    {
      for (address_scan = pages_byaddress.prev_address; ;
	   address_scan = address_scan->prev_address)
	if (p > address_scan || address_scan == &pages_byaddress)
	  {
	    last_add = p;
	    insertafter_address(p, address_scan);
	    return;
	  }
    }
  else
    {
      for (address_scan = pages_byaddress.next_address; ;
	   address_scan = address_scan->next_address)
	if (p < address_scan || address_scan == &pages_byaddress)
	  {
	    last_add = p;
	    insertbefore_address(p, address_scan);
	    return;
	  }
    }
}

/* Doubly linked page list management */
void addfront(struct page **list, struct page *p)
/* Effects: Adds p to the front of doubly-linked list list */
{
  p->previous = NULL;
  p->next = *list;
  if (*list) (*list)->previous = p;
  *list = p;
}

void unlink_page(struct page **list, struct page *p)
/* Effects: Remove p from its doubly linked list */
{
  if (p->previous)
    p->previous->next = p->next;
  else
    *list = p->next;
  if (p->next)
    p->next->previous = p->previous;
}

#undef USE_MMAP
#ifdef USE_MMAP
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

#undef MAP_ANONYMOUS

#ifndef MAP_ANONYMOUS

static int dev_zero_fd = -1; /* Cached file descriptor for /dev/zero. */

#define MMAP(addr, size, prot, flags) \
 ((dev_zero_fd < 0 ? dev_zero_fd = open("/dev/zero", O_RDWR) : 0),  \
  mmap((addr), (size), (prot), (flags), dev_zero_fd, 0))

#else

#define MMAP(addr, size, prot, flags) \
 (mmap((addr), (size), (prot), (flags)|MAP_ANONYMOUS, -1, 0))

#endif

struct page *region_get_mem(size_t s)
{
  struct page *newp;

#if 0
  s = ALIGN(s, 65536);
#endif

  newp = (struct page *)MMAP(0, s, PROT_READ|PROT_WRITE, MAP_PRIVATE);

  if (!newp)
    return NULL;

  if (PALIGN(newp, RPAGESIZE) != newp)
    abort();

  addbyaddress(newp);

  /* Add the new memory to unused_pages */
#ifndef NMEMDEBUG
  set_region_range(newp, (char *)newp + s, FREEPAGE);
#endif
  total_page_count += s >> RPAGELOG;
  newp->pagecount = s >> RPAGELOG;
  newp->free = 1;
  addfront(&unused_pages, newp);

  return newp;
}

#else

/* region_get_mem will never allocate less than MINIMUM_MEM_REQUEST bytes.
   It wastes RPAGESIZE bytes, so there is an overhead of
   RPAGESIZE / MINIMUM_MEM_REQUEST
*/
#define MINIMUM_MEM_REQUEST (PAGE_GROUP_SIZE * K * RPAGESIZE)

struct page *region_get_mem(size_t s)
{
  size_t request_bytes;
  void *mem;
  struct page *newp;

  /* Don't get less than K * RPAGESIZE extra memory (K * RPAGESIZE
     is the minimum useful size for something on unused_pages) */
  if (s + K * RPAGESIZE < MINIMUM_MEM_REQUEST) 
    request_bytes = MINIMUM_MEM_REQUEST;
  else
    request_bytes = s;

  mem = malloc(request_bytes + RPAGESIZE);
  if (!mem)
    return NULL;
  newp = PALIGN(mem, RPAGESIZE);
  if (mem == newp) /* Maybe we were lucky! */
    request_bytes += RPAGESIZE;

  addbyaddress(newp);

  /* Add the new memory to unused_pages */
#ifndef NMEMDEBUG
  set_region_range(newp, (char *)newp + request_bytes, FREEPAGE);
#endif
  total_page_count += request_bytes >> RPAGELOG;
  newp->pagecount = request_bytes >> RPAGELOG;
  newp->free = 1;
  addfront(&unused_pages, newp);

  return newp;
}
#endif

/* Page to region map management */
/* ----------------------------- */

region __rcregionmap[MAXPAGE];


static void set_page_region(pageid pagenb, region r)
{
  __rcregionmap[pagenb] = r;
}

#define page_region(pagenb) (__rcregionmap[(pagenb)])

void set_region(struct page *p, int npages, region r)
{
  pageid pnb = PAGENB(p);

  while (npages-- > 0) 
    set_page_region(pnb++, r);
}

/* Mark the memory range from 'from' (inclusive) to 'to' (exclusive)
   as belonging to region with id 'rid' */
void set_region_range(void *from, void *to, region r)
{
  pageid first = PAGENB(from), last = PAGENB((pageid)to - 1);

  while (first <= last)
    set_page_region(first++, r);
}

/* Multi-page allocation management */
/* -------------------------------- */

struct page *alloc_split(struct page *split, int n, struct page *next)
/* Assumes freepages_lock held */
{
#ifndef NMEMDEBUG
  /* These pages had better be free */
  pageid i, pnb = PAGENB(split);

  assert(split->pagecount >= n);
  for (i = pnb; i < pnb + split->pagecount; i++)
    assert(page_region(i) == FREEPAGE);
#endif
  if (split->pagecount > n)
    {
      struct page *splitoff;

      /* Keep first part of block */
      split->pagecount -= n;
      /* Return latter part of block */
      splitoff = split;
      split = (struct page *)((char *)split + (split->pagecount << RPAGELOG));

      /* Update the by adress list */
      insertbefore_address(split, splitoff->next_address);
    }
  else
    {
      /* remove split from list */
      unlink_page(&unused_pages, split);
    }
  split->next = next;
  split->pagecount = n;
  split->free = 0;

  return split;
}

struct page *alloc_new(int n, struct page *next)
/* Assumes freepages_lock held */
{
  struct page *newp = region_get_mem(n << RPAGELOG);

  if (!newp)
    {
      if (nomem_h)
	nomem_h();
      abort();
    }
  assert(!((long)newp & (RPAGESIZE - 1)));

  /* region_get_mem may get us more memory than we asked for */
  return alloc_split(newp, n, next);
}

struct page *alloc_pages(int n, struct page *next)
{
  struct page *best;
  int bestn;
  struct page *scan;

  assert(n >= K);

  scan = unused_pages;
  /* Find first fit */
  for (;;)
    {
      if (!scan)
	return alloc_new(n, next);

      if (scan->pagecount >= n) break;
      scan = scan->next;
    }

  /* Now find best fit */
  best = scan;
  bestn = scan->pagecount;
  for (;;)
    {
      scan = scan->next;
      if (!scan)
	return alloc_split(best, n, next);

      if (scan->pagecount >=n && scan->pagecount < bestn)
	{
	  best = scan;
	  bestn = scan->pagecount;
	}
    }
}

static void coalesce(struct page *p)
{
  struct page *prev = p->prev_address, *next;

  p->free = 1;

  /* Coalesce with predecessor ? */
  if (prev->free && (char *)prev + (prev->pagecount << RPAGELOG) == (char *)p)
    {
      prev->pagecount += p->pagecount;
      unlink_address(p);
      p = prev;
    }
  else /* No, add to free pages list */
    addfront(&unused_pages, p);

  next = p->next_address;
  /* Coalesce with successor ? */
  if (next->free && (char *)p + (p->pagecount << RPAGELOG) == (char *)next)
    {
      unlink_page(&unused_pages, next);
      p->pagecount += next->pagecount;
      unlink_address(next);
    }
}

void free_pages(region r, struct page *p)
/* Assumes freepages_lock held */
{
#ifndef NMEMDEBUG
  pageid i, pnb = PAGENB(p);

  for (i = pnb; i < pnb + p->pagecount; i++)
    {
      assert(page_region(i) == r);
      set_page_region(i, FREEPAGE);
    }
#endif

  coalesce(p);
}


/* Single page management */
/* ---------------------- */

static int single_page_count;

static void add_single_pages(struct page *base)
/* Effects: Adds pages at base to the single_pages list */
{
  pageid n = base->pagecount;
  struct page *prev = base->prev_address, *basenext = base->next_address,
    *next;

  single_page_count += n;

  for (;;)
    {
      ASSERT_FREE(base);
      base->free = 0; /* Not free so that coalesce won't steal these back */
      base->prev_address = prev;
      prev = base;
      base->next = single_pages;
      single_pages = base;
      if (--n == 0)
	break;
      next = (struct page *)((char *)base + RPAGESIZE);
      base->next_address = next;
      base = next;
    }
  base->next_address = basenext;
  basenext->prev_address = base;
}

void scavenge_single_pages(int n)
{
  /* Add n pages to the single_pages list */
  struct page *scan, *best;
  __rcintptr bestn;

  /* Take any group in unused_pages that is <= n or < K.
     Remember smallest entry > n too. This is sortof equivalent to
     a best fit where we allow partial allocations to make up a whole */
  best = NULL;
  bestn = (__rcintptr)1 << (sizeof(__rcintptr) * CHAR_BIT - 2);
  scan = unused_pages;
  while (scan)
    {
      /* The pages < K can't be used for anything but single pages so we
	 might as well grab them even if they are a little too big */
      if (scan->pagecount <= n || scan->pagecount < K)
	{
	  struct page *adding = scan;

	  scan = scan->next;
	  n -= adding->pagecount;
	  unlink_page(&unused_pages, adding);
	  add_single_pages(adding);
	  if (n <= 0) return;
	}
      else
	{
	  if (scan->pagecount < bestn)
	    {
	      bestn = scan->pagecount;
	      best = scan;
	    }
	  scan = scan->next;
	}
    }
  /* Still not enough. Split the best block if there is one, allocate
     new pages otherwise */
  if (!best)
    add_single_pages(alloc_new(n, NULL));
  else if (best->pagecount - n < K)
    {
      unlink_page(&unused_pages, best);
      add_single_pages(best);
    }
  else
    add_single_pages(alloc_split(best, n, NULL));
}


struct page *alloc_single_page(struct page *next)
{
  struct page *p;

  if (!single_pages)
    {
      scavenge_single_pages(PAGE_GROUP_SIZE);
    }
  ASSERT_FREE(single_pages);
  p = single_pages;
  single_pages = p->next;
  p->next = next;

  single_page_count--;

  return p;
}

void free_single_page(region r, struct page *p)
/* Assumes freepages_lock held */
{
#ifndef NMEMDEBUG
  ASSERT_INUSE(p, r);
  set_page_region(PAGENB(p), FREEPAGE);
#endif

  /* Don't keep too many single pages (a small fraction of total
     allocated pages) */
  if (single_page_count > PAGE_GROUP_SIZE * 2)
    {
      p->pagecount = 1;
      coalesce(p);
    }
  else
    {
      p->next = single_pages;
      single_pages = p;
      single_page_count++;
    }
}
