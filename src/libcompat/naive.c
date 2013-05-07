#include "stats.c"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include "regions.h"

/* Module: regions
   Date: 14 Feb 92
   Purpose: Fast & easy memory allocator for the mudlle compiler
     Based on the concept of blocks: you allocate memory from a block,
       and can free the whole block at one go.
       Individual deallocations are not possible.
*/

#define ALIGN(x, n) (((x) + ((n) - 1)) & ~((n) - 1))

#define BLOCK_SIZE 8172                /* Should probably be chosen w/ respect to
                                   malloc implementation */
#define BIG_SIZE 16364

typedef double max_aligned_type;

struct region_
{
  struct block *current;
};

struct block
{
#ifndef USEMALLOC
  char *pos;                        /* Where next to allocate from */
  char *end;                        /* End of block */
#endif
  struct block *previous;
  max_aligned_type data[1];
};

nomem_handler nomem_h;
region permanent;

#ifdef BWGC
region newregion(void)
{
  return NULL;
}

void deleteregion(region b)
/* Effect: Free all memory allocated in region b.
*/
{
}

void deleteregion_ptr(region *r)
/* Effect: Free all memory allocated in region *r and sets *r to NULL.
*/
{
}

static void *allocate(region b, unsigned long size)
/* Effects: Allocates size bytes from block b. The result is aligned
     correctly for all types.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated.
*/
{
  return GC_malloc(size);
}
#else

static struct block *make_block(int size)
{
  struct block *newp = malloc(offsetof(struct block, data) + size);

  if (!newp)
    return NULL;

#ifndef USEMALLOC
  newp->pos = (char *)newp->data;
  newp->end = newp->pos + size;
#endif
  newp->previous = 0;

  return newp;
}

region newregion(void)
/* Return: A new region from which to allocate some memory.
*/
{
  region newp = malloc(sizeof *newp);

  if (!newp)
    {
      nomem_h();
      abort();
      return NULL;
    }

  newp->current = make_block(BLOCK_SIZE);
  if (!newp->current)
    {
      free(newp);
      nomem_h();
      abort();
      return NULL;
    }
      
  return newp;
}

region newsubregion(region r)
{
  return newregion();
}

void deleteregion(region b)
/* Effect: Free all memory allocated in region b.
*/
{
  struct block *blk = b->current;

  while (blk)
    {
      struct block *prev = blk->previous;

      free(blk);
      blk = prev;
    }
}

void deleteregion_ptr(region *r)
/* Effect: Free all memory allocated in region *r and sets *r to NULL.
*/
{
  deleteregion(*r);
  *r = NULL;
}

static void *allocate(region b, unsigned long size)
/* Effects: Allocates size bytes from block b. The result is aligned
     correctly for all types.
   Returns: A pointer to the start of the block.
   Note: In this implementation, 12 + average(size)/2 bytes will be wasted
     for every BLOCK_SIZE bytes allocated.
*/
{
#ifdef USEMALLOC
  struct block *old = b->current, *new = make_block(size);

  new->previous = old;
  b->current = new;
  return &new->data;
#else
  struct block *blk = b->current;
  void *result;

  size = ALIGN(size, sizeof(max_aligned_type));

  result = blk->pos;
  if ((blk->pos += size) >= blk->end)
    {
      /* Block full, get new one */
      int bsize;
      struct block *newp;

      if (size < BLOCK_SIZE / 4)
        bsize = BLOCK_SIZE;
      else if (size >= BIG_SIZE)
        bsize = size;
      else
        bsize = size * 4;
      newp = make_block(bsize);

      if (!newp)
        {
          nomem_h();
          abort();
          return NULL;
        }

      newp->previous = blk;
      b->current = newp;
      result = newp->pos;
      newp->pos += size;
    }
  return result;
#endif
}
#endif

void *typed_ralloc(region r, size_t size, type_t type)
{
  return rstralloc0(r, size);
}

void *typed_rarrayalloc(region r, size_t n, size_t size, type_t type)
{
  return typed_ralloc(r, n * size, type);
}

void *typed_rarrayextend(region r, void *old, size_t n, size_t size, type_t t)
{
  return rstrextend0(r, old, n * size);
}

char *rstralloc(region r, size_t size)
{
  return allocate(r, size);
}

char *rstralloc0(region r, size_t size)
{
  void *newo = allocate(r, size);

  if (newo)
    memset(newo, 0, size);

  return newo;
}

char *rstrdup(region r, const char *s)
{
  int l = strlen(s);
  char *news = allocate(r, l + 1);

  if (news)
    memcpy(news, s, l + 1);

  return news;
}

static char *internal_rstrextend(region r, const char *old, size_t newsize,
                                 int needsclear)
{
  abort();
}

char *rstrextend(region r, const char *old, size_t newsize)
{
  return internal_rstrextend(r, old, newsize, 0);
}

char *rstrextend0(region r, const char *old, size_t newsize)
{
  return internal_rstrextend(r, old, newsize, 1);
}

void typed_rarraycopy(void *to, void *from, size_t n, size_t size, type_t type)
{
  memcpy(to, from, n * size);
}

nomem_handler set_nomem_handler(nomem_handler newhandler)
{
  nomem_handler oldh = nomem_h;

  nomem_h = newhandler;

  return oldh;
}

void region_init(void)
{
  permanent = newregion();
  if (getenv("REGIONSTATS"))
    benchmark_init();
}

#ifndef NO_REGION_MAIN
int region_main(int argc, char **argv, char **envp);

int main(int argc, char **argv, char **envp)
{
  region_init();
  return region_main(argc, argv, envp);
}
#endif
