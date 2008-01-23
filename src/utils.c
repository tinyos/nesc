/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
   Copyright (C) 2002 Intel Corporation
It also includes 
   Public domain code from Pat Rankin <rankin@eql.caltech.edu>
and code from libiberty that is
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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
#include <ctype.h>

void renew_region(region *r)
/* Effects: Delete region *r (if not null), allocate a new region in *r */
{
  if (*r)
    deleteregion_ptr(r);
  *r = newregion();
}

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

void *xcalloc(size_t count, size_t size)
{
#ifdef BWGC
  void *x = GC_calloc(count, size);
#else
  void *x = calloc(count, size);
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

char *xstrdup(const char *s)
{
  char *t = strdup(s);

  if (!t) abort();

  return t;
}

void *xmemdup(const void *from, size_t s, size_t nsize)
{
  void *x = xcalloc(1, nsize);

  return memcpy(x, from, s);
}

#define ERRSTR_FMT "undocumented error #%d"
static char xstrerror_buf[sizeof ERRSTR_FMT + 20];

/* Like strerror, but result is never a null pointer.  */
char *xstrerror (int errnum)
{
  char *errstr;
#ifdef VMS
  char *(*vmslib_strerror) (int,...);

  /* Override any possibly-conflicting declaration from system header.  */
  vmslib_strerror = (char *(*) (int,...)) strerror;
  /* Second argument matters iff first is EVMSERR, but it's simpler to
     pass it unconditionally.  `vaxc$errno' is declared in <errno.h>
     and maintained by the run-time library in parallel to `errno'.
     We assume that `errnum' corresponds to the last value assigned to
     errno by the run-time library, hence vaxc$errno will be relevant.  */
  errstr = (*vmslib_strerror) (errnum, vaxc$errno);
#else
  errstr = strerror (errnum);
#endif

  /* If `errnum' is out of range, result might be NULL.  We'll fix that.  */
  if (!errstr)
    {
      sprintf (xstrerror_buf, ERRSTR_FMT, errnum);
      errstr = xstrerror_buf;
    }
  return errstr;
}

/* Make a new unintialised cstring of length l */
cstring alloc_cstring(region r, int l)
{
  cstring cs;

  cs.data = rstralloc(r, l + 1);
  cs.data[l] = '\0';
  cs.length = l;

  return cs;
}

/* Make a new cstring with a copy of s, length l */
cstring make_cstring(region r, const char *s, int l)
{
  cstring cs = alloc_cstring(r, l);

  memcpy(cs.data, s, l);

  return cs;
}

/* Make a new cstring with a copy of regular C string s */
cstring str2cstring(region r, const char *s)
{
  return make_cstring(r, s, strlen(s));
}

/* Make a new C string with a copy of cstring s */
char *cstring2str(region r, cstring s)
{
  int len = s.length + 1;
  char *str = rstralloc(r, len);

  memcpy(str, s.data, len);

  return str;
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

int ilog2(largest_uint x)
{
  /* slow version */
  largest_uint v = 1;
  int log2 = 0;

  while (v < x)
    {
      v <<= 1;
      log2++;
      if (!v)
	return -1;
    }

  return v == x ? log2 : -1;
}

DEFINE_ARRAY(char_array, char)

/* For some obscure reason, there's no standard function for this
   (Linux's wctombs does it, but it's not standard) */
int wcs_mb_size(const wchar_t *wstr)
/* Returns: number of bytes to be allocated for a C string buffer
     that will successfully hold the result of wcstombs(buffer, wstr, ?),
     or -1 if wstr cannot be converted
*/
{
  size_t len = 0;
  char tmp[MB_CUR_MAX];

  wctomb(NULL, 0);
  while (*wstr)
    {
      int mblen = wctomb(tmp, *wstr++);

      if (mblen < 0)
	return -1;
      len += mblen;
    }

  return len + 1;
}

unsigned long hash_ptr(void *p) 
/* Returns: a reasonable hash for a pointer value
 */
{
  return (unsigned long)p >> ALIGNMENT_BITS;
}

int compare_ptr(void *entry1, void *entry2)
/* Returns: entry1 == entry2 (use with new_dhash_table) */
{
  return entry1 == entry2;
}

unsigned long hash_str(const char *s) 
/* Returns: a reasonable hash for a character string.  
    FIXME: the return value is only effected by the final 32 characters
    in the string.
 */
{
  register unsigned long code = 0;

  if( !s ) 
    return 0x57954317;

  while (*s)
    {
      code = ((code << 1) + *s) ^ 0x57954317;
      s++;
    }

  return code;
}

/* On machines with DIR_SEPARATOR defined, replace all DIR_SEPARATOR's
   by / */
void unixify_path(char *path)
{
#ifdef DIR_SEPARATOR
  while ((path = strchr(path, DIR_SEPARATOR)))
    *path++ = '/';
#endif
}

/* TRUE if path is absolute, false otherwise */
bool absolute_path(char *path)
{
#ifndef WIN32
  if (path[0] == '/')
    return TRUE;
#endif

#if defined(WIN32) || defined(__CYGWIN32__)
  if (isalpha(path[0]) && path[1] == ':' &&
      (path[2] == '/' || path[2] == '\\'))
    return TRUE;
#endif

  return FALSE;
}

#if !HAVE_REALPATH
#ifdef WIN32
#include <direct.h>

static bool pathcat(char *base, const char *add)
{
  int l = strlen(base);

  if (l + strlen(add) + 2 >= PATH_MAX)
    return FALSE; /* doesn't fit */

  base[l] = '/';
  strcpy(base + l + 1, add);
  return TRUE;
}

char *realpath(const char *path, char *resolved_path)
{
  char *slash, *last;

  /* This version doesn't support a NULL resolved_path. We don't
     use it that way anyway */

  if (isalpha(path[0]) && path[1] == ':')
    if (path[2] == '/' || path[2] == '\\')
      {
	if (strlen(path) >= PATH_MAX - 1)
	  return NULL;
	strcpy(resolved_path, path); /* absolute path */
      }
    else
      {
	/* drive relative path */
	if (!_getdcwd(tolower(path[0] - 'a' + 1), resolved_path, PATH_MAX))
	  return NULL;
	if (!pathcat(resolved_path, path + 2))
	  return NULL;
      }
  else
    {
      /* fully relative path */
      if (!getcwd(resolved_path, PATH_MAX))
	return NULL;
      if (!pathcat(resolved_path, path))
	return NULL;
    }

  if (!absolute_path(resolved_path))
    return NULL; /* this could be an assert */

  unixify_path(resolved_path);

  /* Then, we remove all duplicate /'s, and . and .. directory
     references. No attempt to avoid n^2 like behaviour. */
  last = resolved_path + 2;
  do
    {
      /* Find next slash or end of path */
      slash = last + 1;
      while (*slash != '/' && *slash)
	slash++;

      if (slash == last + 1 || /* empty dir spec */
	  (slash == last + 2 && last[1] == '.')) /* or . */
	memmove(last, slash, strlen(slash) + 1);
      else if (slash == last + 3 &&
	       last[1] == '.' && last[2] == '.') /* .. */
	{
	  /* look backwards for directory to squash */
	  while (last >= resolved_path + 2 && *--last != '/')
	    ;
	  memmove(last, slash, strlen(slash) + 1);
	}
      else
	last = slash;
    }
  while (*last);

  /* If we have x:, make it into x:/ */
  if (!resolved_path[2])
    {
      resolved_path[2] = '/';
      resolved_path[3] = '\0';
    }

  return resolved_path;
}
#else
#error "realpath missing"
#endif
#endif

#ifdef TEST_REALPATH
void tr(char *path)
{
  char rp[PATH_MAX];

  if (realpath(path, rp))
    printf("%s -> %s\n", path, rp);
  else
    printf("%s fails\n", path);
}

int region_main()
{
  tr("aa");
  tr("n:xx");
  tr("n:\\xx");
  tr("n:\\\\aa\\\\b\\.\\c");
  tr("c:/");
  tr("c:/foo/");
  tr("c:/foo/..");
  tr("c:/../foo");
  tr("c:///////////");
  tr("n:../fun");
  tr("../.././a");
  return 0;
}
#endif
