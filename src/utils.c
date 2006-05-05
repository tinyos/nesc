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
DEFINE_ARRAY(char_array, char)

#ifdef __CYGWIN32__
#include <sys/cygwin.h>
#include <w32api/windows.h>

char *fix_filename(region r, const char *unix_filename)
{
  char winpath[MAX_PATH];

  if (flag_mingw_gcc)
    {
      cygwin_conv_to_win32_path(unix_filename, winpath);
      return rstrdup(r, winpath[0] ? winpath : ".");
    }
  else
    return rstrdup(r, unix_filename);
}
#else
char *fix_filename(region r, const char *unix_filename)
{
  return rstrdup(r, unix_filename);
}
#endif

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

#if !HAVE_REALPATH
char *realpath(const char *path, char *resolved_path)
{
  strcpy(resolved_path, path);
  return resolved_path;
}
#endif

#if !HAVE_BASENAME
/* A trivial version, which should work for unix and cygwin, and for 
   our purposes.
   Does NOT handle trailing / properly (we're using it for files only)
   (returns "" in that case)
*/
char *basename(const char *path)
{
  char *end;

  if (!path || !*path)
    return ".";

  end = (char *)path + strlen(path);
  while (end > path)
    if (*--end == '/' || *end == '\\')
      return end + 1;

  return (char *)path;
}
#endif

/* TRUE if path is absolute, false otherwise */
bool absolute_path(char *path)
{
  if (path[0] == '/')
    return TRUE;

#if defined(WIN32) || defined(__CYGWIN32__)
  if (isalpha(path[0]) && path[1] == ':')
    return TRUE;
#endif

  return FALSE;
}

