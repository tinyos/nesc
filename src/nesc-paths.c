/* This file is part of the nesC compiler.
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
Boston, MA 02111-1307, USA.  */

#include "parser.h"
#include <sys/stat.h>
#include <unistd.h>

/* Locate components/interfaces from their name */

struct path
{
  struct path *next;
  char *dirname;
};

static struct path *searchpath;
static int maxdirlen;
static region pathregion;
static bool include_current_dir;

char **path_argv;
int path_argv_count;

static void build_include_argv(void)
{
  int n = 0;
  struct path *p;
  size_t bytes = 0;
  char *pathdata;

  for (p = searchpath; p; p = p->next)
    n += 2;

  path_argv_count = n;
  path_argv = rarrayalloc(pathregion, n, char *);
  pathdata = rarrayalloc(pathregion, bytes, char);

  n = 0;
  for (p = searchpath; p; p = p->next)
    {
      path_argv[n++] = "-I";
      path_argv[n++] = p->dirname ? fix_filename(pathregion, p->dirname) : "-";
    }
}

static char *canonicalise(region r, const char *path, int len)
{
  int newlen = len + 1;
  char *cp;

  if (len == 0)
    return "";

  if (path[len - 1] != '/'
#ifdef DIR_SEPARATOR
      && path[len - 1] != DIR_SEPARATOR
#endif
      )
    newlen++;

  cp = rarrayalloc(r, newlen, char);
  memcpy(cp, path, len);
  cp[newlen - 2] = '/'; /* make sure last char is / */
  cp[newlen - 1] = '\0'; /* null terminate */

  unixify_path(cp);

  return cp;
}

static void add_minus(region r)
/* Add special marker for -I- directive */
{
  struct path *np = ralloc(r, struct path);

  np->next = searchpath;
  searchpath = np;
  np->dirname = NULL;

  include_current_dir = FALSE;
}

static void add_dir(region r, const char *path, int len)
{
  struct path *np = ralloc(r, struct path);
  int l;

  np->next = searchpath;
  searchpath = np;
  np->dirname = canonicalise(r, path, len);
  l = strlen(np->dirname);
  if (l > maxdirlen)
    maxdirlen = l;
}

void add_nesc_dir(const char *path)
{
  if (!strcmp(path, "-"))
    add_minus(pathregion);
  else
    add_dir(pathregion, path, strlen(path));
}

static void reverse_searchpath(void)
{
  struct path *last, *p, *next;

  last = NULL;
  p = searchpath;
  while (p)
    {
      next = p->next;
      p->next = last;
      last = p;
      p = next;
    }
  searchpath = last;
}

static bool file_exists(const char *fullname)
{
  struct stat sbuf;
  return stat(fullname, &sbuf) == 0 && S_ISREG(sbuf.st_mode);
}

static const char *find_file(char *filename)
{
  char *fullname = alloca(maxdirlen + strlen(filename) + 1);
  struct path *p;

  if (include_current_dir && file_exists(filename))
    return "";
  for (p = searchpath; p; p = p->next)
    /* Hack 'if' to skip '-' directory, while keeping it in searchpath
       so as to preserve its position for preprocessor invocations */
    if (p->dirname)
      {
	sprintf(fullname, "%s%s", p->dirname, filename);
	if (file_exists(fullname))
	  return p->dirname;
      }
  return NULL;
}

static void build_search_path(region r, const char *pathlist)
{
  if (pathlist)
    {
      char *colon;

      while ((colon = strchr(pathlist, ':')))
	{
	  *colon = '\0';
	  add_dir(r, pathlist, colon - pathlist);
	  pathlist = colon + 1;
	}
      add_dir(r, pathlist, strlen(pathlist));
    }
}

void init_nesc_paths_start(region r)
{
  include_current_dir = TRUE;
  maxdirlen = 0;
  pathregion = r;
}

void add_nesc_path(const char *path)
{
  build_search_path(pathregion, path);
}

void init_nesc_paths_end(void)
{
  build_search_path(pathregion, getenv("NESCPATH"));
  reverse_searchpath();
  build_include_argv();
}

#define MAX_EXT_LEN 2

const char *find_nesc_file(region r, source_language l, const char *name)
{
  int filename_len = strlen(name) + MAX_EXT_LEN + 2;
  char *filename = alloca(filename_len);
  const char *dirname;
  int fullname_len;
  char *fullname;
  
  strcpy(filename, name);
  switch (l)
    {
    case l_interface: strcat(filename, ".nc"); break;
    case l_component: strcat(filename, ".nc"); break;
    case l_c: strcat(filename, ".h"); break;
    default: assert(0); break;
    }

  dirname = find_file(filename);

  if (!dirname)
    return NULL;

  fullname_len = strlen(dirname) + filename_len;
  fullname = rarrayalloc(r, fullname_len, char);

  sprintf(fullname, "%s%s", dirname, filename);

  return fullname;
}
