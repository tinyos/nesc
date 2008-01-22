/* This file is part of the nesC compiler.
   Copyright (C) 2002-2008 Intel Corporation

   This file also includes code from gcc that is
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.

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
#include "nesc-paths.h"
#include "semantics.h"

#include "gcc-cpp.h"

/* Locate components/interfaces from their name */

static region pathregion;
/* Include chains heads and tails.  */
static struct cpp_dir *heads[4];
static struct cpp_dir *tails[4];
static bool include_current_dir;
static int maxdirlen;

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

/* Handle -I- */
static void add_minus(region r)
{
  if (!include_current_dir)
    error("-I- specified twice");
  include_current_dir = FALSE;

  heads[CHAIN_QUOTE] = heads[CHAIN_BRACKET];
  tails[CHAIN_QUOTE] = tails[CHAIN_BRACKET];
  heads[CHAIN_BRACKET] = NULL;
  tails[CHAIN_BRACKET] = NULL;
}

static void add_dir(region r, const char *path, int len, int chain)
{
  cpp_dir *np = ralloc(r, struct cpp_dir);
  int l;

  np->next = NULL;
  np->name = canonicalise(r, path, len);;
  np->sysp = chain == CHAIN_SYSTEM || chain == CHAIN_AFTER;
  np->construct = 0;
  np->user_supplied_p = 1;	/* appears unused */

  if (tails[chain])
    tails[chain]->next = np;
  else
    heads[chain] = np;
  tails[chain] = np;

  l = strlen(np->name);
  if (l > maxdirlen)
    maxdirlen = l;
}

void add_nesc_dir(const char *path, int chain)
{
  if (!strcmp(path, "-"))
    add_minus(pathregion);
  else
    add_dir(pathregion, path, strlen(path), chain);
}

static bool file_exists(const char *fullname)
{
  struct stat sbuf;
  return stat(fullname, &sbuf) == 0 && S_ISREG(sbuf.st_mode);
}

static const char *find_file(char *filename)
{
  char *fullname = alloca(maxdirlen + strlen(filename) + 1);
  struct cpp_dir *p;

  if (include_current_dir && file_exists(filename))
    return "";
  for (p = heads[CHAIN_QUOTE]; p; p = p->next)
    {
      sprintf(fullname, "%s%s", p->name, filename);
      if (file_exists(fullname))
	return p->name;
    }
  return NULL;
}

static void build_search_path(region r, const char *pathlist, int chain)
{
  if (pathlist)
    {
      char *colon;

      while ((colon = strchr(pathlist, ':')))
	{
	  *colon = '\0';
	  add_dir(r, pathlist, colon - pathlist, chain);
	  pathlist = colon + 1;
	}
      add_dir(r, pathlist, strlen(pathlist), chain);
    }
}

void init_nesc_paths_start(region r)
{
  include_current_dir = TRUE;
  maxdirlen = 0;
  pathregion = r;
}

void add_nesc_path(const char *path, int chain)
{
  build_search_path(pathregion, path, chain);
}

static void join(int c1, int c2)
{
  if (heads[c1])
    tails[c1]->next = heads[c2];
  else
    heads[c1] = heads[c2];
}

void init_nesc_paths_end(void)
{
  add_nesc_path(getenv("NESCPATH"), CHAIN_BRACKET);

  join(CHAIN_SYSTEM, CHAIN_AFTER);
  join(CHAIN_BRACKET, CHAIN_SYSTEM);
  join(CHAIN_QUOTE, CHAIN_BRACKET);

  /* If verbose, print the list of dirs to search.  */
  if (flag_verbose)
    {
      struct cpp_dir *p;

      fprintf (stderr, "#include \"...\" and component search starts here:\n");
      for (p = heads[CHAIN_QUOTE];; p = p->next)
	{
	  if (p == heads[CHAIN_BRACKET])
	    fprintf (stderr, "#include <...> search starts here:\n");
	  if (!p)
	    break;
	  fprintf (stderr, " %s\n", p->name);
	}
      fprintf (stderr, "End of search list.\n");
    }
}

void set_cpp_include_path(void)
{
  cpp_reader *reader = current.lex.finput;

  cpp_set_include_chains(reader, heads[CHAIN_QUOTE], heads[CHAIN_BRACKET],
			 !include_current_dir);
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
