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
#include "env.h"
#include "dhash.h"
#include "utils.h"

#define DEFAULT_ENV_SIZE 16

struct entry
{
  const char *name;
  void *value;
};

struct env 
{
  env parent;
  region sameregion r;
  dhash_table table;
};

static int env_compare(void *entry1, void *entry2)
{
  struct entry *e1 = entry1, *e2 = entry2;

  return e1->name && e2->name && strcmp(e1->name, e2->name) == 0;
}

static unsigned long env_hash(void *entry)
{
  struct entry *e = entry;

  if (e->name) 
    return hash_str(e->name);
  else /* unnamed (distinct from all other entries), hash on address */
    return hash_ptr(e);
}

/* Create a new, empty environment with ancestor 'parent'.
   The environment, and any future contents, are allocated in r */
env new_env(region r, env parent)
{
  env e = ralloc(r, struct env);

  e->r = r;
  e->parent = parent;
  e->table = new_dhash_table(r, DEFAULT_ENV_SIZE, env_compare, env_hash);

  return e;
}

/* Return parent environment of e */
env env_parent(env e)
{
  return e->parent;
}

/* Return region of e */
region env_region(env e)
{
  return e->r;
}

/* Find entry s in in environment e. If not found, check ancestors
   except if this_level_only is true.
   Returns entry's value if s is found, NULL otherwise */
void *env_lookup(env e, const char *s, bool this_level_only)
{
  struct entry lookup, *found;

  lookup.name = s;
  for (;;)
    {
      found = dhlookup(e->table, &lookup);
      if (found)
	return found->value;
      if (this_level_only || !e->parent) 
	return NULL;
      e = e->parent;
    }
}

/* Add an entry for s, with value 'value' to environment e.
   Behaviour is undefined if e already contains an entry for s.
   Does not copy s. */
void env_add(env e, const char *s, void *value)
{
  struct entry *newe = ralloc(e->r, struct entry);

  newe->name = s;
  newe->value = value;
  dhadd(e->table, newe);
}

void env_scan(env e, env_scanner *scanner)
{
  *scanner = dhscan(e->table);
}

bool env_next(env_scanner *scanner, const char **name, void **value)
{
  struct entry *next = dhnext(scanner);
  if (!next)
    return FALSE;

  *name = next->name;
  *value = next->value;

  return TRUE;
}
