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
#include "nesc-env.h"
#include "env.h"
#include "nesc-interface.h"
#include "nesc-component.h"
#include "nesc-c.h"
#include "nesc-decls.h"
#include "nesc-semantics.h"
#include "c-parse.h"
#include "semantics.h"

/* Top-level nesc environment. Keeps track of loaded interfaces and
   components, loads them on demand */

/* The environments for components and interfaces */
static env nesc_env, nesc_c_env;


/* hack, to give the doc generation an easy way to list interfaces & components */
env get_nesc_env(void)
{
  return nesc_env;
}

void init_nesc_env(region r)
{
  nesc_env = new_env(r, NULL);
  nesc_c_env = new_env(r, NULL);
}

nesc_declaration new_nesc_declaration(region r, source_language kind,
				      const char *name)
{
  nesc_declaration new = ralloc(r, struct nesc_declaration);

  new->kind = kind;
  new->name = new->instance_name = name;
  if (kind == l_component && use_nido)
    new->local_statics = dd_new_list(r);
  new->env = new_environment(r, global_env, TRUE, FALSE);
  new->safe = flag_default_safe;

  return new;
}

void nesc_declare(nesc_declaration d)
{
  check_name(d->name);
  env_add(nesc_env, d->name, d);
}

nesc_declaration nesc_lookup(const char *name)
{
  return env_lookup(nesc_env, name, FALSE);
}

void preload(source_language sl, location l, const char *name)
{
  if (!nesc_lookup(name))
    load(sl, l, name, FALSE);
}
				    
nesc_declaration require(source_language sl, location l, const char *name)
{
  nesc_declaration d = nesc_lookup(name);

  if (!d)
    d = load(sl, l, name, FALSE);

  if (sl != d->kind)
    {
      /* Make a dummy declaration to make everyone happy */
      nesc_decl nd;

      d = new_nesc_declaration(parse_region, sl, name);
      nd = dummy_nesc_decl(l, d);

      error_with_location(l, "expected %s `%s', but got %s %s",
			  language_name(sl), name,
			  d->kind == l_interface ? "an" : "a",
			  language_name(d->kind));
    }
  return d;
}
				    
void require_c(location l, const char *name)
{
  static int dummy;
  int *present = env_lookup(nesc_c_env, name, FALSE);

  if (!present)
    {
      env_add(nesc_c_env, name, &dummy);
      load_c(l, name, FALSE);
    }
}
