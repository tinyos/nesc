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

/* Top-level nesc environment. Keeps track of loaded interfaces and
   components, loads them on demand */

/* The environments for components and interfaces */
static env nesc_component_env, nesc_interface_env, nesc_c_env;

void init_nesc_env(region r)
{
  nesc_interface_env = new_env(r, NULL);
  nesc_component_env = new_env(r, NULL);
  nesc_c_env = new_env(r, NULL);
}

void interface_declare(interface_declaration d)
{
  env_add(nesc_interface_env, d->name, d);
}

void component_declare(component_declaration d)
{
  env_add(nesc_component_env, d->name, d);
}

component_declaration require_component(location l, const char *name)
{
  component_declaration d = 
    env_lookup(nesc_component_env, name, FALSE);

  if (d)
    return d;

  return load_component(l, name, FALSE);
}

interface_declaration require_interface(location l, const char *name)
{
  interface_declaration d =
    env_lookup(nesc_interface_env, name, FALSE);

  if (d)
    return d;

  return load_interface(l, name, FALSE);
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
