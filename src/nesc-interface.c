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
#include "nesc-interface.h"
#include "nesc-semantics.h"
#include "nesc-decls.h"
#include "nesc-paths.h"
#include "nesc-env.h"
#include "semantics.h"
#include "c-parse.h"
#include "edit.h"

bool interface_defines;

interface_declaration
new_interface_declaration(region r, const char *name, nesc_decl ast, environment decls)
{
  interface_declaration new = ralloc(r, struct interface_declaration);

  new->kind = nesc_interface;
  new->name = name;
  new->ast = ast;
  new->env = decls;

  return new;
}

void build_interface(interface_declaration idecl)
{
  AST_set_parents(CAST(node, idecl->ast));
}

interface_declaration load_interface(location l, const char *name)
{
  interface_declaration idecl =
    new_interface_declaration(parse_region, name, NULL, NULL);
  environment interface_env;

  /* We don't get duplicates as we only load on demand */
  interface_declare(idecl);

  the_interface = NULL;
  interface_env = compile(l, l_interface, name, (nesc_declaration)idecl,
			  global_env);

  if (!the_interface)
    {
      word ifname = build_word(parse_region, name);
      the_interface = new_interface(parse_region, dummy_location, ifname, NULL);
    }

  check_nesc_declaration((nesc_declaration)idecl, interface_env,
			 CAST(nesc_decl, the_interface));

  build_interface(idecl);

  return idecl;
}
