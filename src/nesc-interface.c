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

declaration declare_interface_parm(location l, cstring id)
{
  interface_parm_decl d = new_interface_parm_decl(parse_region, l, id, NULL);
  data_declaration ddecl;

  if ((ddecl = lookup_id(id.data, TRUE)))
    error("duplicate parameter name `%s' in parameter list", id.data);
  else
    {
      struct data_declaration tempdecl;

      init_data_declaration(&tempdecl, CAST(declaration, d), id.data,
			    error_type);
      tempdecl.kind = decl_typedef;
      tempdecl.definition = tempdecl.ast;
      ddecl = declare(current.env, &tempdecl, FALSE);
      ddecl->type = make_variable_type(ddecl);
    }
  d->ddecl = ddecl;

  return CAST(declaration, d);
}

void build_interface(region r, nesc_declaration idecl)
{
  AST_set_parents(CAST(node, idecl->ast));
}
