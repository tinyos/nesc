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
#include "nesc-module.h"
#include "nesc-configuration.h"
#include "nesc-component.h"
#include "c-parse.h"
#include "expr.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "nesc-task.h"

expression make_generic_call(location loc, expression iref, expression args)
{
  expression result = CAST(expression, new_generic_call(parse_region, loc, iref, args));

  check_arguments(iref->type, args, get_function_ddecl(iref), TRUE);
  result->type = type_function_return_type(iref->type);

  return result;
}

word make_word(location l, cstring s)
{
  return new_word(parse_region, l, s);
}

declarator make_interface_ref_declarator(location l, cstring w1, cstring w2)
{
  identifier_declarator id =
    new_identifier_declarator(parse_region, l, w2);

  return CAST(declarator,
	      new_interface_ref_declarator(parse_region, l,
					   CAST(declarator, id),
					   make_word(l, w1)));
}

expression make_interface_deref(location loc, expression object, cstring field)
{
  interface_deref result;
  data_declaration iref = type_iref(object->type);
  data_declaration fdecl = interface_lookup(iref, field.data);

  result = new_interface_deref(parse_region, loc, object, field, fdecl);
  if (!fdecl)
    {
      error("interface has no command or event named `%s'", field.data);
      result->type = error_type;
    }
  else
    result->type = fdecl->type;

  return CAST(expression, result);
}

/* Check that function fndecl (from the module's external interface)
   is implemented.
*/
static void check_function_implemented(data_declaration fndecl, void *data)
{
  location *loc = data;
  data_declaration idecl = fndecl->interface;

  assert(fndecl->kind == decl_function);

  if (fndecl->defined && !fndecl->definition)
    {
      if (idecl)
	error_with_location(*loc, "`%s.%s' not implemented",
			    idecl->name, fndecl->name);
      else
	error_with_location(*loc, "`%s' not implemented", fndecl->name);
    }
}

/* Checks that all defined functions of the modules are implemented */
static void check_complete_implementation(module m)
{
  component_functions_iterate(m->cdecl, check_function_implemented,
			      &m->location);
}

void process_module(module m)
{
  check_complete_implementation(m);
  /* Add wires to graph for tasks */
  wire_scheduler(m);
}
