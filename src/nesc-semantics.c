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
#include "nesc-semantics.h"
#include "semantics.h"
#include "nesc-decls.h"
#include "c-parse.h"
#include "input.h"
#include "AST_utils.h"
#include "nesc-paths.h"
#include "nesc-cpp.h"

#include <errno.h>

interface the_interface;
component the_component;
declaration cdecls;

bool nesc_attribute(attribute a)
/* Returns: TRUE if a is a nesc-specific attribute
 */
{
  const char *name = a->word1->cstring.data;

  return !strcmp(name, "C") || !strcmp(name, "spontaneous");
}

type get_actual_function_type(type t)
/* Returns: The actual function type for a (possibly generic) type t
     representing the type of a function/command/event
 */
{
  if (type_generic(t))
    return type_function_return_type(t);
  else
    return t;
}

function_declarator ddecl_get_fdeclarator(data_declaration fndecl)
/* Effects: Returns fndecl's function_declarator
   Requires: fndecl represent a function or function pointer
*/
{
  declarator fd;

  if (is_variable_decl(fndecl->ast))
    fd = CAST(variable_decl, fndecl->ast)->declarator;
  else
    fd = CAST(function_decl, fndecl->ast)->declarator;

  return get_fdeclarator(fd);
}

declaration ddecl_get_gparms(data_declaration fndecl)
/* Effects: Returns the declaration list for fndecl's generic parameters 
   Requires: fndecl represent a function or function pointer
*/
{
  /* For functions in interfaces we get them from the interface ref */
  if (fndecl->kind == decl_function && fndecl->interface)
    return CAST(interface_ref, fndecl->interface->ast)->gparms;

  /* Otherwise we get them from the function's declarator */
  return ddecl_get_fdeclarator(fndecl)->gparms;
}

bool ddecl_is_command_or_event(data_declaration decl)
{
  return decl->kind == decl_function &&
    (decl->ftype == function_event || decl->ftype == function_command);
}

source_language pick_language_from_filename(char *name)
{
  char *dot = strrchr(name, '.');

  if (dot)
    {
      if (!strcmp(dot, ".ti"))
	return l_interface;
      if (!strcmp(dot, ".td"))
	return l_component;
    }
  return l_interface; /* default */
}

const char *language_name(source_language l)
{
  switch (l)
    {
    case l_interface: return "interface";
    case l_component: case l_implementation: return "component";
    case l_c: return "C file";
    default: assert(0); return "BUG";
    }
}

void check_nesc_declaration(nesc_declaration nd, environment env, nesc_decl ast)
{
  const char *actual_name = ast->word1->cstring.data;

  if (strcmp(nd->name, actual_name))
    error_with_location(ast->location,
			"expected interface `%s', got '%s'",
			nd->name, actual_name);

  /* Fill in the declaration */
  nd->ast = ast;
  nd->env = env;
}

environment compile(location loc, source_language l, const char *name,
		    nesc_declaration container, environment parent_env)
{
  const char *path = find_nesc_file(parse_region, l, name);
  FILE *f = NULL;
  struct semantic_state old_semantic_state;
  environment env;

  if (!path)
    error_with_location(loc, "%s %s not found", language_name(l), name);
  else
    {
      if (l == l_c)
	{
	  fprintf(stderr, "DBG> preprocessing %s\n", path);
	  f = preprocess(path);
	}
      else
	{
	  fprintf(stderr, "DBG> loading %s\n", path);
	  f = fopen(path, "r");
	}
      if (!f)
	error_with_location(loc, "cannot open %s: %s",
			    path, strerror(errno));
    }
  if (!f)
    return new_environment(parse_region, global_env, TRUE, FALSE);
	
  set_input(f, path);

  old_semantic_state = current;
  start_lex(l);
  start_semantics(l, container, parent_env);
  env = current.env;
  parse();
  end_input();
  current = old_semantic_state;

  return env;
}

