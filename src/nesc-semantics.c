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
#include "nesc-env.h"
#include "nesc-component.h"
#include "nesc-interface.h"
#include "edit.h"

#include <errno.h>

#ifdef HAVE_BASENAME
#include <libgen.h>
#else
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

  end = path + strlen(path);
  while (end > path)
    if (*--end == '/' || *end == '\\')
      return end + 1;

  return path;
}
#endif

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

bool nesc_filename(const char *name)
{
  char *dot = strrchr(basename((char *)name), '.');

  if (dot)
    {
      if (!strcmp(dot, ".ti"))
	return TRUE;
      if (!strcmp(dot, ".td"))
	return TRUE;
    }
  return FALSE; /* C by default */
}

const char *element_name(region r, const char *path)
/* Returns: Return the "identifier part"
     of path, i.e., remove any directory and extension
     The returned string is allocated in region r.
*/
{
  const char *base, *dot;

  base = basename((char *)path);
  dot = strrchr(base, '.');

  if (dot)
    {
      /* Extract id */
      char *copy = rarrayalloc(r, dot - base + 1, char);

      memcpy(copy, base, dot - base);
      copy[dot - base] = '\0';

      return copy;
    }
  else
    return rstrdup(r, base);
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

environment compile(location loc, source_language l,
		    const char *name, bool name_is_path,
		    nesc_declaration container, environment parent_env)
{
  const char *path =
    name_is_path ? name : find_nesc_file(parse_region, l, name);
  FILE *f = NULL;
  struct semantic_state old_semantic_state;
  environment env;

  old_semantic_state = current;

  if (!path)
    error_with_location(loc, "%s %s not found", language_name(l), name);
  else
    {
      if (flag_verbose)
	fprintf(stderr, "preprocessing %s\n", path);
      f = preprocess(path, l);

      if (!f)
	error_with_location(loc, "cannot open %s: %s",
			    path, strerror(errno));
    }

  if (!f)
    env = new_environment(parse_region, global_env, TRUE, FALSE);
  else
    {	
      set_input(f, path);

      start_lex(l);
      start_semantics(l, container, parent_env);
      env = current.env;
      parse();
      end_input();

      preprocess_file_end();
    }

  current = old_semantic_state;

  return env;
}

nesc_decl dummy_nesc_decl(source_language sl, const char *name)
{
  word wname = build_word(parse_region, name);
  nesc_decl nd;

  switch (sl)
    {
    case l_component: {
      implementation impl = CAST(implementation,
	new_module(parse_region, dummy_location, NULL, NULL));
      nd = CAST(nesc_decl,
	new_component(parse_region, dummy_location, wname, NULL, NULL, impl));
      break;
    }
    case l_interface:
      nd = CAST(nesc_decl,
	new_interface(parse_region, dummy_location, wname, NULL, NULL));
      break;
    default:
      assert(0);
      nd = NULL;
      break;
    }
  return nd;
}

void build(nesc_declaration decl, environment env, nesc_decl ast)
{
  decl->env = env;
  decl->ast = ast;

  if (ast->docstring)
    separate_short_docstring(ast->docstring, &decl->short_docstring,
			     &decl->long_docstring);

  switch (decl->kind)
    {
    case l_interface:
      build_interface(parse_region, decl);
      break;
    case l_component:
      build_component(parse_region, decl);
      break;
    default:
      assert(0);
    }
}

nesc_declaration load(source_language sl, location l,
		      const char *name, bool name_is_path)
{
  const char *element = name_is_path ? element_name(parse_region, name) : name;
  const char *actual_name;
  nesc_declaration decl;
  environment env;

  decl = new_nesc_declaration(parse_region, sl, element);
    
  /* We don't get duplicates as we only load on demand */
  nesc_declare(decl);

  parsed_nesc_decl = NULL;
  env = compile(l, sl, name, name_is_path, decl, global_env);
  if (!parsed_nesc_decl)
    parsed_nesc_decl = dummy_nesc_decl(sl, element);

  /* Patch decl->kind to match loaded kind. Our caller will report the
     appropriate error. */
  if (is_interface(parsed_nesc_decl))
    decl->kind = l_interface;
  else
    decl->kind = l_component;

  actual_name = parsed_nesc_decl->word1->cstring.data;
  if (strcmp(element, actual_name))
    error_with_location(parsed_nesc_decl->location,
			"expected %s `%s', but got %s '%s'",
			language_name(decl->kind),
			element,
			language_name(decl->kind),
			actual_name);

  build(decl, env, parsed_nesc_decl);

  return decl;
}

bool is_module_variable(data_declaration ddecl)
{
  return ddecl->kind == decl_variable &&
    ddecl->Cname == FALSE &&
     /* top-level module var */
    (ddecl->container || 
     /* static var in module function */
     (ddecl->vtype == variable_static && ddecl->container_function &&
      ddecl->container_function->container)); 
}

