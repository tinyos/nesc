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
#include "c-parse.h"

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

  return !strcmp(name, "C") || !strcmp(name, "spontaneous") ||
    !strcmp(name, "combine");
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
      if (!strcmp(dot, ".nc"))
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
	error_with_location(loc, "failed to preprocess %s", path);
    }

  if (!f)
    {
      env = new_environment(parse_region, global_env, TRUE, FALSE);
      if (container)
	container->env = env;
    }
  else
    {	
      set_input(f, path);

      start_lex(l);
      start_semantics(l, container, parent_env);
      current.fileregion = newregion();
      env = current.env;
      if (container)
	container->env = env;
      parse();
      deleteregion_ptr(&current.fileregion);
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

void build(nesc_declaration decl, nesc_decl ast)
{
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

  decl = new_nesc_declaration(parse_region, sl, element);
    
  /* We don't get duplicates as we only load on demand */
  nesc_declare(decl);

  parsed_nesc_decl = NULL;
  compile(l, sl, name, name_is_path, decl, global_env);
  if (!parsed_nesc_decl)
    parsed_nesc_decl = dummy_nesc_decl(sl, element);

  actual_name = parsed_nesc_decl->word1->cstring.data;
  if (strcmp(element, actual_name))
    error_with_location(parsed_nesc_decl->location,
			"expected %s `%s', but got %s '%s'",
			language_name(decl->kind),
			element,
			language_name(decl->kind),
			actual_name);

  build(decl, parsed_nesc_decl);

  return decl;
}

bool is_module_local_static(data_declaration ddecl)
{
  return ddecl->kind == decl_variable &&
    (ddecl->vtype == variable_static && ddecl->container_function &&
     ddecl->container_function->container); 
}

bool is_module_variable(data_declaration ddecl)
{
  return ddecl->kind == decl_variable &&
    ddecl->Cname == FALSE &&
    /* top-level module var or local static module var */
    (ddecl->container || is_module_local_static(ddecl));
}

const char *make_intf_printname(const char *iname, const char *fname)
/* Returns: string "iname.fname" allocated in current.fileregion
 */
{
  size_t ilen = strlen(iname);
  size_t dlen = strlen(fname);
  char *fullname = rstralloc(current.fileregion, ilen + dlen + 2);

  memcpy(fullname, iname, ilen);
  fullname[ilen] = '.';
  strcpy(fullname + ilen + 1, fname);

  return fullname;
}

const char *decl_printname(data_declaration ddecl)
{
  if (ddecl_is_command_or_event(ddecl) && ddecl->interface)
    return make_intf_printname(ddecl->interface->name, ddecl->name);
  else
    return ddecl->name;
}

data_declaration get_function_ddecl(expression e)
/* Returns: If e denotes a specific function, return its data_declaration
     Otherwise return NULL
*/
{
  if (is_identifier(e))
    {
      identifier id = CAST(identifier, e);

      if (id->ddecl->kind == decl_function)
	return id->ddecl;
    }
  else if (is_interface_deref(e))
    return CAST(interface_deref, e)->ddecl;

  return NULL;
}

void handle_combine_attribute(location loc, const char *combiner, type *t)
{
  data_declaration cdecl;
  typelist combiner_args;
  type combiner_sig;
  bool ok = FALSE;

  /* Build combiner signature */
  combiner_args = new_typelist(parse_region);
  typelist_append(combiner_args, *t);
  typelist_append(combiner_args, *t);
  combiner_sig = make_function_type(*t, combiner_args, FALSE, FALSE);

  /* If combiner already declared, declaration should match combiner_sig.
     If not, we declare it with signature combiner_sig */
  cdecl = lookup_id(combiner, FALSE);
  if (cdecl)
    {
      if (cdecl->kind != decl_function ||
	  !(cdecl->ftype == function_normal || cdecl->ftype == function_static))
	error_with_location(loc, "combiner `%s' is not a C function",
			    combiner);
      else if (!type_compatible_unqualified(cdecl->type, combiner_sig))
	error_with_location(loc, "combiner `%s' does not have the right signature",
			    combiner);
      else
	ok = TRUE;
    }
  else
    {
      struct data_declaration tempdecl;
      declaration dummy = make_error_decl();

      /* Declare combiner as a function of arguments *t, *t returning *t */
      dummy->location = loc;
      init_data_declaration(&tempdecl, dummy, combiner, combiner_sig);
      tempdecl.kind = decl_function;
      tempdecl.ftype = function_normal;
      tempdecl.isexternalscope = tempdecl.isfilescoperef = TRUE;
      cdecl = declare(current.env, &tempdecl, FALSE);

      ok = TRUE;
    }

  if (ok)
    *t = make_combiner_type(*t, cdecl);
}
