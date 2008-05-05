/* This file is part of the nesC compiler.
   Copyright (C) 2008 Intel Corporation

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
#include "nesc-deputy.h"
#include "nesc-attributes.h"
#include "AST_walk.h"
#include "AST_utils.h"

static AST_walker deputy_walker;

struct deputy_data
{
  bool deputy_scope;
  tag_declaration in_struct;
  environment env;
};

static void dwalk(struct deputy_data *dd, void *p)
{
  if (p)
    AST_walk(deputy_walker, dd, CASTPTR(node, &p));
}

static AST_walker_result deputy_identifier(AST_walker unused, void *data,
					   identifier *p)
{
  identifier id = *p;
  struct deputy_data *dd = data;

  if (dd->deputy_scope)
    {
      const char *name = id->cstring.data;
      data_declaration realdecl = NULL;

      /* not doing make_identifier's error message suppression */

      if (dd->in_struct &&
	  env_lookup(dd->in_struct->fields, name, TRUE))
	realdecl = bad_decl; /* prevent renaming of field refs */
      if (!realdecl)
	realdecl = env_lookup(dd->env->id_env, name, FALSE);

      if (realdecl)
	id->ddecl = realdecl;
      else
	error_with_location(id->location, "`%s' undeclared", name);
    }
  return aw_done;
}

static AST_walker_result deputy_nesc_attribute(AST_walker unused, void *data,
					       nesc_attribute *p)
{
  nesc_attribute na = *p;
  struct deputy_data ndd = *(struct deputy_data *)data;

  ndd.deputy_scope = na->tdecl->deputy_scope;
  AST_walk_children(deputy_walker, &ndd, CAST(node, na));

  return aw_done;
}

static AST_walker_result deputy_tag_ref(AST_walker unused, void *data,
					tag_ref *p)
{
  tag_ref tref = *p;
  struct deputy_data ndd = *(struct deputy_data *)data;

  ndd.in_struct = tref->kind != kind_enum_ref ? tref->tdecl : NULL;
  dwalk(data, tref->attributes);
  dwalk(&ndd, tref->fields);

  return aw_done;
}

static AST_walker_result deputy_fdeclarator(AST_walker unused, void *data,
					    function_declarator *p)
{
  function_declarator fd = *p;
  struct deputy_data ndd = *(struct deputy_data *)data;

  /* strangely or not, the parameters are no longer in scope within
     (from the AST perspective) the fdeclarator's declarator */
  ndd.env = fd->env->parent;
  dwalk(&ndd, fd->declarator);
  dwalk(data, fd->return_type);
  dwalk(data, fd->parms);
  dwalk(data, fd->gparms);
  dwalk(data, fd->qualifiers);

  return aw_done;
}

static AST_walker_result deputy_function_decl(AST_walker unused, void *data,
					      function_decl *p)
{
  function_decl fdecl = *p;
  function_declarator fd = get_fdeclarator(fdecl->declarator);
  struct deputy_data ndd;

  /* The parameters are in scope for the result type too ... */
  ndd.deputy_scope = FALSE;
  ndd.env = fd->env;
  ndd.in_struct = NULL;
  dwalk(&ndd, fdecl->declarator);
  dwalk(&ndd, fdecl->modifiers);
  dwalk(&ndd, fdecl->attributes);
  dwalk(data, fdecl->stmt);

  return aw_done;
}

static AST_walker_result deputy_data_decl(AST_walker unused, void *data,
					  data_decl *p)
{
  data_decl dd = *p;
  declaration first;
  variable_decl vd;
  function_declarator fd;
  struct deputy_data ndd;

  /* We only get deputy behaviour if there's a single variable
     declaration (variable_decl) with a function_delarator inside it. */
  if (!dd->decls || dd->decls->next)
    return aw_walk;
  first = ignore_extensions(dd->decls);
  if (!is_variable_decl(first))
    return aw_walk;
  vd = CAST(variable_decl, first);
  fd = get_fdeclarator(vd->declarator);
  if (!fd || !vd->ddecl)
    return aw_walk;

  /* The parameters are in scope for the result type too ... */
  ndd.deputy_scope = FALSE;
  ndd.env = fd->env;
  ndd.in_struct = NULL;
  dwalk(&ndd, dd->modifiers);
  dwalk(&ndd, dd->decls);

  return aw_done;
}

static AST_walker_result deputy_compound_stmt(AST_walker unused, void *data,
					      compound_stmt *p)
{
  compound_stmt cs = *p;
  struct deputy_data ndd;

  ndd.deputy_scope = FALSE;
  ndd.env = cs->env;
  ndd.in_struct = NULL;
  AST_walk_children(deputy_walker, &ndd, CAST(node, cs));

  return aw_done;
}

static AST_walker_result deputy_implementation(AST_walker unused, void *data,
					       implementation *p)
{
  implementation impl = *p;
  struct deputy_data ndd;

  ndd.deputy_scope = FALSE;
  ndd.env = impl->ienv;
  ndd.in_struct = NULL;
  AST_walk_children(deputy_walker, &ndd, CAST(node, impl));

  return aw_done;
}

void resolve_deputy_scopes(nesc_decl ast)
{
  struct deputy_data ndd;

  ndd.deputy_scope = FALSE;
  ndd.in_struct = NULL;
  ndd.env = ast->cdecl->env;

  dwalk(&ndd, ast);
}

static void attr_dscope_tdecl(nesc_attribute attr, tag_declaration tdecl)
{
  if (tdecl->kind == kind_attribute_ref)
    tdecl->deputy_scope = TRUE;
  else
    error_with_location(attr->location, "@deputy_scope() can only be applied to attribute declarations");
}

static void attr_ndecl_safe(nesc_attribute attr, nesc_declaration ndecl)
{
  if (ndecl->kind == l_component)
    ndecl->safe = TRUE;
  else
    warning_with_location(attr->location, "@safe() attribute ignored");
}

static void attr_decl_safe(nesc_attribute attr, data_declaration ddecl)
{
  ddecl->safe = TRUE;
}

static void attr_ndecl_unsafe(nesc_attribute attr, nesc_declaration ndecl)
{
  if (ndecl->kind == l_component)
    ndecl->safe = FALSE;
  else
    warning_with_location(attr->location, "@unsafe() attribute ignored");
}

static void attr_decl_unsafe(nesc_attribute attr, data_declaration ddecl)
{
  ddecl->safe = FALSE;
}

void init_deputy(void)
{
  define_internal_attribute("deputy_scope", NULL, NULL, attr_dscope_tdecl, NULL,
			    NULL, NULL);
  define_internal_attribute("safe", attr_ndecl_safe, attr_decl_safe, NULL,
			    NULL, NULL, NULL);
  define_internal_attribute("unsafe", attr_ndecl_unsafe, attr_decl_unsafe, NULL,
			    NULL, NULL, NULL);

  deputy_walker = new_AST_walker(permanent);
  AST_walker_handle(deputy_walker, kind_identifier, deputy_identifier);
  AST_walker_handle(deputy_walker, kind_nesc_attribute, deputy_nesc_attribute);
  AST_walker_handle(deputy_walker, kind_tag_ref, deputy_tag_ref);
  AST_walker_handle(deputy_walker, kind_function_declarator, deputy_fdeclarator);
  AST_walker_handle(deputy_walker, kind_function_decl, deputy_function_decl);
  AST_walker_handle(deputy_walker, kind_data_decl, deputy_data_decl);
  AST_walker_handle(deputy_walker, kind_compound_stmt, deputy_compound_stmt);
  AST_walker_handle(deputy_walker, kind_implementation, deputy_implementation);
}
