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
#include "edit.h"
#include "semantics.h"
#include "stmt.h"
#include "constants.h"
#include "AST_utils.h"
#include "unparse.h"

/* Declare a new temporary that can be assigned a value of type t.
   Place the declaration at the start of block. 
   XXX: See discussion in types.c:tag2ast about the (lack of) correctness of
   this approach.
   Return it's declaration */
data_decl build_declaration(region r, struct environment *e,
			    type t, const char *name, expression init,
			    data_declaration *oddecl)
{
  struct data_declaration tempdecl;
  identifier_declarator id;
  variable_decl vd;
  data_decl dd;
  declarator tdeclarator;
  type_element tmodifiers;

  /* Compute real type, name */
  if (type_array(t))
    t = make_pointer_type(type_array_of(t));
  else if (type_function(t))
    t = make_pointer_type(t);
  /* Qualifiers must not be present on the temp (the qualifiers of t apply
     to the original location we are building a temp) */
  t = make_qualified_type(t, no_qualifiers);

  /* Build AST for the declaration */
  id = new_identifier_declarator(r, dummy_location, str2cstring(r, name));
  type2ast(r, dummy_location, t, CAST(declarator, id), &tdeclarator, &tmodifiers);
  vd = new_variable_decl(r, dummy_location, tdeclarator, NULL, init, NULL, NULL);
  vd->declared_type = t;
  dd = new_data_decl(r, dummy_location, tmodifiers, CAST(declaration, vd));

  if (e) /* Declare the variable */
    {
      init_data_declaration(&tempdecl, CAST(declaration, vd), id->cstring.data, t);
      tempdecl.kind = decl_variable;
      tempdecl.vtype = variable_normal;
      tempdecl.islocal = TRUE;
      *oddecl = vd->ddecl = declare(e, &tempdecl, FALSE);
    }

  return dd;
}

#define TEMP_PREFIX "__nesc_temp"

char *next_temporary(void)
{
  static long nextid = 42;
  static char idname[sizeof(TEMP_PREFIX) + 20];

  sprintf(idname, TEMP_PREFIX "%ld", nextid++);

  return idname;
}

/* Declare a new temporary that can be assigned a value of type t.
   Place the declaration at the start of block. 
   Return it's declaration */
data_declaration add_temporary(region r, compound_stmt block, type t)
{
  const char *name = next_temporary();
  data_decl dd = build_declaration(r, NULL, t, name, NULL, NULL);
  struct data_declaration tempdecl;
  data_declaration ddecl;

  /* Add to the function's declarations */
  dd->next = CAST(node, block->decls);
  block->decls = CAST(declaration, dd);

#if 0
  /* Set parent pointers */
  AST_set_parents(CAST(node, dd));
  dd->parent = CAST(node, block);
  dd->parent_ptr = CASTSRPTR(node, &block->decls);
  if (dd->next)
    dd->next->parent_ptr = &dd->next;
#endif

  /* Declare the variable */
  init_data_declaration(&tempdecl, dd->decls, rstrdup(r, name), t);
  tempdecl.kind = decl_variable;
  tempdecl.vtype = variable_normal;
  tempdecl.islocal = TRUE;
  ddecl = declare(block->env, &tempdecl, FALSE);
  CAST(variable_decl, dd->decls)->ddecl = ddecl;

  return ddecl;
}

word build_word(region r, const char *cword)
{
  return new_word(r, dummy_location, str2cstring(r, cword));
}

expression build_string(region r, location loc, const char *str)
{
  string_cst elems = new_string_cst(r, loc, str2cstring(r, "oops"));
  data_declaration sdecl = declare_string(NULL, str2cstring(r, str), FALSE);
  string s = new_string(r, loc, elems, sdecl);

  s->type = sdecl->type;
  s->static_address = foldaddress_string(s);
  s->lvalue = TRUE;

  return CAST(expression, s);
}

expression build_function_call(region r, location loc,
			       expression fn, expression arglist)
{
  expression result = CAST(expression, new_function_call(r, loc, fn, arglist, NULL, normal_call));
  type fntype = type_default_conversion(fn->type), rettype;

  if (type_pointer(fntype))
    /* All function types come this way because default_conversion makes
       them into pointers to functions... */
    fntype = type_points_to(fntype);

  rettype = type_function_return_type(fntype);
  result->type = rettype;
  result->cst = fold_function_call(result, 0);

  return result;
}

