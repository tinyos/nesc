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
#include "nesc-cg.h"
#include "nesc-abstract.h"
#include "c-parse.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "AST_walk.h"
#include "semantics.h"
#include "constants.h"
#include "init.h"
#include "stmt.h"

static AST_walker folder_walker;

struct folder_data {
  bool *done;
  int pass;
};

static AST_walker_result folder_expression(AST_walker spec, void *data,
					   expression *n)
{
  struct folder_data *d = data;
  expression e = *n;
  known_cst c = NULL, sa = NULL;
  
  /* Constant-fold children first */
  AST_walk_children(spec, data, CAST(node, e));

  /* XXX: default_conversion */

  switch (e->kind)
    {
    case kind_lexical_cst: case kind_string: case kind_extension_expr:
      /* We preserve the constants in lexical_cst's and strings */
      /* XXX: should we allow string arguments to components to 
	 be merged into strings (e.g. "aa" foo "bb", where foo
	 is a `char *' component arg)? 
	 (If so: the ddecl for the args should be classified as
         a decl_magic_string, and make_string and this function must be
	 modified accordingly) */
      c = e->cst;
      sa = e->static_address;
      break;
    case kind_label_address:
      c = fold_label_address(e);
      break;
    case kind_sizeof_expr: 
      c = fold_sizeof(e, CAST(sizeof_expr, e)->arg1->type);
      break;
    case kind_sizeof_type:
      c = fold_sizeof(e, CAST(sizeof_type, e)->asttype->type);
      break;
    case kind_alignof_expr: 
      c = fold_alignof(e, CAST(alignof_expr, e)->arg1->type);
      break;
    case kind_alignof_type:
      c = fold_alignof(e, CAST(alignof_type, e)->asttype->type);
      break;
    case kind_cast:
      c = fold_cast(e);
      sa = CAST(cast, e)->arg1->static_address;
      break;
    case kind_conditional:
      c = fold_conditional(e);
      break;
    case kind_function_call:
      c = fold_function_call(e, d->pass);
      break;
    case kind_identifier:
      c = fold_identifier(e, CAST(identifier, e)->ddecl, d->pass);
      sa = foldaddress_identifier(e, CAST(identifier, e)->ddecl);
      break;
    case kind_field_ref:
      sa = foldaddress_field_ref(e);
      break;
    case kind_dereference:
      sa = CAST(dereference, e)->arg1->cst;
      break;
    case kind_address_of:
      c = CAST(address_of, e)->arg1->static_address;
      break;
    case kind_array_ref: {
      array_ref aref = CAST(array_ref, e);
      type atype;

      /* Find the array type */
      if (type_integer(aref->arg1->type))
	atype = aref->arg2->type;
      else
	atype = aref->arg1->type;

      sa = fold_binary(type_default_conversion(atype), e);
      break;
    }
    case kind_comma: {
      expression sub;;

      scan_expression (sub, CAST(comma, e)->arg1)
	if (!sub->cst)
	  break;
	else if (!sub->next)
	  {
	    /* (e1, ..., en) is a constant expression if all ei are constant
	       expressions. Weird? (see cst10.c) */
	    c = sub->cst;
	  }
      break;
    }
    case kind_component_deref:
      c = fold_identifier(e, CAST(component_deref, e)->ddecl, d->pass);
      break;
    default:
      if (is_binary(e))
	c = fold_binary(e->type, e);
      else if (is_unary(e))
	c = fold_unary(e);
      break;
    }
  e->cst = c;
  e->static_address = sa;

  /* Handle default conversions to pointers */
  if (e->converted_to_pointer)
    e->cst = sa;

  /* Notice unknown csts */
  if ((sa && constant_unknown(sa)) || (c && constant_unknown(c)))
    *d->done = FALSE;

  if (e->ivalue && e->ivalue->kind == iv_base &&
      e->ivalue->u.base.require_constant_value)
    check_init_element(e);

  return aw_done;
}

bool fold_constants_list(node n, int pass)
/* Effects: Folds constants and lays out types in AST n
   Returns: FALSE if any constant folding op returned an unknown cst, TRUE
     otherwise
 */
{
  struct folder_data d;
  bool done = TRUE;

  d.done = &done;
  d.pass = pass;

  AST_walk_list(folder_walker, &d, CASTPTR(node, &n));

  return done;
}

static AST_walker_result folder_array_declarator(AST_walker spec, void *data,
						 array_declarator *n)
{
  expression size = (*n)->arg1;

  AST_walk_children(spec, data, CAST(node, *n));

  if (size)
    check_array_size(size, nice_declarator_name((*n)->declarator));

  return aw_done;
}

static AST_walker_result folder_enum_ref(AST_walker spec, void *data,
					 enum_ref *n)
{
  if (!(*n)->defined)
    return aw_walk;

  layout_enum_start((*n)->tdecl);
  AST_walk_children(spec, data, CAST(node, *n));
  layout_enum_end((*n)->tdecl);
  
  return aw_done;
}

static AST_walker_result folder_enumerator(AST_walker spec, void *data,
					   enumerator *n)
{
  enumerator e = *n;

  AST_walk_children(spec, data, CAST(node, e));
  e->ddecl->value = layout_enum_value(e);

  return aw_done;
}

static AST_walker_result folder_tag_ref(AST_walker spec, void *data,
					tag_ref *n)
{
  if (!(*n)->defined)
    return aw_walk;

  AST_walk_children(spec, data, CAST(node, *n));
  layout_struct((*n)->tdecl);

  return aw_done;
}

static AST_walker_result folder_case_label(AST_walker spec, void *data,
					   case_label *n)
{
  case_label label = *n;

  AST_walk_children(spec, data, CAST(node, *n));
  check_case_value(label->arg1);
  if (label->arg2) 
    check_case_value(label->arg2);

  return aw_done;
}

static AST_walker_result folder_function_decl(AST_walker spec, void *data,
					      function_decl *n)
{
  function_decl fd = *n, old = current.function_decl;

  current.function_decl = fd;
  AST_walk_children(spec, data, CAST(node, *n));
  current.function_decl = old;

  return aw_done;
}

void init_nesc_constants(void)
{
  folder_walker = new_AST_walker(permanent);
  AST_walker_handle(folder_walker, kind_expression, folder_expression);
  AST_walker_handle(folder_walker, kind_array_declarator, folder_array_declarator);
  AST_walker_handle(folder_walker, kind_tag_ref, folder_tag_ref);
  AST_walker_handle(folder_walker, kind_enum_ref, folder_enum_ref);
  AST_walker_handle(folder_walker, kind_enumerator, folder_enumerator);
  AST_walker_handle(folder_walker, kind_case_label, folder_case_label);
  AST_walker_handle(folder_walker, kind_function_decl, folder_function_decl);
}
