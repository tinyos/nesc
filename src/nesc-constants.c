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

static AST_walker folder_walker;

static AST_walker_result folder_expression(AST_walker spec, void *data,
					   expression *n)
{
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
      c = fold_function_call(e);
      break;
    case kind_identifier:
      c = fold_identifier(e, CAST(identifier, e)->ddecl);
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

  return aw_done;
}

void fold_constants_list(node n)
/* Effects: Folds constants and lays out types in AST n
 */
{
  AST_walk_list(folder_walker, NULL, CASTPTR(node, &n));
}

void init_nesc_constants(void)
{
  folder_walker = new_AST_walker(permanent);
  AST_walker_handle(folder_walker, kind_expression, folder_expression);
}
