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
#include "nesc-inline.h"
#include "constants.h"

static size_t statement_size(statement stmt);
static size_t expression_size(expression expr);

static size_t elist_size(expression elist)
{
  expression e;
  size_t sum = 0;

  scan_expression (e, elist)
    sum += expression_size(e);

  return sum;
}


static size_t expression_size(expression expr)
{
  size_t sum = 0;

  if (!expr)
    return 0;

  if (expr->cst || is_string(expr))
    return 1;

  switch (expr->kind)
    {
    case kind_identifier: 
      sum += 1;
      break;

    case kind_comma:
      sum += elist_size(CAST(comma, expr)->arg1);
      break;

    case kind_cast_list: {
      sum += expression_size(CAST(cast_list, expr)->init_expr);
      break;
    }
    case kind_init_index: {
      init_index init = CAST(init_index, expr);

      sum += expression_size(init->init_expr);
      break;
    }
    case kind_init_field: {
      init_field init = CAST(init_field, expr);

      sum += expression_size(init->init_expr);
      break;
    }
    case kind_init_list: {
      sum += elist_size(CAST(init_list, expr)->args);
      break;
    }
    case kind_conditional: {
      conditional ce = CAST(conditional, expr);

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    sum += expression_size(ce->arg2);
	  else
	    sum += expression_size(ce->arg1);
	}
      else
	{
	  sum += 2 + expression_size(ce->condition);
	  sum += expression_size(ce->arg1);
	  sum += expression_size(ce->arg2);
	}
      break;
    }
    case kind_compound_expr:
      sum += statement_size(CAST(compound_expr, expr)->stmt);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      sum += 1 + expression_size(fce->arg1);
      sum += elist_size(fce->args);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      sum += 1 + expression_size(fce->arg1);
      sum += elist_size(fce->args);
      break;
    }
    case kind_extension_expr:
      sum += expression_size(CAST(unary, expr)->arg1);
      break;

    default:
      if (is_unary(expr))
	sum += 1 + expression_size(CAST(unary, expr)->arg1);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);

	  sum += 1 + expression_size(be->arg1);
	  sum += expression_size(be->arg2);
	}
      else 
	assert(0);
      break;
    }

  return sum;
}

static size_t statement_size(statement stmt)
{
  size_t sum = 0;

  if (!stmt)
    return 0;

  switch (stmt->kind)
    {
    case kind_asm_stmt: {
      sum += 1;
      break;
    }
    case kind_compound_stmt: {
      compound_stmt cs = CAST(compound_stmt, stmt);
      statement s;
      declaration d;

      scan_declaration (d, cs->decls)
	if (is_data_decl(d))
	  {
	    variable_decl vd;

	    /* Include size of initialisers of non-static variables */
	    scan_variable_decl (vd, CAST(variable_decl,
					 CAST(data_decl, d)->decls))
	      if (vd->ddecl->kind == decl_variable &&
		  vd->ddecl->vtype != variable_static)
		sum += 1 + expression_size(vd->arg1);
	  }

      scan_statement (s, cs->stmts)
	sum += statement_size(s);
      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    sum += statement_size(is->stmt2);
	  else
	    sum += statement_size(is->stmt1);
	}
      else
	{
	  sum += 2 + expression_size(is->condition);
	  sum += statement_size(is->stmt1);
	  sum += statement_size(is->stmt2);
	}
      break;
    }
    case kind_labeled_stmt: {
      labeled_stmt ls = CAST(labeled_stmt, stmt);

      sum += statement_size(ls->stmt);
      break;
    }
    case kind_expression_stmt: {
      expression_stmt es = CAST(expression_stmt, stmt);

      sum += expression_size(es->arg1);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);

      if (cs->condition->cst && stmt->kind != kind_switch_stmt &&
	  definite_zero(cs->condition))
	{
	  /* do s while (0): just include size of s
	     while (0) s: size is 0 */
	  if (stmt->kind == kind_dowhile_stmt)
	    sum += statement_size(cs->stmt);
	  break;
	}
      sum += 2 + expression_size(cs->condition);
      sum += statement_size(cs->stmt);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      sum += 2 + statement_size(fs->stmt);
      sum += expression_size(fs->arg1);
      sum += expression_size(fs->arg2);
      sum += expression_size(fs->arg3);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      sum += 1;
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      sum += 1 + expression_size(cgs->arg1);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);

      sum += 1 + expression_size(rs->arg1);
      break;
    }
    default: assert(0);
    }

  return sum;
}

static size_t function_size(function_decl fd)
{
  return statement_size(fd->stmt);
}

void inline_functions(cgraph callgraph)
{
  ggraph g = cgraph_graph(callgraph);
  gnode n;

  graph_scan_nodes (n, g)
    {
      endp fn = NODE_GET(endp, n);
      data_declaration fndecl = fn->function;

      if (!fndecl->definition)
	fndecl->makeinline = TRUE;
      else if (!fndecl->isinline)
	{
	  gedge e;
	  size_t edgecount = 0,
	    fnsize = function_size(CAST(function_decl, fndecl->definition));

	  graph_scan_in (e, n)
	    edgecount++;
      
	  if (fnsize <= 15 || edgecount == 1 && fnsize <= 100)
	    fndecl->makeinline = TRUE;
	}
    }
}

