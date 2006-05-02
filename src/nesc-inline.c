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
#include "flags.h"

enum {
  base_inlineable_size = 9,
  inline_per_arg = 2
};

struct inline_node
{
  data_declaration fn;
  size_t size;
  bool uninlinable;
};

static void ig_add_fn(region r, ggraph ig, data_declaration fn, size_t size)
{
  struct inline_node *n = ralloc(r, struct inline_node);

  n->fn = fn;
  n->size = size;
  fn->ig_node = graph_add_node(ig, n);
}

static void ig_add_edge(data_declaration from, data_declaration to)
{
  graph_add_edge(from->ig_node, to->ig_node, NULL);

  /* Recursion - mark as uninlinable */
  if (from == to)
    {
      struct inline_node *n = NODE_GET(struct inline_node *, from->ig_node);

      n->uninlinable = TRUE;
    }
}

static size_t statement_size(statement stmt, bool inatomic);
static size_t expression_size(expression expr, bool inatomic);

static size_t elist_size(expression elist, bool inatomic)
{
  expression e;
  size_t sum = 0;

  scan_expression (e, elist)
    sum += expression_size(e, inatomic);

  return sum;
}


static size_t expression_size(expression expr, bool inatomic)
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
      sum += elist_size(CAST(comma, expr)->arg1, inatomic);
      break;

    case kind_cast_list: {
      sum += expression_size(CAST(cast_list, expr)->init_expr, inatomic);
      break;
    }
    case kind_init_specific: {
      init_specific init = CAST(init_specific, expr);

      sum += expression_size(init->init_expr, inatomic);
      break;
    }
    case kind_init_list: {
      sum += elist_size(CAST(init_list, expr)->args, inatomic);
      break;
    }
    case kind_conditional: {
      conditional ce = CAST(conditional, expr);

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    sum += expression_size(ce->arg2, inatomic);
	  else
	    sum += expression_size(ce->arg1, inatomic);
	}
      else
	{
	  sum += 2 + expression_size(ce->condition, inatomic);
	  sum += expression_size(ce->arg1, inatomic);
	  sum += expression_size(ce->arg2, inatomic);
	}
      break;
    }
    case kind_compound_expr:
      sum += statement_size(CAST(compound_expr, expr)->stmt, inatomic);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      sum += 1 + expression_size(fce->arg1, inatomic);
      sum += elist_size(fce->args, inatomic);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      sum += 1 + expression_size(fce->arg1, inatomic);
      sum += elist_size(fce->args, inatomic);
      break;
    }
    case kind_extension_expr:
      sum += expression_size(CAST(unary, expr)->arg1, inatomic);
      break;

    default:
      if (is_unary(expr))
	sum += 1 + expression_size(CAST(unary, expr)->arg1, inatomic);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);

	  sum += 1 + expression_size(be->arg1, inatomic);
	  sum += expression_size(be->arg2, inatomic);
	}
      else 
	assert(0);
      break;
    }

  return sum;
}

static size_t statement_size(statement stmt, bool inatomic)
{
  size_t sum = 0;

  if (!stmt)
    return 0;

  switch (stmt->kind)
    {
    case kind_asm_stmt: {
      sum += 1; /* A guess. Hard to count asm instructions. */
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
		sum += 1 + expression_size(vd->arg1, inatomic);
	  }

      scan_statement (s, cs->stmts)
	sum += statement_size(s, inatomic);
      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    sum += statement_size(is->stmt2, inatomic);
	  else
	    sum += statement_size(is->stmt1, inatomic);
	}
      else
	{
	  sum += 2 + expression_size(is->condition, inatomic);
	  sum += statement_size(is->stmt1, inatomic);
	  sum += statement_size(is->stmt2, inatomic);
	}
      break;
    }
    case kind_labeled_stmt: {
      labeled_stmt ls = CAST(labeled_stmt, stmt);

      sum += statement_size(ls->stmt, inatomic);
      break;
    }
    case kind_atomic_stmt: {
      atomic_stmt ls = CAST(atomic_stmt, stmt);

      sum += statement_size(ls->stmt, inatomic);
      if (!inatomic)
	sum += 6;
      break;
    }
    case kind_expression_stmt: {
      expression_stmt es = CAST(expression_stmt, stmt);

      sum += expression_size(es->arg1, inatomic);
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
	    sum += statement_size(cs->stmt, inatomic);
	  break;
	}
      sum += 2 + expression_size(cs->condition, inatomic);
      sum += statement_size(cs->stmt, inatomic);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      sum += 2 + statement_size(fs->stmt, inatomic);
      sum += expression_size(fs->arg1, inatomic);
      sum += expression_size(fs->arg2, inatomic);
      sum += expression_size(fs->arg3, inatomic);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      sum += 1;
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      sum += 1 + expression_size(cgs->arg1, inatomic);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);

      sum += 1 + expression_size(rs->arg1, inatomic);
      break;
    }
    default: assert(0);
    }

  return sum;
}

static size_t function_size(function_decl fd)
{
  return statement_size(fd->stmt, fd->ddecl->call_contexts == c_call_atomic);
}

static size_t typelist_length(typelist tl)
{
  typelist_scanner tls;
  size_t count = 0;

  if (!tl)
    return 0;
  
  typelist_scan(tl, &tls);
  while (typelist_next(&tls))
    count++;

  return count;
}

static size_t function_argcount(data_declaration ddecl)
{
  size_t count = typelist_length(type_function_arguments(ddecl->type));

  if (type_generic(ddecl->type))
    count += typelist_length(type_function_arguments(type_function_return_type(ddecl->type)));

  return count;
}

static ggraph make_ig(region r, cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  ggraph ig = new_graph(r);
  gnode n;

  graph_scan_nodes (n, cg)
    {
      /* Size should be at least 1 (because of the -1 in inline_function) */
      data_declaration fn = NODE_GET(endp, n)->function;
      size_t fnsize = 1;

      if (fn->definition)
	fnsize = function_size(CAST(function_decl, fn->definition));

      if (fn->interface && !fn->defined)
	{
	  /* stub function. size is based on number of outgoing edges and
	     number of parameters (first outgoing edge counted as "free") */
	  gedge e;
	  int edgecount = 0;

	  graph_scan_out (e, n)
	    edgecount++;
	  /* use size of default definition (already computed above) if no
	     outgoing edges */
	  if (edgecount > 0)
	    fnsize = (edgecount - 1) * (2 + function_argcount(fn)) + 1;
	}

      ig_add_fn(r, ig, fn, fnsize);
    }
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;
      gedge e;

      graph_scan_out (e, n)
	ig_add_edge(fn, NODE_GET(endp, graph_edge_to(e))->function);
    }
  return ig;
}

static void inline_function(gnode n, struct inline_node *in)
{
  gedge call_edge, called_edge, next_edge;

  if (in->uninlinable || in->fn->noinlinep || in->fn->spontaneous) 
    return;

  in->fn->makeinline = TRUE;
  /* Add callgraph edges implied by this inlining and update caller
     sizes */
  graph_scan_in (call_edge, n)
    {
      gnode caller = graph_edge_from(call_edge);
      struct inline_node *caller_in = NODE_GET(struct inline_node *, caller);
      
      caller_in->size += in->size - 1;

      graph_scan_out (called_edge, n)
	{
	  gnode called = graph_edge_to(called_edge);

	  ig_add_edge(caller_in->fn,
		      NODE_GET(struct inline_node *, called)->fn);
	}
    }

  /* Remove edges leaving inlined function 
     (we don't bother removing incoming edges as we won't look at them
     again) */
  called_edge = graph_first_edge_out(n);
  while (called_edge)
    {
      next_edge = graph_next_edge_out(called_edge);
      graph_remove_edge(called_edge);
      called_edge = next_edge;
    }
}

void inline_functions(cgraph callgraph)
{
  region igr = newregion();
  ggraph ig = make_ig(igr, callgraph);
  gnode n;
  int bis = base_inlineable_size, ipa = inline_per_arg;

  if (getenv("NESC_BIS"))
    bis = atoi(getenv("NESC_BIS"));
  if (getenv("NESC_IPA"))
    ipa = atoi(getenv("NESC_IPA"));

  if (flag_no_inline)
    return;

  /* Inline small fns and single-call fns */
  graph_scan_nodes (n, ig)
    {
      struct inline_node *in = NODE_GET(struct inline_node *, n);
      
      if (!in->fn->isinline && !in->fn->makeinline)
	{
	  gedge e;
	  size_t edgecount = 0;

	  graph_scan_in (e, n)
	    edgecount++;
      
	  if (edgecount == 1 ||
	      (bis >=0 && in->size <= bis + function_argcount(in->fn) * ipa))
	    inline_function(n, in);
	}
    }
  deleteregion(igr);
}
