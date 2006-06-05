/* This file is part of the nesC compiler.
   Copyright (C) 2002-2006 Intel Corporation

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
#include "AST_walk.h"
#include "nesc-cg.h"
#include "constants.h"
#include "nesc-atomic.h"

static atomic_t aseq(atomic_t a1, atomic_t a2)
{
  if (a1 == NOT_ATOMIC || a2 == NOT_ATOMIC)
    return NOT_ATOMIC;
  if (a2 == ATOMIC_ANY)
    return a1;
  if (a1 == ATOMIC_ANY)
    return a2;
  return NOT_ATOMIC;
}

static atomic_t aalt(atomic_t a1, atomic_t a2)
{
  if (a1 == NOT_ATOMIC || a2 == NOT_ATOMIC)
    return NOT_ATOMIC;
  if (a1 == ATOMIC_SINGLE || a2 == ATOMIC_SINGLE)
    return ATOMIC_SINGLE;
  return ATOMIC_ANY;
}

static atomic_t amany(atomic_t a)
{
  if (a == ATOMIC_SINGLE)
    return NOT_ATOMIC;
  else
    return a;
}

static atomic_t aaccess(type t)
{
  /* Single byte values have atomic read/write */
  if (type_size_cc(t))
    {
      cval tsize = type_size(t);

      if (cval_isinteger(tsize) && cval_uint_value(tsize) == 1)
	return ATOMIC_SINGLE;
    }
  return NOT_ATOMIC;
}

static atomic_t avar(data_declaration ddecl, context this_use)
{
  context bad_contexts;

  if (ddecl->kind != decl_variable)
    return ATOMIC_ANY;

  /* Special-case locals: we don't set async_access on locals even if their
     address is taken (see nesc-concurrency.c). So the test below is not
     reliable. */
  if (ddecl->islocal)
    return ddecl->use_summary & c_addressed ? NOT_ATOMIC : ATOMIC_ANY;

  if (!ddecl->async_access)
    return ATOMIC_ANY;

  /* Figure out dangerous accesses: If there are no writes in async
     contexts, then reads need not be protected */
  bad_contexts = c_write;
  if (ddecl->async_write)
    bad_contexts |= c_read;

  if (this_use & bad_contexts)
    return aaccess(ddecl->type);
  else
    return ATOMIC_ANY;
}

/**
 * This code computes a conservative approximation of statements whose
 * execution is guaranteed to be atomic, allowing optimisation of
 * simple atomic statements.
 *
 * Currently it assumes that the only memory accesses that are atomic are
 * single-byte read/writes.  It could be extended with platform-specific
 * knowledge of larger atomic r/w units. Platform-specific knowledge would
 * also allow optimisation of statements such as atomic x |= 4 if it is
 * known that this maps to a single instruction.
 */

/* An AST walker that isatomics all reachable expressions and statements */
static AST_walker isatomic_walker;

static atomic_t isatomic_stmt(statement stmt);

static atomic_t isatomic_children(void *n)
{
  atomic_t a = ATOMIC_ANY;

  AST_walk_children(isatomic_walker, &a, CAST(node, n));

  return a;
}

static atomic_t isatomic_expr(expression expr)
{
  if (!expr)
    return ATOMIC_ANY;

  /* A read of an array-type expression actually takes the address
     of the container */
  if ((expr->type && type_array(expr->type) && expr->static_address) ||
      expr->cst)
    expr->isatomic = ATOMIC_ANY;
  else
    switch (expr->kind)
      {
      case kind_realpart: case kind_imagpart: case kind_unary_minus:
      case kind_unary_plus: case kind_conjugate: case kind_bitnot:
      case kind_not: case kind_plus: case kind_minus: case kind_times:
      case kind_divide: case kind_modulo: case kind_lshift: case kind_rshift:
      case kind_leq: case kind_geq: case kind_lt: case kind_gt: case kind_eq:
      case kind_ne: case kind_bitand: case kind_bitor: case kind_bitxor:
      case kind_andand: case kind_oror: case kind_assign: case kind_comma:
      case kind_extension_expr: case kind_compound_expr: case kind_cast:
	expr->isatomic = isatomic_children(expr);
	break;

      case kind_identifier:
	expr->isatomic = avar(CAST(identifier, expr)->ddecl, expr->context);
	break;

      case kind_field_ref: {
	field_ref fref = CAST(field_ref, expr);

	expr->isatomic = isatomic_expr(fref->arg1);
	/* Bit field r/w's are not atomic. Could try and be more
	   optimistic (e.g., non-byte-boundary crossing bitfield reads
	   are presumably atomic) */
	if (!cval_istop(fref->fdecl->bitwidth))
	  expr->isatomic = NOT_ATOMIC;
	break;
      }
      case kind_interface_deref:
	expr->isatomic = ATOMIC_ANY;
	break;

      case kind_conditional: {
	conditional ce = CAST(conditional, expr);
	atomic_t a, ac, a1, a2;

	ac = isatomic_expr(ce->condition);
	a1 = isatomic_expr(ce->arg1);
	a2 = isatomic_expr(ce->arg2);

	if (ce->condition->cst)
	  {
	    if (definite_zero(ce->condition))
	      a = a2;
	    else
	      a = a1;
	  }
	else
	  a = aseq(ac, aalt(a1, a2));

	expr->isatomic = a;
	break;
      }
	
      case kind_array_ref: {
	array_ref are = CAST(array_ref, expr);
	atomic_t a1, a2;

	a1 = isatomic_expr(are->arg1);
	a2 = isatomic_expr(are->arg2);
	expr->isatomic = aseq(a1, aseq(a2, aaccess(are->type)));
	break;
      }
      case kind_dereference:
	expr->isatomic = aseq(isatomic_children(expr), aaccess(expr->type));
	break;

      case kind_address_of: {
	expression arg = CAST(unary, expr)->arg1;

	/* Some heuristics for atomicity of & */
	while (is_field_ref(arg) || is_extension_expr(arg))
	  arg = CAST(unary, arg)->arg1;

	if (is_dereference(arg))
	  expr->isatomic = isatomic_expr(CAST(dereference, arg)->arg1);
	else if (is_identifier(arg))
	  expr->isatomic = ATOMIC_ANY;
	else
	  expr->isatomic = isatomic_expr(arg);
	break;
      }

      case kind_preincrement: case kind_postincrement:
      case kind_predecrement: case kind_postdecrement: {
	atomic_t a = isatomic_expr(CAST(increment, expr)->arg1);

	expr->isatomic = aseq(a, a);
	break;
      }
      case kind_plus_assign: case kind_minus_assign: 
      case kind_times_assign: case kind_divide_assign: case kind_modulo_assign:
      case kind_bitand_assign: case kind_bitor_assign: case kind_bitxor_assign:
      case kind_lshift_assign: case kind_rshift_assign: {
	assign aexpr = CAST(assignment, expr);
	atomic_t a1, a2;

	a1 = isatomic_expr(aexpr->arg1);
	a2 = isatomic_expr(aexpr->arg2);
	expr->isatomic = aseq(a2, aseq(a1, a1));
	break;
      }
      default:
	/* Default to not-atomic */
	isatomic_children(expr);
	expr->isatomic = NOT_ATOMIC;
	break;
      }

  return expr->isatomic;
}

static atomic_t isatomic_stmt(statement stmt)
{
  if (!stmt)
    return ATOMIC_ANY;

  switch (stmt->kind)
    {
    case kind_compound_stmt:
    case kind_labeled_stmt:
    case kind_expression_stmt:
    case kind_atomic_stmt:
    case kind_break_stmt:
    case kind_continue_stmt:
    case kind_goto_stmt:
    case kind_computed_goto_stmt:
    case kind_empty_stmt:
    case kind_return_stmt:
      stmt->isatomic = isatomic_children(stmt);
      break;

    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);
      atomic_t a, ac, a1, a2;

      ac = isatomic_expr(is->condition);
      a1 = isatomic_stmt(is->stmt1);
      a2 = isatomic_stmt(is->stmt2);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    a = a2;
	  else
	    a = a1;
	}
      else
	a = aseq(ac, aalt(a1, a2));

      stmt->isatomic = a;
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);
      atomic_t ac, as, a;

      ac = isatomic_expr(cs->condition);
      as = isatomic_stmt(cs->stmt);

      if (cs->condition->cst && stmt->kind == kind_while_stmt &&
	  definite_zero(cs->condition))
	a = ATOMIC_ANY;
      else
	a = amany(aseq(ac, as));

      stmt->isatomic = a;
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);
      atomic_t a, a1, a2, a3, as;

      a1 = isatomic_expr(fs->arg1);
      a2 = isatomic_expr(fs->arg2);
      a3 = isatomic_expr(fs->arg3);
      as = isatomic_stmt(fs->stmt);

      if (fs->arg2 && fs->arg2->cst && definite_zero(fs->arg2))
	a = ATOMIC_ANY;
      else
	a = amany(aseq(a2, aseq(as, a3)));

      stmt->isatomic = aseq(a1, a);
      break;
    }

    default:
      /* Default to not-atomic */
      isatomic_children(stmt);
      stmt->isatomic = NOT_ATOMIC;
      break;
    }
  return stmt->isatomic;
}

static AST_walker_result isatomic_ast_expr(AST_walker spec, void *data,
					   expression *e)
{
  atomic_t *a = data;
  *a = aseq(*a, isatomic_expr(*e));
  return aw_done;
}

static AST_walker_result isatomic_ast_stmt(AST_walker spec, void *data,
					   statement *s)
{
  atomic_t *a = data;
  *a = aseq(*a, isatomic_stmt(*s));
  return aw_done;
}

static AST_walker_result isatomic_ast_vdecl(AST_walker spec, void *data,
					    variable_decl *vdp)
{
  variable_decl vd = *vdp;
  atomic_t *a = data, ainit;

  if (vd->arg1 && vd->ddecl->islocal)
    {
      ainit = isatomic_expr(vd->arg1);
      *a = aseq(*a, aseq(ainit, avar(vd->ddecl, c_write)));
    }
  return aw_done;
}

void isatomic(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  /* We need the data-race information to optimise atomics */
  if (!nesc_optimise_atomic || !warn_data_race)
    return;

  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      if (fn->definition)
	isatomic_stmt(CAST(function_decl, fn->definition)->stmt);
    }
}

void init_isatomic(void)
{
  isatomic_walker = new_AST_walker(permanent);
  AST_walker_handle(isatomic_walker, kind_expression, isatomic_ast_expr);
  AST_walker_handle(isatomic_walker, kind_statement, isatomic_ast_stmt);
  AST_walker_handle(isatomic_walker, kind_variable_decl, isatomic_ast_vdecl);
}
