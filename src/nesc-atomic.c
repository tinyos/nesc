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

static bool dirty;
static cgraph callgraph;

static atomic_t isatomic_stmt(statement stmt);

static atomic_t isatomic_children(void *n)
{
  atomic_t a = ATOMIC_ANY;

  AST_walk_children(isatomic_walker, &a, CAST(node, n));

  return a;
}

static atomic_t acall1(data_declaration ddecl)
{
  if (ddecl->definition)
    return CAST(function_decl, ddecl->definition)->stmt->isatomic;
  else
    return NOT_ATOMIC;
}

/* Return atomicness of possible targets of a command or event.
   This could be more accurate if we considered which things are
   dispatched vs multiply wired. But those cases seem unlikely to be
   atomic, so... */
static atomic_t acall_connected(data_declaration ddecl)
{
  gedge called;
  atomic_t a = ATOMIC_ANY;

  /* The nodes connected from a command or event node include all the
     possible targets of that command or event. They also include any
     things called or used from any default handler for the command or
     event */
  graph_scan_out (called, fn_lookup(callgraph, ddecl))
    if (EDGE_GET(use, called)->c & c_fncall)
      a = aseq(a, acall1(NODE_GET(endp, graph_edge_to(called))->function));

  return a;
}

/* Return atomicness of calls to 'ddecl' */
static atomic_t acall(data_declaration ddecl)
{
  atomic_t a;

  if (ddecl->kind != decl_function)
    return NOT_ATOMIC;

  if ((ddecl->ftype == function_command || ddecl->ftype == function_event) &&
      !ddecl->defined)
    {
      a = acall_connected(ddecl);
      if (!ddecl->definition || ddecl->suppress_definition) /* no default */
	return a;
    }
  else
    a = ATOMIC_ANY;

  return aalt(a, acall1(ddecl));
}

static atomic_t isatomic_expr(expression expr)
{
  atomic_t a = NOT_ATOMIC; /* Default to not-atomic */

  if (!expr)
    return ATOMIC_ANY;

  /* A read of an array-type expression actually takes the address
     of the container */
  if ((expr->type && type_array(expr->type) && expr->static_address) ||
      expr->cst)
    a = ATOMIC_ANY;
  else switch (expr->kind)
    {
    case kind_realpart: case kind_imagpart: case kind_unary_minus:
    case kind_unary_plus: case kind_conjugate: case kind_bitnot:
    case kind_not: case kind_plus: case kind_minus: case kind_times:
    case kind_divide: case kind_modulo: case kind_lshift: case kind_rshift:
    case kind_leq: case kind_geq: case kind_lt: case kind_gt: case kind_eq:
    case kind_ne: case kind_bitand: case kind_bitor: case kind_bitxor:
    case kind_andand: case kind_oror: case kind_assign: case kind_comma:
    case kind_extension_expr: case kind_compound_expr: case kind_cast:
    case kind_generic_call:
      a = isatomic_children(expr);
      break;

    case kind_identifier:
      a = avar(CAST(identifier, expr)->ddecl, expr->context);
      break;

    case kind_field_ref: {
      field_ref fref = CAST(field_ref, expr);

      a = isatomic_expr(fref->arg1);
      /* Bit field r/w's are not atomic. Could try and be more
	 optimistic (e.g., non-byte-boundary crossing bitfield reads
	 are presumably atomic) */
      if (!cval_istop(fref->fdecl->bitwidth))
	a = NOT_ATOMIC;
      break;
    }
    case kind_interface_deref:
      a = ATOMIC_ANY;
      break;

    case kind_conditional: {
      conditional ce = CAST(conditional, expr);
      atomic_t ac, a1, a2;

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
      break;
    }
	
    case kind_array_ref: {
      array_ref are = CAST(array_ref, expr);
      atomic_t a1, a2;

      a1 = isatomic_expr(are->arg1);
      a2 = isatomic_expr(are->arg2);
      a = aseq(a1, aseq(a2, aaccess(are->type)));
      break;
    }
    case kind_dereference:
      a = aseq(isatomic_children(expr), aaccess(expr->type));
      break;

    case kind_address_of: {
      expression arg = CAST(unary, expr)->arg1;

      /* Some heuristics for atomicity of & */
      while (is_field_ref(arg) || is_extension_expr(arg))
	arg = CAST(unary, arg)->arg1;

      if (is_dereference(arg))
	a = isatomic_expr(CAST(dereference, arg)->arg1);
      else if (is_identifier(arg))
	a = ATOMIC_ANY;
      else
	a = isatomic_expr(arg);
      break;
    }

    case kind_preincrement: case kind_postincrement:
    case kind_predecrement: case kind_postdecrement: {
      atomic_t a1 = isatomic_expr(CAST(increment, expr)->arg1);

      a = aseq(a1, a1);
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
      a = aseq(a2, aseq(a1, a1));
      break;
    }
    case kind_function_call: {
      function_call fce = CAST(function_call, expr);
      expression called = fce->arg1;

      if (is_generic_call(called))
	called = CAST(generic_call, called)->arg1;

      if (is_identifier(called))
	a = acall(CAST(identifier, called)->ddecl);
      else if (is_interface_deref(called))
	a = acall(CAST(interface_deref, called)->ddecl);
      else
	a = NOT_ATOMIC;

      a = aseq(a, isatomic_children(expr));
      break;
    }
    default:
      /* Just check children. Current statement not atomic. */
      isatomic_children(expr);
      break;
    }

  if (expr->isatomic != a)
    {
      expr->isatomic = a;
      dirty = TRUE;
    }

  return a;
}

static atomic_t isatomic_stmt(statement stmt)
{
  atomic_t a = NOT_ATOMIC; /* Default to not-atomic */

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
      a = isatomic_children(stmt);
      break;

    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);
      atomic_t ac, a1, a2;

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
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);
      atomic_t ac, as;

      ac = isatomic_expr(cs->condition);
      as = isatomic_stmt(cs->stmt);

      if (cs->condition->cst && stmt->kind == kind_while_stmt &&
	  definite_zero(cs->condition))
	a = ATOMIC_ANY;
      else
	a = amany(aseq(ac, as));
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);
      atomic_t a1, a2, a3, as;

      a1 = isatomic_expr(fs->arg1);
      a2 = isatomic_expr(fs->arg2);
      a3 = isatomic_expr(fs->arg3);
      as = isatomic_stmt(fs->stmt);

      if (fs->arg2 && fs->arg2->cst && definite_zero(fs->arg2))
	a = ATOMIC_ANY;
      else
	a = amany(aseq(a2, aseq(as, a3)));

      a = aseq(a1, a);
      break;
    }

    default:
      /* Just check children. Current statement not atomic. */
      isatomic_children(stmt);
      break;
    }

  if (stmt->isatomic != a)
    {
      stmt->isatomic = a;
      dirty = TRUE;
    }

  return a;
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

void isatomic(cgraph g)
{
  ggraph cg = cgraph_graph(g);
  gnode n;
  
  if (!nesc_optimise_atomic)
    return;

  callgraph = g;

  /* Fixed point search for function-atomicity */
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;
      if (fn->definition)
	CAST(function_decl, fn->definition)->stmt->isatomic = ATOMIC_ANY;
    }
  do
    {
      dirty = FALSE;
      graph_scan_nodes (n, cg)
	{
	  data_declaration fn = NODE_GET(endp, n)->function;
	  if (fn->definition)
	    isatomic_stmt(CAST(function_decl, fn->definition)->stmt);
	}
    }
  while (dirty);
}

void init_isatomic(void)
{
  isatomic_walker = new_AST_walker(permanent);
  AST_walker_handle(isatomic_walker, kind_expression, isatomic_ast_expr);
  AST_walker_handle(isatomic_walker, kind_statement, isatomic_ast_stmt);
  AST_walker_handle(isatomic_walker, kind_variable_decl, isatomic_ast_vdecl);
}
