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
Boston, MA 02111-1307, USA. */

#include "parser.h"
#include "constants.h"
#include "nesc-uses.h"
#include "nesc-semantics.h"
#include "AST_utils.h"
#include "AST_walk.h"
#include "c-parse.h"

static region rr;
dd_list nglobal_uses;
data_declaration enable_interrupt;

/* IDEAS: track fields of structs
 */

context exe_context(context c)
{
  return c & (c_atomic | c_executable | c_constant);
}

static context access_context(context c)
{
  /* Something that came via a deref is really just a read */
  if (c & c_deref)
    c = exe_context(c) | c_read;

  /* Constant contexts don't read or write. But they might address... */
  if (c & c_constant)
    c &= ~(c_read | c_write);

  return c;
}

static context use_context(context c)
{
  /* Non-executable contexts do not actually "do" anything. So
     don't set those bits in the 'use' context */
  if (!(c & c_executable))
    return 0;

  return access_context(c);
}

use new_use(location l, data_declaration fn, context c)
{
  use u = ralloc(rr, struct use);

  u->l = l;
  u->fn = fn;
  u->c = use_context(c);

  return u;
}

static iduse new_iduse(data_declaration id, use u)
{
  iduse i = ralloc(rr, struct iduse);

  i->id = id;
  i->u = u;

  return i;
}

void ddecl_used(data_declaration id, use u)
{
  iduse i = new_iduse(id, u);

  if (!id->nuses)
    id->nuses = dd_new_list(rr);
  dd_add_last(rr, id->nuses, u);
  id->use_summary |= u->c;

  if (u->fn)
    {
      if (!u->fn->fn_uses)
	u->fn->fn_uses = dd_new_list(rr);
      dd_add_last(rr, u->fn->fn_uses, i);
    }
  else
    dd_add_last(rr, nglobal_uses, i);
}

static void identifier_used(identifier id, data_declaration fn, context c)
{
  ddecl_used(id->ddecl, new_use(id->location, fn, c));
}

static void interface_used(interface_deref iref, data_declaration fn, context c)
{
  ddecl_used(iref->ddecl, new_use(iref->location, fn, c));
}

static void collect_uses_ast(void *n, data_declaration fn, context c);
static void collect_uses_children(void *n, data_declaration fn, context c);
static void collect_uses_stmt(statement stmt, data_declaration fn, context c);
static void collect_uses_expr(expression expr, data_declaration fn, context c);


static void collect_uses_deref(expression expr, expression dereferenced,
			       data_declaration fn, context c)
{
  c = use_context(c);

  if (c & c_addressed) /* &*x is just x, not a pointer deref */
    c &= ~c_addressed;
  else
    {
      c |= c_deref;
      /*pointer_use(expr, c);*/
    }
  collect_uses_expr(dereferenced, fn, c);
}

static void collect_uses_expr(expression expr, data_declaration fn, context c)
{
  context exe_c = exe_context(c);

  if (!expr)
    return;

  /* A read of an array-type expression actually takes the address
     of the container 
  XXX: test on type needed because we have some unhandled nodes such
  as init_list (Note: these may not have a type in the future too. They
  probably shouldn't be expressions anyway) */
  if (expr->type && type_array(expr->type))
    {
      /* A dereference of an array type means that the context operation
	 is being applied to the array.
	 If not in a dereference, a read of an array returns its address,
	 so is an address-taking context 
      */
      if (c & c_deref)
	c &= ~c_deref;
      else if (c & c_read)
	c = (c & ~c_read) | c_addressed;
    }
  else if (expr->cst)
    c |= c_constant;

  switch (expr->kind)
    {
    case kind_identifier:
      expr->context = access_context(c);
      identifier_used(CAST(identifier, expr), fn, c);
      break;

    case kind_interface_deref:
      interface_used(CAST(interface_deref, expr), fn, c);
      break;

    case kind_comma: {
      expression e;

      scan_expression (e, CAST(comma, expr)->arg1)
	if (e->next)
	  collect_uses_expr(e, fn, exe_c);
	else
	  collect_uses_expr(e, fn, c);
      break;
    }
    case kind_extension_expr:
      collect_uses_expr(CAST(unary, expr)->arg1, fn, c);
      break;

    case kind_conditional: {
      conditional ce = CAST(conditional, expr);
      context true_c = c, false_c = c;

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    true_c &= ~c_executable;
	  else
	    false_c &= ~c_executable;
	}

      collect_uses_expr(ce->condition, fn, exe_c | c_read);
      collect_uses_expr(ce->arg1, fn, true_c);
      collect_uses_expr(ce->arg2, fn, false_c);
      break;
    }
    case kind_compound_expr:
      collect_uses_stmt(CAST(compound_expr, expr)->stmt, fn, c);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);
      expression e;

      // tasks posts are a "read" of the task
      if (fce->call_kind == post_task)
	collect_uses_expr(fce->arg1, fn, exe_c | c_read);
      // C named fn calls, commands and events are c_fncall, 
      // otherwise its c_read (and a warning about fn ptr use)
      else if (!(is_interface_deref(fce->arg1) ||
	    is_generic_call(fce->arg1) ||
	    (is_identifier(fce->arg1) &&
	     (CAST(identifier, fce->arg1)->ddecl->kind == decl_function ||
	      CAST(identifier, fce->arg1)->ddecl->kind == decl_magic_function)) ||
	    fce->va_arg_call))
	{
	  /* We allow a function pointer calls in C code on the assumption
	     that this represents runtime implementation stuff (e.g., 
	     the task scheduler, or tossim stuff) */
	  if (warn_fnptr && 
	      (!fn || fn->container))
	    nesc_warning_with_location(fce->location, "call via function pointer");
	  collect_uses_expr(fce->arg1, fn, exe_c | c_read);
	}
      else
	collect_uses_expr(fce->arg1, fn, exe_c | c_fncall);

      scan_expression (e, fce->args)
	collect_uses_expr(e, fn, exe_c | c_read);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);
      expression e;

      collect_uses_expr(fce->arg1, fn, exe_c | c_fncall);
      scan_expression (e, fce->args)
	collect_uses_expr(e, fn, exe_c | c_read);
      break;
    }

    case kind_array_ref: {
      array_ref are = CAST(array_ref, expr);
      expression arg1 = are->arg1, arg2 = are->arg2, arg;

      /* When presented with something like 1[a], switch args to ensure that
	 arg1 is the pointer or array, arg2 the index */
      if (type_integer(arg1->type)) 
	{
	  arg = arg1; arg1 = arg2; arg2 = arg;
	}

      collect_uses_deref(expr, arg1, fn, c);
      collect_uses_expr(arg2, fn, exe_c | c_read);
      expr->context = access_context(c);
      break;
    }
    case kind_dereference:
      expr->context = access_context(c);
      collect_uses_deref(expr, CAST(dereference, expr)->arg1, fn, c);
      break;

    case kind_address_of: {
      expression arg = CAST(unary, expr)->arg1;

      if (c & c_deref) /* *&x is just x */
	c &= ~c_deref;
      else
	c = exe_c | c_addressed;

      collect_uses_expr(arg, fn, c);
      break;
    }
    case kind_field_ref: {
      expr->context = access_context(c);
      collect_uses_expr(CAST(field_ref, expr)->arg1, fn, c);
      break;
    }
    case kind_preincrement: case kind_postincrement:
    case kind_predecrement: case kind_postdecrement: {
      unary ue = CAST(unary, expr);

      collect_uses_expr(ue->arg1, fn, exe_c | c_read | c_write);
      break;
    }
    case kind_assign: {
      binary be = CAST(binary, expr);

      collect_uses_expr(be->arg1, fn, exe_c | c_write);
      collect_uses_expr(be->arg2, fn, exe_c | c_read);
      break;
    }
    case kind_plus_assign: case kind_minus_assign: 
    case kind_times_assign: case kind_divide_assign: case kind_modulo_assign:
    case kind_bitand_assign: case kind_bitor_assign: case kind_bitxor_assign:
    case kind_lshift_assign: case kind_rshift_assign: {
      binary be = CAST(binary, expr);
      collect_uses_expr(be->arg1, fn, exe_c | c_read | c_write);
      collect_uses_expr(be->arg2, fn, exe_c | c_read);
      break;
    }
    case kind_sizeof_expr: case kind_alignof_expr:
      collect_uses_expr(CAST(unary, expr)->arg1, fn, 0);
      break;

    case kind_cast: {
      cast ce = CAST(cast, expr);

      collect_uses_ast(ce->asttype, fn, c);
      collect_uses_expr(ce->arg1, fn, c);
    }
    default:
      if (is_unary(expr))
	collect_uses_expr(CAST(unary,expr)->arg1, fn, c);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);

	  collect_uses_expr(be->arg1, fn, exe_c | c_read);
	  collect_uses_expr(be->arg2, fn, exe_c | c_read);
	}
      else
	/* A default catch-all for
	     sizeof_type, alignof_type, label_address, cast_list,
	     init_list, init_index, init_field, lexical_cst,
	     string_cst, string
	   Embeddded expressions are compile-time constants or read-only,
	   so the default collect_uses_ast_expr is valid. */
	collect_uses_children(expr, fn, c);
      break;
    }
}

static void collect_uses_asm_operands(asm_operand operands,
				      data_declaration fn, context exe_c)
{
  asm_operand aop;

  scan_asm_operand (aop, operands)
    {
      int mode = asm_rwmode(aop->string);
      context c = exe_c;

      /* = is write, + is r/w, everything else is read */
      if (mode == '=' || mode == '+')
	c |= c_write;
      if (mode != '=')
	c |= c_read;

      collect_uses_expr(aop->arg1, fn, c);
    }
}


static void collect_uses_stmt(statement stmt, data_declaration fn, context c)
{
  context exe_c = exe_context(c);

  if (!stmt)
    return;

  switch (stmt->kind)
    {
    case kind_compound_stmt: {
      compound_stmt cs = CAST(compound_stmt, stmt);
      statement s;

      collect_uses_ast(cs->id_labels, fn, c);
      collect_uses_ast(cs->decls, fn, c);

      /* The last statement is possiblt read (in compound_expr) */
      scan_statement (s, cs->stmts)
        if (s->next) 
          collect_uses_stmt(s, fn, exe_c);
        else 
          collect_uses_stmt(s, fn, c);

      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);
      context true_c = exe_c, false_c = exe_c;

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    true_c &= ~c_executable;
	  else
	    false_c &= ~c_executable;
	}

      collect_uses_expr(is->condition, fn, exe_c | c_read);
      collect_uses_stmt(is->stmt1, fn, true_c);
      collect_uses_stmt(is->stmt2, fn, false_c);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);
      context body_c = exe_c;

      if (cs->condition->cst && stmt->kind == kind_while_stmt &&
	  definite_zero(cs->condition))
	body_c &= ~c_executable;
      collect_uses_expr(cs->condition, fn, exe_c | c_read);
      collect_uses_stmt(cs->stmt, fn, body_c);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);
      context body_c = exe_c;

      if (fs->arg2 && fs->arg2->cst && definite_zero(fs->arg2))
	body_c &= ~c_executable;
      collect_uses_expr(fs->arg1, fn, exe_c | c_read);
      collect_uses_expr(fs->arg2, fn, exe_c | c_read);
      collect_uses_expr(fs->arg3, fn, body_c | c_read);
      collect_uses_stmt(fs->stmt, fn, body_c);
      break;
    }
    case kind_labeled_stmt:
      collect_uses_ast(CAST(labeled_stmt, stmt)->label, fn, c);
      collect_uses_stmt(CAST(labeled_stmt, stmt)->stmt, fn, c);
      break;

    case kind_expression_stmt:
      collect_uses_expr(CAST(expression_stmt, stmt)->arg1, fn, c);
      break;

    case kind_atomic_stmt:
      collect_uses_stmt(CAST(atomic_stmt,stmt)->stmt, fn, exe_c | c_atomic);
      break;

    case kind_asm_stmt: {
      asm_stmt as = CAST(asm_stmt, stmt);

      collect_uses_expr(as->arg1, fn, exe_c | c_read);
      collect_uses_asm_operands(as->asm_operands1, fn, exe_c);
      collect_uses_asm_operands(as->asm_operands2, fn, exe_c);
      break;
    }
    default:
      /* for break_stmt, continue_stmt, goto_stmt, computed_goto_stmt,
             empty_stmt, return_stmt */
      collect_uses_children(stmt, fn, c);
      break;
    }
}

/* An AST walker that collect_usess all reachable expressions */
static AST_walker collect_uses_walker;

struct uses_data
{
  context c;
  data_declaration function;
};

static AST_walker_result collect_uses_ast_expr(AST_walker spec, void *data,
					       expression *e)
{
  struct uses_data *ud = data;
  collect_uses_expr(*e, ud->function, exe_context(ud->c) | c_read);
  return aw_done;
}

static AST_walker_result collect_uses_ast_stmt(AST_walker spec, void *data,
					       statement *s)
{
  struct uses_data *ud = data;
  collect_uses_stmt(*s, ud->function, exe_context(ud->c));
  return aw_done;
}

static AST_walker_result collect_uses_ast_fdecl(AST_walker spec, void *data,
						function_decl *fd)
{
  struct uses_data *ud = data;
  struct uses_data new_ud = *ud;

  new_ud.function = (*fd)->ddecl;
  AST_walk_children(spec, &new_ud, CAST(node, *fd));

  return aw_done;
}

static void init_collect_uses_walker(void)
{
  collect_uses_walker = new_AST_walker(rr);
  AST_walker_handle(collect_uses_walker, kind_expression,
		    collect_uses_ast_expr);
  AST_walker_handle(collect_uses_walker, kind_statement,
		    collect_uses_ast_stmt);
  AST_walker_handle(collect_uses_walker, kind_function_decl,
		    collect_uses_ast_fdecl);
}

static void collect_uses_ast(void *n, data_declaration fn, context c)
{
  node nn = CAST(node, n);
  struct uses_data new_ud;

  new_ud.c = c;
  new_ud.function = fn;

  AST_walk_list(collect_uses_walker, &new_ud, &nn);
}

static void collect_uses_children(void *n, data_declaration fn, context c)
{
  struct uses_data new_ud;

  new_ud.c = c;
  new_ud.function = fn;

  AST_walk_children(collect_uses_walker, &new_ud, CAST(node, n));
}

void collect_uses(declaration decls)
{
  collect_uses_ast(decls, NULL, c_executable);
}

void init_uses(void)
{
  type ei_type;

  rr = parse_region;
  nglobal_uses = dd_new_list(rr);
  init_collect_uses_walker();

  ei_type = build_function_type(parse_region, void_type, NULL);
  enable_interrupt =
    declare_function(dummy_location, "__nesc_enable_interrupt", ei_type);
}
