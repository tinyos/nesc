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

/* IDEAS: track fields of structs
 */
static data_declaration current_function;

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

use new_use(location l, context c)
{
  use u = ralloc(rr, struct use);

  u->l = l;
  u->fn = current_function;
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

static void ddecl_used(data_declaration id, use u)
{
  iduse i = new_iduse(id, u);

  if (!id->nuses)
    id->nuses = dd_new_list(rr);
  dd_add_last(rr, id->nuses, u);
  id->use_summary |= u->c;

  if (current_function)
    {
      if (!current_function->fn_uses)
	current_function->fn_uses = dd_new_list(rr);
      dd_add_last(rr, current_function->fn_uses, i);
    }
  else
    dd_add_last(rr, nglobal_uses, i);
}

static void identifier_used(identifier id, context c)
{
  ddecl_used(id->ddecl, new_use(id->location, c));
}

static void interface_used(interface_deref iref, context c)
{
  ddecl_used(iref->ddecl, new_use(iref->location, c));
}

static void collect_uses_ast(void *n, context c);
static void collect_uses_children(void *n, context c);
static void collect_uses_stmt(statement stmt, context c);
static void collect_uses_expr(expression expr, context c);


static void collect_uses_deref(expression expr,
			       expression dereferenced, context c)
{
  c = use_context(c);

  if (c & c_addressed) /* &*x is just x, not a pointer deref */
    c &= ~c_addressed;
  else
    {
      c |= c_deref;
      /*pointer_use(expr, c);*/
    }
  collect_uses_expr(dereferenced, c);
}

static void collect_uses_expr(expression expr, context c)
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
      identifier_used(CAST(identifier, expr), c);
      break;

    case kind_interface_deref:
      interface_used(CAST(interface_deref, expr), c);
      break;

    case kind_comma: {
      expression e;

      scan_expression (e, CAST(comma, expr)->arg1)
	if (e->next)
	  collect_uses_expr(e, exe_c);
	else
	  collect_uses_expr(e, c);
      break;
    }
    case kind_extension_expr:
      collect_uses_expr(CAST(unary, expr)->arg1, c);
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

      collect_uses_expr(ce->condition, exe_c | c_read);
      collect_uses_expr(ce->arg1, true_c);
      collect_uses_expr(ce->arg2, false_c);
      break;
    }
    case kind_compound_expr:
      collect_uses_stmt(CAST(compound_expr, expr)->stmt, c);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);
      expression e;

      // tasks posts are a "read" of the task
      if (fce->call_kind == post_task)
	collect_uses_expr(fce->arg1, exe_c | c_read);
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
	      (!current_function || current_function->container))
	    nesc_warning_with_location(fce->location, "call via function pointer");
	  collect_uses_expr(fce->arg1, exe_c | c_read);
	}
      else
	collect_uses_expr(fce->arg1, exe_c | c_fncall);

      scan_expression (e, fce->args)
	collect_uses_expr(e, exe_c | c_read);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);
      expression e;

      collect_uses_expr(fce->arg1, exe_c | c_read);
      scan_expression (e, fce->args)
	collect_uses_expr(e, exe_c | c_read);
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

      collect_uses_deref(expr, arg1, c);
      collect_uses_expr(arg2, exe_c | c_read);
      expr->context = access_context(c);
      break;
    }
    case kind_dereference:
      expr->context = access_context(c);
      collect_uses_deref(expr, CAST(dereference, expr)->arg1, c);
      break;

    case kind_address_of: {
      expression arg = CAST(unary, expr)->arg1;

      if (c & c_deref) /* *&x is just x */
	c &= ~c_deref;
      else
	c = exe_c | c_addressed;

      collect_uses_expr(arg, c);
      break;
    }
    case kind_field_ref: {
      expr->context = access_context(c);
      collect_uses_expr(CAST(field_ref, expr)->arg1, c);
      break;
    }
    case kind_preincrement: case kind_postincrement:
    case kind_predecrement: case kind_postdecrement: {
      unary ue = CAST(unary, expr);

      collect_uses_expr(ue->arg1, exe_c | c_read | c_write);
      break;
    }
    case kind_assign: {
      binary be = CAST(binary, expr);

      collect_uses_expr(be->arg1, exe_c | c_write);
      collect_uses_expr(be->arg2, exe_c | c_read);
      break;
    }
    case kind_plus_assign: case kind_minus_assign: 
    case kind_times_assign: case kind_divide_assign: case kind_modulo_assign:
    case kind_bitand_assign: case kind_bitor_assign: case kind_bitxor_assign:
    case kind_lshift_assign: case kind_rshift_assign: {
      binary be = CAST(binary, expr);
      collect_uses_expr(be->arg1, exe_c | c_read | c_write);
      collect_uses_expr(be->arg2, exe_c | c_read);
      break;
    }
    case kind_sizeof_expr: case kind_alignof_expr:
      collect_uses_expr(CAST(unary, expr)->arg1, 0);
      break;

    case kind_cast: {
      cast ce = CAST(cast, expr);

      collect_uses_ast(ce->asttype, c);
      collect_uses_expr(ce->arg1, c);
    }
    default:
      if (is_unary(expr))
	collect_uses_expr(CAST(unary,expr)->arg1, c);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);

	  collect_uses_expr(be->arg1, exe_c | c_read);
	  collect_uses_expr(be->arg2, exe_c | c_read);
	}
      else
	/* A default catch-all for
	     sizeof_type, alignof_type, label_address, cast_list,
	     init_list, init_index, init_field, lexical_cst,
	     string_cst, string
	   Embeddded expressions are compile-time constants or read-only,
	   so the default collect_uses_ast_expr is valid. */
	collect_uses_children(expr, c);
      break;
    }
}

static void collect_uses_asm_operands(asm_operand operands, context exe_c)
{
  asm_operand aop;

  scan_asm_operand (aop, operands)
    {
      wchar_t mode = asm_rwmode(aop->string);
      context c = exe_c;

      /* = is write, + is r/w, everything else is read */
      if (mode == '=' || mode == '+')
	c |= c_write;
      if (mode != '=')
	c |= c_read;

      collect_uses_expr(aop->arg1, c);
    }
}


static void collect_uses_stmt(statement stmt, context c)
{
  context exe_c = exe_context(c);

  if (!stmt)
    return;

  switch (stmt->kind)
    {
    case kind_compound_stmt: {
      compound_stmt cs = CAST(compound_stmt, stmt);
      statement s;

      collect_uses_ast(cs->id_labels, c);
      collect_uses_ast(cs->decls, c);

      /* The last statement is possiblt read (in compound_expr) */
      scan_statement (s, cs->stmts)
        if (s->next) 
          collect_uses_stmt(s, exe_c);
        else 
          collect_uses_stmt(s, c);

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

      collect_uses_expr(is->condition, exe_c | c_read);
      collect_uses_stmt(is->stmt1, true_c);
      collect_uses_stmt(is->stmt2, false_c);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);
      context body_c = exe_c;

      if (cs->condition->cst && stmt->kind == kind_while_stmt &&
	  definite_zero(cs->condition))
	body_c &= ~c_executable;
      collect_uses_expr(cs->condition, exe_c | c_read);
      collect_uses_stmt(cs->stmt, body_c);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);
      context body_c = exe_c;

      if (fs->arg2 && fs->arg2->cst && definite_zero(fs->arg2))
	body_c &= ~c_executable;
      collect_uses_expr(fs->arg1, exe_c | c_read);
      collect_uses_expr(fs->arg2, exe_c | c_read);
      collect_uses_expr(fs->arg3, body_c | c_read);
      collect_uses_stmt(fs->stmt, body_c);
      break;
    }
    case kind_labeled_stmt:
      collect_uses_ast(CAST(labeled_stmt, stmt)->label, c);
      collect_uses_stmt(CAST(labeled_stmt, stmt)->stmt, c);
      break;

    case kind_expression_stmt:
      collect_uses_expr(CAST(expression_stmt, stmt)->arg1, c);
      break;

    case kind_atomic_stmt:
      collect_uses_stmt(CAST(atomic_stmt,stmt)->stmt, exe_c | c_atomic);
      break;

    case kind_asm_stmt: {
      asm_stmt as = CAST(asm_stmt, stmt);

      collect_uses_expr(as->arg1, exe_c | c_read);
      collect_uses_asm_operands(as->asm_operands1, exe_c);
      collect_uses_asm_operands(as->asm_operands2, exe_c);
      break;
    }
    default:
      /* for break_stmt, continue_stmt, goto_stmt, computed_goto_stmt,
             empty_stmt, return_stmt */
      collect_uses_children(stmt, c);
      break;
    }
}

/* An AST walker that collect_usess all reachable expressions */
static AST_walker collect_uses_walker;

static AST_walker_result collect_uses_ast_expr(AST_walker spec, void *data,
					  expression *e)
{
  collect_uses_expr(*e, exe_context(*(context *)data) | c_read);
  return aw_done;
}

static AST_walker_result collect_uses_ast_stmt(AST_walker spec, void *data,
					       statement *s)
{
  collect_uses_stmt(*s, exe_context(*(context *)data));
  return aw_done;
}

static AST_walker_result collect_uses_ast_fdecl(AST_walker spec, void *data,
						function_decl *fd)
{
  data_declaration oldfn = current_function;

  current_function = (*fd)->ddecl;
  AST_walk_children(spec, data, CAST(node, *fd));
  current_function = oldfn;

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

static void collect_uses_ast(void *n, context c)
{
  node nn = CAST(node, n);

  AST_walk_list(collect_uses_walker, &c, &nn);
}

static void collect_uses_children(void *n, context c)
{
  AST_walk_children(collect_uses_walker, &c, CAST(node, n));
}

void collect_uses(declaration decls)
{
  current_function = NULL;
  collect_uses_ast(decls, c_executable);
}

void init_uses(void)
{
  rr = parse_region;
  nglobal_uses = dd_new_list(rr);
  init_collect_uses_walker();
}
