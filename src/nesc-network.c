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
#include "nesc-network.h"
#include "nesc-semantics.h"
#include "AST_utils.h"
#include "AST_walk.h"
#include "c-parse.h"
#include "edit.h"
#include "unparse.h"
#include "nesc-uses.h"

static void xtox_used(data_declaration transcoderfn, function_decl fn)
{
  data_declaration fdecl = fn ? fn->ddecl : NULL;

  ddecl_used(transcoderfn, new_use(dummy_location, fdecl, c_executable | c_fncall));
}

static void hton_used(type t, function_decl fn)
{
  xtox_used(type_network_base_decl(t)->encoder, fn);
}

static void ntoh_used(type t, function_decl fn)
{
  xtox_used(type_network_base_decl(t)->decoder, fn);
}

static void validate_network_lvalue(expression e)
{
  conditional cond;

  if ((cond = conditional_lvalue(e)) &&
      (type_network_base_type(cond->arg1->type) ||
       type_network_base_type(cond->arg2->type)))
    error_with_location(e->location,
			"Conditional assignment not supported for network types");
}

static data_declaration add_network_temporary(function_decl fn, type t)
{
  /* See Weird hack comment in prt_network_assignment */
  if (fn)
    return add_temporary(parse_region, CAST(compound_stmt, fn->stmt), t);
  else
    return NULL;
}

static AST_walker_result network_expression(AST_walker spec, void *data,
					    expression *n)
{
  expression e = *n;
  function_decl fn = data;

  if (e->type && type_network_base_type(e->type) &&
      (e->context & c_read) && !(e->context & c_write))
    ntoh_used(e->type, fn);

  return aw_walk;
}

static AST_walker_result network_assignment(AST_walker spec, void *data,
					    assignment *n)
{
  assignment a = *n;
  function_decl fn = data;

  if (type_network_base_type(a->type))
    {
      if (a->kind != kind_assign) /* op= reads too */
	ntoh_used(a->type, fn);
      hton_used(a->type, fn);
    }

  validate_network_lvalue(a->arg1);
  /* See problem/ugly hack comment in network_increment */
  if (type_network_base_type(a->type) && !a->temp1)
    {
      /* op= needs a temp */
      if (a->kind != kind_assign)
	a->temp1 = add_network_temporary(fn, make_pointer_type(a->arg1->type));
    }

  return aw_walk;
}

static AST_walker_result network_increment(AST_walker spec, void *data,
					   increment *n)
{
  increment i = *n;
  function_decl fn = data;

  if (type_network_base_type(i->type))
    {
      ntoh_used(i->type, fn);
      hton_used(i->type, fn);
    }

  validate_network_lvalue(i->arg1);

  /* Problem: adding the declarations for the temporaries changes the AST
     as we're walking through it. If we add a temporary while walking 
     through the first declaration, we'll revisit this declaration. Oops.
     Ugly hack fix: don't create the temporaries if they've already been
     created. Note that this could lead to duplicate error messages from
     validate_network_lvalue, but that's for use of a deprecated gcc
     feature which is going away soon. */
  if (type_network_base_type(i->type) && !i->temp1)
    {
      /* pre-ops need 1 temp, post-ops need 2 */
      i->temp1 = add_network_temporary(fn, make_pointer_type(i->type));
      if (i->kind == kind_postincrement || i->kind == kind_postdecrement)
	i->temp2 = add_network_temporary(fn, type_network_platform_type(i->type));
    }

  return aw_walk;
}

static AST_walker_result network_fdecl(AST_walker spec, void *data,
				       function_decl *fd)
{
  AST_walk_children(spec, *fd, CAST(node, *fd));
  return aw_done;
}

/* An AST walker that does network type processing on the AST */
static AST_walker network_walker;

static void init_network_walker(void)
{
  network_walker = new_AST_walker(parse_region);
  AST_walker_handle(network_walker, kind_expression, network_expression);
  AST_walker_handle(network_walker, kind_increment, network_increment);
  AST_walker_handle(network_walker, kind_assignment, network_assignment);
  AST_walker_handle(network_walker, kind_function_decl, network_fdecl);
}

void handle_network_types(declaration decls)
{
  node n = CAST(node, decls);

  AST_walk_list(network_walker, NULL, &n);
}

static void output_hton(type t)
{
  output_string(type_network_base_decl(t)->encoder->name);
}

static void output_ntoh(type t)
{
  output_string(type_network_base_decl(t)->decoder->name);
}

static bool prt_network_assignment(expression e)
{
  char *selfassign = NULL;
  assignment a;

  if (!(is_assignment(e) && type_network_base_type(e->type)))
    return FALSE;

  a = CAST(assignment, e);

  switch (e->kind)
    {
    case kind_plus_assign:   selfassign = "+"; break;
    case kind_minus_assign:  selfassign = "+"; break;
    case kind_times_assign:  selfassign = "*"; break;
    case kind_divide_assign: selfassign = "/"; break;
    case kind_lshift_assign: selfassign = "<<"; break;
    case kind_rshift_assign: selfassign = ">>"; break;
    case kind_bitand_assign: selfassign = "&"; break;
    case kind_bitor_assign:  selfassign = "|"; break;
    case kind_bitxor_assign: selfassign = "^"; break;
    default: break;
    }

  /* Weird hack: when temp1 is not set, this op= is outside
     a function, i.e., in something like a sizeof. We can just
     pretend it's a regular assignment. */
  if (selfassign && !a->temp1)
    selfassign = NULL;

  set_location(e->location);
  if (selfassign)
    {
      const char *temp = a->temp1->name;

      output("(%s = &", temp);
      prt_expression(a->arg1, P_CAST);
      output(", ");
      output_hton(a->type);
      output("(%s, ", temp);
      output_ntoh(e->type);
      output("(%s) %s ", temp, selfassign);
      prt_expression(a->arg2, P_TIMES);
      output("))");
    }
  else
    {
      output_hton(e->type);
      output("(&");
      prt_expression(a->arg1, P_CAST);
      output(", ");
      prt_expression(a->arg2, P_ASSIGN);
      output(")");
    }
  return TRUE;
}

static bool prt_network_increment(expression e)
{
  increment i;
  const char *temp;
  char incop;

  if (!(is_increment(e) && type_network_base_type(e->type)))
    return FALSE;

  i = CAST(increment, e);
  temp = i->temp1->name;
  incop = i->kind == kind_preincrement || i->kind == kind_postincrement ? '+' : '-';

  /* pre-op: (t1 = &e, HTON(t1, NTOH(t1) +/- 1))
     post-op: (t1 = &e, HTON(t1, (t2 = NTOH(t1)) +/- 1), t2) */
  set_location(i->location);
  output("(%s = &", temp);
  prt_expression(i->arg1, P_CAST);
  output(", ");
  output_hton(i->type);
  output("(%s, ", temp);
  if (i->temp2)
    {
      const char *val = i->temp2->name;

      output("(%s = ", val);
      output_ntoh(i->type);
      output("(%s)) %c 1), %s)", temp, incop, val);
    }
  else
    {
      output_ntoh(i->type);
      output("(%s) %c 1))", temp, incop);
    }
  return TRUE;
}

static bool prt_network_read(expression e)
{
  if (!(e->type && type_network_base_type(e->type) &&
	(e->context & c_read) && !(e->context & c_write)))
    return FALSE;

  output_ntoh(e->type);
  output("(&");
  prt_expression_helper(e, P_ASSIGN);
  output(")");

  return TRUE;
}

bool prt_network_expression(expression e)
{
  return prt_network_read(e) ||
    prt_network_assignment(e) ||
    prt_network_increment(e);
}

bool prt_network_typedef(data_decl d, variable_decl vd)
{
  if (vd->ddecl->kind == decl_typedef && vd->ddecl->basetype)
    {
      /* A Network base type typedef */
      type basetype = vd->ddecl->basetype;

      if (!type_size_cc(basetype) && cval_isinteger(type_size(basetype)))
	error_with_location(vd->location, "network base type `%s' is of unknown size", vd->ddecl->name);
      else
	{
	  set_location(vd->location);
	  output("typedef struct { char data[%d]; } __attribute__((packed)) %s;",
		 (int)type_size_int(basetype), vd->ddecl->name);
	}
      return TRUE;
    }
  return FALSE;
}

void init_network(void)
{
  init_network_walker();
}
