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
#include "c-parse.h"
#include "nesc-generate.h"
#include "nesc-inline.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-c.h"
#include "unparse.h"
#include "AST_utils.h"
#include "edit.h"
#include "semantics.h"
#include "constants.h"
#include "nesc-concurrency.h"
#include "nesc-uses.h"
#include "nesc-network.h"
#include "nesc-atomic.h"
#include "nesc-cpp.h"

static void prt_nesc_function_hdr(data_declaration fn_decl,
				  psd_options options)
/* Effects: prints the C function declaration for fn_decl
*/
{
  /* We print the declaration from the interface rather than that of fn_decl
     itself, as this latter may use not-yet-defined typedefs.
     prt_declarator will use the name from fn_decl in its output. */
  variable_decl ifn_vd = CAST(variable_decl, fn_decl->ast);
  data_decl fn_dd = CAST(data_decl, ifn_vd->parent);
  psd_options opts;
  function_declarator fd;
  asttype ret;

  prt_diff_info(fn_decl);
  set_location(fn_dd->location);
  if (!is_binary_component(fn_decl->container->impl))
    output("static ");

  /* Functions returning a network type should return the base type
     instead */
  if (is_function_declarator(ifn_vd->declarator))
    opts = psd_rewrite_nxbase;
  else
    opts = 0;
  options |= psd_rename_parameters;

  fd = get_fdeclarator(ifn_vd->declarator);
  /* Handle nesdoc-overridden return type */
  if (fd && (ret = fd->return_type))
    {
      prt_attribute_elements(fn_dd->modifiers);
      prt_type_elements(ret->qualifiers, opts);
      prt_declarator(ret->declarator, NULL, ifn_vd->attributes, fn_decl,
		     options | psd_print_ddecl_fdeclarator);
    }
  else
    {
      prt_type_elements(fn_dd->modifiers, opts);
      prt_declarator(ifn_vd->declarator, NULL, ifn_vd->attributes, fn_decl, 
		     options);
    }
}

void prt_nesc_function_declaration(data_declaration fndecl, void *data)
{
  if (fndecl->definition && fndecl->isused && !fndecl->suppress_definition)
    {
      prt_nesc_function_hdr(fndecl, psd_print_default);
      outputln(";");
    }
}

void prt_nesc_function_declarations(nesc_declaration mod)
{
  component_functions_iterate(mod, prt_nesc_function_declaration, NULL);
}

/* A description of the target functions a used function is connected to */
typedef struct full_connection
{
  endp ep;
  expression cond;
  expression args;
} *full_connection;


struct connections
{
  /* Connection being sought */
  region r;
  cgraph cg;
  data_declaration called;

  /* the list of targets which are called generically (with generic arguments
     passed through unchanged). NULL if 'called' is not generic.
     Both 'cond' and 'args' are NULL for generic_calls */
  dd_list/*<full_connection>*/ generic_calls; 

  /* normal_calls is the list of all other targets.

     If 'called' is generic, 'cond' is the expression list that must match
     the generic parameters of 'called' for the call to 'ep' to
     take place. 'args' is the expression list to add to the arguments
     if 'ep' is generic. */
  dd_list/*<full_connection>*/ normal_calls;

  /* The combiner function used, if any */
  data_declaration combiner;
};

static full_connection new_full_connection(region r, endp ep, expression cond,
				 expression args)
{
  full_connection c = ralloc(r, struct full_connection);

  c->ep = ep;
  c->cond = cond;
  c->args = args;

  return c;
}

static type function_return_type(data_declaration fndecl)
{
  return type_function_return_type(get_actual_function_type(fndecl->type));
}

void prt_ncf_header(struct connections *c, type return_type)
{
  if (c->called->makeinline && flag_no_inline < 2)
    output("inline ");
  prt_nesc_function_hdr(c->called, 0);
  outputln("{");
  indent();
  if (!type_void(return_type))
    {
      prt_data_decl(build_declaration(parse_region, NULL, return_type, "__nesc_result", NULL, NULL));
      newline();
    }
}

void prt_ncf_trailer(type return_type)
{
  if (!type_void(return_type))
    {
      newline();
      outputln("return __nesc_result;");
    }
  unindent();
  outputln("}");
}

static bool prt_arguments(declaration parms, bool first, bool rename)
/* Effects: prints argument list composed of the variables declared in 'parms'.
     'first' must be TRUE iff no arguments have yet been printed.
   Returns: TRUE iff 'first' and no arguments printed
*/
{
  declaration parm;

  scan_declaration (parm, parms)
    {
      /* Not supporting ... here for now. Fix requires different approach */
      data_decl dd = CAST(data_decl, parm);
      variable_decl vd = CAST(variable_decl, dd->decls);
	  
      if (!vd->ddecl) /* empty (void) parameter list */
	break;

      if (!first)
	output(", ");
      first = FALSE;

      if (rename || !vd->ddecl->name)
	output("arg_%p", vd->ddecl);
      else
	output((char *)vd->ddecl->name);
    }
  return first;
}

void prt_ncf_direct_call(struct connections *c,
			 full_connection ccall,
			 bool first_call,
			 psd_options options,
			 type return_type,
			 function_declarator called_fd)
/* Effects: prints call to 'calls' in a connection function.
     Assigns result if last_call is TRUE.
*/
{
  bool first_arg = TRUE;
  data_declaration combiner = type_combiner(return_type);
  bool calling_combiner = FALSE;

  if (!type_void(return_type))
    {
      output("__nesc_result = ");

      /* Combine w/ the combiner on subsequent calls */
      if (!first_call && combiner)
	{
	  output("%s(__nesc_result, ", combiner->name);
	  calling_combiner = TRUE;
	}
    }

  prt_ddecl_full_name(ccall->ep->function, options);
  output("(");
  if (ccall->ep->function->gparms)
    {
      if (ccall->args)
	{
	  /* Non-generic calling generic, add arguments */
	  prt_expressions(ccall->args, first_arg);
	  first_arg = FALSE;
	}
      else
	{
	  /* Generic calling generic, pass arguments through */
	  first_arg = prt_arguments(ddecl_get_gparms(c->called), first_arg, TRUE);
	}
    }
  else
    assert(!ccall->args);

  prt_arguments(called_fd->parms, first_arg, FALSE);

  if (calling_combiner)
    output(")");

  outputln(");");
}

void prt_ncf_default_call(struct connections *c,
			  type return_type,
			  function_declarator called_fd)
{
  struct full_connection default_target;
  struct endp default_ep;

  default_target.ep = &default_ep;
  default_target.cond = default_target.args = NULL;
  default_ep.function = c->called;

  prt_ncf_direct_call(c, &default_target, TRUE, psd_print_default,
		      return_type, called_fd);
}

bool prt_ncf_direct_calls(struct connections *c,
			  dd_list/*<full_connection>*/ calls,
			  type return_type)
/* Effects: prints calls to 'calls' in a connection function.
*/
{
  dd_list_pos call;
  bool first_call = TRUE;
  function_declarator called_fd = ddecl_get_fdeclarator(c->called);

  dd_scan (call, calls)
    {
      full_connection ccall = DD_GET(full_connection, call);

      assert(!ccall->cond);

      prt_ncf_direct_call(c, ccall, first_call, 0, return_type, called_fd);
      first_call = FALSE;
    }

  return first_call;
}

static int constant_expression_list_compare(expression arg1, expression arg2)
{
  while (arg1)
    {
      largest_int uval1, uval2;

      uval1 = cval_sint_value(arg1->cst->cval);
      uval2 = cval_sint_value(arg2->cst->cval);

      /* Can't use - as might overflow and mod down to 0 */
      if (uval1 < uval2)
	return -1;
      else if (uval1 > uval2)
	return 1;

      arg1 = CAST(expression, arg1->next);
      arg2 = CAST(expression, arg2->next);
    }
  assert(!arg2);

  return 0;
}

static int condition_compare(const void *p1, const void *p2)
{
 struct full_connection *const *c1 = p1, *const *c2 = p2;

  return constant_expression_list_compare((*c1)->cond, (*c2)->cond);
}

static void prt_ncf_condition(struct connections *c, expression cond)
{
  declaration gparm;
  bool first = TRUE;

  scan_declaration (gparm, ddecl_get_gparms(c->called))
    {
      data_decl dd = CAST(data_decl, gparm);
      variable_decl vd = CAST(variable_decl, dd->decls);
	  
      if (first)
	output("if (");
      else
	output(" && ");
      first = FALSE;

      output("arg_%p == ", vd->ddecl);
      prt_expression(cond, P_REL);

      cond = CAST(expression, cond->next);
    }
  output(") ");
}

static void prt_ncf_conditional_calls(struct connections *c, bool first_call, type return_type)
{
  dd_list_pos call;
  int i, j, ncalls = dd_length(c->normal_calls);
  full_connection *cond_eps =
    rarrayalloc(c->r, ncalls, full_connection);
  function_declarator called_fd = ddecl_get_fdeclarator(c->called);
  bool one_arg = FALSE;

  /* No work to do */
  if (ncalls == 0 && !dd_is_empty(c->generic_calls))
    return;

  /* Sort calls so we can find connections with the same conditions */
  i = 0;
  dd_scan (call, c->normal_calls)
    cond_eps[i++] = DD_GET(full_connection, call);
  qsort(cond_eps, ncalls, sizeof(full_connection), condition_compare);

  if (ncalls > 0 && !cond_eps[0]->cond->next)
    {
      /* use switch rather than cascaded ifs (gcc generate better code) */
      one_arg = TRUE;
      output("switch (");
      prt_arguments(ddecl_get_gparms(c->called), TRUE, TRUE);
      outputln(") {");
      indent();
    }

  /* output the calls */
  i = 0;
  while (i < ncalls)
    {
      expression cond = cond_eps[i]->cond;
      bool first_cond_call = first_call;

      /* output latest condition */
      if (one_arg)
	{
	  output("case ");
	  prt_expression(cond, P_ASSIGN);
	  outputln(":");
	}
      else
	{
	  if (i != 0)
	    output("else ");
	  prt_ncf_condition(c, cond);
	  outputln("{");
	}
      indent();

      /* find last target with same condition */
      j = i;
      while (++j < ncalls && condition_compare(&cond_eps[i], &cond_eps[j]) == 0)
	;

      /* print them, setting result for the last one */
      while (i < j)
	{
	  prt_ncf_direct_call(c, cond_eps[i], first_cond_call, 0,
			      return_type, called_fd);
	  first_cond_call = FALSE;
	  i++;
	}
	
      if (one_arg)
	outputln("break;");
      unindent();
      if (!one_arg)
	outputln("}");
    }
  /* output call to default if there are no non-conditional calls */
  if (first_call)
    {
      if (ncalls > 0)
	{
	  if (one_arg)
	    outputln("default:");
	  else
	    outputln("else");
	}
      indent();
      prt_ncf_default_call(c, return_type, called_fd);
      unindent();
      if (ncalls > 0 && one_arg)
	{
	  outputln("  break;");
	  outputln("}");
	  unindent();
	}
    }
  else if (one_arg)
    {
      unindent();
      outputln("}");
    }
}

static void prt_nesc_connection_function(struct connections *c)
{
  type return_type = function_return_type(c->called);

  set_fixed_location(c->called->ast->location);

  if (type_network_base_type(return_type))
    return_type = type_network_platform_type(return_type);

  prt_ncf_header(c, return_type);

  if (c->called->gparms)
    {
      bool first_call;

      first_call = prt_ncf_direct_calls(c, c->generic_calls, return_type);
      prt_ncf_conditional_calls(c, first_call, return_type);
    }
  else
    {
      if (dd_is_empty(c->normal_calls))
	prt_ncf_default_call(c, return_type,
			     ddecl_get_fdeclarator(c->called));
      else
	prt_ncf_direct_calls(c, c->normal_calls, return_type);
    }

  prt_ncf_trailer(return_type);

  clear_fixed_location();
}

void prt_nesc_called_function_hdr(data_declaration fndecl, void *data)
{
  if (!(fndecl->defined || fndecl->uncallable) && fndecl->isused)
    {
      prt_nesc_function_hdr(fndecl, 0);
      outputln(";");
    }
  /* This is a handy place to check that all binary entry points are
     callable */
  if (!fndecl->defined && fndecl->uncallable &&
      is_binary_component(fndecl->container->impl))
    error("binary entry point %s%s%s.%s is not fully wired",
	  fndecl->container->instance_name, 
	  fndecl->interface ? "." : "",
	  fndecl->interface ? fndecl->interface->name : "",
	  fndecl->name);
}

void prt_nesc_called_function_headers(cgraph cg, nesc_declaration mod)
{
  component_functions_iterate(mod, prt_nesc_called_function_hdr, NULL);
}

void prt_nesc_module(cgraph cg, nesc_declaration mod)
{
  prt_nesc_called_function_headers(cg, mod);

  if (is_binary_component(mod->impl))
    return;

  prt_toplevel_declarations(CAST(module, mod->impl)->decls);

  /* Make local static variables gloabal when nido is used.
     Note: this raises several issues (aka problems):
     - local static variables are named mod$fn$x rather than just x
       (mildly confusing when debugging)
     - the generated code will have errors (or worse, incorrect 
       behaviour) if the local static declaration relied on local
       declarations inside fn (e.g., typedefs, enums, structs).
       There's no really nice fix to this except renaming and
       extracting all such entities (maybe this can be done when
       we have our own version of gdb and preserving symbol names
       is less important)
   */
  if (use_nido)
    {
      dd_list_pos scan;

      dd_scan (scan, mod->local_statics)
	{
	  data_declaration localsvar = DD_GET(data_declaration, scan);
	  variable_decl localsvd;
	  data_decl localsdd;

	  if (!localsvar->isused)
	    continue;

	  localsvd = CAST(variable_decl, localsvar->ast);
	  localsdd = CAST(data_decl, localsvd->parent);
	  /* Note: we don't print the elements with pte_duplicate as we
	     don't (easily) know here if the elements will be printed
	     several times. If the type elements define a new type we most
	     likely have a problem anyway (see discussion above) */
	  prt_variable_decl(localsdd->modifiers, localsvd, 0);
	  outputln(";");
	}
    }
}

static bool find_reachable_functions(struct connections *c, gnode n,
				     expression gcond, expression gargs)
{
  endp ep = NODE_GET(endp, n);

  if (ep->args_node)
    {
      /* First set of arguments is a condition if 'called' is generic */
      if (c->called->gparms && !gcond)
	gcond = ep->args_node;
      else if (gargs)
	{
	  /* We already have some arguments, so this is a condition again.
	     If the condition doesn't match gargs, then the call is
	     filtered out. If they do match, we set gargs to null (we're
	     back to a non-parameterised call) */
	  if (constant_expression_list_compare(gargs, ep->args_node) != 0)
	    return FALSE;
	  gargs = NULL;
	}
      else
	{
	  assert(!gargs);
	  gargs = ep->args_node;
	}
    }
  if (graph_node_markedp(n))
    return TRUE;
  else if (!ep->args_node && ep->function->defined &&
	   !ep->function->container->configuration)
    {
      full_connection target = new_full_connection(c->r, ep, gcond, gargs);

      assert(!graph_first_edge_out(n));

      dd_add_last(c->r, 
		  c->called->gparms && !gcond ?
		    c->generic_calls : c->normal_calls, 
		  target);
    }
  else
    {
      gedge out;

      graph_mark_node(n);
      graph_scan_out (out, n)
	if (find_reachable_functions(c, graph_edge_to(out), gcond, gargs))
	  return TRUE;
      graph_unmark_node(n);
    }
  return FALSE;
}

static void find_connected_functions(struct connections *c)
{
  gnode called_fn_node;

  graph_clear_all_marks(cgraph_graph(c->cg));
  called_fn_node = fn_lookup(c->cg, c->called);
  assert(!graph_first_edge_in(called_fn_node));
  if (find_reachable_functions(c, called_fn_node, NULL, NULL))
    error_with_location(c->called->ast->location,
			"cycle in configuration (for %s%s%s.%s)",
			c->called->container->name,
			c->called->interface ? "." : "",
			c->called->interface ? c->called->interface->name : "",
			c->called->name);
}

static void combine_warning(struct connections *c)
{
  if (warn_no_combiner)
    {
      /* Warnings to be enabled when result_t gets defined correctly */
      if (c->called->interface)
	nesc_warning("calls to %s.%s in %s fan out, but there is no combine function specified for the return type",
		     c->called->interface->name,
		     c->called->name,
		     c->called->container->name);
      else
	nesc_warning("calls to %s in %s fan out, but there is no combine function specified for the return type" ,
		     c->called->name, c->called->container->name);
    }
}

static bool combiner_used;

static bool cicn_direct_calls(dd_list/*<full_connection>*/ calls)
{
  dd_list_pos first = dd_first(calls);

  if (dd_is_end(first))
    return TRUE;

  if (!dd_is_end(dd_next(first)))
    combiner_used = TRUE;

  return FALSE;
}

static void cicn_conditional_calls(struct connections *c, bool first_call)
{
  dd_list_pos call;
  int i, j, ncalls = dd_length(c->normal_calls);
  full_connection *cond_eps =
    rarrayalloc(c->r, ncalls, full_connection);

  /* Sort calls so we can find connections with the same conditions */
  i = 0;
  dd_scan (call, c->normal_calls)
    cond_eps[i++] = DD_GET(full_connection, call);
  qsort(cond_eps, ncalls, sizeof(full_connection), condition_compare);

  /* output the calls */
  i = 0;
  while (i < ncalls)
    {
      /* find last target with same condition */
      j = i;
      while (++j < ncalls && condition_compare(&cond_eps[i], &cond_eps[j]) == 0)
	;

      if (i + first_call < j)
	combiner_used = TRUE;
      i = j;
    }
}

static void check_if_combiner_needed(struct connections *c)
{
  /* To see if a combiner is needed, we follow (a simplified form of) the
     logic used in printing the connection function (see
     prt_nesc_connection_function) */
  type return_type = function_return_type(c->called);

  if (type_void(return_type)) /* No combiner needed */
    return;

  combiner_used = FALSE;

  if (c->called->gparms)
    {
      bool first_call;

      first_call = cicn_direct_calls(c->generic_calls);
      cicn_conditional_calls(c, first_call);
    }
  else
    cicn_direct_calls(c->normal_calls);

  if (combiner_used)
    {
      c->combiner = type_combiner(return_type);
      if (!c->combiner)
	combine_warning(c);
    }
}

void find_function_connections(data_declaration fndecl, void *data)
{
  cgraph cg = data;

  if (!fndecl->defined)
    {
      region r = parse_region;
      struct connections *connections;

      fndecl->connections = connections = ralloc(r, struct connections);
      connections->r = r;
      connections->cg = cg;
      connections->called = fndecl;

      connections->generic_calls = dd_new_list(r);
      connections->normal_calls = dd_new_list(r);

      find_connected_functions(connections);

      /* a function is uncallable if it has no default definition and
	   non-generic: no connections
	   generic: no generic connections
      */
      if (!(fndecl->definition ||
	    !dd_is_empty(connections->generic_calls) ||
	    (!fndecl->gparms && !dd_is_empty(connections->normal_calls))))
	fndecl->uncallable = TRUE;
      else
	fndecl->suppress_definition =
	  !dd_is_empty(fndecl->gparms ? connections->generic_calls :
		       connections->normal_calls);

      check_if_combiner_needed(connections);
    }
}

void find_connections(cgraph cg, nesc_declaration mod)
{
  component_functions_iterate(mod, find_function_connections, cg);
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    use caller_use);

static void mark_connected_function_list(cgraph cg,
					 data_declaration caller,
					 dd_list/*full_connection*/ calls)
{
  dd_list_pos connected;

  dd_scan (connected, calls)
    {
      full_connection conn = DD_GET(full_connection, connected);

      mark_reachable_function(cg, caller, conn->ep->function,
			      new_use(dummy_location, caller, c_executable | c_fncall));
    }
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    use caller_use)
{
  dd_list_pos use;

  if (caller && ddecl->kind == decl_function)
    graph_add_edge(fn_lookup(cg, caller), fn_lookup(cg, ddecl), caller_use);

  /* Hack because ALLCODE env variable adds task decl's to spontaneous_calls */
  if (type_task(ddecl->type) && ddecl->interface)
    return;

  if (ddecl->isused)
    return;
  ddecl->isused = TRUE;

  if (ddecl->kind != decl_function ||
      (ddecl->container && 
       !(ddecl->container->kind == l_component &&
	 !ddecl->container->configuration)))
    return;

  if ((ddecl->ftype == function_command || ddecl->ftype == function_event) &&
      !ddecl->defined)
    {
      struct connections *conn = ddecl->connections;

      /* Call to a command or event not defined in this module.
	 Mark all connected functions */
      mark_connected_function_list(cg, ddecl, conn->generic_calls);
      mark_connected_function_list(cg, ddecl, conn->normal_calls);
      if (conn->combiner)
	mark_reachable_function(cg, ddecl, conn->combiner,
				new_use(dummy_location, caller, c_executable | c_fncall));

      /* Don't process body of suppressed default defs */
      if (ddecl->suppress_definition)
	return;
    }

  /* Make sure ddecl gets a node in the graph even if it doesn't call
     anything */
  fn_lookup(cg, ddecl);

  if (ddecl->fn_uses)
    dd_scan (use, ddecl->fn_uses)
      {
	iduse i = DD_GET(iduse, use);

	mark_reachable_function(cg, ddecl, i->id, i->u);
      }
}

static declaration dummy_function(data_declaration ddecl)
{
  empty_stmt body = new_empty_stmt(parse_region, dummy_location);
  function_decl fd = 
    new_function_decl(parse_region, dummy_location, NULL, NULL, NULL, NULL,
		      CAST(statement, body), NULL, NULL);

  fd->ddecl = ddecl;

  return CAST(declaration, fd);
}

static void mark_binary_reachable(data_declaration fndecl, void *data)
{
  if (fndecl->defined)
    {
      fndecl->definition = dummy_function(fndecl);
      fndecl->noinlinep = TRUE;
    }
  else
    mark_reachable_function(data, NULL, fndecl, NULL);
}

static cgraph mark_reachable_code(dd_list modules)
{
  dd_list_pos used, mod;
  cgraph cg = new_cgraph(parse_region);

  /* We use the connection graph type to represent our call graph */

  dd_scan (used, spontaneous_calls)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used), NULL);
  dd_scan (used, nglobal_uses)
    mark_reachable_function(cg, NULL, DD_GET(iduse, used)->id, NULL);

  /* All used functions from binary components are entry points */
  dd_scan (mod, modules)
    {
      nesc_declaration m = DD_GET(nesc_declaration, mod);

      if (is_binary_component(m->impl))
	component_functions_iterate(m, mark_binary_reachable, cg);
    }

  return cg;
}

static void prt_nesc_function(data_declaration fn)
{
  assert(fn->kind == decl_function);

  if (fn->definition && !fn->suppress_definition &&
      !(fn->container && is_binary_component(fn->container->impl)))
    prt_function_body(CAST(function_decl, fn->definition));

  /* if this is a connection function, print it now */
  if ((fn->ftype == function_command || fn->ftype == function_event) &&
      !fn->defined && !fn->uncallable)
    prt_nesc_connection_function(fn->connections);
}

static bool isinlined(data_declaration fn)
{
  return fn->isinline || fn->makeinline;
}

static void topological_prt(gnode gep, bool force)
{
  gedge out;
  data_declaration fn;

  fn = NODE_GET(endp, gep)->function;
  if (isinlined(fn) || force)
    {
      if (graph_node_markedp(gep))
	return;

      graph_mark_node(gep);

      graph_scan_out (out, gep)
	topological_prt(graph_edge_to(out), FALSE);

      prt_nesc_function(fn);
    }
}
static void prt_inline_functions(cgraph callgraph)
{
  gnode fns;

  graph_clear_all_marks(cgraph_graph(callgraph));
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
      data_declaration fn = NODE_GET(endp, fns)->function;
      if (isinlined(fn))
	{
	  gedge callers;
	  bool inlinecallers = FALSE;

	  graph_scan_in (callers, fns)
	    {
	      data_declaration caller =
		NODE_GET(endp, graph_edge_from(callers))->function;

	      if (isinlined(caller))
		{
		  inlinecallers = TRUE;
		  break;
		}
	    }

	  if (!inlinecallers)
	    topological_prt(fns, FALSE);
	}
    }
}

static void prt_noninline_functions(cgraph callgraph)
{
  gnode fns;
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
      data_declaration fn = NODE_GET(endp, fns)->function;

      if (!isinlined(fn))
	{
	  /* There may be some inlined functions which were not printed
	     earlier, because they were:
	     a) recursive (possibly indirectly), with explicit inline
	        keywords
	     b) not called from another inline function
	     So we use topological_prt here to ensure they are printed
	     before any calls to them from non-inlined functions
	  */
	  topological_prt(fns, TRUE);
	}
    }
}

static void suppress_function(const char *name)
{
  data_declaration d = lookup_global_id(name);

  if (d && d->kind == decl_function && d->definition)
    d->suppress_definition = TRUE;
}

static void prt_ddecl_for_init(region r, data_declaration ddecl)
{
  type_quals dquals = type_qualifiers(ddecl->type);
  if (dquals & const_qualifier)
    {
      /* We need to cast the const away */
      type nonconst;
      declarator nc_decl;
      type_element nc_mods;
      asttype nc_type;

      output("*(");
      nonconst = make_qualified_type(ddecl->type, dquals & ~const_qualifier);
      type2ast(r, dummy_location, make_pointer_type(nonconst), NULL,
	       &nc_decl, &nc_mods);
      nc_type = new_asttype(r, dummy_location, nc_decl, nc_mods);
      prt_asttype(nc_type);
      output(")&");
    }
  prt_plain_ddecl(ddecl, 0);
  output("[__nesc_mote]");
}

static void prt_nido_initializer(region r, variable_decl vd)
{
  data_declaration ddecl = vd->ddecl;
  expression init;

  if (!ddecl || !ddecl->isused || ddecl->kind != decl_variable)
    return; /* Don't print if not referenced */

  init = vd->arg1;

  if (!init)
    {
      output("memset((void *)&");
      prt_ddecl_for_init(r, ddecl);
      output(", 0, sizeof ");
      prt_ddecl_for_init(r, ddecl);
      output(")");
    }
  else if (is_init_list(init))
    {
      declarator vtype;
      type_element vmods;

      output("memcpy((void *)&");
      prt_ddecl_for_init(r, ddecl);

      output(", (void *)&");
      type2ast(parse_region, dummy_location, ddecl->type, NULL,
	       &vtype, &vmods);
      output("(");
      prt_declarator(vtype, vmods, NULL, NULL, 0);
      output(")");
      prt_expression(init, P_ASSIGN);

      output(", sizeof ");
      prt_ddecl_for_init(r, ddecl);
      output(")");
    }
  else 
    {
      prt_ddecl_for_init(r, ddecl);
      output(" = ");
      prt_expression(init, P_ASSIGN);
    }
  outputln(";");
}

static void prt_nido_initializations(nesc_declaration mod) 
{
  declaration dlist;
  declaration d;
  dd_list_pos lscan;
  region r;

  /* binary component? */
  if (!is_module(mod->impl))
    return;

  r = newregion();
  dlist = CAST(module, mod->impl)->decls;
  outputln("/* Module %s */", mod->name);

  /* Static variables */
  scan_declaration (d, dlist)
    {
      declaration reald = ignore_extensions(d);
      variable_decl vd;

      if (reald->kind != kind_data_decl)
	continue;

      scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	prt_nido_initializer(r, vd);
    }

  /* Local static variables */
  dd_scan (lscan, mod->local_statics)
    {
      data_declaration localsd = DD_GET(data_declaration, lscan);

      prt_nido_initializer(r, CAST(variable_decl, localsd->ast));
    }
  deleteregion(r);
  newline();
}

static void prt_nido_initialize(dd_list modules) 
{
  dd_list_pos mod;

  nido_mote_number = "__nesc_mote";
  outputln("/* Invoke static initialisers for mote '__nesc_mote' */\n");
  outputln("static void __nesc_nido_initialise(int __nesc_mote)");
  outputln("{");
  indent();

  dd_scan (mod, modules) 
    prt_nido_initializations(DD_GET(nesc_declaration, mod));
 
  unindent();
  outputln("}");
}

static void prt_typedefs(nesc_declaration comp)
{
  declaration parm;

  scan_declaration (parm, comp->parameters)
    if (is_type_parm_decl(parm))
      {
	type_parm_decl td = CAST(type_parm_decl, parm);
	asttype arg = CAST(type_argument, td->ddecl->initialiser)->asttype;

	output("typedef ");
	prt_declarator(arg->declarator, arg->qualifiers, NULL, td->ddecl,
		       psd_print_ddecl);
	outputln(";");
      }
}

void prt_nesc_interface_typedefs(nesc_declaration comp)
{
  const char *ifname;
  void *ifentry;
  env_scanner scanifs;

  env_scan(comp->env->id_env, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      if (idecl->kind == decl_interface_ref)
	prt_typedefs(idecl->itype);
    }
}

void prt_nesc_typedefs(nesc_declaration comp);

void prt_configuration_declarations(declaration dlist)
{
  declaration d;

  scan_declaration (d, dlist)
    if (is_component_ref(d))
      prt_nesc_typedefs(CAST(component_ref, d)->cdecl);
    else
      prt_toplevel_declaration(d);
}

void prt_nesc_typedefs(nesc_declaration comp)
{
  assert(!comp->abstract);
  if (comp->printed)
    return;
  comp->printed = TRUE;
  if (comp->original)
    prt_typedefs(comp);

  /* Print declarations in specification */
  prt_toplevel_declarations(CAST(component, comp->ast)->decls);

  /* Only module interface type arguments are used in output */
  if (!comp->configuration)
    prt_nesc_interface_typedefs(comp);
  else
    /* Recursively print declarations found in configurations */
    prt_configuration_declarations(CAST(configuration, comp->impl)->decls);
}

static void prt_nido_resolver(region r, variable_decl vd)
{
  data_declaration ddecl = vd->ddecl;
  expression init;

  if (!ddecl || !ddecl->isused || ddecl->kind != decl_variable)
    return; /* Don't print if not referenced */

  init = vd->arg1;

  output("if (!strcmp(varname, \"");
  prt_plain_ddecl(ddecl, 0);
  outputln("\"))");
  outputln("{");
  indent();
  output("*addr = (uintptr_t)&");
  prt_ddecl_for_init(r, ddecl);
  outputln(";");
  output("*size = sizeof(");
  prt_ddecl_for_init(r, ddecl);
  outputln(");");
  outputln("return 0;");
  unindent();
  outputln("}");
}

static void prt_nido_resolvers(nesc_declaration mod) 
{
  declaration dlist;
  declaration d;
  region r;

  /* binary component? */
  if (!is_module(mod->impl))
    return;

  r = newregion();
  dlist = CAST(module, mod->impl)->decls;
  outputln("/* Module %s */", mod->name);

  /* Static variables */
  scan_declaration (d, dlist)
    {
      declaration reald = ignore_extensions(d);
      variable_decl vd;

      if (reald->kind != kind_data_decl)
	continue;
      
      scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	prt_nido_resolver(r, vd);
    }
  deleteregion(r);

  newline();
}

static void prt_nido_resolver_function(dd_list modules)
{
  dd_list_pos mod;

  outputln("/* Nido variable resolver function */\n");
  outputln("static int __nesc_nido_resolve(int __nesc_mote,");
  outputln("                               char* varname,");
  outputln("                               uintptr_t* addr, size_t* size)");
  outputln("{");
  indent();
  
  dd_scan (mod, modules) 
    prt_nido_resolvers(DD_GET(nesc_declaration, mod));

  outputln("return -1;");
  
  unindent();
  outputln("}");
}

static void include_support_functions(void)
{
  static char *fns[] = {
    "__nesc_atomic_start",
    "__nesc_atomic_end",
    "__nesc_enable_interrupt",
    "__nesc_disable_interrupt",
    "__nesc_nido_initialise",
    "__nesc_nido_resolve"
  };
  int i;

  for (i = 0; i < sizeof fns / sizeof *fns; i++)
    {
      data_declaration fndecl = lookup_global_id(fns[i]);

      /* Adding the function to spontaneous_calls w/o setting the
	 spontaneous field makes the function stay static */
      if (fndecl && fndecl->kind == decl_function && !fndecl->spontaneous)
	dd_add_last(parse_region, spontaneous_calls, fndecl);
    }
}

void generate_c_code(const char *target_name, nesc_declaration program,
		     cgraph cg, dd_list modules, dd_list components)
{
  dd_list_pos mod;
  cgraph callgraph;
  FILE *output = NULL, *diff_file = NULL;

  if (target_name)
    {
      output = fopen(target_name, "w");
      if (!output)
	{
	  perror("couldn't create output file");
	  exit(2);
	}
    }

  if (diff_output)
    {
      char *diffname = rstralloc(permanent, strlen(diff_output) + 9);

      if (use_nido)
	{
	  /* There's no use for nido+diffs, and it would complicate
	     things somewhat (nido changes rules for symbols, etc) */
	  error("diff output is not supported with simulation");
	  exit(1);
	}
      sprintf(diffname, "%s/symbols", diff_output);
      diff_file = fopen(diffname, "w");
      if (!diff_file)
	{
	  fprintf(stderr, "couldn't create diff output file %s: ", diffname);
	  perror(NULL);
	  exit(2);
	}
    }

  include_support_functions();

  unparse_start(output ? output : stdout, diff_file);
  disable_line_directives();

  /* define nx_struct as struct and nx_union as union (simplifies
     prt_tag) */
  outputln("#define nx_struct struct");
  outputln("#define nx_union union");

  /* suppress debug functions if necessary */
  if (flag_no_debug)
    {
      suppress_function("dbg");
      suppress_function("dbg_clear");
      suppress_function("dbg_active");
      outputln("#define dbg(mode, format, ...) ((void)0)");
      outputln("#define dbg_clear(mode, format, ...) ((void)0)");
      outputln("#define dbg_active(mode) 0");
    }

  /* We start by finding each module's identifier uses and connections
     and marking uncallable functions */
  collect_uses(all_cdecls);
  handle_network_types(all_cdecls);
  dd_scan (mod, modules)
    {
      nesc_declaration m = DD_GET(nesc_declaration, mod);

      if (is_module(m->impl))
	{
	  declaration body = CAST(module, m->impl)->decls;

	  collect_uses(body);
	  handle_network_types(body);
	}
      
      find_connections(cg, m);
    }

  /* Then we set the 'isused' bit on all functions that are reachable
     from spontaneous_calls or global_uses */
  callgraph = mark_reachable_code(modules);

  check_async(callgraph);
  check_races(callgraph);
  isatomic(callgraph);

  inline_functions(callgraph);

  /* Then we print the code. */
  /* The C declarations first */
  enable_line_directives();
  prt_toplevel_declarations(all_cdecls);
  disable_line_directives();

  /* Typedefs for abstract module type arguments. This relies on the fact
     that abstract configurations are present in the components list ahead
     of the abstract modules that they instantiate */
  if (program)
    prt_nesc_typedefs(program);

  enable_line_directives();

  dd_scan (mod, modules)
    prt_nesc_function_declarations(DD_GET(nesc_declaration, mod));

  dd_scan (mod, modules)
    prt_nesc_module(cg, DD_GET(nesc_declaration, mod));

  prt_inline_functions(callgraph);
  prt_noninline_functions(callgraph);

  if (use_nido)
    {
      prt_nido_resolver_function(modules);
      disable_line_directives();
      prt_nido_initialize(modules); 
    }

  unparse_end();

  if (output)
    fclose(output);
  if (diff_file)
    fclose(diff_file);
}
