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
#include "nesc-configuration.h"
#include "nesc-component.h"
#include "nesc-env.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "constants.h"
#include "c-parse.h"
#include "expr.h"
#include "nesc-abstract.h"

/* define this to forbid linking a single function from an interface
   independently of the whole interface */
#define NO_FUNCTION_INTERFACE_MATCHING

/* define this to forbid the implicit multiple matches from 
   component -> component connections */
#define NO_COMPONENT_MATCHING

void component_scan(data_declaration cref, env_scanner *scan)
{
  env_scan(cref->ctype->env->id_env, scan);
}

static void connect_userg(location l, cgraph userg, struct endp from, struct endp to)
{
  gnode gfrom = endpoint_lookup(userg, &from), gto = endpoint_lookup(userg, &to);

  graph_add_edge(gfrom, gto, l);
}
 
static void connect_cg(cgraph cg, struct endp from, struct endp to)
{
  gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

  graph_add_edge(gfrom, gto, NULL);
  /* If an endpoint has args, we must also connect the node w/o args */
  if (from.args_node)
    graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
  if (to.args_node)
    graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

static void connect_function(location l, cgraph cg, cgraph userg,
			     struct endp from, struct endp to)
{
  connect_cg(cg, from, to);
  connect_userg(l, userg, from, to);
}

static type endpoint_type(endp p)
{
  type t = NULL;

  if (p->args_node)
    {
      if (p->function)
	t = type_function_return_type(p->function->type);
      else if (p->interface)
	t = p->interface->type;
    }
  else
    {
      if (p->function)
	t = p->function->type;
      else if (p->interface)
	{
	  t = p->interface->type;

	  /* We don't normally include the generic parameters in the 
	     interface's type, but we do here to allow correct matching */
	  if (p->interface->gparms)
	    t = make_generic_type(t, p->interface->gparms);
	}
    }
  return t;
}

typelist endpoint_args(endp p)
{
  if (p->function)
    {
      type t = p->function->type;

      if (type_generic(t))
	return type_function_arguments(t);
    }
  else if (p->interface)
    return p->interface->gparms;

  return NULL;
}

void connect_interface(location l, cgraph cg, cgraph userg,
		       struct endp from, struct endp to,
		       bool reverse)
{
  env_scanner scanfns;
  const char *fnname;
  void *fnentry;

  if (to.interface->required ^ reverse)
    connect_userg(l, userg, to, from);
  else
    connect_userg(l, userg, from, to);

  assert(!from.function && !to.function
	 /*&& from.interface->itype == to.interface->itype*/);

  /* All functions */
  interface_scan(to.interface, &scanfns);
  while (env_next(&scanfns, &fnname, &fnentry))
    {
      data_declaration fndecl = fnentry;

      assert(fndecl->kind == decl_function);
      to.function = fndecl;
      from.function = env_lookup(from.interface->functions->id_env, fndecl->name, TRUE);
      if (fndecl->defined ^ reverse)
	connect_cg(cg, from, to);
      else
	connect_cg(cg, to, from);
    }
}


int match_endpoints(endp p1, endp p2, endp amatch)
{
  /* Should this be type_equal ? unclear 
     (only real diff, given that we will forbid old style parameter lists,
     is transparent union handling) */
  if (type_compatible(endpoint_type(p1), endpoint_type(p2)))
    {
      if (amatch)
	*amatch = *p2;
      return 1;
    }
  else
    return 0;
}

int match_function_interface(bool eqconnection,
			     struct endp f, struct endp i, endp amatch)
{
#ifdef NO_FUNCTION_INTERFACE_MATCHING
  return 0;
#else
  env_scanner scanfns;
  const char *fnname;
  void *fnentry;
  int matched = 0;
  bool want_defined;

  assert(f.function && !i.function);

  want_defined = f.function->defined ^ !eqconnection;

  /* Check all functions */
  interface_scan(i.interface, &scanfns);
  while (env_next(&scanfns, &fnname, &fnentry))
    {
      i.function = fnentry;
      if (i.function->defined == want_defined)
	matched += match_endpoints(&f, &i, amatch); 
    }

  return matched;
#endif
}

int match_interface_component(bool eqconnection,
			      struct endp i, struct endp c, endp amatch)
{
  const char *ifname;
  void *ifentry;
  int matched = 0;
  env_scanner scanifs;
  bool want_required;

  assert(i.interface && !c.interface);

  want_required = i.interface->required ^ !eqconnection;

  component_scan(c.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      if (idecl->kind == decl_interface_ref)
	{
	  c.interface = idecl;
	  if (c.interface->required == want_required)
	    matched += match_endpoints(&i, &c, amatch);
	}
    }
  return matched;
}

int match_function_component(bool eqconnection,
			     struct endp f, struct endp c, endp amatch)
{
  const char *ifname;
  void *ifentry;
  int matched = 0;
  env_scanner scanifs;
  bool want_defined;

  assert(f.function && !c.interface && !c.function);

  want_defined = f.function->defined ^ !eqconnection;

  component_scan(c.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      c.function = c.interface = NULL;
      if (idecl->kind == decl_interface_ref)
	{
	  c.interface = idecl;
	  matched += match_function_interface(want_defined ^ idecl->required,
					      f, c, amatch);
	}
      else
	{
	  c.function = idecl;
	  if (c.function->defined == want_defined)
	    matched += match_endpoints(&f, &c, amatch);
	}
    }
  return matched;
}


void check_generic_arguments(expression args, typelist gparms)
{
  expression arg;
  typelist_scanner scan_gparms;

  typelist_scan(gparms, &scan_gparms);
  scan_expression (arg, args)
    {
      location l = arg->location;
      type gparm_type = typelist_next(&scan_gparms);

      if (!gparm_type)
	{
	  error_with_location(l, "too many arguments");
	  return;
	}

      if (arg->type == error_type || !check_constant_once(arg, cst_numerical))
	continue;

      if (!arg->cst || !constant_integral(arg->cst))
	error_with_location(l, "constant expression expected");
      else
	{
	  if (!cval_inrange(arg->cst->cval, gparm_type))
	    error_with_location(l, "constant out of range for argument type");
	}
    }
  if (typelist_next(&scan_gparms))
    error_with_location(args->location, "too few arguments");
}

static bool lookup_endpoint(environment configuration_env, endpoint ep,
			    endp lep)
{
  parameterised_identifier pid;
  environment lookup_env = configuration_env;

  lep->component = lep->interface = lep->function = NULL;
  lep->args_node = NULL;

  scan_parameterised_identifier (pid, ep->ids)
    {
      const char *idname = pid->word1->cstring.data;
      location l = pid->location;

      if (!lookup_env)
	error_with_location(l, "unexpected identifier `%s'", idname);
      else
	{
	  expression args = pid->args;
	  data_declaration d = env_lookup(lookup_env->id_env, idname, TRUE);

	  if (!d)
	    {
	      /* This is a bit hacky: lookup in parent env, but not if
		 it's the global env. We want to check a configuration's
		 env, and it's parent component's env, but not the global
		 env. */
	      if (lookup_env->parent && lookup_env->parent != global_env)
		d = env_lookup(lookup_env->parent->id_env, idname, TRUE);
	      if (!d)
		{
		  error_with_location(l, "cannot find `%s'", idname);
		  return FALSE; /* prevent cascading error messages */
		}
	    }

	  if (args)
	    {
	      if (pid->next)
		error_with_location(l, "arguments must be specified last");
	      lep->args_node = pid->args;
	    }

	  switch (d->kind)
	    {
	    default:
	      error_with_location(l, "cannot find `%s'", idname);
	      return FALSE; /* prevent cascading error messages */

	    case decl_component_ref:
	      assert(!lep->component);
	      lep->component = d;
	      lookup_env = d->ctype->env;
	      break;
	    case decl_interface_ref:
	      assert(!lep->interface);
	      lep->interface = d;

#ifdef NO_FUNCTION_INTERFACE_MATCHING
	      /* Can't lookup a function inside an interface (no partial interface
		 connections) */
	      lookup_env = NULL;
#else
	      /* Get next environment */
	      lookup_env = d->itype->decls;
#endif
	      break;
	    case decl_function:
	      lep->function = d;
	      lookup_env = NULL;
	      break;
	    }
	}
    }

  /* Check generic arguments */
  if (lep->args_node)
    {
      typelist gparms = endpoint_args(lep);

      if (gparms)
	check_generic_arguments(lep->args_node, gparms);
      else
	error_with_location(ep->location, "endpoint is not a parameterised interface");
    }

  return TRUE;
}


static void process_interface_connection(cgraph cg, cgraph userg, connection conn,
					 struct endp p1, struct endp p2)
{
  location l = conn->location;

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (!p1.component && !p2.component)
	{
	  if (p1.interface->required == p2.interface->required)
	    error_with_location(l, "external to external connections must be between provided and used interfaces");
	  else
	    connect_interface(l, cg, userg, p1, p2, TRUE);
	}
      else
	{
	  if (p1.interface->required != p2.interface->required)
	    error_with_location(l, "external to internal connections must be both provided or both used");
	  else if (!p1.component)
	    connect_interface(l, cg, userg, p1, p2, FALSE);
	  else
	    connect_interface(l, cg, userg, p2, p1, FALSE);
	  /* Note: connect_interface takes care of choosing the right edge
	     direction. There are two cases:
	     - the interface is provided: then we want edges from outside in,
	     so from = the outside interface
	     - the interface is required: then we want edges from inside out,
	     but connect_interface will reverse them because the interface
	     is required. So we also pick from = the outside interface.
	  */
	}
    }
  else /* p1 <- p2 */
    {
      if (p1.interface->required)
	error_with_location(l, "target of '<-' interface must be provided");
      else if (!p2.interface->required)
	error_with_location(l, "source of '<-' interface must be required");
      else connect_interface(l, cg, userg, p2, p1, FALSE);
    }
}

static void process_function_connection(cgraph cg, cgraph userg, connection conn,
					struct endp p1, struct endp p2)
{
  location l = conn->location;
  bool p1def = (p1.interface && !p1.interface->required) ^ p1.function->defined;
  bool p2def = (p2.interface && !p2.interface->required) ^ p2.function->defined;

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (!p1.component && !p2.component)
	{
	  if (p1def == p2def)
	    error_with_location(l, "external to external connections must be between provided and used functions");
	  else if (p1def)
	    connect_function(l, cg, userg, p1, p2); /* from provided to used */
	  else
	    connect_function(l, cg, userg, p2, p1);
	}
      else 
	{
	  if (p1def != p2def)
	    error_with_location(l, "external to internal connections must be both provided or both used");
	  else if ((!p1.component && !p1def) || (p1.component && p1def))
	    connect_function(l, cg, userg, p2, p1);
	  else
	    connect_function(l, cg, userg, p1, p2);
	}
    }
  else /* p1 <- p2 */
    {
      if (!p1def)
	error_with_location(l, "target of '<-' function must be defined");
      else if (p2def)
	error_with_location(l, "source of '<-' function must be used");
      else connect_function(l, cg, userg, p2, p1);
    }
}

static void process_actual_connection(cgraph cg, cgraph userg, connection conn,
				      struct endp p1, struct endp p2)
{
  location l = conn->location;

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (p1.component && p2.component)
	error_with_location(l, "there must be at least one external interface in an '=' connection");
    }
  else /* p1 <- p2 */
    {
      if (!p1.component || !p2.component)
	error_with_location(l, "external interfaces cannot be connected with `<-' or `->'");
    }

  if (p1.function)
    process_function_connection(cg, userg, conn, p1, p2);
  else
    process_interface_connection(cg, userg, conn, p1, p2);
}

static void process_connection(cgraph cg, cgraph userg, connection conn,
			       struct endp p1, struct endp p2)
{
  int matches;
  bool eqconnection = is_eq_connection(conn);

  if (p1.function) /* f X ... */
    {
      if (p2.function) /* f X f */
	matches = match_endpoints(&p1, &p2, NULL);
      else if (p2.interface) /* f X i */
	matches = match_function_interface(eqconnection, p1, p2, &p2);
      else /* f X c */
	matches = match_function_component(eqconnection, p1, p2, &p2);
    }
  else if (p1.interface) /* i X ... */
    {
      if (p2.function) /* i X f */
	matches = match_function_interface(eqconnection, p2, p1, &p1);
      else if (p2.interface) /* i X i */
	matches = match_endpoints(&p1, &p2, NULL);
      else /* i X c */
	matches = match_interface_component(eqconnection, p1, p2, &p2);
    }
  else /* c X ... */
    {
      if (p2.function) /* c X f */
	matches = match_function_component(eqconnection, p2, p1, &p1);
      else /* c X i */
	matches = match_interface_component(eqconnection, p2, p1, &p1);
    }

  if (matches == 0)
    error_with_location(conn->location, "no match");
  else if (matches > 1)
    error_with_location(conn->location, "ambiguous match");
  else 
    process_actual_connection(cg, userg, conn, p1, p2);
}

static void process_component_connection(cgraph cg, cgraph userg, connection conn,
					 struct endp p1, struct endp p2)
{
#ifndef NO_COMPONENT_MATCHING
  /* c X c, the only list case */
  const char *ifname;
  void *ifentry;
  int total_matches = 0;
  env_scanner scanifs;
  bool eqconnection = is_eq_connection(conn);

  component_scan(p1.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;
      int matches;

      p1.interface = p1.function = p2.interface = p2.function = NULL;
      if (idecl->kind == decl_interface_ref)
	{
	  p1.interface = idecl;
	  matches = match_interface_component(eqconnection, p1, p2, &p2);
	}
      else
	{
	  p1.function = idecl;
	  matches = match_function_component(eqconnection, p1, p2, &p2);
	}

      total_matches += matches;
      if (matches > 1)
	{
	  error_with_location(conn->location, "ambiguous match");
	  break;
	}
      else if (matches == 1)
	process_actual_connection(cg, userg, conn, p1, p2);
    }
  if (total_matches == 0)
#endif
    error_with_location(conn->location, "no match");
}

static void process_connections(configuration c)
{
  declaration decl;
  struct endp p1, p2;
  cgraph cg = c->cdecl->connections;
  cgraph userg = c->cdecl->user_connections;

  scan_declaration (decl, c->decls)
    if (is_connection(decl))
      {
	connection conn = CAST(connection, decl);

	if (lookup_endpoint(c->ienv, conn->ep1, &p1) &&
	    lookup_endpoint(c->ienv, conn->ep2, &p2))
	  {
	    /* There are a lot of kinds of connections here.
	       lookup_endpoint has already resolved pseudo-interfaces to functions
	       (c is component, i is interface, f is function, X is = or <-)
	       c X c, c X i, i X c, c X f, f X c, i X i, i X f, f X i, f X f

	       We first resolve the c X c case, which can lead to multiple
	       connections, then handle all remaining cases in process_connection
	    */
	    if (!p1.interface && !p2.interface && !p1.function && !p2.function)
	      process_component_connection(cg, userg, conn, p1, p2);
	    else
	      process_connection(cg, userg, conn, p1, p2);
	  }
      }
}

component_ref require_component(component_ref comp, word as)
{
  struct data_declaration tempdecl;
  data_declaration old_decl, ddecl;
  const char *cname = comp->word1->cstring.data;
  const char *asname = (as ? as : comp->word1)->cstring.data;

  comp->word2 = as;

  comp->cdecl = require(l_component, comp->location, cname);

  init_data_declaration(&tempdecl, CAST(declaration, comp), asname,
			void_type);
  tempdecl.kind = decl_component_ref;

  /* Avoid duplicates in implementation *or* specification env */
  old_decl = lookup_id(asname, TRUE);
  if (!old_decl)
    old_decl = env_lookup(current.env->parent->id_env, asname, TRUE);
  if (old_decl)
    error_with_location(comp->location, "redefinition of `%s'", asname);
  ddecl = declare(current.env, &tempdecl, FALSE);

  /* If the component is abstract, we make a copy of its specification
     so that we produce an accurate connection graph. We don't
     actually instantiate the component until later.
     This copy is "abstract" (will need further copying) if we are
     processing an abstract configuration */
  if (comp->cdecl->abstract)
    {
      generic_used = TRUE;

      comp->cdecl = specification_copy(parse_region, comp,
				       current.container->abstract);
      /* give copy a nice instance name if it is inside a generic
	 configuration */
      if (current.container->abstract)
	{
	  size_t inamelen = strlen(current.container->name) +
	    strlen(comp->cdecl->instance_name) + 2;
	  char *iname = rstralloc(parse_region, inamelen);

	  sprintf(iname, "%s.%s", current.container->name, comp->cdecl->instance_name);
	  comp->cdecl->instance_name = iname;
	}

      if (!comp->abstract)
	error_with_location(comp->location, "generic component `%s' requires instantiation arguments", cname);
      else
	check_abstract_arguments("component", ddecl, comp->cdecl->parameters, comp->args);
    }
  else
    {
      if (comp->abstract)
	error_with_location(comp->location, "component `%s' is not generic", cname);
    }

  ddecl->type = make_component_type(ddecl);
  ddecl->ctype = comp->cdecl;

  return comp;
}

expression make_component_deref(location loc, expression object, cstring field)
{
  expression result;
  data_declaration cref = type_cref(object->type);
  data_declaration fdecl = env_lookup(cref->ctype->env->id_env, field.data, TRUE);
  if (!fdecl)
    {
      error("component's specification has no element named `%s'", field.data);
      fdecl = bad_decl;
    }
  result = CAST(expression, new_component_deref(parse_region, loc, object, field, fdecl));
  result->type = fdecl->type;
  result->cst = fold_identifier(result, fdecl, 0);

  return CAST(expression, result);
}

struct cfc_data
{
  location loc;
  cgraph cg;
  data_declaration intf_last_error;
};

/* Check that function fndecl (from the configuration's external interface)
   is connected, i.e.:
   - if defined there is an outgoing edge
   - if used there is an incoming edge
*/
static void check_function_connected(data_declaration fndecl, void *data)
{
  struct cfc_data *d = data;
  gnode epnode;
  data_declaration idecl = fndecl->interface;

  assert(fndecl->kind == decl_function);

#ifdef NO_FUNCTION_INTERFACE_MATCHING
  /* Avoid duplicate error messages: if one function not connected in
     an interface, then none are */
  if (idecl == d->intf_last_error)
    return;
#endif

  epnode = fn_lookup(d->cg, fndecl);

  if ((fndecl->defined && !graph_first_edge_out(epnode)) ||
      (!fndecl->defined && !graph_first_edge_in(epnode)))
    {
      d->intf_last_error = idecl;

      if (idecl)
#ifdef NO_FUNCTION_INTERFACE_MATCHING
	error_with_location(d->loc, "`%s' not connected", idecl->name);
#else
	error_with_location(d->loc, "`%s.%s' not connected",
			    idecl->name, fndecl->name);
#endif
      else
	error_with_location(d->loc, "`%s' not connected", fndecl->name);
    }
}

/* Checks that all external interfaces/functions of the configuration
   are connected somewhere in cg */
static void check_complete_connection(configuration c)
{
  struct cfc_data d;

  d.intf_last_error = NULL;
  d.loc = c->location;
  d.cg = c->cdecl->connections;
  component_functions_iterate(c->cdecl, check_function_connected, &d);
}

void process_configuration(configuration c)
{
  int old_errorcount = errorcount;

  process_connections(c);

  /* Don't give error messages for missing connections if we found
     errors in the connections (to avoid duplicate errors) */
  if (old_errorcount == errorcount)
    check_complete_connection(c);
}
