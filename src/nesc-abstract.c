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

static AST_walker clone_walker;

/* ddecls in:
   oldidentifier_decl: ignored as illegal in modules
   string

   enumerator
   function_decl
   identifier
   interface_deref
   typename
   variable_decl
*/

static void forward(data_declaration *dd)
{
  data_declaration ddecl = *dd;

  if (ddecl->instantiation)
    *dd = ddecl->instantiation;
}

static void clone_ddecl(data_declaration ddecl)
{
  data_declaration copy;

  /* If already cloned, return. */
  if (ddecl->instantiation &&
      ddecl->instantiation->container == current.container)
    return;

  assert(!ddecl_is_command_or_event(ddecl));

  /* Copy module functions (incl. tasks) and variables */

  if (!(ddecl->kind == decl_variable || ddecl->kind == decl_function ||
	ddecl->kind == decl_constant || ddecl->kind == decl_typedef))
    return;

  /* Ignore non-module variables */
  if (!ddecl->container)
    return;

  copy = declare(current.env, ddecl, TRUE);
  ddecl->instantiation = copy;
  copy->fn_uses = NULL;
  copy->nuses = NULL;
  copy->shadowed = ddecl;
  copy->container = current.container;
}

static AST_walker_result clone_function_decl(AST_walker spec, void *data,
					     function_decl *n)
{
  function_decl new = CAST(function_decl, AST_clone(data, CAST(node, *n)));

  clone_ddecl(new->ddecl);

  if (new->ddecl->instantiation)
    {
      data_declaration instance = new->ddecl->instantiation;

      /* We need to forward the ddecl *and* update the definition field in
	 the instantiated data_declaration. */
      instance->definition = CAST(declaration, new);
      /* We update the ast field if it pointed to this function_decl
	 (note that command and event data_declarations assume that the
	 ast field points to the original variable_decl) */
      if (instance->ast == CAST(declaration, *n))
	instance->ast = CAST(declaration, new);
      new->ddecl = instance;
    }
  *n = new;

  return aw_walk;
}

static AST_walker_result clone_identifier(AST_walker spec, void *data,
					  identifier *n)
{
  identifier new = CAST(identifier, AST_clone(data, CAST(node, *n)));

  forward(&new->ddecl);
  *n = new;

  return aw_walk;
}

static AST_walker_result clone_interface_deref(AST_walker spec, void *data,
					     interface_deref *n)
{
  interface_deref new = CAST(interface_deref, AST_clone(data, CAST(node, *n)));

  forward(&new->ddecl);
  *n = new;

  return aw_walk;
}

static AST_walker_result clone_variable_decl(AST_walker spec, void *data,
					     variable_decl *n)
{
  variable_decl new = CAST(variable_decl, AST_clone(data, CAST(node, *n)));

  clone_ddecl(new->ddecl);

  if (new->ddecl->instantiation)
    {
      data_declaration instance = new->ddecl->instantiation;

      /* Forward the ddecl and update the ast and definition fields */
      if (instance->definition == CAST(declaration, *n))
	instance->definition = CAST(declaration, new);
      if (instance->ast == CAST(declaration, *n))
	instance->ast = CAST(declaration, new);
      new->ddecl = instance;
    }
  *n = new;

  return aw_walk;
}

static AST_walker_result clone_typename(AST_walker spec, void *data,
					  typename *n)
{
  typename new = CAST(typename, AST_clone(data, CAST(node, *n)));

  forward(&new->ddecl);
  *n = new;

  return aw_walk;
}

static AST_walker_result clone_enumerator(AST_walker spec, void *data,
					  enumerator *n)
{
  enumerator new = CAST(enumerator, AST_clone(data, CAST(node, *n)));

  clone_ddecl(new->ddecl);

  if (new->ddecl->instantiation)
    {
      data_declaration instance = new->ddecl->instantiation;

      /* Forward the ddecl and update the ast and definition fields */
      instance->definition = CAST(declaration, new);
      instance->ast = CAST(declaration, new);
      new->ddecl = instance;
    }

  *n = new;

  return aw_walk;
}

static AST_walker_result clone_ast(AST_walker spec, void *data, node *n)
{
  *n = AST_clone(data, *n);

  return aw_walk;
}

static void init_clone(void)
{
  clone_walker = new_AST_walker(permanent);
  AST_walker_handle(clone_walker, kind_node, clone_ast);
  AST_walker_handle(clone_walker, kind_function_decl, clone_function_decl);
  AST_walker_handle(clone_walker, kind_identifier, clone_identifier);
  AST_walker_handle(clone_walker, kind_interface_deref, clone_interface_deref);
  AST_walker_handle(clone_walker, kind_variable_decl, clone_variable_decl);
  AST_walker_handle(clone_walker, kind_typename, clone_typename);
  AST_walker_handle(clone_walker, kind_enumerator, clone_enumerator);
}

void set_ddecl_instantiation(data_declaration fndecl, void *data)
{
  data_declaration orig = fndecl;

  /* May be 1 or 2 deep, see below. The last shadowed entry points to the
     declaration in the interface type, we want the one above that... */
  while (orig->shadowed->shadowed)
    orig = orig->shadowed;

  assert(orig != fndecl);
  orig->instantiation = fndecl;
}

void set_specification_instantiations(nesc_declaration component)
/* Effects: Set the instantiation pointers in the data_declarations of
     the original abstract component from which component is derived to
     the copies in component (in preparation for cloning component's
     AST and pointing to component's decls)

     The original data_declarations can be found by following the
     shadowed fields. We may have to follow these one deep (abstract
     modules in configurations) or two deep (abstract modules in
     abstract configurations)...
*/
{
  component_functions_iterate(component, set_ddecl_instantiation, NULL);
}

static declaration instantiate_parameters(declaration orig_parms)
/* Effects: Makes a new list of declarations for an abstract componnent
*/
{
  region r = parse_region;

  /* A new dummy env for the instantiated parameters */
  current.env = new_environment(r, NULL, TRUE, FALSE);
  AST_walk_list(clone_walker, r, CASTPTR(node, &orig_parms));
  AST_set_parents(CAST(node, orig_parms));

  return CAST(declaration, orig_parms);
}

void instantiate_module(nesc_declaration component, module mod)
{
  region r = parse_region;

  current.container = component;
  component->parameters = instantiate_parameters(component->parameters);

  set_specification_instantiations(component);

  /* A new dummy env for all instantiations in the module */
  current.env = new_environment(r, NULL, TRUE, FALSE);
  AST_walk(clone_walker, r, CASTPTR(node, &mod));
  AST_set_parents(CAST(node, mod));
  component->impl = CAST(implementation, mod);
}

static void instantiate_endp(endp ep)
/* Effects: Modifies ep based on instantiated ddecls
 */
{
  /* The component does not get instantiated and is ignored anyway */
  if (ep->interface->instantiation)
    ep->interface = ep->interface->instantiation;
  if (ep->function->instantiation)
    ep->function = ep->function->instantiation;
}

static void instantiate_cg(cgraph copy, cgraph original)
/* Effects: Copies the original graph into copy, with endpoints based
     on the instantiations specified in the function and interface ddecls
*/
{
  ggraph orig_g = cgraph_graph(original);
  gnode n;
  gedge connection;

  /* Add all edges from original to copy, but with updated
     ddecls */
  graph_scan_nodes (n, orig_g)
    {
      struct endp from = *NODE_GET(endp, n);
      gnode cfrom;

      instantiate_endp(&from);
      cfrom = endpoint_lookup(copy, &from);

      graph_scan_out (connection, n)
	{
	  struct endp to = *NODE_GET(endp, graph_edge_to(connection));
	  gnode cto;

	  instantiate_endp(&to);
	  cto = endpoint_lookup(copy, &to);

	  graph_add_edge(cfrom, cto, NULL);
	}
    }
}

void instantiate_configuration(nesc_declaration component, configuration conf)
{
  /* Make a new implementation for the abstract configuration */
  component_ref comp, newcomp = NULL;
  region r = parse_region; /* XXX: mem */
  configuration newconf;

  scan_component_ref (comp, conf->components)
    {
      nesc_declaration compdecl = comp->cdecl;
      component_ref nextcomp =
	new_component_ref(r, dummy_location, NULL, NULL, FALSE, NULL);

      /* We only need a correct component decl in our component_ref */
      if (compdecl->abstract)
	compdecl = specification_copy(r, compdecl, FALSE);
      nextcomp->cdecl = compdecl;
      newcomp = component_ref_chain(nextcomp, newcomp);
    }

  newconf = new_configuration(r, dummy_location, NULL,
			      component_ref_reverse(newcomp), NULL);
  AST_set_parents(CAST(node, newconf));
  component->impl = CAST(implementation, newconf);

  instantiate_cg(component->connections, component->original->connections);
}

void instantiate(nesc_declaration component)
/* Effects: Actually instantiate an abstract component
     For modules: temp noop
     For configurations: make new shallow copies of included abstract
       components, and copy connection graph (using the new shallow
       copies) 
*/
{
  assert(component->kind == l_component && component->original);

  if (is_module(component->impl))
    instantiate_module(component, CAST(module, component->impl));
  else
    instantiate_configuration(component,
			      CAST(configuration, component->impl));
}

#if 0
/* Recursively evaluate the given expression, setting expr->cst to the
 * appropriate constant value.
 */
static void eval_const_expr(declaration parent_aparms, expression expr) {
  fprintf(stderr, "MDW: eval_const_expr expr 0x%lx kind %d %s\n",
      (unsigned long)expr, expr->kind, expr->cst?"CONST":"");

  // Note that 'expr' is shared across all instances of is associated
  // component_ref. Rather than copy 'component_ref->args' for each
  // instance, we just clear out any initializers and overwrite for
  // each instance of 'expr' processed. 
  if (is_lexical_cst(expr)) return;
  expr->cst = NULL; 

  if (is_binary(expr)) {
    binary bin = CAST(binary, expr);
    eval_const_expr(parent_aparms, bin->arg1);
    eval_const_expr(parent_aparms, bin->arg2);
    bin->cst = fold_binary(bin->type, CAST(expression, bin));

  } else if (is_identifier(expr)) {
    /* Only allowed identifiers are in parent_aparms or global */
    identifier id = CAST(identifier, expr);
    declaration d;
    variable_decl found_vd = NULL;
    scan_declaration(d, parent_aparms) {
      data_decl dd; 
      variable_decl vd;
      assert(d->kind == kind_data_decl);
      dd = CAST(data_decl, d);
      assert(dd->decls->kind == kind_variable_decl);
      vd = CAST(variable_decl, dd->decls);
      if (!strcmp(vd->ddecl->name, id->cstring.data)) {
	found_vd = vd;
	break;
      }
    }

    if (found_vd) {
      fprintf(stderr,"MDW: eval_const_expr: identifier assigned from vd 0x%lx ('%s')\n", (unsigned long)found_vd, found_vd->ddecl->name);
      expr->cst = found_vd->arg1->cst; 

    } else {
      // Look in global level
      data_declaration ddecl = lookup_global_id(id->cstring.data);
      if (ddecl == NULL) {
	error("cannot find `%s' in abstract parameters");
	return;
      }
      if (!ddecl->value) {
	error("cannot use non-constant variable `%s' in abstract initializer");
	return;
      }
      expr->cst = ddecl->value;
    }

  } else {
    error_with_location(expr->location, "XXX MDW XXX: eval_const_expr: Cannot handle expr kind %d\n", expr->kind);
    return;
  }

  if (!expr->cst) {
    error("cannot resolve abstract parameter initialization to constant value");
  }
}

static void set_parameter_values(nesc_declaration cdecl, expression args)
{
  data_decl parm;

  /* We know args is the same length as parameters (earlier error if not) */
  scan_data_decl (parm, CAST(data_decl, cdecl->parameters))
    {
      variable_decl vd = CAST(variable_decl, parm->decls);

      vd->ddecl->instantiation->value = args->cst;
      if (!args->cst)
	error("arguments to component not constant");
    }
}

void fold_constants(region r, nesc_declaration cdecl, expression args)
{
  if (cdecl->folded)
    return;

  set_parameter_values(cdecl, args);

  if (is_module(cdecl->impl))
    ;
  else
    {
      component_ref comp;
      configuration c = CAST(configuration, cdecl->impl);

      scan_component_ref (comp, c->components)
	{
	  fold_constants(r, comp->cdecl, comp->args);
	}
    }
}
#endif

void init_abstract(void)
{
  init_clone();
}
