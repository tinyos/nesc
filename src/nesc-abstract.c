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
#include "constants.h"
#include "nesc-constants.h"

static AST_walker clone_walker;

/* data_declaration in:
   oldidentifier_decl: ignored as illegal in modules
   string

   enumerator
   function_decl
   identifier
   interface_deref
   typename
   variable_decl
*/

/* tag_declaration in:
 */

/* field_declaration in:
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

void set_ddecl_instantiation1(data_declaration fndecl, void *data)
{
  data_declaration orig = fndecl;

  /* Here we make the copy of the fndecl created during parsing
     (the copy from the actual interface type) point to fndecl.
     Note that the last shadowed pointer points to the entries in
     the interface type */
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
  component_functions_iterate(component, set_ddecl_instantiation1, NULL);
}

void set_ddecl_instantiation2(data_declaration fndecl, void *data)
{
  /* We just make the decl fndecl is a copy of point back to fndecl */
  fndecl->shadowed->instantiation = fndecl;
}

void set_specification_instantiations_shallow(nesc_declaration component)
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
  component_functions_iterate(component, set_ddecl_instantiation2, NULL);
}

static declaration instantiate_parameters(region r, declaration orig_parms)
/* Effects: Makes a new list of declarations for an abstract componnent
*/
{
  /* A new dummy env for the instantiated parameters */
  current.env = new_environment(r, NULL, TRUE, FALSE);
  AST_walk_list(clone_walker, r, CASTPTR(node, &orig_parms));
  AST_set_parents(CAST(node, orig_parms));

  return CAST(declaration, orig_parms);
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

static AST_walker_result clone_component_ref(AST_walker spec, void *data,
					     component_ref *n)
{
  component_ref new = CAST(component_ref, AST_clone(data, CAST(node, *n)));

  *n = new;

  /* Instantiate any further abstract components inside this abstract
     configuration. */
  if (new->cdecl->abstract)
    {
      new->cdecl = specification_copy(data, new, FALSE);
      set_specification_instantiations_shallow(new->cdecl);
    }

  return aw_walk;
}

static AST_walker_result clone_configuration(AST_walker spec, void *data,
					     configuration *n)
{
  configuration new = CAST(configuration, AST_clone(data, CAST(node, *n)));
  nesc_declaration comp = current.container;

  *n = new;

  /* Copy the components, further instantiating any abstract ones */
  AST_walk_list(spec, data, CASTPTR(node, &new->components));

  /* We don't copy the connections, instead we copy the connection graph
     (note that comp->connections was initialised to an "empty" graph */
  instantiate_cg(comp->connections, original_component(comp)->connections);

  return aw_done;
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
  AST_walker_handle(clone_walker, kind_configuration, clone_configuration);
  AST_walker_handle(clone_walker, kind_component_ref, clone_component_ref);
}

void instantiate(nesc_declaration component)
/* Effects: Actually instantiate an abstract component
     For modules: temp noop
     For configurations: make new shallow copies of included abstract
       components, and copy connection graph (using the new shallow
       copies) 
*/
{
  region r = parse_region;

  assert(component->kind == l_component && component->original);
  current.container = component;

  /* We don't copy the component itself as we're handling the specification
     specially (not copied). So we just copy the parameters and the
     implementation. */

  component->parameters = instantiate_parameters(r, component->parameters);
  set_specification_instantiations(component);

  /* A new dummy env for all instantiations in the implementation */
  current.env = new_environment(r, NULL, TRUE, FALSE);
  AST_walk(clone_walker, r, CASTPTR(node, &component->impl));
  AST_set_parents(CAST(node, component->impl));
}

/* Component stack handling, for error message and loop detection */

struct instance_stack
{
  struct instance_stack *next;
  nesc_declaration component;
};

static struct instance_stack *stack, *avail;

static struct instance_stack *new_instance_stack(void)
/* Returns: a new, cleared, instance_stack
 */
{
  struct instance_stack *new;

  if (avail)
    {
      new = avail;
      avail = avail->next;
      new->next = NULL;
    }
  else
    new = ralloc(permanent, struct instance_stack);

  return new;
}

static void free_instance_stack(struct instance_stack *is)
{
  is->next = avail;
  is->component = NULL;
  avail = is;
}

void push_instance(nesc_declaration component)
/* Effects: push (concrete) component on the stack and set its full instance
     name.
*/
{
  struct instance_stack *new = new_instance_stack();

  assert(!component->abstract);
  if (component->original)
    {
      /* Instantiated component names is parent name (currently at the top
	 of the stack) . name-in-configuration (currently in instance_name) */
      const char *oldname = component->instance_name;
      const char *parentname = stack->component->instance_name;
      int namelen = strlen(parentname) + strlen(oldname) + 2;
      char *newname;

      newname = rstralloc(parse_region, namelen);
      sprintf(newname, "%s.%s", parentname, oldname);
      component->instance_name = newname;
    }

  new->next = stack;
  stack = new;
  new->component = component;

  current.container = component;
}

nesc_declaration abstract_recursion(void)
/* Returns:  If the instance stack indicates the programmer has
     created an instantiation loop, i.e., component Y (instance of
     abstract component X) has caused the instantiation of the top-most
     component (another instance of X).
     Return Y if this is the case, NULL if not.
*/
{
  struct instance_stack *i;
  nesc_declaration component = stack->component;

  /* The case where component is not an instance falls through
     naturally */
  component = original_component(component);

  for (i = stack->next; i; i = i->next)
    {
      /* If we hit a non-instance component there isn't a loop */
      if (!i->component->original)
	return NULL;

      if (original_component(i->component) == component)
	return i->component;
    }
  return NULL;
}

void pop_instance(void)
{
  struct instance_stack *top = stack;

  stack = stack->next;
  free_instance_stack(top);

  if (stack)
    current.container = stack->component;
  else
    current.container = NULL;
}

static void set_parameter_values(nesc_declaration cdecl, expression args)
{
  data_decl parm;

  /* We know args is the same length as parameters (earlier error if not) */
  scan_data_decl (parm, CAST(data_decl, cdecl->parameters))
    {
      variable_decl vd = CAST(variable_decl, parm->decls);

      vd->ddecl->value = args->cst;
      /* We can assume the type is arithmetic (for now at least)
	 (see declare_template_parameter) */
      if (!args->cst)
	error_with_location(args->location,
			    "arguments to component not constant");
      else if (type_integer(vd->ddecl->type))
	{
	  if (!constant_integral(args->cst))
	    error_with_location(args->location, "integer constant expected");
	  else if (!cval_inrange(args->cst->cval, vd->ddecl->type))
	    error_with_location(args->location, "constant out of range for argument type");
	}
      else if (type_floating(vd->ddecl->type))
	{
	  if (!constant_float(args->cst))
	    error_with_location(args->location, "floating-point constant expected");
	}
      else
	assert(0);
    }
}

void fold_components(region r, nesc_declaration cdecl)
{
  if (cdecl->folded)
    return;

  cdecl->folded = TRUE;

  current.container = cdecl;
  fold_constants_list(CAST(node, cdecl->impl));

  if (is_module(cdecl->impl))
    ;
  else
    {
      component_ref comp;
      configuration c = CAST(configuration, cdecl->impl);

      scan_component_ref (comp, c->components)
	{
	  current.container = cdecl;
	  set_parameter_values(comp->cdecl, comp->args);
	  fold_components(r, comp->cdecl);
	}
    }
}

void init_abstract(void)
{
  init_clone();
}
