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
#include "nesc-c.h"
#include "nesc-abstract.h"
#include "c-parse.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-configuration.h"
#include "AST_walk.h"
#include "semantics.h"
#include "constants.h"
#include "nesc-constants.h"
#include "c-lex.h"
#include "expr.h"
#include "nesc-env.h"
#include "init.h"
#include "attributes.h"
#include "nesc-attributes.h"
#include "unparse.h"

static AST_walker clone_walker;

/* data_declaration in:
   oldidentifier_decl: ignored as illegal in modules
   string

   enumerator
   function_decl
   identifier
   component_deref
   interface_deref
   typename
   variable_decl
   type_parm_decl
*/

/* tag_declaration in:
   tag_ref
   types
 */

/* field_declaration in:
   field_ref
   field_decl
   tag_declaration
 */

/* types in:
   (ignoring declared_type, currently unused)
   expression
   asttype
   field_declaration
   data_declaration
   (reptype in tag_declaration does not need instantiating)
 */

/* typelist in:
   data_declaration (gparms, can ignore oldstyle_args as not allowed in
                     modules)
*/

static void *clone(region r, void *vn)
{
  struct location l;
  node *n = vn;
  node new = AST_clone(r, *n);

  (*n)->instantiation = new;
  new->instantiation = NULL;
  *n = new;

  /* Update location to include the container (so we can print instance
     names in error messages) */
  l = *new->location;
  l.container = current.container;
  new->location = make_location(l);

  return new;
}

static void forward(data_declaration *dd)
{
  data_declaration ddecl = *dd;

  if (ddecl->instantiation)
    *dd = ddecl->instantiation;
}

static void instantiate_ddecl_types(data_declaration ddecl)
{
  /* type's in declarations come from the source code, so cannot be unknown */
  ddecl->type = instantiate_type(ddecl->type);
  if (ddecl->kind == decl_interface_ref && ddecl->gparms)
    ddecl->gparms = instantiate_typelist(ddecl->gparms);
}

static data_declaration hack_interface;
static int hack_required;

static void clone_ddecl(data_declaration ddecl)
{
  data_declaration copy;

  /* If already cloned, return. */
  if (ddecl->instantiation &&
      (!hack_interface || ddecl->instantiation->interface == hack_interface))
    {
      /* If the instantiation's context matches the current one, the
	 instantiation was already done. */
      if (ddecl->container &&
	  ddecl->instantiation->container == current.container)
	return;
      if (ddecl->container_function && 
	  ddecl->instantiation->container_function == current.function_decl->ddecl)
	return;
    }

  /* Copy module functions (incl. tasks) and variables */

  if (!(ddecl->kind == decl_variable || ddecl->kind == decl_function ||
	ddecl->kind == decl_constant || ddecl->kind == decl_typedef ||
	ddecl->kind == decl_interface_ref))
    return;

  /* Instantiate decls in modules */
  if (!(ddecl->container ||
	(ddecl->container_function && ddecl->container_function->container)))
    return;

  copy = declare(current.env, ddecl, TRUE);
  /* We don't have a proper environment, so the container and 
     container_function fields are bogus. Set them correctly. */
  if (ddecl->container) /* module level */
    {
      copy->container = current.container;
      copy->container_function = NULL;
    }
  else /* local */
    {
      copy->container = NULL;
      copy->container_function = current.function_decl->ddecl;
    }

  ddecl->instantiation = copy;
  copy->instantiation = NULL;
  copy->fn_uses = NULL;
  copy->nuses = NULL;
  copy->instanceof = ddecl;
  copy->interface = hack_interface;
  /* This hack_required thing is ugly. It's used when instantiating
     generic interfaces, to match the used/provides at the particular
     instantiation. */
  if (ddecl_is_command_or_event(copy) && hack_required)
    copy->defined = (copy->ftype == function_command) ^ (hack_required - 1);
  instantiate_ddecl_types(copy);
}

static void copy_fields(region r, tag_declaration copy, tag_declaration orig)
{
  field_declaration ofield, *nextfield;
  
  copy->fields = new_env(r, NULL);
  nextfield = &copy->fieldlist;

  for (ofield = orig->fieldlist; ofield; ofield = ofield->next)
    {
      field_declaration cfield = ralloc(r, struct field_declaration);

      *cfield = *ofield;
      ofield->instantiation = cfield;
      cfield->instantiation = NULL;
      if (cfield->name)
	env_add(copy->fields, cfield->name, cfield);
      *nextfield = cfield;
      nextfield = &cfield->next;
    }
}

static void forward_tdecl(region r, tag_ref tref)
{
  tag_declaration tdecl = tref->tdecl, copy;  

  /* Ignore non-module tags */
  if (!(tdecl->container ||
	(tdecl->container_function && tdecl->container_function->container)))
    return;

  /* If already cloned, use instance & return */
  if (tdecl->instantiation)
    {
      /* If the instantiation's context matches the current one, the
	 instantiation was already done. */
      tref->tdecl = tdecl->instantiation;
      if (tdecl->container &&
	  tdecl->instantiation->container == current.container)
	return;
      if (tdecl->container_function && 
	  tdecl->instantiation->container_function == current.function_decl->ddecl)
	return;
    }

  copy = declare_tag(tref);
  /* We don't have a proper environment, so the container and 
     container_function fields are bogus. Set them correctly. */
  if (tdecl->container) /* module level */
    {
      copy->container = current.container;
      copy->container_function = NULL;
    }
  else /* local */
    {
      copy->container = NULL;
      copy->container_function = current.function_decl->ddecl;
    }


  tref->tdecl = copy;
  tdecl->instantiation = copy;

  copy->reptype = tdecl->reptype;
  if (tdecl->defined)
    copy_fields(r, copy, tdecl);
  copy->instanceof = tdecl;
  copy->defined = tdecl->defined;
  copy->fields_const = tdecl->fields_const;
  copy->fields_volatile = tdecl->fields_volatile;
  copy->transparent_union = tdecl->transparent_union;
  copy->collapsed = tdecl->collapsed;
  copy->container = current.container;
}

static ivalue instantiate_ivalue(region r, ivalue value);

static void instantiate_ivalue_array(region r, ivalue copy, ivalue value)
{
  ivalue_array elem, *new_elems = &copy->u.array;

  for (elem = value->u.array; elem; elem = elem->next)
    {
      ivalue_array copy_elem = ralloc(r, struct ivalue_array);

      *new_elems = copy_elem;
      new_elems = &copy_elem->next;

      copy_elem->from = elem->from;
      copy_elem->to = elem->to;
      copy_elem->value = instantiate_ivalue(r, elem->value);
    }
}

static void instantiate_ivalue_structured(region r, ivalue copy, ivalue value)
{
  ivalue_field field, *new_fields = &copy->u.structured;

  for (field = value->u.structured; field; field = field->next)
    {
      ivalue_field copy_field = ralloc(r, struct ivalue_field);

      *new_fields = copy_field;
      new_fields = &copy_field->next;

      if (field->field->instantiation)
	copy_field->field = field->field->instantiation;
      else
	copy_field->field = field->field;
      copy_field->value = instantiate_ivalue(r, field->value);
    }
}

static ivalue instantiate_ivalue(region r, ivalue value)
{
  ivalue copy;

  // If already instantiated on this pass, return instantiation
  if (value->instantiation)
    return value->instantiation;

  /* type's in ivalues come from the source code, so cannot be unknown */
  copy = new_ivalue(r, value->kind, instantiate_type(value->type));
  value->instantiation = copy;
  switch (value->kind)
    {
    case iv_base:
      copy->u.base.require_constant_value = value->u.base.require_constant_value;
      break;
    case iv_array: instantiate_ivalue_array(r, copy, value); break;
    case iv_structured: instantiate_ivalue_structured(r, copy, value); break;
    default: assert(0); 
    }
  return copy;
}

static void clear_ivalue_instantiations(ivalue value);

static void clear_ivalue_array(ivalue value)
{
  ivalue_array elem;

  for (elem = value->u.array; elem; elem = elem->next)
    clear_ivalue_instantiations(elem->value);
}

static void clear_ivalue_structured(ivalue value)
{
  ivalue_field field;

  for (field = value->u.structured; field; field = field->next)
    clear_ivalue_instantiations(field->value);
}

static void clear_ivalue_instantiations(ivalue value)
{
  // We've cleared here and beneath if instantiation is already NULL.
  if (!value->instantiation)
    return;
  value->instantiation = NULL;

  switch (value->kind)
    {
    case iv_base: break;
    case iv_array: clear_ivalue_array(value); break;
    case iv_structured: clear_ivalue_structured(value); break;
    default: assert(0); 
    }
}

static type unary_type(unary e)
{
  switch (e->kind)
    {
    case kind_unary_plus:
    case kind_unary_minus:
    case kind_bitnot:
      return type_default_conversion(e->arg1->type);
    case kind_realpart: case kind_imagpart: {
      type etype = type_default_conversion(e->arg1->type);

      return type_complex(etype) ? make_base_type(etype) : etype;
    }
    default: /* ++, --, cast, address of, dereference, field ref, 
		component deref, not, sizeof expr, alignof expr */
      return instantiate_type(e->type);
    }
}

static type binary_type(binary e)
{
  switch(e->kind)
    {
    case kind_plus: case kind_minus: case kind_times: case kind_divide:
    case kind_modulo: case kind_bitand: case kind_bitor: case kind_bitxor:
    case kind_lshift: case kind_rshift:
    case kind_plus_assign: case kind_minus_assign: case kind_times_assign:
    case kind_divide_assign: case kind_modulo_assign: case kind_bitand_assign:
    case kind_bitor_assign: case kind_bitxor_assign: case kind_lshift_assign:
    case kind_rshift_assign: {
      type t1 = type_default_conversion(e->arg1->type);
      type t2 = type_default_conversion(e->arg2->type);

      /* Detect the various pointer arithmetic cases. These cannot lead to
	 an unknown type, and don't want to be passed to common type */
      if (type_pointer(t1) || type_pointer(t2))
	return instantiate_type(e->type);
      else
	return common_type(t1, t2);
    }
    case kind_leq: case kind_geq: case kind_lt: case kind_gt:
    case kind_eq: case kind_ne:
    case kind_andand: case kind_oror:
    case kind_array_ref: case kind_assign:
      return instantiate_type(e->type);

    default: assert(0); return NULL;
    }
}

static type conditional_type(conditional e)
{
  type rtype = NULL;
  type ttype = type_default_conversion(e->arg1->type);
  type ftype = type_default_conversion(e->arg2->type);

  if (type_equal(ttype, ftype))
    rtype = ttype;
  else if (type_equal_unqualified(ttype, ftype))
    rtype = make_qualified_type(ttype, no_qualifiers);
  else if (type_real(ttype) && type_real(ftype))
    /* This should probably be type_arithmetic. See complex3.c/C9X */
    rtype = common_type(ttype, ftype);
  else if (type_void(ttype) || type_void(ftype))
    rtype = void_type;
  else if (type_pointer(ttype) && type_pointer(ftype))
    {
      type tpointsto = type_points_to(ttype), fpointsto = type_points_to(ftype);

      if (type_compatible_unqualified(tpointsto, fpointsto))
	rtype = common_type(tpointsto, fpointsto);
      else if (definite_null(e->arg1) && type_void(tpointsto))
	rtype = fpointsto;
      else if (definite_null(e->arg2) && type_void(fpointsto))
	rtype = tpointsto;
      else if (type_void(tpointsto))
	rtype = tpointsto; /* void * result */
      else if (type_void(fpointsto))
	rtype = fpointsto; /* void * result */
      else
	/* Slight difference from GCC: I qualify the result type with
	   the appropriate qualifiers */
	rtype = void_type;

      /* Qualifiers depend on both types */
      rtype = make_pointer_type(qualify_type2(rtype, tpointsto, fpointsto));
    }
  else if (type_pointer(ttype) && type_integer(ftype))
    rtype = ttype;
  else if (type_pointer(ftype) && type_integer(ttype))
    rtype = ftype;
  else if (flag_cond_mismatch)
    rtype = void_type;
  else
    assert(0);
  
  /* Qualifiers depend on both types */
  return qualify_type2(rtype, ttype, ftype);
}

static type expression_type(expression e)
{
  switch (e->kind)
    {
    default:
      if (is_binary(e))
	return binary_type(CAST(binary, e));
      if (is_unary(e))
	return unary_type(CAST(unary, e));
      /* constants, label address, sizeof type, alignof type, identifier,
	 function call: these cannot be unknown type */
      return instantiate_type(e->type); 

    case kind_comma:
      return last_comma(CAST(comma, e))->type;

    case kind_conditional:
      return conditional_type(CAST(conditional, e));
      /* ick */
      break;
    }
}

static AST_walker_result clone_expression(AST_walker spec, void *data,
					  expression *n)
{
  expression new = clone(data, n);
  ivalue old_ivalue = NULL;

  if (new->ivalue)
    {
      ivalue copy = instantiate_ivalue(data, new->ivalue);

      old_ivalue = new->ivalue;
      new->ivalue = copy;
      if (copy->kind == iv_base)
	copy->u.base.expr = new;
    }

  AST_walk_children(spec, data, CAST(node, new));

  // clear instantiation field in ivalues to be ready for next
  // instantiation attempt
  if (old_ivalue)
    clear_ivalue_instantiations(old_ivalue);

  /* A few nodes (related to initialisation) don't have types */
  if (new->type)
    new->type = expression_type(new);

  return aw_done;
}

static AST_walker_result clone_stmt(AST_walker spec, void *data, statement *n)
{
  statement new = clone(data, n);

  /* Update containing_atomic and parent_loop */
  if (new->containing_atomic)
    new->containing_atomic = CAST(atomic_stmt, new->containing_atomic->instantiation);
  if (new->parent_loop)
    new->parent_loop = CAST(statement, new->parent_loop->instantiation);

  return aw_walk;
}

static AST_walker_result clone_asttype(AST_walker spec, void *data, asttype *n)
{
  asttype new = clone(data, n);

  /* type's in asttype come from the source code, so cannot be unknown */
  new->type = instantiate_type(new->type);

  return aw_walk;
}

static AST_walker_result clone_function_decl(AST_walker spec, void *data,
					     function_decl *n)
{
  declaration old = CAST(declaration, *n);
  function_decl new = clone(data, n);

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
      if (instance->ast == old)
	instance->ast = CAST(declaration, new);
      new->ddecl = instance;
    }

  current.function_decl = new;
  current.env->fdecl = new;
  AST_walk_children(spec, data, CAST(node, new));
  current.function_decl = NULL;
  current.env->fdecl = NULL;

  return aw_done;
}

static AST_walker_result clone_identifier(AST_walker spec, void *data,
					  identifier *n)
{
  clone_expression(spec, data, CASTPTR(expression, n));
  forward(&(*n)->ddecl);

  return aw_done;
}

static AST_walker_result clone_interface_deref(AST_walker spec, void *data,
					       interface_deref *n)
{
  clone_expression(spec, data, CASTPTR(expression, n));
  forward(&(*n)->ddecl);

  return aw_done;
}

static AST_walker_result clone_component_deref(AST_walker spec, void *data,
					       component_deref *n)
{
  component_deref new = clone(data, n);

  new->type = instantiate_type(new->type);
  forward(&new->ddecl);

  return aw_walk;
}

static AST_walker_result clone_variable_decl(AST_walker spec, void *data,
					     variable_decl *n)
{
  declaration old = CAST(declaration, *n);
  variable_decl new = clone(data, n);

  if (new->ddecl)
    {
      clone_ddecl(new->ddecl);

      if (new->ddecl->instantiation)
	{
	  data_declaration instance = new->ddecl->instantiation;

	  /* Forward the ddecl and update the ast and definition fields */
	  if (instance->definition == old)
	    instance->definition = CAST(declaration, new);
	  if (instance->ast == old)
	    instance->ast = CAST(declaration, new);
	  new->ddecl = instance;
	}
    }

  return aw_walk;
}

static AST_walker_result clone_type_parm_decl(AST_walker spec, void *data,
					     type_parm_decl *n)
{
  type_parm_decl new = clone(data, n);
  data_declaration instance;

  clone_ddecl(new->ddecl);
  instance = new->ddecl->instantiation;
  instance->definition = CAST(declaration, new);
  instance->ast = CAST(declaration, new);
  new->ddecl = instance;

  return aw_walk;
}

static AST_walker_result clone_typename(AST_walker spec, void *data,
					typename *n)
{
  typename new = clone(data, n);

  forward(&new->ddecl);

  return aw_walk;
}

static AST_walker_result clone_enumerator(AST_walker spec, void *data,
					  enumerator *n)
{
  enumerator new = clone(data, n);

  clone_ddecl(new->ddecl);

  if (new->ddecl->instantiation)
    {
      data_declaration instance = new->ddecl->instantiation;

      /* Forward the ddecl and update the ast and definition fields */
      instance->definition = CAST(declaration, new);
      instance->ast = CAST(declaration, new);
      new->ddecl = instance;
    }

  return aw_walk;
}

static AST_walker_result clone_tag_ref(AST_walker spec, void *data,
				       tag_ref *n)
{
  tag_ref new = clone(data, n);

  forward_tdecl(data, new);
  if (new->defined)
    new->tdecl->definition = new;
  AST_walk_children(spec, data, CAST(node, new));

  return aw_done;
}

static AST_walker_result clone_field_decl(AST_walker spec, void *data,
				       field_decl *n)
{
  field_decl new = clone(data, n);

  AST_walk_children(spec, data, CAST(node, new));
  new->fdecl = new->fdecl->instantiation;
  new->fdecl->type = instantiate_type(new->fdecl->type);
  new->fdecl->ast = new;

  return aw_done;
}

static AST_walker_result clone_field_ref(AST_walker spec, void *data,
					 field_ref *n)
{
  field_ref new;

  clone_expression(spec, data, CASTPTR(expression, n));
  new = *n;
  if (new->fdecl->instantiation)
    new->fdecl = new->fdecl->instantiation;

  return aw_done;
}

static void set_ddecl_instantiation1(data_declaration fndecl, void *data)
{
  data_declaration orig;

  instantiate_ddecl_types(fndecl);

  /* Here we make the copy of the fndecl created during parsing
     (the copy from the actual interface type) point to fndecl.
     We may have to go two deep for abstract modules in abstract
     configurations (but don't get fooled by generic interfaces) */
  orig = fndecl->instanceof;
  if (orig->instanceof && orig->instanceof->container->kind == l_component)
    orig = orig->instanceof;

  orig->instantiation = fndecl;
}

static void set_env_instantiations(environment env)
{
  const char *name;
  void *entry;
  env_scanner scan;

  env_scan(env->id_env, &scan);
  while (env_next(&scan, &name, &entry))
    set_ddecl_instantiation1(entry, NULL);
}

static void set_specification_instantiations(nesc_declaration component)
/* Effects: Set the instantiation pointers in the data_declarations of
     the original abstract component from which component is derived to
     the copies in component (in preparation for cloning component's
     AST and pointing to component's decls)

     Also instantiate the types in the copies

     The original data_declarations can be found by following the
     instanceof fields. We may have to follow these one deep (abstract
     modules in configurations) or two deep (abstract modules in
     abstract configurations)...
*/
{
  component_spec_iterate(component, set_ddecl_instantiation1, NULL, TRUE, TRUE);
}

static void set_ddecl_instantiation2(data_declaration fndecl, void *data)
{
  /* We just make the decl fndecl is a copy of point back to fndecl */

  fndecl->instanceof->instantiation = fndecl;
}

static void set_specification_instantiations_shallow(nesc_declaration component)
/* Effects: Set the instantiation pointers in the data_declarations of
     the original abstract component from which component is derived to
     the copies in component (in preparation for cloning component's
     AST and pointing to component's decls)
*/
{
  component_spec_iterate(component, set_ddecl_instantiation2, NULL, TRUE, TRUE);
}

static declaration instantiate_parameters(region r, declaration orig_parms)
/* Effects: Makes a new list of declarations for an abstract componnent
*/
{
  AST_walk_list(clone_walker, r, CASTPTR(node, &orig_parms));
  AST_set_parents(CAST(node, orig_parms));

  return CAST(declaration, orig_parms);
}

static void instantiate_endp(endp ep)
/* Effects: Modifies ep based on instantiated ddecls
 */
{
  /* The component does not get instantiated and is ignored anyway */
  if (ep->interface && ep->interface->instantiation)
    ep->interface = ep->interface->instantiation;
  if (ep->function && ep->function->instantiation)
    ep->function = ep->function->instantiation;
  if (ep->args_node)
    ep->args_node = CAST(expression, ep->args_node->instantiation);
}

static void instantiate_cg(cgraph copy, cgraph original)
/* Effects: Copies the original graph into copy, with endpoints based
     on the instantiations specified in the function and interface ddecls
*/
{
  ggraph orig_g = cgraph_graph(original);
  gnode n;
  gedge connection;

  /* Add all edges from original to copy, but with updated ddecls */
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

	  /* User graphs have locations on the edges */
	  graph_add_edge(cfrom, cto, EDGE_GET(location, connection));
	}
    }
}

static AST_walker_result clone_component_ref(AST_walker spec, void *data,
					     component_ref *n)
{
  component_ref new = clone(data, n);

  /* Instantiate any further abstract components inside this abstract
     configuration. */
  if (new->cdecl->abstract)
    {
      new->cdecl = specification_copy(data, new, FALSE);
      set_specification_instantiations_shallow(new->cdecl);
    }

  return aw_walk;
}

static AST_walker_result clone_interface_ref(AST_walker spec, void *data,
					     interface_ref *n)
{
  interface_ref new = clone(data, n);

  AST_walk_children(spec, data, CAST(node, new));

  clone_ddecl(new->ddecl);
  new->ddecl = new->ddecl->instantiation;

  if (new->ddecl->itype->abstract)
    {
      new->ddecl->itype = interface_copy(data, new, current.container->abstract);
      new->ddecl->functions = new->ddecl->itype->env;
    }
  else
    copy_interface_functions(data, current.container, 
			     new->ddecl, new->ddecl->functions);

  return aw_done;
}

static AST_walker_result clone_implementation(AST_walker spec, void *data,
					      implementation *n)
{
  implementation new = clone(data, n);
  nesc_declaration comp = current.container, orig = original_component(comp);

  /* Copy the components and connections (configurations) or the
     declarations (modules) */
  AST_walk_children(spec, data, CAST(node, new));

  /* Copy the local_statics list for nido by following the instantiation
     links in the original list */
  if (orig->local_statics)
    {
      dd_list_pos scan;
      region r = regionof(comp->local_statics);

      dd_scan (scan, orig->local_statics)
	{
	  data_declaration localsd = DD_GET(data_declaration, scan);

	  dd_add_last(r, comp->local_statics, localsd->instantiation);
	}
    }

  /* Copy the connection graph
     (note that comp->connections was initialised to an "empty" graph */
  instantiate_cg(comp->connections, orig->connections);
  instantiate_cg(comp->user_connections, orig->user_connections);

  return aw_done;
}

static AST_walker_result clone_ast(AST_walker spec, void *data, node *n)
{
  clone(data, n);
  return aw_walk;
}

static void init_clone(void)
{
  clone_walker = new_AST_walker(permanent);
  AST_walker_handle(clone_walker, kind_node, clone_ast);

  AST_walker_handle(clone_walker, kind_expression, clone_expression);
  AST_walker_handle(clone_walker, kind_field_ref, clone_field_ref);
  AST_walker_handle(clone_walker, kind_identifier, clone_identifier);
  AST_walker_handle(clone_walker, kind_interface_deref, clone_interface_deref);
  AST_walker_handle(clone_walker, kind_component_deref, clone_component_deref);

  AST_walker_handle(clone_walker, kind_statement, clone_stmt);

  AST_walker_handle(clone_walker, kind_asttype, clone_asttype);
  AST_walker_handle(clone_walker, kind_function_decl, clone_function_decl);
  AST_walker_handle(clone_walker, kind_variable_decl, clone_variable_decl);
  AST_walker_handle(clone_walker, kind_type_parm_decl, clone_type_parm_decl);
  AST_walker_handle(clone_walker, kind_typename, clone_typename);
  AST_walker_handle(clone_walker, kind_enumerator, clone_enumerator);
  AST_walker_handle(clone_walker, kind_implementation, clone_implementation);
  AST_walker_handle(clone_walker, kind_component_ref, clone_component_ref);
  AST_walker_handle(clone_walker, kind_interface_ref, clone_interface_ref);
  AST_walker_handle(clone_walker, kind_tag_ref, clone_tag_ref);
  AST_walker_handle(clone_walker, kind_field_decl, clone_field_decl);
}

void set_parameter_values(nesc_declaration cdecl, expression args)
{
  declaration parm;

  /* We know args is the same length as parameters (earlier error if not) */
  scan_declaration (parm, cdecl->parameters)
    {
      if (is_data_decl(parm))
	{
	  variable_decl vd = CAST(variable_decl, CAST(data_decl, parm)->decls);
	  cst_kind k = type_real(vd->ddecl->type) ?
	    cst_numerical : cst_address;

	  if (!args)
	    {
	      vd->ddecl->value = make_unknown_cst(k == cst_numerical ?
						  cval_unknown_number :
						  cval_unknown_address,
						  vd->ddecl->type);
	      continue;
	    }

	  if (!is_type_argument(args) && check_constant_once(args, k))
	    {
	      location l = args->location;

	      /* We can assume the type is arithmetic (for now at least)
		 (see declare_template_parameter) */
	      if (!args->cst)
		{
		  /* avoid duplicate error messages */
		  if (args->type != error_type)
		    error_with_location(l, "component arguments must be constants");
		}
	      else if (type_integer(vd->ddecl->type))
		{
		  if (!constant_integral(args->cst))
		    error_with_location(l, "integer constant expected");
		  else if (!cval_inrange(args->cst->cval, vd->ddecl->type))
		    error_with_location(l, "constant out of range for argument type");
		}
	      else if (type_floating(vd->ddecl->type))
		{
		  if (!constant_float(args->cst))
		    error_with_location(l, "floating-point constant expected");
		}
	      else if (type_charstar(vd->ddecl->type))
		{
		  /* Check that it's an actual string */
		  data_declaration sym;
		  bool ok = FALSE;

		  if (constant_address(args->cst))
		    {
		      sym = cval_ddecl(args->cst->cval);
		      /* We don't want any offset to the string either
			 (could lift this restriction) */
		      ok = sym && sym->kind == decl_magic_string &&
			cval_knownbool(args->cst->cval);
		    }
		  if (!ok)
		    error_with_location(l, "string argument expected");
		}
	    }

	  vd->ddecl->value = args->cst;
	}
      else /* type */
	{
	  type_parm_decl td = CAST(type_parm_decl, parm);

	  if (!args)
	    {
	      td->ddecl->type = error_type;
	      continue;
	    }

	  td->ddecl->type = args->type;
	  td->ddecl->initialiser = args;
	}

      args = CAST(expression, args->next);
    }
}

node instantiate_ast_list(region r, node n)
{
  AST_walk_list(clone_walker, r, &n);
  AST_set_parents(n);

  return n;
}

node instantiate_ast(region r, node n)
{
  AST_walk(clone_walker, r, &n);
  AST_set_parents(n);

  return n;
}

dd_list instantiate_dd_list(region r, dd_list l)
{
  dd_list copy;
  dd_list_pos scan;

  if (!l)
    return NULL;

  copy = dd_new_list(r);
  dd_scan (scan, l)
    dd_add_last(r, copy, instantiate_ast(r, DD_GET(node, scan)));

  return copy;
}

void instantiate(nesc_declaration component, expression arglist)
/* Effects: Actually instantiate an abstract component
     For modules: copy module's AST, declarations, etc
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

  set_env_instantiations(component->parameter_env);
  set_specification_instantiations(component);

  /* A new dummy env for all instantiations in the implementation */
  current.env = new_environment(r, NULL, TRUE, FALSE);
  component->impl = CAST(implementation,
    instantiate_ast_list(r, CAST(node, original_component(component)->impl)));
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

static void check_cg(cgraph connections)
/* Effects: Checks constants used in the connections graph
 */
{
  ggraph g = cgraph_graph(connections);
  gnode n;

  graph_scan_nodes (n, g)
    {
      endp ep = NODE_GET(endp, n);

      if (ep->args_node)
	check_generic_arguments(ep->args_node, endpoint_args(ep));
    }
}

static bool fold_components(nesc_declaration cdecl, int pass)
{
  bool done;
  declaration spec;
  dd_list_pos attr;

  if (cdecl->folded == pass)
    return TRUE;
  cdecl->folded = pass;

  spec = CAST(component, cdecl->ast)->decls;
  done = fold_constants_list(CAST(node, spec), pass);
  if (cdecl->attributes)
    dd_scan (attr, cdecl->attributes)
      done = fold_constants_list(DD_GET(node, attr), pass) && done;
  done = fold_constants_list(CAST(node, cdecl->impl), pass) && done;

  if (cdecl->configuration)
    {
      declaration d;
      configuration c = CAST(configuration, cdecl->impl);

      check_cg(cdecl->connections);

      scan_declaration (d, c->decls)
	if (is_component_ref(d))
	  {
	    component_ref comp = CAST(component_ref, d);

	    set_parameter_values(comp->cdecl, comp->args);
	    done = fold_components(comp->cdecl, pass) && done;
	  }
    }
  return done;
}

void fold_program(nesc_declaration program, nesc_declaration scheduler)
{
  int pass = 1;
  bool done;

  do
    {
      done = fold_constants_list(CAST(node, all_cdecls), pass);
      if (program)
	done = fold_components(program, pass) && done;
      if (scheduler)
	done = fold_components(scheduler, pass) && done;
      pass++;
    }
  while (!done);

  current.container = NULL;
}

void check_abstract_arguments(const char *kind, data_declaration ddecl,
			      declaration parms, expression arglist)
{
  location loc = ddecl->ast->location;
  int parmnum = 1;

  while (parms && arglist)
    {
      const char *errmsg = NULL;

      if (arglist->type == error_type)
	;
      else if (is_data_decl(parms))
	{
	  variable_decl vparm = CAST(variable_decl, CAST(data_decl, parms)->decls);
	  type parmtype = vparm->ddecl->type;

	  if (type_incomplete(parmtype))
	    errmsg = "type of formal parameter %d is incomplete";
	  else if (is_type_argument(arglist))
	    errmsg = "formal parameter %d must be a value";
	  else 
	    {
	      set_error_location(arglist->location);
	      check_assignment(parmtype, arglist->type, arglist, NULL,
			       ddecl, parmnum);
	    }
	}
      else /* type argument */
	{
	  type_parm_decl tparm = CAST(type_parm_decl, parms);

	  if (!is_type_argument(arglist))
	    errmsg = "formal parameter %d must be a type";
	  else switch (tparm->ddecl->typevar_kind)
	    {
	    case typevar_normal: 
	      /* These tests ensure the type can be used in assignments 
	         and as a function argument. */
	      if (type_array(arglist->type))
		errmsg = "type parameter cannot be an array type (parameter %d)";
	      else if (type_function(arglist->type))
		errmsg = "type parameter cannot be a function type (parameter %d)";
	      else if (type_incomplete(arglist->type))
		errmsg = "type parameter %d is an incomplete type";
	      break;
	    case typevar_integer:
	      if (!type_integer(arglist->type))
		errmsg = "parameter %d must be an integer type";
	      break;
	    case typevar_number:
	      if (!type_real(arglist->type))
		errmsg = "parameter %d must be a numerical type";
	      break;
	    default: assert(0); break;
	    }
	}
      if (errmsg)
	error_with_location(loc, errmsg, parmnum);
      parmnum++;
      arglist = CAST(expression, arglist->next);
      parms = CAST(declaration, parms->next);
    }
  clear_error_location();

  if (parms)
    error_with_location(loc, "too few arguments to %s `%s'",
			kind, ddecl->name);
  else if (arglist)
    error_with_location(loc, "too many arguments to %s `%s'",
			kind, ddecl->name);
}

static nesc_declaration 
nesc_declaration_copy(region r, nesc_declaration old, expression args,
		      bool copy_is_abstract, data_declaration ddecl)
{
  nesc_declaration copy;

  copy = new_nesc_declaration(r, old->kind, old->name);
  copy->configuration = old->configuration;
  copy->doc = old->doc;
  copy->abstract = copy_is_abstract;
  copy->original = old;

  /* Copy the parameters into new env, make new top-level env */
  copy->parameter_env = current.env = new_environment(r, NULL, TRUE, FALSE);
  copy->env = new_environment(r, copy->parameter_env, TRUE, FALSE);
  hack_interface = ddecl;
  current.container = ddecl ? ddecl->container : copy;
  copy->parameters = instantiate_parameters(r, old->parameters);
  copy->arguments = args;
  set_parameter_values(copy, args);
  copy->attributes = instantiate_dd_list(r, old->attributes);

  current.env = copy->env;
  //current.container = copy;

  return copy;
}

nesc_declaration interface_copy(region r, interface_ref iref,
				bool copy_is_abstract)
/* Returns: A copy of abstract interface intf, instantiated with arguments
     in arglist.
*/
{
  nesc_declaration intf = iref->ddecl->itype, copy;
  struct semantic_state old = current;

  assert(intf->kind == l_interface);

  copy = nesc_declaration_copy(r, intf, iref->args, copy_is_abstract,
			       iref->ddecl);
  hack_required = 1 + iref->ddecl->required;
  copy->ast = CAST(nesc_decl, instantiate_ast_list(r, CAST(node, intf->ast)));
  hack_required = 0;
  hack_interface = NULL;
  current = old;
  
  return copy;
}

nesc_declaration specification_copy(region r, component_ref cref,
				    bool copy_is_abstract)
/* Returns: A copy of the parameters and specification of the
     component specified by cref, with arguments specified by cref
*/
{
  component spec;
  nesc_declaration comp = cref->cdecl, copy;
  struct semantic_state old = current;

  assert(comp->kind == l_component);

  copy = nesc_declaration_copy(r, comp, cref->args, copy_is_abstract, NULL);
  copy->instance_name = (cref->word2 ? cref->word2 : cref->word1)->cstring.data;
  if (!copy_is_abstract)
    {
      /* Give it a new name */
      /* component may itself be a copy of the real original abstract
	 component */
      nesc_declaration abs_comp = comp->original ? comp->original : comp;
      char *newname = rstralloc(r, strlen(copy->name) + 20);

      copy->instance_number = abs_comp->instance_count++;
      sprintf(newname, "%s%s%d", copy->name, get_function_separator(),
	      copy->instance_number);
      copy->name = newname;
    }

  copy->ast = comp->ast;
  clone(r, &copy->ast);

  /* Copy the specification into the copy's env */
  spec = CAST(component, copy->ast);
  spec->decls = CAST(declaration,
		     instantiate_ast_list(r, CAST(node, spec->decls)));
  current = old;

  /* Give the copy an "empty" specification graph */
  build_external_graph(r, copy);

  return copy;
}

static void typevar_attr(nesc_attribute attr, data_declaration ddecl,
			 int kind)
{
  if (ddecl->typevar_kind != typevar_normal)
    ignored_nesc_attribute(attr);
  else
    ddecl->typevar_kind = kind;
}

static void handle_integer_decl(nesc_attribute attr, data_declaration ddecl)
{
  typevar_attr(attr, ddecl, typevar_integer);
}

static void handle_number_decl(nesc_attribute attr, data_declaration ddecl)
{
  typevar_attr(attr, ddecl, typevar_number);
}

void init_abstract(void)
{
  init_clone();
  define_internal_attribute("integer", NULL, handle_integer_decl, NULL, NULL,
			    NULL, NULL);
  define_internal_attribute("number", NULL, handle_number_decl, NULL, NULL,
			    NULL, NULL);
}
