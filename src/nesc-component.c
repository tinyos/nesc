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
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-interface.h"
#include "nesc-configuration.h"
#include "nesc-module.h"
#include "nesc-decls.h"
#include "nesc-paths.h"
#include "nesc-env.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "c-parse.h"
#include "input.h"
#include "edit.h"
#include "nesc-abstract.h"

void interface_scan(data_declaration iref, env_scanner *scan)
{
  env_scan(iref->functions->id_env, scan);
}

data_declaration interface_lookup(data_declaration iref, const char *name)
{
  return env_lookup(iref->functions->id_env, name, FALSE);
}

void component_spec_iterate(nesc_declaration c,
			    void (*iterator)(data_declaration fndecl,
					     void *data),
			    void *data,
			    bool interfaces)
{
  const char *ifname;
  void *ifentry;
  env_scanner scanifs;

  env_scan(c->env->id_env, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      if (!(idecl->kind == decl_interface_ref ||
	    idecl->kind == decl_function))
	continue;

      if (idecl->kind != decl_interface_ref || interfaces)
	iterator(idecl, data);

      if (idecl->kind == decl_interface_ref)
	{
	  env_scanner scanfns;
	  const char *fnname;
	  void *fnentry;

	  interface_scan(idecl, &scanfns);
	  while (env_next(&scanfns, &fnname, &fnentry))
	    iterator(fnentry, data);
	}
    }
}

void component_functions_iterate(nesc_declaration c,
				 void (*iterator)(data_declaration fndecl,
						  void *data),
				 void *data)
{
  component_spec_iterate(c, iterator, data, FALSE);
}

static typelist make_gparm_typelist(declaration gparms)
{
  declaration gparm;
  typelist gtypes = new_typelist(parse_region);

  scan_declaration (gparm, gparms)
    if (is_data_decl(gparm))
      {
	data_decl gd = CAST(data_decl, gparm);
	variable_decl gv = CAST(variable_decl, gd->decls);

	typelist_append(gtypes, gv->ddecl->type);
      }

  return gtypes;
}

void copy_interface_functions(region r, nesc_declaration container,
			      data_declaration iref, environment fns)
{
  environment icopy = new_environment(r, NULL, TRUE, FALSE);
  env_scanner scanif;
  const char *fnname;
  void *fnentry;

  env_scan(fns->id_env, &scanif);
  while (env_next(&scanif, &fnname, &fnentry))
    {
      data_declaration fndecl = fnentry, fncopy;

      fncopy = declare(icopy, fndecl, FALSE);
      fncopy->fn_uses = NULL;
      fncopy->nuses = NULL;
      fncopy->shadowed = fndecl;
      fncopy->container = container;
      fncopy->interface = iref;
      /* required events and provided commands are defined */
      fncopy->defined = (fncopy->ftype == function_command) ^ iref->required;
      if (iref->gparms) /* Push generic args onto fn type and decl */
	{
	  fncopy->gparms = iref->gparms;
	  fncopy->type = make_generic_type(fncopy->type, iref->gparms);
	}
    }

  iref->functions = icopy;
}

void declare_interface_ref(interface_ref iref, declaration gparms,
			   environment genv)
{
  const char *iname = (iref->word2 ? iref->word2 : iref->word1)->cstring.data;
  nesc_declaration idecl = 
    require(l_interface, iref->location, iref->word1->cstring.data);
  struct data_declaration tempdecl;
  data_declaration old_decl, ddecl;

  init_data_declaration(&tempdecl, CAST(declaration, iref), iname, void_type);
  tempdecl.kind = decl_interface_ref;
  tempdecl.type = NULL;
  tempdecl.itype = idecl;
  tempdecl.container = current.container;
  tempdecl.required = current.spec_section == spec_uses;
  tempdecl.gparms = gparms ? make_gparm_typelist(gparms) : NULL;

  old_decl = lookup_id(iname, TRUE);
  if (old_decl)
    error("redefinition of `%s'", iname);
  ddecl = declare(current.env, &tempdecl, FALSE);
  iref->ddecl = ddecl;

  if (idecl->abstract)
    {
      check_abstract_arguments("interface", ddecl,
			       idecl->parameters, iref->args);
      ddecl->itype = interface_copy(parse_region, iref,
				    current.container->abstract);
      ddecl->functions = ddecl->itype->env;
    }
  else
    copy_interface_functions(parse_region, current.container, ddecl,
			     ddecl->itype->env);

  /* We don't make the interface type generic. Instead, we push the generic
     type into each function in copy_interface_functions.  This is because
     the syntax for invoking or defining a function on a generic interface
     is interfacename.functionname[generic args](...) */
  ddecl->type = make_interface_type(ddecl);
}

void check_generic_parameter_type(location l, data_declaration gparm)
{
  if (!type_integral(gparm->type))
    {
      error_with_location(l, "integral type required for generic parameter `%s'",
			  gparm->name);
      gparm->type = int_type;
    }
}

struct beg_data
{
  cgraph cg;
};

void beg_iterator(data_declaration fndecl, void *data)
{
  struct beg_data *d = data;
  struct endp node;

  node.component = NULL;
  node.interface = fndecl->interface;
  node.function = fndecl;
  node.args_node = NULL;
  endpoint_lookup(d->cg, &node);
}

cgraph build_external_graph(region r, nesc_declaration cdecl)
{
  cgraph cg = new_cgraph(r);
  struct beg_data d;

  /* A very simple graph, with single unconnected nodes for each endpoint
     of cdecl */
  d.cg = cg;
  component_functions_iterate(cdecl, beg_iterator, &d);

  return cg;
}

void build_component(region r, nesc_declaration cdecl)
{
  component the_component = CAST(component, cdecl->ast);

  the_component->implementation->cdecl = cdecl;
  cdecl->impl = the_component->implementation;

  AST_set_parents(CAST(node, cdecl->ast));

  /* Build the default connection graph (just nodes for the external
     endpoints) */
  cdecl->connections = build_external_graph(r, cdecl);

  if (is_configuration(cdecl->impl))
    process_configuration(CAST(configuration, cdecl->impl));
  else
    process_module(CAST(module, cdecl->impl));
}

environment start_implementation(void)
{
  start_semantics(l_implementation, current.container, current.env);

  return current.env;
}
