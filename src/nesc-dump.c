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

/* The internal nesC dump information system.
   Current allowable requests:
     - components, interfaces, interfacedefs, tags:
       extract lists of the specified internal objects
       managed via the lists[] array (see below)
       support filter arguments (see nesc-dfiler.c)
     - referenced(<any of the lists above>):
       implicitly dump any referenced item of the specified kind, e.g.,
       referenced(interfacedefs) will dump all interface definitions referred
       to from other XML elements
     - wiring: dump wiring graph
       wiring(functions) dumps the function-level graph (but this is currently
       in a somewhat different form than the regular graph, so should probably
       not be used)
*/

#include "parser.h"
#include "nesc-dump.h"
#include "nesc-env.h"
#include "nesc-dspec.h"
#include "nesc-dfilter.h"
#include "nesc-xml.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "nesc-component.h"
#include "constants.h"

/* The current set of dump requests */
static dd_list/*nd_option*/ opts;
region dump_region; /* for dump request allocations */
static const char *dumpfile;

/* What to output */
enum { wiring_none, wiring_user, wiring_functions } wiring = wiring_none;

/* More OOish stuff to manage dump requests of concrete nesC internal
   objects (components, tags, etc). The lists array contains the currently
   supported list.

   Each of these concrete requests supports filters (and does not have any
   other options).
*/
static bool tdecl_addfilter(void *entry);
static bool ndecl_addfilter(void *entry);
static bool ddecl_addfilter(void *entry);

static void dump_component(void *entry);
static void dump_interface(void *entry);
static void dump_interfacedef(void *entry);
static void dump_tag(void *entry);

static void select_components(xml_list l, nd_option opt, dd_list comps);
static void select_interfaces(xml_list l, nd_option opt, dd_list comps);
static void select_interfacedefs(xml_list l, nd_option opt, dd_list comps);
static void select_tags(xml_list l, nd_option opt, dd_list comps);

/* lists */
static struct {
  const char *name;

  /* Return TRUE if entry has not yet been added to its dump list */
  bool (*addfilter)(void *entry);

  /* Add entries selected by 'opt' to list 'l'. 'comps' is the list of the
     program's components. */
  void (*select)(xml_list l, nd_option opt, dd_list comps);

  /* Dump a single entry in XML */
  void (*dump)(void *entry);

  /* The list of referenced items of, e.g., components is created by 
     adding entries to a global variable (xl_components in this case) holding
     an xml_list (see xml_list_add in nesc-xml.c). By default this variable
     is NULL, so nothing happens. The 'referenced' dump request sets this
     variable to point to the list l (see next field) to track, e.g.,
     referenced components. The 'referenced' field contains the address of
     this global variable. */
  xml_list *referenced;

  xml_list l; /* The list of entries of this kind */
} lists[] = {
  { "components", ndecl_addfilter, select_components, dump_component, &xl_components },
  { "interfaces", ddecl_addfilter, select_interfaces, dump_interface, &xl_interfaces },
  { "interfacedefs", ndecl_addfilter, select_interfacedefs, dump_interfacedef, &xl_interfacedefs },
  { "tags", tdecl_addfilter, select_tags, dump_tag, &xl_tags }
};

#define NLISTS (sizeof lists / sizeof *lists)

static bool tdecl_addfilter(void *entry)
{
  tag_declaration decl = entry;

  if (decl->dumped)
    return FALSE;
  decl->dumped = TRUE;
  return TRUE;
}

static bool ndecl_addfilter(void *entry)
{
  nesc_declaration decl = entry;

  if (decl->dumped)
    return FALSE;
  decl->dumped = TRUE;
  return TRUE;
}

static bool ddecl_addfilter(void *entry)
{
  data_declaration decl = entry;

  if (decl->dumped)
    return FALSE;
  decl->dumped = TRUE;
  return TRUE;
}

/* Actual XML dump functions. See doc/dump for the corresponding DSD schemas.
   The dump functions are found partially here (for high-level elements),
   in nesc-xml.c (low-level elements and basic functions) and types.c (for
   types). */

static void dump_attributes(dd_list/*nesc_attribute*/ attributes)
{
  dd_list_pos scan;

  if (!attributes)
    return;

  dd_scan (scan, attributes)
    {
      nesc_attribute attr = DD_GET(nesc_attribute, scan);

      indentedtag("attribute-value");
      nxml_tdecl_ref(attr->tdecl);
      nxml_value(attr->arg1->ivalue);
      indentedtag_pop();
    }
}

void dump_ddecl(data_declaration ddecl)
{
  switch (ddecl->kind)
    {
    case decl_variable: indentedtag_start("variable"); break;
    case decl_constant:
      indentedtag_start("constant");
      xml_attr_cval("cst", ddecl->value->cval);
      break;
    case decl_function:
      indentedtag_start("function");
      switch (ddecl->ftype)
	{
	case function_event: xml_attr_noval("event"); break;
	case function_command: xml_attr_noval("command"); break;
	default: break;
	}
      break;
    case decl_typedef: indentedtag_start("typedef"); break;
    case decl_interface_ref:
      indentedtag_start("interface");
      xml_attr_int("provided", !ddecl->required);
      break;
    case decl_component_ref: indentedtag_start("internal-component"); break;
    default: assert(0);
    }
  xml_attr("name", ddecl->name);
  xml_attr_ptr("ref", ddecl);
  xml_attr_loc((ddecl->definition ? ddecl->definition : ddecl->ast)->location);
  xml_tag_end();

  /* Symbols have either a nesC container, a containing function, containing
     interface or none of these (global symbols) */
  xstartline();
  if (ddecl->container)
    nxml_ndecl_ref(ddecl->container);
  if (ddecl->container_function)
    nxml_ddecl_ref(ddecl->container_function);

  nxml_type(ddecl->type);
  dump_attributes(ddecl->attributes);

  switch (ddecl->kind)
    {
    case decl_interface_ref: 
      {
	env_scanner fns;
	const char *fnname;
	void *fnentry;
	
	nxml_instance(ddecl->itype);
	if (ddecl->gparms)
	  nxml_typelist("interface-parameters", ddecl->gparms);

	indentedtag("interface-functions");
	interface_scan(ddecl, &fns);
	while (env_next(&fns, &fnname, &fnentry))
	  {
	    xstartline();
	    nxml_ddecl_ref(fnentry);
	  }
	indentedtag_pop();
	break;
      }
    case decl_function:
      if (ddecl->interface)
	nxml_ddecl_ref(ddecl->interface);
      break;
    default: break;
    }

  indentedtag_pop();
}

static void dump_parameter(declaration parm)
{
  data_declaration pdecl = NULL;

  /* (void) parameters have a NULL ddecl, so will be ignored */
  if (is_data_decl(parm)) /* regular parameter */
    {
      data_decl data = CAST(data_decl, parm);
      variable_decl vdecl = CAST(variable_decl, data->decls);


      pdecl = vdecl->ddecl;
    }
  if (is_ellipsis_decl(parm))
    {
      xml_qtag("varargs");
      xnewline();
    }
  if (pdecl)
    dump_ddecl(pdecl);
}

static void dump_parameters(const char *name, declaration parms)
{
  declaration parm;

  indentedtag(name);
  scan_declaration (parm, parms)
    dump_parameter(parm);
  indentedtag_pop();
}

static void dump_endp(const char *tag, endp ep)
{
  xstartline();
  xml_tag(tag);
  nxml_ddecl_ref(ep->function ? ep->function : ep->interface);
  if (ep->args_node)
    nxml_arguments(ep->args_node);
  xml_pop();
}

static void dump_wire(location l, gnode from, gnode to)
{
  endp fdata = NODE_GET(endp, from), tdata = NODE_GET(endp, to);

  indentedtag_start("wire");
  if (l)
    xml_attr_loc(l);
  xml_tag_end();
  dump_endp("from", fdata);
  dump_endp("to", tdata);
  indentedtag_pop();
}

static void dump_wiring(cgraph cg)
{
  gnode from;
  gedge wire;

  /* Print complete wiring graph */
  indentedtag("wiring");
  graph_scan_nodes (from, cgraph_graph(cg))
    {
      graph_scan_out (wire, from)
	dump_wire(EDGE_GET(location, wire), from, graph_edge_to(wire));
    }
  indentedtag_pop();
}

static void dump_component(void *entry)
{
  nesc_declaration comp = entry;

  /* We're not supposed to dump partially instantiated components
     (i.e., generic components created inside generic configurations) */
  assert(!(comp->original && comp->abstract));

  indentedtag_start("component");
  xml_attr("qname", comp->instance_name);
  xml_attr_loc(comp->ast->location);
  xml_tag_end();
  xnewline();

  if (comp->original)
    nxml_instance(comp);
  if (comp->abstract)
    dump_parameters("parameters", comp->parameters);
  xml_qtag(comp->configuration ? "configuration" : "module");
  dump_attributes(comp->attributes);
  indentedtag_pop();
}

static void dump_interface(void *entry)
{
  dump_ddecl(entry);
}

static void dump_interfacedef(void *entry)
{
  nesc_declaration idef = entry;
  env_scanner fns;
  const char *fnname;
  void *fnentry;

  indentedtag_start("interfacedef");
  xml_attr("qname", idef->name);
  xml_attr_loc(idef->ast->location);
  xml_tag_end();

  if (idef->abstract)
    dump_parameters("parameters", idef->parameters);
  dump_attributes(idef->attributes);

  env_scan(idef->env->id_env, &fns);
  while (env_next(&fns, &fnname, &fnentry))
    {
      data_declaration fndecl = fnentry;

      if (fndecl->kind != decl_magic_string)
	dump_ddecl(fnentry);
    }

  indentedtag_pop();
}

static void dump_field(field_declaration field)
{
  indentedtag_start("field");
  xml_attr("name", field->name);
  xml_attr_ptr("ref", field); /* collapsing structs into their parent can 
				 cause duplicate names */
  xml_attr_bool("packed", field->packed);
  xml_attr_cval("bit-offset", field->offset);
  if (cval_istop(field->bitwidth))
    xml_attr_cval("size", type_size_cc(field->type) ?
		  type_size(field->type) : cval_top);
  else
    xml_attr_cval("bit-size", field->bitwidth);
  xml_tag_end();
  nxml_type(field->type);
  dump_attributes(field->attributes);
  indentedtag_pop();
}

static void dump_tag(void *entry)
{
  tag_declaration tdecl = entry;

  indentedtag_start(tagkind_name(tdecl->kind));
  if (tdecl->name)
    xml_attr("name", tdecl->name);
  if (tdecl->definition)
    xml_attr_loc(tdecl->definition->location);
  xml_attr_ptr("ref", tdecl);
  xml_attr_bool("defined", tdecl->defined);
  xml_attr_bool("packed", tdecl->packed);
  xml_attr_bool("scoped", !!tdecl->container/* || tdecl->container_function*/);
  xml_attr_cval("size", tdecl->size);
  xml_attr_cval("alignment", tdecl->alignment);
  xml_tag_end();
  xnewline();

  if (tdecl->container)
    {
      nxml_ndecl_ref(tdecl->container);
      xnewline();
    }
#if 0
  if (tdecl->containing_function)
    {
      nxml_ddecl_ref(tdecl->containing_function);
      xnewline();
    }
#endif
  dump_attributes(tdecl->attributes);

  if (tdecl->kind == kind_enum_ref)
    nxml_type(tdecl->reptype);
  else
    {
      field_declaration fields;

      for (fields = tdecl->fieldlist; fields; fields = fields->next)
	if (fields->name) /* skip unnamed fields */
	  dump_field(fields);
    }

  indentedtag_pop();
}

static void dump_list(const char *name, xml_list l,
		      void (*dump)(void *entry))
{
  dd_list latest = xml_list_latest(l);
  dd_list_pos elem;

  if (!latest)
    return;

  indentedtag(name);
  dd_scan (elem, latest)
    dump(DD_GET(void *, elem));
  indentedtag_pop();
}

/* The toplevel requests supported by -fnesc-dump */
/* ---------------------------------------------- */
/* Most of these are handled via the lists system (see above) */

static void select_components(xml_list l, nd_option opt, dd_list comps)
{
  dd_list_pos scan_components;

  if (!comps)
    {
      error("components can only be requested on an actual program");
      return;
    }

  dd_scan (scan_components, comps)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      if (dump_filter_ndecl(comp))
	xml_list_add(l, comp);
    }
}

static void process_component_interfaces(xml_list l, nesc_declaration comp)
{
  env_scanner scan;
  const char *name;
  void *decl;

  env_scan(comp->env->id_env, &scan);
  while (env_next(&scan, &name, &decl))
    {
      data_declaration ddecl = decl;

      if (ddecl->kind == decl_interface_ref && dump_filter_ddecl(ddecl))
	xml_list_add(l, ddecl);
    }
}

static void select_interfaces(xml_list l, nd_option opt, dd_list comps)
{
  dd_list_pos scan_components;

  if (!comps)
    {
      error("interfaces can only be requested on an actual program");
      return;
    }

  dd_scan (scan_components, comps)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      process_component_interfaces(l, comp);
    }
}

static void add_defs(int kind, xml_list l)
{
  env_scanner scanenv;
  const char *name;
  void *val;

  env_scan(get_nesc_env(), &scanenv);
  while (env_next(&scanenv, &name, &val))
    {
      nesc_declaration ndecl = val;

      if (ndecl->kind == kind && dump_filter_ndecl(ndecl))
	xml_list_add(l, ndecl);
    }
}

static void select_interfacedefs(xml_list l, nd_option opt, dd_list comps)
{
  add_defs(l_interface, l);
}

static void select_tags(xml_list l, nd_option opt, dd_list comps)
{
  env_scanner scan;
  const char *name;
  void *decl;

  env_scan(global_env->tag_env, &scan);
  while (env_next(&scan, &name, &decl))
    {
      tag_declaration tdecl = decl;

      if (dump_filter_tdecl(tdecl))
	xml_list_add(l, tdecl);
    }
}

static void select_wiring(nd_option opt, dd_list comps)
{
  nd_arg arg;

  if (!comps)
    {
      error("wiring can only be requested on an actual program");
      return;
    }
  wiring = wiring_user;

  scan_nd_arg (arg, opt->args)
    if (is_nd_token(arg))
      {
	const char *req = nd_tokenval(arg);

	if (!strcmp(req, "functions"))
	  wiring = wiring_functions;
	else
	  error("unknown wiring request for `%s'", req);
      }
    else
      error("bad argument to wiring");
}

static void select_referenced(nd_option opt)
{
  nd_arg arg;

  scan_nd_arg (arg, opt->args)
    if (is_nd_token(arg))
      {
	const char *req = nd_tokenval(arg);
	int i;

	for (i = 0; i < NLISTS; i++)
	  if (!strcmp(req, lists[i].name))
	    {
	      *lists[i].referenced = lists[i].l;
	      break;
	    }
	if (i == NLISTS)
	  error("unknown referenced request for `%s'", req);
      }
    else
      error("bad argument to referenced");
}

void select_dump(char *what)
{
  nd_option opt;

  if (!dump_region)
    dump_region = permanent;

  opt = nd_parse(what);

  if (opt)
    {
#if 0
      nd_arg arg;
      int i = 0;

      fprintf(stderr, "opt %s, %d args\n", opt->name, opt->count);
      scan_nd_arg (arg, opt->args)
	if (is_nd_int(arg))
	  fprintf(stderr, "  arg %d int %ld\n", ++i,
		  (long)ND_CAST(nd_int, arg)->val);
	else
	  fprintf(stderr, "  arg %d token %s\n", ++i,
		  ND_CAST(nd_token, arg)->str);
#endif

      if (!opts)
	opts = dd_new_list(dump_region);
      dd_add_last(dump_region, opts, opt);
    }
}

void select_dumpfile(char *name)
{
  dumpfile = name;
}

bool dump_selected(void)
{
  return opts != NULL;
}

static void do_wiring(cgraph cg, cgraph userg)
{
  if (wiring == wiring_functions)
    dump_wiring(cg);
  if (wiring == wiring_user)
    dump_wiring(userg);
}

static void do_lists(void)
{
  int i;

  for (i = 0; i < NLISTS; i++)
    dump_list(lists[i].name, lists[i].l, lists[i].dump);
}

void dump_info(nesc_declaration program, cgraph cg, cgraph userg,
	       dd_list modules, dd_list comps)
{
  dd_list_pos scan_opts;
  bool list_change = FALSE;
  int i;
  FILE *dumpf = NULL;

  for (i = 0; i < NLISTS; i++)
    lists[i].l = new_xml_list(dump_region, &list_change, lists[i].addfilter);

  /* Process options to find out what is selected */
  dd_scan (scan_opts, opts)
    {
      nd_option opt = DD_GET(nd_option, scan_opts);
      int i;

      dump_set_filter(opt);

      for (i = 0; i < NLISTS; i++)
	if (!strcmp(opt->name, lists[i].name))
	  {
	    lists[i].select(lists[i].l, opt, comps);
	    break;
	  }

      if (i < NLISTS)
	;
      else if (!strcmp(opt->name, "wiring"))
	select_wiring(opt, comps);
      else if (!strcmp(opt->name, "referenced"))
	select_referenced(opt);
      else
	error("unknown dump request `%s'", opt->name);
    }

  /* Repeatedly dump selected information (w/o performing any actual I/O).
     This will collect all items selected by the 'referenced' request.
     This repeated collection of items is supported by the xml_list type
     (from nesc-xml.c) */
  xml_start_dummy();
  do_wiring(cg, userg);
  for (;;)
    {
      do_lists();
      if (!list_change)
	break;
      list_change = FALSE;
    }

  /* All information now collected. Reset the lists and dump the information
     for real. */
  for (i = 0; i < NLISTS; i++)
    xml_list_reset(lists[i].l);

  if (!dumpfile)
    xml_start(stdout);
  else
    {
      dumpf = fopen(dumpfile, "w");
      if (!dumpf)
	{
	  perror("couldn't create dump file");
	  return;
	}
      xml_start(dumpf);
    }
  indentedtag_start("nesc");
  xml_attr("xmlns", "http://www.tinyos.net/nesC");
  xml_tag_end(); xnewline();

  do_wiring(cg, userg);
  do_lists();

  indentedtag_pop();
  xml_end();

  if (dumpf)
    fclose(dumpf);

  /* Nothing should have been added to the lists in the actual output pass */
  assert(!list_change);
}
