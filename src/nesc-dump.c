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
#include "nesc-dump.h"
#include "nesc-env.h"
#include "nesc-dspec.h"
#include "nesc-dfilter.h"
#include "nesc-xml.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "nesc-semantics.h"

dd_list/*nd_option*/ opts;
region dump_region;

/* What to output */
enum { wiring_none, wiring_user, wiring_functions } wiring = wiring_none;

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
  bool (*addfilter)(void *entry);
  void (*select)(xml_list l, nd_option opt, dd_list comps);
  void (*dump)(void *entry);
  xml_list *referenced;
  xml_list l;
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

static void dump_parameter(declaration parm)
{
  if (is_type_parm_decl(parm))
    {
      data_declaration pdecl = CAST(type_parm_decl, parm)->ddecl;

      xml_tag_start("type-parameter");
      xml_attr_ptr("ref", pdecl);
      xml_tag_end_pop();
      xnewline();
    }
  else if (is_data_decl(parm)) /* regular parameter */
    {
      data_decl data = CAST(data_decl, parm);
      variable_decl vdecl = CAST(variable_decl, data->decls);

      if (vdecl->ddecl) /* ignore (void) parameter */
	nxml_type(vdecl->ddecl->type);
    }
  else
    {
      assert(is_ellipsis_decl(parm));
      xml_qtag("varargs");
      xnewline();
    }
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
  xml_tag(tag);
  nxml_ddecl_ref(ep->function ? ep->function : ep->interface);
  xml_pop();
  xnewline();
}

static void dump_wire(gnode from, gnode to)
{
  endp fdata = NODE_GET(endp, from), tdata = NODE_GET(endp, to);

  indentedtag("wire");
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
	dump_wire(from, graph_edge_to(wire));
    }
  indentedtag_pop();
}

static void dump_component(void *entry)
{
  nesc_declaration comp = entry;

  if (comp->configuration)
    indentedtag_start("configuration");
  else
    indentedtag_start("module");
  xml_attr("name", comp->instance_name);
  xml_tag_end();
  xnewline();

  if (comp->original && !comp->abstract)
    {
      indentedtag_start("instance");
      xml_attr_int("number", comp->instance_number);
      xml_tag_end();
      nxml_ndefinition_ref(comp);
      indentedtag_pop();
    }
  if (comp->abstract)
    dump_parameters("parameters", comp->parameters);
  indentedtag_pop();
}

static void dump_attributes(dd_list/*nesc_attribute*/ attributes)
{
  dd_list_pos scan;

  if (!attributes)
    return;

  dd_scan (scan, attributes)
    {
      nesc_attribute attr = DD_GET(nesc_attribute, scan);

      indentedtag_start("attribute");
      xml_attr("name", attr->word1->cstring.data);
      xml_tag_end();
      nxml_value(attr->arg1->ivalue);
      indentedtag_pop();
    }
}

static void nxml_typelist(const char *name, typelist types)
{
  typelist_scanner scantypes;
  type t;

  indentedtag(name);
  typelist_scan(types, &scantypes);
  while ((t = typelist_next(&scantypes)))
    nxml_type(t);
  indentedtag_pop();
}

static void dump_interface(void *entry)
{
  data_declaration iref = entry;

  assert(iref->kind == decl_interface_ref);

  indentedtag_start("interface");
  xml_attr("name", iref->name);
  xml_attr_ptr("ref", iref);
  xml_attr_int("provided", !iref->required);
  xml_tag_end();

  xstartline(); nxml_ninstance_ref(iref->container);
  nxml_ndefinition_ref(iref->itype);
  dump_attributes(iref->attributes);
  if (iref->gparms)
    nxml_typelist("parameters", iref->gparms);

  indentedtag_pop();
}

static void dump_interfacedef(void *entry)
{
  nesc_declaration comp = entry;

  indentedtag_start("interfacedef");
  xml_attr("name", comp->name);
  xml_tag_end();

  if (comp->abstract)
    dump_parameters("parameters", comp->parameters);

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
    {
      if (type_size_cc(field->type))
	xml_attr_cval("size", type_size(field->type));
    }
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
  xml_attr_ptr("ref", tdecl);
  xml_attr_bool("defined", tdecl->defined);
  xml_attr_bool("packed", tdecl->packed);
  xml_attr_cval("size", tdecl->size);
  xml_attr_cval("alignment", tdecl->alignment);
  xml_tag_end();
  xnewline();

  if (tdecl->container)
    {
      nxml_ninstance_ref(tdecl->container);
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
    {
      indentedtag("reptype");
      nxml_type(tdecl->reptype);
      indentedtag_pop();
    }
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

/* The toplevel requests supported -fnesc-dump */
/* ------------------------------------------- */

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

  for (i = 0; i < NLISTS; i++)
    lists[i].l = new_xml_list(dump_region, &list_change, lists[i].addfilter);

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

  xml_start_dummy();
  do_wiring(cg, userg);
  for (;;)
    {
      do_lists();
      if (!list_change)
	break;
      list_change = FALSE;
    }

  for (i = 0; i < NLISTS; i++)
    xml_list_reset(lists[i].l);

  xml_start(stdout);
  indentedtag("nesc");
  do_wiring(cg, userg);
  do_lists();
  indentedtag_pop();
  xml_end();

  assert(!list_change);
}
