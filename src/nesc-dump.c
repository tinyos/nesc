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
#include "nesc-dspec.h"
#include "nesc-dfilter.h"
#include "nesc-xml.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "nesc-semantics.h"

dd_list/*nd_option*/ opts;
region dump_region;

/* What to output */
bool wiring;
dhash_table components;
dhash_table componentdefs;
dhash_table interfaces;
dhash_table interfacedefs;
dhash_table tags;

/* What to implicitly output */
bool reachable_tags;

static void create_components(void)
{
  if (!components)
    components = new_dhash_ptr_table(dump_region, 256);
}

#if 0
static void create_componentdefs(void)
{
  if (!componentdefs)
    componentdefs = new_dhash_ptr_table(dump_region, 256);
}

static void create_interfacedefs(void)
{
  if (!interfacedefs)
    interfacedefs = new_dhash_ptr_table(dump_region, 256);
}
#endif

static void create_interfaces(void)
{
  if (!interfaces)
    interfaces = new_dhash_ptr_table(dump_region, 256);
}

static void create_tags(void)
{
  if (!tags)
    tags = new_dhash_ptr_table(dump_region, 256);
}

static dhash_table get_tags(void)
{
  if (reachable_tags)
    return tags;
  else
    return NULL;
}

static void simpletag_start(const char *name)
{
  xstartline();
  xml_tag_start(name);
  xindent();
}

static void simpletag(const char *name)
{
  xstartline();
  xml_tag(name);
  xindent();
  xnewline();
}

static void simpletag_pop(void)
{
  xstartline();
  xunindent();
  xml_pop();
  xnewline();
}

static void dump_wiring(cgraph cg)
{
  xml_tag("wiring");
  xml_pop();
  xnewline();
}

static void dump_component(void *entry)
{
  nesc_declaration comp = entry;

  if (comp->configuration)
    simpletag_start("configuration");
  else
    simpletag_start("module");
  xml_attr("name", comp->instance_name);
  if (comp->original && !comp->abstract)
    xml_attr_int("instance", comp->instance_number);
  xml_tag_end();

  nxml_ndefinition_ref(comp, get_tags());

  simpletag_pop();
}

static void dump_attributes(dd_list/*nesc_attribute*/ attributes)
{
  dd_list_pos scan;

  if (!attributes)
    return;

  dd_scan (scan, attributes)
    {
      nesc_attribute attr = DD_GET(nesc_attribute, scan);

      xstartline();
      xml_tag_start("attribute");
      xml_attr("name", attr->word1->cstring.data);
      xml_tag_end();
      /* Should have value here */
      xml_pop();
      xnewline();
    }
}

static void dump_typelist(const char *name, typelist types)
{
  typelist_scanner scantypes;
  type t;

  simpletag(name);
  typelist_scan(types, &scantypes);
  while ((t = typelist_next(&scantypes)))
    nxml_type(t, get_tags());
  simpletag_pop();
}

static void dump_interface(void *entry)
{
  data_declaration iref = entry;

  assert(iref->kind == decl_interface_ref);

  simpletag_start("interface");
  xml_attr("name", iref->name);
  xml_attr_int("provided", !iref->required);
  xml_tag_end();

  xstartline(); nxml_ninstance_ref(iref->container);
  nxml_ndefinition_ref(iref->itype, get_tags());
  dump_attributes(iref->attributes);
  if (iref->gparms)
    dump_typelist("parameters", iref->gparms);

  simpletag_pop();
}

static void dump_tag(void *entry)
{
  tag_declaration tdecl = entry;

  simpletag_start(tagkind_name(tdecl->kind));
  xml_attr("name", tdecl->name);
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
      simpletag("reptype");
      nxml_type(tdecl->reptype, get_tags());
      simpletag_pop();
    }
  else
    {
    }

  simpletag_pop();
}

static void dump_list(const char *name, dhash_table list,
		      void (*dump)(void *entry))
{
  dhash_scan scanner = dhscan(list);
  void *entry;

  simpletag(name);
  while ((entry = dhnext(&scanner)))
    dump(entry);
  simpletag_pop();
}

/* The toplevel requests supported -fnesc-dump */
/* ------------------------------------------- */

void process_components(nd_option opt, nesc_declaration program, cgraph cg,
			dd_list modules, dd_list comps)
{
  dd_list_pos scan_components;

  if (!comps)
    {
      error("components can only be requested on an actual program");
      return;
    }

  create_components();
  dd_scan (scan_components, comps)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      if (dump_filter_ndecl(comp))
	dhaddif(components, comp);
    }
}

static void process_component_interfaces(nesc_declaration comp)
{
  env_scanner scan;
  const char *name;
  void *decl;

  env_scan(comp->env->id_env, &scan);
  while (env_next(&scan, &name, &decl))
    {
      data_declaration ddecl = decl;

      if (ddecl->kind == decl_interface_ref &&
	  dump_filter_ddecl(ddecl))
	dhaddif(interfaces, ddecl);
    }
}

void process_interfaces(nd_option opt, nesc_declaration program, cgraph cg,
		     dd_list modules, dd_list comps)
{
  dd_list_pos scan_components;

  if (!comps)
    {
      error("interfaces can only be requested on an actual program");
      return;
    }

  create_interfaces();
  dd_scan (scan_components, comps)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      process_component_interfaces(comp);
    }
}

void process_tags(nd_option opt, nesc_declaration program, cgraph cg,
	       dd_list modules, dd_list comps)
{
  env_scanner scan;
  const char *name;
  void *decl;

  create_tags();
  env_scan(global_env->tag_env, &scan);
  while (env_next(&scan, &name, &decl))
    {
      tag_declaration tdecl = decl;

      if (dump_filter_tdecl(tdecl))
	dhaddif(tags, tdecl);
    }
}

void process_wiring(nd_option opt, nesc_declaration program, cgraph cg,
		 dd_list modules, dd_list comps)
{
  if (!comps)
    {
      error("wiring can only be requested on an actual program");
      return;
    }
  wiring = TRUE;
}

void process_referenced(nd_option opt, nesc_declaration program, cgraph cg,
		     dd_list modules, dd_list comps)
{
  nd_arg arg;

  scan_nd_arg (arg, opt->args)
    if (is_nd_token(arg))
      {
	const char *req = nd_tokenval(arg);

	if (!strcmp(req, "tags"))
	  {
	    reachable_tags = TRUE;
	    create_tags();
	  }
	else
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

void dump_info(nesc_declaration program, cgraph cg,
	       dd_list modules, dd_list comps)
{
  dd_list_pos scan_opts;

  dd_scan (scan_opts, opts)
    {
      nd_option opt = DD_GET(nd_option, scan_opts);

      dump_set_filter(opt);

      if (!strcmp(opt->name, "components"))
	process_components(opt, program, cg, modules, comps);
      else if (!strcmp(opt->name, "interfaces"))
	process_interfaces(opt, program, cg, modules, comps);
      else if (!strcmp(opt->name, "tags"))
	process_tags(opt, program, cg, modules, comps);
      else if (!strcmp(opt->name, "wiring"))
	process_wiring(opt, program, cg, modules, comps);
      else if (!strcmp(opt->name, "referenced"))
	process_referenced(opt, program, cg, modules, comps);
      else
	error("unknown dump request `%s'", opt->name);
    }

  xml_start(stdout);
  simpletag("nesc");

  if (wiring)
    dump_wiring(cg);
  if (components)
    dump_list("components", components, dump_component);
  if (interfaces)
    dump_list("interfaces", interfaces, dump_interface);
  if (tags)
    dump_list("tags", tags, dump_tag);

  simpletag_pop();
  xml_end();
}
