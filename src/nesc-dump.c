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

dd_list/*nd_option*/ opts;

static void dump_nesc_container(nesc_declaration comp)
{
  if (comp->configuration)
    xml_tag_start("configuration");
  else
    xml_tag_start("module");
  xml_attr("name", comp->instance_name);
  xml_qtag_end();
  xnewline();
}

static void dump_attributes(dd_list/*nesc_attribute*/ attributes)
{
  dd_list_pos scan;

  if (!attributes)
    return;

  dd_scan (scan, attributes)
    {
      nesc_attribute attr = DD_GET(nesc_attribute, scan);

      xml_tag_start("attribute");
      xml_attr("name", attr->word1->cstring.data);
      xml_tag_end();
      /* Should have value here */
      xml_pop();
      xnewline();
    }
}

static bool dump_interface_ref(data_declaration iref)
{
  assert(iref->kind == decl_interface_ref);

  if (!dump_filter_ddecl(iref))
    return FALSE;

  xml_tag_start("interface");
  xml_attr("name", iref->name);
  xml_attr_int("provided", !iref->required);
  xml_tag_end();
  xnewline(); 

  xindent();
  dump_nesc_container(iref->container);
  dump_attributes(iref->attributes);
  xunindent();

  xstartline(); xml_pop();
  xnewline();
  
  return TRUE;
}

static void dump_component_interfaces(nesc_declaration comp)
{
  env_scanner scan;
  const char *name;
  void *decl;
  bool any = FALSE;

  env_scan(comp->env->id_env, &scan);
  while (env_next(&scan, &name, &decl))
    {
      data_declaration ddecl = decl;

      if (ddecl->kind == decl_interface_ref)
	any = dump_interface_ref(decl) || any;
    }
  if (any)
    xnewline();
}

/* The toplevel requests supported -fnesc-dump */


void dump_components(nd_option opt, nesc_declaration program, cgraph cg,
		     dd_list modules, dd_list components)

{
  dd_list_pos scan_components;

  if (!components)
    {
      error("components can only be requested on an actual program");
      return;
    }

  xml_tag("components");
  xnewline(); xindent();
  dd_scan (scan_components, components)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      dump_nesc_container(comp);
    }
  xunindent();
  xstartline(); xml_pop();
  xnewline();
}

void dump_interfaces(nd_option opt, nesc_declaration program, cgraph cg,
		     dd_list modules, dd_list components)

{
  dd_list_pos scan_components;

  if (!components)
    {
      error("interfaces can only be requested on an actual program");
      return;
    }

  xml_tag("interfaces");
  xnewline(); xindent();
  dd_scan (scan_components, components)
    {
      nesc_declaration comp = DD_GET(nesc_declaration, scan_components);

      dump_component_interfaces(comp);
    }
  xunindent();
  xml_pop();
  xnewline();
}

void select_dump(char *what)
{
  nd_option opt = nd_parse(what);

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
	opts = dd_new_list(permanent);
      dd_add_last(permanent, opts, opt);
    }
}

bool dump_selected(void)
{
  return opts != NULL;
}

void dump_info(nesc_declaration program, cgraph cg,
	       dd_list modules, dd_list components)
{
  dd_list_pos scan_opts;

  xml_start(stdout);
  dd_scan (scan_opts, opts)
    {
      nd_option opt = DD_GET(nd_option, scan_opts);

      dump_set_filter(opt);

      if (!strcmp(opt->name, "components"))
	dump_components(opt, program, cg, modules, components);
      else if (!strcmp(opt->name, "interfaces"))
	dump_interfaces(opt, program, cg, modules, components);
      else
	error("unknown dump request `%s'", opt->name);
    }
  xml_end();
}
