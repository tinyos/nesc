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
#include "semantics.h"
#include "nesc-env.h"
#include "nesc-decls.h"
#include "nesc-cg.h"
#include "nesc-paths.h"
#include "nesc-c.h"
#include "c-parse.h"
#include "nesc-generate.h"

/* Adds the component graph 'component' to the whole program graph 'master' */
static void connect_graph(cgraph master, cgraph component)
{
  ggraph cg = cgraph_graph(component);
  gnode n;
  gedge connection;

  /* Add all edges from component to master */
  graph_scan_nodes (n, cg)
    {
      endp from = NODE_GET(endp, n);
      gnode mfrom = endpoint_lookup(master, from);

      graph_scan_out (connection, n)
	{
	  endp to = NODE_GET(endp, graph_edge_to(connection));
	  gnode mto = endpoint_lookup(master, to);

	  graph_add_edge(mfrom, mto, NULL);	  
	}
    }
}

static void connect(component_declaration cdecl,
		    cgraph cg, dd_list modules, dd_list components)
{
  if (!dd_find(components, cdecl))
    {
      dd_add_last(regionof(components), components, cdecl);
      connect_graph(cg, cdecl->connections);

      if (is_module(cdecl->impl))
	dd_add_last(regionof(modules), modules, cdecl);
      else
	{
	  component_ref comp;
	  configuration c = CAST(configuration, cdecl->impl);

	  scan_component_ref (comp, c->components)
	    connect(comp->cdecl, cg, modules, components);
	}
    }
}

static void connect_graphs(region r, component_declaration program,
			   cgraph *cg, dd_list *modules, dd_list *components)
{
  *cg = new_cgraph(r);
  *modules = dd_new_list(r);
  *components = dd_new_list(r);

  connect(program, *cg, *modules, *components);
}

void nesc_compile(const char *component_name, const char *target_name)
{
  struct location toplevel;
  component_declaration program;

  parse_region = newregion();
  init_types();
  cval_init();
  init_lex();
  init_semantics();
  init_nesc_env(parse_region);

  init_nesc_paths_end();

  toplevel.filename = "<commandline>";
  toplevel.lineno = 0;
  toplevel.in_system_header = FALSE;
  
  require_c(&toplevel, "tos");
  if (component_name == NULL) {
    fprintf(stderr, ("usage: parser <component>\n"));
    return;
  }
  program = require_component(&toplevel, component_name);

  if (errorcount == 0)
    {
      cgraph cg;
      dd_list modules, components;

      connect_graphs(parse_region, program, &cg, &modules, &components);
      generate_c_code(program, target_name, cg, modules);
    }
}
