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

#include <fcntl.h>
#include <errno.h>

#include "parser.h"
#include "semantics.h"
#include "nesc-env.h"
#include "nesc-decls.h"
#include "nesc-cg.h"
#include "nesc-paths.h"
#include "nesc-c.h"
#include "nesc-interface.h"
#include "nesc-component.h"
#include "c-parse.h"
#include "nesc-generate.h"
#include "nesc-doc.h"
#include "nesc-semantics.h"
#include "nesc-cpp.h"
#include "nesc-msg.h"
#include "nesc-magic.h"
#include "nesc-uses.h"
#include "nesc-abstract.h"
#include "nesc-constants.h"
#include "edit.h"
#include "machine.h"

/* The set of C files to require before loading the main component */
struct ilist
{
  struct ilist *next;
  char *name;
};

static struct ilist *includelist;
static region includeregion;

void add_nesc_include(const char *name)
{
  struct ilist *np;

  if (!includeregion)
    includeregion = newregion();

  np = ralloc(includeregion, struct ilist);
  np->next = includelist;
  includelist = np;
  np->name = rstrdup(includeregion, name);
}

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

static void connect(location loc, nesc_declaration cdecl,
		    cgraph cg, dd_list modules, dd_list components)
{
  nesc_declaration loop;

  if ((loop = abstract_recursion()))
    {
      /* We can help the programmer find the loop by showing the 
	 instantiation path that causes it. loop's instance name is a prefix
	 of cdecl's, the looping path is loop's abstract component name
	 followed by the difference between loop's and cdecl's instance name.
      */
      error_with_location(loc, "component instantiation loop `%s%s'",
			  original_component(loop)->name,
			  cdecl->instance_name + strlen(loop->instance_name));
    }
  else if (!dd_find(components, cdecl))
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
	    {
	      push_instance(comp->cdecl);
	      if (comp->cdecl->original)
		instantiate(comp->cdecl);
	      connect(comp->location, comp->cdecl, cg, modules, components);
	      pop_instance();
	    }
	}
    }
}

static void connect_graphs(region r, nesc_declaration program,
			   cgraph *cg, dd_list *modules, dd_list *components)
{
  *cg = new_cgraph(r);
  *modules = dd_new_list(r);
  *components = dd_new_list(r);

  push_instance(program);
  connect(toplevel_location, program, *cg, *modules, *components);
  pop_instance();
}

bool nesc_option(char *p)
{
  if (p[0] != '-')
    return FALSE;

  /* Skip optional (make gcc 3.x happy) initial _ added by ncc */
  if (p[1] == '_')
    p += 2;
  else
    p += 1;

  /* Yes, using here strlen is evil. But who *really* cares? */
  if (!strncmp (p, "fnesc-nido-tosnodes=", strlen("fnesc-nido-tosnodes=")))
    {
      nido_num_nodes = p + strlen("fnesc-nido-tosnodes=");
    }
  else if (!strncmp (p, "fnesc-nido-motenumber=", strlen("fnesc-nido-motenumber=")))
    {
      nido_mote_number = p + strlen("fnesc-nido-motenumber=");
    }
  else if (!strncmp (p, "fnesc-include=", strlen("fnesc-include=")))
    {
      add_nesc_include(p + strlen("fnesc-include="));
    }
  else if (!strncmp (p, "fnesc-path=", strlen("fnesc-path=")))
    {
      add_nesc_path(p + strlen("fnesc-path="));
    }
  else if (!strncmp (p, "fnesc-msg=", strlen("fnesc-msg=")))
    {
      select_nesc_msg(p + strlen("fnesc-msg="));
    }
  else if (!strcmp (p, "fnesc-csts"))
    {
      select_nesc_csts();
    }
  else if (!strncmp (p, "fnesc-target=", strlen("fnesc-target=")))
    {
      char *target = p + strlen("fnesc-target=");
      if (!strcmp(target, "pc"))
	use_nido = TRUE;
      select_target(target);
    }
  else if (!strcmp (p, "fnesc-no-debug"))
    {
      flag_no_debug = 1;
    }
  else if (!strcmp (p, "fnesc-no-inline"))
    {
      flag_no_inline = 1;
    }
  else if (!strcmp (p, "fnesc-verbose"))
    {
      flag_verbose = 2;
    }
  else if (!strcmp (p, "fnesc-save-macros"))
    {
      flag_save_macros = 1;
    }
  else if (!strncmp (p, "fnesc-docdir=", strlen("fnesc-docdir=")))
    {
      doc_set_outdir(p + strlen("fnesc-docdir="));
    }
  else if (!strncmp (p, "fnesc-topdir=", strlen("fnesc-topdir=")))
    {
      doc_add_topdir(p + strlen("fnesc-topdir="));
    }
  else if (!strncmp (p, "fnesc-is-app", strlen("fnesc-is-app")))
    {
      doc_is_app(TRUE);
    }
  else if (!strncmp (p, "fnesc-docs-use-graphviz", strlen("fnesc-docs-use-graphviz")))
    {
      doc_use_graphviz(TRUE);
    }
  else if (!strcmp (p, "Wnesc-docstring"))
    warn_unexpected_docstring = 1;
  else if (!strcmp (p, "Wno-nesc-docstring"))
    warn_unexpected_docstring = 0;
  else if (!strcmp (p, "Wnesc-fnptr"))
    warn_fnptr = 1;
  else if (!strcmp (p, "Wno-nesc-fnptr"))
    warn_fnptr = 0;
  else if (!strcmp (p, "Wnesc-data-race"))
    warn_data_race = 1;
  else if (!strcmp (p, "Wno-nesc-data-race"))
    warn_data_race = 0;
  else if (!strcmp (p, "Wnesc-async"))
    warn_async = 1;
  else if (!strcmp (p, "Wno-nesc-async"))
    warn_async = 0;
  else if (!strcmp (p, "Wnesc-combine"))
    warn_no_combiner = 1;
  else if (!strcmp (p, "Wno-nesc-combine"))
    warn_no_combiner = 0;
  else if (!strcmp (p, "Wnesc-all"))
    warn_data_race = warn_fnptr = warn_async = warn_no_combiner = 1;
  else if (!strcmp (p, "Wnesc-error"))
    nesc_error = TRUE;
  else
    return FALSE;

  return TRUE;
}

static void destroy_target(const char *name)
{
  if (name)
    {
      /* unlink would be nicer, but would have nasty consequences for
	 -o /dev/null when run by root... */
      int fd = creat(name, 0666);

      if (fd < 0)
	{
	  fprintf(stderr, "%s: ", name);
	  perror("failed to truncate target");
	}
      else
	close(fd);
    }

}

void nesc_compile(const char *filename, const char *target_name)
{
  struct ilist *includes;

  if (filename == NULL)
    {
      fprintf(stderr, "usage: nesc1 <filename>\n");
      return;
    }

  parse_region = newregion();
  preprocess_init();
  cval_init();
  init_types();
  init_lex();
  init_semantics();
  init_nesc_env(parse_region);
  init_nesc_paths_end();
  init_magic_functions();
  init_uses();
  init_abstract();
  init_nesc_constants();

  for (includes = includelist; includes; includes = includes->next)
    require_c(toplevel_location, includes->name);

  if (nesc_filename(filename))
    {
      /* We need to assume some language - it will get fixed once we
	 see the actual file */
      nesc_declaration program = load(l_component, toplevel_location, filename, TRUE);

      if (errorcount == 0)
	{
	  /* Destroy target in all circumstances (prevents surprises
	     when "compiling" interfaces) */
	  destroy_target(target_name);

	  if (dump_msg_layout())
	    ;
	  else if (program->kind == l_component && !program->abstract)
	    {
	      cgraph cg;
	      dd_list modules, components;

	      connect_graphs(parse_region, program, &cg, &modules, &components);
	      current.container = NULL;
	      fold_program(program);

	      if (errorcount == 0 && !generate_docs(filename, cg))
		generate_c_code(program, target_name, cg, modules);
	    }
	  else /* generate docs if requested */
	    generate_docs(filename, NULL);
	}
    }
  else
    {
      /* load C file and extract any requested message formats */
      load_c(toplevel_location, filename, TRUE);
      if (errorcount == 0)
	{
	  fold_program(NULL);
	  if (errorcount == 0)
	    {
	      /* Destroy target in all circumstances (prevents surprises
		 when "compiling" interfaces) */
	      destroy_target(target_name);

	      dump_msg_layout();
	    }
	}
    }
}
