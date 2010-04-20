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
#include <unistd.h>

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
#include "nesc-attributes.h"
#include "nesc-constants.h"
#include "nesc-dump.h"
#include "nesc-network.h"
#include "nesc-task.h"
#include "nesc-deputy.h"
#include "edit.h"
#include "machine.h"
#include "nesc-atomic.h"
#include "unparse.h"
#include "nesc-main.h"


/* The set of C files to require before loading the main component */
struct ilist
{
  struct ilist *next;
  char *name;
  bool name_is_path;
};

static struct ilist *includelist, **includelist_end = &includelist;
static region includeregion;

void add_nesc_include(const char *name, bool name_is_path)
{
  struct ilist *np;

  if (!includeregion)
    includeregion = newregion();

  np = ralloc(includeregion, struct ilist);
  np->next = NULL;
  np->name = rstrdup(includeregion, name);
  np->name_is_path = name_is_path;

  *includelist_end = np;
  includelist_end = &np->next;
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

	  graph_add_edge(mfrom, mto, EDGE_GET(location, connection));
	}
    }
}

static void connect(location loc, nesc_declaration cdecl, cgraph cg,
		    cgraph userg, dd_list modules, dd_list components)
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
      connect_graph(userg, cdecl->user_connections);

      if (!cdecl->configuration)
	dd_add_last(regionof(modules), modules, cdecl);
      else
	{
	  configuration c = CAST(configuration, cdecl->impl);
	  declaration d;

	  scan_declaration (d, c->decls)
	    if (is_component_ref(d))
	      {
		component_ref comp = CAST(component_ref, d);

		push_instance(comp->cdecl);
		if (comp->cdecl->original)
		  instantiate(comp->cdecl, comp->args);
		connect(comp->location, comp->cdecl, cg, userg, modules, components);
		pop_instance();
	      }
	}
    }
}

static void connect_graphs(region r, nesc_declaration program, nesc_declaration scheduler,
			   cgraph *cg, cgraph *userg, dd_list *modules, dd_list *components)
{
  *cg = new_cgraph(r);
  *userg = new_cgraph(r);
  *modules = dd_new_list(r);
  *components = dd_new_list(r);

  push_instance(program);
  connect(toplevel_location, program, *cg, *userg, *modules, *components);
  if (scheduler)
    connect(toplevel_location, scheduler, *cg, *userg, *modules, *components);
  pop_instance();
}

int nesc_option(char *p)
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
      add_nesc_include(p + strlen("fnesc-include="), FALSE);
    }
  else if (!strncmp (p, "fnesc-path=", strlen("fnesc-path=")))
    {
      add_nesc_path(p + strlen("fnesc-path="), CHAIN_BRACKET);
    }
  else if (!strncmp (p, "fnesc-msg=", strlen("fnesc-msg=")))
    {
      select_nesc_msg(p + strlen("fnesc-msg="));
    }
  else if (!strcmp (p, "fnesc-csts"))
    {
      select_nesc_csts();
    }
  else if (!strncmp (p, "fnesc-dump=", strlen("fnesc-dump=")))
    {
      select_dump(p + strlen("fnesc-dump="));
    }
  else if (!strncmp (p, "fnesc-dumpfile=", strlen("fnesc-dumpfile=")))
    {
      select_dumpfile(p + strlen("fnesc-dumpfile="));
    }
  else if (!strncmp (p, "fnesc-target=", strlen("fnesc-target=")))
    {
      select_target(p + strlen("fnesc-target="));
    }
  else if (!strcmp (p, "fnesc-simulate"))
    {
      use_nido = TRUE;
    }
  else if (!strncmp (p, "fnesc-gcc=", strlen("fnesc-gcc=")))
    {
      target_compiler = p + strlen("fnesc-gcc=");
    }
  else if (!strcmp (p, "fnesc-mingw-gcc"))
    {
      flag_mingw_gcc = 1;
    }
  else if (!strcmp (p, "fnesc-no-debug"))
    {
      flag_no_debug = 1;
    }
  else if (!strcmp (p, "fnesc-no-inline"))
    {
      flag_no_inline++;
    }
  else if (!strcmp (p, "fnesc-verbose"))
    {
      flag_verbose = 2;
    }
  else if (!strcmp (p, "fnesc-save-macros"))
    {
      flag_save_macros = 1;
    }
  else if (!strncmp (p, "fnesc-scheduler=", strlen("fnesc-scheduler=")))
    {
      set_scheduler(p + strlen("fnesc-scheduler="));
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
  else if (!strcmp (p, "fnesc-optimize-atomic"))
    nesc_optimise_atomic = 1;
  else if (!strncmp (p, "fnesc-genprefix=", strlen("fnesc-genprefix=")))
    unparse_prefix(p + strlen("fnesc-genprefix="));
  else if (!strcmp (p, "fnesc-deputy"))
    flag_deputy = 1;
  else if (!strcmp (p, "fnesc-no-deputy"))
    flag_deputy = 0;
  else if (!strcmp (p, "fnesc-default-safe"))
    flag_default_safe = 1;
  else if (!strcmp (p, "fnesc-default-unsafe"))
    flag_default_safe = 0;
  else if (!strcmp (p, "fnesc-gccize"))
    flag_gccize = 1;
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
  else if (!strncmp(p, "fnesc-diff=", strlen("fnesc-diff=")))
    {
      char *dirs = p + 11, *comma = strchr(dirs, ',');

      /* <inputdir>,<outputdir> or <outputdir> only for original */
      if (comma)
	{
	  diff_input = rstralloc(permanent, comma - dirs + 1);
	  strncpy(diff_input, dirs, comma - dirs);
	  diff_input[comma - dirs] = '\0';
	  diff_output = comma + 1;
	}
      else
	diff_output = dirs;
    }
  else if (!strncmp(p, "fnesc-separator=", strlen("fnesc-separator=")))
    set_function_separator(p + 16);
  else if (!strncmp(p, "fnesc-cppdir=", strlen("fnesc-cppdir=")))
    save_pp_dir(p + 13);
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
  nesc_declaration program = NULL;
  bool gencode = FALSE;
  cgraph cg = NULL, userg = NULL;
  dd_list modules = NULL, components = NULL;

  if (filename == NULL)
    {
      fprintf(stderr, "usage: nesc1 <filename>\n");
      return;
    }

  parse_region = newregion();
  preprocess_init();
  init_nesc_attributes();
  cval_init();
  init_types();
  init_lex();
  init_semantics();
  init_nesc_env(parse_region);
  init_magic_functions();
  init_uses();
  init_abstract();
  init_nesc_constants();
  init_network();
  init_internal_nesc_attributes();
  init_isatomic();
  init_deputy();
  if (target->init)
    target->init();

  for (includes = includelist; includes; includes = includes->next)
    if (includes->name_is_path)
      load_c(toplevel_location, includes->name, TRUE);
    else
      require_c(toplevel_location, includes->name);

  if (flag_use_scheduler)
    load_scheduler();

  if (nesc_filename(filename))
    /* We need to assume some language - it will get fixed once we
       see the actual file */
    program = load(l_any, toplevel_location, filename, TRUE);
  else
    {
      flag_c = TRUE;
      load_c(toplevel_location, filename, TRUE);
    }

  if (errorcount)
    return;

  if (program && program->kind == l_component && !program->abstract)
    {
      connect_graphs(parse_region, program, scheduler, &cg, &userg, &modules, &components);
      if (errorcount)
	return;
      current.container = NULL;
      fold_program(program, scheduler);
      gencode = TRUE;
    }
  else 
    /* The "program" is a C file, interface or abstract component */
    fold_program(NULL, NULL);

  if (errorcount)
    return;

  /* Destroy target in all circumstances (prevents surprises
     when "compiling" interfaces) */
  destroy_target(target_name);

  if (docs_requested())
    {
      if (generic_used)
	error("documentation system does not yet support generic components and interfaces");
      else if (program)
	generate_docs(filename, cg);
      else
	error("documentation requested on a C file ");
      gencode = FALSE;
    }
  if (layout_requested())
    {
      dump_msg_layout();
      gencode = FALSE;
    }
  if (dump_selected())
    dump_info(program, cg, userg, modules, components);
  if (gencode)
    generate_c_code(target_name, program, cg, modules, components);
  else if (flag_c)
    generate_c_code(target_name, NULL,
		    new_cgraph(permanent), dd_new_list(permanent),
		    dd_new_list(permanent));
}
