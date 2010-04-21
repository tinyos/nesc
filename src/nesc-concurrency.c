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
#include "nesc-concurrency.h"
#include "nesc-semantics.h"
#include "machine.h"
#include "c-parse.h"

static inline bool is_call_edge(gedge e)
{
  return (EDGE_GET(use, e)->c & c_fncall) != 0;
}

static void rec_async(gnode n, bool async_caller)
{
  gedge edge;
  data_declaration fn = NODE_GET(endp, n)->function;
  bool async = fn->async || fn->actual_async || async_caller;


  if (async == fn->actual_async)
    return;
  fn->actual_async = async;

  /* Martin Leopold: add an "async" gcc attribute to any async elements for targets
     that need to identify functions callable from interrupts. */
  if (fn->definition && target->async_functions_atribute) 
    {
      function_decl fd = CAST(function_decl, fn->definition);
      location l = fd->modifiers->location;
      region r = parse_region;
      word aname = new_word(r, l, str2cstring(r, target->async_functions_atribute));
      target_attribute attr = new_target_attribute(r, l, aname, NULL);

      fd->attributes = attribute_chain(attr, fd->attributes);
    }

  /* We don't pass async through commands or events that are not
     declared async to avoid reporting errors for the fns called
     by the "async but not so declared" command or event */
  if (ddecl_is_command_or_event(fn))
    async = fn->async;

  graph_scan_out (edge, n) 
    if (is_call_edge(edge))
      rec_async(graph_edge_to(edge), async);
}

void async_violation(gnode n)
{
  data_declaration fn = NODE_GET(endp, n)->function;
  gedge edge;

  graph_scan_in (edge, n)
    if (is_call_edge(edge))
      {
	use u = EDGE_GET(use, edge);
	data_declaration caller = NODE_GET(endp, graph_edge_from(edge))->function;

	if (caller->actual_async)
	  nesc_warning_with_location(u->l, "`%s' called asynchronously from `%s'",
				     decl_printname(fn), decl_printname(caller));
      }
}


void check_async(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  /* Find least fixed point of async w/ recursive graph walk */
  graph_scan_nodes (n, cg)
    rec_async(n, FALSE);

  /* Report violations of async. We force async warnings when detecting
     data races. */
  if (warn_async || warn_data_race)
    graph_scan_nodes (n, cg)
      {
	data_declaration fn = NODE_GET(endp, n)->function;

	if (ddecl_is_command_or_event(fn) && fn->actual_async && !fn->async)
	  async_violation(n);
      }
}

static dd_list find_async_variables(region r, cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  dd_list avars = dd_new_list(r);

  /* Set async_access in all global or static variables that are accessed
     (r, w) in an async function. Return list of all such variables */

  /* XXX: aliasing issues ignored for now. */

  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;
      dd_list_pos use;
      
      if (fn->actual_async && fn->fn_uses)
	dd_scan (use, fn->fn_uses)
	  {
	    iduse i = DD_GET(iduse, use);
	    data_declaration id = i->id;
	    context c = i->u->c;

	    if (id->kind == decl_variable && !id->islocal &&
		c & (c_read | c_write))
	      {
		if (!id->async_access)
		  {
		    id->async_access = TRUE;
		    dd_add_last(r, avars, id);
		  }
		if (c & c_write)
		  id->async_write = TRUE;
	      }
	  }
    }
  return avars;
}

static void rec_contexts(gnode n, int call_contexts)
{
  gedge edge;
  data_declaration fn = NODE_GET(endp, n)->function;
  int new_context = fn->call_contexts | fn->extra_contexts |
    call_contexts | fn->spontaneous;

  if (new_context == fn->call_contexts)
    return;
  fn->call_contexts = new_context;

  graph_scan_out (edge, n) 
    {
      use u = EDGE_GET(use, edge);
      int cc = new_context;

      if (u->c & c_fncall)
	{
	  if (u->c & c_atomic)
	    cc = c_call_atomic;
	}
      else /* Non-call use. Conservatively assume that there may be
	      atomic and non-atomic calls if this value ends up used as
	      a function pointer */
	cc = c_call_atomic | c_call_nonatomic;
      rec_contexts(graph_edge_to(edge), cc);
    }
}

static void find_fn_contexts(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  /* Find least fixed point of call_contexts w/ recursive graph walk */
  graph_scan_nodes (n, cg)
    rec_contexts(n, 0);
}

static void check_async_vars(dd_list avars)
{
  dd_list_pos avar;

  dd_scan (avar, avars)
    {
      data_declaration v = DD_GET(data_declaration, avar);
      dd_list_pos ause;
      bool first = TRUE;

      if (!v->norace)
	dd_scan (ause, v->nuses)
	  {
	    use u = DD_GET(use, ause);
	    context bad_contexts = c_write;

	    /* If there are no writes in async contexts, then reads
	       need not be protected */
	    if (v->async_write)
	      bad_contexts |= c_read;

	    /* Bad uses are uses that are both:
	       - outside atomic statements (and fns only called from atomic
	         statements)
	       - uses specified by bad_contexts
	       u->fn can be NULL in weird cases which don't correspond to
	       executable code.
	    */
	    if (u->fn &&
		!(u->c & c_atomic ||
		  !(u->fn->call_contexts & c_call_nonatomic))
		&& u->c & bad_contexts)
	      {
		const char *cname;

		if (first)
		  {
		    location vloc =
		      v->definition ? v->definition->location : v->ast->location;
		    first = FALSE;
		    nesc_warning_with_location
		      (vloc, "non-atomic accesses to shared variable `%s':",
		       v->name);
		  }

		if ((u->c & (c_read | c_write)) == (c_read | c_write) &&
		    v->async_write)
		  cname = "r/w";
		else if (u->c & c_read)
		  cname = "read";
		else
		  cname = "write";
		nesc_warning_with_location(u->l, "  non-atomic %s", cname);
	      }
	  }
    }
}

void check_races(cgraph callgraph)
{
  region r = newregion();
  dd_list avars;

  /* First we mark all variables which are accessed in an async function.
     Then, we issue a warning for all uses of such variables which are not
     in an atomic context. To do that, we first need to know which contexts
     (atomic vs non-atomic) each function is called in. 
     Exception: read-only variables do not need warnings.
  */

  avars = find_async_variables(r, callgraph);
  find_fn_contexts(callgraph);

  if (warn_data_race)
    check_async_vars(avars);

  deleteregion(r);
}

