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

static void rec_async(gnode n, bool async_caller)
{
  gedge edge;
  data_declaration fn = NODE_GET(endp, n)->function;
  bool async = fn->async || fn->actual_async || async_caller;


  if (async == fn->actual_async)
    return;
  fn->actual_async = async;

  graph_scan_out (edge, n) 
    if (1)//EDGE_GET(void *, edge)) /* a call */
      rec_async(graph_edge_to(edge), async);
}

void async_violation(gnode n)
{
  data_declaration fn = NODE_GET(endp, n)->function;
  gedge edge;

  error_with_decl(fn->ast, "`%s' called asynchronously from:",
		  decl_printname(fn));

  graph_scan_in (edge, n)
    if (1)//EDGE_GET(void *, edge))
      {
	data_declaration caller = NODE_GET(endp, graph_edge_from(edge))->function;

	if (caller->actual_async)
	  error_with_decl(fn->ast, "  %s", decl_printname(caller));
      }
}


void check_async(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  /* Find least fixed point of async w/ recursive graph walk */
  graph_scan_nodes (n, cg)
    rec_async(n, FALSE);

  /* Report violations of async */
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      if (ddecl_is_command_or_event(fn) && fn->actual_async && !fn->async)
	async_violation(n);
    }
}

