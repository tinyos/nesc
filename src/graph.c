/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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
Boston, MA 02111-1307, USA. */

#include <regions.h>
#include "parser.h"
#include "graph.h"
#include "dd_list.h"
#include "sd_list.h"

#undef new

/* A generic directed graph type, with support for typical graph algos.
*/

/* A graph is a mutable data structure composed of nodes linked 
   by edges.

   Edges can be added and removed between existing nodes, nodes can
   be added at will but only removed if they have no ingoing and no
   outgoing edges.

   Each edge & node has associated user data (stored as a (void *))
   and a mark. Marks can be cleared individually or over the
   whole graph (this last op remains O(1)).

   The order of edges within a node is not preserved.
*/

struct ggraph 
{
  region sameregion r;
  sd_list sameregion nodes;
  long mark_count;		/* Value used to mark nodes/edges */
};

struct gnode
{
  struct sd_list_pos node;
  ggraph sameregion graph;
  gedge sameregion in;		/* Ingoing edges */
  gedge sameregion out;		/* Outgoing edges */
  void *data;
  long mark;			/* Mark count for this node */
};

struct gedge
{
  gnode sameregion in_node, out_node;	/* Extremities */
  gedge sameregion next_in, next_out;	/* Edge in in/outgoing lists */
  void *data;
  long mark;
};

/* Update functions */
/* ---------------- */

ggraph new_graph(region r)
/* Returns: A new empty graph.
*/
{
  ggraph new = ralloc(r, struct ggraph);

  new->r = r;
  new->nodes = sd_new_list(r);
  new->mark_count = 1;

  return new;
}

void delete_graph(ggraph g,
		  void (*delete_node)(gnode n),
		  void (*delete_edge)(gedge e))
/* Effects: Deletes graph g. Calls functions delete_node & delete_edge
     on nodes and edges before deleting them. All edges of a node
     are deleted before it is.

     If a delete_node/edge function is NULL it is not called.
   Modifies: g
*/
{
  /* Delete all edges */
  sd_list_pos node, next;

  sd_scan (node, g->nodes)
    {
      gnode n = SD_GET(gnode, node);
      gedge e, next;
      
      for (e = n->in; e; e = next)
	{
	  next = e->next_in;
	  if (delete_edge) delete_edge(e);
	}
    }
  for (node = sd_first(g->nodes); !sd_is_end(node); node = next)
    {
      gnode n = SD_GET(gnode, node);

      next = sd_next(node);
      if (delete_node) delete_node(n);
    }
  sd_del_list(g->nodes);
}

ggraph copy_graph(region r, ggraph g)
/* Effects: Returns a graph that is a copy of g (i.e. with the same
     number of nodes, and the same connections between them, the same
     marks, the same data, etc).
     Modifications to g won't modify the copy, and vice-versa.
*/
{
  ggraph copy = new_graph(r);
  gnode onode, nnode;
  gedge oedge, nedge;

  copy->mark_count = g->mark_count;

  /* Make nodes in copy */
  graph_scan_nodes (onode, g) onode->data = graph_add_node(copy, onode->data);

  /* Then make edges */
  graph_scan_nodes (onode, g)
    {
      nnode = onode->data;

      graph_scan_in (oedge, onode)
	{
	  nedge = graph_add_edge(graph_edge_from(oedge)->data, nnode, oedge->data);
	  nedge->mark = oedge->mark;
	}
    }

  /* Finally restore original graph */
  graph_scan_nodes (onode, g)
    {
      nnode = onode->data;
      nnode->mark = onode->mark;
      onode->data = nnode->data;
    }

  return copy;
}

gnode graph_add_node(ggraph g, void *data)
/* Effects: Adds a new node to graph g, with user data `data'.
     The new node has no edges and is not marked.
   Returns: The new node.
   Modifies: g
*/
{
  gnode new = ralloc(g->r, struct gnode);

  new->graph = g;
  /*new->in = new->out = NULL;*/
  new->data = data;
  new->mark = g->mark_count - 1; /* Unmarked */
  sd_add_last(g->nodes, &new->node);

  return new;
}

bool graph_remove_node(gnode n)
/* Effects: Removes node n from its graph if it has no in/outgoing edges.
   Returns: TRUE if n could be removed.
   Modifies: n
*/
{
  if (n->in || n->out) return FALSE;
  sd_remove(&n->node);

  return TRUE;
}

gedge graph_add_edge(gnode from, gnode to, void *data)
/* Effects: Adds an edge between nodes `from' & `to', with user data
     `data'. The new edge is not marked.
   Returns: The new edge.
   Modifies: from, to
*/
{
  gedge new = ralloc(from->graph->r, struct gedge);

  new->out_node = from;
  new->in_node = to;
  new->data = data;
  new->mark = from->graph->mark_count - 1;
  
  /* Add to nodes */
  new->next_out = from->out;
  from->out = new;
  new->next_in = to->in;
  to->in = new;

  return new;
}

void graph_remove_edge(gedge e)
/* Effects: Removes edge e from its graph.
   Modifies: e
*/
{
  gedge sameregion *scan;

  /* Remove edge from nodes */
  for (scan = &e->in_node->in; *scan != e; scan = &(*scan)->next_in) ;
  *scan = e->next_in;

  for (scan = &e->out_node->out; *scan != e; scan = &(*scan)->next_out) ;
  *scan = e->next_out;
}

/* Accessor operations */
/* ------------------- */

dd_list graph_nodes(region r, ggraph g)
/* Returns: A list of all the nodes of g, each element of the list has
     type gnode.
*/
{
  dd_list nodes = dd_new_list(r);
  sd_list_pos node;

  sd_scan (node, g->nodes)
    dd_add_last(r, nodes, SD_GET(gnode, node));

  return nodes;
}

gnode graph_first_node(ggraph g)
/* Returns: The first node of g, or NULL if none
     The order of nodes is arbitrary
*/
{
  sd_list_pos first = sd_first(g->nodes);

  return sd_is_end(first) ? NULL : SD_GET(gnode, first);
}

gnode graph_next_node(gnode n)
/* Returns: The node after n, or NULL if no more
     The order of nodes is arbitrary
*/
{
  sd_list_pos next = sd_next(&n->node);

  return sd_is_end(next) ? NULL : SD_GET(gnode, next);
}

ggraph graph_node_graph(gnode n)
/* Returns: The graph n is a node of
*/
{
  return n->graph;
}

void *_graph_node_data(gnode n)
/* Returns: the data of node n
*/
{
  return n->data;
}

void _graph_node_set(gnode n, void *data)
/* Effects: Sets the data of node n to `data'.
   Modifies: n
*/
{
  n->data = data;
}

dd_list graph_edges_in(region r, gnode n)
/* Returns: The list of ingoing edges for n.
     Each element of the list has type gedge.
     It is up to the caller to free the list with
       dd_free_list(l, NULL)
*/
{
  dd_list edges = dd_new_list(r);
  gedge in;

  for (in = n->in; in; in = in->next_in) dd_add_last(r, edges, in);

  return edges;
}

dd_list graph_edges_out(region r, gnode n)
/* Returns: The list of outgoing edges for n.
     Each element of the list has type gedge.
     It is up to the caller to free the list with
       dd_free_list(l, NULL)
*/
{
  dd_list edges = dd_new_list(r);
  gedge out;

  for (out = n->out; out; out = out->next_out) dd_add_last(r, edges, out);

  return edges;
}

gedge graph_first_edge_in(gnode n)
/* Returns: The first ingoing edge of n, or NULL if none
     The order of edges is arbitrary
*/
{
  return n->in;
}

gedge graph_next_edge_in(gedge e)
/* Returns: The ingoing edge after e, or NULL if no more
     The order of edges is arbitrary
*/
{
  return e->next_in;
}

gedge graph_first_edge_out(gnode n)
/* Returns: The first outgoing edge of n, ore NULL if none
     The order of edges is arbitrary
*/
{
  return n->out;
}

gedge graph_next_edge_out(gedge e)
/* Returns: The outgoing edge after e, or NULL if no more
     The order of edges is arbitrary
*/
{
  return e->next_out;
}

gnode graph_edge_from(gedge e)
/* Returns: The node this edge comes from.
*/
{
  return e->out_node;
}

gnode graph_edge_to(gedge e)
/* Returns: The node this edge goes to.
*/
{
  return e->in_node;
}

void *_graph_edge_data(gedge n)
/* Returns: the data of edge n
*/
{
  return n->data;
}

void _graph_edge_set(gedge n, void *data)
/* Effects: Sets the data of edge n to `data'.
   Modifies: n
*/
{
  n->data = data;
}


/* Marks */
/* ----- */

void graph_clear_all_marks(ggraph g)
/* Effects: Clears all marks on edges and nodes of g.
   Modifies: g
*/
{
  g->mark_count++;
  /* A wrap around of this 32 bit counter is rather unlikely. */
  assert(g->mark_count != 0);
}

void graph_mark_node(gnode n)
/* Effects: Marks node n.
   Modifies: n
*/
{
  n->mark = n->graph->mark_count;
}

void graph_unmark_node(gnode n)
/* Effects: Removed mark from node n.
   Modifies: n
*/
{
  n->mark = n->graph->mark_count - 1;
}

bool graph_node_markedp(gnode n)
/* Returns: TRUE if n is marked
*/
{
  return n->mark == n->graph->mark_count;
}

void graph_mark_edge(gedge n)
/* Effects: Marks edge n.
   Modifies: n
*/
{
  n->mark = n->in_node->graph->mark_count;
}

void graph_unmark_edge(gedge n)
/* Effects: Removed mark from edge n.
   Modifies: n
*/
{
  n->mark = n->in_node->graph->mark_count - 1;
}

bool graph_edge_markedp(gedge n)
/* Returns: TRUE if n is marked
*/
{
  return n->mark == n->in_node->graph->mark_count;
}

void dbg_graph(ggraph g, void (*pnode)(gnode g)) deletes
{
  region temp = newregion();
  dd_list allnodes = graph_nodes(temp, g);
  dd_list_pos anode;
  int i = 0;

  dd_scan (anode, allnodes)
    {
      gnode node = DD_GET(gnode, anode);
      gedge out;

      fprintf(stderr, "%d(0x%p):", i++, node);

      graph_scan_out (out, node)
	{
	  gnode to = graph_edge_to(out);
	  int j = 0;
	  dd_list_pos anode2;

	  dd_scan (anode2, allnodes)
	    {
	      if (DD_GET(gnode, anode2) == to)
		{
		  fprintf(stderr, " %d", j);
		  break;
		}
	      j++;
	    }
	}
      if (pnode)
	pnode(node);
      fprintf(stderr, "\n");
    }
  deleteregion(temp);
}
