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

#ifndef GRAPH_H
#define GRAPH_H

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

typedef struct ggraph *ggraph;
typedef struct gnode *gnode;
typedef struct gedge *gedge;

/* Update functions */
/* ---------------- */

ggraph new_graph(region r);
/* Returns: A new empty graph. All allocations are performed in region r.
*/

void delete_graph(ggraph g,
		  void (*delete_node)(gnode n),
		  void (*delete_edge)(gedge e));
/* Effects: Deletes graph g. Calls functions delete_node & delete_edge
     on nodes and edges before deleting them. All edges of a node
     are deleted before it is.

     If a delete_node/edge function is NULL it is not called.
   Modifies: g
*/

ggraph copy_graph(region r, ggraph g);
/* Effects: Returns a graph that is a copy of g (i.e. with the same
     number of nodes, and the same connections between them).
     Modifications to g won't modify the copy, and vice-versa.
*/

gnode graph_add_node(ggraph g, void *data);
/* Effects: Adds a new node to graph g, with user data `data'.
     The new node has no edges and is not marked.
   Returns: The new node.
   Modifies: g
*/

bool graph_remove_node(gnode n);
/* Effects: Removes node n from its graph if it has no in/outgoing edges.
   Returns: TRUE if n could be removed.
   Modifies: n
*/

gedge graph_add_edge(gnode from, gnode to, void *data);
/* Effects: Adds an edge between nodes `from' & `to', with user data
     `data'. The new edge is not marked.
   Returns: The new edge.
   Modifies: from, to
*/

void graph_remove_edge(gedge e);
/* Effects: Removes edge e from its graph.
   Modifies: e
*/

/* Accessor operations */
/* ------------------- */

dd_list graph_nodes(region r, ggraph g);
/* Returns: A list of all the nodes of g, each element of the list has
     type gnode.
     The list is allocated in region r.
*/

gnode graph_first_node(ggraph g);
/* Returns: The first node of g, or NULL if none
     The order of nodes is arbitrary
*/

gnode graph_next_node(gnode n);
/* Returns: The node after n, or NULL if no more
     The order of nodes is arbitrary
*/

#define graph_scan_nodes(node, g) \
  for (node = graph_first_node((g)); node; node = graph_next_node(node))
/* Effects: Iterates variable node over the nodes of graph g
   Requires: No nodes are added or removed from the graph during
     the iteration.
   Example:
     count = 0;
     graph_scan_nodes (node, g) count++;
*/


ggraph graph_node_graph(gnode n);
/* Returns: The graph n is a node of
*/

void *_graph_node_data(gnode n);
#define NODE_GET(type, n) ((type)_graph_node_data((n)))
/* Returns: the data of node n, cast to `type'
*/

void _graph_node_set(gnode n, void *data);
#define NODE_SET(n, data) (_graph_node_set((n), (void *)(data)))
/* Effects: Sets the data of node n to `data'.
   Modifies: n
*/

dd_list graph_edges_in(region r, gnode n);
/* Returns: The list of ingoing edges for n.
     Each element of the list has type gedge.
     It is up to the caller to free the list with
       dd_free_list(l, NULL)
*/

dd_list graph_edges_out(region r, gnode n);
/* Returns: The list of outgoing edges for n.
     Each element of the list has type gedge.
     It is up to the caller to free the list with
       dd_free_list(l, NULL)
*/

gedge graph_first_edge_in(gnode n);
/* Returns: The first ingoing edge of n, or NULL if none
     The order of edges is arbitrary
*/

gedge graph_next_edge_in(gedge e);
/* Returns: The ingoing edge after e, or NULL if no more
     The order of edges is arbitrary
*/

gedge graph_first_edge_out(gnode n);
/* Returns: The first outgoing edge of n, ore NULL if none
     The order of edges is arbitrary
*/

gedge graph_next_edge_out(gedge e);
/* Returns: The outgoing edge after e, or NULL if no more
     The order of edges is arbitrary
*/

#define graph_scan_in(edge, n) \
  for (edge = graph_first_edge_in((n)); edge; edge = graph_next_edge_in(edge))
/* Effects: Iterates variable edge over the ingoing edges of node n
   Requires: The node and its ingoing edges not be modified during
     the iteration.
   Example:
     in = 0;
     graph_scan_in (e, n) in++;
*/

#define graph_scan_out(edge, n) \
  for (edge = graph_first_edge_out((n)); edge; edge = graph_next_edge_out(edge))
/* Effects: Iterates variable edge over the outgoing edges of node n
   Requires: The node and its outgoing edges not be modified during
     the iteration.
   Example:
     out = 0;
     graph_scan_out (e, n) out++;
*/

gnode graph_edge_from(gedge e);
/* Returns: The node this edge comes from.
*/

gnode graph_edge_to(gedge e);
/* Returns: The node this edge goes to.
*/

void *_graph_edge_data(gedge n);
#define EDGE_GET(type, n) ((type)_graph_edge_data((n)))
/* Returns: the data of edge n, cast to `type'
*/

void _graph_edge_set(gedge n, void *data);
#define EDGE_SET(n, data) (_graph_edge_set((n), (void *)(data)))
/* Effects: Sets the data of edge n to `data'.
   Modifies: n
*/


/* Marks */
/* ----- */

void graph_clear_all_marks(ggraph g);
/* Effects: Clears all marks on edges and nodes of g.
   Modifies: g
*/

void graph_mark_node(gnode n);
/* Effects: Marks node n.
   Modifies: n
*/

void graph_unmark_node(gnode n);
/* Effects: Removed mark from node n.
   Modifies: n
*/

bool graph_node_markedp(gnode n);
/* Returns: TRUE if n is marked
*/

void graph_mark_edge(gedge n);
/* Effects: Marks edge n.
   Modifies: n
*/

void graph_unmark_edge(gedge n);
/* Effects: Removed mark from edge n.
   Modifies: n
*/

bool graph_edge_markedp(gedge n);
/* Returns: TRUE if n is marked
*/

#endif
