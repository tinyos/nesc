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
Boston, MA 02111-1307, USA. */

#ifndef AST_WALK_H
#define AST_WALK_H

/* A generic, OO-ish AST walker.
   This is probably a visitor, for those into that kind of thing. 
   The walker functions receive a node * rather than a node, 
   so can modify the tree during the walk. This allows, e.g., 
   cloning the AST.
*/

/* Untyped to make declaration easier. Actual signature is
     AST_walker_result AST_walker_fn(AST_walker spec, void *data,
  				     <your node type> *n);
   Note: the aw_walk and aw_call_parent results will be applied to
   *n after the walker function returns, not to the orignal node.
*/
typedef enum {
  aw_walk, /* walk children */
  aw_call_parent, /* call parent function */
  aw_done  /* don't walk children */
} AST_walker_result;

typedef AST_walker_result (*AST_walker_fn)();
typedef struct AST_walker *AST_walker;

AST_walker new_AST_walker(region r);
/* Effcts: creates a new AST walker in region r. Default behaviour
     is to just walk through the children (i.e., a function returning
     aw_walk) */

void AST_walker_handle(AST_walker spec, AST_kind kind, AST_walker_fn fn);
/* Effects: Sets walker function for node kind and all its children to
     fn
*/

/* Recursive walk from n */
void AST_walk(AST_walker spec, void *data, node *n);

void AST_walk_list(AST_walker spec, void *data, node *n);
/* Effects: Walks through the list starting at *n
     The walkers "divert" AST_walk_list if they modify the
     node or node pointer they receive.
*/

/* Walk children of n */
void AST_walk_children(AST_walker spec, void *data, node n);

/* Just execute the walker function for node-type 'kind' */
AST_walker_result AST_walker_call(AST_walker spec, AST_kind kind,
				  void *data, node *n);

#endif
