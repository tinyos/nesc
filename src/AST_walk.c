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

#include "parser.h"
#include "AST_walk.h"
#include "AST_utils.h"

struct AST_walker
{
  AST_walker_fn walkers[postkind_node + 1 - kind_node];
};

static AST_walker_result default_walker(AST_walker spec, void *data,
					node *n)
{
  return aw_walk;
}

AST_walker new_AST_walker(region r)
/* Effcts: creates a new AST walker in region r. Default behaviour
     is to assert(0); */
{
  AST_walker walker = ralloc(r, struct AST_walker);

  AST_walker_handle(walker, kind_node, default_walker);

  return walker;
}

void AST_walker_handle(AST_walker spec, AST_kind kind, AST_walker_fn fn)
/* Effects: Sets walker function for node kind and all its children to
     fn
*/
{
  AST_kind k;

  assert(kind >= kind_node && kind <= postkind_node);
  for (k = kind; k <= AST_post_kind[kind - kind_node]; k++)
    spec->walkers[k - kind_node] = fn;
}

/* Just execute the walker function for node-type 'kind' */
AST_walker_result AST_walker_call(AST_walker spec, AST_kind kind, void *data, node *n)
{
  assert(kind >= kind_node && kind <= postkind_node && IS_A(*n, kind));
  return spec->walkers[kind - kind_node](spec, data, n);
}

void AST_walk_list(AST_walker s, void *d, node *n)
{
  while (*n)
    {
      AST_walk(s, d, n);
      n = &(*n)->next;
    }
}

void AST_walk_children(AST_walker s, void *d, node n)
{
  switch (n->kind)
    {
#include "AST_walk_children.c"
    default:
      break;
    }
}

/* Recursive walk from n */
void AST_walk(AST_walker spec, void *data, node *n)
{
  for (;;)
    {
      AST_kind k = (*n)->kind;

      switch (AST_walker_call(spec, k, data, n))
	{
	case aw_done: return;
	case aw_call_parent:
	  k = AST_parent_kind[k - kind_node];
	  if (!k)
	    return;
	  break;
	case aw_walk:
	  AST_walk_children(spec, data, *n);
	  return;
	}
    }
}
