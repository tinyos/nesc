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

/* A connection graph */
#include "parser.h"
#include "nesc-cg.h"
#include "utils.h"

struct cgraph
{
  ggraph sameregion g;
  dhash_table sameregion ep_table;
};

typedef struct ep_table_entry
{
  struct endp ep; /* ep.function is the key */
  gnode n;
} *ep_table_entry;

static int ep_compare(void *e1, void *e2)
{
  ep_table_entry ep1 = e1, ep2 = e2;

  return ep1->ep.function == ep2->ep.function &&
    ep1->ep.interface == ep2->ep.interface &&
    ep1->ep.args_node == ep2->ep.args_node;
}

static unsigned long ep_hash(void *e)
{
  ep_table_entry ep = e;

  return hash_ptr(ep->ep.interface) ^ hash_ptr(ep->ep.function) ^ hash_ptr(ep->ep.args_node);
}

cgraph new_cgraph(region r)
{
  cgraph cg = ralloc(r, struct cgraph);

  cg->g = new_graph(r);
  cg->ep_table = new_dhash_table(r, 64, ep_compare, ep_hash);

  return cg;
}

gnode endpoint_lookup(cgraph cg, endp ep)
{
  ep_table_entry gep;

  gep = dhlookup(cg->ep_table, ep);

  if (gep)
    return gep->n;

  gep = ralloc(regionof(cg), struct ep_table_entry);
  gep->ep = *ep;

  dhadd(cg->ep_table, gep);
  return gep->n = graph_add_node(cg->g, &gep->ep);
}

gnode fn_lookup(cgraph cg, data_declaration fndecl)
{
  struct endp ep;

  ep.component = NULL;
  ep.interface = fndecl->interface;
  ep.function = fndecl;
  ep.args_node = NULL;
  return endpoint_lookup(cg, &ep);
}

ggraph cgraph_graph(cgraph cg)
{
  return cg->g;
}

