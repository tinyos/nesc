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

#ifndef AST_H
#define AST_H

typedef int id_declaration_list;
typedef struct typelist *typelist;
typedef struct type *type;
typedef struct known_cst *known_cst;

typedef struct edge *edge;

#ifdef NODEREFINT
typedef unsigned short noderef;
#else
struct node;
typedef struct AST_node *noderef;
#endif

#include "sd_list.h"
#include "dd_list.h"
#include "graph.h"
#include "dhash.h"

#include "AST_types.h"
#include "c-lex.h"
#include "config.h"
#include "decls.h"
#include "env.h"
#include "types.h"
#include "cval.h"
#include "utils.h"

enum { struct_type, union_type, enum_type };

typedef enum { command_call, event_signal, post_task, normal_call } nesc_call_kind;

#include "AST_defs.h"

typedef struct AST_ast_generic
{
  AST_kind kind;
} *ast_generic;

#ifdef __GNUC__
#define CAST(type, x) ({ast_generic tEmPcast = (ast_generic)(x); if (tEmPcast) assert(is_ ## type(tEmPcast)); (type)(tEmPcast); })
#define CASTPTR(type, x) ({ast_generic *tEmPcast = (ast_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type *)(tEmPcast); })
#define CASTSRPTR(type, x) ({ast_generic *tEmPcast = (ast_generic *)(x); if (tEmPcast && *tEmPcast) assert(is_ ## type(*tEmPcast)); (type sameregion *)(tEmPcast); })
#else
/* Could also generate some code to make this safe */
#define CAST(type, x) ((type)(x))
#define CASTPTR(type, x) ((type *)(x))
#define CASTSRPTR(type, x) ((type sameregion *)(x))
#endif

unary newkind_unary(region r, AST_kind kind, location location, expression arg1);
binary newkind_binary(region r, AST_kind kind, location location,
		      expression arg1, expression arg2);
tag_ref newkind_tag_ref(region r, AST_kind kind, location location, word word1, attribute attributes, declaration fields, bool defined);
node last_node(node n);
int chain_length(node n);
node AST_chain(node l1, node l2);
void insert_before(node sameregion *list, node before, node n);
node ast_reverse(node l);

void AST_set_parents(node n);
void set_parent(node sameregion *nptr, node parent);
void set_parent_list(node sameregion *list, node parent);

node AST_clone(region r, node n);

void AST_print(node n);

#define AST_SET(parent, ptr, value) \
  (*(ptr) = (value), (value) ? (set_parent(CASTSRPTR(node, (ptr)), CAST(node, (parent))), 0) : 0)

#define AST_SET_FIELD(parent, field, value) \
  AST_SET((parent), &(parent)->field, (value))

#define AST_SET_NEXT(previous, value) \
  AST_SET((previous)->parent, &(previous)->next, (value))

#define AST_REPLACE(n, value) \
  AST_SET((n)->parent_ptr, (n)->parent, (value))

#endif
