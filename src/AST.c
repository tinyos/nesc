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

#include "parser.h"
#include "semantics.h"

#include "AST_types.c"
#include "AST_list_node.c"

void insert_before(node sameregion *list, node before, node n)
{
  while (*list != before)
    list = &(*list)->next;
  *list = n;
  n->next = before;
}


unary newkind_unary(region r, AST_kind kind, location location, expression arg1)
{
  unary obj = new_unary(r, location, arg1);

  obj->kind = kind;

  return obj;
}

binary newkind_binary(region r, AST_kind kind, location location,
		      expression arg1, expression arg2)
{
  binary obj;

  if (kind >= kind_assignment && kind <= postkind_assignment) /* XXX: Yuck */
    obj = CAST(binary, new_assign(r, location, arg1, arg2));
  else
    obj = new_binary(r, location, arg1, arg2);

  obj->kind = kind;

  return obj;
}

tag_ref newkind_tag_ref(region r, AST_kind kind, location location, word word1, attribute attributes, declaration fields, bool defined)
{
  tag_ref obj = new_tag_ref(r, location, word1, attributes, fields, defined);

  obj->kind = kind;

  return obj;
}

/* The separate static version of AST_set_parent allows the qualifier
   check optimiser to figure out a useful signature for this function */
static void AST_set_parent(node sameregion *nptr, node parent)
{
  (*nptr)->parent = parent;
  (*nptr)->parent_ptr = nptr;
}

void set_parent(node sameregion *nptr, node parent)
{
  (*nptr)->parent = parent;
  (*nptr)->parent_ptr = nptr;
}

void set_parent_list(node sameregion *list, node parent)
{
  while (*list)
    {
      set_parent(list, parent);
      list = &(*list)->next;
    }
}

static void AST_set_parent_list(void *vnptr, node parent);

static void AST_set_parent1(node sameregion *nptr, node parent)
{
  node n = *nptr;

  if (parent)
    AST_set_parent(nptr, parent);

  switch (n->kind)
    {
/* Template:
    case kind_foo: {
      foo x = CAST(foo, n);

      AST_set_parent_list(&x->field1, n);
      AST_set_parent_list(&x->field2, n);
      break;
    }
*/
#include "AST_parent.c"
    default:
      assert(0);
    }
}

static void AST_set_parent_list(void *vnptr, node parent)
{
  node sameregion *nptr = CASTSRPTR(node, vnptr);

  while (*nptr)
    {
      AST_set_parent1(nptr, parent);
      nptr = &(*nptr)->next;
    }
}

void AST_set_parents(node n)
{
  
  AST_set_parent_list(&n, NULL);
}

node AST_clone(region r, node n)
{
  int k = n->kind - kind_node;
  node copy;

  /* Copy all of n's field except parent, parent_ptr */
  copy = typed_ralloc(r, AST_sizeof[k], AST_typeof[k]);
  typed_rarraycopy(copy, n, 1, AST_sizeof[k], AST_typeof[k]);
  copy->parent = NULL;
  copy->parent_ptr = NULL;

  return copy;
}

static void AST_print_list(int indent, void *vn);

static void pindent(int by)
{
  int i;

  for (i = 0; i < by; i++)
    putchar(' ');
}

static void AST_print1(int indent, node n)
{
  pindent(indent);
  indent += 1;

  /* Special cases */
  switch (n->kind)
    {
    case kind_identifier: {
      identifier x = CAST(identifier, n);

      printf("identifier %s\n", x->ddecl->name);
      return;
    }
    case kind_lexical_cst: {
      lexical_cst x = CAST(lexical_cst, n);

      printf("lexical_cst %s\n", x->cstring.data);
      return;
    }
    case kind_string_cst: {
      string_cst x = CAST(string_cst, n);

      printf("string_cst %s\n", x->cstring.data);
      return;
    }
    case kind_id_label: {
      id_label x = CAST(id_label, n);

      printf("id_label %s\n", x->cstring.data);
      return;
    }
    case kind_identifier_declarator: {
      identifier_declarator x = CAST(identifier_declarator, n);

      printf("identifier_declarator %s\n", x->cstring.data);
      return;
    }
    case kind_word: {
      word x = CAST(word, n);

      printf("word %s\n", x->cstring.data);
      return;
    }
    case kind_rid: {
      rid x = CAST(rid, n);

      printf("rid %s\n", rid_name(x));
      return;
    }
    default:
      break;
    }

  switch (n->kind)
    {
/* Template:
    case kind_foo: {
      foo x = CAST(foo, n);

      puts("foo\n");
      pindent(indent); puts("field1:\n"); AST_print_list(indent, x->field1);
      pindent(indent); puts("field2:\n"); AST_print_list(indent, x->field2);
      break;
    }
*/
#include "AST_print.c"
    default:
      assert(0);
    }
}

static void AST_print_list(int indent, void *vn)
{
  node n = CAST(node, vn);

  while (n)
    {
      AST_print1(indent, n);
      n = n->next;
    }
}

void AST_print(node n)
{
  fflush(stdout);
  AST_print_list(0, n);
  fflush(stdout);
}

