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
#include "dd_list.h"

/* This is a hack: the list header holds the two pseudo-elements for the
   beginning & end of a list: the `beginning' pseudo element has:
     next = first element
     previous = NULL
     data = xxx
   the `end' pseudo element has:
     next = NULL
     previous = last element
     data = xxx

   if l is a dd_list, the `beginning' element is (dd_list_pos *)l
   and the `end' is (dd_list_pos *)&l->null

   l is initialised to make all this work ...
   Source: AmigaOS Exec kernel
*/
struct dd_list
{
  struct dd_list_pos *first;
  struct dd_list_pos *null;
  struct dd_list_pos *last;
};

static dd_list_pos new_hdr(region r, void *data, dd_list_pos previous, dd_list_pos next)
/* Returns: A new list element containing `data', with previous & next
    initialised
*/
{
  dd_list_pos new = ralloc(r, struct dd_list_pos);

  new->data = data;
  new->previous = previous;
  new->next = next;

  return new;
}

dd_list dd_new_list(region r)
/* Returns: A new empty list
*/
{
  dd_list new = ralloc(r, struct dd_list);

  /* Initialise `beginning' and `end' elements (see comment at top) */
  new->first = (dd_list_pos)&new->null;
  new->null = NULL;
  new->last = (dd_list_pos)new;

  return new;
}

void dd_add_first(region r, dd_list l, void *data)
/* Effects: Adds a new element containing `data' to the beginning of l.
   Modifies: l
*/
{
  dd_insert_after(r, (dd_list_pos)l, data);
}

void dd_add_last(region r, dd_list l, void *data)
{
  dd_insert_before(r, (dd_list_pos)&l->null, data);
}

void dd_insert_before(region r, dd_list_pos where, void *data)
/* Effects: Adds a new element containg `data' after element `where'.
   Modifies: the list containing `where'
*/
{
  dd_list_pos new = new_hdr(r, data, where->previous, where);

  where->previous->next = new;
  where->previous = new;
}

void dd_insert_after(region r, dd_list_pos where, void *data)
/* Effects: Adds a new element containg `data' before element `where'.
   Modifies: the list containing `where'
*/
{
  dd_list_pos new = new_hdr(r, data, where, where->next);

  where->next->previous = new;
  where->next = new;
}

void dd_remove(dd_list_pos what)
/* Effects: Removes element `what' from its list.
     No operations on what are valid after the call to dd_remove.
   Modifies: the list containing `what'.
*/
{
  what->previous->next = what->next;
  what->next->previous = what->previous;

  what->next = what->previous = NULL;
  what->data = NULL;
}

dd_list_pos dd_first(dd_list l)
/* Returns: The first element of list l
*/
{
  return l->first;
}

dd_list_pos dd_last(dd_list l)
/* Returns: The last element of list l
*/
{
  return l->last;
}

unsigned long dd_length(dd_list l)
/* Returns: length of list l
*/
{
  dd_list_pos scan;
  unsigned long len = 0;

  dd_scan (scan, l) len++;

  return len;
}

void dd_append(dd_list l1, dd_list l2)
/* Effects: Appends list l2 to the end of list l1.
     List l2 is destroyed
   Modifies: l1, l2
*/
{
  l1->last->next = l2->first;
  l2->first->previous = l1->last;
  l2->last->next = (dd_list_pos)&l1->null;
  l1->last = l2->last;

  l2->first = l2->last = NULL;
}

dd_list dd_copy(region r, dd_list l)
/* Returns: A new list with the same elements as l
*/
{
  dd_list new = dd_new_list(r);
  dd_list_pos scan;
  
  dd_scan (scan, l) dd_add_last(r, new, scan->data);

  return new;
}

void dd_free_list(dd_list l, void (*delete)(dd_list_pos p))
/* Effects: Destroys list l and all its elements.
     If delete is not NULL, calls it before deleting each element.
*/
{
  dd_list_pos scan, next;

  scan = dd_first(l);
  while (!dd_is_end(scan))
    {
      next = dd_next(scan);
      if (delete) delete(next);

      scan->next = scan->previous = NULL;
      scan->data = NULL;

      scan = next;
    }
  l->first = l->last = NULL;
}

dd_list_pos dd_find(dd_list l, void *find)
/* Returns: The element of l whose data is 'find', or NULL if it isn't found
*/
{
  dd_list_pos scan;

  dd_scan (scan, l)
    if (DD_GET(void *, scan) == find) return scan;

  return NULL;
}
