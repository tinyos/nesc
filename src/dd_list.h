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

#ifndef DD_LIST_H
#define DD_LIST_H

/* 
 * Doubly-linked list storing arbitrary pointers.
 * The lists may homogeneous or heterogeneous at the discretion of
 * the user (the list stores no type indications)
 *
 * dd_list is the type of all lists
 * dd_list_pos is the type that represents a particular element of a list
 */

typedef struct dd_list *dd_list; /* A list */
typedef struct dd_list_pos	 /* A position in a list */
{
  /* PRIVATE! Do not use the fields directly */
  struct dd_list_pos *next;
  struct dd_list_pos *previous;
  void *data;
} *dd_list_pos;

dd_list dd_new_list(region r);
/* Returns: A new empty list
*/

void dd_add_first(region r, dd_list l, void *data);
/* Effects: Adds a new element containing `data' to the beginning of l.
   Modifies: l
*/
void dd_add_last(region r, dd_list l, void *data);
/* Effects: Adds a new element containing `data' to the end of l.
   Modifies: l
*/
void dd_insert_before(region r, dd_list_pos where, void *data);
/* Effects: Adds a new element containg `data' after element `where'.
   Modifies: the list containing `where'
*/
void dd_insert_after(region r, dd_list_pos where, void *data);
/* Effects: Adds a new element containg `data' before element `where'.
   Modifies: the list containing `where'
*/
void dd_remove(dd_list_pos what);
/* Effects: Removes element `what' from its list.
     No operations on what are valid after the call to dd_remove.
   Modifies: the list containing `what'.
*/

dd_list_pos dd_first(dd_list l);
/* Returns: The first element of list l
*/
dd_list_pos dd_last(dd_list l);
/* Returns: The last element of list l
*/
#define dd_is_beginning(l) (!(l)->previous)
/* Returns: TRUE if l is the pseudo-element at the beginning of a list.
   Note: dd_is_beginning(dd_previous(dd_first(l))) == TRUE
*/
#define dd_is_end(l) (!(l)->next)
/* Returns: TRUE if l is the pseudo-element at the end of a list.
   Note: dd_is_end(dd_next(dd_last(l))) == TRUE
*/
#define dd_next(l) ((l)->next)
/* Returns: The element after l, or the pseudo-element indicating the
     end of the list if none remain (see dd_is_end)
*/
#define dd_previous(l) ((l)->previous)
/* Returns: The element before l, or the pseudo-element indicating the
     beginning of the list if none remain (see dd_is_beginning)
*/

#define dd_is_empty(l) (dd_is_end(dd_first((l))))
/* Returns: TRUE if l is the empty list
*/

#define DD_GET(type, l) ((type)((l)->data))
/* Returns: The contents of element l, cast to type `type'
     This operation is obviously invalid on the pseudo-elements at
     the beginning and end of lists
*/
#define DD_SET(l, to) ((l)->data = (to))
/* Effects: Sets the contents of element l to `to'
     This operation is obviously invalid on the pseudo-elements at
     the beginning and end of lists
   Modifies: l
*/

#define dd_scan(var, list) for (var = dd_first((list)); !dd_is_end(var); var = dd_next(var))
/* Effects: Iterates variable `scan' over the contents of the list.
   Requires: list not ne modified during iteration.
   Example:
     length = 0;
     dd_scan (element, l)
       length++;
*/

unsigned long dd_length(dd_list l);
/* Returns: length of list l
*/

void dd_append(dd_list l1, dd_list l2);
/* Effects: Appends list l2 to the end of list l1.
     List l2 is destroyed
   Modifies: l1, l2
*/

dd_list dd_copy(region r, dd_list l);
/* Returns: A new list with the same elements as l
*/

void dd_free_list(dd_list l, void (*delete)(dd_list_pos p));
/* Effects: Destroys list l and all its elements.
     If delete is not NULL, calls it before deleting each element.
   Modifies: l
*/

dd_list_pos dd_find(dd_list l, void *find);
/* Returns: The element of l whose data is 'find', or NULL if it isn't found
*/

#endif
