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
#include "sd_list.h"

/* static doubly-linked list */

struct sd_list
{
  struct sd_list_pos *first;
  struct sd_list_pos *null;
  struct sd_list_pos *last;
};

sd_list sd_new_list(region r)
{
  sd_list new = ralloc(r, struct sd_list);

  new->first = (sd_list_pos)&new->null;
  new->null = NULL;
  new->last = (sd_list_pos)new;

  return new;
}

void sd_del_list(sd_list l)
{
}

void sd_add_first(sd_list l, sd_list_pos data)
{
  sd_insert_after((sd_list_pos)l, data);
}

void sd_add_last(sd_list l, sd_list_pos data)
{
  sd_insert_before((sd_list_pos)&l->null, data);
}

void sd_insert_before(sd_list_pos where, sd_list_pos data)
{
  data->previous = where->previous;
  data->next = where;
  where->previous->next = data;
  where->previous = data;
}

void sd_insert_after(sd_list_pos where, sd_list_pos data)
{
  data->previous = where;
  data->next = where->next;
  where->next->previous = data;
  where->next = data;
}

void sd_remove(sd_list_pos what)
{
  what->previous->next = what->next;
  what->next->previous = what->previous;
}

sd_list_pos sd_first(sd_list l)
{
  return l->first;
}

sd_list_pos sd_last(sd_list l)
{
  return l->last;
}

unsigned long sd_length(sd_list l)
{
  sd_list_pos scan;
  unsigned long len = 0;

  sd_scan (scan, l) len++;

  return len;
}
