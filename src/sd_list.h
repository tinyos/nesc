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

#ifndef SD_LIST_H
#define SD_LIST_H

/* static doubly-linked list */

typedef struct sd_list *sd_list;
typedef struct sd_list_pos 
{
  struct sd_list_pos *next;
  struct sd_list_pos *previous;
} *sd_list_pos;

sd_list sd_new_list(region r);
void sd_del_list(sd_list l); /* XXX: a noop */

void sd_add_first(sd_list l, sd_list_pos data);
void sd_add_last(sd_list l, sd_list_pos data);
void sd_insert_before(sd_list_pos where, sd_list_pos data);
void sd_insert_after(sd_list_pos where, sd_list_pos data);
void sd_remove(sd_list_pos what);

sd_list_pos sd_first(sd_list l);
sd_list_pos sd_last(sd_list l);

#define sd_is_beginning(l) (!(l)->previous)
#define sd_is_end(l) (!(l)->next)
#define sd_next(l) ((l)->next)
#define sd_previous(l) ((l)->previous)

#define SD_GET(type, where) ((type)(where))

#define sd_scan(var, list) for (var = sd_first((list)); !sd_is_end(var); var = sd_next(var))

unsigned long sd_length(sd_list l);

#endif
