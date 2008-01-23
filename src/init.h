/* Initialiser handling.
   This file is part of the nesC compiler.

This file is derived from the GNU C Compiler. It is thus
   Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
Changes for nesC are
   Copyright (C) 2002, 2003 Intel Corporation

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

#ifndef INIT_H
#define INIT_H

/* Types representing a parsed initialiser. Unspecified fields and
   array elements were unspecified in the initialiser. */
struct ivalue {
  enum { iv_base, iv_array, iv_structured } kind;

  type type;

  ivalue instantiation;

  union {
    struct {			/* for iv_base */
      expression expr;		/* not an init_list */
      bool require_constant_value;
      cval value; 		/* value if constant, cval_top otherwise */
    } base;
    struct ivalue_array *array; /* for iv_array */
    struct ivalue_field *structured; /* for iv_structured */
  } u;
};

typedef struct ivalue_array {
  struct ivalue_array *next;
  largest_int from, to;
  ivalue value;
} *ivalue_array;

typedef struct ivalue_field {
  struct ivalue_field *next;
  field_declaration field;
  ivalue value;
} *ivalue_field;

ivalue new_ivalue(region r, int kind, type t);

void start_init(declaration decl, nesc_attribute attr);
void finish_init(void);
void simple_init(expression expr);
void really_start_incremental_init(type t);
void push_init_level(int implicit);
designator set_init_index(location loc, expression first, expression last);
designator set_init_label(location loc, cstring fieldname);
void process_init_element(expression value);

expression make_init_specific(designator dlist, expression initval);
expression make_init_list(location loc, expression elist);
expression make_cast_list(location loc, asttype t, expression init);

void check_init_element(expression init);

#endif
