/* This file is part of the nesC compiler.

This file is derived from RC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
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

#ifndef EXPR_H
#define EXPR_H

expression make_error_expr(void);

expression make_comma(location loc, expression elist);
expression make_dereference(location loc, expression e);
expression make_extension_expr(location loc, expression e);
expression make_unary(location loc, int unop, expression e);
expression make_label_address(location loc, id_label label);
expression make_sizeof_expr(location loc, expression e);
expression make_sizeof_type(location loc, asttype t);
expression make_alignof_expr(location loc, expression e);
expression make_alignof_type(location loc, asttype t);
expression make_cast(location loc, asttype t, expression e);
expression make_binary(location loc, int binop, expression e1, expression e2);
expression make_conditional(location loc, expression cond,
			    expression true, expression false);
expression make_assign(location loc, int assignop, expression left, expression right);
expression make_identifier(location loc, cstring id, bool maybe_implicit);
expression make_compound_expr(location loc, statement block);
expression make_function_call(location loc, expression fn, expression arglist);
expression make_va_arg(location loc, expression arg, asttype type);
expression make_offsetof(location loc, asttype t, dd_list fields);
expression make_array_ref(location loc, expression array, expression index);
expression make_field_ref(location loc, expression object, cstring field);
expression make_field_indirectref(location loc, expression object,
				  cstring field);
expression make_postincrement(location loc, expression e);
expression make_preincrement(location loc, expression e);
expression make_postdecrement(location loc, expression e);
expression make_predecrement(location loc, expression e);

/* Return TRUE if no error and lhstype and rhstype are not error_type */
bool check_assignment(type lhstype, type rhstype, expression rhs,
		      const char *context, data_declaration fundecl,
		      int parmnum);

bool check_conversion(type to, type from);
bool check_arguments(type fntype, expression arglist,
		     data_declaration fundecl, bool generic_call);

type default_conversion(expression e);
type default_function_array_conversion(expression e);

/* called default_function_array_conversion in gcc 3.x */
type default_conversion_for_assignment(expression e);

#endif
