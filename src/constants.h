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

/* Constants come in three kinds:
   - "unknown": value is completely unknown (constant_unknown)
   - "address": value is the address of some global symbol and an offset 
                (constant_address)
   - "value": known value (integer, floating point)
*/
#ifndef CONSTANTS_H
#define CONSTANTS_H

struct known_cst {
  type type;
  cval cval;
};

known_cst make_unknown_cst(cval c, type t);
known_cst make_cst(cval c, type t);
known_cst make_address_cst(data_declaration ddecl, label_declaration ldecl,
			   largest_int offset, type t);
known_cst make_signed_cst(largest_int x, type t);
known_cst make_unsigned_cst(largest_uint x, type t);

known_cst cast_constant(known_cst c, type to);

lexical_cst fold_lexical_int(type itype, location loc, cstring tok,
			     bool iscomplex, largest_uint intvalue, bool overflow);
lexical_cst fold_lexical_real(type realtype, location loc, cstring tok);
/* XXX: What's the right type for charvalue ? (must hold wchar_t or int) */
lexical_cst fold_lexical_char(location loc, cstring tok,
			      bool wide_flag, int charvalue);
string fold_lexical_string(location loc, string_cst components, cstring value,
			   bool wide_flag);

known_cst fold_label_address(expression e);
known_cst fold_sizeof(expression e, type stype);
known_cst fold_alignof(expression e, type atype);
known_cst fold_cast(expression e);
known_cst fold_unary(expression e);
known_cst fold_binary(type restype, expression e);
known_cst fold_conditional(expression e);
known_cst fold_function_call(expression e, int pass);
known_cst fold_identifier(expression e, data_declaration decl, int pass);

known_cst fold_add(type restype, known_cst c1, known_cst c2);

known_cst foldaddress_identifier(expression e, data_declaration decl);
known_cst foldaddress_string(string s);
known_cst foldaddress_field_ref(expression e);

bool definite_null(expression e);
bool definite_zero(expression e);
bool is_zero_constant(known_cst c);

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */
void constant_overflow_warning(known_cst c);

/*bool constant_address(known_cst c);*/
#define constant_address(c) cval_isaddress((c)->cval)
/* Returns: TRUE if c is an address constant
 */

/*bool constant_unknown(known_cst c);*/
#define constant_unknown(c) cval_isunknown((c)->cval)
/* Returns: TRUE if c is a constant whose value is not yet known
 */

/*bool constant_unknown_number(known_cst c);*/
#define constant_unknown_number(c) cval_isunknown_number((c)->cval)
/* Returns: TRUE if c is a numeric constant whose value is not yet known
 */

/*bool constant_integral(known_cst c);*/
#define constant_integral(c) cval_isinteger((c)->cval)
/* Returns: TRUE if c is an integer constant (signed or unsigned)
*/

/*bool constant_float(known_cst c);*/
#define constant_float(c) (cval_isfloating((c)->cval))
/* Returns: TRUE if c is a floating-point constant
*/

/*largest_uint constant_uint_value(known_cst c);*/
#define constant_uint_value(c) cval_uint_value((c)->cval)
/* Returns: Value of c as an unsigned integer.
   Requires: see cval_uint_value
*/

/*largest_int constant_sint_value(known_cst c);*/
#define constant_sint_value(c) cval_sint_value((c)->cval)
/* Returns: Value of c as an unsigned integer.
   Requires: see cval_sint_value
*/

/*long double constant_float_value(known_cst c);*/
#define constant_float_value(c) cval_float_value((c)->cval)
/* Returns: Value of c as an unsigned integer.
   Requires: see cval_float_value
*/

/*bool constant_knownbool(known_cst c);*/
/* Returns: TRUE if the truth-value of c can be determined (use 
   constant_boolvalue to get that value)
*/
#define constant_knownbool(c) cval_knownbool((c)->cval)

/*bool cval_boolvalue(cval c);*/
/* Returns: TRUE if c is a non-zero constant
   Requires: cval_knownbool(c)
 */
#define constant_boolvalue(c) cval_boolvalue((c)->cval)

typedef enum {
  cst_any,
  cst_numerical,
  cst_address
} cst_kind;

bool check_constant_once(expression e, cst_kind k);
/* Effects: We want to check whether e is a constant, and possibly for
     valid constant values, exactly once (to avoid repeated errors and
     warnings) over our multiple constant folding passes. Additionally,
     we can't check unknown constants until their value is known. We can
     rely on the following:
     - a non-constant will not become constant
     - we assume, for checking purposes, that a constant's kind (numerical
       vs address) will not change (this in some sense untrue, as in:
         <unknown> ? <numerical> : <address>
       but we treat that as <address> for checking purposes)
     - a known constant will maintain its value
     - an unknown constant will become either non-constant or a known constant

     Additionally, if the constant kind does not match k, we can check it
     immediately (presumably to report some error).
     
     check_constant_once supports this by returning TRUE exactly once, when
     its possible to check e's value

   Returns: TRUE the first time !e->cst || e->cst && !constant_unkown(e) ||
     e->cst && e->cst does not match k
*/

#endif
