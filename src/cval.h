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

#ifndef CVAL_H
#define CVAL_H

/* Note: legal ops on complex are:
   add, sub, times, divide, realpart, imagpart, conjugate
*/
typedef struct {
  enum {
    cval_variable,		    /* not a constant */
    cval_unk_number,		    /* some unknown number */
    cval_unk_address,		    /* an unknown symbol with unknown offset */
    cval_address_unk_offset,        /* known symbol with unknown offset */
    cval_address,		    /* symbol with offset */
    cval_float, cval_float_complex, /* a (complex) floating point number */
    cval_uint, cval_uint_complex,   /* a (complex) unsigned number */
    cval_sint, cval_sint_complex    /* a (complex) signed number */
  } kind;
#ifdef USE_UNNAMED_UNION
  union {
    struct {
      long double d, d_i; /* for cval_float */
    };
    struct {
      size_t isize;
      union {
	largest_int si;
	largest_uint ui;
      };
      union {
	largest_int si_i;
	largest_uint ui_i;
	struct { /* for cval_address, cval_address_unk_offset */
	  struct data_declaration *ddecl;
	  struct label_declaration *ldecl;
	};
      };
    };
  };
#else
  long double d, d_i; /* for cval_float */
  struct data_declaration *ddecl; /* for cval_address */
  struct label_declaration *ldecl;  /* for cval_address */
  largest_int si, si_i;
  largest_uint ui, ui_i;
  size_t isize;
#endif
} cval;

extern cval cval_top; /* The non-constant value */
extern cval cval_unknown_number; /* The unknown number value */
extern cval cval_unknown_address; /* The unknown address value */
extern cval cval_zero; /* A zero value. Use cval_cast to make the desired 
			  kind of constant */
extern cval cval_one; /* A one value. Use cval_cast to make the desired 
			  kind of constant */
extern cval cval_bitsperbyte; /* BITSPERBYTE, unsigned */

void cval_init(void);

cval make_cval_signed(largest_int i, type t);
cval make_cval_unsigned(largest_uint i, type t);

cval make_type_cval(size_t s);
/* Effects: Make a cval representing a type size. This is special-cased
     because we need to make these for type sizes before any types are
     available
   Returns: A cval representing s, with size set to the target's size_t size
*/

cval make_cval_float(long double d);
cval make_cval_complex(cval r, cval i);
cval make_cval_address(data_declaration ddecl, label_declaration ldecl,
		       largest_int offset);

cval make_cval_address_unknown_offset(cval c);
/* Requires: cval_isaddress(c)
   Returns: a constant identical to c except that the offset is now unknown
*/

bool cval_isunknown(cval c);
/* Return: TRUE if c is an unknown constant
   (one of unknown_number, unknown_address, address_unknown_offset) */

/*bool cval_isunknown_number(cval c);*/
#define cval_isunknown_number(c) ((c).kind == cval_unk_number)
/* Return: TRUE if c is an unknown constant */

/*bool cval_isunknown_address(cval c);*/
#define cval_isunknown_address(c) ((c).kind == cval_unk_address)
/* Return: TRUE if c is an unknown constant */

/*bool cval_istop(cval c);*/
#define cval_istop(c) ((c).kind == cval_variable)
/* Return: TRUE if c is not a constant */

bool cval_isaddress(cval c);
/* Return: TRUE if c is an address constant (known or unknown) */

bool cval_isinteger(cval c);
/* Return: TRUE if c is an integer constant */

bool cval_isunsigned(cval c);
/* Return: TRUE if c is an unsigned integer constant */

bool cval_isfloating(cval c);
/* Return: TRUE if c is a floating-point constant */

bool cval_iscomplex(cval c);
/* Return: TRUE if c is a complex constant */

bool cval_knownbool(cval c);
/* Returns: TRUE if the truth-value of c can be determined (use cval_boolvalue
     to get that value). Note that address values with offset 0 are known to
     be true...
*/

bool cval_boolvalue(cval c);
/* Returns: TRUE if c is a non-zero constant
   Requires: cval_knownbool(c)
 */

bool cval_knownvalue(cval c);
/* Returns: TRUE if the value of c can be determined (this is false for
   address constants, while cval_knownbool is true for address constants
   with offset 0)
*/

largest_uint cval_uint_value(cval c);
/* Returns: The value of c as an unsigned int
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/

largest_int cval_sint_value(cval c);
/* Returns: The value of c as a signed int
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/

long double cval_float_value(cval c);
/* Returns: The value of c as a long double
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/

bool cval_isone(cval c);
/* Returns: TRUE if c is 1 (FALSE for unknown constants)
   Requires: c not be cval_top
 */

data_declaration cval_ddecl(cval c);
/* Returns: c's declaration, or NULL if c denotes a label
   Requires: cval_isaddress(c)  && !cval_isunknown_address(c)
*/

label_declaration cval_ldecl(cval c);
/* Returns: c's declaration, or NULL if c denotes a variable
   Requires: cval_isaddress(c)  && !cval_isunknown_address(c)
*/

/* All of these functions will return cval_top if the result is not a
   constant expression */

cval cval_cast(cval c, type to); /* Cast c to type to */
cval cval_not(cval c); /* !c */
cval cval_negate(cval c); /* -c */
cval cval_bitnot(cval c); /* ~c */
cval cval_conjugate(cval c); /* ~c */
cval cval_realpart(cval c); /* __real__ c */
cval cval_imagpart(cval c); /* __imag__ c */

/* The binary operators require that both arguments have been cast to a common
   type. */
cval cval_add(cval c1, cval c2);
cval cval_sub(cval c1, cval c2);
cval cval_times(cval c1, cval c2);
cval cval_divide(cval c1, cval c2);
cval cval_modulo(cval c1, cval c2);
cval cval_lshift(cval c1, cval c2);
cval cval_rshift(cval c1, cval c2);
cval cval_bitand(cval c1, cval c2);
cval cval_bitor(cval c1, cval c2);
cval cval_bitxor(cval c1, cval c2);
cval cval_eq(cval c1, cval c2);
/*cval cval_ne(cval c1, cval c2);*/
cval cval_leq(cval c1, cval c2);
/*cval cval_lt(cval c1, cval c2);*/
/*cval cval_geq(cval c1, cval c2);*/
/*cval cval_gt(cval c1, cval c2);*/
#define cval_gt(c1, c2) (cval_not(cval_leq((c1), (c2))))
#define cval_lt(c1, c2) (cval_gt((c2), (c1)))
#define cval_geq(c1, c2) (cval_leq((c2), (c1)))
#define cval_ne(c1, c2) (cval_not(cval_eq((c1), (c2))))

/* True if x fits in the range of type t */
bool uint_inrange(largest_uint x, type t);
bool sint_inrange(largest_int x, type t);

bool cval_inrange(cval c, type t);
/* Requires: constant_integral(c)
   Returns: TRUE if c is in range of type t. */

largest_int cval_intcompare(cval c1, cval c2);
/* Requires: cval_isinteger(c1) && cval_isinteger(c2)
   Returns: x, x<0 if c1 < c2, x = 0 if c1 = c2 and x > 0 if c1 > c2
*/

void cval_print(FILE *f, cval c);
/* Requires: cval_knownvalue(c)
   Effects: prints a parsable representable of c to f
 */

/* Utility functions (used for struct layout) */
cval cval_lcm(cval x, cval y);
cval cval_gcd(cval x, cval y);
cval cval_align_to(cval n, cval alignment);
cval cval_max(cval c1, cval c2);
cval cval_min(cval c1, cval c2);

#endif
