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

/* SAME: There's a lot of assumptions about floating point here, and behaviour
   of operations when performed on wider types. I believe them to be valid for
   IEEE with round-to-nearest. XXX: check.
   XXX: overflow.
*/
/* Note: this code is not particularly aiming for efficiency (in particular,
   complex constants will be inefficient) */
#include "parser.h"
#include "cval.h"
#include "machine.h"

cval cval_top; /* The non-constant value */
cval cval_unknown_number; /* The unknown number value */
cval cval_unknown_address; /* The unknown address value */
cval cval_zero; /* A zero value. Use cval_cast to make the desired kind of
		   constant */
cval cval_one; /* A one value. Use cval_cast to make the desired kind of
		   constant */
cval cval_bitsperbyte; /* BITSPERBYTE, unsigned */

/* We use cval_invalid_address to mark those places where a constant
   is computed which is "not computable at load time"
   (these used to be cval_unknown_number, but we're reusing that for constants
   built from abstract component args. Knowing that something is a
   constant "not computable at load time" seems to have no use, except
   in producing a slightly nicer error message) 
*/
#define cval_invalid_address cval_top

void cval_init(void)
{
  /* Code will be unhappy if this is not true. */
  assert(sizeof(largest_int) == sizeof(largest_uint));
  /* assert(This is a 2's complement machine); */

  cval_top.kind = cval_variable;
  cval_unknown_number.kind = cval_unk_number;
  cval_unknown_address.kind = cval_unk_address;
  cval_zero.kind = cval_sint;
  cval_zero.si = 0;
  cval_zero.isize = target->tint.size;
  cval_one.kind = cval_sint;
  cval_one.si = 1;
  cval_one.isize = target->tint.size;
  cval_bitsperbyte.kind = cval_uint;
  cval_bitsperbyte.ui = BITSPERBYTE;
  cval_bitsperbyte.isize = target->size_t_size;
}

cval make_cval_unsigned(largest_uint i, type t)
{
  cval c;

  assert(type_integral(t) && type_unsigned(t));
  c.kind = cval_uint;
  c.ui = i;
  c.isize = type_size_int(t);
  return c;
}

cval make_cval_signed(largest_int i, type t)
{
  cval c;

  assert(type_integral(t) && !type_unsigned(t));
  c.kind = cval_sint;
  c.si = i;
  c.isize = type_size_int(t);
  return c;
}

cval make_type_cval(size_t s)
/* Effects: Make a cval representing a type size. This is special-cased
     because we need to make these for type sizes before any types are
     available
   Returns: A cval representing s, with size set to the target's size_t size
*/
{
  cval c;

  c.kind = cval_uint;
  c.ui = s;
  c.isize = target->size_t_size;

  return c;
}

cval make_cval_float(long double d)
{
  cval c;
  c.kind = cval_float;
  c.d = d;
  return c;
}

cval make_cval_complex(cval r, cval i)
{
  assert(r.kind == i.kind);

  switch (r.kind)
    {
    case cval_float:
      r.d_i = i.d;
      r.kind = cval_float_complex;
      return r;
    case cval_uint:
      assert(r.isize == i.isize);
      r.kind = cval_uint_complex;
      r.ui_i = i.ui;
      return r;
    case cval_sint:
      assert(r.isize == i.isize);
      r.kind = cval_sint_complex;
      r.si_i = i.si;
      return r;

    default: abort(); return r;
    }
}

cval make_cval_address(data_declaration ddecl, label_declaration ldecl,
		       largest_int offset)
{
  cval c = make_cval_signed(offset, ptrdiff_t_type);

  assert(!(ldecl && ddecl));
  c.kind = cval_address;
  c.ddecl = ddecl;
  c.ldecl = ldecl;

  return c;
}

cval make_cval_address_unknown_offset(cval c)
/* Requires: cval_isaddress(c)
   Returns: a constant identical to c except that the offset is now unknowjn
*/
{
  assert(cval_isaddress(c));
  if (c.kind == cval_address)
    c.kind = cval_address_unk_offset;

  return c;
}

bool cval_isunknown(cval c)
/* Return: TRUE if c is an unknown constant */
{
  return c.kind == cval_unk_number || c.kind == cval_unk_address;
}

bool cval_isaddress(cval c)
{
  return c.kind == cval_unk_address || c.kind == cval_address ||
    c.kind == cval_address_unk_offset;
}

bool cval_isinteger(cval c)
{
  return c.kind == cval_sint || c.kind == cval_uint;
}

bool cval_isunsigned(cval c)
{
  return c.kind == cval_uint;
}

bool cval_isfloating(cval c)
{
  return c.kind == cval_float;
}

bool cval_iscomplex(cval c)
{
  return c.kind == cval_sint_complex || c.kind == cval_uint_complex ||
    c.kind == cval_float_complex;
}

bool cval_knownbool(cval c)
/* Returns: TRUE if the truth-value of c can be determined (use cval_boolvalue
   to get that value)
*/
{
  switch (c.kind) {
  default: case cval_variable: assert(0);
  case cval_unk_number: case cval_unk_address: case cval_address_unk_offset:
    return FALSE;
  case cval_address: return c.si == 0;
  case cval_uint: case cval_sint: case cval_float:
  case cval_uint_complex: case cval_sint_complex: case cval_float_complex:
    return TRUE;
  }
}

bool cval_boolvalue(cval c)
/* Returns: TRUE if c is a non-zero constant
   Requires: cval_knownbool(c)
 */
{
  switch (c.kind) {
  default: assert(0);
  case cval_address: assert(c.si == 0); return TRUE;
  case cval_uint: return c.ui != 0;
  case cval_sint: return c.si != 0;
  case cval_float: return c.d != 0;
  case cval_uint_complex: return c.ui && c.ui_i;
  case cval_sint_complex: return c.si && c.si_i;
  case cval_float_complex: return c.d && c.d_i;
  }
}

bool cval_knownvalue(cval c)
/* Returns: TRUE if the value of c can be determined (this is false for
   address constants, while cval_knownbool is true for address constants
   with offset 0)
*/
{
  switch (c.kind) {
  default: case cval_variable: assert(0);
  case cval_unk_number: case cval_unk_address: case cval_address_unk_offset:
  case cval_address: return FALSE;
  case cval_uint: case cval_sint: case cval_float:
  case cval_uint_complex: case cval_sint_complex: case cval_float_complex:
    return TRUE;
  }
}

largest_uint cval_uint_value(cval c)
/* Returns: The value of c as an unsigned int
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/
{
  switch (c.kind) {
  default: assert(0); return 0;
  case cval_uint: return c.ui;
  case cval_sint: return c.si;
  case cval_float: return c.d;
  }
}

largest_int cval_sint_value(cval c)
/* Returns: The value of c as a signed int
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/
{
  switch (c.kind) {
  default: assert(0); return 0;
  case cval_uint: return c.ui;    
  case cval_sint: return c.si;
  case cval_float: return c.d;
  }
}

long double cval_float_value(cval c)
/* Returns: The value of c as a long double
   Requires: cval_knownvalue(c) && !cval_iscomplex(c)
*/
{
  switch (c.kind) {
  default: assert(0); return 0;
  case cval_uint: return c.ui;    
  case cval_sint: return c.si;
  case cval_float: return c.d;
  }
}

bool cval_isone(cval c)
/* Returns: TRUE if c is 1 (FALSE for unknown constants)
   Requires: c not be cval_top
 */
{
  switch (c.kind) {
  default: case cval_variable: assert(0);
  case cval_unk_number: case cval_unk_address: case cval_address_unk_offset:
  case cval_address: return FALSE;
  case cval_uint: return c.ui == 1;
  case cval_sint: return c.si == 1;
  case cval_float: return c.d == 1;
  case cval_uint_complex: return c.ui == 1 && c.ui_i == 0;
  case cval_sint_complex: return c.si == 1 && c.si_i == 0;
  case cval_float_complex: return c.d == 1 && c.d_i == 0;
  }
}

static largest_uint truncate_unsigned(largest_uint x, size_t tsize)
{
  /* The shift is undefined for the size of largest_uint (and the masking is then
     a no-op) */
  assert(tsize <= sizeof(largest_uint));

  if (tsize == sizeof(largest_uint))
    return x;
  else
    return x & (((largest_uint)1 << BITSPERBYTE * tsize) - 1);
}

/* SAME: 2's complement */
static largest_int truncate_signed(largest_int x, size_t tsize)
{
  largest_uint umask, uval;

  assert(tsize <= sizeof(largest_int));

  if (tsize == sizeof(largest_int))
    return x;

  /* masking */
  umask = (((largest_uint)1 << BITSPERBYTE * tsize) - 1);
  uval = x & umask;
  /* sign extension */
  if (uval & ((largest_uint)1 << (BITSPERBYTE * tsize - 1)))
    return (largest_int)(((largest_uint)-1 & ~umask) | uval);
  else
    return uval;
}

/* All of these functions will return cval_top if the result is not a
   constant expression */

cval cval_cast(cval c, type to)
/* Returns: c cast to type to
   Requires: type_scalar(to)
*/
{
  if (cval_istop(c))
    return cval_top;

  if (cval_isunknown_number(c))
    return cval_unknown_number;

  if (type_complex(to))
    {
      type base = make_base_type(to);

      switch (c.kind)
	{
	case cval_unk_address: case cval_address_unk_offset: case cval_address:
	  return cval_top;
	case cval_sint: case cval_uint: case cval_float:
	  return make_cval_complex(cval_cast(c, base),
				   cval_cast(cval_zero, base));
	  return c;
	case cval_sint_complex: case cval_uint_complex: case cval_float_complex:
	  return make_cval_complex(cval_cast(cval_realpart(c), base),
				   cval_cast(cval_imagpart(c), base));
	default:assert(0); return c;
	}
    }

  if (cval_iscomplex(c))
    return cval_cast(cval_realpart(c), to);

  if (type_floating(to))
    {
      switch (c.kind)
	{
	case cval_unk_address: case cval_address_unk_offset: case cval_address:
	  return cval_top; /* And not cval_invalid_address for some reason */
	case cval_sint: case cval_uint:
	  c.kind = cval_float;
	  /* Note that the cast is necessary otherwise it would cast to the common
	     type of largest_int/largest_uint (largest_uint), so c.si would be
	     cast to unsigned. */
	  c.d = c.kind == cval_sint ? (long double)c.si : (long double)c.ui;
	  return c;
	case cval_float:
	  if (type_float(to))
	    c.d = (float)c.d;
	  else if (type_double(to))
	    c.d = (double)c.d;
	  return c;
	default:assert(0); return c;
	}
    }
  else
    {
      cval tosize_cval = type_size(to);
      size_t tosize;

      // Cast to int of unknown size produces unknown value
      if (cval_isunknown_number(tosize_cval))
	switch (c.kind)
	  {
	  case cval_unk_address: case cval_address_unk_offset:
	  case cval_address:
	    return cval_unknown_address;
	  default:
	    return cval_unknown_number;
	  }

      tosize = cval_uint_value(tosize_cval);
      switch (c.kind)
	{
	case cval_float:
	  /* If it's floating, make it an integer */
	  /* Note: can't cast floating point number to a pointer */
	  assert(!type_pointer(to));
	  if (type_unsigned(to))
	    {
	      c.kind = cval_uint;
	      c.ui = c.d;
	      c.isize = tosize;
	    }
	  else
	    {
	      c.kind = cval_sint;
	      c.si = c.d;
	      c.isize = tosize;
	    }
	  return c;

	case cval_unk_address: case cval_address_unk_offset: case cval_address:
	  /* Lose value if cast address of symbol to too-narrow a type */
	  if (!type_array(to) && tosize < type_size_int(intptr_type))
	    return cval_invalid_address;
	  /* Otherwise nothing happens (the offset is already restricted to
	     the range of intptr_type). */
	  return c;

	case cval_uint: case cval_sint:
	  c.isize = tosize;
	  if (type_unsigned(to) || type_pointer(to))
	    {
	      if (c.kind == cval_sint)
		c.ui = c.si;
	      c.ui = truncate_unsigned(c.ui, tosize);
	      c.kind = cval_uint;
	    }
	  else
	    {
	      if (c.kind == cval_uint)
		c.si = c.ui;
	      c.si = truncate_signed(c.si, tosize);
	      c.kind = cval_sint;
	    }
	  return c;

	default: assert(0); return c;
	}
    }
}

cval cval_not(cval c)
{
  if (cval_istop(c))
    return cval_top;
  else if (cval_isunknown(c))
    return cval_unknown_number;
  else if (!cval_knownbool(c))
    return cval_invalid_address;
  else
    return make_cval_signed(!cval_boolvalue(c), int_type);
}

cval cval_negate(cval c)
{
  switch (c.kind)
    {
    case cval_variable: return cval_top;
    case cval_unk_number: return cval_unknown_number;
    case cval_address: case cval_unk_address: case cval_address_unk_offset:
      return cval_invalid_address;
    case cval_sint: c.si = -c.si; return c; /* XXX: overflow */
    case cval_uint: c.ui = truncate_unsigned(-c.ui, c.isize); return c;
    case cval_float: c.d = -c.d; return c;
    case cval_sint_complex:
      c.si = -c.si;
      c.si_i = -c.si_i; 
      return c; /* XXX: overflow */
    case cval_uint_complex:
      c.ui = truncate_unsigned(-c.ui, c.isize);
      c.ui_i = truncate_unsigned(-c.ui_i, c.isize);
      return c;
    case cval_float_complex: c.d = -c.d; c.d_i = -c.d_i; return c;
    default: abort(); return cval_top;
    }
}

cval cval_bitnot(cval c)
{
  switch (c.kind)
    {
    case cval_variable: return cval_top;
    case cval_unk_number: return cval_unknown_number;
    case cval_address: case cval_unk_address: case cval_address_unk_offset:
      return cval_invalid_address;
    case cval_sint: c.si = truncate_signed(~c.si, c.isize); return c;
    case cval_uint: c.ui = truncate_unsigned(~c.ui, c.isize); return c;
    default: abort(); return cval_top;
    }
}

cval cval_conjugate(cval c)
{
  switch (c.kind)
    {
    case cval_variable: return cval_top;
    case cval_unk_number: return cval_unknown_number;
    case cval_sint_complex:
      c.si_i = -c.si_i; 
      return c; /* XXX: overflow */
    case cval_uint_complex:
      c.ui_i = truncate_unsigned(-c.ui_i, c.isize);
      return c;
    case cval_float_complex: c.d_i = -c.d_i; return c;
    default: abort(); return cval_top;
    }
}

cval cval_realpart(cval c)
{
  switch (c.kind)
    {
    case cval_variable: return cval_top;
    case cval_unk_number: return cval_unknown_number;
    case cval_sint_complex: c.kind = cval_sint; return c;
    case cval_uint_complex: c.kind = cval_uint; return c;
    case cval_float_complex: c.kind = cval_float; return c;
    case cval_sint: case cval_uint: case cval_float: return c;
    default: abort(); return cval_top;
    }
}

cval cval_imagpart(cval c)
{
  switch (c.kind)
    {
    case cval_variable: return cval_top;
    case cval_unk_number: return cval_unknown_number;
    case cval_sint_complex: c.kind = cval_sint; c.si = c.si_i; return c;
    case cval_uint_complex: c.kind = cval_uint; c.ui = c.ui_i; return c;
    case cval_float_complex: c.kind = cval_float; c.d = c.d_i; return c;
    case cval_sint: c.si = 0; return c;
    case cval_uint: c.ui = 0; return c;
    case cval_float: c.d = 0; return c;
    default: abort(); return cval_top;
    }
}

/* The binary operators require that both arguments have been cast to a common
   type. */
cval cval_add(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  if (cval_isaddress(c2))
    {
      cval tmp = c1; c1 = c2; c2 = tmp;
    }

  if (cval_isaddress(c1))
    switch (c2.kind)
      {
      case cval_unk_number: return make_cval_address_unknown_offset(c1);
      case cval_address: case cval_unk_address: case cval_address_unk_offset:
	return cval_invalid_address;
      case cval_sint: c1.si = truncate_signed(c1.si + c2.si, c1.isize); return c1;
      case cval_uint: c1.si = truncate_signed(c1.si + c2.ui, c1.isize); return c1;
      default: assert(0); return c1;
      }

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  switch (c1.kind)
    {
    case cval_float:
      assert(c2.kind == cval_float);
      c1.d += c2.d;
      return c1;

    case cval_sint:
      assert(c2.kind == cval_sint && c1.isize == c2.isize);
      c1.si = truncate_signed(c1.si + c2.si, c1.isize);
      return c1;
      
    case cval_uint:
      assert(c2.kind == cval_uint && c1.isize == c2.isize);
      c1.ui = truncate_unsigned(c1.ui + c2.ui, c1.isize);
      return c1;
      
    case cval_float_complex:
      assert(c2.kind == cval_float_complex);
      c1.d += c2.d;
      c1.d_i += c2.d_i;
      return c1;

    case cval_sint_complex:
      assert(c2.kind == cval_sint_complex && c1.isize == c2.isize);
      c1.si = truncate_signed(c1.si + c2.si, c1.isize);
      c1.si_i = truncate_signed(c1.si_i + c2.si_i, c1.isize);
      return c1;
      
    case cval_uint_complex:
      assert(c2.kind == cval_uint_complex && c1.isize == c2.isize);
      c1.ui = truncate_unsigned(c1.ui + c2.ui, c1.isize);
      c1.ui_i = truncate_unsigned(c1.ui_i + c2.ui_i, c1.isize);
      return c1;
      
    default:
      assert(0);
      return c1;
    }
}

cval cval_sub(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  // <x> - <address> is cst iff x is an address from the same symbol
  // (in particular, x cannot be unknown)
  if (cval_isaddress(c2))
    {
      if (cval_isaddress(c1) &&
	  !cval_isunknown_address(c1) && !cval_isunknown_address(c2) &&
	  c1.ddecl == c2.ddecl && c1.ldecl == c2.ldecl)
	{
	  if (c1.kind == cval_address_unk_offset ||
	      c2.kind == cval_address_unk_offset)
	    return cval_unknown_number;

	  c1.kind = cval_sint;
	  c1.si = truncate_signed(c1.si - c2.si, c1.isize);
	  return c1;
	}
      return cval_invalid_address;
    }
  // <address> - <x>
  if (cval_isaddress(c1))
    switch (c2.kind)
      {
      case cval_unk_number: return make_cval_address_unknown_offset(c1);
      case cval_sint: c1.si = truncate_signed(c1.si - c2.si, c1.isize); return c1;
      case cval_uint: c1.si = truncate_signed(c1.si - c2.ui, c1.isize); return c1;
      default: assert(0); return c1;
      }

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  switch (c1.kind)
    {
    case cval_float:
      assert(c2.kind == cval_float);
      c1.d -= c2.d;
      return c1;

    case cval_sint:
      assert(c2.kind == cval_sint && c1.isize == c2.isize);
      c1.si = truncate_signed(c1.si - c2.si, c1.isize);
      return c1;
      
    case cval_uint:
      assert(c2.kind == cval_uint && c1.isize == c2.isize);
      c1.ui = truncate_unsigned(c1.ui - c2.ui, c1.isize);
      return c1;
      
    case cval_float_complex:
      assert(c2.kind == cval_float_complex);
      c1.d -= c2.d;
      c1.d_i -= c2.d_i;
      return c1;

    case cval_sint_complex:
      assert(c2.kind == cval_sint_complex && c1.isize == c2.isize);
      c1.si = truncate_signed(c1.si - c2.si, c1.isize);
      c1.si_i = truncate_signed(c1.si_i - c2.si_i, c1.isize);
      return c1;
      
    case cval_uint_complex:
      assert(c2.kind == cval_uint_complex && c1.isize == c2.isize);
      c1.ui = truncate_unsigned(c1.ui - c2.ui, c1.isize);
      c1.ui_i = truncate_unsigned(c1.ui_i - c2.ui_i, c1.isize);
      return c1;
      
    default:
      assert(0);
      return c1;
    }
}

cval cval_times(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  // <address> * 1 and 1 * <address> are csts, everything else involving 
  // addresses isn't
  if (cval_isaddress(c1) || cval_isaddress(c2))
    {
      if (cval_isone(c1))
	return c2;
      if (cval_isone(c2))
	return c1;
      return cval_invalid_address;
    }

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  if (cval_iscomplex(c1))
    {
      cval c1r = cval_realpart(c1), c1i = cval_imagpart(c1),
	c2r = cval_realpart(c2), c2i = cval_imagpart(c2);

      assert(cval_iscomplex(c2));
      /* Note: this is what gcc does. The C99 standard appears to
	 require something rather more complicated (aka "do the right
	 thing") */
      return make_cval_complex(cval_sub(cval_times(c1r, c2r),
					cval_times(c1i, c2i)),
			       cval_add(cval_times(c1r, c2i),
					cval_times(c1i, c2r)));
    }

  switch (c1.kind)
    {
    case cval_float:
      assert(c2.kind == cval_float);
      c1.d *= c2.d;
      return c1;

    case cval_sint:
      assert(c2.kind == cval_sint && c1.isize == c2.isize);
      c1.si = truncate_signed(c1.si * c2.si, c1.isize);
      return c1;
      
    case cval_uint:
      assert(c2.kind == cval_uint && c1.isize == c2.isize);
      c1.ui = truncate_unsigned(c1.ui * c2.ui, c1.isize);
      return c1;
      
    default:
      assert(0);
      return c1;
    }
}

cval cval_divide(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  // <address> / 1 is a cst, everything else involving addresses isn't
  if (cval_isaddress(c1) || cval_isaddress(c2))
    return cval_isone(c2) ? c1 : cval_invalid_address;

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  if (cval_iscomplex(c1))
    {
      cval c1r = cval_realpart(c1), c1i = cval_imagpart(c1),
	c2r = cval_realpart(c2), c2i = cval_imagpart(c2);
      cval mag = cval_add(cval_times(c1r, c2r), cval_times(c1i, c2i));

      assert(cval_iscomplex(c2));
      /* Note: this is what gcc does. The C99 standard appears to
	 require something rather more complicated (aka "do the right
	 thing") */
      return make_cval_complex(cval_divide(cval_add(cval_times(c1r, c2r),
						    cval_times(c1i, c2i)),
					   mag),
			       cval_divide(cval_sub(cval_times(c1i, c2r),
						    cval_times(c1r, c2i)),
					   mag));
    }

  switch (c1.kind)
    {
    case cval_float:
      assert(c2.kind == cval_float);
      c1.d /= c2.d;
      return c1;

    case cval_sint:
      assert(c2.kind == cval_sint && c1.isize == c2.isize);
      if (c2.si == 0)
	return cval_top;
      /* Note that signed division can overflow (MININT / -1). */
      c1.si = truncate_signed(c1.si / c2.si, c1.isize);
      return c1;
      
    case cval_uint:
      assert(c2.kind == cval_uint && c1.isize == c2.isize);
      if (c2.ui == 0)
	return cval_top;
      c1.ui /= c2.ui;
      return c1;
      
    default:
      assert(0);
      return c1;
    }
}

cval cval_modulo(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  if (cval_isone(c2))
    return make_cval_signed(0, int_type);

  if (cval_isaddress(c1) || cval_isaddress(c2))
    return cval_invalid_address;

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  switch (c1.kind)
    {
    case cval_float:
      assert(c2.kind == cval_float);
      c1.d /= c2.d;
      return c1;

    case cval_sint:
      assert(c2.kind == cval_sint && c1.isize == c2.isize);
      if (c2.si == 0)
	return cval_top;
      c1.si = truncate_signed(c1.si % c2.si, c1.isize);
      return c1;
      
    case cval_uint:
      assert(c2.kind == cval_uint && c1.isize == c2.isize);
      if (c2.ui == 0)
	return cval_top;
      c1.ui %= c2.ui;
      return c1;
      
    default:
      assert(0);
      return c1;
    }
}

#define CVAL_BITOP(OP) \
{ \
  if (cval_istop(c1) || cval_istop(c2)) \
    return cval_top; \
 \
  if (cval_isaddress(c1) || cval_isaddress(c2)) \
    return cval_invalid_address; \
  if (cval_isunknown_number(c1) || cval_isunknown_number(c2)) \
    return cval_unknown_number; \
 \
  assert(c1.kind == c2.kind && c1.isize == c2.isize); \
  switch (c1.kind) \
    { \
    case cval_sint: \
      c1.si = truncate_signed(c1.si OP c2.si, c1.isize); \
      return c1; \
       \
    case cval_uint: \
      c1.ui = truncate_signed(c1.ui OP c2.ui, c1.isize); \
      return c1; \
       \
    default: \
      assert(0); \
      return c1; \
    } \
}

cval cval_lshift(cval c1, cval c2) CVAL_BITOP(<<)
cval cval_rshift(cval c1, cval c2) CVAL_BITOP(>>)
cval cval_bitand(cval c1, cval c2) CVAL_BITOP(&)
cval cval_bitor(cval c1, cval c2)  CVAL_BITOP(|)
cval cval_bitxor(cval c1, cval c2) CVAL_BITOP(^)

#define CVAL_RELOP(OP) \
{ \
  bool res; \
 \
  if (cval_istop(c1) || cval_istop(c2)) \
    return cval_top; \
 \
  if (cval_isaddress(c1) || cval_isaddress(c2)) \
    return cval_invalid_address; \
  /* Surprisingly (?) &x == &x is not a constant expression */ \
  if (cval_isunknown_number(c1) || cval_isunknown_number(c2)) \
    return cval_unknown_number; \
 \
  switch (c1.kind) \
    { \
    case cval_float: \
      assert(c2.kind == cval_float); \
      res = c1.d OP c2.d; \
      break; \
 \
    case cval_sint: \
      assert(c2.kind == cval_sint && c1.isize == c2.isize); \
      res = c1.si OP c2.si; \
      break; \
       \
    case cval_uint: \
      assert(c2.kind == cval_uint && c1.isize == c2.isize); \
      res = c1.ui OP c2.ui; \
      break; \
       \
    default: \
      assert(0); \
      res = FALSE; \
      break; \
    } \
  return make_cval_signed(res, int_type); \
}

static cval cval_simpleeq(cval c1, cval c2) CVAL_RELOP(==)
cval cval_leq(cval c1, cval c2) CVAL_RELOP(<=)

cval cval_eq(cval c1, cval c2)
{
  if (cval_iscomplex(c1) && cval_iscomplex(c2))
    {
      cval req = cval_simpleeq(cval_realpart(c1), cval_realpart(c2));
      cval ieq = cval_simpleeq(cval_imagpart(c1), cval_imagpart(c2));

      return make_cval_signed(cval_isone(req) && cval_isone(ieq), int_type);
    }
  else
    return cval_simpleeq(c1, c2);
}

/* True if x fits in the range of type t */
bool uint_inrange(largest_uint x, type t)
{
  size_t tsize = type_size_int(t);
  largest_uint max;

  assert(tsize <= sizeof(largest_uint));

  if (tsize == sizeof(largest_uint) && type_unsigned(t))
    return TRUE;

  max = (largest_uint)1 << (BITSPERBYTE * tsize - !type_unsigned(t));

  return x < max;
}

bool sint_inrange(largest_int x, type t)
{
  size_t tsize = type_size_int(t);
  largest_int max;

  assert(tsize <= sizeof(largest_uint));

  if (x < 0 && type_unsigned(t))
    return FALSE;

  if (tsize == sizeof(largest_uint))
    return TRUE;

  max = (largest_int)1 << (BITSPERBYTE * tsize - !type_unsigned(t));

  /* The x<0&&unsigned and largest_int cases have been handled above. */
  return x >= -max && x < max;
}

bool cval_inrange(cval c, type t)
/* Requires: constant_integral(c)
   Returns: TRUE if c is in range of type t. */
{
  switch (c.kind)
    {
    case cval_sint: return sint_inrange(c.si, t);
    case cval_uint: return uint_inrange(c.ui, t);
    default: abort(); return FALSE;
    }
}

largest_int cval_intcompare(cval c1, cval c2)
/* Requires: cval_isinteger(c1) && cval_isinteger(c2)
   Returns: x, x<0 if c1 < c2, x = 0 if c1 = c2 and x > 0 if c1 > c2
*/
{
  /* beware of overflow of difference w/ respect to largest_int */
  switch (c1.kind)
    {
    case cval_sint:
      switch (c2.kind)
	{
	case cval_sint:
	  return c1.si - c2.si;
	case cval_uint:
	  /* can't use c1.si < c2.ui because of implicit conversion */
	  if (c1.si < 0 || c1.si < c2.ui)
	    return -1;
	  return c1.si - c2.ui; /* common type is largest_uint */
	default: abort(); return 0;
	}
    case cval_uint:
      switch (c2.kind)
	{
	case cval_sint:
	  if (c2.si < 0 || c1.ui < c2.si)
	    return 1;
	  /* result might overflow so compare with 0 */
	  return (c1.ui - c2.si) > 0; /* common type is largest_uint */
	case cval_uint:
	  /* We do the cases because the result might overflow
	     largest_int */
	  if (c1.ui < c2.ui)
	    return -1;
	  if (c1.ui > c2.ui)
	    return 1;
	  return 0;
	default: abort(); return 0;
	}
    default: abort(); return 0;
    }
}

void cval_print(FILE *f, cval c)
/* Effects: prints a parsable representable of c to f
 */
{
  switch (c.kind)
    {
    case cval_float:
    case cval_float_complex:
      break;
    case cval_uint: fprintf(f, "%llu", c.ui); break;
    case cval_uint_complex: fprintf(f, "%llu %llu", c.ui, c.ui_i); break;
    case cval_sint: fprintf(f, "%lld", c.si); break;
    case cval_sint_complex: fprintf(f, "%lld %lld", c.si, c.si_i); break;
    default: assert(0); break;
    }
}

void cval_debug(cval c)
/* For use while debugging. gdb doesn't print cvals right */
{
  switch (c.kind)
    {
    case cval_variable: printf("<top>"); break;
    case cval_unk_number: printf("<number>"); break;
    case cval_unk_address: printf("<address>"); break;
    case cval_address: case cval_address_unk_offset:
      if (c.ldecl)
	printf("<address label=%s(%p)", c.ldecl->name, c.ldecl);
      else
	printf("<address sym=%s(%p)", c.ddecl->name, c.ddecl);
      if (c.kind == cval_address_unk_offset)
	printf(" + <number>");
      else if (c.si > 0)
	printf(" + %lld", c.si);
      else
	printf(" - %lld", -c.si);
      printf(">");
    default:
      printf("[size: %u]", (unsigned)c.isize);
      cval_print(stdout, c);
    }
  printf("\n");
}

cval cval_align_to(cval n, cval alignment)
{
  cval count = cval_divide(cval_sub(cval_add(n, alignment),
				    make_type_cval(1)),
			   alignment);

  return cval_times(count, alignment);
}

cval cval_gcd(cval x, cval y)
{
  cval z;

  if (cval_istop(x) || cval_istop(y))
    return cval_top;

  if (cval_isunknown_number(x) || cval_isunknown_number(y))
    return cval_unknown_number;

  for (;;)
    {
      if (!cval_boolvalue(y)) /* ie 0 */
	return x;
      
      z = cval_modulo(x, y); x = y; y = z;
    }
}

cval cval_lcm(cval x, cval y)
{
  /* ignoring risk of overflow (used for alignments which are typically <= 16) */
  return cval_divide(cval_times(x, y), cval_gcd(x, y)); 
}

cval cval_max(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  // Not used for anything else yet. Fix later if need to.
  assert(cval_isinteger(c1) && cval_isinteger(c2));

  return cval_intcompare(c1, c2) > 0 ? c1 : c2;
}

cval cval_min(cval c1, cval c2)
{
  if (cval_istop(c1) || cval_istop(c2))
    return cval_top;

  if (cval_isunknown_number(c1) || cval_isunknown_number(c2))
    return cval_unknown_number;

  // Not used for anything else yet. Fix later if need to.
  assert(cval_isinteger(c1) && cval_isinteger(c2));

  return cval_intcompare(c1, c2) < 0 ? c1 : c2;
}

data_declaration cval_ddecl(cval c)
/* Returns: c's declaration
   Requires: cval_isaddress(c)  && !cval_isunknown_address(c) && 
     c doesn't denote a label
*/
{
  assert(cval_isaddress(c) && !cval_isunknown_address(c));
  return c.ddecl;
}

label_declaration cval_ldecl(cval c)
/* Returns: c's declaration, or NULL if c denotes a variable
   Requires: cval_isaddress(c)  && !cval_isunknown_address(c)
*/
{
  assert(cval_isaddress(c) && !cval_isunknown_address(c));
  return c.ldecl;
}

