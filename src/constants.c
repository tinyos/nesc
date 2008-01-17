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

/* ASSUME: (float)(long double)f == f
           (double)(long double)d == d
*/
/* XXX: overflow detection (signed ints only, gcc doesn't do reals)
	initialised arrays are missing their size
*/

/* Note: legal ops on complex are:
   +, -, *, /, ==, !=, &&, ||, __real__, __imag__, ~ (conjugate)
*/

#define _ISOC99_SOURCE
#include "parser.h"
#include "constants.h"
#include "c-parse.h"
#include "semantics.h"
#include "cval.h"
#include "nesc-magic.h"
#include <stdlib.h>
#include "AST_utils.h"

static known_cst new_known_cst(region r, type t, cval c)
{
  known_cst kc = ralloc(parse_region, struct known_cst);

  kc->type = t;
  kc->cval = c;

  return kc;
}
  
known_cst make_unknown_cst(cval c, type t)
{
  cval cst;

  if (cval_isaddress(c))
    cst = cval_unknown_address;
  else
    cst = cval_unknown_number;

  return new_known_cst(parse_region, t, cst);
}

known_cst make_cst(cval c, type t)
{
  if (cval_istop(c))
    return NULL;

  return new_known_cst(parse_region, t, cval_cast(c, t));
}

known_cst make_address_cst(data_declaration ddecl, label_declaration ldecl,
			   largest_int offset, type t)
{
  return make_cst(make_cval_address(ddecl, ldecl, offset), t);
}

known_cst make_unsigned_cst(largest_uint x, type t)
{
  return make_cst(make_cval_unsigned(x, t), t);
}

known_cst make_signed_cst(largest_int x, type t)
{
  return make_cst(make_cval_signed(x, t), t);
}

known_cst cast_constant(known_cst c, type to)
{
  if (type_equal(c->type, to))
    return c;

  return make_cst(cval_cast(c->cval, to), to);
}

known_cst fold_label_address(expression e)
{
  return make_address_cst(NULL, CAST(label_address, e)->id_label->ldecl, 0, e->type);
}

known_cst fold_sizeof(expression e, type stype)
{
  if (type_size_cc(stype))
    return make_cst(type_size(stype), e->type);
  else
    return NULL;
}

known_cst fold_alignof(expression e, type atype)
{
  if (type_has_size(atype))
    return make_cst(type_alignment(atype), e->type);
  else
    /* compile-time error, alignof incomplete type */
    return make_unsigned_cst(1, e->type); 
}

known_cst fold_cast(expression e)
{
  cast cc = CAST(cast, e);

  if (cc->arg1->cst && !type_void(e->type))
    return cast_constant(cc->arg1->cst, e->type);
  else
    return NULL;
}

/* +, -, ! and ~ only */
known_cst fold_unary(expression e)
{
  unary u = CAST(unary, e);
  known_cst arg = u->arg1->cst;
  type t = u->type;

  if (arg)
    {
      switch (u->kind)
	{
	case kind_unary_plus:
	  /* Note that this allows +(int)&x to be a constant... 
	     This is consistent with gcc */
	  return cast_constant(arg, t); /* essentially a no-op */

	case kind_unary_minus:
	  return make_cst(cval_negate(cval_cast(arg->cval, t)), t);

	case kind_not:
	  return make_cst(cval_not(cval_cast(arg->cval, t)), t);

	case kind_bitnot:
	  return make_cst(cval_bitnot(cval_cast(arg->cval, t)), t);

	case kind_conjugate:
	  return make_cst(cval_conjugate(cval_cast(arg->cval, t)), t);

	case kind_realpart:
	  if (!type_complex(u->arg1->type))
	    return cast_constant(arg, t);
	  else
	    return make_cst(cval_realpart(cval_cast(arg->cval, t)), t);

	case kind_imagpart:
	  if (!type_complex(u->arg1->type))
	    return make_cst(cval_cast(cval_zero, t), t);
	  else
	    return make_cst(cval_imagpart(cval_cast(arg->cval, t)), t);

	default:
	  assert(0);
	  break;
	}
    }
  return NULL;
}

/* XXX: overflow */
static known_cst fold_sub(type restype, known_cst c1, known_cst c2)
{
  type t1 = c1->type, t2 = c2->type, basetype, ct;
  cval s = make_cval_unsigned(1, size_t_type), res;

  basetype = type_pointer(t1) ? t1 : type_pointer(t2) ? t2 : NULL;
  if (basetype)
    {
      basetype = type_points_to(basetype);

      if (!type_size_cc(basetype))
	return NULL;

      s = cval_cast(type_size(basetype), size_t_type);
      ct = intptr_type;
    }
  else
    ct = restype;

  s = cval_cast(s, ct);
  if (type_pointer(t1) && type_pointer(t2))
    res = cval_divide(cval_cast(cval_sub(cval_cast(c1->cval, ct), cval_cast(c2->cval, ct)), ct),
		      s);
  else
    res = cval_sub(cval_cast(c1->cval, ct),
		   cval_divide(cval_cast(c2->cval, ct), s));

  return make_cst(res, restype);
}

/* XXX: overflow */
known_cst fold_add(type restype, known_cst c1, known_cst c2)
{
  type t1 = c1->type, t2 = c2->type, ct;
  cval s = make_cval_unsigned(1, size_t_type);

  if (type_pointer(t2))
    {
      known_cst ctmp;
      type ttmp;

      ctmp = c1; c1 = c2; c2 = ctmp;
      ttmp = t1; t1 = t2; t2 = ttmp;
    }

  if (type_pointer(t1))
    {
      type basetype = type_points_to(t1);

      if (!type_size_cc(basetype))
	return NULL;

      s = cval_cast(type_size(basetype), size_t_type);
      ct = intptr_type;
    }
  else
    ct = restype;

  return make_cst(cval_add(cval_cast(c1->cval, ct),
			   cval_times(cval_cast(c2->cval, ct), cval_cast(s, ct))),
		  restype);
}

known_cst fold_binary(type t, expression e)
{
  binary b = CAST(binary, e);
  known_cst c1 = b->arg1->cst, c2 = b->arg2->cst;
  type t1 = type_default_conversion(b->arg1->type),
    t2 = type_default_conversion(b->arg2->type);
  
  if (b->kind == kind_andand || b->kind == kind_oror)
    {
      if (c1)
	{
	  if (constant_knownbool(c1))
	    {
	      bool c1val = constant_boolvalue(c1);

	      if (b->kind == kind_andand ? !c1val : c1val)
		return make_signed_cst(c1val, t);
	    }
	  if (constant_unknown_number(c1))
	    return make_unknown_cst(cval_unknown_number, t);
	}

      if (c1 && c2)
	{
	  if (constant_knownbool(c2))
	    {
	      bool c2val = constant_boolvalue(c2);
	      if (b->kind == kind_andand ? !c2val : c2val)
		return make_signed_cst(c2val, t);
	    }

	  if (constant_unknown_number(c2))
	    return make_unknown_cst(cval_unknown_number, t);
	}
    }
  else if (c1 && c2)
    {
      cval cv1 = c1->cval, cv2 = c2->cval;

      switch (b->kind)
	{
	case kind_plus: case kind_array_ref:
	  return fold_add(t, c1, c2);

	case kind_minus:
	  return fold_sub(t, c1, c2);

	case kind_times: case kind_divide: case kind_modulo:
	case kind_lshift: case kind_rshift:
	case kind_bitand: case kind_bitor: case kind_bitxor: {
	  cval res;

	  cv1 = cval_cast(cv1, t);
	  cv2 = cval_cast(cv2, t);

	  switch (b->kind) {
	  case kind_times: res = cval_times(cv1, cv2); break;
	  case kind_divide: res = cval_divide(cv1, cv2); break;
	  case kind_modulo: res = cval_modulo(cv1, cv2); break;
	  case kind_lshift: res = cval_lshift(cv1, cv2); break;
	  case kind_rshift: res = cval_rshift(cv1, cv2); break;
	  case kind_bitand: res = cval_bitand(cv1, cv2); break;
	  case kind_bitor: res = cval_bitor(cv1, cv2); break;
	  case kind_bitxor: res = cval_bitxor(cv1, cv2); break;
	  default: abort(); return NULL;
	  }
	  return make_cst(res, t);
	}
	case kind_eq: case kind_ne:
	case kind_leq: case kind_geq: case kind_lt: case kind_gt: {
	  cval res;
	  type ct;

	  /* Pointers win. */
	  if (type_pointer(t1) || type_pointer(t2))
	    ct = intptr_type; 
	  else
	    ct = common_type(t1, t2);

	  cv1 = cval_cast(cv1, ct);
	  cv2 = cval_cast(cv2, ct);

	  switch (b->kind) {
	  case kind_eq: res = cval_eq(cv1, cv2); break;
	  case kind_ne: res = cval_ne(cv1, cv2); break;
	  case kind_leq: res = cval_leq(cv1, cv2); break;
	  case kind_geq: res = cval_geq(cv1, cv2); break;
	  case kind_lt: res = cval_lt(cv1, cv2); break;
	  case kind_gt: res = cval_gt(cv1, cv2); break;
	  default: abort(); return NULL;
	  }
	  return make_cst(res, t);
	}
	default:
	  assert(0); return NULL;
	}
      }

  return NULL;
}

known_cst fold_conditional(expression e)
{
  conditional c = CAST(conditional, e);
  known_cst cond = c->condition->cst;

  if (cond)
    {
      expression arg1 = c->arg1 ? c->arg1 : c->condition;
      known_cst kc1 = arg1->cst, kc2 = c->arg2->cst;

      if (constant_knownbool(cond))
	{
	  expression value = constant_boolvalue(cond) ? arg1 : c->arg2;

	  e->static_address = value->static_address;
	  if (value->cst)
	    return cast_constant(value->cst, e->type);
	  else
	    return NULL;
	}
      else if (constant_unknown_number(cond) && kc1 && kc2)
	/* Faced with an unknown condition and two constant arguments, we
	   return an unknown constant. Unknown number if both arg1 and arg2
	   are unknown numbers, an unknown address otherwise */
	return make_unknown_cst(cval_isaddress(kc1->cval) ?
				kc1->cval : kc2->cval, e->type);
    }

  return NULL;
}

known_cst fold_function_call(expression e, int pass)
{
  function_call fce = CAST(function_call, e);

  if (call_to(builtin_constant_p, fce) && fce->args)
    return make_signed_cst(fce->args->cst != NULL, int_type);

  return fold_magic(fce, pass);
}

known_cst fold_identifier(expression e, data_declaration decl, int pass)
{
  if (decl->kind == decl_constant)
    // We don't know template arg values at parse time
    return pass == 0 && decl->substitute ?
      make_unknown_cst(type_real(e->type) ? cval_unknown_number :
		       cval_unknown_address, e->type) :
      decl->value;
  else
    return NULL;
}

known_cst foldaddress_identifier(expression e, data_declaration decl)
{
  if ((decl->kind == decl_function && decl->ftype != function_nested) ||
      (decl->kind == decl_variable && !decl->islocal) ||
      decl->kind == decl_magic_string)
    return make_address_cst(decl, NULL, 0, make_pointer_type(e->type));
  else
    return NULL;
}

known_cst foldaddress_string(string s)
{
  return make_address_cst(s->ddecl, NULL, 0, s->type);
}

known_cst foldaddress_field_ref(expression e)
{
  field_ref fref = CAST(field_ref, e);
  field_declaration fdecl = fref->fdecl;
  known_cst object = fref->arg1->static_address;
  cval field_offset;
  type pftype;

  if (!object || cval_istop(fdecl->offset) || !cval_istop(fdecl->bitwidth))
    return NULL;

  if (constant_unknown(object))
    return make_unknown_cst(object->cval, object->type);

  pftype = make_pointer_type(fdecl->type);
  field_offset = cval_divide(fdecl->offset, cval_bitsperbyte);

  /* Conceivably, size_t could be smaller than pointers. So we cast
     the offset to the pointer-to-field type (we are assuming that is
     the same size as the pointer to base-struct type) */
  return make_cst(cval_add(object->cval, cval_cast(field_offset, pftype)),
		  pftype);
}

#ifndef HAVE_STRTOLD
#define strtold strtod
#endif

lexical_cst fold_lexical_real(type realtype, location loc, cstring tok)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  cval realvalue = make_cval_float(strtold(tok.data, NULL));

  if (type_complex(realtype))
    realvalue = make_cval_complex(cval_cast(cval_zero, make_base_type(realtype)), realvalue);
  c->cst = make_cst(realvalue, realtype);
  c->type = realtype;
  return c;
}

lexical_cst fold_lexical_char(location loc, cstring tok,
			      bool wide_flag, int charvalue)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  type ctype = wide_flag ? wchar_type : int_type;

  c->type = ctype;
  c->cst = make_cst(type_unsigned(ctype) ?
		    make_cval_unsigned(charvalue, ctype) :
		    make_cval_signed(charvalue, ctype),
		    ctype);
  return c;
}

string fold_lexical_string(location loc, string_cst components, cstring value,
			   bool wide_flag)
{
  data_declaration sdecl = declare_string(NULL, value, wide_flag);
  string s = new_string(parse_region, loc, components, sdecl);

  s->type = sdecl->type;
  s->static_address = foldaddress_string(s);
  s->lvalue = TRUE;

  return s;
}

lexical_cst fold_lexical_int(type itype, location loc, cstring tok,
			     bool iscomplex, largest_uint intvalue, bool overflow)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  cval cv;

  if (overflow)
    {
      warning_with_location(loc, "integer constant out of range");
      itype = unsigned_long_long_type;
    }
  else
    {
      /* Do some range checks */
      if (!uint_inrange(intvalue, itype))
	{
	  if (uint_inrange(intvalue, unsigned_int_type))
	    {
	      warning_with_location(loc, "decimal constant is so large that it is unsigned");
	      itype = unsigned_int_type;
	    }
	  /* These other cases cause no warnings */
	  else if (uint_inrange(intvalue, long_type))
	    itype = long_type;
	  else if (uint_inrange(intvalue, unsigned_long_type))
	    itype = unsigned_long_type;
	  else if (uint_inrange(intvalue, long_long_type))
	    itype = long_long_type;
	  else if (uint_inrange(intvalue, unsigned_long_long_type))
	    itype = unsigned_long_long_type;
	}
    }

  cv = type_unsigned(itype) ? make_cval_unsigned(intvalue, itype) :
    make_cval_signed(intvalue, itype);
  if (iscomplex)
    {
      cv = make_cval_complex(cval_cast(cval_zero, itype), cv);
      itype = make_complex_type(itype);
    }

  c->type = itype;
  c->cst = make_cst(cv, itype);

  return c;
}


bool definite_null(expression e)
{
  return
    (type_integer(e->type) ||
     (type_pointer(e->type) && type_void(type_points_to(e->type)))) &&
    definite_zero(e);
}

bool definite_zero(expression e)
{
  return e->cst && is_zero_constant(e->cst);
}

bool is_zero_constant(known_cst c)
{
  return cval_knownbool(c->cval) && !constant_boolvalue(c);
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression. */
void constant_overflow_warning(known_cst c)
{
}

bool check_constant_once(expression e, cst_kind k)
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
{
  if (e->cst_checked)
    return FALSE;

  if (e->cst && constant_unknown(e->cst))
    /* Unknown constant. We can't check yet it if matches k */
    if (k == cst_any ||
	(k == cst_numerical && constant_unknown_number(e->cst)) ||
	(k == cst_address && constant_address(e->cst)))
      return FALSE;

  e->cst_checked = TRUE;

  return TRUE;
}


