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

#include "parser.h"
#include "types.h"
#include "constants.h"
#include "c-parse.h"
#include "machine.h"
#include <stddef.h>

struct type
{
  enum { tk_primitive, tk_complex, tk_tagged, tk_error, tk_void,
	 tk_pointer, tk_function, tk_array, tk_iref, tk_variable } kind;
  type_quals qualifiers;
  data_declaration combiner;

  /* size is not used for aggregate types
     (as the values may be discovered after the type is created)
     or for arrays (as they may be arrays of aggregates)
     alignment is used for aggregates and arrays only if it is non-zero
     (indicating an alignment overridden by an attribute)
  */
  cval size;
  cval alignment; 

  union {
    /* tk_primtive and tk_complex.
       The order reflects promotion order (for an arbitrary machine,
       see common_primitive_type) */
    enum { /* The elements of this enum must be ordered as follows:
	      - all integral types before tp_first_floating
	      - do not change the floating type order
	      - the integral types are ordered by "rank" (see c9x std) and
	        unsignedness (unsigned > signed) (common_primitive_type
		relies on this order)
	      - The tp_[u]int<n> types must be before tp_char (for the 
	        assert in common_primitive_type). These types are only
		used when the corresponding size is not available amongst
		short/int/long/long long
	        If this frontend followed c9x, these types would always exist
	        and be special integer types distinct from the regular ones
	        (see the rank stuff), but we're following gcc so they
	        aren't. If this changes, common_primitive_type, 
		default_conversion and type_default_conversion need revising.
	   */
           tp_error,
	   tp_int2, tp_uint2, tp_int4, tp_uint4, tp_int8, tp_uint8,
           tp_char, 
	   tp_signed_char, tp_unsigned_char,
	   tp_short, tp_unsigned_short,
	   tp_int, tp_unsigned_int,
	   tp_long, tp_unsigned_long,
	   tp_long_long, tp_unsigned_long_long,
	   /* Used as the rep type of enums whose constants are derived
	      from template arguments and whose size is hence unknown.
	      The unknown int type has the highest rank (its unsignedness
	      is unknown, we assume its signed). */
	   tp_unknown_int,

	   
	   tp_first_floating,
	   tp_float = tp_first_floating, tp_double, tp_long_double,
	   tp_last
    } primitive;

    /* tk_tagged */
    tag_declaration tag;

    /* tk_pointer */
    type pointsto;

    /* tk_function */
    struct {
      type returns;
      typelist argtypes;
      bool varargs;
      bool oldstyle;
      enum { tkf_c, tkf_event, tkf_command, tkf_task, tkf_generic } fkind;
    } fn;

    /* tk_array */
    struct {
      type arrayof;
      expression size;
      /* If size is a known constant, it is guaranteed >= 0 */
    } array;

    /* tk_iref */
    data_declaration iref;

    /* tk_variable */
    data_declaration tdecl;
  } u;
};

#ifdef RC_ADJUST
static void rc_update_type(struct type *old, struct type *new)
{
  regionid base = regionidof(old);

  switch (old->kind)
    {
    case tk_tagged:
      RC_UPDATE(base, old->u.tag, new->u.tag);
      break;
    case tk_pointer:
      RC_UPDATE(base, old->u.pointsto, new->u.pointsto);
      break;
    case tk_function:
      RC_UPDATE(base, old->u.fn.returns, new->u.fn.returns);
      RC_UPDATE(base, old->u.fn.argtypes, new->u.fn.argtypes);
      break;
    case tk_array:
      RC_UPDATE(base, old->u.array.arrayof, new->u.array.arrayof);
      RC_UPDATE(base, old->u.array.size, new->u.array.size);
      break;
    default:
      break;
    }
}

static size_t rc_adjust_type(void *x, int by)
{
  struct type *p = x;
  RC_ADJUST_PREAMBLE;

  switch (p->kind)
    {
    case tk_tagged:
      RC_ADJUST(p->u.tag, by);
      break;
    case tk_pointer:
      RC_ADJUST(p->u.pointsto, by);
      break;
    case tk_function:
      RC_ADJUST(p->u.fn.returns, by);
      RC_ADJUST(p->u.fn.argtypes, by);
      break;
    case tk_array:
      RC_ADJUST(p->u.array.arrayof, by);
      RC_ADJUST(p->u.array.size, by);
      break;
    default:
      break;
    }

  return sizeof *p;
}
#endif

static region types_region;

static type primitive_types[tp_last];
static type complex_types[tp_last];

type float_type, double_type, long_double_type, 
  int_type, unsigned_int_type, long_type, unsigned_long_type,
  long_long_type, unsigned_long_long_type, short_type, unsigned_short_type,
  char_type, char_array_type, wchar_type, wchar_array_type,
  unsigned_char_type, signed_char_type, void_type, ptr_void_type,
  size_t_type, ptrdiff_t_type, intptr_type,
  int2_type, uint2_type, int4_type, uint4_type, int8_type, uint8_type,
  unknown_int_type;
type error_type;

static bool legal_array_size(cval c) 
{
  return !cval_isinteger(c) || cval_intcompare(c, cval_zero) >= 0;
}

static type copy_type(type t)
{
  type nt = ralloc(types_region, struct type);
  *nt = *t;
  return nt;
}

static type new_type(int kind)
{
  type nt = ralloc(types_region, struct type);

  nt->kind = kind;
  /*nt->qualifiers = 0;
    nt->combiner = NULL;*/
  nt->size = nt->alignment = cval_top;
  return nt;
}

/* Return the 'complex t' version of basic type t (one of the integral or
   floating-point types) */
type make_complex_type(type t)
{
  assert(t->kind == tk_primitive);

  return qualify_type1(complex_types[t->u.primitive], t);
}

/* Return the base type of complex type t (one of the integral or
   floating-point types) */
type make_base_type(type t)
{
  assert(t->kind == tk_complex);

  return primitive_types[t->u.primitive];
}

/* Return the type t with it's qualifiers set to tq (old qualifiers are 
   ignored). This is illegal for function types. For arrays, the qualifiers
   get pushed down to the base type. */
type make_qualified_type(type t, type_quals qualifiers)
{
  /* Push const or volatile down to base type */
  if (t->kind == tk_array)
    return make_array_type(make_qualified_type(t->u.array.arrayof, qualifiers),
			   t->u.array.size);
  else
    {
      type nt = copy_type(t);

      nt->qualifiers = qualifiers;

      return nt;
    }
}

/* Return type 'pointer to t' (unqualified) */
type make_pointer_type(type t)
{
  type nt = new_type(tk_pointer);
  nt->u.pointsto = t;

  /* ASSUME: all pointers are the same */
  nt->size = make_type_cval(target->tptr.size);
  nt->alignment = make_type_cval(target->tptr.align);

  return nt;
}

/* Return type 'array [size] of t'. size is optional */
type make_array_type(type t, expression size)
{
  type nt = new_type(tk_array);

  nt->u.array.arrayof = t;
  nt->u.array.size = size;
  assert(!size || !size->cst || legal_array_size(size->cst->cval));

  return nt;
}

/* Return type 'function with argument types argtypes returning t'.
   If oldstyle is true, this is an oldstyle function type and
   argtypes is NULL */
type make_function_type(type t, typelist argtypes, bool varargs,
			bool oldstyle)
{
  type nt = new_type(tk_function);
  nt->u.fn.fkind = tkf_c;
  nt->u.fn.returns = t;
  nt->u.fn.argtypes = argtypes;
  nt->u.fn.varargs = varargs;
  nt->u.fn.oldstyle = oldstyle;
  nt->size = nt->alignment = make_type_cval(1);
  return nt;
}

/* Return the tagged type whose declaration is d */
type make_tagged_type(tag_declaration d)
{
  type nt = new_type(tk_tagged);
  nt->u.tag = d;
  return nt;
}

/* Make the single instance of pk, with specified size and alignment
   (Note that we may make copies if this single instance for different alignments)
   Requires: must be called at most once for each pk, from types_init
*/

static type make_primitive(int pk, int size, int alignment)
{
  type nt = new_type(tk_primitive), ct;

  nt->u.primitive = pk;
  nt->size = make_type_cval(size);
  nt->alignment = make_type_cval(alignment);
  primitive_types[pk] = nt;

  ct = new_type(tk_complex);
  ct->u.primitive = pk;
  ct->size = make_type_cval(size * 2);
  ct->alignment = nt->alignment; /* ASSUME: alignof(complex t) == alignof(t) */
  complex_types[pk] = ct;

  return nt;
}

static type make_unknown_int()
{
  type nt = new_type(tk_primitive), ct;

  nt->u.primitive = tp_unknown_int;
  nt->size = nt->alignment = cval_unknown;
  primitive_types[tp_unknown_int] = nt;

  ct = new_type(tk_complex);
  ct->u.primitive = tp_unknown_int;
  ct->size = ct->alignment = cval_unknown;
  complex_types[tp_unknown_int] = ct;

  return nt;
}

static type lookup_primitive(int default_kind, int size, int alignment,
			     bool isunsigned)
{
  int i;

  for (i = tp_signed_char; i < tp_unknown_int; i++)
    if (cval_uint_value(primitive_types[i]->size) == size &&
	type_unsigned(primitive_types[i]) == isunsigned)
      return primitive_types[i];

  return make_primitive(default_kind, size, alignment);
}

/* Return the integral type of size 'size', unsigned if 'isunsigned' is true */
type type_for_size(cval size, bool isunsigned)
{
  type t;

  if (cval_isunknown(size))
    return unknown_int_type;

  t = lookup_primitive(tp_error, cval_sint_value(size), 0, isunsigned);
  assert(t->u.primitive != tp_error);
  return t;
}

static type type_for_size_int(int size, bool isunsigned)
{
  type t = lookup_primitive(tp_error, size, 0, isunsigned);
  assert(t->u.primitive != tp_error);
  return t;
}

type type_for_cval(cval c, bool isunsigned)
{
  int i;

  if (cval_isunknown(c))
    return unknown_int_type;

  for (i = tp_signed_char; i < tp_unknown_int; i++)
    if (type_unsigned(primitive_types[i]) == isunsigned &&
	cval_inrange(c, primitive_types[i]))
      return primitive_types[i];

  return NULL;
}

void init_types(void)
{
  types_region = newregion();

  float_type = make_primitive
    (tp_float, target->tfloat.size, target->tfloat.align);
  double_type = make_primitive
    (tp_double, target->tdouble.size, target->tdouble.align);
  long_double_type = make_primitive
    (tp_long_double, target->tlong_double.size, target->tlong_double.align);

  short_type = make_primitive
    (tp_short, target->tshort.size, target->tshort.align);
  unsigned_short_type = make_primitive
    (tp_unsigned_short, target->tshort.size, target->tshort.align);

  int_type = make_primitive
    (tp_int, target->tint.size, target->tint.align);
  unsigned_int_type = make_primitive
    (tp_unsigned_int, target->tint.size, target->tint.align);

  long_type = make_primitive
    (tp_long, target->tlong.size, target->tlong.align);
  unsigned_long_type = make_primitive
    (tp_unsigned_long, target->tlong.size, target->tlong.align);

  long_long_type = make_primitive
    (tp_long_long, target->tlong_long.size, target->tlong_long.align);
  unsigned_long_long_type = make_primitive
    (tp_unsigned_long_long, target->tlong_long.size, target->tlong_long.align);

  signed_char_type = make_primitive(tp_signed_char, 1, target->int1_align);
  unsigned_char_type = make_primitive(tp_unsigned_char, 1, target->int1_align);
  char_type = make_primitive(tp_char, 1, target->int1_align);

  int2_type = lookup_primitive(tp_int2, 2, target->int2_align, FALSE);
  uint2_type = lookup_primitive(tp_uint2, 2, target->int2_align, TRUE);
  int4_type = lookup_primitive(tp_int4, 4, target->int4_align, FALSE);
  uint4_type = lookup_primitive(tp_uint4, 4, target->int4_align, TRUE);
  int8_type = lookup_primitive(tp_int8, 8, target->int8_align, FALSE);
  uint8_type = lookup_primitive(tp_uint8, 8, target->int8_align, TRUE);

  unknown_int_type = make_unknown_int();

  char_array_type = make_array_type(char_type, NULL);
  error_type = new_type(tk_error);
  error_type->size = error_type->alignment = make_type_cval(1);
  void_type = new_type(tk_void);
  void_type->size = void_type->alignment = make_type_cval(1);
  ptr_void_type = make_pointer_type(void_type);

  wchar_type = type_for_size_int(target->wchar_t_size, !target->wchar_t_signed);
  wchar_array_type = make_array_type(wchar_type, NULL);
  size_t_type = type_for_size_int(target->size_t_size, TRUE);
  ptrdiff_t_type = type_for_size_int(target->tptr.size, FALSE);
  intptr_type = type_for_size_int(target->tptr.size, TRUE);
}

struct typelist
{
  region sameregion r;
  struct typelist_element *sameregion first;
};

struct typelist_element
{
  struct typelist_element *sameregion next;
  type t;
};

typelist new_typelist(region r)
{
  typelist tl = ralloc(r, struct typelist);
  tl->r = r;
  tl->first = NULL;
  return tl;
}

void typelist_append(typelist tl, type t)
{
  struct typelist_element *nte = ralloc(tl->r, struct typelist_element);
  struct typelist_element *sameregion *last;

  nte->t = t;
  last = &tl->first;
  while (*last)
    last = &(*last)->next;
  *last = nte;
}

bool empty_typelist(typelist tl)
{
  return tl->first == NULL;
}

void typelist_scan(typelist tl, typelist_scanner *scanner)
{
  *scanner = tl->first;
}

type typelist_next(typelist_scanner *scanner)
{
  type t;

  if (!*scanner)
    return NULL;
  t = (*scanner)->t;
  *scanner = (*scanner)->next;
  return t;
}

type_quals type_qualifiers(type t)
{
  /* Arrays don't have qualifiers */
  while (t->kind == tk_array)
    t = t->u.array.arrayof;
  return t->qualifiers;
}

#define Q(name, kind, tq, val) \
bool type_ ## name(type t) \
{ \
  return (type_qualifiers(t) & tq) != 0; \
}
#include "qualifiers.h"
#undef Q

bool type_transparent(type t)
{
  return (type_qualifiers(t) & transparent_qualifier) != 0;
}

bool type_readonly(type t)
{
  return type_const(t) || (type_tagged(t) && type_tag(t)->fields_const);
}

bool type_integral(type t) /* Does not include enum's */
{
  return t->kind == tk_primitive && t->u.primitive < tp_first_floating;
}

bool type_smallerthanint(type t)
{
  return t->kind == tk_primitive && t->u.primitive < tp_unknown_int &&
    cval_intcompare(t->size, int_type->size) < 0;
}

bool type_unsigned(type t)
{
  return t->kind == tk_primitive &&
    (t->u.primitive == tp_unsigned_char ||
     (!target->char_signed && t->u.primitive == tp_char) ||
     t->u.primitive == tp_unsigned_short ||
     t->u.primitive == tp_unsigned_int ||
     t->u.primitive == tp_unsigned_long ||
     t->u.primitive == tp_unsigned_long_long);
}

bool type_floating(type t)
{
  return t->kind == tk_primitive && t->u.primitive >= tp_first_floating;
}

bool type_plain_char(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_char;
}

bool type_signed_char(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_signed_char;
}

bool type_unsigned_char(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unsigned_char;
}

bool type_short(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_short;
}

bool type_unsigned_short(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unsigned_short;
}

bool type_int(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_int;
}

bool type_unsigned_int(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unsigned_int;
}

bool type_long(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_long;
}

bool type_unsigned_long(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unsigned_long;
}

bool type_long_long(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_long_long;
}

bool type_unsigned_long_long(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unsigned_long_long;
}

bool type_unknown_int(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_unknown_int;
}

bool type_float(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_float;
}

bool type_double(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_double;
}

bool type_long_double(type t)
{
  return t->kind == tk_primitive && t->u.primitive == tp_long_double;
}

bool type_char(type t)
{
  return t->kind == tk_primitive &&
    (t->u.primitive == tp_char || t->u.primitive == tp_unsigned_char ||
     t->u.primitive == tp_signed_char);
}

bool type_void(type t)
{
  return t->kind == tk_void;
}

bool type_function(type t)
{
  return t->kind == tk_function && t->u.fn.fkind == tkf_c;
}

bool type_array(type t)
{
  return t->kind == tk_array;
}

bool type_pointer(type t)
{
  return t->kind == tk_pointer;
}

bool type_complex(type t)
{
  return t->kind == tk_complex;
}

bool type_enum(type t)
{
  return t->kind == tk_tagged && t->u.tag->kind == kind_enum_ref;
}

bool type_tagged(type t)
{
  return t->kind == tk_tagged;
}

bool type_struct(type t)
{
  return t->kind == tk_tagged && t->u.tag->kind == kind_struct_ref;
}

bool type_union(type t)
{
  return t->kind == tk_tagged && t->u.tag->kind == kind_union_ref;
}

type type_function_return_type(type t)
{
  assert(t->kind == tk_function);
  return t->u.fn.returns;
}

typelist type_function_arguments(type t)
{
  assert(t->kind == tk_function);
  return t->u.fn.argtypes;
}

bool type_function_varargs(type t)
{
  assert(t->kind == tk_function);
  return t->u.fn.varargs;
}

bool type_function_oldstyle(type t)
{
  assert(t->kind == tk_function);
  return t->u.fn.oldstyle;
}

type type_points_to(type t)
{
  assert(t->kind == tk_pointer);
  return t->u.pointsto;
}

type type_array_of(type t)
{
  assert(t->kind == tk_array);
  return t->u.array.arrayof;
}

type type_array_of_base(type t)
{
  while (t->kind == tk_array)
    t = t->u.array.arrayof;

  return t;
}

expression type_array_size(type t)
{
  assert(t->kind == tk_array);
  return t->u.array.size;
}

cval type_array_size_cval(type t)
/* Returns: number of elements in array type t if known, cval_top otherwise */
{
  known_cst s;

  if (t->u.array.size && (s = t->u.array.size->cst) &&
      (constant_integral(s) || constant_unknown(s)))
    return cval_cast(s->cval, size_t_type);

  return cval_top;
}

tag_declaration type_tag(type t)
{
  assert(t->kind == tk_tagged);
  return t->u.tag;
}

bool type_incomplete(type t)
{
  return t->kind == tk_void ||
    (t->kind == tk_tagged && !t->u.tag->defined) ||
    (t->kind == tk_array &&
     (type_incomplete(t->u.array.arrayof) || !t->u.array.size));
}

/* Return TRUE if two type lists are equal */
static bool type_lists_equal(typelist al1, typelist al2)
{
  struct typelist_element *args1 = al1->first, *args2 = al2->first;

  for (;;)
    {
      if (args1 == 0 && args2 == 0)
	return TRUE;
      /* If one list is shorter than the other,
	 they fail to match.  */
      if (args1 == 0 || args2 == 0)
	return FALSE;

      if (!type_equal(args1->t, args2->t))
	return FALSE;

      args1 = args1->next;
      args2 = args2->next;
    }
}

/* Return TRUE if two function types F1 and F2 are equal. */
bool function_equal(type t1, type t2)
{
  /* Function kinds and return types must match */
  if (!(t1->u.fn.fkind == t2->u.fn.fkind &&
	type_equal(t1->u.fn.returns, t2->u.fn.returns)))
    return FALSE;

  if (!t1->u.fn.oldstyle && !t2->u.fn.oldstyle)
    return t1->u.fn.varargs == t2->u.fn.varargs &&
      type_lists_equal(t1->u.fn.argtypes, t2->u.fn.argtypes);
  else
    return t1->u.fn.oldstyle == t2->u.fn.oldstyle;
}

bool type_equal(type t1, type t2)
{
  return t1->qualifiers == t2->qualifiers && type_equal_unqualified(t1, t2);
}

static bool array_sizes_match(type t1, type t2)
/* Return: TRUE if we think the array sizes of t1, t2 match
 */
{
  known_cst s1 = t1->u.array.size ? t1->u.array.size->cst : NULL;
  known_cst s2 = t2->u.array.size ? t2->u.array.size->cst : NULL;

  // Non-constant array sizes match everything
  // Unknown array sizes match everything too 
  // (XXX: we should check again after instantiation, but we'll leave
  // that to the backend C compiler)
  if (!s1 || !constant_integral(s1) || !s2 || !constant_integral(s2))
    return TRUE;

  return cval_intcompare(s1->cval, s2->cval) == 0;
}

bool type_equal_unqualified(type t1, type t2)
{
  if (t1 == error_type || t2 == error_type)
    return TRUE;

  /* Short-circuit easy case */
  if (t1 == t2)
    return TRUE;

  /* Different classes of types can't be compatible.  */
  if (t1->kind != t2->kind)
    return FALSE;

  /* sameregion, traditional and parentptr qualifiers must always match */
  if ((t1->qualifiers & ~(const_qualifier | volatile_qualifier | restrict_qualifier | transparent_qualifier)) !=
      (t2->qualifiers & ~(const_qualifier | volatile_qualifier | restrict_qualifier | transparent_qualifier)))
    return 0;

  switch (t1->kind)
    {
    case tk_primitive: case tk_complex:
      return t1->u.primitive == t2->u.primitive;

    case tk_void:
      return TRUE;

    case tk_tagged:
      return t1->u.tag == t2->u.tag;

    case tk_pointer:
      return type_equal(t1->u.pointsto, t2->u.pointsto);

    case tk_function:
      return function_equal(t1, t2);

    case tk_array:
      return type_equal(t1->u.array.arrayof, t2->u.array.arrayof) &&
	array_sizes_match(t1, t2);

    case tk_variable:
      return t1->u.tdecl == t2->u.tdecl;

    default: assert(0); return FALSE;
    }
}

/* Return TRUE if T is not affected by default promotions.  */
bool type_self_promoting(type t)
{
  if (t->kind != tk_primitive)
    return TRUE;

  /* Could also use array. Hmm. */
  switch (t->u.primitive)
    {
    case tp_float: case tp_char: case tp_unsigned_char: case tp_signed_char:
    case tp_short: case tp_unsigned_short:
      return FALSE;
    default:
      return TRUE;
    }
}

/* Return TRUE if function type FNTYPE specifies a fixed number of parameters
   and none of their types is affected by default promotions.
   TRUE for oldstyle functions */
bool self_promoting_args(type fntype)
{
  struct typelist_element *parms;

  if (type_function_varargs(fntype))
    return FALSE;

  if (type_function_oldstyle(fntype))
    return TRUE;

  for (parms = type_function_arguments(fntype)->first; parms;
       parms = parms->next)
    if (!type_self_promoting(parms->t))
      return FALSE;

  return TRUE;
}

/* Allow  wait (union {union wait *u; int *i})
   and  wait (union wait *)  to be compatible.  */
static bool weird_parameter_match(type t1, type t2)
{
  tag_declaration t1decl;

  if (type_union(t1)
      && (!(t1decl = type_tag(t1))->name || t1decl->transparent_union)
      && type_size_cc(t1) && type_size_cc(t2))
    {
      cval s1 = type_size(t1), s2 = type_size(t2);

      /* We don't do this for types of unknown size */
      if (cval_isinteger(s1) && cval_isinteger(s2) &&
	  cval_intcompare(s1, s2) == 0)
	{
	  field_declaration field;

	  for (field = t1decl->fieldlist; field; field = field->next)
	    if (type_compatible(field->type, t2))
	      return TRUE;
	}
    }
  return FALSE;
}

static type weird_common_parameter(type t1, type t2)
{
  /* Given  wait (union {union wait *u; int *i} *)
     and  wait (union wait *),
     prefer  union wait *  as type of parm.  */
  if (weird_parameter_match(t1, t2))
    {
      if (pedantic)
	pedwarn("function types not truly compatible in ANSI C");
      return t2;
    }
  return NULL;
}

/* Check two lists of types for compatibility,
   returning 0 for incompatible, 1 for compatible,
   or 2 for compatible with warning.  */
static int type_lists_compatible(typelist al1, typelist al2)
{
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;
  int newval = 0;
  struct typelist_element *args1 = al1->first, *args2 = al2->first;

  while (1)
    {
      if (args1 == 0 && args2 == 0)
	return val;
      /* If one list is shorter than the other,
	 they fail to match.  */
      if (args1 == 0 || args2 == 0)
	return 0;

      if (!(newval = type_compatible_unqualified(args1->t, args2->t)))
	{
	  if (!weird_parameter_match(args1->t, args2->t) &&
	      !weird_parameter_match(args2->t, args1->t))
	    return 0;
	}

      /* type_compatible said ok, but record if it said to warn.  */
      if (newval > val)
	val = newval;

      args1 = args1->next;
      args2 = args2->next;
    }
}

/* Return 1 if two function types F1 and F2 are compatible.
   If either type specifies no argument types,
   the other must specify a fixed number of self-promoting arg types.
   Otherwise, if one type specifies only the number of arguments, 
   the other must specify that number of self-promoting arg types.
   Otherwise, the argument types must match.  */
int function_compatible(type t1, type t2)
{
  typelist args1, args2;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;

  /* Kinds and return types must match */
  if (!(t1->u.fn.fkind == t2->u.fn.fkind &&
	type_compatible(t1->u.fn.returns, t2->u.fn.returns)))
    return 0;

  args1 = t1->u.fn.argtypes;
  args2 = t2->u.fn.argtypes;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (t1->u.fn.oldstyle)
    {
      if (args2 && !self_promoting_args(t2))
	return 0;
#if 0
      /* If one of these types comes from a non-prototype fn definition,
	 compare that with the other type's arglist.
	 If they don't match, ask for a warning (but no error).  */
      if (TYPE_ACTUAL_ARG_TYPES (f1)
	  && 1 != type_lists_compatible(args2, TYPE_ACTUAL_ARG_TYPES (f1)))
	val = 2;
#endif
      return val;
    }
  if (t2->u.fn.oldstyle)
    {
      if (args1 && !self_promoting_args(t1))
	return 0;
#if 0
      if (TYPE_ACTUAL_ARG_TYPES (f2)
	  && 1 != type_lists_compatible(args1, TYPE_ACTUAL_ARG_TYPES (f2)))
	val = 2;
#endif
      return val;
    }

  if (t1->u.fn.varargs != t2->u.fn.varargs)
    return 0;

  /* Both types have argument lists: compare them and propagate results.  */
  return type_lists_compatible(args1, args2);
}

bool type_compatible_unqualified(type t1, type t2)
{
  if (t1 == error_type || t2 == error_type)
    return 1;

  /* Short-circuit easy case */
  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */
  if (t1->kind != t2->kind)
    return 0;

#if 0
  /* The combiners must match too */
  if (t1->combiner != t2->combiner)
    return 0;
#endif

  /* sameregion, traditional and parentptr qualifiers must always match */
  if ((t1->qualifiers & ~(const_qualifier | volatile_qualifier | restrict_qualifier | transparent_qualifier)) !=
      (t2->qualifiers & ~(const_qualifier | volatile_qualifier | restrict_qualifier | transparent_qualifier)))
    return 0;

  switch (t1->kind)
    {
    case tk_primitive: case tk_complex:
      return t1->u.primitive == t2->u.primitive;

    case tk_void:
      return 1;

    case tk_tagged:
      return t1->u.tag == t2->u.tag;

    case tk_pointer:
      return type_compatible(t1->u.pointsto, t2->u.pointsto);

    case tk_function:
      return function_compatible(t1, t2);

    case tk_array:
      return type_compatible(t1->u.array.arrayof, t2->u.array.arrayof) &&
	array_sizes_match(t1, t2);

    case tk_iref:
      return t1->u.iref->itype == t2->u.iref->itype;

    case tk_variable:
      return t1->u.tdecl == t2->u.tdecl;

    default: assert(0); return 0;
    }
}

bool type_compatible(type t1, type t2)
{
  /* Qualifiers must match.  */
  /* GCC appears to allow changes to restrict (see /usr/include/sys/stat.h
     and the decls/defs of stat/lstat in redhat linux 7) */
  /* XXX: investigate more. */
  if ((t1->qualifiers & ~restrict_qualifier) != (t2->qualifiers & ~restrict_qualifier))
    return 0;
  else
    return type_compatible_unqualified(t1, t2);
}

type qualify_type1(type t, type t1)
{
  return make_qualified_type(t, type_qualifiers(t1));
}

type qualify_type2(type t, type t1, type t2)
{
  return make_qualified_type(t, type_qualifiers(t1) | type_qualifiers(t2));
}

type align_type(type t, cval new_alignment)
{
  type nt = copy_type(t);

  nt->alignment = new_alignment;

  return nt;
}

static int common_primitive_type(type t1, type t2)
{
  int pk1 = t1->u.primitive, pk2 = t2->u.primitive;
  int pk = pk1 < pk2 ? pk2 : pk1;

  /* Note: the results of this function depend on the relative sizes of 
     char, short, int, long and long long. The results are only correct
     for a version of C that has types that match those set by types_init
     (which uses those from the C compiler that compiles this file)
  */
  if (pk1 < tp_first_floating && pk2 < tp_first_floating)
    {
      largest_uint s1, s2;

      /* The simple cases */
      if (pk1 == tp_unknown_int || pk2 == tp_unknown_int)
	return tp_unknown_int;

      s1 = cval_uint_value(t1->size);
      s2 = cval_uint_value(t2->size);
      if (s1 < s2)
	return pk2;
      else if (s1 > s2)
	return pk1;

      /* The pain starts, see 6.3.1.8 of c9x */
      /* If the sizes are the same, then we can't have a tp_[u]int<n> or a 
	 tp_char/short/int/long/etc pair (as we only have tp_[u]int<n> if there
	 is no corresponding integer type of the same size. So we can compare rank
	 by comparing pk1 and pk2 */
      assert(!((pk1 < tp_char && pk2 >= tp_char) || (pk1 >= tp_char && pk2 < tp_char)));

      /* the higher rank wins, and if either of the types is unsigned, the
	 result is (thus unsigned short + int == unsigned int if
	 sizeof(short) == sizeof(int) */
      if ((type_unsigned(t1) || type_unsigned(t2)) && !type_unsigned(primitive_types[pk]))
	/* A bit inefficient, admittedly */
	pk = make_unsigned_type(primitive_types[pk])->u.primitive;

      return pk;
    }

  /* Floating point types follow the order specified in the enum and win
     over all integral types */
  return pk;
}

/* Return the common type of two types.

   This is used in two cases:
    - when type_compatible(t1, t2) is true, to pick a merged type for
    declarations
    - to merge two types that are compatible in some expressions (e.g. ?:,
    arithmetic) */
type common_type(type t1, type t2)
{
  type rtype;
  data_declaration combiner;

  /* Save time if the two types are the same.  */
  if (t1 == t2)
    return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_type)
    return t2;
  if (t2 == error_type)
    return t1;

  combiner = NULL;
  if (t1->combiner == t2->combiner)
    combiner = t1->combiner;

  /* Special enum handling: if same enum, just merge qualifiers.  otherwise
     treat an enum type as the unsigned integer type of the same width.
     Note that gcc does not check for the same enum. Some weird behaviour...
     (see enum2.c)
  */
  if (type_enum(t1))
    {
      if (type_equal_unqualified(t1, t2))
	return make_combiner_type(qualify_type2(t1, t1, t2), combiner);
      t1 = qualify_type1(type_for_size(type_size(t1), TRUE), t1);
    }
  if (type_enum(t2))
    t2 = qualify_type1(type_for_size(type_size(t1), TRUE), t2);

  /* If one type is complex, form the common type of the non-complex
     components, then make that complex. */
  if (type_complex(t1) || type_complex(t2))
    {
      int pk = common_primitive_type(t1, t2);
      assert((t1->kind == tk_primitive || t1->kind == tk_complex) && 
	     (t2->kind == tk_primitive || t2->kind == tk_complex));
      rtype = complex_types[pk];
    }
  else
    switch (t1->kind)
      {
      case tk_primitive:
	{
	  int pk = common_primitive_type(t1, t2);
	  assert(t2->kind == tk_primitive);
	  rtype = primitive_types[pk];
	  break;
	}

      case tk_void: case tk_tagged: case tk_variable:
	rtype = t1;
	break;

      case tk_pointer:
	rtype = make_pointer_type(common_type(t1->u.pointsto, t2->u.pointsto));
	break;

      case tk_array:
	{
	  /* Merge the element types, and have a size if either arg has one.  */
	  type element_type =
	    common_type(t1->u.array.arrayof, t2->u.array.arrayof);
	  expression size1 = t1->u.array.size, size2 = t2->u.array.size;

	  rtype = make_array_type(element_type, size1 ? size1 : size2);
	  break;
	}

      case tk_function:
	/* Function types: prefer the one that specified arg types.
	   If both do, merge the arg types.  Also merge the return types.  */
	{
	  type valtype = common_type(t1->u.fn.returns, t2->u.fn.returns);
	  typelist args;
	  bool oldstyle, varargs;

	  if (t1->u.fn.oldstyle && t2->u.fn.oldstyle)
	    {
	      args = NULL;
	      oldstyle = TRUE;
	      varargs = FALSE;
	    }
	  else if (t1->u.fn.oldstyle)
	    {
	      args = t2->u.fn.argtypes;
	      oldstyle = FALSE;
	      varargs = t2->u.fn.varargs;
	    }
	  else if (t2->u.fn.oldstyle)
	    {
	      args = t1->u.fn.argtypes;
	      oldstyle = FALSE;
	      varargs = t1->u.fn.varargs;
	    }
	  else
	    {
	      /* If both args specify argument types, we must merge the two
		 lists, argument by argument.  */
	      struct typelist_element *args1 = t1->u.fn.argtypes->first;
	      struct typelist_element *args2 = t2->u.fn.argtypes->first;
	      type argtype;

	      oldstyle = FALSE;
	      varargs = t1->u.fn.varargs;
	      args = new_typelist(t1->u.fn.argtypes->r);

	      while (args1)
		{
		  (void) ((argtype = weird_common_parameter(args1->t, args2->t)) ||
			  (argtype = weird_common_parameter(args2->t, args1->t)) ||
			  (argtype = common_type(args1->t, args2->t)));
		  typelist_append(args, argtype);

		  args1 = args1->next;
		  args2 = args2->next;
		}

	    }
	  rtype = make_function_type(valtype, args, varargs, oldstyle);
	  /* Hack up the function kind */
	  rtype->u.fn.fkind = t1->u.fn.fkind;
	  break;
	}

      default:
	assert(0); return NULL;
      }

  rtype = qualify_type2(rtype, t1, t2);

  return make_combiner_type(rtype, combiner);
}


type type_base(type t)
{
  while (t->kind == tk_array)
    t = t->u.array.arrayof;

  return t;
}

bool type_integer(type t)
{
  return type_integral(t) || type_enum(t);
}

bool type_real(type t)
{
  return type_integer(t) || type_floating(t);
}

bool type_arithmetic(type t)
{
  return type_real(t) || type_complex(t);
}

bool type_scalar(type t)
{
  return type_arithmetic(t) || type_pointer(t);
}

bool type_aggregate(type t)
{
  return type_struct(t) || type_union(t);
}

type make_unsigned_type(type t)
{
  if (t->kind != tk_primitive)
    return t;

  if (t->u.primitive == tp_char || t->u.primitive == tp_signed_char)
    return qualify_type1(unsigned_char_type, t);
  if (t->u.primitive == tp_short)
    return qualify_type1(unsigned_short_type, t);
  if (t->u.primitive == tp_int)
    return qualify_type1(unsigned_int_type, t);
  if (t->u.primitive == tp_long)
    return qualify_type1(unsigned_long_type, t);
  if (t->u.primitive == tp_long_long)
    return qualify_type1(unsigned_long_long_type, t);

  return t;
}

static type_element rid2ast(region r, location loc, int keyword, type_element rest)
{
  type_element ast = CAST(type_element, new_rid(r, loc, keyword));
  ast->next = CAST(node, rest);
  return ast;
}

static type_element qualifier2ast(region r, location loc, int keyword, type_element rest)
{
  type_element ast = CAST(type_element, new_qualifier(r, loc, keyword));
  ast->next = CAST(node, rest);
  return ast;
}

static type_element qualifiers2ast(region r, location loc, type_quals quals,
				   type_element rest)
{
  if (quals & volatile_qualifier)
    rest = qualifier2ast(r, loc, volatile_qualifier, rest);
  if (quals & const_qualifier)
    rest = qualifier2ast(r, loc, const_qualifier, rest);
  return rest;
}

static type_element primitive2ast(region r, location loc, int primitive,
				  type_element rest)
{
  bool isunsigned = FALSE;
  int keyword;

  switch (primitive)
    {
    case tp_unsigned_char:
      isunsigned = TRUE;
    case tp_char:
      keyword = RID_CHAR;
      break;
    case tp_signed_char:
      return rid2ast(r, loc, RID_SIGNED, rid2ast(r, loc, RID_CHAR, rest));
    case tp_unsigned_short:
      isunsigned = TRUE;
    case tp_short:
      keyword = RID_SHORT;
      break;
    case tp_unsigned_int:
      isunsigned = TRUE;
    case tp_int:
      keyword = RID_INT;
      break;
    case tp_unsigned_long:
      isunsigned = TRUE;
    case tp_long:
      keyword = RID_LONG;
      break;
    case tp_unsigned_long_long:
      isunsigned = TRUE;
    case tp_long_long:
      keyword = RID_LONG;
      rest = rid2ast(r, loc, RID_LONG, rest);
      break;
    case tp_float:
      keyword = RID_FLOAT;
      break;
    case tp_double:
      keyword = RID_DOUBLE;
      break;
    case tp_long_double:
      keyword = RID_DOUBLE;
      rest = rid2ast(r, loc, RID_LONG, rest);
      break;
    default:
      assert(0);
      keyword = RID_INT; break;
    }

  rest = rid2ast(r, loc, keyword, rest);
  if (isunsigned)
    rest = rid2ast(r, loc, RID_UNSIGNED, rest);

  return rest;
}

#define UNNAMED_STRUCT_PREFIX "__nesc_unnamed"

void name_tag(tag_declaration tag)
{
  /* Name unnamed structs, unions or enums */
  if (!tag->name)
    {
      static long nextid = 4242;
      char tagname[sizeof(UNNAMED_STRUCT_PREFIX) + 20];

      sprintf(tagname, UNNAMED_STRUCT_PREFIX "%ld", nextid++);
      tag->definition->word1 = new_word(parse_region, dummy_location,
					str2cstring(parse_region, tagname));
      tag->name = tag->definition->word1->cstring.data;
    }
}

static type_element tag2ast(region r, location loc, tag_declaration tag,
			    type_element rest)
{
  tag_ref tr;

  name_tag(tag);
  tr = newkind_tag_ref(r, tag->kind, loc, 
		       new_word(r, loc, str2cstring(r, tag->name)),
		       NULL, NULL, FALSE);

  tr->tdecl = tag;
  tr->next = CAST(node, rest);

  /* creating a type naming a tag that shadows another is tricky as
     the placement of the type affects its meaning. 

     The current use of type2ast is for declaring temporaries. These are
     placed at the start of the closest enclosing block. This is correct as
     long as the temporary does not rely on a tag declared in the same
     block that shadows a tag in an enclosing scope (as the temporary would
     then erroneously refer to the enclosing tag). The simplest check which
     detects this situation is any use of a shadowed tag (which I am
     assuming are very rare anyway).

     The correct solution is that if the type of temporary x refers to tags
     t1...tn declared in the current block, then x should be placed just
     before the declaration of the latest of ti, the tag in t1...tn which
     was declared latest, and the declaration of x should be preceded by
     'struct/union/enum ti;'. This is somewhat painful.

     To avoid any problems, compile with error_shadow = warn_shadow = 1
     (this is what RC does) */

  assert(!tag->shadowed);

  return CAST(type_element, tr);
}

static declaration parameter2ast(region r, location loc, type t)
{
  variable_decl vd;
  data_decl dd;
  declarator tdeclarator;
  type_element tmodifiers;

  /* Build AST for the declaration */
  type2ast(r, loc, t, NULL, &tdeclarator, &tmodifiers);
  vd = new_variable_decl(r, loc, tdeclarator, NULL, NULL, NULL, NULL);
  vd->declared_type = t;
  dd = new_data_decl(r, loc, tmodifiers, CAST(declaration, vd));

  return CAST(declaration, dd);
}

/* Build AST nodes such that "MODIFIERS D" represents the declaration of
   "T INSIDE", at location loc, allocating in region r */
void type2ast(region r, location loc, type t, declarator inside,
	      declarator *d, type_element *modifiers)
{
  /* XXX: De-recursify */
  type_element qualifiers = qualifiers2ast(r, loc, t->qualifiers, NULL);

  switch (t->kind)
    {
    case tk_primitive:
      *modifiers = primitive2ast(r, loc, t->u.primitive, qualifiers);
      *d = inside;
      break;
    case tk_complex:
      *modifiers =
	rid2ast(r, loc, RID_COMPLEX, 
	primitive2ast(r, loc, t->u.primitive, qualifiers));
      *d = inside;
      break;
    case tk_tagged:
      *modifiers = tag2ast(r, loc, t->u.tag, qualifiers);
      *d = inside;
      break;
    case tk_void:
      *modifiers = rid2ast(r, loc, RID_VOID, qualifiers);
      *d = inside;
      break;
    case tk_pointer:
      if (qualifiers)
	inside = CAST(declarator,
		      new_qualified_declarator(r, loc, inside, qualifiers));
      inside = CAST(declarator,
		    new_pointer_declarator(r, loc, inside));
      type2ast(r, loc, t->u.pointsto, inside, d, modifiers);
      break;
    case tk_array:
      assert(qualifiers == NULL);
      inside = CAST(declarator,
		    new_array_declarator(r, loc, inside, t->u.array.size));
      type2ast(r, loc, t->u.array.arrayof, inside, d, modifiers);
      break;
    case tk_function: {
      declaration parms;

      assert(t->u.fn.fkind == tkf_c); /* XXX: not done for nesC stuff yet
					 (not needed yet) */
      /* XXX: doesn't rebuild fn qualifiers. Are we generating C here
	 or not ? */
      /* XXX: Should build environment for parameters */
      if (t->u.fn.oldstyle)
	parms = NULL;
      else if (empty_typelist(t->u.fn.argtypes))
	parms = parameter2ast(r, loc, void_type);
      else
	{
	  struct typelist_element *args = t->u.fn.argtypes->first;
	  declaration *lastparm = &parms;

	  while (args)
	    {
	      *lastparm = parameter2ast(r, loc, args->t);
	      lastparm = (declaration *)&(*lastparm)->next;
	      args = args->next;
	    }
	  if (t->u.fn.varargs)
	    {
	      *lastparm = CAST(declaration, new_ellipsis_decl(r, loc));
	      lastparm = (declaration *)&(*lastparm)->next;
	    }
	  *lastparm = NULL;
	}
      inside = CAST(declarator,
		    new_function_declarator(r, loc, inside, parms, NULL, NULL, NULL));
      type2ast(r, loc, t->u.fn.returns, inside, d, modifiers);
      break;
    }
    default: assert(0); break;
    }
}

bool type_contains_pointers(type t)
{
  field_declaration field;

  if (type_pointer(t))
    return TRUE;

  if (type_array(t))
    return type_contains_pointers(type_array_of(t));

  if (!type_aggregate(t))
    return FALSE;

  for (field = type_tag(t)->fieldlist; field; field = field->next)
    if (type_contains_pointers(field->type))
      return TRUE;

  return FALSE;
}

bool type_contains_union_with_pointers(type t)
{
  field_declaration field;

  if (type_array(t))
    return type_contains_union_with_pointers(type_array_of(t));

  if (type_union(t))
    return type_contains_pointers(t);

  if (!type_struct(t))
    return FALSE;

  for (field = type_tag(t)->fieldlist; field; field = field->next)
    if (type_contains_union_with_pointers(field->type))
      return TRUE;

  return FALSE;
}

type type_default_conversion(type from)
{
  if (type_enum(from))
    from = type_tag(from)->reptype;

  if (type_smallerthanint(from))
    {
      /* Traditionally, unsignedness is preserved in default promotions. */
      if (flag_traditional && type_unsigned(from))
	return unsigned_int_type;
      else
	return int_type;
    }
  /* Note: if we had a type of same size as int, but of lesser rank, we should be
     returning one of int/unsigned int here, but we don't support that kind of
     type */

  if (flag_traditional && !flag_allow_single_precision && type_float(from))
    return double_type;

  if (type_function(from))
    return make_pointer_type(from);

  if (type_array(from))
    return make_pointer_type(type_array_of(from));

  return from;
}

/* would be called type_default_function_array_conversion in gcc 3.x */
type type_default_conversion_for_assignment(type from)
{
  if (type_array(from) || type_function(from))
    return type_default_conversion(from);
  else
    return from;
}

type function_call_type(function_call fcall)
{
  type fntype = fcall->arg1->type;

  if (type_pointer(fntype))
    fntype = type_points_to(fntype);

  assert(type_functional(fntype));

  return fntype;
}

cval type_size(type t)
/* Requires: type_size_cc(t)
   Returns: size of type t in a cval. The cval is either unknown (for types
     derived in some way from template arguments) or an unsigned number
*/
{
  assert(type_size_cc(t));

  if (type_tagged(t))
    return t->u.tag->size;

  if (type_array(t))
    return cval_times(type_array_size_cval(t), type_size(t->u.array.arrayof));

  return t->size;
}

largest_uint type_size_int(type t)
/* Requires: type_size_cc(t) && cval_isinteger(type_size(t))
     (i.e., t not variable or unknown size)
   Returns: size of t
*/
{
  return cval_uint_value(type_size(t));
}

cval type_alignment(type t)
{
  assert(type_has_size(t));

  if (!cval_istop(t->alignment)) /* Possibly overridden alignment */
    return t->alignment;

  if (type_tagged(t))
    return t->u.tag->alignment;

  if (type_array(t))
    return type_alignment(t->u.array.arrayof);

  /* We should never get here (all types have non-zero alignment) */
  assert(0);

  return t->alignment;
}

/* True if the sizeof of t is a compile-time constant (known or unknown)
 */
bool type_size_cc(type t)
{
  if (!type_has_size(t))
    return FALSE;

  if (type_tagged(t))
    return !cval_istop(t->u.tag->size);

  if (type_array(t))
    return !cval_istop(type_array_size_cval(t)) &&
      type_size_cc(t->u.array.arrayof);

  return TRUE;
}

bool type_has_size(type t)
{
  /* Similar, but not identical to, type_incomplete */
  return type_void(t) || !type_incomplete(t);
}


char *qualifier_name(type_quals q)
{
  switch (q)
    {
    default: abort(); return NULL;
#define Q(name, kind, tq, val) case tq: return #name;
#include "qualifiers.h"
#undef Q
    }
}

/* nesc types */
bool type_command(type t)
{
  return t->kind == tk_function && t->u.fn.fkind == tkf_command;
}

bool type_event(type t)
{
  return t->kind == tk_function && t->u.fn.fkind == tkf_event;
}

bool type_task(type t)
{
  return t->kind == tk_function && t->u.fn.fkind == tkf_task;
}

bool type_generic(type t)
{
  return t->kind == tk_function && t->u.fn.fkind == tkf_generic;
}

type make_command_type(type t, typelist argtypes, bool varargs)
{
  type newt = make_function_type(t, argtypes, varargs, FALSE);
  newt->u.fn.fkind = tkf_command;
  return newt;
}

type make_event_type(type t, typelist argtypes, bool varargs)
{
  type newt = make_function_type(t, argtypes, varargs, FALSE);
  newt->u.fn.fkind = tkf_event;
  return newt;
}

type make_task_type(type t, typelist argtypes, bool varargs)
{
  type newt = make_function_type(t, argtypes, varargs, FALSE);
  newt->u.fn.fkind = tkf_task;
  return newt;
}

type make_generic_type(type t, typelist argtypes)
{
  type newt = make_function_type(t, argtypes, FALSE, FALSE);
  newt->u.fn.fkind = tkf_generic;
  return newt;
}

type make_interface_type(data_declaration itype)
{
  type nt = new_type(tk_iref);
  nt->u.iref = itype;

  /* These are not yet stored, but I'll assume they might be like
     pointers some day... */
  /* ASSUME: all pointers are the same */
  nt->size = make_type_cval(target->tptr.size);
  nt->alignment = make_type_cval(target->tptr.align);

  return nt;
}

bool type_interface(type t)
{
  return t->kind == tk_iref;
}

data_declaration type_iref(type t)
{
  assert(type_interface(t));
  return t->u.iref;
}

bool type_functional(type t)
{
  return t->kind == tk_function && t->u.fn.fkind != tkf_generic;
}

type make_combiner_type(type t, data_declaration combiner)
{
  type nt;

  if (!combiner)
    return t;

  nt = copy_type(t);
  nt->combiner = combiner;

  return nt;
}

data_declaration type_combiner(type t)
{
  return t->combiner;
}

/* Type variables */
type make_variable_type(data_declaration tdecl)
/* Requires: tdecl->kind == decl_typedef.
*/
{
  type nt = new_type(tk_variable);
  nt->u.tdecl = tdecl;

  /* Type variables have unknown size and alignment */
  nt->size = nt->alignment = cval_unknown;

  return nt;
}

bool type_variable(type t)
{
  return t->kind == tk_variable;
}

data_declaration type_variable_decl(type t)
{
  assert(type_variable(t));
  return t->u.tdecl;
}

type instantiate_type(type t)
/* Effects: Instantiate a type with type variables based on the instantiation
     of the variables and tag declarations. These are found in 
       type_variable_decl(vartype)->instantiation->type for variables
       type_tag(tagtype)->instantiation for tags
   Returns: The instantiated type
*/
{
  type newt = NULL;

  switch (t->kind)
    {
    case tk_tagged:
      if (t->u.tag->instantiation)
	newt = make_tagged_type(t->u.tag->instantiation);
      break;

    case tk_pointer:
      newt = make_pointer_type(instantiate_type(t->u.pointsto));
      break;

    case tk_function: {
      type ret = instantiate_type(t->u.fn.returns);
      typelist args = NULL;

      if (t->u.fn.argtypes)
	{
	  typelist old = t->u.fn.argtypes;
	  struct typelist_element *scan = old->first;

	  args = new_typelist(old->r);
	  while (scan)
	    {
	      typelist_append(args, instantiate_type(scan->t));
	      scan = scan->next;
	    }
	}
      newt = make_function_type(ret, args, t->u.fn.varargs, t->u.fn.oldstyle);
      newt->u.fn.fkind = t->u.fn.fkind;
      break;
    }
    case tk_array:
#if 0
      newt = make_array_type(instantiate_type(t->u.array.arrayof),
			     t->u.array.size->hmm);
#endif
      break;

    case tk_iref:
      if (t->u.iref->instantiation)
	newt = make_interface_type(t->u.iref->instantiation);
      break;

    case tk_variable:
      if (t->u.tdecl->instantiation)
	newt = t->u.tdecl->instantiation->type;
      break;

    default:
      break;
    }

  if (newt)
    return make_qualified_type(newt, t->qualifiers);
  else
    return t;
}

static char *primname[] = {
  NULL,
  "int16_t",
  "uint16_t",
  "int32_t",
  "uint32_t",
  "int64_t",
  "uint64_t",
  "char",
  "signed char",
  "unsigned char",
  "short",
  "unsigned short",
  "int",
  "unsigned int",
  "long",
  "unsigned long",
  "long long",
  "unsigned long long",
  "float",
  "double",
  "long double"
};

static const char *rconcat(region r, const char *s1, const char *s2)
{
  int l = strlen(s1) + strlen(s2) + 1;
  char *s = rstralloc(r, l);

  strcpy(s, s1);
  strcat(s, s2);
  
  return s;
}

static const char *add_qualifiers(region r, type_quals qs, const char *to)
{
  type_quals q;

  for (q = 1; q < last_qualifier; q <<= 1)
    if (qs & q)
      {
	to = rconcat(r, " ", to);
	to = rconcat(r, qualifier_name(q), to);
      }

  return to;
}

static const char *add_parens(region r, const char *to)
{
  to = rconcat(r, "(", to);
  to = rconcat(r, to, ")");
  return to;
}

static void split_type_name(region r, type t, const char **prefix,
			    const char **decls, bool decls_is_star)
{
  const char *basic;

  switch (t->kind)
    {
    case tk_primitive:
      basic = primname[t->u.primitive];
      basic = add_qualifiers(r, t->qualifiers, basic);
      break;
    case tk_complex:
      basic = rconcat(r, "complex ", primname[t->u.primitive]);
      basic = add_qualifiers(r, t->qualifiers, basic);
      break;
    case tk_void:
      basic = "void";
      basic = add_qualifiers(r, t->qualifiers, basic);
      break;
    case tk_pointer: 
      *decls = add_qualifiers(r, t->qualifiers, *decls);
      *decls = rconcat(r, "*", *decls);
      split_type_name(r, t->u.pointsto, &basic, decls, TRUE);
      break;
    case tk_array:
      /* can't have qualifiers here - see make_qualified_type */
      if (decls_is_star)
	*decls = add_parens(r, *decls);
      *decls = rconcat(r, *decls, "[]");
      split_type_name(r, t->u.array.arrayof, &basic, decls, FALSE);
      break;
    case tk_function: {
      const char *args= "";

      if (!t->u.fn.oldstyle)
	{
	  typelist_scanner scanargs;
	  type argt;
	  bool first = TRUE;

	  typelist_scan(t->u.fn.argtypes, &scanargs);
	  while ((argt = typelist_next(&scanargs)))
	    {
	      if (!first)
		args = rconcat(r, args, ", ");
	      args = rconcat(r, args, type_name(r, argt));
	      first = FALSE;
	    }
	  if (t->u.fn.varargs)
	    args = rconcat(r, args, ", ...");
	}

      if (decls_is_star)
	*decls = add_parens(r, *decls);

      if (t->qualifiers)
	/* This isn't legal C syntax, but seems the reasonable rep */
	*decls = add_parens(r, add_qualifiers(r, t->qualifiers, *decls));

      *decls = rconcat(r, *decls, add_parens(r, args));

      split_type_name(r, t->u.fn.returns, &basic, decls, FALSE);
      break;
    }
    case tk_tagged: {
      tag_declaration tdecl = t->u.tag;

      switch (tdecl->kind)
	{
	case kind_struct_ref: basic = "struct "; break;
	case kind_union_ref: basic = "union "; break;
	case kind_enum_ref: basic = "enum "; break;
	default: assert(0); basic = ""; break;
	}
      if (tdecl->container)
	{
	  basic = rconcat(r, basic, tdecl->container->name);
	  basic = rconcat(r, basic, ".");
	}
      if (tdecl->name)
	basic = rconcat(r, basic, tdecl->name);
      else
	basic = rconcat(r, basic, "/*anon*/");
      basic = add_qualifiers(r, t->qualifiers, basic);
      break;
    }
    default: /* for bugs, tk_error tk_iref */
      basic = "error";
      break;
    }

  *prefix = basic;
}

const char *type_name(region r, type t)
{
  const char *prefix, *decls;

  decls = "";
  split_type_name(r, t, &prefix, &decls, FALSE);

  if (decls[0])
    return rconcat(r, prefix, rconcat(r, " ", decls));
  else
    return prefix;
}


enum {
  TYPE_HASH_PRIMITIVE = 1,
  TYPE_HASH_COMPLEX = TYPE_HASH_PRIMITIVE + tp_last,
  TYPE_HASH_ERROR = TYPE_HASH_COMPLEX + tp_last,
  TYPE_HASH_VOID,
  TYPE_HASH_FUNCTION
};


/* Return a hash value to distinguish types.  Note that we are much
   less careful here than in type_equal, since hash collisions only
   effect performance. */
unsigned long type_hash(type t)
{
  switch (t->kind)
    {
    case tk_error:
      return TYPE_HASH_ERROR;

    case tk_primitive: 
      return TYPE_HASH_PRIMITIVE + t->u.primitive;

    case tk_complex:
      return TYPE_HASH_COMPLEX + t->u.primitive;

    case tk_void:
      return TYPE_HASH_VOID;

    case tk_tagged:
      return hash_ptr(t->u.tag);

    case tk_pointer:
      return (type_hash(t->u.pointsto) << 1) ^ 0x51353157;

    case tk_function:
      return TYPE_HASH_FUNCTION;

    case tk_array:
      return (type_hash(t->u.array.arrayof) << 1) ^ 0x40142453 ;

    default: 
      assert(0);
    }
}


/* True if an assignment to type child could modify a value of type parent
   according to the ANSI C rules.
   Note: child cannot be an array type (assignments to arrays do not exist
   in C)
*/
bool type_contains(type parent, type child)
{
  assert(!type_array(child));

  /* Short-circuit easy case */
  if (parent == child)
    return TRUE;

  /* Char writes can be used on any value */
  if (type_char(child))
    return TRUE;

  switch (parent->kind)
    {
    default:
      /* for primitive, void, function */
      return type_equal_unqualified(parent, child);

    case tk_complex:
      /* true if child is primitive or complex and same base primitive type */
      return (child->kind == tk_primitive || child->kind == tk_complex) &&
	parent->u.primitive == child->u.primitive;

    case tk_tagged: {
      /* Same tags -> yes. Otherwise, for structs, unions: true if parent
	 has a field type that contains child */
      field_declaration field;

      if (child->kind == tk_tagged && parent->u.tag == child->u.tag)
	return TRUE;

      if (parent->u.tag->kind == kind_enum_ref)
	return FALSE;

      for (field = parent->u.tag->fieldlist; field; field = field->next)
	if (type_contains(field->type, child))
	  return TRUE;

      return FALSE;
    }

    case tk_pointer:
      /* base types must match, but can have different qualifiers
	 (this is different from type_equal) */
      return 
	child->kind == tk_pointer &&
	type_equal_unqualified(parent->u.pointsto, child->u.pointsto);

    case tk_array: {
      type base_parent = parent;
    
      while (base_parent->kind == tk_array)
	base_parent = base_parent->u.array.arrayof;

      return type_contains(base_parent, child);
    }
    }
}
