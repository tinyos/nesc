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

#ifndef TYPES_H
#define TYPES_H

/* Type sizes, alignments and offsets are represented as cvals, and can be
   one of: 
   - top: variable-sized types
   - unknown: derived from a template argument
   - an unsigned number (preferably created with make_type_cval)
     (note: must be a cval_uint!)

   These numbers represent bytes, except in field offsets which use bits
*/

#include "cval.h"

#include "decls.h"

typedef enum 
{
  no_qualifiers = 0, 
  transparent_qualifier = 1,
#define Q(name, kind, qual, val) qual = val,
#include "qualifiers.h"
#undef Q
  last_qualifier
} type_quals;

extern type float_type, double_type, long_double_type, 
  int_type, unsigned_int_type, long_type, unsigned_long_type,
  long_long_type, unsigned_long_long_type, short_type, unsigned_short_type,
  char_type, char_array_type, wchar_type, wchar_array_type,
  unsigned_char_type, signed_char_type, void_type, ptr_void_type,
  size_t_type, ptrdiff_t_type, intptr_type, unknown_int_type,
  unknown_number_type, const_ptr_void_type;

extern type error_type;

void init_types(void);

void set_typedef_type(data_declaration def, bool network);
/* Requires: def->kind == decl_typedef
   Effects: Sets def's type to remember the typedef it comes from
     If network is true, the type becomes a network base type
*/

data_declaration type_typedef(type t);
/* Returns: the typedef t comes from, or NULL if none
*/

data_declaration type_networkdef(type t);
/* Requires: type_network_base_type(t)
   Returns: the network base type definition for t
*/

/* Build types */

/* Return the 'complex t' version of basic type t (one of the integral or
   floating-point types) */
type make_complex_type(type t);

/* Return the base type of complex type t (one of the integral or
   floating-point types) */
type make_base_type(type t);

/* Return the type t with it's qualifiers set to qualifiers (old qualifiers
   are ignored). This is illegal for array types */
type make_qualified_type(type t, type_quals qualifiers);
type qualify_type1(type t, type t1);
type qualify_type2(type t, type t1, type t2);

/* Return 't' modified to have alignment 'new_alignment' */
type align_type(type t, cval new_alignment);

/* Return TRUE if t's alignment was changed with align_type */
bool type_realigned(type t);

/* Return type 'pointer to t' (unqualified) */
type make_pointer_type(type t);

/* Return type 'array [size] of t'. size is optional */
type make_array_type(type t, expression size);

/* Return type 'function with argument types argtypes returning t'.
   If oldstyle is true, this is an oldstyle function type and
   argtypes is NULL */
type make_function_type(type t, typelist argtypes, bool varargs, bool oldstyle);

type build_function_type(region r, type returns, ...);

/* Return the tagged type whose declaration is d */
type make_tagged_type(tag_declaration d);

typelist new_typelist(region r);
void typelist_append(typelist l, type t);

bool empty_typelist(typelist l);

/* Scanning */
typedef struct typelist_element *typelist_scanner;
void typelist_scan(typelist tl, typelist_scanner *scanner);
type typelist_next(typelist_scanner *scanner);

/* Size and alignment */
cval type_size(type t); /* Requires: type_size_cc(t) */
cval type_alignment(type t);
largest_uint type_size_int(type t);
/* Requires: type_size_cc(t) && cval_isinteger(type_size(t))
     (i.e., t not variable or unknown size)
   Returns: size of t
*/

/* True if t has a size (void or not incomplete) */
bool type_has_size(type t);
/* True if the sizeof of t is a compile-time constant */
bool type_size_cc(type t);
/* Note: type_size_cc => type_has_size */

type common_type(type t1, type t2);

bool type_equal(type t1, type t2);
bool type_equal_unqualified(type t1, type t2);
bool type_compatible(type t1, type t2);
bool type_compatible_unqualified(type t1, type t2);

/* Return TRUE if T is not affected by default promotions.  */
bool type_self_promoting(type t);

bool type_incomplete(type t);

/* Return name of qualifier q (must not be a qualifier set) */
char *qualifier_name(type_quals q);

type_quals type_qualifiers(type t);
#define Q(name, kind, tq, val) bool type_ ## name(type t);
#include "qualifiers.h"
#undef Q
bool type_transparent(type t);
bool type_readonly(type t);

bool type_plain_char(type t);
bool type_signed_char(type t);
bool type_unsigned_char(type t);
bool type_short(type t);
bool type_unsigned_short(type t);
bool type_int(type t);
bool type_unsigned_int(type t);
bool type_long(type t);
bool type_unsigned_long(type t);
bool type_long_long(type t);
bool type_unsigned_long_long(type t);
bool type_unknown_int(type t);
bool type_long_double(type t);
bool type_unknown_number(type t);
bool type_unknown(type t); /* unknown_int or unknown_number */

bool type_tagged(type t);
bool type_integral(type t);	/* Does not include enum's */
bool type_floating(type t);
bool type_complex(type t);
bool type_float(type t);
bool type_double(type t);
bool type_void(type t);
bool type_char(type t);
bool type_function(type t);
bool type_array(type t);
bool type_pointer(type t);
bool type_enum(type t);
bool type_struct(type t);
bool type_attribute(type t); 	/* For internal use for @attributes */
bool type_union(type t);
bool type_integer(type t);	/* Does include enum's */
bool type_unsigned(type t);
bool type_smallerthanint(type t);
bool type_real(type t);
bool type_arithmetic(type t);
bool type_scalar(type t);
bool type_aggregate(type t);	/* struct or union */

type make_unsigned_type(type t);

type type_function_return_type(type t);
typelist type_function_arguments(type t);
bool type_function_varargs(type t);
bool type_function_oldstyle(type t);

/* Return TRUE if function type FNTYPE specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */
bool self_promoting_args(type fntype);

type type_points_to(type t);
type type_array_of(type t);
type type_array_of_base(type t);
expression type_array_size(type t);
cval type_array_size_cval(type t);
/* Returns: number of elements in array type t if known, cval_top otherwise */
tag_declaration type_tag(type t);
type type_base(type t);

/* Build AST nodes such that "MODIFIERS D" represents the declaration of
   "T INSIDE", at location loc, allocating in region r */
void type2ast(region r, location loc, type t, declarator inside,
	      declarator *d, type_element *modifiers);

bool type_contains_pointers(type t);
bool type_contains_cross_pointers(type t);
bool type_contains_qualified_pointers(type t); /* True if any sameregion/traditional ptrs */
bool type_contains_union_with_pointers(type t);

type type_default_conversion(type from);
type type_default_conversion_for_assignment(type from);
type function_call_type(function_call fcall);

void name_tag(tag_declaration tag);

/* Return the integral type of size 'size', unsigned if 'isunsigned' is true */
type type_for_size(cval size, bool isunsigned);

type type_for_cval(cval c, bool isunsigned);

/* nesc type extensions */
type make_interface_type(data_declaration itype);
bool type_interface(type t);
data_declaration type_iref(type t);

type make_component_type(data_declaration ctype);
bool type_component(type t);
data_declaration type_cref(type t);

/* Similar to functions, but different to avoid surprising behaviour... */
bool type_command(type t);
bool type_event(type t);
bool type_task(type t);
bool type_functional(type t); /* all of the above, and type_function */

bool type_generic(type t); /* for generic interfaces */

type make_command_type(type t, typelist argtypes, bool varargs);
type make_event_type(type t, typelist argtypes, bool varargs);
type make_task_type(type t, typelist argtypes, bool varargs);
type make_generic_type(type t, typelist argtypes);

type make_combiner_type(type t, data_declaration combiner);
data_declaration type_combiner(type t);

bool type_network_base_type(type t);
bool type_network(type t);
type type_network_platform_type(type t);
/* Requires: type_network_base_type(t)
   Returns: A non-network type with the same size and signedness as t
     Note that such a type is platform-dependent
*/

/* Type variables */
type make_variable_type(data_declaration tdecl);
/* Requires: tdecl->kind == decl_typedef.
*/

bool type_variable(type t);
data_declaration type_variable_decl(type t);

type instantiate_type(type t);
/* Effects: Instantiate a type with type variables based on the instantiation
     of the variables and tag declarations. These are found in 
       type_variable_decl(vartype)->instantiation->type for variables
       type_tag(tagtype)->instantiation for tags
   Returns: The instantiated type
*/

typelist instantiate_typelist(typelist old);
/* Returns: An instantiated copy of typelist old, allocated in the same
     region
*/

const char *type_name(region r, type t);

unsigned long type_hash(type t);

bool type_contains(type parent, type child);

bool type_charstar(type t);
bool type_chararray(type t, bool no_size_allowed);
bool type_wchararray(type t, bool no_size_allowed);
 
void nxml_type(type t);
/* Effects: Outputs XML representation of type t.
*/
void nxml_typelist(const char *name, typelist types);
/* Effects: Outputs XML representartion of typelist types wrapped in tag 'name'
*/

type type_for_mode(const char *mode, bool isunsigned);
/* Returns: type (unsigned if 'unsigned' is TRUE) corresponding to the
     specified mode
*/

#endif
