/* XXX: figure out when an init_list is constant (so that we can allow
   a cast_list inside an init_list that requires constants) 
*/

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

#include "parser.h"
#include "c-parse.h"
#include "init.h"
#include "expr.h"
#include "constants.h"
#include "AST_utils.h"
#include "semantics.h"

static type set_array_length(type t, largest_int length)
{
  return make_array_type(type_array_of(t),
			 build_uint_constant(parse_region, dummy_location,
					     size_t_type, length));
}

static largest_int string_constant_length(expression e)
{
  return CAST(string, e)->ddecl->schars.length + 1;
}

/* Make a version of tdecl (an array type) with its length set to the 
   length of the e (a string constant) */
static type set_string_length(type tdecl, expression e)
{
  return set_array_length(tdecl, string_constant_length(e));
}

field_declaration type_fields(type t)
/* Requires: type_aggregate(t)
   Returns: first field_declaration of t
*/
{
  return type_tag(t)->fieldlist;
}

field_declaration skip_unnamed_bitfields(field_declaration flist)
/* Returns: return first field which is not an unnamed bit field */
{
  while (flist && !cval_istop(flist->bitwidth) && !flist->name)
    flist = flist->next;

  return flist;
}


/* Methods for storing and printing names for error messages.  */

/* Implement a spelling stack that allows components of a name to be pushed
   and popped.  Each element on the stack is this structure.  */

struct spelling
{
  int kind;
  union
    {
      largest_int i;
      const char *s;
    } u;
};

#define SPELLING_STRING 1
#define SPELLING_MEMBER 2
#define SPELLING_BOUNDS 3

static struct spelling *spelling;	/* Next stack element (unused).  */
static struct spelling *spelling_base;	/* Spelling stack base.  */
static int spelling_size;		/* Size of the spelling stack.  */

/* Macros to save and restore the spelling stack around push_... functions.
   Alternative to SAVE_SPELLING_STACK.  */

#define SPELLING_DEPTH() (spelling - spelling_base)
#define RESTORE_SPELLING_DEPTH(DEPTH) (spelling = spelling_base + (DEPTH))

/* Save and restore the spelling stack around arbitrary C code.  */

#define SAVE_SPELLING_DEPTH(code)		\
{						\
  int __depth = SPELLING_DEPTH ();		\
  code;						\
  RESTORE_SPELLING_DEPTH (__depth);		\
}

/* Push an element on the spelling stack with type KIND and assign VALUE
   to MEMBER.  */

#define PUSH_SPELLING(KIND, VALUE, MEMBER)				\
{									\
  int depth = SPELLING_DEPTH ();					\
									\
  if (depth >= spelling_size)						\
    {									\
      spelling_size += 10;						\
      if (spelling_base == 0)						\
	spelling_base							\
	  = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling));	\
      else								\
        spelling_base							\
	  = (struct spelling *) xrealloc (spelling_base,		\
					  spelling_size * sizeof (struct spelling));	\
      RESTORE_SPELLING_DEPTH (depth);					\
    }									\
									\
  spelling->kind = (KIND);						\
  spelling->MEMBER = (VALUE);						\
  spelling++;								\
}

/* Push STRING on the stack.  Printed literally.  */

static void push_string(const char *string)
{
  PUSH_SPELLING (SPELLING_STRING, string, u.s);
}

/* Push a member name on the stack.  Printed as '.' STRING.  */
static void push_member_name(field_declaration fdecl)
{
  const char *const string = nice_field_name(fdecl->name);
  PUSH_SPELLING (SPELLING_MEMBER, string, u.s);
}

/* Push an array bounds on the stack.  Printed as [BOUNDS].  */

static void push_array_bounds (largest_int bounds)
{
  PUSH_SPELLING (SPELLING_BOUNDS, bounds, u.i);
}

/* Compute the maximum size in bytes of the printed spelling.  */

static int spelling_length(void)
{
  int size = 0;
  struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    {
      if (p->kind == SPELLING_BOUNDS)
	size += 25;
      else
	size += strlen (p->u.s) + 1;
    }

  return size;
}

/* Print the spelling to BUFFER and return it.  */

static char *
print_spelling (buffer)
     char *buffer;
{
  char *d = buffer;
  struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    if (p->kind == SPELLING_BOUNDS)
      {
	sprintf (d, "[%ld]", (long)p->u.i);
	d += strlen (d);
      }
    else
      {
	const char *s;
	if (p->kind == SPELLING_MEMBER)
	  *d++ = '.';
	for (s = p->u.s; (*d = *s++); d++)
	  ;
      }
  *d++ = '\0';
  return buffer;
}

static void save_expression_spelling(expression e)
{
  if (spelling_base)
    e->spelling = print_spelling(rstralloc(regionof(e), spelling_length() + 1));
}

/* Issue an error message for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

void error_init (const char *msgid)
{
  char *ofwhat;

  error ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    error ("(near initialization for `%s')", ofwhat);
}

void error_init_expr(expression e, const char *msgid)
{
  const char *ofwhat;

  error_with_location(e->location, "%s", msgid);
  if (e->spelling)
    ofwhat = e->spelling;
  else
    ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    error_with_location(e->location, "(near initialization for `%s')", ofwhat);
}

/* Issue a pedantic warning for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

void pedwarn_init (const char *msgid)
{
  char *ofwhat;

  pedwarn ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    pedwarn ("(near initialization for `%s')", ofwhat);
}

/* Issue a warning for a bad initializer component.
   MSGID identifies the message.
   The component name is taken from the spelling stack.  */

static void warning_init (const char *msgid)
{
  char *ofwhat;

  warning ("%s", msgid);
  ofwhat = print_spelling ((char *) alloca (spelling_length () + 1));
  if (*ofwhat)
    warning ("(near initialization for `%s')", ofwhat);
}

/* Digest the parser output INIT as an initializer for type TYPE.
   Return FALSE if the initialisation is erroneous
*/
static bool digest_init(type t, expression init)
{
  type itype = init->type;

  if (t == error_type || itype == error_type)
    return FALSE;

  /* Initialization of an array of chars from a string constant
     optionally enclosed in braces.  */

  if (type_array(t))
    {
      type typ1 = type_array_of(t);

      if ((type_char(typ1) || type_equal_unqualified(typ1, wchar_type))
	  && is_string(init))
	{
	  type init_chartype = type_array_of(itype);
	  cval tcsize;

	  if (type_compatible_unqualified(t, itype))
	    return TRUE;

	  if (!type_char(init_chartype) && type_char(typ1))
	    {
	      error_init ("char-array initialized from wide string");
	      return FALSE;
	    }
	  if (type_char(init_chartype) && !type_char(typ1))
	    {
	      error_init ("int-array initialized from non-wide string");
	      return FALSE;
	    }

	  tcsize = type_array_size_cval(t);
	  if (!cval_istop(tcsize))
	    {
	      largest_uint tsize = cval_uint_value(tcsize);
	      data_declaration sdecl = CAST(string, init)->ddecl;

	      /* Don't count the null char (char x[1] = "a" is ok) */
	      if (tsize && tsize < sdecl->schars.length / type_size_int(type_array_of(sdecl->type)))
		pedwarn_init ("initializer-string for array of chars is too long");
	    }

	  return TRUE;
	}
    }

  /* Any type can be initialized
     from an expression of the same type, optionally with braces. 
     Initialisations of pointers from arrays and functions allow the
     usual default conversions */

  if (type_compatible_unqualified(itype, t) ||
      type_compatible(t, type_default_conversion_for_assignment(itype)))
    {
      if (type_pointer(t))
	itype = default_conversion_for_assignment(init);

#if 0
      if (require_constant /*&& !flag_isoc99*/ && is_cast_list(init))
	{
	  /* As an extension, allow initializing objects with static storage
	     duration with compound literals (which are then treated just as
	     the brace enclosed list they contain).  */
	  init = CAST(cast_list, init)->init_expr;
	  itype = init->type;
	}
#endif

      if (type_array(t) && !(is_string(init) || is_init_list(init)))
	{
	  error_init ("array initialized from non-constant array expression");
	  return FALSE;
	}

      /* Note: gcc allows "static int a = (1,2)" if -pedantic is
	 specified even though it doesn't allow case (1,2) when
	 -pedantic is specified. For the sake of consistency,
	 I'm not allowing either when -pedantic is specified. */

      return TRUE;
    }

  /* Handle scalar types, including conversions.  */

  if (type_scalar(t))
    return check_assignment(t, default_conversion_for_assignment(init), init, "initialization", NULL, 0);

  /* Come here only for records and arrays.  */

  /* Traditionally, you can write  struct foo x = 0;
     and it initializes the first element of x to 0.  */
  if (flag_traditional)
    {
      bool changed = FALSE;

      while (type_array(t) || type_aggregate(t))
	{
	  changed = TRUE;
	  if (type_array(t))
	    t = type_array_of(t);
	  else
	    {
	      tag_declaration tdecl = type_tag(t);

	      if (tdecl->fieldlist)
		t = tdecl->fieldlist->type;
	      else
		{
		  error_init ("invalid initializer");
		  return FALSE;
		}
	    }
	}

      if (changed)
	return digest_init(t, init);
    }
  error_init("invalid initializer");
  return FALSE;
}

/* Handle initializers that use braces.  */

typedef enum { c_none, c_aggregate, c_array, c_scalar } c_kind;

/* The kind of object we're constructing at this level of the
   initialiser */
static c_kind constructor_kind;

/* Type of object we are accumulating a constructor for.
   This type is always a type_struct, type_union or type_array */
static type constructor_type;

/* For a RECORD_TYPE or UNION_TYPE, this is the chain of fields
   left to fill.  */
static field_declaration constructor_fields;

/* For an ARRAY_TYPE, this is the specified index
   at which to store the next element we get.  */
static largest_int constructor_index;

/* Largest array index seen+1 (used to determine size of int x[] = { ... } */
static largest_int constructor_array_size;

/* For an ARRAY_TYPE, this is the maximum index.  */
static largest_int constructor_max_index;

/* The count of elements specified at this depth */
static size_t constructor_count;

/* The SPELLING_DEPTH of this constructor.  */
static int constructor_depth;

/* The value currently being initialised */
ivalue constructor_value;

static int require_constant_value;

/* DECL node for which an initializer is being read.
   0 means we are reading a constructor expression
   such as (struct foo) {...}.  */
static data_declaration constructor_decl;

/* Nonzero if there were any member designators in this initializer.  */
static int constructor_designated;

/* Nesting depth of designator list.  */
static int designator_depth;

/* Nonzero if there were diagnosed errors in this designator list.  */
static int designator_erroneous;

/* Nonzero if we've already printed a "missing braces around initializer"
   message within this initializer.  */
static int missing_braces_mentioned;


/* This stack has a level for each implicit or explicit level of
   structuring in the initializer, including the outermost one.  It
   saves the values of most of the variables above.  */

struct constructor_range_stack;

struct constructor_stack
{
  struct constructor_stack *next;
  c_kind kind;
  type type;
  field_declaration fields;
  largest_int index;
  largest_int array_size;
  largest_int max_index;
  ivalue value;
  size_t count;
  int offset;
  int depth;
  struct constructor_range_stack *range_stack;
  char constant;
  char simple;
  char implicit;
  char erroneous;
  char outer;
  char incremental;
  char designated;
};

struct constructor_stack *constructor_stack;

/* This stack represents designators from some range designator up to
   the last designator in the list.  */

struct constructor_range_stack
{
  struct constructor_range_stack *next, *prev;
  struct constructor_stack *stack;
  largest_int range_start;
  largest_int index;
  largest_int range_end;
  bool has_end;
  field_declaration fields;
};

struct constructor_range_stack *constructor_range_stack;

/* This stack records separate initializers that are nested.
   Nested initializers can't happen in ANSI C, but GNU C allows them
   in cases like { ... (struct foo) { ... } ... }.  */

struct initializer_stack
{
  struct initializer_stack *next;
  data_declaration decl;
  struct constructor_stack *constructor_stack;
  struct constructor_range_stack *constructor_range_stack;
  struct spelling *spelling;
  struct spelling *spelling_base;
  int spelling_size;
  char require_constant_value;
};

struct initializer_stack *initializer_stack;

static type pop_init_level(void);

static void pop_implicit_level(void)
{
  assert(constructor_stack->implicit);
  pop_init_level();
  process_init_element(NULL);
}

static void pop_all_implicit_levels(void)
{
  while (constructor_stack->implicit)
    pop_implicit_level();
}

static void pop_exhausted_levels(void)
{
  /* If we've exhausted any levels that didn't have braces,
     pop them now.  */
  while (constructor_stack->implicit &&
	 ((constructor_kind == c_aggregate && constructor_fields == 0) ||
	  (constructor_kind == c_array && constructor_max_index < constructor_index)))
    pop_implicit_level();
}

/* Prepare to parse and output the initializer for variable DECL.  */

void start_init(declaration decl, nesc_attribute attr)
/* decl is really a variable_decl */
{
  const char *locus;
  struct initializer_stack *p
    = (struct initializer_stack *) xmalloc (sizeof (struct initializer_stack));

  p->decl = constructor_decl;
  p->require_constant_value = require_constant_value;
  p->constructor_stack = constructor_stack;
  p->constructor_range_stack = constructor_range_stack;
  p->spelling = spelling;
  p->spelling_base = spelling_base;
  p->spelling_size = spelling_size;
  p->next = initializer_stack;
  initializer_stack = p;

  constructor_designated = 0;

  if (decl != 0)
    {
      data_declaration ddecl = CAST(variable_decl, decl)->ddecl;

      constructor_decl = ddecl;
      require_constant_value = ddecl->needsmemory;
      locus = ddecl->name;
    }
  else if (attr)
    {
      cstring aname = attr->word1->cstring;

      constructor_decl = NULL;
      require_constant_value = !(attr->tdecl && attr->tdecl->deputy_scope);
      locus = rstralloc(current.fileregion, aname.length + 2);
      sprintf((char *)locus, "@%s", aname.data);
    }
  else
    {
      constructor_decl = NULL;
      require_constant_value = 0;
      locus = "(anonymous)";
    }

  constructor_stack = 0;
  constructor_range_stack = 0;

  missing_braces_mentioned = 0;

  spelling_base = 0;
  spelling_size = 0;
  RESTORE_SPELLING_DEPTH (0);

  if (locus)
    push_string(locus);
}

void finish_init(void)
{
  struct initializer_stack *p = initializer_stack;

  /* Free the whole constructor stack of this initializer.  */
  while (constructor_stack)
    {
      struct constructor_stack *q = constructor_stack;
      constructor_stack = q->next;
      free (q);
    }

  if (constructor_range_stack)
    abort ();

  /* Pop back to the data of the outer initializer (if any).  */
  constructor_decl = p->decl;
  require_constant_value = p->require_constant_value;
  constructor_stack = p->constructor_stack;
  constructor_range_stack = p->constructor_range_stack;
  spelling = p->spelling;
  spelling_base = p->spelling_base;
  spelling_size = p->spelling_size;
  initializer_stack = p->next;
  free (p);
}

/* ivalue constructors */
ivalue new_ivalue(region r, int kind, type t)
{
  ivalue newp = ralloc(r, struct ivalue);

  newp->kind = kind;
  newp->type = t;

  if (newp->kind == iv_base)
    newp->u.base.value = cval_top;

  return newp;
}

static void add_ivalue_array(ivalue to, largest_int index, ivalue element)
{
  largest_int end = index;
  struct ivalue_array *newp;

  assert(to->kind == iv_array);

  /* Detect when pushing a range initialiser */
  if (constructor_range_stack && constructor_range_stack->has_end)
    end = constructor_range_stack->range_end;

  newp = ralloc(parse_region, struct ivalue_array);
  newp->next = to->u.array;
  to->u.array = newp;
  newp->from = index;
  newp->to = end;
  newp->value = element;
}

static void add_ivalue_field(ivalue to, field_declaration field, ivalue element)
{
  struct ivalue_field *newp;

  assert(to->kind == iv_structured);

  newp = ralloc(parse_region, struct ivalue_field);
  newp->next = to->u.structured;
  to->u.structured = newp;
  newp->field = field;
  newp->value = element;
}

static struct constructor_stack *push_constructor_stack(int implicit)
{
  struct constructor_stack *p
    = (struct constructor_stack *) xmalloc (sizeof (struct constructor_stack));

  p->kind = constructor_kind;
  p->type = constructor_type;
  p->fields = constructor_fields;
  p->index = constructor_index;
  p->array_size = constructor_array_size;
  p->max_index = constructor_max_index;
  p->value = constructor_value;
  p->count = constructor_count;
  p->depth = constructor_depth;
  p->implicit = implicit;
  p->range_stack = 0;
  p->outer = 0;
  p->designated = constructor_designated;
  p->next = constructor_stack;
  constructor_stack = p;

  constructor_depth = SPELLING_DEPTH ();
  constructor_designated = 0;
  constructor_count = 0;

  if (!implicit)
    {
      p->range_stack = constructor_range_stack;
      constructor_range_stack = 0;
      designator_depth = 0;
      designator_erroneous = 0;
    }

  return p;
}

static bool new_constructor_type(void);

/* Call here when we see the initializer is surrounded by braces.
   This is instead of a call to push_init_level;
   it is matched by a call to pop_init_level.

   TYPE is the type to initialize, for a constructor expression.
   For an initializer for a decl, TYPE is zero.  */

void really_start_incremental_init(type t)
{
  if (t == 0)
    t = constructor_decl->type;

  assert(constructor_stack == 0);
  push_constructor_stack(0);

  constructor_type = t;
  new_constructor_type();
}

/* Push down into a subobject, for initialization.
   If this is for an explicit set of braces, IMPLICIT is 0.
   If it is because the next element belongs at a lower level,
   IMPLICIT is 1 (or 2 if the push is because of designator list).  */

void push_init_level(int implicit)
{
  struct constructor_stack *p;

  pop_exhausted_levels();

  p = push_constructor_stack(implicit);

  /* Don't die if an entire brace-pair level is superfluous
     in the containing level.  */
  if (constructor_type == 0)
    ;
  else if (constructor_kind == c_aggregate)
    {
      /* Don't die if there are extra init elts at the end.  */
      if (constructor_fields == 0)
	constructor_type = 0;
      else
	{
	  constructor_type = constructor_fields->type;
	  push_member_name(constructor_fields);
	  constructor_depth++;
	}
    }
  else if (constructor_kind == c_array)
    {
      constructor_type = type_array_of(constructor_type);
      push_array_bounds(constructor_index);
      constructor_depth++;
    }

  if (constructor_type == 0)
    {
      error_init("extra brace group at end of initializer");
    }
  else if (implicit == 1 && warn_missing_braces && !missing_braces_mentioned)
    {
      missing_braces_mentioned = 1;
      warning_init ("missing braces around initializer");
    }

  if (!new_constructor_type())
    warning_init ("braces around scalar initializer");

  switch (p->kind)
    {
    case c_aggregate:
      add_ivalue_field(p->value, p->fields, constructor_value);
      break;
    case c_array:
      add_ivalue_array(p->value, p->index, constructor_value);
      break;
    case c_scalar:
      break;
    default: assert(0); break;
    }
}

/* Set state for new constructor type (constructor_kind and associated state)
   Return FALSE if the constructor_type is a scalar type */
static bool new_constructor_type(void)
{
  if (!constructor_type)
    {
      constructor_kind = c_none;
      constructor_value = NULL;
    }
  else if (type_aggregate(constructor_type))
    {
      constructor_kind = c_aggregate;
      constructor_fields = skip_unnamed_bitfields(type_fields(constructor_type));
      constructor_value = new_ivalue(parse_region, iv_structured, constructor_type);
    }
  else if (type_array(constructor_type))
    {
      cval max = type_array_size_cval(constructor_type);

      constructor_kind = c_array;
      if (cval_istop(max))
	constructor_max_index = -1;
      else
	constructor_max_index = cval_sint_value(max) - 1;
      constructor_index = constructor_array_size = 0;
      constructor_value = new_ivalue(parse_region, iv_array, constructor_type);
    }
  else
    {
      /* Handle the case of int x = {5}; */
      constructor_kind = c_scalar;
      constructor_index = 0;
      constructor_value = new_ivalue(parse_region, iv_base, constructor_type);

      return FALSE;
    }
  return TRUE;
}

/* At the end of an implicit or explicit brace level, 
   finish up that level of constructor.
   Return the type that this level was for */

static type pop_init_level(void)
{
  struct constructor_stack *p;
  type ctype;

  ctype = constructor_type ? constructor_type : error_type;

  p = constructor_stack;

  /* Error for initializing a flexible array member, or a zero-length
     array member in an inappropriate context.  */
  if (constructor_type && constructor_fields && constructor_depth
      && type_array(constructor_type)
      && !type_array_size(constructor_type))
    {
      /* Silently discard empty initializations.  The parser will
	 already have pedwarned for empty brackets.  */
      if (constructor_count > 0)
	{
	  if (constructor_depth > 2)
	    error_init("initialization of flexible array member in a nested context");
	  else if (pedantic)
	    pedwarn_init("initialization of a flexible array member");

	  /* We have already issued an error message for the existence
	     of a flexible array member not at the end of the structure.
	     Discard the initializer so that we do not abort later.  */
	  if (constructor_fields->next)
	    constructor_type = NULL;
	}
    }

#if 0
  /* Warn when some struct elements are implicitly initialized to zero.  */
  if (extra_warnings
      && constructor_type
      && TREE_CODE (constructor_type) == RECORD_TYPE
      && constructor_unfilled_fields)
    {
	/* Do not warn for flexible array members or zero-length arrays.  */
	while (constructor_unfilled_fields
	       && (! DECL_SIZE (constructor_unfilled_fields)
		   || integer_zerop (DECL_SIZE (constructor_unfilled_fields))))
	  constructor_unfilled_fields = TREE_CHAIN (constructor_unfilled_fields);

	/* Do not warn if this level of the initializer uses member
	   designators; it is likely to be deliberate.  */
	if (constructor_unfilled_fields && !constructor_designated)
	  {
	    push_member_name (constructor_unfilled_fields);
	    warning_init ("missing initializer");
	    RESTORE_SPELLING_DEPTH (constructor_depth);
	  }
    }
#endif

  if (constructor_kind == c_scalar && constructor_count == 0)
    error_init("empty scalar initializer");

  constructor_kind = p->kind;
  constructor_type = p->type;
  constructor_fields = p->fields;
  constructor_index = p->index;
  constructor_array_size = p->array_size;
  constructor_max_index = p->max_index;
  constructor_value = p->value;
  constructor_count = p->count;
  constructor_designated = p->designated;
  constructor_depth = p->depth;
  if (!p->implicit)
    constructor_range_stack = p->range_stack;
  RESTORE_SPELLING_DEPTH (constructor_depth);

  constructor_stack = p->next;
  free (p);

  return ctype;
}

/* Common handling for both array range and field name designators.
   ARRAY argument is non-zero for array ranges.  Returns zero for success.  */

static bool set_designator(bool array)
{
  type subtype;

  /* Don't die if an entire brace-pair level is superfluous
     in the containing level.  */
  if (constructor_kind == c_none)
    return TRUE;

  /* If there were errors in this designator list already, bail out silently.  */
  if (designator_erroneous)
    return TRUE;

  if (!designator_depth)
    {
      if (constructor_range_stack)
	abort ();

      /* Designator list starts at the level of closest explicit
	 braces.  */
      pop_all_implicit_levels();
      constructor_designated = 1;
      return FALSE;
    }

  if (constructor_kind == c_aggregate)
    {
      subtype = constructor_fields->type;
    }
  else if (constructor_kind == c_array)
    {
      subtype = type_array_of(constructor_type);
    }
  else
    abort();

  if (array && !type_array(subtype))
    {
      error_init ("array index in non-array initializer");
      return TRUE;
    }
  else if (!array && !type_aggregate(subtype))
    {
      error_init ("field name not in record or union initializer");
      return TRUE;
    }

  constructor_designated = 1;
  push_init_level(2);
  return FALSE;
}

/* If there are range designators in designator list, push a new designator
   to constructor_range_stack.  RANGE_END is end of such stack range or
   NULL if there is no range designator at this level.  */

static void push_range_stack(expression range_end)
{
  struct constructor_range_stack *p;

  p = (struct constructor_range_stack *)
      xmalloc(sizeof (struct constructor_range_stack));
  p->prev = constructor_range_stack;
  p->next = 0;
  p->fields = constructor_fields;
  p->range_start = constructor_index;
  p->index = constructor_index;
  p->stack = constructor_stack;
  if (range_end)
    {
      p->range_end = constant_sint_value(range_end->cst);
      p->has_end = TRUE;
    }
  else
    p->has_end = FALSE;
  if (constructor_range_stack)
    constructor_range_stack->next = p;
  constructor_range_stack = p;
}

/* Within an array initializer, specify the next index to be initialized.
   FIRST is that index.  If LAST is nonzero, then initialize a range
   of indices, running from FIRST through LAST.  */

designator set_init_index(location loc, expression first, expression last)
{
  designator d = CAST(designator,
		      new_designate_index(parse_region, loc, first, last));

  if (set_designator(TRUE))
    return d;

  designator_erroneous = 1;

  if (!(first->cst && constant_integral(first->cst)))
    error_init("nonconstant array index in initializer");
  else if (last != 0 && !(last->cst && constant_integral(last->cst)))
    error_init("nonconstant array index in initializer");
  else if (constructor_kind != c_array)
    error_init("array index in non-array initializer");
  else if (constructor_max_index >= 0
	   && constructor_max_index < constant_sint_value(first->cst))
    error_init("array index in initializer exceeds array bounds");
  else
    {
      largest_int fval = constant_sint_value(first->cst);

      constructor_index = fval;

      if (last)
	{
	  largest_int lval = constant_sint_value(last->cst);

	  if (fval == lval)
	    last = 0;
	  else if (lval < fval)
	    {
	      error_init("empty index range in initializer");
	      last = 0;
	    }
	  else
	    {
	      if (constructor_max_index >= 0 && constructor_max_index < lval)
		{
		  error_init("array index range in initializer exceeds array bounds");
		  last = 0;
		}
	    }
	}

      designator_depth++;
      designator_erroneous = 0;
      if (constructor_range_stack || last)
	push_range_stack(last);
    }

  return d;
}

/* Within a struct initializer, specify the next field to be initialized.  */

designator set_init_label(location loc, cstring fieldname)
{
  designator d = CAST(designator,
		      new_designate_field(parse_region, loc, fieldname));
  field_declaration tail;
  tag_declaration tdecl;

  if (set_designator(FALSE))
    return d;

  designator_erroneous = 1;

  if (constructor_kind != c_aggregate)
    {
      error_init("field name not in record or union initializer");
      return d;
    }
    
  tdecl = type_tag(constructor_type);
  tail = env_lookup(tdecl->fields, fieldname.data, TRUE);
  if (tail == 0)
    error("unknown field `%s' specified in initializer", fieldname.data);
  else
    {
      constructor_fields = tail;
      designator_depth++;
      designator_erroneous = 0;
      if (constructor_range_stack)
	push_range_stack(NULL);
    }

  return d;
}

void check_init_element(expression init)
{
  known_cst c;

  if (!check_constant_once(init, cst_any))
    return;

  /* XXX: process_init_element set the ivalue to iv_base for strings used
     to initialise arrays. So we need to special case here too. We don't
     need to do anything, as this only happened for string constants, which
     are clearly constant ;-)

     See string_flag test in that function. */
  if (is_init_list(init))
    return;

  /* Arrays are "constant" if they have a static address 
     (see default_conversion on arrays) - this essentially handles the
     case of string constants */
  c = type_array(init->type) ? init->static_address : init->cst;

  if (!c)
    {
      error_init_expr(init, "initializer element is not constant");
      return;
    }
#if 0
  /* This is no longer detected as constant_unknown has been recycled
     for use with abstract component arguments. It could be resurrected
     if we made cval more complicated, but it doesn't seem worth the
     effort. */
  else if (constant_uncomputable(c))
    {
      error_init_expr(init, "initializer element is not computable at load time");
      return;
    }
#endif
  else
    {
      constant_overflow_warning(c);
      if (init->ivalue)
	{
	  assert(init->ivalue->kind == iv_base);
	  init->ivalue->u.base.value = cval_cast(c->cval, init->ivalue->type);
	}
    }
}

/* "Output" the next constructor element.
   At top level, really output it to assembler code now.
   Otherwise, collect it in a list from which we will make a CONSTRUCTOR.
   TYPE is the data type that the containing data type wants here.
   FIELD is the field (a FIELD_DECL) or the index that this element fills.

   PENDING if non-nil means output pending elements that belong
   right after this element.  (PENDING is normally 1;
   it is 0 while outputting pending elements, to avoid recursion.)  */

static void output_init_element(expression init, type t)
{
  assert(init->ivalue->kind == iv_base);
  init->ivalue->u.base.expr = init;
  init->ivalue->u.base.require_constant_value = require_constant_value;

  if (digest_init(t, init) && require_constant_value)
    {
      check_init_element(init);
      /* If we haven't checked it yet then we'll need the spelling later
	 (see nesc-constants.c) */
      if (!init->cst_checked)
	save_expression_spelling(init);
    }
}


/* Add one non-braced element to the current constructor level.
   This adjusts the current position within the constructor's type.
   This may also start or terminate implicit levels
   to handle a partly-braced initializer.

   Once this has found the correct level for the new element,
   it calls output_init_element.  

   If VALUE is NULL, just advance to the next element without 
   any error messages
*/

void process_init_element(expression value)
{
  bool string_flag = value && is_string(value);

  designator_depth = 0;
  designator_erroneous = 0;

  /* Handle superfluous braces around string cst as in
     char x[] = {"foo"}; */
  if (string_flag
      && constructor_kind == c_array
      && type_integer(type_array_of(constructor_type))
      && constructor_count == 0)
    {
      constructor_kind = c_scalar;
      constructor_index = 0;
      if (!type_array_size(constructor_type))
	constructor_type = constructor_value->type =
	  set_string_length(constructor_type, value);
      /* XXX: maybe this should stay as a iv_array, and the string should
	 be broken down into characters? */
      constructor_value->kind = iv_base;
      constructor_value->u.base.expr = NULL;
      constructor_value->u.base.value = cval_top;
    }

  /* Ignore elements of a brace group if it is entirely superfluous
     and has already been diagnosed.  */
  if (constructor_kind == c_none)
    return;

  /* If we've exhausted any levels that didn't have braces,
     pop them now.  */
  pop_exhausted_levels();

 tryagain:
  if (value)
    {
      type elttype = error_type;

      switch (constructor_kind)
	{
	case c_aggregate:
	  if (constructor_fields == 0)
	    {
	      pedwarn_init("excess elements in struct or union initializer");
	      break;
	    }
	  elttype = constructor_fields->type;

	  /* Error for non-static initialization of a flexible array member.  */
	  if (type_array(elttype)
	      && !require_constant_value
	      && (type_array_size(elttype) && definite_zero(type_array_size(elttype)))
	      && !constructor_fields->next)
	    {
	      error_init("non-static initialization of a flexible array member");
	      break;
	    }
	  push_member_name (constructor_fields);

	  break;
	case c_array:
	  if (constructor_max_index >= 0
	      && constructor_max_index < constructor_index)
	    {
	      pedwarn_init("excess elements in array initializer");
	      break;
	    }

	  elttype = type_array_of(constructor_type);
	  push_array_bounds(constructor_index);
	  if (type_array(elttype) && !type_array_size(elttype))
	    {
	      elttype = error_type;
	      error_init("array type has incomplete element type");
	    }
	  break;
	case c_scalar:
	  if (constructor_count == 0)
	    elttype = constructor_type;
	  else if (constructor_count == 1) /* Only warn once */
	    pedwarn_init("excess elements in scalar initializer");
	  break;
	default: assert(0); break;
	}

      /* Accept a string constant to initialize a subarray.  */
      if (type_array(elttype) && type_integer(type_array_of(elttype))
	  && string_flag)
	;
      /* Otherwise, if we have come to a subaggregate,
	 and we don't have an element of its type, push into it.  */
      else if ((constructor_kind == c_array || constructor_kind == c_aggregate) &&
	       value->type != error_type
	       && !type_equal_unqualified(value->type, elttype)
	       && (type_aggregate(elttype) || type_array(elttype)))
	{
	  push_init_level(1);
	  goto tryagain;
	}

      /* This is here rather than in the previous switch because of
	 the tryagain ("walk-into-array-or-aggregate") case */
      if (elttype != error_type)
	{
	  ivalue valueholder = NULL;

	  switch (constructor_kind)
	    {
	    case c_aggregate:
	      valueholder = new_ivalue(parse_region, iv_base, elttype);
	      add_ivalue_field(constructor_value, constructor_fields, valueholder);
	      break;
	    case c_array:
	      valueholder = new_ivalue(parse_region, iv_base, elttype);
	      add_ivalue_array(constructor_value, constructor_index, valueholder);
	      break;
	    case c_scalar:
	      valueholder = constructor_value;
	      break;
	    default: assert(0); break;
	    }
	  value->ivalue = valueholder;
	}
      else
	value->ivalue = new_ivalue(parse_region, iv_base, error_type);

      output_init_element (value, elttype);
    }

  constructor_count++;

  switch (constructor_kind)
    {
    case c_aggregate:
      if (value)
	RESTORE_SPELLING_DEPTH (constructor_depth);

      if (!type_union(constructor_type))
	{
	  if (constructor_fields)
	    constructor_fields = skip_unnamed_bitfields(constructor_fields->next);
	}
      else
	{
	  /* Warn that traditional C rejects initialization of unions.
	     We skip the warning if the value is zero.  This is done
	     under the assumption that the zero initializer in user
	     code appears conditioned on e.g. __STDC__ to avoid
	     "missing initializer" warnings and relies on default
	     initialization to zero in the traditional C case.
	     We also skip the warning if the initializer is designated,
	     again on the assumption that this must be conditional on
	     __STDC__ anyway (and we've already complained about the
	     member-designator already).  */
	  if (value && warn_traditional &&
	      !value->location->in_system_header &&
	      !constructor_designated && !definite_zero(value))
	    warning("traditional C rejects initialization of unions");

	  constructor_fields = 0;
	}
      break;
    case c_array:
      if (value)
	RESTORE_SPELLING_DEPTH (constructor_depth);
      constructor_index++;
      if (constructor_index > constructor_array_size)
	constructor_array_size = constructor_index;
      break;
    case c_scalar: /* the weird {"foo"} case above */
      break;
    default: assert(0); break;
    }

  /* Pop back to the level before the designator */
  if (constructor_range_stack)
    {
      struct constructor_range_stack *p, *range_stack;

      range_stack = constructor_range_stack;
      constructor_range_stack = 0;

      /* First pop back to the level at which the designator ended */
      while (constructor_stack != range_stack->stack)
	pop_implicit_level();

      /* Then pop back up all the designators (note that the topmost one
	 does not have an implicit level) */
      for (p = range_stack; p->prev; p = p->prev)
	pop_implicit_level();
    }

  // XXX: mem dealloc for range stack
  constructor_range_stack = 0;
}

expression make_init_specific(designator dlist, expression initval)
{
  return CAST(expression,
    new_init_specific(parse_region, dlist->location, dlist, initval));
}

expression make_init_list(location loc, expression elist)
{
  expression ilist;
  type itype;
  largest_int array_size;

  /* Explicit close brace:
     pop any inner levels that didn't have explicit braces.  */
  pop_all_implicit_levels();
  /* Save array size (if any) */
  array_size = constructor_array_size; 

  assert(!constructor_range_stack);

  ilist = CAST(expression, new_init_list(parse_region, loc, elist));
  ilist->ivalue = constructor_value;
  itype = pop_init_level();

  /* Complete array types based on the initialiser */
  if (type_array(itype) && !type_array_size(itype))
    itype = set_array_length(itype, array_size);
  ilist->type = itype;

  return ilist;
}

expression make_cast_list(location loc, asttype t, expression init)
{
  cast_list result;

  result = new_cast_list(parse_region, loc, t, init);
  result->type = t->type;
  /* gcc 2.x only considers this an lvalue if it's defined from constant
     expressions. But 3.x always considers it an lvalue... */
  result->lvalue = TRUE;

  return CAST(expression, result);
}

/* Code to handle simple initialisers like 'int x = <expr>'.
   The variable being initialised is constructor_decl */
void simple_init(expression expr)
{
  type tdecl = constructor_decl->type;

  if (is_string(expr) && type_array(tdecl) && !type_array_size(tdecl))
    tdecl = set_string_length(tdecl, expr);
  expr->ivalue = new_ivalue(parse_region, iv_base, tdecl);
  output_init_element(expr, tdecl);
}
