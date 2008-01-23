#include "parser.h"
#include "attributes.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "nesc-attributes.h"
#include "machine.h"
#include "c-parse.h"
#include "constants.h"
#include "AST_utils.h"

/* Provide warnings about ignored attributes and attribute lists */

void ignored_attribute(attribute attr)
{
  warning_with_location(attr->location, "`%s' attribute directive ignored",
			attr->word1->cstring.data);
}

void ignored_gcc_attribute(gcc_attribute attr)
{
  ignored_attribute(CAST(attribute, attr));
}

void ignored_nesc_attribute(nesc_attribute attr)
{
  ignored_attribute(CAST(attribute, attr));
}

void ignored_attributes(attribute alist)
{
  scan_attribute (alist, alist)
    ignored_attribute(alist);
}

void ignored_dd_attributes(dd_list alist)
{
  dd_list_pos attr;

  if (alist)
    dd_scan (attr, alist)
      ignored_attribute(DD_GET(attribute, attr));
}

cval gcc_attr_get_constant(gcc_attribute attr)
{
  if (!attr->args || attr->args->next || !attr->args->cst)
    return cval_top;
  else
    return attr->args->cst->cval;
}

const char *gcc_attr_get_word(gcc_attribute attr)
{
  if (attr->args && !attr->args->next && is_identifier(attr->args))
    return CAST(identifier, attr->args)->cstring.data;

  error_with_location(attr->location, "wrong number of arguments specified for `%s' attribute",
		      attr->word1->cstring.data);

  return NULL;
}

static size_t max_useful_alignment(void)
{
  size_t max_align = 0;

  /* Of current machine. Returns max of long double, long long, int8 and ptr
  alignment (ok, this is a slight hack, but it seems unlikely that other
  smaller types will have worse restrictions) */
#define IMAX(a) if (max_align < a) max_align = a
  IMAX(target->int8_align);
  IMAX(target->tptr.align);
  IMAX(target->tlong_double.align);
  IMAX(target->tlong_long.align);
#undef IMAX

  return max_align;
}

static cval get_alignment(gcc_attribute attr)
{
  cval arg;

  if (!attr->args)
    return make_cval_unsigned(max_useful_alignment(), size_t_type);

  arg = gcc_attr_get_constant(attr);
  if (cval_isinteger(arg))
    if (ilog2(cval_uint_value(arg)) != -1)
      return cval_cast(arg, size_t_type);
    else
      error("requested alignment is not a power of 2");
  else
    error("requested alignment is not a constant");

  return cval_top;
}

/* handle_X_attribute(attr, obj):
   Attempt to apply attribute attr to obj of kind X (decl, field, tag, type),
   modifying obj.

   For decls, fields, tags:
     If attr is not applicable: issue a warning with ignored_attributes
   For types:
     Return TRUE if applicable, FALSE if not
   (this difference is due to the funky rules of attributes used as type qualifiers)
*/

static void transparent_union_argument(data_declaration ddecl)
{
  ddecl->type = make_qualified_type
    (ddecl->type, type_qualifiers(ddecl->type) | transparent_qualifier);
}

static bool require_function(gcc_attribute attr, data_declaration ddecl)
{
  if (ddecl->kind == decl_function && ddecl->ftype == function_normal)
    return TRUE;

  error_with_location(attr->location, "`%s' attribute is for external functions only", attr->word1->cstring.data);
  return FALSE;
}

bool handle_gcc_type_attribute(gcc_attribute attr, type *t)
{
  const char *name = attr->word1->cstring.data;

  if (is_attr_name(name, "combine"))
    {
      const char *word = gcc_attr_get_word(attr);

      if (word)
	handle_combine_attribute(attr->location, word, t);
      return TRUE;
    }
  else if (is_attr_name(name, "aligned"))
    {
      cval arg = get_alignment(attr);

      if (cval_isinteger(arg))
	*t = align_type(*t, arg);
      return TRUE;
    }
  else 
    return target->type_attribute && target->type_attribute(attr, t);
}

void handle_gcc_decl_attribute(gcc_attribute attr, data_declaration ddecl)
{
  const char *name = attr->word1->cstring.data;

  if (is_attr_name(name, "transparent_union"))
    {
      if (attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `transparent_union' attribute");

      if (ddecl->kind == decl_variable && ddecl->isparameter &&
	  type_union(ddecl->type))
	transparent_union_argument(ddecl);
      else if (ddecl->kind == decl_typedef && type_union(ddecl->type))
	transparent_union_argument(ddecl);
      else
	ignored_gcc_attribute(attr);
    }
  else if (is_attr_name(name, "aligned"))
    {
      cval arg = get_alignment(attr);

      if (cval_isinteger(arg))
	{
	  if (ddecl->kind == decl_variable || ddecl->kind == decl_typedef)
	    ddecl->type = align_type(ddecl->type, arg);
	  else
	    ignored_gcc_attribute(attr);
	}
    }
  else if (is_attr_name(name, "mode"))
    {
      const char *word = gcc_attr_get_word(attr);

      if (word)
	if (!handle_mode_attribute(attr->location, ddecl, word))
	  ignored_gcc_attribute(attr);
    }
  else if (is_attr_name(name, "C"))
    {
      if (!ddecl->isexternalscope)
	error_with_location(attr->location, "`C' attribute is for symbols with external scope only");
      else
	ddecl->Cname = TRUE;
    }
  else if (is_attr_name(name, "spontaneous"))
    {
      if (require_function(attr, ddecl))
	{
	  /* The test avoids overriding the effect of atomic_hwevent */
	  if (!ddecl->spontaneous)
	    ddecl->spontaneous = c_call_nonatomic;
	}
    }
  else if (is_attr_name(name, "atomic_hwevent"))
    {
      if (require_function(attr, ddecl))
	{
	  ddecl->async = TRUE;
	  ddecl->spontaneous = c_call_atomic;
	}
    }
  else if (is_attr_name(name, "hwevent"))
    {
      if (require_function(attr, ddecl))
	{
	  ddecl->async = TRUE;
	  ddecl->spontaneous = c_call_nonatomic;
	}
    }
  else if (is_attr_name(name, "noinline"))
    {
      ddecl->noinlinep = TRUE;
    }
  else if (is_attr_name(name, "nx_base_le") || is_attr_name(name, "nx_base_be") || is_attr_name(name, "nx_base"))
    {
      const char *word = gcc_attr_get_word(attr);

      if (ddecl->kind != decl_typedef)
	error_with_location(attr->location, "`%s' attribute can only be applied to typedefs", name);
      else if (word)
	handle_nxbase_attribute(attr->location, is_attr_name(name, "nx_base_be"), TRUE, word, ddecl);
    }
  else if (!(target->decl_attribute &&
	     target->decl_attribute(attr, ddecl)) &&
	   !handle_gcc_type_attribute(attr, &ddecl->type))
    /*ignored_gcc_attribute(attr)*/;
}

/* Note: fdecl->bitwidth is not yet set when this is called */
void handle_gcc_field_attribute(gcc_attribute attr, field_declaration fdecl)
{
  const char *name = attr->word1->cstring.data;

  if (is_attr_name(name, "packed"))
    fdecl->packed = TRUE;
  else if (is_attr_name(name, "aligned"))
    {
      cval arg = get_alignment(attr);

      if (cval_isinteger(arg))
	fdecl->type = align_type(fdecl->type, arg);
    }
  else if (!(target->field_attribute &&
	     target->field_attribute(attr, fdecl)))
    /*ignored_gcc_attribute(attr)*/;
}

void handle_gcc_tag_attribute(gcc_attribute attr, tag_declaration tdecl)
{
  const char *name = attr->word1->cstring.data;

  if (is_attr_name(name, "transparent_union"))
    {
      if (attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `transparent_union' attribute");

      if (tdecl->kind == kind_union_ref)
	{
	  tdecl->transparent_union = TRUE;
	  /* XXX: Missing validity checks (need cst folding I think) */
	}
      else
	ignored_gcc_attribute(attr);
    }
  else if (is_attr_name(name, "packed"))
    tdecl->packed = TRUE;
  else if (is_attr_name(name, "aligned"))
    {
      cval arg = get_alignment(attr);

      if (cval_isinteger(arg))
	tdecl->user_alignment = arg;
    }
  else if (is_attr_name(name, "C"))
    {
      if (tdecl->container_function)
	error_with_location(attr->location, "`C' attribute is for symbols with external scope only");
      else
	tdecl->Cname = TRUE;
    }
  else if (!(target->tag_attribute &&
	     target->tag_attribute(attr, tdecl)))
    /*ignored_gcc_attribute(attr)*/;
}

void handle_nescdecl_attribute(attribute attr, nesc_declaration ndecl)
{
  handle_nesc_nescdecl_attribute(CAST(nesc_attribute, attr), ndecl);
}

void handle_decl_attribute(attribute attr, data_declaration ddecl)
{
  if (is_gcc_attribute(attr))
    handle_gcc_decl_attribute(CAST(gcc_attribute, attr), ddecl);
  else
    handle_nesc_decl_attribute(CAST(nesc_attribute, attr), ddecl);
}

void handle_field_attribute(attribute attr, field_declaration fdecl)
{
  if (is_gcc_attribute(attr))
    handle_gcc_field_attribute(CAST(gcc_attribute, attr), fdecl);
  else
    handle_nesc_field_attribute(CAST(nesc_attribute, attr), fdecl);
}

void handle_tag_attribute(attribute attr, tag_declaration tdecl)
{
  if (is_gcc_attribute(attr))
    handle_gcc_tag_attribute(CAST(gcc_attribute, attr), tdecl);
  else
    handle_nesc_tag_attribute(CAST(nesc_attribute, attr), tdecl);
}

bool handle_type_attribute(attribute attr, type *t)
{
  if (is_gcc_attribute(attr))
    return handle_gcc_type_attribute(CAST(gcc_attribute, attr), t);
  else
    {
      /* nesC attributes don't have a broken syntax, so don't need
	 to flow up to the declaration */
      handle_nesc_type_attribute(CAST(nesc_attribute, attr), t);
      return TRUE;
    }
}

/* Functions to handle regular and dd list of attributes */

void handle_nescdecl_attributes(attribute alist, nesc_declaration ndecl)
{
  scan_attribute (alist, alist)
    handle_nescdecl_attribute(alist, ndecl);
}

void handle_decl_attributes(attribute alist, data_declaration ddecl)
{
  scan_attribute (alist, alist)
    handle_decl_attribute(alist, ddecl);
}

void handle_field_attributes(attribute alist, field_declaration fdecl)
{
  scan_attribute (alist, alist)
    handle_field_attribute(alist, fdecl);
}

void handle_tag_attributes(attribute alist, tag_declaration tdecl)
{
  scan_attribute (alist, alist)
    handle_tag_attribute(alist, tdecl);
}

void handle_nescdecl_dd_attributes(dd_list alist, nesc_declaration ndecl)
{
  dd_list_pos attr;

  if (alist)
    dd_scan (attr, alist)
      handle_nescdecl_attribute(DD_GET(attribute, attr), ndecl);
}

void handle_decl_dd_attributes(dd_list alist, data_declaration ddecl)
{
  dd_list_pos attr;

  if (alist)
    dd_scan (attr, alist)
      handle_decl_attribute(DD_GET(attribute, attr), ddecl);
}

void handle_field_dd_attributes(dd_list alist, field_declaration fdecl)
{
  dd_list_pos attr;

  if (alist)
    dd_scan (attr, alist)
      handle_field_attribute(DD_GET(attribute, attr), fdecl);
}

void handle_tag_dd_attributes(dd_list alist, tag_declaration tdecl)
{
  dd_list_pos attr;

  if (alist)
    dd_scan (attr, alist)
      handle_tag_attribute(DD_GET(attribute, attr), tdecl);
}
