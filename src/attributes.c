#include "parser.h"
#include "attributes.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "machine.h"

/* Provide warnings about ignored attributes and attribute lists */

void ignored_attribute(attribute attr)
{
  warning_with_location(attr->location, "`%s' attribute directive ignored",
			attr->word1->cstring.data);
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

static bool require_function(attribute attr, data_declaration ddecl)
{
  if (ddecl->kind == decl_function && ddecl->ftype == function_normal)
    return TRUE;

  error_with_location(attr->location, "`%s' attribute is for external functions only", attr->word1->cstring.data);
  return FALSE;
}

void handle_decl_attribute(attribute attr, data_declaration ddecl)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "transparent_union") ||
      !strcmp(name, "__transparent_union__"))
    {
      if (attr->word2 || attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `transparent_union' attribute");

      if (ddecl->kind == decl_variable && ddecl->isparameter &&
	  type_union(ddecl->type))
	transparent_union_argument(ddecl);
      else if (ddecl->kind == decl_typedef && type_union(ddecl->type))
	transparent_union_argument(ddecl);
      else
	ignored_attribute(attr);
    }
  else if (!strcmp(name, "C"))
    {
      if (!ddecl->isexternalscope)
	error_with_location(attr->location, "`C' attribute is for symbols with external scope only");
      else
	ddecl->Cname = TRUE;
    }
  else if (!strcmp(name, "spontaneous"))
    {
      if (require_function(attr, ddecl))
	{
	  /* The test avoids overriding the effect of atomic_hwevent */
	  if (!ddecl->spontaneous)
	    ddecl->spontaneous = c_call_nonatomic;
	}
    }
  else if (!strcmp(name, "atomic_hwevent"))
    {
      if (require_function(attr, ddecl))
	{
	  ddecl->async = TRUE;
	  ddecl->spontaneous = c_call_atomic;
	}
    }
  else if (!strcmp(name, "hwevent"))
    {
      if (require_function(attr, ddecl))
	{
	  ddecl->async = TRUE;
	  ddecl->spontaneous = c_call_nonatomic;
	}
    }
  else if (!strcmp(name, "noinline"))
    {
      ddecl->noinline = TRUE;
    }
  else if (!(target->decl_attribute &&
	     target->decl_attribute(attr, ddecl)) &&
	   !handle_type_attribute(attr, &ddecl->type))
    /*ignored_attribute(attr)*/;
}

/* Note: fdecl->bitwidth is not yet set when this is called */
void handle_field_attribute(attribute attr, field_declaration fdecl)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "packed") || !strcmp(name, "__packed__"))
    fdecl->packed = TRUE;
  else if (!(target->field_attribute &&
	     target->field_attribute(attr, fdecl)))
    /*ignored_attribute(attr)*/;
}

void handle_tag_attribute(attribute attr, tag_declaration tdecl)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "transparent_union") ||
      !strcmp(name, "__transparent_union__"))
    {
      if (attr->word2 || attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `transparent_union' attribute");

      if (tdecl->kind == kind_union_ref)
	{
	  tdecl->transparent_union = TRUE;
	  /* XXX: Missing validity checks (need cst folding I think) */
	}
      else
	ignored_attribute(attr);
    }
  else if (!strcmp(name, "packed") || !strcmp(name, "__packed__"))
    tdecl->packed = TRUE;
  else if (!(target->tag_attribute &&
	     target->tag_attribute(attr, tdecl)))
    /*ignored_attribute(attr)*/;
}

bool handle_type_attribute(attribute attr, type *t)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "combine") || !strcmp(name, "__combine__"))
    {
      if (!attr->word2 || attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `combine' attribute");
      else
	handle_combine_attribute(attr->location, attr->word2->cstring.data, t);
      return TRUE;
    }
  else 
    return target->type_attribute && target->type_attribute(attr, t);
}

/* Functions to handle regular and dd list of attributes */

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
