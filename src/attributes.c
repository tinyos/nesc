#include "parser.h"
#include "attributes.h"
#include "semantics.h"

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
      if (ddecl->kind == decl_function && ddecl->ftype == function_normal)
	ddecl->spontaneous = TRUE;
      else
	error_with_location(attr->location, "`spontaneous' attribute is for external functions only");
    }
  else
    handle_type_attribute(attr, &ddecl->type);
  /*else
    ignored_attribute(attr);*/
}

/* Note: fdecl->bitwidth is not yet set when this is called */
void handle_field_attribute(attribute attr, field_declaration fdecl)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "packed") || !strcmp(name, "__packed__"))
    fdecl->packed = TRUE;
  /*else
    ignored_attribute(attr);*/
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
  /*else
    ignored_attribute(attr);*/
}

bool handle_type_attribute(attribute attr, type *t)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "combine") || !strcmp(name, "__combine__"))
    {
      if (!attr->word2 || attr->args)
	error_with_location(attr->location, "wrong number of arguments specified for `combine' attribute");
      else
	{
	  const char *combiner = attr->word2->cstring.data;
	  data_declaration cdecl = lookup_id(combiner, FALSE);


	  if (cdecl->kind != decl_function ||
	      !(cdecl->ftype == function_normal || cdecl->ftype == function_static))
	    error_with_location(attr->location, "combiner `%s' is not a C function");
	  else /* XXX: should check type's sig */
	    *t = make_combiner_type(*t, cdecl);
	}

      return TRUE;
    }
  return FALSE;
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
