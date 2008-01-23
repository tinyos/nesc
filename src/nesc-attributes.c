#include "parser.h"
#include "init.h"
#include "c-parse.h"
#include "semantics.h"
#include "nesc-attributes.h"
#include "env.h"
#include "edit.h"
#include "attributes.h"

typedef struct internal_attribute
{
  const char *name;

  void (*handle_ndecl)(nesc_attribute attr,
		       nesc_declaration ndecl);
  void (*handle_decl)(nesc_attribute attr,
		      data_declaration ddecl);
  void (*handle_tag)(nesc_attribute attr,
		     tag_declaration tdecl);
  void (*handle_field)(nesc_attribute attr,
		       field_declaration fdecl);
  void (*handle_type)(nesc_attribute attr,
		      type *t);
} *iattr;

static env internal_attributes;

/* Return a reference to attribute tag 
   (different from xref_tag because there is no implicit declaration) */
tag_ref lookup_attribute(word tag)
{
  tag_ref tref = newkind_tag_ref(parse_region, kind_attribute_ref,
				 tag->location, tag, NULL, NULL, FALSE);
  tag_declaration tdecl = lookup_tag(tref, FALSE);

  if (!tdecl)
    error_with_location(tag->location, "unknown attribute `%s'",
			tag->cstring.data);
  tref->tdecl = tdecl;

  return tref;
}

nesc_attribute start_attribute_use(word name)
{
  /* Prepare to read an initialiser for the attribute definition 
     specified by name */
  nesc_attribute attr = new_nesc_attribute(parse_region, name->location, name,
					   NULL);
  tag_ref aref = lookup_attribute(name); /* XXX: aref leaks */
  type atype = error_type;

  /* Create new environment so that we can track whether this is a
     deputy scope or not. Using an environment makes it easy to
     recover parsing errors: we just call poplevel in the appropriate
     error production (see nattrib rules in c-parse.y). */
  pushlevel(FALSE);

  attr->tdecl = aref->tdecl;
  if (aref->tdecl)
    {
      atype = make_tagged_type(aref->tdecl);
      if (aref->tdecl->deputy_scope)
	current.env->deputy_scope = TRUE;
    }

  start_init(NULL, attr);
  really_start_incremental_init(atype);

  return attr;
}

attribute finish_attribute_use(nesc_attribute attr, expression init)
{
  attr->arg1 = make_init_list(attr->word1->location, init); 
  finish_init(); 
  poplevel();

  return CAST(attribute, attr);
}

void define_internal_attribute(const char *name,
			       void (*handle_ndecl)(nesc_attribute attr,
						    nesc_declaration ndecl),
			       void (*handle_decl)(nesc_attribute attr,
						   data_declaration ddecl),
			       void (*handle_tag)(nesc_attribute attr,
						  tag_declaration tdecl),
			       void (*handle_field)(nesc_attribute attr,
						    field_declaration fdecl),
			       void (*handle_type)(nesc_attribute attr,
						   type *t),
			       ...)
{
  va_list args;
  field_declaration *next_field;
  word attr_word;
  type_element attr_tag;
  tag_declaration attr_decl;
  struct internal_attribute *iattr;

  /* Build and declare the attribute */
  current.env = global_env;
  attr_word = build_word(parse_region, name);
  attr_tag = start_struct(dummy_location, kind_attribute_ref, attr_word);
  attr_decl = CAST(tag_ref, attr_tag)->tdecl;
  attr_decl->fields = new_env(parse_region, NULL);
  next_field = &attr_decl->fieldlist;

  /* Fields. A fieldname, fieldtype argument list, terminated with a
     null fieldname. We build a semi-fake struct for these.
  */
  va_start(args, handle_type);
  for (;;)
    {
      const char *field_name = va_arg(args, const char *);
      field_declaration field;

      if (!field_name)
	break;
      field = ralloc(parse_region, struct field_declaration);
      field->containing_tag = attr_decl;
      *next_field = field;
      next_field = &field->next;
      field->name = field_name;
      field->type = va_arg(args, type);
      field->bitwidth = field->offset = cval_unknown_number;

      env_add(attr_decl->fields, field_name, field);
    }
  va_end(args);

  /* Add to internal attributes table */
  iattr = ralloc(permanent, struct internal_attribute);
  iattr->name = name;
  iattr->handle_ndecl = handle_ndecl;
  iattr->handle_decl = handle_decl;
  iattr->handle_tag = handle_tag;
  iattr->handle_field = handle_field;
  iattr->handle_type = handle_type;
  env_add(internal_attributes, name, iattr);
}

ivalue lookup_attribute_field(nesc_attribute attr, const char *name)
/* Returns: The initialiser for field name in attr, or NULL if it's not
     found 
*/
{
  ivalue init = attr->arg1->ivalue;
  ivalue_field ifields;

  assert(init->kind == iv_structured);
  for (ifields = init->u.structured; ifields; ifields = ifields->next)
    if (!strcmp(ifields->field->name, name))
      return ifields->value;

  return NULL;
}

static iattr internal_lookup(nesc_attribute attr)
{
  return env_lookup(internal_attributes, attr->word1->cstring.data, TRUE);
}

static void save_user_attribute(nesc_attribute attr, dd_list *alist)
{
  if (!*alist)
    *alist = dd_new_list(parse_region);
  dd_add_last(parse_region, *alist, attr);
}

void handle_nesc_type_attribute(nesc_attribute attr, type *t)
{
  iattr handler = internal_lookup(attr);

  if (handler)
    {
      if (handler->handle_type)
	handler->handle_type(attr, t);
      else
	ignored_nesc_attribute(attr);
    }
  /* In the future, we might want to record the attribute on the type,
     and support dumping this information in nesc-dump.c. For now,
     though, attributes on types are only useful if they are @macro()
     attributes */
}

void handle_nesc_decl_attribute(nesc_attribute attr, data_declaration ddecl)
{
  iattr handler = internal_lookup(attr);

  if (handler)
    {
      if (handler->handle_decl)
	handler->handle_decl(attr, ddecl);
      else
	ignored_nesc_attribute(attr);
    }
  else
    save_user_attribute(attr, &ddecl->attributes);
}

void handle_nesc_field_attribute(nesc_attribute attr, field_declaration fdecl)
{
  iattr handler = internal_lookup(attr);

  if (handler)
    {
      if (handler->handle_field)
	handler->handle_field(attr, fdecl);
      else
	ignored_nesc_attribute(attr);
    }
  else
    save_user_attribute(attr, &fdecl->attributes);
}

void handle_nesc_tag_attribute(nesc_attribute attr, tag_declaration tdecl)
{
  iattr handler = internal_lookup(attr);

  if (handler)
    {
      if (handler->handle_tag)
	handler->handle_tag(attr, tdecl);
      else
	ignored_nesc_attribute(attr);
    }
  else
  save_user_attribute(attr, &tdecl->attributes);
}

void handle_nesc_nescdecl_attribute(nesc_attribute attr, nesc_declaration ndecl)
{
  iattr handler = internal_lookup(attr);

  if (handler)
    {
      if (handler->handle_ndecl)
	handler->handle_ndecl(attr, ndecl);
      else
	ignored_nesc_attribute(attr);
    }
  else
    save_user_attribute(attr, &ndecl->attributes);
}

void init_nesc_attributes(void)
{
  internal_attributes = new_env(permanent, NULL);
}
