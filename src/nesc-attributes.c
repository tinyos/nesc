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

tag_declaration start_attribute_use(word name)
{
  /* Prepare to read an initialiser for the attribute definition 
     specified by name */
  tag_ref aref = lookup_attribute(name);
  type atype = aref->tdecl ? make_tagged_type(aref->tdecl) : error_type;
  char *attrname = rstralloc(current.fileregion, strlen(name->cstring.data) + 2);

  sprintf(attrname, "@%s", name->cstring.data);
  start_init(NULL, attrname);
  really_start_incremental_init(atype);

  /* XXX: aref leaks */
  return aref->tdecl;
}

attribute finish_attribute_use(word name, expression init, tag_declaration tdecl)
{
  expression args = make_init_list(name->location, init); 
  nesc_attribute attr = new_nesc_attribute(parse_region, name->location, name, args);

  finish_init(); 
  attr->tdecl = tdecl;

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
			       ...)
{
  va_list args;
  declaration fields = NULL;
  word attr_word;
  tag_ref attr_tag;
  tag_declaration attr_decl;
  struct internal_attribute *iattr;

  /* Build and declare the attribute */

  /* Fields. Not yet implemented, will be a
     fieldname, fieldtype argument list, terminated with a null fieldname
  */
  va_start(args, handle_field);
  for (;;)
    {
      const char *field_name = va_arg(args, const char *);

      if (!field_name)
	break;
      assert(0); /* not implemented yet */
    }
  va_end(args);

  attr_word = build_word(parse_region, name);
  attr_tag = new_attribute_ref(parse_region, dummy_location, attr_word, NULL,
			       fields, TRUE);
  attr_decl = declare_global_tag(attr_tag);

  /* Add to internal attributes table */
  iattr = ralloc(permanent, struct internal_attribute);
  iattr->name = name;
  iattr->handle_ndecl = handle_ndecl;
  iattr->handle_decl = handle_decl;
  iattr->handle_tag = handle_tag;
  iattr->handle_field = handle_field;
  env_add(internal_attributes, name, iattr);
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

bool handle_nesc_type_attribute(nesc_attribute attr, type *t)
{
  assert(0); /* not used at the moment */
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
