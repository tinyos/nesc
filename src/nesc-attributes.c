#include "parser.h"
#include "init.h"
#include "c-parse.h"
#include "semantics.h"
#include "nesc-attributes.h"

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

  start_init(NULL);
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
