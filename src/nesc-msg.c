/* This file is part of the nesC compiler.
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
Boston, MA 02111-1307, USA.  */

#include <ctype.h>

#include "parser.h"
#include "nesc-msg.h"
#include "semantics.h"
#include "constants.h"

const char *selected_type;

void select_nesc_msg(const char *name)
{
  selected_type = name;
}

static void dump_type(type t)
{
  if (type_complex(t))
    {
      printf("C");
      t = make_base_type(t);
    }

  /* Enums treated as ints for now */
  if (type_integer(t))
    if (type_unsigned(t))
      printf("U");
    else
      printf("I");
  else if (type_float(t))
    printf("F");
  else if (type_double(t))
    printf("D");
  else if (type_long_double(t))
    printf("LD");
  else if (type_union(t))
    printf("AU");
  else if (type_struct(t))
    printf("AS");
  else
    assert(0);
}

static void dump_fields(region r, const char *prefix, field_declaration fields)
{
  while (fields)
    {
      if (fields->name) /* skip anon fields */
	{
	  type t = fields->type;

	  printf("  %s%s ", prefix, fields->name);
	  while (type_array(t))
	    {
	      type base = type_array_of(t);
	      expression size = type_array_size(t);

	      printf("[%lu]", (unsigned long)constant_uint_value(size->cst));
	      t = base;
	    }
	  dump_type(t);

	  assert(fields->offset_cc);
	  printf(" %lu %lu\n", (unsigned long)fields->offset,
		 (unsigned long)
		 (fields->bitwidth >= 0 ? fields->bitwidth :
		  BITSPERBYTE * type_size(t)));

	  if (type_aggregate(t))
	    {
	      tag_declaration tdecl = type_tag(t);
	      char *newprefix = rarrayalloc(r, strlen(prefix) + strlen(fields->name) + 2, char);

	      sprintf(newprefix, "%s%s.", prefix, fields->name);
	      dump_fields(r, newprefix, tdecl->fieldlist);
	      printf("  %s%s AX\n", prefix, fields->name);
	    }
	}
      fields = fields->next;
    }
}

static int am_type(region r, tag_declaration tdecl)
{
  char *am_name = rarrayalloc(r, strlen(tdecl->name) + 4, char), *s;
  data_declaration am_decl;

  sprintf(am_name, "AM_%s", tdecl->name);
  for (s = am_name; *s; s++)
    *s = toupper(*s);

  am_decl = lookup_global_id(am_name);

  if (am_decl && am_decl->kind == decl_constant)
    {
      known_cst am_val = am_decl->value;

      if (type_integer(am_val->type) && cval_knownvalue(am_val->cval))
	return constant_sint_value(am_val);
    }
  return -1;
}

static void dump_layout(tag_declaration tdecl)
{
  region r = newregion();
  

  printf("%s %s %lu %d\n",
	 tdecl->kind == kind_struct_ref ? "struct" : "union",
	 tdecl->name,
	 (unsigned long)tdecl->size,
	 am_type(r, tdecl));

  dump_fields(r, "", tdecl->fieldlist);

  deleteregion(r);
}

void dump_msg_layout(void)
{
  tag_declaration tdecl;

  /* We look for a tagged type with name selected_type in the global
     environment, and dump the layout in a perl-friendly format */

  if (!selected_type)
    return;

  tdecl = env_lookup(global_env->tag_env, selected_type, FALSE);

  if (!tdecl)
    {
      fprintf(stderr, "error: tag %s not found\n", selected_type);
      exit(1);
    }

  if (tdecl->kind == kind_enum_ref)
    {
      fprintf(stderr, "error: %s is an enum\n", selected_type);
      exit(1);
    }

  if (!tdecl->size_cc)
    {
      fprintf(stderr, "error: %s is variable size\n", selected_type);
      exit(1);
    }

  if (type_contains_pointers(make_tagged_type(tdecl)))
    {
      fprintf(stderr, "error: %s contains pointers\n", selected_type);
      exit(1);
    }

  dump_layout(tdecl);
}

