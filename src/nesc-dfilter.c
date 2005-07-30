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

#include <sys/types.h>
#include <regex.h>
#include <fnmatch.h>
#include "parser.h"
#include "nesc-dump.h"
#include "nesc-dspec.h"
#include "nesc-dspec-int.h"
#include "nesc-semantics.h"

/* Filters for XML dump requests. Current filters:
     file(globexp): match containing file name (unix-style file matching)
     name(regexp): match by item name (regular expression matching)
     attribute(namelist): match items containing one of the attributes
       in the namelist
     component(cname): match containing component name
     global(): match items from the global (C) scope
     instance(): match items in instances of generic components
     abstract(): match items that are not fully instantiated
       !global() && !abstract() && !instances(): "in a non-generic component"
       abstract() && !instance(): "in a generic component"
       !abstract() && instance(): "in a fully instantiated generic
         component (i.e., shows up in the generated C code)"
       abstract() && instance(): "a partially instantiated generic component,
         i.e., a generic component instantiated within a 
	 generic configuration"

  Filters can be combined with and, or, not.
*/

/* Implementation of filters is OOish, see the filter_op structure.
   Filters can be applied to nesc/tag/data_declaration objects (separate
   method for each).

   Each filter has an argument specification string of the form:
     - a sequence of n and t's for token and int arguments (in order)
     - "*t" or "*n" for an arbitrary list of tokens or numbers.
   See check_arg, make_ndf_op to extend this.
*/

static bool filter_file(ndf_op filter, location loc)
{
  return !fnmatch(nd_tokenval(filter->args), loc->filename, 0);
}

static bool filter_name(ndf_op filter, const char *name)
{
  return name && !regexec(filter->info, name, 0, NULL, 0);
}

static bool filter_attribute(ndf_op filter, dd_list/*nesc_attribute*/ attrs)
{
  nd_arg reqattr;
  dd_list_pos actualattr;

  if (!attrs)
    return FALSE;

  /* return true if intersection of attributes in filter and attrs is
     non-empty */
  scan_nd_arg (reqattr, filter->args)
    {
      const char *reqname = nd_tokenval(reqattr);

      dd_scan (actualattr, attrs)
	{
	  nesc_attribute a = DD_GET(nesc_attribute, actualattr);

	  if (!strcmp(a->word1->cstring.data, reqname))
	    return TRUE;
	}
    }
  return FALSE;
}

static bool filter_component(ndf_op filter, nesc_declaration container)
{
  return container &&
    !strcmp(nd_tokenval(filter->args), container->instance_name);
}



static bool fddecl_file(ndf_op op, data_declaration ddecl)
{
  return filter_file(op, ddecl->definition ? ddecl->definition->location : ddecl->ast->location);
}

static bool fndecl_file(ndf_op op, nesc_declaration ndecl)
{
  return filter_file(op, ndecl->ast->location);
}

static bool ftdecl_file(ndf_op op, tag_declaration tdecl)
{
  return tdecl->definition && filter_file(op, tdecl->definition->location);
}

static bool fddecl_attribute(ndf_op op, data_declaration ddecl)
{
  return filter_attribute(op, ddecl->attributes);
}

static bool fndecl_attribute(ndf_op op, nesc_declaration ndecl)
{
  return filter_attribute(op, ndecl->attributes);
}

static bool ftdecl_attribute(ndf_op op, tag_declaration tdecl)
{
  return filter_attribute(op, tdecl->attributes);
}

static void fcompile_name(ndf_op op)
{
  int err;

  op->info = ralloc(dump_region, regex_t);
  err = regcomp(op->info, nd_tokenval(op->args), REG_EXTENDED);
  if (err)
    {
      char errmsg[200];

      regerror(err, op->info, errmsg, sizeof errmsg);
      nderror(errmsg);
    }
 }

static bool fddecl_name(ndf_op op, data_declaration ddecl)
{
  return filter_name(op, ddecl->name);
}

static bool fndecl_name(ndf_op op, nesc_declaration ndecl)
{
  return filter_name(op, ndecl->instance_name);
}

static bool ftdecl_name(ndf_op op, tag_declaration tdecl)
{
  return filter_name(op, tdecl->name);
}

static bool fddecl_component(ndf_op op, data_declaration ddecl)
{
  return filter_component(op, ddecl->container);
}

static bool fndecl_component(ndf_op op, nesc_declaration ndecl)
{
  /* XXX: should this do something obvious w/ instantiated
     abstract components? */
  return FALSE;
}

static bool ftdecl_component(ndf_op op, tag_declaration tdecl)
{
  return filter_component(op, tdecl->container);
}

static bool fddecl_global(ndf_op op, data_declaration ddecl)
{
  return !ddecl->container && !ddecl->container_function;
}

static bool fndecl_global(ndf_op op, nesc_declaration ndecl)
{
  /* XXX: see fndecl_component comment */
  return TRUE;
}

static bool ftdecl_global(ndf_op op, tag_declaration tdecl)
{
  return !tdecl->container /* && !tdecl->container_function*/;
}

static bool is_instance(nesc_declaration ndecl)
{
  return ndecl && ndecl->arguments;
}

static bool fddecl_instance(ndf_op op, data_declaration ddecl)
{
  return is_instance(ddecl_container(ddecl));
}

static bool fndecl_instance(ndf_op op, nesc_declaration ndecl)
{
  return is_instance(ndecl);
}

static bool ftdecl_instance(ndf_op op, tag_declaration tdecl)
{
  return is_instance(tdecl_container(tdecl));
}

static bool is_abstract(nesc_declaration ndecl)
{
  return ndecl && ndecl->abstract;
}

static bool fddecl_abstract(ndf_op op, data_declaration ddecl)
{
  return is_abstract(ddecl_container(ddecl));
}

static bool fndecl_abstract(ndf_op op, nesc_declaration ndecl)
{
  return is_abstract(ndecl);
}

static bool ftdecl_abstract(ndf_op op, tag_declaration tdecl)
{
  return is_abstract(tdecl_container(tdecl));
}

static struct filter_op {
  const char *name;
  const char *args; /* Argument specification (see top) */

  /* (optional) filter "compilation" (eg for regexps) */
  void (*compile)(ndf_op op); 

  /* Execute filter op on Xdecl */
  bool (*execute_ddecl)(ndf_op op, data_declaration ddecl);
  bool (*execute_ndecl)(ndf_op op, nesc_declaration ndecl);
  bool (*execute_tdecl)(ndf_op op, tag_declaration tdecl);
} ops[] = {
  { "file", "t", NULL, fddecl_file, fndecl_file, ftdecl_file },
  { "name", "t", fcompile_name, fddecl_name, fndecl_name, ftdecl_name },
  { "component", "t", NULL, fddecl_component, fndecl_component, ftdecl_component },
  { "global", "", NULL, fddecl_global, fndecl_global, ftdecl_global },
  { "instance", "", NULL, fddecl_instance, fndecl_instance, ftdecl_instance },
  { "abstract", "", NULL, fddecl_abstract, fndecl_abstract, ftdecl_abstract },
  { "attribute", "*t", NULL, fddecl_attribute, fndecl_attribute, ftdecl_attribute }
};

static void check_arg(nd_arg arg, int kind)
{
  bool ok;

  switch (kind)
    {
    default: ok = TRUE;
    case 't': ok = is_nd_token(arg); break;
    case 'n': ok = is_nd_int(arg); break;
    }
  if (!ok)
    nderror("wrong argument type");
}

nd_filter make_ndf_op(region r, const char *name, nd_arg args)
{
  int nargs = nd_arg_length(args);
  ndf_op op = new_ndf_op(r, name, args, nargs);
  int i;
  
  for (i = 0; i < sizeof ops / sizeof *ops; i++)
    if (!strcmp(name, ops[i].name))
      {
	const char *argspec = ops[i].args;
	nd_arg arg;
	int old_ec = errorcount;

	op->filter_index = i;

	/* Check arguments */
	if (argspec[0] == '*')
	  scan_nd_arg (arg, args)
	    check_arg(arg, argspec[1]);
	else
	  {
	    scan_nd_arg (arg, args)
	      {
		if (!*argspec)
		  nderror("too many arguments");
		else
		  check_arg(arg, *argspec++);
	      }
	    if (*argspec)
	      nderror("not enough arguments");
	  }

	if (errorcount == old_ec && ops[i].compile)
	  ops[i].compile(op);

	return CAST(nd_filter, op);
      }
  nderror("unknown filter operator");

  return CAST(nd_filter, op);
}

/* The current filter, applied by dump_filter_Xdecl */
static nd_filter current_filter;

enum { filter_ddecl, filter_ndecl, filter_tdecl };

static bool dofilter(int op, nd_filter f, void *decl)
{
  switch (f->kind)
    {
    case kind_ndf_and: {
      ndf_and f1 = CAST(ndf_and, f);
      return dofilter(op, f1->filter1, decl) && dofilter(op, f1->filter2, decl);
    }
    case kind_ndf_or: {
      ndf_or f1 = CAST(ndf_or, f);
      return dofilter(op, f1->filter1, decl) || dofilter(op, f1->filter2, decl);
    }
    case kind_ndf_not: {
      ndf_not f1 = CAST(ndf_not, f);
      return !dofilter(op, f1->filter1, decl);
    }
    case kind_ndf_op: {
      ndf_op f1 = CAST(ndf_op, f);
      struct filter_op *fop = &ops[f1->filter_index];

      switch (op)
	{
	case filter_ddecl: return fop->execute_ddecl(f1, decl);
	case filter_ndecl: return fop->execute_ndecl(f1, decl);
	case filter_tdecl: return fop->execute_tdecl(f1, decl);
	default: assert(0); return FALSE;
	}
    }
    default: 
      assert(0); return FALSE;
    }
}

bool dump_filter_ddecl(data_declaration ddecl)
{
  return !current_filter || dofilter(filter_ddecl, current_filter, ddecl);
}

bool dump_filter_ndecl(nesc_declaration ndecl)
{
  return !current_filter || dofilter(filter_ndecl, current_filter, ndecl);
}

bool dump_filter_tdecl(tag_declaration tdecl)
{
  return !current_filter || dofilter(filter_tdecl, current_filter, tdecl);
}

void dump_set_filter(nd_option opt)
{
  nd_arg *optargs = &opt->args;
  nd_filter extracted = NULL;

  /* Extract filters from opt, build current filter. If multiple filters,
     just and them together. */
  while (*optargs)
    if (!is_nd_filter(*optargs))
      optargs = &(*optargs)->next;
    else
      {
	nd_filter f = CAST(nd_filter, *optargs);
	*optargs = (*optargs)->next;

	if (extracted)
	  {
	    ndf_and x = new_ndf_and(dump_region, extracted, f);
	    extracted = CAST(nd_filter, x);
	  }
	else
	  extracted = f;
      }

  current_filter = extracted;
}
