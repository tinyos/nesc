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

static const char *tokenval(nd_arg arg)
{
  return CAST(nd_token, arg)->str;
}

static bool filter_file(ndf_op filter, location loc)
{
  return !fnmatch(tokenval(filter->args), loc->filename, 0);
}

static bool filter_name(ndf_op filter, const char *name)
{
  return !regexec(filter->info, name, 0, NULL, 0);
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
      const char *reqname = tokenval(reqattr);

      dd_scan (actualattr, attrs)
	{
	  nesc_attribute a = DD_GET(nesc_attribute, actualattr);

	  if (!strcmp(a->word1->cstring.data, reqname))
	    return TRUE;
	}
    }
  return FALSE;
}


static bool fddecl_file(ndf_op op, data_declaration ddecl)
{
  return filter_file(op, ddecl->definition ? ddecl->definition->location : ddecl->ast->location);
}

static bool fddecl_attribute(ndf_op op, data_declaration ddecl)
{
  return filter_attribute(op, ddecl->attributes);
}

static void fcompile_name(ndf_op op)
{
  int err;

  op->info = ralloc(permanent, regex_t);
  err = regcomp(op->info, tokenval(op->args), REG_EXTENDED);
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

static struct {
  const char *name;
  const char *args;
  void (*compile)(ndf_op op);
  bool (*execute_ddecl)(ndf_op op, data_declaration ddecl);
} ops[] = {
  { "file", "t", NULL, fddecl_file },
  { "name", "t", fcompile_name, fddecl_name },
  { "attribute", "*t", NULL, fddecl_attribute }
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

static nd_filter current_filter;

static bool dofilter_ddecl(nd_filter f, data_declaration ddecl)
{
  switch (f->kind)
    {
    case kind_ndf_and: {
      ndf_and f1 = CAST(ndf_and, f);
      return dofilter_ddecl(f1->filter1, ddecl) &&
	dofilter_ddecl(f1->filter2, ddecl);
    }
    case kind_ndf_or: {
      ndf_or f1 = CAST(ndf_or, f);
      return dofilter_ddecl(f1->filter1, ddecl) ||
	dofilter_ddecl(f1->filter2, ddecl);
    }
    case kind_ndf_not: {
      ndf_not f1 = CAST(ndf_not, f);
      return !dofilter_ddecl(f1->filter1, ddecl);
    }
    case kind_ndf_op: {
      ndf_op f1 = CAST(ndf_op, f);
      return ops[f1->filter_index].execute_ddecl(f1, ddecl);
    }
    default:
      assert(0); return FALSE;
    }
}

bool dump_filter_ddecl(data_declaration ddecl)
{
  return !current_filter || dofilter_ddecl(current_filter, ddecl);
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
	    ndf_and x = new_ndf_and(permanent, extracted, f);
	    extracted = CAST(nd_filter, x);
	  }
	else
	  extracted = f;
      }

  current_filter = extracted;
}
