#include <stdlib.h>

#include "parser.h"
#include "nesc-magic.h"
#include "semantics.h"
#include "AST_utils.h"
#include "c-parse.h"
#include "constants.h"
#include "unparse.h"

void declare_magic(const char *name,
		   type return_type, typelist argument_types,
		   expression (*magic_reduce)(function_call fcall),
		   void (*magic_print)(function_call fcall))
{
  struct data_declaration tempdecl;
  type ftype = make_function_type(return_type, argument_types, FALSE, FALSE);

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location), name, ftype);
  tempdecl.kind = decl_magic_function;
  tempdecl.magic_reduce = magic_reduce;
  tempdecl.magic_print = magic_print;
  tempdecl.ftype = function_normal;

  declare(global_env, &tempdecl, FALSE);
}

expression magic_reduce(function_call fcall)
{
  if (is_identifier(fcall->arg1))
    {
      identifier called = CAST(identifier, fcall->arg1);

      if (called->ddecl->kind == decl_magic_function)
	{
	  /* we can assume arguments are of valid type and number,
	     check that they are constants */
	  bool all_constant = TRUE;
	  expression arg;
	  int argn = 1;

	  scan_expression (arg, fcall->args)
	    {
	      if (!(arg->cst || is_string(arg)))
		{
		  error("argument %d to magic function `%s' is not constant",
			argn, called->ddecl->name);
		  all_constant = FALSE;
		}
	      argn++;
	    }
	  
	  if (all_constant)
	    return called->ddecl->magic_reduce(fcall);
	}
    }
  return CAST(expression, fcall);
}

bool magic_print(function_call fcall)
{
  if (is_identifier(fcall->arg1))
    {
      identifier called = CAST(identifier, fcall->arg1);

      if (called->ddecl->kind == decl_magic_function)
	{
	  called->ddecl->magic_print(fcall);
	  return TRUE;
	}
    }
  return FALSE;
}

static env unique_env;
static region unique_region;

unsigned int *unique_parse(const char *uname, function_call fcall)
{
  expression name = fcall->args;
  unsigned int *lastval;
  const wchar_t *name_wstr;
  char *name_str;
  int length_as_str;

  if (!is_string(name))
    {
      error("argument to `%s' must be a string", uname);
      return NULL;
    }

  name_wstr = CAST(string, name)->ddecl->chars;
  length_as_str = wcs_mb_size(name_wstr);
  if (length_as_str < 0)
    {
      error("can't handle this string as argument to `%s'", uname);
      return NULL;
    }
  name_str = alloca(length_as_str);
  length_as_str = wcstombs(name_str, name_wstr, length_as_str);
  assert(length_as_str >= 0);

  lastval = env_lookup(unique_env, name_str, FALSE);
  if (!lastval)
    {
      lastval = ralloc(unique_region, int);
      name_str = rstrdup(unique_region, name_str);
      env_add(unique_env, name_str, lastval);
    }

  return lastval;
}

expression unique_reduce(function_call fcall)
{
  unsigned int *lastval = unique_parse("unique", fcall);

  if (lastval)
    return build_uint_constant(parse_region, fcall->location,
			       unsigned_int_type, (*lastval)++);
  else
    return CAST(expression, fcall);
}

expression uniqueCount_reduce(function_call fcall)
{
  unique_parse("uniqueCount", fcall);

  fcall->cst = make_unsigned_cst(0, unsigned_int_type);
  return CAST(expression, fcall);
}

void uniqueCount_print(function_call fcall)
{
  /* We already checked for errors when uniqueCount_reduce was called.
     So unique_parse will succeed. */
  output("%u", *unique_parse("uniqueCount", fcall));
}

static void unique_init(void)
{
  typelist string_args;

  string_args = new_typelist(parse_region);
  typelist_append(string_args, make_pointer_type(char_type));
  declare_magic("unique", unsigned_int_type, string_args, unique_reduce, NULL);
  declare_magic("uniqueCount", unsigned_int_type, string_args,
		uniqueCount_reduce, uniqueCount_print);
  unique_region = newregion();
  unique_env = new_env(unique_region, NULL);
}

void init_magic_functions(void)
{
  unique_init();
}
