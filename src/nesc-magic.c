#include <stdlib.h>

#include "parser.h"
#include "nesc-magic.h"
#include "semantics.h"
#include "AST_utils.h"
#include "c-parse.h"

void declare_magic(const char *name,
		   type return_type, typelist argument_types,
		   expression (*magic_reduce)(function_call fcall))
{
  struct data_declaration tempdecl;
  type ftype = make_function_type(return_type, argument_types, FALSE, FALSE);

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location), name, ftype);
  tempdecl.kind = decl_magic_function;
  tempdecl.magic_reduce = magic_reduce;
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

static env unique_env;
static region unique_region;

expression unique_reduce(function_call fcall)
{
  expression name = fcall->args;
  unsigned int *lastval;
  const wchar_t *name_wstr;
  char *name_str;
  size_t length_as_str;

  if (!is_string(name))
    {
      error("argument to `unique' must be a string");
      return CAST(expression, fcall);
    }

  name_wstr = CAST(string, name)->ddecl->chars;
  length_as_str = wcstombs(NULL, name_wstr, 0) + 1;
  if (length_as_str < 0)
    {
      error("can't handle this string as argument to `unique'");
      return CAST(expression, fcall);
    }
  name_str = alloca(length_as_str);
  length_as_str = wcstombs(name_str, name_wstr, length_as_str + 1);
  assert(length_as_str >= 0);

  lastval = env_lookup(unique_env, name_str, FALSE);
  if (!lastval)
    {
      lastval = ralloc(unique_region, int);
      name_str = rstrdup(unique_region, name_str);
      env_add(unique_env, name_str, lastval);
    }

  return build_uint_constant(parse_region, fcall->location,
			     unsigned_int_type, (*lastval)++);
}

static void unique_init(void)
{
  typelist string_args;

  string_args = new_typelist(parse_region);
  typelist_append(string_args, make_pointer_type(char_type));
  declare_magic("unique", int_type, string_args, unique_reduce);
  unique_region = newregion();
  unique_env = new_env(unique_region, NULL);
}

void init_magic_functions(void)
{
  unique_init();
}
