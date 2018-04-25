/* This file is part of the nesC compiler.
   Copyright (C) 2004 The Regents of the University of California.

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA 02110-1301 USA.  */

//$Id$
//@author Cory Sharp <cssharp@eecs.berkeley.edu>

/* Basic pointer sizes and alignments for a machine set in the environment
 * variable NESC_MACHINE of the form:
 *
 *   export NESC_MACHINE="long_double=8,4 gcc=some-other-gcc"
 *
 * Particular order of the keynames is not necessary.  The defaults below
 * are taken for otherwise unspecified values.
 */

#include <string.h>

static machine_spec env_machine = {
  "env", 
  gcc_save_machine_options,
  /* [default] */       /* [keyname] */
  FALSE,		/* big_endian */
  FALSE,		/* pcc_bitfield_type_matters */
  8,			/* empty_field_boundary */
  8,			/* structure_size_boundary */
  1,			/* word size */
  {2, 1},		/* pointer */
  {4, 1},		/* float */
  {4, 1},		/* double */
  {4, 1},		/* long_double */
  {2, 1},		/* short */
  {2, 1},		/* int */
  {4, 1},		/* long */
  {8, 1},		/* long_long */
  {1, 1},		/* _Bool */
  1, 1, 1, 1,		/* int1248_align */
  2, 2,			/* wchar_size_size */
  TRUE, TRUE,		/* char_wchar_signed */
  NULL,			/* no attribute for async functions */

  NULL,				/* adjust_field_align */

  NULL, NULL, NULL, NULL,	/* Attributes: need some way to specify this */
  NULL, NULL,			/* preinit, init */
  NULL,				/* token */
  NULL,				/* keil special */
  gcc_global_cpp_init,		/* global cpp support */
  NULL				/* per-file cpp support */
};

static const char *find_char(const char *str, const char *strend, char ch)
{
  while (str != strend && *str != ch)
    str++;
  return str;
}

static const char *find_not_char(const char *str, const char *strend, char ch)
{
  while (str != strend && *str == ch)
    str++;
  return str;
}

static bool is_literali(const char *literal, const char *str, const char *strend)
{
  int n = strlen(literal);
  if (n == (strend - str) && strncasecmp(literal, str, n) == 0)
    return TRUE;
  return FALSE;
}

static int scan_boolean(const char *str, const char *strend)
{
  if (is_literali("false", str, strend))
    return 0;
  if (is_literali("true", str, strend))
    return 1;
  return -1;
}

static bool scan_intlist(const char *str, const char *strend, int *intlist,
			 int count)
{
  while (count-- > 0)
    {
      if (str == strend)
	return FALSE;
      *intlist++ = atoi(str);
      str = find_not_char(find_char(str, strend, ','), strend, ',');
    }
  if (str != strend)
    return FALSE;
  return TRUE;
}

static bool scan_env_machine(machine_spec * machine, const char *envname)
{
  const char *begin = getenv(envname);
  const char *end = begin;
  int n_errors = 0;

  struct {
    const char *name;
    machine_type_spec *spec;
  } typespecs[] = {
    { "pointer", &(machine->tptr) },
    { "float", &(machine->tfloat) },
    { "double", &(machine->tdouble) },
    { "long_double", &(machine->tlong_double) },
    { "short", &(machine->tshort) },
    { "int", &(machine->tint) },
    { "long", &(machine->tlong) },
    { "long_long", &(machine->tlong_long) },
    { "_Bool", &(machine->t_Bool) },
    { NULL, NULL }
  };

  if (begin == NULL)
    {
      error("environment variable %s is undefined", envname);
      return FALSE;
    }

  end = begin + strlen(begin);
  while (begin != end)
    {
      const char *space = find_char(begin, end, ' ');
      const char *equal = find_char(begin, space, '=');
      const char *value = find_not_char(equal, space, '=');
      const char *name = "(unknown)";
      int intlist[4] = { 0, 0, 0, 0 };

      if (is_literali(name = "pcc_bitfield_type_matters", begin, equal))
	{
	  int b = scan_boolean(value, space);
	  if (b != -1)
	    {
	      machine->pcc_bitfield_type_matters = b ? TRUE : FALSE;
	    }
	  else
	    {
	      error("%s.%s, expected 'false' or 'true'", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "big_endian", begin, equal))
	{
	  int b = scan_boolean(value, space);
	  if (b != -1)
	    {
	      machine->big_endian = b ? TRUE : FALSE;
	    }
	  else
	    {
	      error("%s.%s, expected 'false' or 'true'", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "empty_field_boundary", begin, equal))
	{
	  if (scan_intlist(value, space, intlist, 1) == TRUE)
	    {
	      machine->empty_field_boundary = intlist[0];
	    }
	  else
	    {
	      error("%s.%s, expected one int", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "structure_size_boundary", begin, equal))
	{
	  if (scan_intlist(value, space, intlist, 1) == TRUE)
	    {
	      machine->structure_size_boundary = intlist[0];
	    }
	  else
	    {
	      error("%s.%s, expected one int", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "word_size", begin, equal))
	{
	  if (scan_intlist(value, space, intlist, 1) == TRUE)
	    {
	      machine->word_size = intlist[0];
	    }
	  else
	    {
	      error("%s.%s, expected one int", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "int1248_align", begin, equal))
	{
	  if (scan_intlist(value, space, intlist, 4) == TRUE)
	    {
	      machine->int1_align = intlist[0];
	      machine->int2_align = intlist[1];
	      machine->int4_align = intlist[2];
	      machine->int8_align = intlist[3];
	    }
	  else
	    {
	      error("%s.%s, expected 4 ints", envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "wchar_size_size", begin, equal))
	{
	  if (scan_intlist(value, space, intlist, 2) == TRUE)
	    {
	      machine->wchar_t_size = intlist[0];
	      machine->size_t_size = intlist[1];
	    }
	  else
	    {
	      error("%s.%s, expected 2 ints, wchar_t size and size_t size",
		     envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "char_wchar_signed", begin, equal))
	{
	  const char *comma = find_char(value, space, ',');
	  if (comma != space)
	    {
	      int b1 = scan_boolean(value, comma);
	      int b2 =
		scan_boolean(find_not_char(comma, space, ','), space);
	      if (b1 != -1 && b2 != -1)
		{
		  machine->char_signed = b1 ? TRUE : FALSE;
		  machine->wchar_t_signed = b2 ? TRUE : FALSE;
		}
	      else
		{
		  error("%s.%s, bools must be 'false' or 'true'", envname,
			 name);
		  n_errors++;
		}
	    }
	  else
	    {
	      error("%s.%s, expected 2 bools, char and wchar signed",
		     envname, name);
	      n_errors++;
	    }
	}
      else if (is_literali(name = "async_functions", begin, equal))
	{
	  int l = space - value;
	  char *s = rstralloc(permanent, l + 1);

	  memcpy(s, value, l);
	  s[l] = '\0';
	  machine->async_functions_atribute = s;
	}
      else
	{
	  int i = 0;
	  for (i = 0; typespecs[i].name != NULL; i++)
	    {
	      if (is_literali(name = typespecs[i].name, begin, equal))
		{
		  if (scan_intlist(value, space, intlist, 2) == TRUE)
		    {
		      typespecs[i].spec->size = intlist[0];
		      typespecs[i].spec->align = intlist[1];
		      break;
		    }
		  else
		    {
		      error("%s.%s, expected 2 ints, size and align",
			     envname, name);
		      n_errors++;
		    }
		}
	    }

	  if (typespecs[i].name == NULL)
	    {
	      error("%s, unknown field name starting at %s", envname, begin);
	      n_errors++;
	    }
	}

      begin = find_not_char(space, end, ' ');
    }

  return (n_errors == 0) ? TRUE : FALSE;
}
