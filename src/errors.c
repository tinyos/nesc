/* This file is part of the nesC compiler.

This file is derived from RC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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
Boston, MA 02111-1307, USA. */

#include <stdarg.h>

#include "parser.h"
#include "errors.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "flags.h"

/* Name of program invoked (from argv[0]).  */
const char *progname;

int errorcount;
int warningcount;

static location error_location;

/* Set and clear the error/warning location to use when there is
   no input file stack */
void set_error_location(location l)
{
  error_location = l;
}

void clear_error_location(void)
{
  error_location = NULL;
}

location current_location(void)
{
  if (current.lex.input)
    return &current.lex.input->l;
  else if (error_location)
    return error_location;
  else
    return dummy_location;
}

/* Count an error or warning.  Return 1 if the message should be printed.  */
int count_error(int warningp)
{
  if (warningp && inhibit_warnings)
    return 0;

  if (warningp && !warnings_are_errors)
    warningcount++;
  else
    {
      static int warning_message = 0;

      if (warningp && !warning_message)
	{
	  fprintf (stderr, "%s: warnings being treated as errors\n", progname);
	  warning_message = 1;
	}
      errorcount++;
    }

  return 1;
}

/* Function of last error message;
   more generally, function such that if next error message is in it
   then we don't have to mention the function name.  */
static function_decl last_error_function = NULL;

/* Used to detect when current.lex.input has changed since last described.  */
static int last_error_tick;

/* The default function to print out name of current function that caused
   an error.  */

/* Called by report_error_function to print out function name. */
void print_error_function(const char *file)
{
  if (last_error_function != current.function_decl)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (current.function_decl == NULL)
	fprintf (stderr, "At top level:\n");
      else
	{
	  const char *name, *iname;

	  declarator_name(current.function_decl->declarator, &name, &iname);
	  fprintf (stderr, "In function `%s%s%s':\n",
		   iname ? iname : "", iname ? "." : "", name);
	}

      last_error_function = current.function_decl;
    }
}

/* Print the current component if it's changed */
void print_current_nesc_instance(void)
{
  static nesc_declaration last_container;

  if (last_container != current.container)
    {
      if (current.container)
	fprintf(stderr, "In %s `%s':\n", 
		language_name(current.container->kind),
		current.container->instance_name);
      else
	fprintf(stderr, "In C file:\n");
      last_container = current.container;
    }
}


/* Prints out, if necessary, the name of the current function
  that caused an error.  Called from all error and warning functions.  */

void report_error_function(const char *file)
{
  struct file_stack *p;

  if (current.lex.input && current.lex.input->next != 0
      && input_file_stack_tick != last_error_tick
      && file == current.lex.input->l.filename)
    {
      fprintf (stderr, "In file included");
      for (p = current.lex.input->next; p; p = p->next)
	{
	  fprintf (stderr, " from %s:%lu", p->l.filename, p->l.lineno);
	  if (p->next)
	    fprintf (stderr, ",\n                ");
	}
      fprintf (stderr, ":\n");
      last_error_tick = input_file_stack_tick;
    }
  print_current_nesc_instance();

  print_error_function(file);
}


static void pfile_and_line(FILE *f, location l)
{
  if (l->container)
    fprintf(f, "%s(%s):%lu: ", l->filename, l->container->instance_name, l->lineno);
  else if (l->lineno)
    fprintf(f, "%s:%lu: ", l->filename, l->lineno);
  else
    fprintf(f, "%s: ", l->filename);
}

/* Report error msg at l */
void verror_with_location(location l, const char *format, va_list args)
{
  count_error(FALSE);
  report_error_function(l->filename);
  pfile_and_line(stderr, l);
  vfprintf(stderr, format, args);
  putc('\n', stderr);
}

/* Report error msg at decl */
void verror_with_decl(declaration d, const char *format, va_list args)
{
  verror_with_location(d->location, format, args);
}

/* Report error msg at current filename, lineno */
void verror(const char *format, va_list args)
{
  if (current.lex.input)
    verror_with_location(&current.lex.input->l, format, args);
  else if (error_location)
    verror_with_location(error_location, format, args);
  else
    {
      count_error(FALSE);
      fprintf(stderr, "%s: ", progname);
      vfprintf(stderr, format, args);
      putc('\n', stderr); 
   }
}

/* Report error msg at current filename, lineno */
void error(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  verror(format, args);
  va_end(args);
}

/* Report error msg at decl */
void error_with_decl(declaration d, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  verror_with_decl(d, format, args);
  va_end(args);
}

/* Report error msg at l */
void error_with_location(location l, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  verror_with_location(l, format, args);
  va_end(args);
}

/* Report a fatal error at the current line number.  */
void vfatal(const char *format, va_list args)
{
  verror(format, args);
  exit(FATAL_EXIT_CODE);
}

void fatal(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vfatal(format, args);
  va_end(args);
}

/* Report warning msg at l */
void vwarning_with_location(location l, const char *format, va_list args)
{
  if (count_error(TRUE))
    {
      report_error_function(l->filename);
      pfile_and_line(stderr, l);
      fprintf(stderr, "warning: ");
      vfprintf(stderr, format, args);
      putc('\n', stderr);
    }
}

/* Report warning msg at decl */
void vwarning_with_decl(declaration d, const char *format, va_list args)
{
  vwarning_with_location(d->location, format, args);
}

/* Report warning msg at current filename, lineno */
void vwarning(const char *format, va_list args)
{
  if (current.lex.input)
    vwarning_with_location(&current.lex.input->l, format, args);
  else if (error_location)
    vwarning_with_location(error_location, format, args);
  else if (count_error(TRUE))
    {
      fprintf(stderr, "%s: warning: ", progname);
      vfprintf(stderr, format, args);
      putc('\n', stderr); 
   }
}

/* Report warning msg at current filename, lineno */
void warning(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vwarning(format, args);
  va_end(args);
}


/* Report warning msg at decl */
void warning_with_decl(declaration d, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vwarning_with_decl(d, format, args);
  va_end(args);
}

/* Report warning msg at l */
void warning_with_location(location l, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vwarning_with_location(l, format, args);
  va_end(args);
}

/* Report warning msg at current filename, lineno */
void warning_or_error(bool iswarning, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (iswarning)
    vwarning(format, args);
  else
    verror(format, args);
  va_end(args);
}


/* Report warning msg at decl */
void warning_or_error_with_decl(bool iswarning, declaration d,
				const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (iswarning)
    vwarning_with_decl(d, format, args);
  else
    verror_with_decl(d, format, args);
  va_end(args);
}

/* Report warning msg at l */
void warning_or_error_with_location(bool iswarning, location l,
				    const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (iswarning)
    vwarning_with_location(l, format, args);
  else
    verror_with_location(l, format, args);
  va_end(args);
}

/* Report pedantic warning or error msg at current filename, lineno */
void pedwarn(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (flag_pedantic_errors)
    verror(format, args);
  else
    vwarning(format, args);
  va_end(args);
}

/* Report pedantic warning or error msg at d */
void pedwarn_with_decl(declaration d, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (flag_pedantic_errors)
    verror_with_decl(d, format, args);
  else
    vwarning_with_decl(d, format, args);
  va_end(args);
}

/* Report pedantic warning or error msg at l */
void pedwarn_with_location(location l, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  if (flag_pedantic_errors)
    verror_with_location(l, format, args);
  else
    vwarning_with_location(l, format, args);
  va_end(args);
}

