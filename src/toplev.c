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

#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

#include "parser.h"
#include "c-parse.h"
#include "unparse.h"
#include "semantics.h"
#include "machine.h"
#include "nesc-main.h"
#include "nesc-paths.h"
#include "nesc-cpp.h"
#include "nesc-msg.h"
#include "nesc-doc.h"

#if HAVE_POLL
#include <sys/poll.h>
#endif

/* Table of language-independent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static struct { char *string; int *variable; int on_value;} f_options[] =
{
  {"volatile", &flag_volatile, 1},
  {"volatile-global", &flag_volatile_global, 1},
  {"syntax-only", &flag_syntax_only, 1},
  {"parse-only", &flag_parse_only, 1},
  {"pack-struct", &flag_pack_struct, 1}
};

/* Table of language-specific options.  */

static char *lang_options[] =
{
  "-ansi",
  "-fallow-single-precision",

  "-fsigned-bitfields",
  "-funsigned-bitfields",
  "-fno-signed-bitfields",
  "-fno-unsigned-bitfields",
  "-fsigned-char",
  "-funsigned-char",
  "-fno-signed-char",
  "-fno-unsigned-char",

  "-ftraditional",
  "-traditional",
  "-fnotraditional",
  "-fno-traditional",

  "-fasm",
  "-fno-asm",
  "-fbuiltin",
  "-fno-builtin",
  "-fhosted",
  "-fno-hosted",
  "-ffreestanding",
  "-fno-freestanding",
  "-fcond-mismatch",
  "-fno-cond-mismatch",
  "-fdollars-in-identifiers",
  "-fno-dollars-in-identifiers",
  "-fident",
  "-fno-ident",
  "-fshort-double",
  "-fno-short-double",
  "-fshort-enums",
  "-fno-short-enums",

  "-Wall",
  "-Wbad-function-cast",
  "-Wno-bad-function-cast",
  "-Wcast-qual",
  "-Wno-cast-qual",
  "-Wchar-subscripts",
  "-Wno-char-subscripts",
  "-Wcomment",
  "-Wno-comment",
  "-Wcomments",
  "-Wno-comments",
  "-Wconversion",
  "-Wno-conversion",
  "-Wformat",
  "-Wno-format",
  "-Wimport",
  "-Wno-import",
  "-Wimplicit-function-declaration",
  "-Wno-implicit-function-declaration",
  "-Werror-implicit-function-declaration",
  "-Wimplicit-int",
  "-Wno-implicit-int",
  "-Wimplicit",
  "-Wno-implicit",
  "-Wmain",
  "-Wno-main",
  "-Wmissing-braces",
  "-Wno-missing-braces",
  "-Wmissing-declarations",
  "-Wno-missing-declarations",
  "-Wmissing-prototypes",
  "-Wno-missing-prototypes",
  "-Wnested-externs",
  "-Wno-nested-externs",
  "-Wparentheses",
  "-Wno-parentheses",
  "-Wpointer-arith",
  "-Wno-pointer-arith",
  "-Wredundant-decls",
  "-Wno-redundant-decls",
  "-Wsign-compare",
  "-Wno-sign-compare",
  "-Wstrict-prototypes",
  "-Wno-strict-prototypes",
  "-Wtraditional",
  "-Wno-traditional",
  "-Wtrigraphs",
  "-Wno-trigraphs",
  "-Wundef",
  "-Wno-undef",
  "-Wwrite-strings",
  "-Wno-write-strings",
  0
};


/* Likewise for -W.  */

static struct { char *string; int *variable; int on_value;} W_options[] =
{
  {"unused", &warn_unused, 1},
  {"error", &warnings_are_errors, 1},
  {"shadow", &warn_shadow, 1},
  {"switch", &warn_switch, 1},
  {"aggregate-return", &warn_aggregate_return, 1},
  {"cast-align", &warn_cast_align, 1},
  {"uninitialized", &warn_uninitialized, 1},
  {"inline", &warn_inline, 1}
};

#ifdef SIGPIPE
/* Handler for SIGPIPE.  */
static void pipe_closed (int signo)
{
  fatal("output pipe has been closed");
}
#endif

/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */


/* Decode the string P as a language-specific option for C. */
static void c_decode_option(char *p)
{
  if (!strcmp (p, "-ftraditional") || !strcmp (p, "-traditional"))
    {
      flag_traditional = 1;
    }
  else if (!strcmp (p, "-fallow-single-precision"))
    flag_allow_single_precision = 1;
  else if (!strcmp (p, "-fnotraditional") || !strcmp (p, "-fno-traditional"))
    {
      flag_traditional = 0;
    }
  else if (!strcmp (p, "-fdollars-in-identifiers"))
    {
      fprintf(stderr, "nesC does not support $ in identifiers");
    }
  else if (!strcmp (p, "-fno-dollars-in-identifiers"))
    dollars_in_ident = 0;
  else if (!strcmp (p, "-fsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-funsigned-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-signed-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-unsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-fsigned-bitfields")
	   || !strcmp (p, "-fno-unsigned-bitfields"))
    {
      flag_signed_bitfields = 1;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-funsigned-bitfields")
	   || !strcmp (p, "-fno-signed-bitfields"))
    {
      flag_signed_bitfields = 0;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-fshort-enums"))
    flag_short_enums = 1;
  else if (!strcmp (p, "-fno-short-enums"))
    flag_short_enums = 0;
  else if (!strcmp (p, "-fcond-mismatch"))
    flag_cond_mismatch = 1;
  else if (!strcmp (p, "-fno-cond-mismatch"))
    flag_cond_mismatch = 0;
  else if (!strcmp (p, "-fasm"))
    flag_no_asm = 0;
  else if (!strcmp (p, "-fno-asm"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-ansi"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-Werror-implicit-function-declaration"))
    mesg_implicit_function_declaration = 2;
  else if (!strcmp (p, "-Wimplicit-function-declaration"))
    mesg_implicit_function_declaration = 1;
  else if (!strcmp (p, "-Wno-implicit-function-declaration"))
    mesg_implicit_function_declaration = 0;
  else if (!strcmp (p, "-Wimplicit-int"))
    warn_implicit_int = 1;
  else if (!strcmp (p, "-Wno-implicit-int"))
    warn_implicit_int = 0;
  else if (!strcmp (p, "-Wimplicit"))
    {
      warn_implicit_int = 1;
      if (mesg_implicit_function_declaration != 2)
        mesg_implicit_function_declaration = 1;
    }
  else if (!strcmp (p, "-Wno-implicit"))
    warn_implicit_int = 0, mesg_implicit_function_declaration = 0;
  else if (!strcmp (p, "-Wwrite-strings"))
    warn_write_strings = 1;
  else if (!strcmp (p, "-Wno-write-strings"))
    warn_write_strings = 0;
  else if (!strcmp (p, "-Wcast-qual"))
    warn_cast_qual = 1;
  else if (!strcmp (p, "-Wno-cast-qual"))
    warn_cast_qual = 0;
  else if (!strcmp (p, "-Wbad-function-cast"))
    warn_bad_function_cast = 1;
  else if (!strcmp (p, "-Wno-bad-function-cast"))
    warn_bad_function_cast = 0;
  else if (!strcmp (p, "-Wpointer-arith"))
    warn_pointer_arith = 1;
  else if (!strcmp (p, "-Wno-pointer-arith"))
    warn_pointer_arith = 0;
  else if (!strcmp (p, "-Wstrict-prototypes"))
    warn_strict_prototypes = 1;
  else if (!strcmp (p, "-Wno-strict-prototypes"))
    warn_strict_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-prototypes"))
    warn_missing_prototypes = 1;
  else if (!strcmp (p, "-Wno-missing-prototypes"))
    warn_missing_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-declarations"))
    warn_missing_declarations = 1;
  else if (!strcmp (p, "-Wno-missing-declarations"))
    warn_missing_declarations = 0;
  else if (!strcmp (p, "-Wredundant-decls"))
    warn_redundant_decls = 1;
  else if (!strcmp (p, "-Wno-redundant-decls"))
    warn_redundant_decls = 0;
  else if (!strcmp (p, "-Wnested-externs"))
    warn_nested_externs = 1;
  else if (!strcmp (p, "-Wno-nested-externs"))
    warn_nested_externs = 0;
  else if (!strcmp (p, "-Wtraditional"))
    warn_traditional = 1;
  else if (!strcmp (p, "-Wno-traditional"))
    warn_traditional = 0;
  else if (!strcmp (p, "-Wformat"))
    warn_format = 1;
  else if (!strcmp (p, "-Wno-format"))
    warn_format = 0;
  else if (!strcmp (p, "-Wchar-subscripts"))
    warn_char_subscripts = 1;
  else if (!strcmp (p, "-Wno-char-subscripts"))
    warn_char_subscripts = 0;
  else if (!strcmp (p, "-Wconversion"))
    warn_conversion = 1;
  else if (!strcmp (p, "-Wno-conversion"))
    warn_conversion = 0;
  else if (!strcmp (p, "-Wparentheses"))
    warn_parentheses = 1;
  else if (!strcmp (p, "-Wno-parentheses"))
    warn_parentheses = 0;
  else if (!strcmp (p, "-Wreturn-type"))
    warn_return_type = 1;
  else if (!strcmp (p, "-Wno-return-type"))
    warn_return_type = 0;
  else if (!strcmp (p, "-Wmissing-braces"))
    warn_missing_braces = 1;
  else if (!strcmp (p, "-Wno-missing-braces"))
    warn_missing_braces = 0;
  else if (!strcmp (p, "-Wmain"))
    warn_main = 1;
  else if (!strcmp (p, "-Wno-main"))
    warn_main = 0;
  else if (!strcmp (p, "-Wsign-compare"))
    warn_sign_compare = 1;
  else if (!strcmp (p, "-Wno-sign-compare"))
    warn_sign_compare = 0;
  else if (!strcmp (p, "-Wmultichar"))
    warn_multichar = 1;
  else if (!strcmp (p, "-Wno-multichar"))
    warn_multichar = 0;
  else if (!strcmp (p, "-Wall"))
    {
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = 2;
      warn_implicit_int = 1;
      mesg_implicit_function_declaration = 1;
      warn_return_type = 1;
      warn_unused = 1;
      warn_switch = 1;
      warn_format = 1;
      warn_char_subscripts = 1;
      warn_parentheses = 1;
      warn_missing_braces = 1;
      /* We set this to 2 here, but 1 in -Wmain, so -ffreestanding can turn
	 it off only if it's not explicit.  */
      warn_main = 2;
    }
}

static void rcc_aborting(int s)
{
  location where;

  signal(SIGABRT, 0);
  fprintf(stderr, "nesC: Internal error. Please send a bug report to the nesC bug mailing list\nat nescc-bugs@lists.sourceforge.net\n");
  where = current_location();
  if (where != dummy_location)
    fprintf(stderr, "Current location (guess): %s:%lu\n", where->filename, where->lineno);
  if (getenv("RCCDEBUG"))
    abort();
  else
    exit(FATAL_EXIT_CODE);
}

/* Entry point of cc1/c++.  Decode command args, then call compile_file.
   Exit code is 35 if can't open files, 34 if fatal error,
   33 if had nonfatal errors, else success.  */

static void outofmemory(void)
{
  fprintf(stderr, "Out of memory - exiting\n");
  exit(FATAL_EXIT_CODE);
}

int region_main(int argc, char **argv) deletes
{
  register int i;
  char *filename = 0;
  char *targetfile = 0;
  int version_flag = 0;
  char *p;

#if HAVE_POLL
  char* waitforgdb;
  /*
   * Check for an environment variable NCCGDB, and if set, block
   * calling poll(). When gdb attaches, it sends us a signal which
   * causes poll to return with EINTR, and we continue on our merry
   * way.
   */
  waitforgdb = getenv("NCCGDB");
  if (waitforgdb) {
    fprintf(stderr, "ncc pid %d waiting for gdb attach\n", getpid());
    poll(0, 0, -1); // should return with EINTR
  }
#endif
  
  signal(SIGABRT, rcc_aborting);
  signal(SIGSEGV, rcc_aborting);
#ifdef SIGBUS
  signal(SIGBUS, rcc_aborting);
#endif
  set_nomem_handler(outofmemory);

  init_nesc_paths_start(newregion());

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/'
#ifdef DIR_SEPARATOR
	 && p[-1] != DIR_SEPARATOR
#endif
	 )
    --p;
  progname = p;

#ifdef SIGPIPE
  signal (SIGPIPE, pipe_closed);
#endif

  for (i = 1; i < argc; i++)
    {
      int j;

      /* If this is a language-specific option,
	 decode it in a language-specific way.  */
      for (j = 0; lang_options[j] != 0; j++)
	if (!strncmp (argv[i], lang_options[j],
		      strlen (lang_options[j])))
	  break;

      if (lang_options[j] != 0)
	/* If the option is valid for *some* language,
	   treat it as valid even if this language doesn't understand it.  */
	c_decode_option(argv[i]);
      else if (nesc_option(argv[i]))
	;
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  register char *str = argv[i] + 1;

	  if (str[0] == 'Y')
	    str++;

	  if (str[0] == 'I')
	    {
	      if (str[1])
		add_nesc_dir(str + 1);
	      else if (i + 1 < argc)
		add_nesc_dir(argv[++i]);
	      else
		error("argument to `-I' is missing");
	    }
	  else if (str[0] == 'D' || str[0] == 'm')
	    save_option(argv[i]);
	  else if (!strcmp (str, "dumpbase"))
	    i++;
	  else if (str[0] == 'f')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -f option.
		 P's value is the option sans `-f'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (f_options) / sizeof (f_options[0]);
		   j++)
		{
		  if (!strcmp (p, f_options[j].string))
		    {
		      *f_options[j].variable = f_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, f_options[j].string))
		    {
		      *f_options[j].variable = ! f_options[j].on_value;
		      found = 1;
		    }
		}
	    }
	  else if (!strcmp (str, "v"))
	    {
	      if (!flag_verbose) /* avoid overriding -fnesc-verbose */
		flag_verbose = 1;
	    }
	  else if (!strcmp (str, "pedantic"))
	    pedantic = 1;
	  else if (!strcmp (str, "pedantic-errors"))
	    flag_pedantic_errors = pedantic = 1;
	  else if (!strcmp (str, "quiet"))
	    quiet_flag = 1;
	  else if (!strcmp (str, "version"))
	    version_flag = 1;
	  else if (!strcmp (str, "w"))
	    inhibit_warnings = 1;
	  else if (!strcmp (str, "W"))
	    {
	      extra_warnings = 1;
	      /* We save the value of warn_uninitialized, since if they put
		 -Wuninitialized on the command line, we need to generate a
		 warning about not using it without also specifying -O.  */
	      if (warn_uninitialized != 1)
		warn_uninitialized = 2;
	    }
	  else if (str[0] == 'W')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -W option.
		 P's value is the option sans `-W'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (W_options) / sizeof (W_options[0]);
		   j++)
		{
		  if (!strcmp (p, W_options[j].string))
		    {
		      *W_options[j].variable = W_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, W_options[j].string))
		    {
		      *W_options[j].variable = ! W_options[j].on_value;
		      found = 1;
		    }
		}

	      if (found)
		;
	      else if (!strncmp (p, "id-clash-", 9))
		{
		  char *endp = p + 9;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto id_clash_lose;
			}
		    }
		  warn_id_clash = 1;
		  id_clash_len = atoi (str + 10);
		id_clash_lose: ;
		}
	      else if (!strncmp (p, "larger-than-", 12))
		{
		  char *endp = p + 12;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto larger_than_lose;
			}
		    }
		  warn_larger_than = 1;
		  larger_than_size = atoi (str + 13);
		larger_than_lose: ;
		}
	    }
	  else if (!strcmp (str, "o") && i + 1 < argc)
	    {
	      i++;
	      targetfile = argv[i];
	    }
	  else if (str[0] == 'G')
	    {
	      if (str[1] == '\0')
		i++;
	    }
	  else if (!strncmp (str, "aux-info", 8))
	    {
	      if (str[8] == '\0')
		i++;
	    }
	}
      else if (argv[i][0] == '+')
	;
      else
	filename = argv[i];
    }

  /* Pass options on to the target too */
  if (target->handle_option)
    for (i = 1; i < argc; i++)
      if (argv[i][0] == '-' && argv[i][1] != 0)
	target->handle_option(argv[i]);

  if (filename)
    nesc_compile (filename, targetfile);
  else
    {
      fprintf(stderr, "usage: %s [options] <filename>\n", argv[0]);
      exit(FATAL_EXIT_CODE);
    }
    
  if (errorcount)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);
  return 0;
}
