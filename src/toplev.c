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

/* Table of language-independent -f and -W options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static struct { char c; char *string; int *variable; int on_value;} fW_options[] =
{
  { 'f', "syntax-only", &flag_syntax_only, 1 },
  { 'f', "parse-only", &flag_parse_only, 1 },
  { 'f', "pack-struct", &flag_pack_struct, 1 },
  { 'f', "allow-single-precision", &flag_allow_single_precision, 1 },
  { 'f', "signed-char", &flag_signed_char, 1 },
  { 'f', "unsigned-char", &flag_signed_char, 0 },
  { 'f', "signed-bitfields", &flag_signed_bitfields, 1 },
  { 'f', "unsigned-bitfields", &flag_signed_bitfields, 0 },
  { 'f', "short-enums", &flag_short_enums, 1 },
  { 'f', "cond-mismatch", &flag_cond_mismatch, 1 },
  { 'f', "asm", &flag_no_asm, 0 },

  { 'W', "unused", &warn_unused, 1 },
  { 'W', "error", &warnings_are_errors, 1 },
  { 'W', "shadow", &warn_shadow, 1 },
  { 'W', "switch", &warn_switch, 1 },
  { 'W', "aggregate-return", &warn_aggregate_return, 1 },
  { 'W', "cast-align", &warn_cast_align, 1 },
  { 'W', "uninitialized", &warn_uninitialized, 1 },
  { 'W', "inline", &warn_inline, 1 },
  { 'W', "error-implicit-function-declaration", &mesg_implicit_function_declaration, 2 },
  { 'W', "implicit-function-declaration", &mesg_implicit_function_declaration, 1 },
  { 'W', "implicit-int", &warn_implicit_int, 1 },
  { 'W', "write-strings", &warn_write_strings, 1 },
  { 'W', "cast-qual", &warn_cast_qual, 1 },
  { 'W', "bad-function-cast", &warn_bad_function_cast, 1 },
  { 'W', "pointer-arith", &warn_pointer_arith, 1 },
  { 'W', "strict-prototypes", &warn_strict_prototypes, 1 },
  { 'W', "missing-prototypes", &warn_missing_prototypes, 1 },
  { 'W', "missing-declarations", &warn_missing_declarations, 1 },
  { 'W', "redundant-decls", &warn_redundant_decls, 1 },
  { 'W', "nested-externs", &warn_nested_externs, 1 },
  { 'W', "traditional", &warn_traditional, 1 },
  { 'W', "format", &warn_format, 1 },
  { 'W', "char-subscripts", &warn_char_subscripts, 1 },
  { 'W', "conversion", &warn_conversion, 1 },
  { 'W', "parentheses", &warn_parentheses, 1 },
  { 'W', "return-type", &warn_return_type, 1 },
  { 'W', "missing-braces", &warn_missing_braces, 1 },
  { 'W', "main", &warn_main, 1 },
  { 'W', "sign-compare", &warn_sign_compare, 1 },
  { 'W', "multichar", &warn_multichar, 1 },
  { 'W', "comment", &warn_comments, 1 },
  { 'W', "comments", &warn_comments, 1 },
  { 'W', "trigraphs", &warn_trigraphs, 1 },
  { 'W', "unused-macros", &warn_unused_macros, 1 },
  { 'W', "endif-labels", &warn_endif_labels, 1 },
  { 'W', "system-headers", &warn_system_headers, 1 },
  { 'W', "undef", &warn_undef, 1 },
  { 'W', "missing-include-dirs", &warn_missing_include_dirs, 1 },
  { 'W', "multichar", &warn_multichar, 1 }
};

/* Options with arguments */
#define OPTS_WITH_ARGS "DUAIo"

const char *opts_with_args[] = 
{
  "include",
  "imacros",
  "idirafter",
  "iprefix",
  "iwithprefix",
  "iwithprefixbefore",
  "isystem",
  "iquote"
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
static bool c_option(char *p)
{
  if (!strcmp (p, "-v"))
    flag_verbose = 1;
  else if (!strcmp (p, "-traditional"))
    flag_traditional = 1;
  else if (!strcmp (p, "-trigraphs"))
    flag_trigraphs = 1;
  else if (!strcmp (p, "-ansi"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-pedantic"))
    pedantic = 1;
  else if (!strcmp (p, "-pedantic-errors"))
    flag_pedantic_errors = pedantic = 1;
  else if (!strcmp (p, "-nostdinc"))
    flag_nostdinc = 1;
  else if (!strcmp (p, "-undef"))
    flag_undef = 1;
  else if (!strcmp (p, "-fdollars-in-identifiers"))
    fprintf(stderr, "nesC does not support $ in identifiers");
  else if (!strcmp (p, "-fno-dollars-in-identifiers"))
    dollars_in_ident = 0;
  else if (!strcmp (p, "-w"))
    inhibit_warnings = 1;
  else if (!strcmp (p, "-Wimplicit"))
    {
      warn_implicit_int = 1;
      if (mesg_implicit_function_declaration != 2)
        mesg_implicit_function_declaration = 1;
    }
  else if (!strcmp (p, "-Wno-implicit"))
    warn_implicit_int = 0, mesg_implicit_function_declaration = 0;
  else if (!strcmp (p, "-W"))
    {
      extra_warnings = 1;
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = 2;
    }
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

      warn_trigraphs = 1;
      warn_comments = 1;
    }
  else if (!strcmp (p, "-H"))
    {
      print_include_names = 1;
    }
  else
    return FALSE;

  return TRUE;
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
  int i;
  char *filename = 0;
  char *targetfile = 0;
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
  if (waitforgdb) 
    {
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

  flag_signed_char = 2; /* Detect if user specifies a value */

  for (i = 1; i < argc; )
    {
      int j;

      if (c_option(argv[i]) || nesc_option(argv[i]))
	i++;
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  char *str = argv[i++] + 1;
	  char *arg = NULL;

	  if (strchr(OPTS_WITH_ARGS, str[0]))
	    {
	      if (!str[1])
		if (i < argc)
		  arg = argv[i++];
		else
		  {
		    str = "";
		    error("argument to `-%c' is missing", str[0]);
		  }
	      else
		arg = str + 1;
	    }

	  for (j = 0; j < sizeof opts_with_args / sizeof *opts_with_args; j++)
	    if (!strcmp(str, opts_with_args[j]))
	      {
		if (i < argc)
		  arg = argv[i++];
		else
		  {
		    str = "";
		    error("argument to `-%s' is missing", str);
		  }
		break;
	      }

	  if (str[0] == 'o')
	    targetfile = arg;
	  else if (str[0] == 'I')
	    add_nesc_dir(arg, CHAIN_BRACKET);
	  else if (str[0] == 'D' || str[0] == 'U' || str[0] == 'A')
	    save_cpp_option(str, arg);
	  else if (str[0] == 'f' || str[0] == 'W')
	    {
	      char kind = str[0];
	      char *p = &str[1];

	      /* Some kind of -f or -W option.
		 p's value is the option sans -f/W.
		 Search for it in the table of options.  */
	      for (j = 0;
		   j < sizeof (fW_options) / sizeof (fW_options[0]);
		   j++)
		{
		  if (kind == fW_options[j].c &&
		      !strcmp (p, fW_options[j].string))
		    {
		      *fW_options[j].variable = fW_options[j].on_value;
		      break;
		    }
		  if (kind == fW_options[j].c &&
		      p[0] == 'n' && p[1] == 'o' && p[2] == '-' &&
		      !strcmp (p+3, fW_options[j].string))
		    {
		      *fW_options[j].variable = ! fW_options[j].on_value;
		      break;
		    }
		}
	    }
	  else if (!strcmp(str, "include"))
	    add_nesc_include(arg, TRUE);
	}
      else
	filename = argv[i++];
    }

  /* Pass options on to the target too (this a bit hacky, but fine so far) */
  if (target->handle_option)
    for (i = 1; i < argc; i++)
      if (argv[i][0] == '-' && argv[i][1] != 0)
	target->handle_option(argv[i]);

  if (target->preinit)
    target->preinit();

  if (flag_signed_char == 2) /* not set by user */
    flag_signed_char = target->char_signed;

  if (filename)
    nesc_compile (filename, targetfile);
  else
    {
      fprintf(stderr, "usage: %s [options] <filename>\n", argv[0]);
      exit(FATAL_EXIT_CODE);
    }
    
  if (errorcount)
    exit (FATAL_EXIT_CODE);
  else
    exit (SUCCESS_EXIT_CODE);
  return 0;
}
