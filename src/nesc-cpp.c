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

#include "parser.h"
#include "nesc-cpp.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include "nesc-paths.h"
#include "machine.h"
#include "flags.h"
#include "semantics.h"
#include "c-parse.h"

static region opt_region;

struct cpp_option {
  struct cpp_option *next;
  const char *opt;
};

static struct cpp_option *saved_options;
static int saved_options_count;

void save_option(const char *option)
{
  struct cpp_option *newopt;

  if (!opt_region)
    opt_region = newregion();

  newopt = ralloc(opt_region, struct cpp_option);
  newopt->opt = option;
  newopt->next = saved_options;
  saved_options = newopt;
  saved_options_count++;
}

static char kwd_macros[] = "/tmp/nesccppkXXXXXX";
static char cpp_macros[] = "/tmp/nesccppmXXXXXX";

static char *nesc_keywords[] = {
#define K(name, token, rid) #name,
#include "nesc-keywords.h"
NULL
};

static FILE *macros_file;

static void mktempfile(char *name)
{
  int fd = mkstemp(name);

  if (fd < 0)
    {
      perror("couldn't create temporary file");
      exit(2);
    }

  close(fd);
}

void preprocess_cleanup(void)
{
  unlink(cpp_macros);
  unlink(kwd_macros);
}

void create_nesc_keyword_macros(const char *macro_filename)
{
  FILE *mf = fopen(macro_filename, "w");
  int i;

  if (!mf) 
    {
      fprintf(stderr, "couldn't create temporary file - aborting\n");
      exit(2);
    }

  for (i = 0; nesc_keywords[i]; i++) 
    fprintf(mf, "#define %s __nesc_keyword_%s\n",
	    nesc_keywords[i], nesc_keywords[i]);

  /* this will set the enum value of TOSNODES to the value specified
     during compilation by the flag "-fnesc-tossim-tosnodes = 1000"
     which in this case is 1000. */     
  if (use_tossim)
    fprintf(mf, "#define TOSH_NUM_NODES %s\n",
	    tossim_num_nodes ? tossim_num_nodes : "1000");

  fclose(mf);
}

void preprocess_init(void)
{
  atexit(preprocess_cleanup);

  mktempfile(cpp_macros);
  mktempfile(kwd_macros);

  create_nesc_keyword_macros(kwd_macros);
}

FILE *preprocess(const char *filename, source_language l)
{
  int cpp_pid, cpp_stat, res;
  char *cpp_dest = rstrdup(parse_region, "/tmp/nesccppsXXXXXX");

  current.preprocessed_file = cpp_dest;
  mktempfile(cpp_dest);

  if ((cpp_pid = fork()) == 0)
    {
      char **argv;
      int nargs = 11 + path_argv_count + saved_options_count, arg = 0, i;
      struct cpp_option *saved;
      int destfd = creat(cpp_dest, 0666);
      region filename_region = newregion();

      if (destfd < 0 || dup2(destfd, 1) < 0)
	exit(2);

      close(destfd);

      argv = alloca(nargs * sizeof *argv);
      argv[arg++] = (char *)target->gcc_compiler;

      rarraycopy(argv + arg, path_argv, path_argv_count, char *);
      arg += path_argv_count;

      /* The saved options are reversed */
      for (saved = saved_options, i = saved_options_count; saved;
	   saved = saved->next)
	argv[arg + --i] = (char *)saved->opt;
      arg += saved_options_count;

      argv[arg++] = "-E";
      argv[arg++] = "-C";

      /* For C files, we define keywords away (kwd_macros) and ask cpp
	 to output macros */
      if (l == l_c)
	{
	  argv[arg++] = "-dD";
	  argv[arg++] = "-imacros";
	  argv[arg++] = fix_filename(filename_region, kwd_macros);
	}
      else
	{
	  argv[arg++] = "-x";
	  argv[arg++] = "c";
	}
      argv[arg++] = "-imacros";
      argv[arg++] = fix_filename(filename_region, cpp_macros);
      argv[arg++] = fix_filename(filename_region, filename);
      argv[arg++] = NULL;
      assert(arg <= nargs);

      /* It's really spammy with this on */
      if (flag_verbose >= 2)
	{
	  for (i = 0; i < arg - 1; i++)
	    fprintf(stderr, "%s ", argv[i]);
	  fprintf(stderr, "\n");
	}

      execvp(target->gcc_compiler, argv);
      exit(2);
    }

  for (;;)
    {
      int pid = wait(&cpp_stat);

      if (pid == -1)
	{
	  if (errno == EINTR)
	    continue;
	  else
	    {
	      res = 2;
	      break;
	    }
	}

      if (pid == cpp_pid)
	{
	  if (WIFEXITED(cpp_stat))
	    res = WEXITSTATUS(cpp_stat);
	  else
	    res = 2;
	  break;
	}
    }

  if (res == 0) /* cpp succeeded */
    {
      FILE *output = fopen(cpp_dest, "r");

      /* Save the macros for C */
      /* (note: this only works with a global macros file because we
	 don't reenter the parser when parsing a C file) */
      if (l == l_c)
	{
	  macros_file = fopen(cpp_macros, "w");
	  if (!macros_file)
	    error("failed to create temporary file");
	}

      return output;
    }
  else
    return NULL;
}

void handle_directive(const char *directive, const char *args)
{
  const char *arg2;

  if (!(strcmp(directive, "define") == 0 || strcmp(directive, "undef") == 0))
    return;

  if (strncmp(args, "__STDC__ ", 9) == 0 ||
      strncmp(args, "__STDC_HOSTED__ ", 16) == 0)
    return;

  arg2 = strchr(args, ' ');
  if (arg2 && strncmp(arg2 + 1, "__nesc_keyword_", 15) == 0)
    return;

  if (macros_file)
    fprintf(macros_file, "#%s %s\n", directive, args);
}

void preprocess_file_end(void)
{
  if (macros_file)
    {
      fclose(macros_file);
      macros_file = NULL;
    }
  unlink(current.preprocessed_file);
}

