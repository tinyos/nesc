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
#include "nesc-paths.h"

static region opt_region;

struct option {
  struct option *next;
  const char *opt;
};

static struct option *saved_options;
static int saved_options_count;

void save_option(const char *option)
{
  struct option *newopt;

  if (!opt_region)
    opt_region = newregion();

  newopt = ralloc(opt_region, struct option);
  newopt->opt = option;
  newopt->next = saved_options;
  saved_options = newopt;
  saved_options_count++;
}

static char tmpfile1[] = "/tmp/nesccpp2XXXXXX";
static char tmpfile2[] = "/tmp/nesccpp1XXXXXX";

static char *cpp_macros, *cpp_dest;
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
  unlink(tmpfile1);
  unlink(tmpfile2);
}

void preprocess_init(void)
{
  atexit(preprocess_cleanup);

  mktempfile(tmpfile1);
  mktempfile(tmpfile2);

  cpp_macros = tmpfile1;
  cpp_dest = tmpfile2;
}

FILE *preprocess(const char *filename)
{
  int cpp_pid, cpp_stat, res;

  if ((cpp_pid = fork()) == 0)
    {
      char **argv;
      int nargs = 10 + path_argv_count + saved_options_count, arg = 0, i;
      struct option *saved;

      argv = alloca(nargs * sizeof *argv);
      argv[arg++] = "avr-gcc";

      rarraycopy(argv + arg, path_argv, path_argv_count, char *);
      arg += path_argv_count;

      /* The saved options are reversed */
      for (saved = saved_options, i = saved_options_count; saved;
	   saved = saved->next)
	argv[arg + --i] = (char *)saved->opt;
      arg += saved_options_count;

      argv[arg++] = "-E";
      argv[arg++] = "-dD";
      argv[arg++] = "-imacros";
      argv[arg++] = cpp_macros;
      argv[arg++] = "-o";
      argv[arg++] = cpp_dest;
      argv[arg++] = (char *)filename;
      argv[arg++] = NULL;
      assert(arg <= nargs);

#if 0
      for (i = 0; i < arg - 1; i++)
	fprintf(stderr, "%s ", argv[i]);
      fprintf(stderr, "\n");
#endif

      execvp("avr-gcc", argv);
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

      macros_file = fopen(cpp_macros, "w");
      if (!macros_file)
	error("failed to create temporary file");

      return output;
    }
  else
    return NULL;
}

void handle_directive(const char *directive, const char *args)
{
  if (strcmp(directive, "define"))
    return;

  if (strncmp(args, "__STDC__ ", 9) == 0 ||
      strncmp(args, "__STDC_HOSTED__ ", 16) == 0)
    return;

  if (macros_file)
    fprintf(macros_file, "#define %s\n", args);
}

void preprocess_file_end(void)
{
  if (macros_file)
    {
      fclose(macros_file);
      macros_file = NULL;
    }
}

