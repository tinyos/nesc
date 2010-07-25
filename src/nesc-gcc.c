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
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include "nesc-paths.h"
#include "machine.h"
#include "flags.h"
#include "semantics.h"
#include "c-parse.h"

#include "gcc-cpp.h"

/* Some extra options for gcc */

struct extra_option {
  struct extra_option *next;
  const char *opt;
};

static struct extra_option *extra_options;
static int extra_options_count;

void add_gcc_option(const char *option)
{
  struct extra_option *newopt;

  newopt = ralloc(permanent, struct extra_option);
  newopt->opt = option;
  newopt->next = extra_options;
  extra_options = newopt;
  extra_options_count++;
}

static char *mktempfile(region r, const char *name)
{
  char *newname;

#ifdef WIN32
  if (!strncmp(name, "/tmp/", 5))
    {
      char *tmpenv = getenv("TMP");

      if (!tmpenv)
	{
	  fprintf(stderr, "You must define the TMP environment variable to point to a directory\n");
	  fprintf(stderr, "for temporary files.\n");
	  exit(2);
	}
      newname = rstralloc(r, strlen(tmpenv) + strlen(name));
      sprintf(newname, "%s/%s", tmpenv, name + 5);
    }
  else
    newname = rstrdup(r, name);
  if (!mktemp(newname))
    {
      perror("couldn't create temporary file");
      exit(2);
    }

#else
  int fd;

  newname = rstrdup(r, name);
  fd = mkstemp(newname);

  if (fd < 0)
    {
      perror("couldn't create temporary file");
      exit(2);
    }

  close(fd);
#endif

  return newname;
}

#if defined(__CYGWIN__) || defined(WIN32)
#include <process.h>
#ifndef WIN32
#include <sys/wait.h>
#endif

static bool safe_dup(int from, int to, int save)
{
  if (dup2(to, save) < 0)
    return FALSE;

  return dup2(from, to) >= 0;
}

static void dup_restore(int to, int save)
{
  if (dup2(save, to) < 0)
    {
      perror("internal problem - canot restore file descriptor");
      exit(2);
    }
}

static bool 
exec_gcc(char *gcc_output_template, bool mkotmp, char **gcc_output_file,
	 char *gcc_error_template, bool mketmp, char **gcc_error_file, 
	 int nargs, void (*setargs)(void *data, const char **argv), void *data)
{
  int gcc_stat, res, outputfd, errorfd;
  const char **argv;
  static int tmpfd1 = -1, tmpfd2 = -1;
  char *outputf, *errorf;

  argv = alloca((nargs + 2) * sizeof *argv);
  argv[0] = target_compiler;
  setargs(data, argv + 1);

  /* It's really spammy with this on */
  if (flag_verbose >= 2)
    {
      int i;

      for (i = 0; argv[i]; i++)
	fprintf(stderr, "%s ", argv[i]);
      fprintf(stderr, "\n");
    }

  if (tmpfd1 < 0 || tmpfd2 < 0)
    {
      tmpfd1 = open(DEVNULL, O_RDONLY);
      tmpfd2 = open(DEVNULL, O_RDONLY);

      if (tmpfd1 < 0 || tmpfd2 < 0)
	{
	  fprintf(stderr, "Internal error (can't open " DEVNULL "!?)\n");
	  exit(2);
	}
    }

  if (mkotmp)
    outputf = mktempfile(permanent, gcc_output_template);
  else
    outputf = gcc_output_template;
  *gcc_output_file = outputf;

  if (mketmp)
    errorf = mktempfile(permanent, gcc_error_template);
  else
    errorf = gcc_error_template;
  *gcc_error_file = errorf;

  outputfd = creat(outputf, 0666);
  errorfd = creat(errorf, 0666);
  
  if (outputfd < 0 || errorfd < 0)
    {
      if (outputfd >= 0)
	close(outputfd);
      if (errorfd >= 0)
	close(errorfd);

      return FALSE;
    }

  if (!safe_dup(outputfd, 1, tmpfd1))
    return FALSE;

  if (!safe_dup(errorfd, 2, tmpfd2))
    {
      dup_restore(1, tmpfd1);
      return FALSE;
    }

  close(outputfd);
  close(errorfd);

  gcc_stat = spawnvp(_P_WAIT, target_compiler, argv);
#ifdef WIN32
  res = gcc_stat == 0 ? 0 : 2;
#else
  if (WIFEXITED(gcc_stat))
    res = WEXITSTATUS(gcc_stat);
  else
    res = 2;
#endif
  if (res != 0) 
    fprintf(stderr, "could not execute %s\n", target_compiler);

  dup_restore(1, tmpfd1);
  dup_restore(2, tmpfd2);

  return res == 0;
}
#else
#include <sys/wait.h>

static bool 
exec_gcc(char *gcc_output_template, bool mkotmp, char **gcc_output_file,
	 char *gcc_error_template, bool mketmp, char **gcc_error_file, 
	 int nargs, void (*setargs)(void *data, const char **argv), void *data)
{
  int gcc_pid, gcc_stat, res;
  char *outputf, *errorf;

  if (mkotmp)
    outputf = mktempfile(permanent, gcc_output_template);
  else
    outputf = gcc_output_template;
  *gcc_output_file = outputf;

  if (mketmp)
    errorf = mktempfile(permanent, gcc_error_template);
  else
    errorf = gcc_error_template;
  *gcc_error_file = errorf;

  if ((gcc_pid = fork()) == 0)
    {
      const char **argv;
      int outputfd = creat(outputf, 0666);
      int errorfd = creat(errorf, 0666);

      argv = alloca((nargs + 2) * sizeof *argv);
      argv[0] = target_compiler;
      setargs(data, argv + 1);

      /* It's really spammy with this on */
      if (flag_verbose >= 2)
	{
	  int i;

	  for (i = 0; argv[i]; i++)
	    fprintf(stderr, "%s ", argv[i]);
	  fprintf(stderr, "\n");
	}

      if (outputfd < 0 || dup2(outputfd, 1) < 0 ||
	  errorfd < 0 || dup2(errorfd, 2) < 0)
	exit(2);

      close(outputfd);
      close(errorfd);

      execvp(target_compiler, (char **)argv);
      fprintf(stderr, "could not execute %s: %s\n", target_compiler, strerror(errno));
      exit(2);
    }

  for (;;)
    {
      int pid = wait(&gcc_stat);

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

      if (pid == gcc_pid)
	{
	  if (WIFEXITED(gcc_stat))
	    res = WEXITSTATUS(gcc_stat);
	  else
	    res = 2;
	  break;
	}
    }

  return res == 0;
}
#endif

#define LINELEN 160

/* Make a copy of path with leading+trailing whitespace removed */
static char *sanitize_path(region r, const char *path)
{
  char *pcopy; 
  int l;

  while (ISSPACE(*path))
    path++;
  pcopy = rstrdup(r, path);
  l = strlen(pcopy);
  while (l > 0 && ISSPACE(pcopy[l - 1]))
    pcopy[--l] = '\0';

  return pcopy;
}

static void gcc_preprocess_init_setargs(void *data, const char **argv)
{
  int opt = 0, i;
  struct extra_option *extras;

  argv[opt++] = "-v";
  argv[opt++] = "-x";
  argv[opt++] = "c";
  if (flag_mingw_gcc)
    argv[opt++] = "nul:";
  else
    argv[opt++] = "/dev/null";
  argv[opt++] = "-E";
  argv[opt++] = "-dM";
  if (flag_nostdinc)
    argv[opt++] = "-nostdinc";

  /* The saved options are reversed */
  for (extras = extra_options, i = extra_options_count; extras;
       extras = extras->next)
    argv[opt + --i] = extras->opt;
  opt += extra_options_count;

  argv[opt++] = NULL;
}

static char *gcc_builtin_macros_file;

static void gcc_cpp_cleanup(void)
{
  if (gcc_builtin_macros_file)
    unlink(gcc_builtin_macros_file);
}

static void print_error_file(FILE *to, char *filename)
{
  char line[256];
  FILE *f;

  if (!filename)
    {
      fprintf(to, "-- no output\n");
      return;
    }

  f = fopen(filename, "r");
  if (!f) 
    {
      fprintf(to, "-- failed to open error message file %s\n", filename);
      return;
    }

  while (fgets(line, sizeof line - 1, f))
    fputs(line, to);

  if (ferror(f))
    fprintf(to, "-- error reading message file %s\n", filename);

  fclose(f);
}

const char *gcc_global_cpp_init(void)
{
  static char tbuiltins[] = "/tmp/nesccppbXXXXXX";
  static char tincludes[] = "/tmp/nesccppiXXXXXX";
  char *includes = NULL;
  FILE *incf;
  char line[LINELEN];
  bool quote_includes = FALSE, bracket_includes = FALSE;

  atexit(gcc_cpp_cleanup);

  /* Execute gcc to get builtin macros and include search path */
  if (!exec_gcc(tbuiltins, TRUE, &gcc_builtin_macros_file,
		tincludes, TRUE, &includes, 
		7 + extra_options_count, gcc_preprocess_init_setargs, NULL))
    {
      error("invocation of %s to find builtin macros failed (error message output follows)", 
	    target_compiler);
      print_error_file(stderr, includes);

      return NULL;
    }

  /* Read gcc error output to get search path */
  incf = fopen(includes, "r");
  if (!incf)
    {
      unlink(includes);
      return NULL;
    }

  while (fgets(line, LINELEN - 1, incf))
    {
      if (!strncmp(line, "#include \"...\"", 14))
	quote_includes = TRUE;
      else if (!strncmp(line, "#include <...>", 14))
	bracket_includes = TRUE;
      else if (!strncmp(line, "End of search list.", 19))
	break;
      else if (bracket_includes)
	add_nesc_dir(sanitize_path(permanent, line), CHAIN_SYSTEM);
      else if (quote_includes)
	add_nesc_dir(sanitize_path(permanent, line), CHAIN_QUOTE);
    }
  fclose(incf);
  unlink(includes);

  return gcc_builtin_macros_file;
}

void gcc_save_machine_options(const char *opt)
{
  if (opt[1] == 'm')
    add_gcc_option(opt);
}

