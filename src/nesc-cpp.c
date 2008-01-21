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

struct cpp_option {
  struct cpp_option *next;
  const char *opt, *arg;
};

static region opt_region;
static char *cpp_save_dir;
static dhash_table current_macros;
static struct cpp_option *saved_options;

void save_cpp_option(const char *option, const char *arg)
{
  struct cpp_option *newopt;

  if (!opt_region)
    opt_region = newregion();

  newopt = ralloc(opt_region, struct cpp_option);
  newopt->opt = option;
  newopt->arg = arg;
  newopt->next = saved_options;
  saved_options = newopt;
}

static void cpp_unlink(const char *name)
{
  static bool checkedDebug;
  static bool nounlink;

  if (!checkedDebug)
    {
      checkedDebug = TRUE;
      nounlink = getenv("NESC_KEEP_CPP") != NULL;
    }

  if (!nounlink)
    unlink(name);
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

      if (outputfd < 0 || dup2(outputfd, 1) < 0 ||
	  errorfd < 0 || dup2(errorfd, 2) < 0)
	exit(2);

      close(outputfd);
      close(errorfd);

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

      execvp(target_compiler, (char **)argv);
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

/* Make a copy of path with trailing whitespace (normally CRs, LFs) removed */
static char *sanitize_path(region r, const char *path)
{
  char *pcopy = rstrdup(r, path);
  int l = strlen(pcopy);

  while (l > 0 && (pcopy[l - 1] == '\r' || pcopy[l - 1] == '\n'))
    pcopy[--l] = '\0';

  return pcopy;
}

static void gcc_preprocess_init_setargs(void *data, const char **argv)
{
  argv[0] = "-v";
  argv[1] = "-x";
  argv[2] = "c";
  argv[3] = "/dev/null";
  argv[4] = "-dM";
  if (flag_nostdinc)
    argv[5] = "-nostdinc";
  else
    argv[5] = NULL;
  argv[6] = NULL;
}

static char *gcc_builtin_macros_file;

static void gcc_cpp_cleanup(void)
{
  if (gcc_builtin_macros_file)
    cpp_unlink(gcc_builtin_macros_file);
}

const char *gcc_global_cpp_init(void)
{
  static char tbuiltins[] = "/tmp/nesccppbXXXXXX";
  static char tincludes[] = "/tmp/nesccppiXXXXXX";
  char *includes;
  FILE *incf;
  char line[LINELEN];
  bool quote_includes = FALSE, bracket_includes = FALSE;

  atexit(gcc_cpp_cleanup);

  /* Execute gcc to get builtin macros and include search path */
  if (!exec_gcc(tbuiltins, TRUE, &gcc_builtin_macros_file,
		tincludes, TRUE, &includes, 
		6, gcc_preprocess_init_setargs, NULL))
    return NULL;

  /* Read gcc error output to get search path */
  incf = fopen(includes, "r");
  if (!incf)
    {
      cpp_unlink(includes);
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
  cpp_unlink(includes);

  return gcc_builtin_macros_file;
}

void preprocess_init(void)
{
  struct cpp_option *opt;
  cpp_reader *reader;
  cpp_options *cpp_opts;
  const char *builtin_macros_file;

  builtin_macros_file = target->global_cpp_init();
  init_nesc_paths_end();
  if (!start_lex(l_c, builtin_macros_file))
    {
      error("internal error: couldn't define builtin macros - exiting");
      exit(2);
    }

  reader = current.lex.finput;
  cpp_opts = cpp_get_options(reader);
  cpp_opts->warn_unused_macros = 0;
  if (!flag_undef)
    cpp_scan_nooutput(reader);

  /* Process saved options */
  current.lex.input->l.filename = "<command-line>";
  current.lex.input->l.lineno = 0;
  for (opt = saved_options; opt; opt = opt->next)
    {
      if (opt->opt[0] == 'D')
	cpp_define(reader, opt->arg);
      else if (opt->opt[0] == 'U')
	cpp_undef(reader, opt->arg);
      else if (opt->opt[0] == 'A')
	{
	  if (opt->arg[0] == '-')
	    cpp_unassert(reader, opt->arg + 1);
	  else
	    cpp_assert(reader, opt->arg);
	}
    }
  end_lex();
}

static void macro_set(const unsigned char *name, const unsigned char *value)
{
  const unsigned char **old_value = dhlookup(current_macros, (char *)name);

  if (!old_value)
    {
      old_value = ralloc(permanent, const unsigned char *);
      dhadd(current_macros, old_value);
    }
  *old_value = value;
}

static void cb_define(cpp_reader *reader, source_location loc, 
		      cpp_hashnode *macro)
{
  macro_set(NODE_NAME(macro), cpp_macro_definition(reader, macro));
}

static void cb_undef (cpp_reader *reader, source_location loc,
		      cpp_hashnode *macro)
{
  macro_set(NODE_NAME(macro), NULL);
}

void start_macro_saving(void)
{
  cpp_reader *reader = current.lex.finput;
  cpp_callbacks *cbacks = cpp_get_callbacks(reader);
  dhash_scan existing_macros;
  unsigned char **macro;

  /* Start by defining the current macros */
  existing_macros = dhscan(current_macros);
  while ((macro = dhnext(&existing_macros)))
    if (*macro) /* Ignore undef'ed macros - see cb_undef */
      cpp_define(reader, (const char *)*macro);

  /* And set the include chain stuff */

  cbacks->define = cb_define;
  cbacks->undef = cb_undef;
  if (target->file_cpp_init)
    target->file_cpp_init();
}

void end_macro_saving(void)
{
  cpp_reader *reader = current.lex.finput;
  cpp_callbacks *cbacks = cpp_get_callbacks(reader);

  cbacks->define = NULL;
  cbacks->undef = NULL;
}

void set_cpp_dir(const char *dir)
{
  struct stat dbuf;
  int l = strlen(dir);

  cpp_save_dir = xstrdup(dir);

  /* Remove trailing slashes */
  while (l > 1 && cpp_save_dir[l - 1] == '/')
    cpp_save_dir[--l] = '\0';

  mkdir(cpp_save_dir, 0777);
  if (stat(cpp_save_dir, &dbuf) < 0 || !S_ISDIR(dbuf.st_mode))
    {
      /* There's no real recovery from this problem */
      fprintf(stderr, "Couldn't create directory `%s'\n", cpp_save_dir);
      exit(2);
    }
}
