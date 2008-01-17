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

static region pragma_region;
dd_list pragmas;

static region opt_region;
static char *cpp_dir;

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

static char *kwd_macros, tkwd_macros[] = "/tmp/nesccppkXXXXXX";
static char *cpp_macros1, tcpp_macros1[] = "/tmp/nesccppm1XXXXXX";
static char *cpp_macros2, tcpp_macros2[] = "/tmp/nesccppm2XXXXXX";
static char *cpp_macros;

static char *nesc_keywords[] = {
#define K(name, token, rid) #name,
#include "nesc-keywords.h"
NULL
};

static FILE *macros_file;
static const char *macros_mode;

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

static FILE *exec_gcc(char *gcc_output_template, bool mktmp,
		      char **gcc_output_file, bool redirect_errors, 
		      int nargs,
		      void (*setargs)(void *data, const char **argv), void *data)
{
  int gcc_stat, res, destfd;
  const char **argv;
  static int tmpfd1 = -1, tmpfd2 = -1;
  char *outputf;

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

  if (mktmp)
    outputf = mktempfile(permanent, gcc_output_template);
  else
    outputf = gcc_output_template;
  *gcc_output_file = outputf;
  destfd = creat(outputf, 0666);

  if (destfd < 0)
    return NULL;

  if (!safe_dup(destfd, 1, tmpfd1))
    return NULL;

  if (redirect_errors)
    if (!safe_dup(destfd, 2, tmpfd2))
      {
	dup_restore(1, tmpfd1);
	return NULL;
      }

  close(destfd);

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
  if (redirect_errors)
    dup_restore(2, tmpfd2);

  if (res == 0) /* gcc succeeded */
    return fopen(outputf, "r");
  else
    return NULL;
}
#else
#include <sys/wait.h>

static FILE *exec_gcc(char *gcc_output_template, bool mktmp,
		      char **gcc_output_file,
		      bool redirect_errors, int nargs,
		      void (*setargs)(void *data, const char **argv), void *data)
{
  int gcc_pid, gcc_stat, res;
  char *outputf;

  if (mktmp)
    outputf = mktempfile(permanent, gcc_output_template);
  else
    outputf = gcc_output_template;
  *gcc_output_file = outputf;

  if ((gcc_pid = fork()) == 0)
    {
      const char **argv;
      int destfd = creat(outputf, 0666);

      if (destfd < 0 || dup2(destfd, 1) < 0)
	exit(2);

      if (redirect_errors)
	if (dup2(destfd, 2) < 0)
	  exit(2);

      close(destfd);

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

  if (res == 0) /* gcc succeeded */
    return fopen(outputf, "r");
  else
    return NULL;
}
#endif

void preprocess_cleanup(void)
{
  cpp_unlink(cpp_macros1);
  cpp_unlink(cpp_macros2);
  cpp_unlink(kwd_macros);
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
     during compilation by the flag "-fnesc-nido-tosnodes = 1000"
     which in this case is 1000. */     
  if (use_nido)
    fprintf(mf, "#define TOSH_NUM_NODES %s\n", nido_num_nodes);

  fclose(mf);
}

static void version_setargs(void *data, const char **argv)
{
  argv[0] = "-v";
  argv[1] = NULL;
}

#define LINELEN 160

void select_macros_mode(void)
{
  static char gcc_version_name[] = "/tmp/nesccppvXXXXXX";
  char *gcc_version_file;
  FILE *gcc_version = exec_gcc(gcc_version_name, TRUE, &gcc_version_file, TRUE, 1, version_setargs, NULL);

  macros_mode = "w";

  if (gcc_version)
    {
      char line[LINELEN];

      while (fgets(line, LINELEN - 1, gcc_version))
	{
	  if (!strncmp(line, "gcc version ", 12))
	    {
	      double version;

	      if (sscanf(line, "gcc version %lf", &version) == 1)
		{
		  if (version <= 2.95)
		    macros_mode = "a";
		}
	    }
	}
      fclose(gcc_version);
    }
  cpp_unlink(gcc_version_file);
}

void preprocess_init(void)
{
  atexit(preprocess_cleanup);

  cpp_macros1 = mktempfile(permanent, tcpp_macros1);
  cpp_macros2 = mktempfile(permanent, tcpp_macros2);
  kwd_macros = mktempfile(permanent, tkwd_macros);

  create_nesc_keyword_macros(kwd_macros);

  select_macros_mode();

  pragma_region = newregion();
  pragmas = dd_new_list(pragma_region);
}

struct preprocess_args_closure
{
  /* Ok, ok, the function isn't *in* the closure */
  source_language l;
  const char *filename;
  int nargs;
};

static void preprocess_setargs(void *data, const char **argv)
{
  struct preprocess_args_closure *closure = data;
  struct cpp_option *saved;
  region filename_region = newregion();
  int arg = 0, i;

  rarraycopy(argv + arg, path_argv, path_argv_count, const char *);
  arg += path_argv_count;

  /* The saved options are reversed */
  for (saved = saved_options, i = saved_options_count; saved;
       saved = saved->next)
    argv[arg + --i] = (char *)saved->opt;
  arg += saved_options_count;

  argv[arg++] = "-E";
  argv[arg++] = "-C";
  argv[arg++] = "-dD"; /* ask cpp to output macros */

  /* For C files, we define keywords away (kwd_macros) */
  if (closure->l == l_c)
    {
      argv[arg++] = "-imacros";
      argv[arg++] = fix_filename(filename_region, kwd_macros);
    }
  else
    {
      argv[arg++] = "-x"; /* preprocess as C file */
      argv[arg++] = "c";
    }

  if (cpp_macros)
    {
      argv[arg++] = "-imacros";
      argv[arg++] = fix_filename(filename_region, cpp_macros);
    }
  argv[arg++] = fix_filename(filename_region, closure->filename);
  argv[arg++] = NULL;
  assert(arg <= closure->nargs);
}

FILE *preprocess(const char *filename, source_language l)
{
  struct preprocess_args_closure closure;
  char *cpp_dest;
  int nargs = 12 + path_argv_count + saved_options_count;
  FILE *output;

  closure.l = l;
  closure.filename = filename;
  closure.nargs = nargs;
  if (cpp_dir)
    {
      char *tmp = rstrdup(parse_region, filename);
      char *base = basename(tmp);

      cpp_dest = rstralloc(parse_region, strlen(cpp_dir) + strlen(base) + 2);
      sprintf(cpp_dest, "%s/%s", cpp_dir, base);
    }
  else
    cpp_dest = rstrdup(parse_region, "/tmp/nesccppsXXXXXX");
  output = exec_gcc(cpp_dest, cpp_dir == 0, &current.preprocessed_file,
		    FALSE, nargs, preprocess_setargs, &closure);


  if (output)
    {
      /* Save macros */
      /* Note: this works with a global macros file because we use it one
	 of two modes:
	 - old style with 'includes': 
	   we only keep macros from C files, which don't cause any other files
	   to be loaded
	 - new style with #include:
	   we only keep macros from before the module, interface, configuration
	   or component keyword => we complete using the file before loading
	   the next one.
      */
      if (cpp_macros == cpp_macros1)
	cpp_macros = cpp_macros2;
      else
	cpp_macros = cpp_macros1;
      macros_file = fopen(cpp_macros, macros_mode);
      if (!macros_file)
	error("failed to create temporary file");

      return output;
    }
  else
    return NULL;
}

void save_pragma(struct location l, const char *args)
{
  struct pragma *p = ralloc(pragma_region, struct pragma);

  p->l = make_location(l);
  p->args = rstrdup(pragma_region, args);
  dd_add_last(pragma_region, pragmas, p);
}

void handle_directive(const char *directive, const char *args)
{
  const char *arg2;

  if (!strcmp(directive, "pragma"))
    {
      save_pragma(current.lex.input->l, args);
      return;
    }

  /* If the filename starts with <, these are special macros (built in
     or from the command line) */
  if (current.lex.input && current.lex.input->l.filename[0] == '<')
    return;

  if (!(strcmp(directive, "define") == 0 || strcmp(directive, "undef") == 0))
    return;

  arg2 = strchr(args, ' ');
  if (arg2 && strncmp(arg2 + 1, "__nesc_keyword_", 15) == 0)
    return;

  if (macros_file)
    fprintf(macros_file, "#%s %s\n", directive, args);
}

void end_macro_saving(void)
{
  fclose(macros_file);
  macros_file = NULL;
}

void preprocess_file_end(void)
{
  if (macros_file)
    end_macro_saving();

  if (!cpp_dir)
    cpp_unlink(current.preprocessed_file);
}

void set_cpp_dir(const char *dir)
{
  struct stat dbuf;
  int l = strlen(dir);

  cpp_dir = strdup(dir);

  /* Remove trailing slashes */
  while (l > 1 && cpp_dir[l - 1] == '/')
    cpp_dir[--l] = '\0';

  mkdir(cpp_dir, 0777);
  if (stat(cpp_dir, &dbuf) < 0 || !S_ISDIR(dbuf.st_mode))
    {
      /* There's no real recovery from this problem */
      fprintf(stderr, "Couldn't create directory `%s'\n", cpp_dir);
      exit(2);
    }
}
