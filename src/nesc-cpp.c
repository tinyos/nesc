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

struct macro_def {
  const unsigned char *name;
  const unsigned char *def;
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

static int macro_compare(void *entry1, void *entry2)
{
  struct macro_def *e1 = entry1, *e2 = entry2;

  return !strcmp((const char *)e1->name, (const char *)e2->name);
}

static unsigned long macro_hash(void *entry)
{
  struct macro_def *e = entry;

  return hash_str((const char *)e->name);
}

static void macro_set(const unsigned char *name, const unsigned char *value)
{
  struct macro_def fake = { name, NULL };
  struct macro_def *old_value = dhlookup(current_macros, &fake);

  if (!old_value)
    {
      old_value = ralloc(permanent, struct macro_def);
      old_value->name = name;
      dhadd(current_macros, old_value);
    }
  old_value->def = value;
}

void preprocess_init(void)
{
  struct cpp_option *opt;
  cpp_callbacks *cpp_cbacks;
  cpp_reader *reader;
  cpp_options *cpp_opts;
  const char *builtin_macros_file;

  current_macros = new_dhash_table(permanent, 512, macro_compare, macro_hash);

  builtin_macros_file = target->global_cpp_init();
  init_nesc_paths_end();
  current.fileregion = newregion();
  if (!builtin_macros_file || !start_lex(l_c, builtin_macros_file))
    {
      error("internal error: couldn't define builtin macros - exiting");
      exit(2);
    }

  reader = current.lex.finput;
  cpp_opts = cpp_get_options(reader);
  cpp_cbacks = cpp_get_callbacks(reader);
  cpp_opts->warn_unused_macros = 0;
  if (!flag_undef)
    cpp_scan_nooutput(reader);

  /* Process saved options */
  cpp_cbacks->file_change(reader, linemap_add(current.lex.line_map, LC_RENAME, 0,
					      "<command-line>", 0));

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
  current.lex.input = NULL;
  deleteregion_ptr(&current.fileregion);
}

static void cb_define(cpp_reader *reader, source_location loc, 
		      cpp_hashnode *macro)
{
  char *def = rstrdup(permanent, (char *)cpp_macro_definition(reader, macro));
  char *firstspace = strchr(def, ' ');

  /* Massage def into cpp_define's format (same as -D) */
  *firstspace = '=';
  macro_set(NODE_NAME(macro), (const unsigned char *)def);
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
  struct macro_def *macro;

  /* Start by defining the current macros */
  existing_macros = dhscan(current_macros);
  while ((macro = dhnext(&existing_macros)))
    if (macro->def) /* Ignore undef'ed macros - see cb_undef */
      cpp_define(reader, (const char *)macro->def);

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
