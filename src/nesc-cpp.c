/* This file is part of the nesC compiler.

   This file is derived in part from the GNU C Compiler. It is thus
     Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Changes for nesC are
     Copyright (C) 2002-2008 Intel Corporation

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

static void account_for_newlines (const unsigned char *, size_t);
static void print_line (source_location, const char *);
static void maybe_print_line (source_location);
static void save_pp_define(cpp_reader *pfile, source_location line,
			    cpp_hashnode *node);
static void save_pp_undef(source_location line, cpp_hashnode *node);

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
      old_value->name = (unsigned char *)rstrdup(permanent, (char *)name);
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
  if (use_nido)
    {
      char *buf = alloca(16 + strlen(nido_num_nodes));

      sprintf(buf, "TOSH_NUM_NODES=%s", nido_num_nodes);
      cpp_define(reader, buf);
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

  save_pp_define(reader, loc, macro);
}

static void cb_undef(cpp_reader *reader, source_location loc,
		     cpp_hashnode *macro)
{
  macro_set(NODE_NAME(macro), NULL);
  save_pp_undef(loc, macro);
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

void save_pp_dir(const char *dir)
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
      /* Just give up on saving preprocessed output */
      fprintf(stderr, "Couldn't create directory `%s'\n", cpp_save_dir);
      cpp_save_dir = NULL;
    }
}

void save_pp_file_start(const char *path)
{
  if (!cpp_save_dir)
    return;

  const char *fname = lbasename(path);
  char *pp_path = rstralloc(current.fileregion,
			    strlen(cpp_save_dir) + strlen(fname) + 2);

  sprintf(pp_path, "%s/%s", cpp_save_dir, fname);
  current.lex.pp.outf = fopen(pp_path, "w");
  if (!current.lex.pp.outf)
    {
      static int first = 1;

      if (first)
	warning("cannot create preprocessed output file `%s'", pp_path);
      first = FALSE;
      return;
    }

  /* Initialize the print structure.  Setting current.lex.pp.src_line to -1 here is
     a trick to guarantee that the first token of the file will cause
     a linemarker to be output by maybe_print_line.  */
  current.lex.pp.src_line = -1;
  current.lex.pp.printed = 0;
  current.lex.pp.prev = 0;
  current.lex.pp.first_time = 1;
  current.lex.pp.avoid_paste = 0;
  current.lex.pp.source = NULL;
}

void save_pp_file_end(void)
{
  if (!current.lex.pp.outf)
    return;

  /* Flush any pending output.  */
  if (current.lex.pp.printed)
    putc('\n', current.lex.pp.outf);
  fclose(current.lex.pp.outf);
  current.lex.pp.outf = NULL;
}

void save_pp_token(const cpp_token *token)
{
  cpp_reader *pfile = current.lex.finput;

  if (!current.lex.pp.outf)
    return;

  if (token->type == CPP_PADDING)
    {
      current.lex.pp.avoid_paste = true;
      if (current.lex.pp.source == NULL
	  || (!(current.lex.pp.source->flags & PREV_WHITE)
	      && token->val.source == NULL))
	current.lex.pp.source = token->val.source;
      return;
    }

  if (token->type == CPP_EOF)
    return;

  /* Subtle logic to output a space if and only if necessary.  */
  if (current.lex.pp.avoid_paste)
    {
      if (current.lex.pp.source == NULL)
	current.lex.pp.source = token;
      if (current.lex.pp.source->flags & PREV_WHITE
	  || (current.lex.pp.prev
	      && cpp_avoid_paste(pfile, current.lex.pp.prev, token))
	  || (current.lex.pp.prev == NULL && token->type == CPP_HASH))
	putc(' ', current.lex.pp.outf);
    }
  else if (token->flags & PREV_WHITE)
    putc(' ', current.lex.pp.outf);

  current.lex.pp.avoid_paste = false;
  current.lex.pp.source = NULL;
  current.lex.pp.prev = token;
  cpp_output_token(token, current.lex.pp.outf);

  if(token->type == CPP_COMMENT)
    account_for_newlines(token->val.str.text, token->val.str.len);
}

/* Adjust current.lex.pp.src_line for newlines embedded in output.  */
static void account_for_newlines(const unsigned char *str, size_t len)
{
  while (len--)
    if (*str++ == '\n')
      current.lex.pp.src_line++;
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker, and return 1.  Otherwise return 0.  */
static void maybe_print_line(source_location src_loc)
{
  const struct line_map *map = linemap_lookup(current.lex.line_map, src_loc);
  int src_line = SOURCE_LINE(map, src_loc);
  /* End the previous line of text.  */
  if (current.lex.pp.printed)
    {
      putc('\n', current.lex.pp.outf);
      current.lex.pp.src_line++;
      current.lex.pp.printed = 0;
    }

  if (src_line >= current.lex.pp.src_line && src_line < current.lex.pp.src_line + 8)
    {
      while (src_line > current.lex.pp.src_line)
	{
	  putc('\n', current.lex.pp.outf);
	  current.lex.pp.src_line++;
	}
    }
  else
    print_line(src_loc, "");
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  */
static void print_line(source_location src_loc, const char *special_flags)
{
  /* End any previous line of text.  */
  if (current.lex.pp.printed)
    putc('\n', current.lex.pp.outf);
  current.lex.pp.printed = 0;

  /*if (!flag_no_line_commands)*/
    {
      const struct line_map *map = linemap_lookup(current.lex.line_map, src_loc);

      size_t to_file_len = strlen(map->to_file);
      unsigned char *to_file_quoted =
         (unsigned char *) alloca(to_file_len * 4 + 1);
      unsigned char *p;

      current.lex.pp.src_line = SOURCE_LINE(map, src_loc);

      /* cpp_quote_string does not nul-terminate, so we have to do it
	 ourselves.  */
      p = cpp_quote_string(to_file_quoted,
			    (unsigned char *) map->to_file, to_file_len);
      *p = '\0';
      fprintf(current.lex.pp.outf, "# %u \"%s\"%s",
	       current.lex.pp.src_line == 0 ? 1 : current.lex.pp.src_line,
	       to_file_quoted, special_flags);

      if (map->sysp == 2)
	fputs(" 3 4", current.lex.pp.outf);
      else if (map->sysp == 1)
	fputs(" 3", current.lex.pp.outf);

      putc('\n', current.lex.pp.outf);
    }
}

/* Called when a line of output is started.  TOKEN is the first token
   of the line, and at end of file will be CPP_EOF.  */
void save_pp_line_change(cpp_reader *pfile, const cpp_token *token)
{
  if (!current.lex.pp.outf)
    return;

  source_location src_loc = token->src_loc;

  maybe_print_line(src_loc);
  current.lex.pp.prev = 0;
  current.lex.pp.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.  */
  /*if (!CPP_OPTION(pfile, traditional))*/
    {
      const struct line_map *map = linemap_lookup(current.lex.line_map, src_loc);
      int spaces = SOURCE_COLUMN(map, src_loc) - 2;
      current.lex.pp.printed = 1;

      while (-- spaces >= 0)
	putc(' ', current.lex.pp.outf);
    }
}

static void save_pp_define(cpp_reader *pfile, source_location line,
			   cpp_hashnode *node)
{
  if (!current.lex.pp.outf)
    return;

  maybe_print_line (line);

  fprintf(current.lex.pp.outf, "#define %s\n",
	  (const char *)cpp_macro_definition(pfile, node));

  if (linemap_lookup(current.lex.line_map, line)->to_line != 0)
    current.lex.pp.src_line++;
}

static void save_pp_undef(source_location line, cpp_hashnode *node)
{
  if (!current.lex.pp.outf)
    return;

  maybe_print_line(line);
  fprintf(current.lex.pp.outf, "#undef %s\n", NODE_NAME(node));
  current.lex.pp.src_line++;
}
