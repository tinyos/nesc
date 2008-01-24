#include <ctype.h>

#include "parser.h"
#include "nesc-ndoc.h"
#include "c-parse.h"
#include "semantics.h"

static char *rmakestr(region r, char *s, char *e)
{
  int l = e - s;
  char *news = rstralloc(r, l + 1);

  news[l] = '\0';
  memcpy(news, s, l);

  return news;
}

static void save_doctag(location loc, region entries_r, dd_list *entries,
			char *tag_s, char *tag_e,
			char *arg_s, char *arg_e,
			int lineno)
{
  if (entries)
    {
      struct doctag *dtag = typed_ralloc(entries_r, sizeof(struct doctag) + sizeof(const char *), rctypeof(struct doctag));

      dtag->lineno = lineno;
      dtag->tag = rmakestr(entries_r, tag_s, tag_e);
      dtag->args[0] = rmakestr(entries_r, arg_s, arg_e);
      dd_add_last(entries_r, *entries, dtag);
    }
  else
    {
      /* warn about ignored quoted arguments that normally have
	 semantic significance */
      struct location here = *loc;

      here.lineno += lineno;
      warning_with_location(&here, "@%s argument ignored",
			    rmakestr(current.fileregion, tag_s, tag_e));
    }
}

bool get_latest_docstring(struct docstring *doc, region tags_r, dd_list *tags)
{
  enum { s_linestart, s_normal, s_doctag, s_docarg_start, s_docarg } state;
  const char *raw, *r;
  char *parsed, *p, *short_end, *doctag, *doctag_end, *docarg;
  bool cpp_comment;
  int lineno;
  char c;
  location loc;

  if (!get_raw_docstring(&raw, &loc))
    return FALSE;

  if (warn_unexpected_docstring && doc->short_s)
    {
      warning_with_location(loc, "duplicate documentation string");
      warning_with_location(doc->loc, "(this is the location of the previous documentation string)");
    }
  doc->loc = loc;
  doc->long_s = NULL;

  if (tags)
    *tags = dd_new_list(tags_r);
  parsed = alloca(strlen(raw) + 1);

  /* Prepare to parse raw */
  cpp_comment = raw[1] == '/';
  if (cpp_comment)
    r = raw;
  else
    r = raw + 2;
  p = parsed;
  lineno = 0;
  state = s_linestart;
  short_end = NULL;
  while ((c = *r++))
    {
      if (state == s_linestart)
	{
	  if (isspace(c) || (cpp_comment && c == '/'))
	    continue;
	  state = s_normal;
	  /* Skip one * in C-style comments (we've cleared line_start, so
	     won't do this twice) */
	  if (!cpp_comment && c == '*')
	    continue;
	}

      if (c == '\r' || c == '\n')
	{
	  if (state == s_docarg)
	    {
	      /* warn about broken quoted arguments, as they have semantic
		 significance */
	      struct location here = *loc;

	      here.lineno += lineno;
	      warning_with_location(&here, "unterminated @%s argument ignored",
				    rmakestr(current.fileregion, doctag, doctag_end));
	    }

	  *p++ = '\n';
	  lineno++;
	  state = s_linestart;
	  continue;
	}

      /* Skip trailing / in C-style comments */
      if (!cpp_comment && c == '/' && !*r)
	break;

      *p++ = c;

      /* Space after . or space before @ indicates end of short doc string */
      if (!short_end)
	{
	  if (c == '.' && isspace(*r))
	    short_end = p;
	  else if (c == '@' && p - 2 >= parsed && isspace(p[-2]))
	    short_end = p - 1;

	  if (short_end)
	    doc->short_s = rmakestr(parse_region, parsed, short_end);
	}

      /* Extract tags with a fun state machine */
    redo:
      switch (state)
	{
	case s_normal:
	  if (c == '@' && p - 2 >= parsed && isspace(p[-2]))
	    {
	      doctag = p;
	      state = s_doctag;
	    }
	  break;
	case s_doctag:
	  if (!isalpha(c)) /* doctag keyword done, decide if we like it */
	    {
	      /* Currently we like all non-empty doctags and assume
		 they might have up to one quoted argument - filtering
		 is left to our caller. This might want to change if
		 we get into fancier syntax for doctag arguments with
		 semantic significance */
	      if (p - 1 > doctag) /* not empty */
		{
		  doctag_end = p - 1;
		  state = s_docarg_start;
		}
	      else
		state = s_normal;
	      goto redo;
	    }
	  break;
	case s_docarg_start:
	  if (isspace(c))
	    ;
	  else if (c == '\'')
	    {
	      docarg = p;
	      state = s_docarg;
	    }
	  else
	    {
	      state = s_normal;
	      goto redo;
	    }
	  break;
	case s_docarg:
	  if (c == '\'')
	    {
	      save_doctag(loc, tags_r, tags,
			  doctag, doctag_end, docarg, p - 1, lineno);
	      state = s_normal;
	    }
	  break;
	default:
	  assert(0);
	  break;
	}
    }

  if (short_end)
    {
      /* if there's only whitespace after short_end, then there's no long
	 string */
      while (*short_end && isspace(*short_end))
	short_end++;
      if (*short_end)
	doc->long_s = rmakestr(parse_region, parsed, p);
    }
  else /* only a short string */
    doc->short_s = rmakestr(parse_region, parsed, p);

  return TRUE;
}

#ifdef TESTING
char doc_string[65536];

struct location dummy = { "<stdin>", NULL, 1 };

bool get_raw_docstring(const char **docs, location *docl)
{
  *docs = doc_string;
  *docl = &dummy;
  return TRUE;
}

region parse_region;

/* Report warning msg at l */
void vwarning_with_location(location l, const char *format, va_list args)
{
  fprintf(stderr, "%s:%lu: ", l->filename, l->lineno);
  fprintf(stderr, "warning: ");
  vfprintf(stderr, format, args);
  putc('\n', stderr);
}

/* Report warning msg at l */
void warning_with_location(location l, const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vwarning_with_location(l, format, args);
  va_end(args);
}

struct semantic_state current;
int warn_unexpected_docstring;

int region_main(int argc, char **argv)
{
  struct docstring doc = { NULL, NULL, NULL };
  dd_list tags;
  dd_list_pos e;

  if (argc != 1)
    return 2;

  current.fileregion = parse_region = newregion();
  fread(doc_string, sizeof doc_string, 1, stdin);
  get_latest_docstring(&doc, permanent, &tags);
  printf("short: %s\n", doc.short_s);
  printf("long: %s\n", doc.long_s ? doc.long_s : "<null>");

  dd_scan (e, tags)
    {
      struct doctag *dtag = DD_GET(struct doctag *, e);

      printf("at %d: %s is %s\n", dtag->lineno, dtag->tag, dtag->args[0]);
    }

  return 0;
}
#endif
