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

#include "parser.h"
#include <stdio.h>
#include <setjmp.h>

#include "c-lex.h"
#include "c-parse.h"
#include "c-parse.tab.h"
#include "input.h"
#include "semantics.h"
#include "constants.h"

#include <ctype.h>

/* Define to support nested comments. Probably a bad idea given we're 
   pre-processing w/ gcc which doesn't know about nested comments */
#undef NESTED_COMMENTS

/* MULTIBYTE_CHARS support only works for native compilers.
   ??? Ideally what we want is to model widechar support after
   the current floating point support.  */
#ifdef CROSS_COMPILE
#undef MULTIBYTE_CHARS
#endif

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <stdlib.h>
#include <locale.h>
#endif

static int max_char_length = 1; /* Default value if not MULTIBYTE_CHARS */

/* Location of last token. Used for location of error nodes */
location last_location;

location dummy_location, toplevel_location;

/* Location cache handling */
static location last_allocated_location;

static location make_location(struct location l)
{
  if (l.lineno == last_allocated_location->lineno &&
      l.filename == last_allocated_location->filename &&
      l.in_system_header == last_allocated_location->in_system_header)
    return last_allocated_location;

  last_allocated_location = ralloc(parse_region, struct location);
  *last_allocated_location = l;

  return last_allocated_location;
}

static size_t int_type_size;

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

void lex_ungetc(int c)
{
  ungetc (c, input_file_stack->lex.finput);
}

int lex_getc(void)
{
  /* Nuke CR from CR-LF */
  int c = getc (input_file_stack->lex.finput);

  if (c != '\r')
    return c;

  c = getc (input_file_stack->lex.finput);
  if (c == '\n')
    return c;

  lex_ungetc(c);

  return '\r';
}

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
/*tree lastiddecl;*/

extern int yydebug;

static int maxtoken;		/* Current nominal length of token buffer.  */
static char *traditional token_buffer;	/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2. */

static wchar_array string_array;
static char_array docstring_array;

static int language_token;

static char *extend_token_buffer(char *);
int check_newline(void);

/* Do not insert generated code into the source, instead, include it.
   This allows us to build gcc automatically even for targets that
   need to add or modify the reserved keyword lists.  */
#include "c-gperf.h"

void
init_lex (void)
{
  static struct location dummy, toplevel;

  dummy.lineno = 0;
  dummy.filename = "<dummy>";
  dummy.in_system_header = FALSE;
  dummy_location = last_allocated_location = &dummy;

  toplevel.filename = "<commandline>";
  toplevel.lineno = 0;
  toplevel.in_system_header = FALSE;
  toplevel_location = &toplevel;

#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
  literal_codeset = getenv ("LANG");
  max_char_length = local_mb_cur_max ();
#endif

  maxtoken = 40;
  token_buffer = (char *) xmalloc (maxtoken + 2);

  string_array = new_wchar_array(parse_region, 512);
  docstring_array = new_char_array(parse_region, 2048);

  int_type_size = type_size_int(int_type);

  /* Some options inhibit certain reserved words.
     Clear those words out of the hash table so they won't be recognized.  */
#define UNSET_RESERVED_WORD(STRING) \
  do { struct resword *s = is_reserved_word (STRING, sizeof (STRING) - 1); \
       if (s) s->name = ""; } while (0)

  if (flag_traditional)
    {
      UNSET_RESERVED_WORD ("const");
      UNSET_RESERVED_WORD ("volatile");
      UNSET_RESERVED_WORD ("typeof");
      UNSET_RESERVED_WORD ("signed");
      UNSET_RESERVED_WORD ("inline");
      UNSET_RESERVED_WORD ("iterator");
      UNSET_RESERVED_WORD ("complex");
    }
  if (flag_no_asm)
    {
      UNSET_RESERVED_WORD ("asm");
      UNSET_RESERVED_WORD ("typeof");
      UNSET_RESERVED_WORD ("inline");
      UNSET_RESERVED_WORD ("iterator");
      UNSET_RESERVED_WORD ("complex");
    }

}

void
start_lex (source_language l)
{
  input_file_stack->lex.end_of_file = 0;
  input_file_stack->lex.nextchar = -1;
  input_file_stack->lex.indent_level = 0;
  language_token = -1;

  lex_ungetc(check_newline());

  switch (l)
    {
    case l_interface: case l_component: language_token = DISPATCH_NESC; break;
    case l_c: language_token = DISPATCH_C; break;
    default: assert(0); break;
    }
}

#ifdef RC_ADJUST
static size_t rc_adjust_yystype(void *x, int by) 
{
  struct yystype *p = x;
  RC_ADJUST_PREAMBLE;

  RC_ADJUST(p->u.ptr, by);
  RC_ADJUST(p->idtoken.location.filename, by);
  RC_ADJUST(p->idtoken.id.data, by);
  RC_ADJUST(p->idtoken.decl, by);

  return sizeof *p;
}
#endif

/* Function used when yydebug is set, to print a token in more detail.  */

void
yyprint (file, yychar, yylval)
     FILE *file;
     int yychar;
     YYSTYPE yylval;
{
  switch (yychar)
    {
    case IDENTIFIER:
      fprintf(file, " '%s'", yylval.idtoken.id.data);
      break;
    }
}

/* flag to allow skipping of initial whitespace & * character */ 
bool doc_skip_prefix;
unsigned long prev_cpp_docstring_line = ULONG_MAX;
struct location doc_location;
bool in_cpp_docstring = FALSE;

/* initialize an accumulating documentation string */
static void init_c_docstring() 
{
  if(warn_unexpected_docstring  &&  char_array_length(docstring_array) != 0)
    warning("discarding unexpected docstring from %s:%ld.", doc_location.filename, doc_location.lineno);

  char_array_reset(docstring_array);
  doc_skip_prefix = FALSE;
  doc_location = input_file_stack->l;
  in_cpp_docstring = FALSE;
}

/* set up for a CPP style documentation string.  This allows for
   multiple comment lines to be strung together into a single comment */
static void init_cpp_docstring() 
{
  // FIXME
  //fprintf(stderr, "init_cpp_docstring()  %s:%ld\n", input_file_stack->l.filename, input_file_stack->l.lineno);

  if(!in_cpp_docstring  || 
     prev_cpp_docstring_line + 1  !=  input_file_stack->l.lineno) {
    if(warn_unexpected_docstring  &&  char_array_length(docstring_array) != 0)
      warning("discarding unexpected docstring from %s:%ld.", doc_location.filename, doc_location.lineno);

    char_array_reset(docstring_array);
  }
  prev_cpp_docstring_line = input_file_stack->l.lineno;
  doc_skip_prefix = FALSE;
  doc_location = input_file_stack->l;
  in_cpp_docstring = TRUE;
}


/* add chars to the a documentation comment string */
static void add_to_docstring(int c)
{
  /* skip the initial "   *" stuff after a newline */
  if(doc_skip_prefix) {
    if( c == ' ' || c == '\t' || c == '\r') 
      return;
    doc_skip_prefix = FALSE;
    if( c == '*' ) /* skip the first * char */
      return;
  }
  if(c == '\n')
    doc_skip_prefix = TRUE;
  
  /* make sure there is space, and then copy the new char */
  {
    char *p = char_array_extend(docstring_array, 1);
    *p = c;
  }
}

/* copy out the current docstring, if any */
void get_latest_docstring(char **short_s, char **long_s, struct location **loc)
{
  char *str;

  str = get_docstring();
  separate_short_docstring(str, short_s, long_s);

  *loc = ralloc(parse_region, struct location);
  **loc = doc_location;
}

void separate_short_docstring(char *str, char **short_s, char **long_s)
{
  char *dot, *at;
  assert(short_s != NULL);
  assert(long_s != NULL);
  
  if(str == NULL) {
    *short_s = NULL;
    *long_s = NULL;
    return;
  }

  /* find the first period, followed by whitespace, or the first '@', preceded by whitespace */
  dot = str;
  do {
    dot = strchr(dot,'.');
    if(dot == NULL) break;
    dot++;
  } while(*dot != '\0'  &&  *dot != ' '   &&  *dot != '\t'   &&  *dot != '\r'   &&  *dot != '\n');

  at = str-2;
  do {
    at = strchr(at+2,'@');
    if(at == NULL) break;
    at--;
    if(at < str) at++;
  } while(*at != ' '   &&  *at != '\t'   &&  *at != '\r'   &&  *at != '\n');

  if(at && at < dot) 
    dot = at;


  /* check for the beginning of the next sentance */
  if(dot != NULL) {
    dot += strspn(dot, " \t\n\r.");
    if( *dot == '\0' ) 
      dot = NULL;
  }

  /* short description only  */
  if(dot == NULL) {
    *short_s = str;
    *long_s = NULL;
  } 

  /* both short and long descriptions */
  else {
    *(dot - 1)= '\0';
    *short_s = rstrdup(parse_region, str);
    *(dot - 1)= ' ';
    *long_s = str;
  }
}

/**
 * Return the latest docstring, as a string
 **/
char *get_docstring() {
  size_t length = char_array_length(docstring_array);
  char *str;

  /* no doc string available */
  if( length <= 0 )
    return NULL;


  /* copy out the text, and reset docstring_array */
  str = rarrayalloc(parse_region, length + 1, char);
  memcpy(str, char_array_data(docstring_array), length * sizeof(char));
  str[length] = '\0';
  prev_cpp_docstring_line = ULONG_MAX;
  char_array_reset(docstring_array);
  in_cpp_docstring = FALSE;
  
  return str;
}



/* Skip / *-style comment. */
void skip_c_comment(void)
{
  int last_c = 0, c, depth = 1;
  struct location start = input_file_stack->l;
  bool docstring = FALSE;

  /* if the first char is '*', then this is a code documentation comment */
  c = lex_getc();
  if(c == '*') 
  {
    /* handle empty comments */
    char c1 = lex_getc();
    if(c1 == '/') return;
    else lex_ungetc(c1);

    docstring = TRUE;
    init_c_docstring();
  } 
  else 
  {
    lex_ungetc(c);
  }


  for (;;)
    {
      c = lex_getc();

      switch (c)
	{
	case EOF:
	  error_with_location(&start, "unterminated comment");
	  return;
	case '\n':
	  input_file_stack->l.lineno++;
	  break;
	case '/':
	  if (last_c == '*')
	    {
	      if (--depth == 0)
		return;
	    }
	  break;
	case '*':
#ifdef NESTED_COMMENTS
	  if (last_c == '/')
	    ++depth;
#endif
	  break;
	}

      /* add to the docstring, skipping the final "* /" sequence */
      if( docstring ) {
        if( last_c == '*' ) /* add any '*' that we skipped */
          add_to_docstring('*');

        if( c != '*' ) /* skip '/', in case its the beginning of the end... */
          add_to_docstring(c);
      }

      last_c = c;
    }
}




void skip_cpp_comment(void)
{
  int c;
  bool docstring = FALSE;

  /* if the first char is '/', then this is a code documentation comment */
  c = lex_getc();
  if(c == '/') {
    docstring = TRUE;
    init_cpp_docstring();
  } else {
    lex_ungetc(c);
  }

  for (;;)
    {
      c = lex_getc();

      switch (c)
	{
	case EOF:
	  return;
	case '\n':
	  lex_ungetc(check_newline());
	  return;
	}
      
      if(docstring)
        add_to_docstring( c );
    }
}

/* If C is not whitespace, return C.
   Otherwise skip whitespace and return first nonwhite char read.  */

static int
skip_white_space (c)
     int c;
{
  static int newline_warning = 0;
  int c1;

  for (;;)
    {
      switch (c)
	{
	case '/':
	  /* check for comments */
	  c1 = lex_getc();

	  if (c1 == '/')
	    skip_cpp_comment();
	  else if (c1 == '*')
	    skip_c_comment();
	  else
	    {
	      lex_ungetc(c1);
	      return c;
	    }
	  c = lex_getc();
	  break;

	case '\n':
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\v':
	case '\b':
	  c = lex_getc();
	  break;

	case '\r':
	  /* ANSI C says the effects of a carriage return in a source file
	     are undefined.  */
	  if (pedantic && !newline_warning)
	    {
	      warning ("carriage return in source file");
	      warning ("(we only warn about the first carriage return)");
	      newline_warning = 1;
	    }
	  c = lex_getc();
	  break;

	case '\\':
	  c = lex_getc();
	  if (c == '\n')
	    input_file_stack->l.lineno++;
	  else
	    error ("stray '\\' in program");
	  c = lex_getc();
	  break;

	default:
	  return (c);
	}
    }
}

/* Make the token buffer longer, preserving the data in it.
   P should point to just beyond the last valid character in the old buffer.
   The value we return is a pointer to the new buffer
   at a place corresponding to P.  */

static char *
extend_token_buffer (p)
     char *p;
{
  int offset = p - token_buffer;

  maxtoken = maxtoken * 2 + 10;
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);

  return token_buffer + offset;
}

static char *traditional token_ptr;
#define token_ungetc(c) (token_ptr--, lex_ungetc((c)))

static int token_getc(void)
{
  int c = lex_getc();

  if (c != EOF)
    {
      if (token_ptr == token_buffer + maxtoken)
	token_ptr = extend_token_buffer(token_ptr);
      *token_ptr++ = c;
    }
  return c;
}

static cstring make_token_cstring(void)
{
  return make_cstring(parse_region, token_buffer, token_ptr - token_buffer);
}


#define GET_DIRECTIVE_LINE() get_directive_line (input_file_stack->lex.finput)

/* Read the rest of a #-directive from input stream FINPUT.
   In normal use, the directive name and the white space after it
   have already been read, so they won't be included in the result.
   We allow for the fact that the directive line may contain
   a newline embedded within a character or string literal which forms
   a part of the directive.

   The value is a string in a reusable buffer.  It remains valid
   only until the next time this function is called.

   The terminating character ('\n' or EOF) is left in FINPUT for the
   caller to re-read.  */

char *
get_directive_line (finput)
     FILE *finput;
{
  static char *directive_buffer = NULL;
  static unsigned buffer_length = 0;
  char *p;
  char *buffer_limit;
  int looking_for = 0;
  int char_escaped = 0;

  if (buffer_length == 0)
    {
      directive_buffer = (char *)xmalloc (128);
      buffer_length = 128;
    }

  buffer_limit = &directive_buffer[buffer_length];

  for (p = directive_buffer; ; )
    {
      int c;

      /* Make buffer bigger if it is full.  */
      if (p >= buffer_limit)
        {
	  unsigned bytes_used = (p - directive_buffer);

	  buffer_length *= 2;
	  directive_buffer
	    = (char *)xrealloc (directive_buffer, buffer_length);
	  p = &directive_buffer[bytes_used];
	  buffer_limit = &directive_buffer[buffer_length];
        }

      c = getc (finput);

      /* Discard initial whitespace.  */
      if ((c == ' ' || c == '\t') && p == directive_buffer)
	continue;

      /* Detect the end of the directive.  */
      if (looking_for == 0
	  && (c == '\n' || c == EOF))
	{
          ungetc (c, finput);
	  c = '\0';
	}

      *p++ = c;

      if (c == 0)
	return directive_buffer;

      /* Handle string and character constant syntax.  */
      if (looking_for)
	{
	  if (looking_for == c && !char_escaped)
	    looking_for = 0;	/* Found terminator... stop looking.  */
	}
      else
        if (c == '\'' || c == '"')
	  looking_for = c;	/* Don't stop buffering until we see another
				   another one of these (or an EOF).  */

      /* Handle backslash.  */
      char_escaped = (c == '\\' && ! char_escaped);
    }
}

static char *parse_string_token(string_cst c)
{
  char *s = string_cst_to_c(parse_region, c);

  /* If we can't decode the string, we punt and use its lexical rep (minus
     the intrudoctory L", and final " -- we can only fail for wide strings) */
  if (!s)
    {
      s = rstralloc(parse_region, c->cstring.length - 2);
      s[c->cstring.length - 3] = '\0';
      memcpy(s, c->cstring.data + 2, c->cstring.length - 3);
    }

  return s;
}

static bool token_isint(int token, struct yystype *lvalp)
{
  return token == CONSTANT && type_integral(lvalp->u.constant->type);
}

static int token_intvalue(struct yystype *lvalp)
{
  return constant_sint_value(lvalp->u.constant->cst);
}

/* At the beginning of a line, increment the line number
   and process any #-directive on this line.
   If the line is a #-directive, read the entire line and return a newline.
   Otherwise, return the line's first non-whitespace character.  */

int
check_newline ()
{
  int c;
  int token;
  struct yystype lval;
  
  input_file_stack->l.lineno++;

  /* Read first nonwhite char on the line.  */

  c = lex_getc();
  while (c == ' ' || c == '\t')
    c = lex_getc();

  if (c != '#')
    {
      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Read first nonwhite char after the `#'.  */

  c = lex_getc();
  while (c == ' ' || c == '\t')
    c = lex_getc();

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma', `ident', `define', or `undef'.  */

  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    {
      if (c == 'p')
	{
	  if (lex_getc() == 'r'
	      && lex_getc() == 'a'
	      && lex_getc() == 'g'
	      && lex_getc() == 'm'
	      && lex_getc() == 'a'
	      && ((c = lex_getc()) == ' ' || c == '\t' || c == '\n'))
	    {
	      return save_directive("pragma");
	    }
	}

      else if (c == 'd')
	{
	  if (lex_getc() == 'e'
	      && lex_getc() == 'f'
	      && lex_getc() == 'i'
	      && lex_getc() == 'n'
	      && lex_getc() == 'e'
	      && ((c = lex_getc()) == ' ' || c == '\t' || c == '\n'))
	    {
	      return save_directive("define");
	    }
	}
      else if (c == 'u')
	{
	  if (lex_getc() == 'n'
	      && lex_getc() == 'd'
	      && lex_getc() == 'e'
	      && lex_getc() == 'f'
	      && ((c = lex_getc()) == ' ' || c == '\t' || c == '\n'))
	    {
	      return save_directive("undef");
	    }
	}
      else if (c == 'l')
	{
	  if (lex_getc() == 'i'
	      && lex_getc() == 'n'
	      && lex_getc() == 'e'
	      && ((c = lex_getc()) == ' ' || c == '\t'))
	    goto linenum;
	}
      else if (c == 'i')
	{
	  if (lex_getc() == 'd'
	      && lex_getc() == 'e'
	      && lex_getc() == 'n'
	      && lex_getc() == 't'
	      && ((c = lex_getc()) == ' ' || c == '\t'))
	    {
	      /* #ident.  The pedantic warning is now in cccp.c.  */

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = lex_getc();

	      /* If no argument, ignore the line.  */
	      if (c == '\n')
		return c;

	      lex_ungetc (c);
	      token = yylex (&lval);
	      if (token != STRING)
		{
		  error ("invalid #ident");
		  goto skipline;
		}

	      /* Skip the rest of this line.  */
	      goto skipline;
	    }
	}

      error ("undefined or invalid # directive");
      goto skipline;
    }

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = lex_getc();

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == '\n')
    return c;

  /* Something follows the #; read a token.  */

  lex_ungetc (c);
  token = yylex (&lval);

  if (token_isint(token, &lval))
    {
      const char *new_filename;
      int used_up = 0;
      /* subtract one, because it is the following line that
	 gets the specified number */
      int l = token_intvalue(&lval) - 1;

      /* Is this the last nonwhite stuff on the line?  */
      c = lex_getc();
      while (c == ' ' || c == '\t')
	c = lex_getc();
      if (c == '\n')
	{
	  /* No more: store the line number and check following line.  */
	  input_file_stack->l.lineno = l;
	  return c;
	}
      lex_ungetc (c);

      /* More follows: it must be a string constant (filename).  */

      /* Read the string constant.  */
      token = yylex (&lval);

      if (token != STRING)
	{
	  error ("invalid #line");
	  goto skipline;
	}

      new_filename = parse_string_token(CAST(string_cst, lval.u.constant));
      /* Each change of file name
	 reinitializes whether we are now in a system header.  */
      input_file_stack->l.in_system_header = 0;

      /* Is this the last nonwhite stuff on the line?  */
      c = lex_getc();
      while (c == ' ' || c == '\t')
	c = lex_getc();
      if (c == '\n')
	{
	  input_file_stack->l.filename = new_filename;
	  input_file_stack->l.lineno = l;
	  return c;
	}

      lex_ungetc (c);

      token = yylex (&lval);
      used_up = 0;

      /* `1' after file name means entering new file.
	 `2' after file name means just left a file.  */

      if (token_isint(token, &lval))
	{
	  int cst = token_intvalue(&lval);

	  if (cst == 1)
	    {
	      push_input();
	      used_up = 1;
	    }
	  else if (cst == 2)
	    {
	      /* Popping out of a file.  */
	      if (input_file_stack->next &&
		  input_file_stack->lex.finput == input_file_stack->next->lex.finput)
		{
		  int current_il = input_file_stack->lex.indent_level;
		  int previous_il = input_file_stack->next->lex.indent_level;
		  if (current_il != previous_il)
		    {
		      warning_with_location
			(&input_file_stack->l,
			 "This file contains more `%c's than `%c's.",
			 current_il > previous_il ? '{' : '}',
			 current_il > previous_il ? '}' : '{');
		    }
		  pop_input();
		}
	      else
		error ("#-lines for entering and leaving files don't match");

	      used_up = 1;
	    }
	}

      input_file_stack->l.filename = new_filename;
      input_file_stack->l.lineno = l;

      /* `3' after file name or 1, 2 means this is a system header file.  */

      if (token_isint(token, &lval) && token_intvalue(&lval) == 3)
	input_file_stack->l.in_system_header = 1;

      /* We just ignore the rest of the line.
	 (we could complain about extra stuff, but, e.g., gcc 3.2 adds an
	 extra flag after 3 (if 4 means "needs to be extern "C" protected -
	 I'm ignoring this as it shouldn't be relevant to C stuff)) */
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  if (c != '\n' && c != EOF && input_file_stack->lex.nextchar >= 0)
    c = input_file_stack->lex.nextchar, input_file_stack->lex.nextchar = -1;
  while (c != '\n' && c != EOF)
    c = lex_getc();
  return c;
}

#define ENDFILE -1  /* token that represents end-of-file */

/* Read an escape sequence, saving it in the token_buffer.
   Return the escape sequence's value.
   store 1 in *ignore_ptr if escape sequence is backslash-newline.  */

static int
readescape (int *ignore_ptr)
{
  int c = token_getc();
  int code;
  unsigned count;
  unsigned firstdig = 0;
  int nonnull;

  switch (c)
    {
    case 'x':
      if (warn_traditional)
	warning ("the meaning of `\\x' varies with -traditional");

      if (flag_traditional)
	return c;

      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = token_getc();
	  if (!(c >= 'a' && c <= 'f')
	      && !(c >= 'A' && c <= 'F')
	      && !(c >= '0' && c <= '9'))
	    {
	      token_ungetc (c);
	      break;
	    }

	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if (count - 1 >= int_type_size * 2
	       || (count > 1
		   && ((1 << (int_type_size * 8 - (count - 1) * 4))
		       <= firstdig)))
	pedwarn ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = token_getc();
	}
      token_ungetc (c);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      input_file_stack->l.lineno++;
      *ignore_ptr = 1;
      return 0;

      /* SAME */
    case 'n': return '\n';
    case 't': return '\t';
    case 'r': return '\r';
    case 'f': return '\f';
    case 'b': return '\b';
    case '?': return '\?';

    case 'a':
      if (warn_traditional)
	warning ("the meaning of `\\a' varies with -traditional");
      if (flag_traditional)
	return c;
      return '\a'; /* SAME */

    case 'v':
#if 0 /* Vertical tab is present in common usage compilers.  */
      if (flag_traditional)
	return c;
#endif
      return '\v'; /* SAME */

    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return 033;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      /* `\%' is used to prevent SCCS from getting confused.  */
    case '%':
      if (pedantic)
	pedwarn ("non-ANSI escape sequence `\\%c'", c);
      return c;

    default:
      if (c >= 040 && c < 0177)
	pedwarn ("unknown escape sequence `\\%c'", c);
      else
	pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
      return c;
    }
}

static int read_char(char *context, char terminating_char,
		     char *cp, wchar_t *wcp)
{
  unsigned width = wcp ? type_size_int(wchar_type) * BITSPERBYTE
    : BITSPERBYTE; /* sizeof(char) == 1 */
  int c;
#ifdef MULTIBYTE_CHARS
  (void) local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

 tryagain:
  c = token_getc ();

  if (c == terminating_char)
    return 0;

  if (c < 0)
    return -1;

  if (c == '\\')
    {
      int ignore = 0;
      c = readescape (&ignore);
      if (ignore)
	goto tryagain;

      if (width < sizeof(unsigned) * BITSPERBYTE
	  && (unsigned) c >= ((unsigned)1 << width))
	pedwarn ("escape sequence out of range for character");
    }
  else if (c == '\n')
    {
      if (pedantic)
	pedwarn ("ANSI C forbids newline in %s constant", context);
      input_file_stack->l.lineno++;
    }
  else
    {
#ifdef MULTIBYTE_CHARS
      wchar_t wc;
      int i;
      int char_len = -1;
      for (i = 0; i < max_char_length; ++i)
	{
	  cp[i] = c;

	  char_len = local_mbtowc (& wc, cp, i + 1);
	  if (char_len != -1)
	    break;
	  c = token_getc ();
	}
      if (char_len == -1)
	{
	  warning ("Ignoring invalid multibyte character");
	  /* Note: gcc just takes the character following the
	     invalid multibyte-char-sequence as being the next 
	     character. This is obviously incorrect. */
	  token_ungetc (c);
	  goto tryagain;
	}
      else
	{
	  /* mbtowc sometimes needs an extra char before accepting */
	  if (char_len <= i)
	    token_ungetc (c);
	  if (! wcp)
	    return i + 1;
	  else
	    {
	      *wcp = wc;
	      return 1;
	    }
	}
#endif /* MULTIBYTE_CHARS */
    }

  if (wcp)
    *wcp = c;
  else
    *cp = c;

  return 1;
}


void
yyerror (char *string)
{
  char buf[200];

  strcpy (buf, string);

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  /* XXX: This is not true anymore, but ignore that for a while */
  if (input_file_stack->lex.end_of_file)
    strcat (buf, " at end of input");
  else if (token_buffer[0] == 0)
    strcat (buf, " at null character");
  else if (token_buffer[0] == '"')
    strcat (buf, " before string constant");
  else if (token_buffer[0] == '\'')
    strcat (buf, " before character constant");
  else if (token_buffer[0] < 040 || (unsigned char) token_buffer[0] >= 0177)
    sprintf (buf + strlen (buf), " before character 0%o",
	     (unsigned char) token_buffer[0]);
  else
    strcat (buf, " before `%s'");

  error (buf, token_buffer);
}


int
yylex(struct yystype *lvalp)
{
  int c;
  int value;
  int wide_flag = 0;

  /* Grammar selection hack */
  if (language_token != -1)
    {
      int token = language_token;
      language_token = -1;
      return token;
    }

  if (input_file_stack->lex.nextchar >= 0)
    c = input_file_stack->lex.nextchar, input_file_stack->lex.nextchar = -1;
  else
    c = lex_getc();

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      case '\b':
	c = lex_getc();
	break;

      case '\r':
	/* Call skip_white_space so we can warn if appropriate.  */

      case '\n':
      case '/':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_ptr = token_buffer;
  *token_ptr++ = c;

  last_location = make_location(input_file_stack->l);

  lvalp->u.itoken.location = last_location;
  lvalp->u.itoken.i = 0;

  switch (c)
    {
    case EOF:
      input_file_stack->lex.end_of_file = 1;
      token_buffer[0] = 0;
      value = ENDFILE;
      break;

    case 'L':
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	int c = token_getc();
	if (c == '\'')
	  {
	    wide_flag = 1;
	    goto char_constant;
	  }
	if (c == '"')
	  {
	    wide_flag = 1;
	    goto string_constant;
	  }
	token_ungetc (c);
      }
      goto letter;

    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':		  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    case '$':
    letter:
      while (isalnum (c) || c == '_' || c == '$')
	{
	  /* Make sure this char really belongs in an identifier.  */
	  if (c == '$')
	    {
	      if (! dollars_in_ident)
		error ("`$' in identifier");
	      else if (pedantic)
		pedwarn ("`$' in identifier");
	    }

	  c = token_getc();
	}

      *token_ptr = 0;
      token_ptr--;
      input_file_stack->lex.nextchar = c;

      value = IDENTIFIER;

      /* Try to recognize a keyword.  Uses minimum-perfect hash function */
      {
	struct resword *ptr;

	*token_ptr = '\0';
	if ((ptr = is_reserved_word (token_buffer, token_ptr - token_buffer)))
	  {
	    value = (int) ptr->token;
	    lvalp->u.itoken.i = (int) ptr->rid;

	    /* Even if we decided to recognize asm, still perhaps warn.  */
	    if (pedantic
		&& (value == ASM_KEYWORD || value == TYPEOF
		    || ptr->rid == RID_INLINE)
		&& token_buffer[0] != '_')
	      pedwarn ("ANSI does not permit the keyword `%s'",
		       token_buffer);
	  }
      }

      /* If we did not find a keyword, look for an identifier
	 (or a typename).  */
      if (value == IDENTIFIER)
	{
	  lvalp->idtoken.location = last_location;
	  lvalp->idtoken.id = make_token_cstring();
	  lvalp->idtoken.decl = lookup_id(lvalp->idtoken.id.data, FALSE);

	  if (lvalp->idtoken.decl)
	    {
	      if (lvalp->idtoken.decl->kind == decl_typedef)
		value = TYPENAME;
	      else if (lvalp->idtoken.decl->kind == decl_magic_string)
		value = MAGIC_STRING;
	    }
	}

      break;

    case '0':  case '1': case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    case '.':
      {
	int base = 10;
	int largest_digit = 0;
	int numdigits = 0;
	largest_uint cstvalue = 0, maxbase;
	int overflow = 0;

	enum anon1 { NOT_FLOAT, AFTER_POINT, TOO_MANY_POINTS} floatflag
	  = NOT_FLOAT;

	if (c == '0')
	  {
	    c = token_getc();
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		c = token_getc();
	      }
	    /* Leading 0 forces octal unless the 0 is the only digit.  */
	    else if (c >= '0' && c <= '9')
	      {
		base = 8;
		numdigits++;
	      }
	    else
	      numdigits++;
	  }

	/* Read all the digits-and-decimal-points.  */
	/* Find the maximum value that can be multiplied by base without
	   overflowing a largest_uint, i.e., (1 << LARGEST_UINTBITS) / base */
	maxbase = (((largest_uint)1 << (LARGEST_UINTBITS - 1)) / base) << 1;
	maxbase += ((((largest_uint)1 << (LARGEST_UINTBITS - 1)) % base) << 1) / base;

	while (c == '.'
	       || (isalnum (c) && c != 'l' && c != 'L'
		   && c != 'u' && c != 'U'
		   && c != 'i' && c != 'I' && c != 'j' && c != 'J'
		   && (floatflag == NOT_FLOAT || ((c != 'f') && (c != 'F')))))
	  {
	    if (c == '.')
	      {
		if (base == 16)
		  error ("floating constant may not be in radix 16");
		if (floatflag == TOO_MANY_POINTS)
		  /* We have already emitted an error.  Don't need another.  */
		  ;
		else if (floatflag == AFTER_POINT)
		  {
		    error ("malformed floating constant");
		    floatflag = TOO_MANY_POINTS;
		  }
		else
		  floatflag = AFTER_POINT;

		base = 10;
		c = token_getc();
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.
		   Otherwise, unread the following non-digit
		   and use the '.' as a structural token.  */
		if (token_ptr == token_buffer + 2 && !isdigit (c))
		  {
		    if (c == '.')
		      {
			c = token_getc();
			if (c == '.')
			  {
			    value = ELLIPSIS;
			    goto done;
			  }
			error ("parse error at `..'");
		      }
		    token_ungetc (c);
		    value = '.';
		    goto done;
		  }
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (isdigit (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if (c == 'e' || c == 'E')
		      {
			base = 10;
			floatflag = AFTER_POINT;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		/* Keep track of constants up to largest_uint's range (unsigned) */
		if (cstvalue > maxbase)
		  overflow = 1;
		cstvalue = cstvalue * base;
		if ((largest_uint)-1 - cstvalue < c)
		  overflow = 1;
		cstvalue += c;

		c = token_getc();
	      }
	  }

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	if (floatflag != NOT_FLOAT)
	  {
	    type ftype;
	    int imag = 0;
	    int fflag = 0, lflag = 0;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((c == 'e') || (c == 'E'))
	      {
		c = token_getc();
		if ((c == '+') || (c == '-'))
		  {
		    c = token_getc();
		  }
		if (! isdigit (c))
		  error ("floating constant exponent has no digits");
	        while (isdigit (c))
		  {
		    c = token_getc();
		  }
	      }

	    while (1)
	      {
		int lose = 0;

		/* Read the suffixes to choose a data type.  */
		switch (c)
		  {
		  case 'f': case 'F':
		    if (fflag)
		      error ("more than one `f' in numeric constant");
		    fflag = 1;
		    break;

		  case 'l': case 'L':
		    if (lflag)
		      error ("more than one `l' in numeric constant");
		    lflag = 1;
		    break;

		  case 'i': case 'I':
		    if (imag)
		      error ("more than one `i' or `j' in numeric constant");
		    else if (pedantic)
		      pedwarn ("ANSI C forbids imaginary numeric constants");
		    imag = 1;
		    break;

		  default:
		    lose = 1;
		  }

		if (lose)
		  break;

		c = token_getc();
	      }

	    token_ungetc(c);

	    if (fflag)
	      {
		if (lflag)
		  error ("both `f' and `l' in floating constant");

		ftype = float_type;
	      }
	    else if (lflag)
	      ftype = long_double_type;
	    else
	      ftype = double_type;

	    if (imag)
	      ftype = make_complex_type(ftype);

	    lvalp->u.constant = fold_lexical_real(ftype, last_location, make_token_cstring());
	  }
	else
	  {
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int spec_imag = 0;
	    type itype;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      {
			if (spec_long_long)
			  error ("three `l's in integer constant");
			else if (pedantic)
			  pedwarn ("ANSI C forbids long long integer constants");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else if (c == 'i' || c == 'j' || c == 'I' || c == 'J')
		  {
		    if (spec_imag)
		      error ("more than one `i' or `j' in numeric constant");
		    else if (pedantic)
		      pedwarn ("ANSI C forbids imaginary numeric constants");
		    spec_imag = 1;
		  }
		else
		  break;
		c = token_getc();
	      }

	    token_ungetc (c);

	    /* Collect type as specified in the lexeme. The constant folder
	       will expand the type if necessary. */
	    if (base != 10)
	      spec_unsigned = 1;

	    if (spec_long_long)
	      itype = spec_unsigned ? unsigned_long_long_type : long_long_type;
	    else if (spec_long)
	      itype = spec_unsigned ? unsigned_long_type : long_type;
	    else
	      itype = spec_unsigned ? unsigned_int_type : int_type;

	    lvalp->u.constant =
	      fold_lexical_int(itype, last_location, make_token_cstring(),
			       spec_imag, cstvalue, overflow);
	  }

	if (isalnum (c) || c == '.' || c == '_' || c == '$'
	    || (!flag_traditional && (c == '-' || c == '+')
		&& (token_ptr[-1] == 'e' || token_ptr[-1] == 'E')))
	  error ("missing white space after number `%s'", token_buffer);

	value = CONSTANT; break;
      }

    case '\'':
    char_constant:
      {
	wchar_t wc;
	char *cbuf = alloca(max_char_length);
	int chars_seen = 0, count, result = 0;
	unsigned width = wide_flag ? type_size_int(wchar_type) * BITSPERBYTE
	  : BITSPERBYTE; /* sizeof(char) == 1 */

	for (;;)
	  {
	    count = read_char("character", '\'', cbuf, wide_flag ? &wc : NULL);
	    if (count <= 0)
	      break;

	    chars_seen += count;

	    if (wide_flag)
	      {
		/* Note: read_char always returns 1 when wide_flag is true,
		   so chars_seen will always be 1 exactly once */
		assert(count == 1);
		if (chars_seen == 1)
		  result = wc;
	      }
	    else
	      {
		int i;

		/* Weird code if you ask me. But this is what gcc 2.95.3 does.
		   There's no particular consistency between compilers
		   on how to handle these (e.g., 'ab' gives different values
		   with Sun's cc and gcc). */
		for (i = 0; i < count && i < int_type_size; i++)
		  if (width < sizeof(unsigned) * BITSPERBYTE)
		    result = (result << width) | (cbuf[i] & ((1 << width) - 1));
		  else
		    result = cbuf[i];
	      }
	  }

	if (count < 0)
	  error ("malformatted character constant");
	else if (chars_seen == 0)
	  error ("empty character constant");
	else if (chars_seen > int_type_size) /* this is what gcc is testing */
	  error ("character constant too long");
	else if (chars_seen != 1 && ! flag_traditional && warn_multichar)
	  warning ("multi-character character constant");

	lvalp->u.constant = fold_lexical_char(last_location, make_token_cstring(),
					      wide_flag, result);

	value = CONSTANT;
	break;
      }

    case '"':
    string_constant:
      {
	wchar_t wc;
	char *cbuf = alloca(max_char_length);
	int count;

	wchar_array_reset(string_array);

	for (;;)
	  {
	    wchar_t *p;

	    count = read_char("string", '"', cbuf, wide_flag ? &wc : NULL);
	    if (count <= 0)
	      break;

	    p = wchar_array_extend(string_array, count);
	    if (wide_flag)
	      {
		assert(count == 1);
		*p = wc;
	      }
	    else
	      {
		int i;
		
		for (i = 0; i < count; i++)
		  *p++ = cbuf[i];
	      }
	  }
	if (count < 0)
	  error ("Unterminated string constant");

	lvalp->u.string_cst = fold_lexical_string(last_location, make_token_cstring(),
						  wide_flag, string_array);

	value = STRING;

	break;
      }

    case '@': 
      value = '*';
      break;

    case '+':
    case '-':
    case '&':
    case '|':
    case ':':
    case '<':
    case '>':
    case '*':
    case '/':
    case '%':
    case '^':
    case '!':
    case '=':
      {
	int c1;

      combine:

	c1 = token_getc();

	if (c1 == '=')
	  {
	    value = ASSIGN;
	    switch (c)
	      {
	      case '<':
		value = ARITHCOMPARE; lvalp->u.itoken.i = kind_leq; break;
	      case '>':
		value = ARITHCOMPARE; lvalp->u.itoken.i = kind_geq; break;
	      case '!':
		value = EQCOMPARE; lvalp->u.itoken.i = kind_ne; break;
	      case '=':
		value = EQCOMPARE; lvalp->u.itoken.i = kind_eq; break;

	      case '+':
		lvalp->u.itoken.i = kind_plus_assign; break;
	      case '-':
		lvalp->u.itoken.i = kind_minus_assign; break;
	      case '&':
		lvalp->u.itoken.i = kind_bitand_assign; break;
	      case '|':
		lvalp->u.itoken.i = kind_bitor_assign; break;
	      case '*':
		lvalp->u.itoken.i = kind_times_assign; break;
	      case '/':
		lvalp->u.itoken.i = kind_divide_assign; break;
	      case '%':
		lvalp->u.itoken.i = kind_modulo_assign; break;
	      case '^':
		lvalp->u.itoken.i = kind_bitxor_assign; break;
	      case LSHIFT:
		lvalp->u.itoken.i = kind_lshift_assign; break;
	      case RSHIFT:
		lvalp->u.itoken.i = kind_rshift_assign; break;
	      }
	    goto done;
	  }
	else if (c == c1)
	  switch (c)
	    {
	    case '+':
	      value = PLUSPLUS; goto done;
	    case '-':
	      value = MINUSMINUS; goto done;
	    case '&':
	      value = ANDAND; goto done;
	    case '|':
	      value = OROR; goto done;
	    case '<':
	      c = LSHIFT;
	      goto combine;
	    case '>':
	      c = RSHIFT;
	      goto combine;
	    }
	else
	  switch (c)
	    {
	    case '-':
	      if (c1 == '>')
		{ value = POINTSAT; goto done; }
	      break;
	    case ':':
	      if (c1 == '>')
		{ value = ']'; goto done; }
	      break;
	    case '<':
	      if (c1 == '%')
		{ value = '{'; input_file_stack->lex.indent_level++; goto done; }
	      if (c1 == ':')
		{ value = '['; goto done; }
	      break;
	    case '%':
	      if (c1 == '>')
		{ value = '}'; input_file_stack->lex.indent_level--; goto done; }
	      break;
	    }
	token_ungetc (c1);

	if (c == '<') 
	  {
	    c1 = token_getc();

	    if (c1 == '-')
	      value = TASTNIOP;
	    else
	      {
		token_ungetc (c1);
		value = ARITHCOMPARE;
		lvalp->u.itoken.i = kind_lt;
	      }
	  }
	else if (c == '>')
	  {
	    value = ARITHCOMPARE;	
	    lvalp->u.itoken.i = kind_gt;
	  }
	else
	  value = c;
	goto done;
      }

    case 0:
      /* Don't make yyparse think this is eof.  */
      value = 1;
      break;

    case '{':
      input_file_stack->lex.indent_level++;
      value = c;
      break;

    case '}':
      input_file_stack->lex.indent_level--;
      value = c;
      break;

    default:
      value = c;
    }

done:
  *token_ptr = 0;

  return value;
}

/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */

void
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}
