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
#include "c-lex-int.h"
#include "c-parse.h"
#include "c-parse.tab.h"
#include "semantics.h"
#include "constants.h"
#include "nesc-cpp.h"
#include "machine.h"

/* libcpp */
#include "system.h"
#include "config.h"
#include "cpplib.h"
#include "line-map.h"
#undef bool
#define TRUE 1
#define FALSE 0

static char_array doc_string;
static location doc_location;

DECLARE_ARRAY(string_array, cpp_string)
DEFINE_ARRAY(string_array, cpp_string)
static string_array string_sequence;

/* Last token read. Used in syntax error reports. */
static const cpp_token *last_token;

location dummy_location, toplevel_location;

/* Location cache handling */
static location last_allocated_location;

location make_location(struct location l)
{
  if (l.lineno == last_allocated_location->lineno &&
      l.filename == last_allocated_location->filename &&
      l.container == last_allocated_location->container &&
      l.in_system_header == last_allocated_location->in_system_header)
    return last_allocated_location;

  last_allocated_location = ralloc(parse_region, struct location);
  *last_allocated_location = l;

  return last_allocated_location;
}

location new_location(const char *filename, int lineno)
{
  location l = ralloc(parse_region, struct location);

  l->filename = filename;
  l->lineno = lineno;
  l->in_system_header = FALSE;

  return l;
}

static location last_location(void)
{
  return dummy_location;//make_location(current.lex.input->l);
}

/* Do not insert generated code into the source, instead, include it.
   This allows us to build gcc automatically even for targets that
   need to add or modify the reserved keyword lists.  */
#include "c-gperf.h"

void init_lex(void)
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

  string_sequence = new_string_array(parse_region, 16);
  doc_string = new_char_array(parse_region, 2048);

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

static cpp_reader *current_reader(void)
{
  return current.lex.finput;
}

bool start_lex(source_language l, const char *path)
{
  cpp_options *cpp_opts;

  switch (l)
    {
    case l_interface: case l_component:
      current.lex.token_s1 = DISPATCH_NESC;
      break;
    case l_c:
      current.lex.token_s1 = DISPATCH_C;
      break;
    default:
      assert(0); 
      break;
    }
  current.lex.token_s2 = -1;

  current.lex.line_map = ralloc(current.fileregion, struct line_maps);
  linemap_init(current.lex.line_map);
  current.lex.finput = cpp_create_reader(CLK_GNUC89, NULL, current.lex.line_map);
  cpp_opts = cpp_get_options(current_reader());
  cpp_opts->discard_comments = 0;
  cpp_opts->dollars_in_ident = 0;
  cpp_opts->warn_long_long = 0;
  cpp_opts->wchar_precision = CHAR_BIT * target->wchar_t_size;
  cpp_opts->int_precision = CHAR_BIT * target->tint.size;
  cpp_opts->precision = CHAR_BIT * target->tlong.size;
  cpp_opts->unsigned_char = !target->char_signed;
  cpp_opts->unsigned_wchar = !target->wchar_t_signed;
  cpp_opts->cplusplus_comments = 1;
  //cpp_opts->extended_numbers = ?;
  cpp_opts->bytes_big_endian = target->big_endian;
  cpp_init_iconv(current_reader());

  return cpp_read_main_file(current.lex.finput, path) != NULL;
}

void end_lex(void)
{
  errorcount += cpp_finish(current.lex.finput, NULL);
}

static cstring make_token_cstring(const cpp_token *token)
{
  unsigned int len = cpp_token_len(token) + 1;
  cstring tokcs = alloc_cstring(parse_region, len);
  unsigned char *end;

  end = cpp_spell_token(current_reader(), token,
			(unsigned char *)tokcs.data, FALSE);
  end[0] = '\0';
  tokcs.length = (char *)end - tokcs.data;

  return tokcs;
}

static void handle_comment(const cpp_token *token)
{
  const cpp_string *comment = &token->val.str;
  bool new_docstring = FALSE;
  
  if (!strncmp((char *)comment->text, "/**", 3))
    new_docstring = TRUE;
  else if (!strncmp((char *)comment->text, "///", 3))
    {
      if (doc_location && last_location()->filename == doc_location->filename &&
	  last_location()->lineno + 1 == doc_location->lineno)
	memcpy(char_array_extend(doc_string, comment->len), comment->text,
	       comment->len);
      else
	new_docstring = TRUE;
    }

  if (new_docstring)
    {
      if (warn_unexpected_docstring && char_array_length(doc_string))
	warning_with_location(doc_location, "discarding unexpected docstring");

      char_array_reset(doc_string);
      memcpy(char_array_extend(doc_string, comment->len), comment->text,
	     comment->len);
      doc_location = last_location();
    }
}

void get_latest_docstring(char **short_s, char **long_s, struct location **loc)
{
  *short_s = *long_s = NULL;
  *loc = NULL;
}

/* Convert a series of STRING and/or WSTRING tokens into a string,
   performing string constant concatenation.  TOK is the first of
   these.  VALP is the location to write the string into.  

   This is unfortunately more work than it should be.  If any of the
   strings in the series has an L prefix, the result is a wide string
   (6.4.5p4).  Whether or not the result is a wide string affects the
   meaning of octal and hexadecimal escapes (6.4.4.4p6,9).  But escape
   sequences do not continue across the boundary between two strings in
   a series (6.4.5p7), so we must not lose the boundaries.  Therefore
   cpp_interpret_string takes a vector of cpp_string structures, which
   we must arrange to provide.  */

static void lex_string(const cpp_token *tok, struct yystype *lvalp)
{
  bool wide = FALSE;
  location first_loc = last_location();
  cpp_string istr;
  cstring cstr;
  string_cst string_components = NULL, *next_sc = &string_components;

  /* Collect all consecutive string tokens */
  string_array_reset(string_sequence);
  do
    {
      string_cst one_string =
	new_string_cst(parse_region, last_location(), make_token_cstring(tok));

      *next_sc = one_string;
      next_sc = CASTPTR(string_cst, &one_string->next);

      *string_array_extend(string_sequence, 1) = tok->val.str;

      if (tok->type == CPP_WSTRING)
	wide = true;

      do
	tok = cpp_get_token(current_reader());
      while (tok->type == CPP_PADDING);
    }
  while (tok->type == CPP_STRING || tok->type == CPP_WSTRING);

  /* We have read one more token than we want.  */
  _cpp_backup_tokens(current_reader(), 1);

  if (cpp_interpret_string(current_reader(),
			   string_array_data(string_sequence),
			   string_array_length(string_sequence),
			   &istr, wide))
    {
      cstr = make_cstring(parse_region, (char *)istr.text, istr.len);
      free((char *)istr.text);
    }
  else
    {
      /* Use empty string as the value in case of error. Assumes the
	 widest supported wchar_t is 32 bits */
      cstr = make_cstring(parse_region, "\0\0\0",
			  wide ? type_size_int(wchar_type) : 1);
    }

  lvalp->u.string = fold_lexical_string(first_loc, string_components, cstr, wide);
}

static void lex_charconst(const cpp_token *token, struct yystype *lvalp)
{
  cppchar_t result;
  unsigned int chars_seen;
  int unsignedp;

  result = cpp_interpret_charconst(current_reader(), token,
				   &chars_seen, &unsignedp);
  lvalp->u.constant = fold_lexical_char(last_location(), make_token_cstring(token),
					token->type == CPP_WCHAR, result);
}

/* Interpret TOKEN, an integer with FLAGS as classified by cpplib.  */
static lexical_cst interpret_integer(const cpp_token *token, unsigned int flags)
{
  type t;
  cpp_num integer;
  cpp_options *options = cpp_get_options(current_reader());

  integer = cpp_interpret_integer(current_reader(), token, flags);
  integer = cpp_num_sign_extend(integer, options->precision);

  if (flags & CPP_N_UNSIGNED)
    if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
      t = unsigned_int_type;
    else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
      t = unsigned_long_type;
    else
      t = unsigned_long_long_type;
  else
    if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
      t = int_type;
    else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
      t = long_type;
    else
      t = long_long_type;

  /* We're assuming that the our largest int type is the same as cpp's
     HOST_WIDE_INT, and then ignoring the high field as we only handle
     values that fit in largest_uint anyway */
  assert(sizeof(HOST_WIDE_INT) == sizeof(largest_uint));
  return fold_lexical_int(t, last_location(), make_token_cstring(token),
			  (flags & CPP_N_IMAGINARY) != 0,
			  integer.low,
			  integer.overflow);
}

/* Interpret TOKEN, a floating point number with FLAGS as classified
   by cpplib.  This is a very hacky, partial implementation. */
static lexical_cst interpret_float(const cpp_token *token, unsigned int flags)
{
  type t;

  /* Give up on _Fract and _Accum.  */
  if (flags & CPP_N_FRACT || flags & CPP_N_ACCUM)
    return NULL;
  /* Give up on decimal and machine dependent values */
  if (flags & CPP_N_DFLOAT || flags & CPP_N_WIDTH_MD)
    return NULL;

  /* Decode type based on width and properties. */
  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
    t = long_double_type;
  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL/* || flag_single_precision_constant*/)
    t = float_type;
  else
    t = double_type;

  if (flags & CPP_N_IMAGINARY)
    t = make_complex_type(t);

  return fold_lexical_real(t, last_location(), make_token_cstring(token));
}

static int interpret_name(const cpp_token *token, struct yystype *lvalp)
{
  ht_identifier *id = HT_NODE(token->val.node);
  struct resword *ptr;
  data_declaration decl;
  int kind = IDENTIFIER;

  /* Try to recognize a keyword.  Uses minimum-perfect hash function */
  if ((ptr = is_reserved_word(id->str, id->len)))
    {
      lvalp->u.itoken.i = ptr->rid;

      /* Even if we decided to recognize asm, still perhaps warn.  */
      if (pedantic &&
	  (ptr->token == ASM_KEYWORD || ptr->token == TYPEOF ||
	   ptr->rid == RID_INLINE) &&
	  id->str[0] != '_')
	pedwarn ("ANSI does not permit the keyword `%s'", id->str);

      return ptr->token;
    }

  /* If we did not find a keyword, look for a typename  */
  if (target->token)
    kind = target->token((char *)id->str, id->len, lvalp);

  if (kind == IDENTIFIER)
    {
      lvalp->idtoken.location = last_location();
      lvalp->idtoken.id = make_token_cstring(token);
      decl = lookup_id(lvalp->idtoken.id.data, FALSE);
      lvalp->idtoken.decl = decl;

      if (decl)
	switch (decl->kind)
	  {
	  case decl_typedef: kind = TYPENAME; break;
	  case decl_magic_string: kind = MAGIC_STRING; break;
	  case decl_component_ref: kind = COMPONENTREF; break;
	  default: break;
	  }
    }
  return kind;
}

static int lex_token(struct yystype *lvalp)
{
  const cpp_token *tok;
  enum cpp_ttype type;

 retry:
  last_token = tok = cpp_get_token(current_reader());
  type = tok->type;
  lvalp->u.itoken.location = last_location();
  lvalp->u.itoken.i = 0;

  switch (type)
    {
    case CPP_EOF:
      return -1;

    case CPP_PADDING:
      goto retry;

    case CPP_COMMENT:
      handle_comment(tok);
      goto retry;

    case CPP_NAME:
      return interpret_name(tok, lvalp);

    case CPP_NUMBER:
      {
	unsigned int flags = cpp_classify_number(current_reader(), tok);
	lexical_cst num = NULL;

	switch (flags & CPP_N_CATEGORY)
	  {
	  case CPP_N_INTEGER:
	    num = interpret_integer(tok, flags);
	    break;
	  case CPP_N_FLOATING:
	    num = interpret_float (tok, flags);
	    break;
	  }
	/* cpplib has issued an error or we ran into something we don't
	   support (e.g. fixed point), pretend the constant was 0  */
	if (num == NULL)
	    num = fold_lexical_int(int_type, last_location(),
				   make_token_cstring(tok),
				   FALSE, 0, FALSE);

	lvalp->u.constant = num;
	return CONSTANT;
      }

    case CPP_HASH:
    case CPP_PASTE:
    case CPP_SCOPE:
    case CPP_DEREF_STAR:
    case CPP_DOT_STAR:
      {
	unsigned char name[4];

	*cpp_spell_token(current_reader(), tok, name, true) = 0;

	error("stray %qs in program", name);
      }
      goto retry;

    case CPP_OTHER:
      {
	cppchar_t c = tok->val.str.text[0];

	if (c == '"' || c == '\'')
	  error("missing terminating %c character", (int) c);
	else if (ISGRAPH (c))
	  error("stray %qc in program", (int) c);
	else
	  error("stray %<\\%o%> in program", (int) c);
      }
      goto retry;

    case CPP_CHAR:
    case CPP_WCHAR:
      lex_charconst(tok, lvalp);
      return CONSTANT;

    case CPP_STRING:
    case CPP_WSTRING:
      lex_string(tok, lvalp);
      return STRING;
      
    case CPP_PRAGMA:
      goto retry;

      /* Translate to the parser's symbols - somewhat of a legacy effect,
	 but having the characters does make the parser more readable... */
    case CPP_EQ: return '=';
    case CPP_NOT: return '!';
    case CPP_GREATER: return '>';
    case CPP_LESS: return '<';
    case CPP_PLUS: return '+';
    case CPP_MINUS: return '-';
    case CPP_MULT: return '*';
    case CPP_DIV: return '/';
    case CPP_MOD: return '%';
    case CPP_AND: return '&';
    case CPP_OR: return '|';
    case CPP_XOR: return '^';
    case CPP_RSHIFT: return RSHIFT;
    case CPP_LSHIFT: return LSHIFT;
    case CPP_COMPL: return '~';
    case CPP_AND_AND: return ANDAND;
    case CPP_OR_OR: return OROR;
    case CPP_QUERY: return '?';
    case CPP_COLON: return ':';
    case CPP_COMMA: return ',';
    case CPP_OPEN_PAREN: return '(';
    case CPP_CLOSE_PAREN: return ')';
    case CPP_EQ_EQ: lvalp->u.itoken.i = kind_eq; return EQCOMPARE;
    case CPP_NOT_EQ: lvalp->u.itoken.i = kind_ne; return EQCOMPARE;
    case CPP_GREATER_EQ: lvalp->u.itoken.i = kind_geq; return ARITHCOMPARE;
    case CPP_LESS_EQ: lvalp->u.itoken.i = kind_leq; return ARITHCOMPARE;
    case CPP_PLUS_EQ: lvalp->u.itoken.i = kind_plus_assign; return ASSIGN;
    case CPP_MINUS_EQ: lvalp->u.itoken.i = kind_minus_assign; return ASSIGN;
    case CPP_MULT_EQ: lvalp->u.itoken.i = kind_times_assign; return ASSIGN;
    case CPP_DIV_EQ: lvalp->u.itoken.i = kind_divide_assign; return ASSIGN;
    case CPP_MOD_EQ: lvalp->u.itoken.i = kind_modulo_assign; return ASSIGN;
    case CPP_AND_EQ: lvalp->u.itoken.i = kind_bitand_assign; return ASSIGN;
    case CPP_OR_EQ: lvalp->u.itoken.i = kind_bitor_assign; return ASSIGN;
    case CPP_XOR_EQ: lvalp->u.itoken.i = kind_bitxor_assign; return ASSIGN;
    case CPP_RSHIFT_EQ: lvalp->u.itoken.i = kind_rshift_assign; return ASSIGN;
    case CPP_LSHIFT_EQ: lvalp->u.itoken.i = kind_lshift_assign; return ASSIGN;
    case CPP_OPEN_SQUARE: return '[';
    case CPP_CLOSE_SQUARE: return ']';
    case CPP_OPEN_BRACE: return '{';
    case CPP_CLOSE_BRACE: return '}';
    case CPP_SEMICOLON: return ';';
    case CPP_ELLIPSIS: return ELLIPSIS;
    case CPP_PLUS_PLUS: return PLUSPLUS;
    case CPP_MINUS_MINUS: return MINUSMINUS;
    case CPP_DEREF: return POINTSAT;
    case CPP_FERED: return TASTNIOP;
    case CPP_DOT: return '.';
    case CPP_ATSIGN: return '@';

      /* These tokens should not be visible outside cpplib.  */
    case CPP_HEADER_NAME:
    case CPP_MACRO_ARG:
    default:
      assert(0);
    }
}

/* We keep a 2-element queue of pre-read tokens to deal with the 
   lookahead of checking for typedef references in components,
   stored in token_s1/l1, token_s2/l2.
   - gettoken returns the next token from the queue, or reads the
   next token if the queue is empty.
   - pushtoken pushes a token onto the queue
   - yylex does the special component.typedef processing. 

   This code could be optimised to use token_[sl][12] directly in
   yylex, but then it would be even more confusing.

   It would be nicer to just use _cpp_backup_tokens, but the comments
   about pushing back more than one token when a macro is involed,
   and the corresponding abort in the implementation are too worrying...
*/

static int poptoken(struct yystype *lvalp)
{
  /* Check the queue first */
  if (current.lex.token_s1 != -1)
    {
      int token = current.lex.token_s1;
      *lvalp = current.lex.token_l1;

      current.lex.token_s1 = current.lex.token_s2;
      current.lex.token_l1 = current.lex.token_l2;
      current.lex.token_s2 = -1;

      return token;
    }
  else
    return lex_token(lvalp);
}

static void pushtoken(int t, struct yystype *lvalp)
{
  /* Save token on our 2-element queue */
  if (current.lex.token_s1 == -1)
    {
      current.lex.token_s1 = t;
      current.lex.token_l1 = *lvalp;
    }
  else
    {
      current.lex.token_s2 = t;
      current.lex.token_l2 = *lvalp;
    }
}

int
yylex(struct yystype *lvalp)
{
  int token = poptoken(lvalp);

  /* Detect component-ref '.' identifier, where the
     identifier denotes a typedef in the referenced component --
     we can't do this in the parser as the resulting grammer is not
     context-free. So instead we detect it here, and mark the 
     component-ref as special. */
  if (token == COMPONENTREF)
    {
      struct yystype val1;
      int token1 = poptoken(&val1);

      token = IDENTIFIER; /* default to regular identifier */
      if (token1 == '.')
	{
	  struct yystype val2;
	  int token2 = poptoken(&val2);

	  if (token2 == IDENTIFIER || token2 == TYPENAME ||
	      token2 == MAGIC_STRING)
	    {
	      data_declaration cref = lvalp->idtoken.decl;
	      data_declaration fdecl = env_lookup(cref->ctype->env->id_env, val2.idtoken.id.data, TRUE);

	      if (fdecl && fdecl->kind == decl_typedef)
		{
		  /* The special typedef reference case. Fix the tokens */
		  token = COMPONENTREF;
		  token2 = IDENTIFIER;
		  val2.idtoken.decl = fdecl;
		}
	    }
	  pushtoken(token1, &val1);
	  pushtoken(token2, &val2);
	}
      else
	pushtoken(token1, &val1);
    }

  return token;
}

void yyerror(char *string)
{
  char buf[200];
  enum cpp_ttype ttype;
  unsigned char *ttext;

  if (!last_token)
    {
      error(string);
      return;
    }

  ttype = last_token->type;
  if (ttype != CPP_EOF)
    ttext = cpp_token_as_text(current_reader(), last_token);

  strcpy (buf, string);

  if ((ttype = last_token->type) == CPP_EOF)
    strcat(buf, " at end of input");
  else if (ttype == CPP_STRING || ttype == CPP_WSTRING)
    strcat(buf, " before string constant");
  else if (ttype == CPP_CHAR || ttype == CPP_WCHAR)
    strcat(buf, " before character constant");
  else if (ttext[0] < 040 || ttext[0] >= 0177)
    sprintf(buf + strlen (buf), " before character 0%o", ttext[0]);
  else
    strcat (buf, " before `%s'");

  error(buf, ttext);
}

/* Function used when yydebug is set, to print a token in more detail.  */
void yyprint (FILE *file, int yychar, YYSTYPE yylval)
{
  switch (yychar)
    {
    case IDENTIFIER:
      fprintf(file, " '%s'", yylval.idtoken.id.data);
      break;
    }
}
