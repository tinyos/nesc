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
#include "nesc-xml.h"

/* Pick an indentation amount */
#define INDENT 2

static region xml_region;
static FILE *xml_file;
static int indent_level;
static dd_list tags;
static bool at_line_start;

void xindent(void)
{
  indent_level += INDENT;
}

void xunindent(void)
{
  indent_level -= INDENT;
}

static void output_indentation(void)
{
  int i;

  for (i = 0; i < indent_level; i++) putc(' ', xml_file);
}

static void output_indent_if_needed(void)
{
  if (at_line_start)
    {
      at_line_start = FALSE;
      output_indentation();
    }
}

void xnewline(void)
{
  putc('\n', xml_file);
  at_line_start = TRUE;
}

void xstartline(void)
{
  if (!at_line_start) xnewline();
}

void xstartline_noindent(void)
{
  xstartline();
  at_line_start = FALSE;
}

void xvprintf(char *format, va_list args)
{
  output_indent_if_needed();
  vfprintf(xml_file, format, args);
}

void xprintf(char *format, ...)
{
  va_list args;

  va_start(args, format);
  xvprintf(format, args);
  va_end(args);
}

void xqputs(const char *s)
{
  /* Output a string quoted to match XML AttValue rules */
  while (*s)
    {
      switch (*s)
	{
	case '\n': case '"': case '<': case '&': 
	  fprintf(xml_file, "&#%d;", (unsigned char)*s);
	  break;
	default:
	  putc(*s, xml_file);
	  break;
	}
      s++;
    }
}

void xputs(const char *s)
{
  output_indent_if_needed();
  fputs(s, xml_file);
}


/* Leaks until xml_end. */
static void push_tag(const char *tag)
{
  dd_add_first(xml_region, tags, (char *)tag);
}

static const char *pop_tag(void)
{
  dd_list_pos top = dd_first(tags);
  const char *tag = DD_GET(const char *, top);

  dd_remove(top);

  return tag;
}

void xml_tag_start(const char *tag)
{
  push_tag(tag);
  xprintf("<%s", tag);
}

void xml_tag(const char *tag)
{
  xml_tag_start(tag);
  xml_tag_end();
}

void xml_tag_end(void)
{
  xputs(">");
}

void xml_qtag_end(void)
{
  pop_tag();
  xputs("/>");
}

void xml_pop(void)
{
  xprintf("</%s>", pop_tag());
}

void xml_attr(const char *name, const char *val)
{
  xprintf(" %s=\"", name);
  xqputs(val);
  xputs("\"");
}

void xml_start(FILE *f)
{
  xml_region = newregion();
  xml_file = f;
  indent_level = 0;
  at_line_start = TRUE;
  tags = dd_new_list(xml_region);
}

void xml_end(void)
{
  deleteregion_ptr(&xml_region);
  xml_file = NULL;
}
