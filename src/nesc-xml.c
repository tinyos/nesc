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

#include <ctype.h>
#include "parser.h"
#include "nesc-xml.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "constants.h"
#include "init.h"

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

void xputc(char c)
{
  if (xml_file)
    putc(c, xml_file);
}

static void output_indentation(void)
{
  int i;

  if (xml_file)
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
  xputc('\n');
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
  if (xml_file)
    {
      output_indent_if_needed();
      vfprintf(xml_file, format, args);
    }
}

void xprintf(char *format, ...)
{
  va_list args;

  va_start(args, format);
  xvprintf(format, args);
  va_end(args);
}

/* Output an arbitrary C character in an XML-friendly way. Control
   characters are remapped to 0x2400, except CR, LF and Tab */
void xqputc(int c)
{
  /* Ahh, the joys of XML. The control characters are lurking
     from 0x2400 onwards, except for CR, LF and Tab which exist
     at their usual value. Furthermore, ", < and & need to be quoted */
  /* Some characters show up as small numbers */
  if (c == '"' || c == '<' || c == '&' ||
      c == '\r' || c == '\n' || c == '\t')
    xprintf("&#%d;", c);
  /* Regular ASCII chars show up as themselves */
  else if ((unsigned char)c == c && isprint(c))
    putc(c, xml_file);
  else 
    {
      /* Everything else shows up on the 0x24xx page */
      c += 0x2400;
      xprintf("&#%d;", c);
    }
}

/* Output a string quoted to match XML AttValue rules */
void xqputs(const char *s)
{
  if (!xml_file)
    return;

  while (*s)
    xqputc(*s++);
}

/* Output a wide-char string quoted to match XML AttValue rules */
void xqputcs(const cstring s)
{
  int i;

  if (!xml_file)
    return;

  for (i = 0; i < s.length; i++)
    xqputc(s.data[i]);
}


void xputs(const char *s)
{
  if (!xml_file)
    return;

  output_indent_if_needed();
  fputs(s, xml_file);
}


/* Leaks until xml_end. */
static void push_tag(const char *tag)
{
  if (tags)
    dd_add_first(xml_region, tags, (char *)tag);
}

static const char *pop_tag(void)
{
  dd_list_pos top;
  const char *tag;

  if (!tags)
    return NULL;

  top = dd_first(tags);
  tag = DD_GET(const char *, top);
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

void xml_qtag(const char *tag)
{
  xprintf("<%s/>", tag);
}

void xml_tag_end_pop(void)
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

void xml_attr_int(const char *name, largest_int val)
{
  xprintf(" %s=\"%lld\"", name, val);
}

void xml_attr_ptr(const char *name, void *val)
{
  xprintf(" %s=\"%p\"", name, val);
}

void xml_attr_noval(const char *name)
{
  xprintf(" %s=\"\"", name);
}

void xml_attr_bool(const char *name, bool val)
{
  if (val)
    xml_attr_noval(name);
}

void xml_attr_cval(const char *name, cval val)
{
  bool unknown = FALSE;

  xprintf(" %s=\"", name);

  if (cval_isunsigned(val))
    xprintf("I:%llu", cval_uint_value(val));
  else if (cval_isinteger(val))
    xprintf("I:%lld", cval_sint_value(val));
  else if (cval_isunknown(val))
    unknown = TRUE;
  else if (cval_isfloating(val))
    /* XXX: hacky version */
    xprintf("F:%.20Le", cval_float_value(val));
  else if (cval_isaddress(val))
    {
      data_declaration ddecl = cval_ddecl(val);

      /* XXX: We don't (yet) support strings with an offset */
      if (ddecl && ddecl->kind == decl_magic_string && cval_knownbool(val))
	{
	  /* Wide strings are printed as their byte-by-byte rep. FIXME */
	  xputs("S:");
	  xqputcs(ddecl->schars);
	}
      else
	unknown = TRUE;
    }
  else if (cval_istop(val))
    xputs("V:");
  else
    unknown = TRUE;

  if (unknown)
    xputs("U:");
  xputc('"');
}

void xml_attr_loc(location loc)
{
  if (loc == dummy_location)
    {
      xprintf("  loc=\"NONE\"");
      return;
    }
  xprintf(" loc=\"%d", loc->lineno);
  if (loc->container)
    {
      xputc('(');
      xqputs(loc->container->instance_name);
      xputc(')');
    }
  xputc(':');
  xqputs(loc->filename);
  xputc('"');
}


void xml_start_dummy(void)
{
  xml_file = NULL;
  indent_level = 0;
  at_line_start = TRUE;
  tags = NULL;
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
  tags = NULL;
}

/* Convenient shortcuts */

void indentedtag_start(const char *name)
{
  xstartline();
  xml_tag_start(name);
  xindent();
}

void indentedtag(const char *name)
{
  xstartline();
  xml_tag(name);
  xindent();
  xnewline();
}

void indentedtag_pop(void)
{
  xstartline();
  xunindent();
  xml_pop();
  xnewline();
}

/* Standard nesC xml elements */
xml_list xl_variables, xl_constants, xl_functions, xl_typedefs,
  xl_interfaces, xl_icomponents, xl_interfacedefs, xl_components, xl_tags;

void nxml_simple_value(type t, cval val)
{
  indentedtag_start("value");
  xml_attr_cval("cst", val); /* val can be cval_top */
  xml_tag_end();
  nxml_type(t);
  indentedtag_pop();
}

void nxml_ddecl_ref(data_declaration ddecl)
{
  xml_list l = NULL;

  switch (ddecl->kind)
    {
    case decl_variable: xml_tag_start("variable-ref"); l = xl_variables; break;
    case decl_constant: xml_tag_start("constant-ref"); l = xl_constants; break;
    case decl_function: xml_tag_start("function-ref"); l = xl_functions; break;
    case decl_typedef: xml_tag_start("typedef-ref"); l = xl_typedefs; break;
    case decl_interface_ref: xml_tag_start("interface-ref"); l = xl_interfaces; break;
    case decl_component_ref: xml_tag_start("internal-component-ref"); l = xl_icomponents; break;
    default: assert(0);
    }
  xml_list_add(l, ddecl);
  xml_attr("name", ddecl->name);
  if (ddecl->container || ddecl->container_function)
    xml_attr_noval("scoped");
  xml_attr_ptr("ref", ddecl);
  xml_tag_end_pop();
}

void nxml_ndecl_ref(nesc_declaration ndecl)
{
  if (ndecl->kind == l_interface)
    {
      xml_tag_start("interfacedef-ref");
      xml_list_add(xl_interfacedefs, ndecl);
    }
  else
    {
      xml_list_add(xl_components, ndecl);
      xml_tag_start("component-ref");
    }
  xml_attr("qname", ndecl->instance_name);
  //xml_attr_ptr("ref", ndecl);
  xml_tag_end_pop();
}

void nxml_arguments(expression arguments)
{
  expression arg;

  indentedtag("arguments");
  scan_expression (arg, arguments)
    {
      if (is_type_argument(arg))
	nxml_type(CAST(type_argument, arg)->asttype->type);
      else 
	nxml_simple_value(arg->type, arg->cst ? arg->cst->cval : cval_top);
    }
  indentedtag_pop();
}

void nxml_instance(nesc_declaration ndecl)
{
  nesc_declaration orig = original_component(ndecl);

  indentedtag_start("instance");
  if (ndecl->kind == l_component && ndecl->original && !ndecl->abstract)
    xml_attr_int("number", ndecl->instance_number);
  xml_tag_end(); xnewline();

  nxml_ndecl_ref(orig);
  if (ndecl->arguments)
    nxml_arguments(ndecl->arguments);

  indentedtag_pop();
}

void nxml_tdecl_ref(tag_declaration tdecl)
{
  char tag[20];

  xml_list_add(xl_tags, tdecl);
  sprintf(tag, "%s-ref", tagkind_name(tdecl->kind));
  xml_tag_start(tag);
  if (tdecl->name)
    xml_attr("name", tdecl->name);
  xml_attr_bool("scoped", !!tdecl->container/* || tdecl->container_function*/);
  xml_attr_ptr("ref", tdecl);
  xml_tag_end_pop();
}

static void nxml_value_base(ivalue value)
{
  nxml_simple_value(value->type, value->u.base.value);
}

static void nxml_value_array(ivalue value)
{
  ivalue_array elem;

  indentedtag("value-array");
  nxml_type(value->type);
  for (elem = value->u.array; elem; elem = elem->next)
    {
      indentedtag_start("array-element");
      xml_attr_int("from", elem->from);
      xml_attr_int("to", elem->to);
      xml_tag_end();
      nxml_value(elem->value);
      indentedtag_pop();
    }
  indentedtag_pop();
}

static void nxml_value_structured(ivalue value)
{
  ivalue_field field;

  indentedtag("value-structured");
  nxml_type(value->type);
  for (field = value->u.structured; field; field = field->next)
    {
      indentedtag_start("structured-element");
      xml_attr("field", field->field->name);
      xml_attr_ptr("ref", field->field);
      xml_tag_end();
      nxml_value(field->value);
      indentedtag_pop();
    }
  indentedtag_pop();
}

void nxml_value(ivalue value)
{
  switch (value->kind)
    {
    case iv_base: nxml_value_base(value); break;
    case iv_array: nxml_value_array(value); break;
    case iv_structured: nxml_value_structured(value); break;
    default: assert(0);
    }
}

void nxml_doc(struct docstring *doc)
{
  if (!doc->short_s)
    return;
  indentedtag_start("documentation");
  xml_attr_loc(doc->loc);
  xml_tag_end();
  xnewline();
  xml_tag("short");
  xqputs(doc->short_s);
  xml_pop();
  xnewline();
  if (doc->long_s)
    {
      xml_tag("long");
      xqputs(doc->long_s);
      xml_pop();
      xnewline();
    }
  indentedtag_pop();
}


/* Incremental list creation support */

struct xml_list
{
  region r;
  dd_list all;
  dd_list latest;
  bool *changed;
  bool (*addfilter)(void *entry);
};

xml_list new_xml_list(region r, bool *changed, bool (*addfilter)(void *entry))
{
  xml_list l = ralloc(r, struct xml_list);

  l->r = r;
  l->all = dd_new_list(l->r);
  l->changed = changed;
  l->addfilter = addfilter;

  return l;
}

void xml_list_add(xml_list l, void *entry)
{
  if (!l || !l->addfilter(entry))
    return;

  if (!l->latest)
    l->latest = dd_new_list(l->r);
  dd_add_last(l->r, l->latest, entry);
  dd_add_last(l->r, l->all, entry);
  *l->changed = TRUE;
}

dd_list xml_list_latest(xml_list l)
{
  dd_list latest = l->latest;
  l->latest = NULL;
  return latest;
}

void xml_list_reset(xml_list l)
{
  l->latest = l->all;
}




