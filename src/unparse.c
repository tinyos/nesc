/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
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

#include <stdarg.h>
#include <stdlib.h>

#include "parser.h"
#include "unparse.h"
#include "semantics.h"
#include "constants.h"
#include "AST_utils.h"
#include "errors.h"
#include "nesc-semantics.h"
#include "nesc-magic.h"

/* Set this to 1 to avoid warnings from gcc about paren use with
   -Wparentheses */
#define CONSERVATIVE_PARENS 1

/* Pick an indentation amount */
#define INDENT 2

/* The output file for unparsing */
static FILE *of;
static bool no_line_directives;
static int indent_level;
static struct location output_loc;
static location fixed_location;
static bool at_line_start;

/* separator used between module name and function name */
static char *function_separator;

/* modify behavior of low-level functions when printing docs */
static bool documentation_mode;

typedef struct prt_closure {
  void (*fn)(struct prt_closure *closure);

  const char *name;
  struct prt_closure *parent;
} *prt_closure;

void indent(void)
{
  indent_level += INDENT;
}

void unindent(void)
{
  indent_level -= INDENT;
}

static void output_indentation(void)
{
  int i;

  for (i = 0; i < indent_level; i++) putc(' ', of);
}

void newline(void)
{
  putc('\n', of);
  at_line_start = TRUE;
  if (fixed_location)
    fprintf(of, "#line %lu\n", fixed_location->lineno);
  else
    output_loc.lineno++;
}

void startline(void)
{
  if (!at_line_start) newline();
}

void startline_noindent(void)
{
  startline();
  at_line_start = FALSE;
}

static void output_indent_if_needed(void)
{
  if (at_line_start)
    {
      at_line_start = FALSE;
      output_indentation();
    }
}

static void voutput(char *format, va_list args)
{
  output_indent_if_needed();
  vfprintf(of, format, args);
}

void output(char *format, ...)
{
  va_list args;

  va_start(args, format);
  voutput(format, args);
  va_end(args);
}

void outputln(char *format, ...)
{
  va_list args;

  va_start(args, format);
  voutput(format, args);
  va_end(args);

  newline();
}

void output_quoted(const char *s)
{
  /* Output a string which may contain newlines, \ and " */
  while (*s)
    {
      if (*s == '\n') /* don't confuse the line numbers */
	fputs("\\n", of);
      else 
	{
	  if (*s == '\\' || *s == '"')
	    putc('\\', of);
	  putc(*s, of);
	}
      s++;
    }
}


/**
 * copy a file to the output stream.
 **/
void copy_file_to_output(char *filename)
{
  char buf[1024 * 4];
  FILE *infile = fopen(filename, "r");
  size_t nread;
  size_t nwritten;
  assert(infile);
  
  while( !feof(infile) ) {
    nread = fread(buf, 1, sizeof(buf), infile);
    assert( !ferror(infile) );
    if(nread > 0) {
      nwritten = fwrite(buf, 1, nread, of);
      assert( !ferror(of) );
      assert(nwritten == nread);
    }
  }

  fclose(infile);
}

#define STRIP_PREFIX "__nesc_keyword_"
#define STRIP_PREFIX_LEN (sizeof(STRIP_PREFIX) - 1)

void output_stripped_cstring(cstring s)
{
  output_indent_if_needed();
  if (strncmp(s.data, STRIP_PREFIX, STRIP_PREFIX_LEN) == 0)
    fwrite(s.data + STRIP_PREFIX_LEN, s.length - STRIP_PREFIX_LEN, 1, of);
  else
    fwrite(s.data, s.length, 1, of);
}

void output_cstring(cstring s)
{
  output_indent_if_needed();
  fwrite(s.data, s.length, 1, of);
}

void output_stripped_string(const char *s)
{
  output_indent_if_needed();
  if (strncmp(s, STRIP_PREFIX, STRIP_PREFIX_LEN) == 0)
    fputs(s + STRIP_PREFIX_LEN, of);
  else
    fputs(s, of);
}

void output_stripped_string_dollar(const char *s)
{
  output_stripped_string(s);
  output(function_separator);
}

static void output_line_directive(location l, bool include_filename)
{
  if (fixed_location)
    return;

  startline_noindent();
  if (include_filename)
    {
      output("# %lu \"", l->lineno);
      output_quoted(l->filename);
      outputln("\"%s", l->in_system_header ? " 3" : "");
    }
  else
    outputln("#line %lu", l->lineno);
}

void set_location(location l)
{
  /* Ignore dummy locations */
  if (l->filename == dummy_location->filename || no_line_directives)
    return;

  if ((l->filename != output_loc.filename &&
       strcmp(l->filename, output_loc.filename)) ||
      l->in_system_header != output_loc.in_system_header)
    output_line_directive(l, TRUE);
  else if (output_loc.lineno != l->lineno)
    {
      /* Just send some newlines for small changes */
      if (!fixed_location &&
	  output_loc.lineno < l->lineno && output_loc.lineno + 10 >= l->lineno)
	{
	  while (output_loc.lineno != l->lineno)
	    newline();
	}
      else
	output_line_directive(l, FALSE);
    }

  output_loc = *l;
}

void set_fixed_location(location l)
{
  assert(l->filename != dummy_location->filename);

  set_location(l);
  fixed_location = l;
}

void clear_fixed_location(void)
{
  fixed_location = NULL;
}

struct location output_location(void)
{
  return output_loc;
}


void prt_toplevel_declarations(declaration dlist);
void prt_toplevel_declaration(declaration d);
void prt_asm_decl(asm_decl d);
void prt_extension_decl(extension_decl d);
void prt_data_decl(data_decl d);
void prt_ellipsis_decl(ellipsis_decl d);
void prt_function_decl(function_decl d);
void prt_variable_decl(variable_decl d);
void prt_type_elements(type_element elements, pte_options options);
void prt_type_element(type_element em, pte_options options);
void prt_typename(typename tname);
void prt_typeof_expr(typeof_expr texpr);
void prt_typeof_type(typeof_type ttype);
void prt_attribute(attribute a);
void prt_rid(rid r, pte_options options);
void prt_qualifier(qualifier q);
void prt_tag_ref(tag_ref sr, pte_options options);
void prt_fields(declaration flist);
void prt_enumerators(declaration elist, tag_declaration ddecl);
void prt_field_declaration(declaration d);
void prt_field_extension_decl(extension_decl d);
void prt_field_data_decl(data_decl d);
void prt_field_decl(field_decl fd);
void prt_enumerator(enumerator ed, tag_declaration ddecl);
void prt_asttype(asttype t);
void prt_word(word w);

void prt_expressions(expression elist, bool isfirst);
void prt_expression(expression e, int context_priority);
void prt_comma(comma e, int context_priority);
void prt_sizeof_type(sizeof_type e, int context_priority);
void prt_alignof_type(alignof_type e, int context_priority);
void prt_label_address(label_address e, int context_priority);
void prt_cast(cast e, int context_priority);
void prt_cast_list(cast_list e, int context_priority);
void prt_conditional(conditional e, int context_priority);
void prt_identifier(identifier e, int context_priority);
void prt_compound_expr(compound_expr e, int context_priority);
void prt_function_call(function_call e, int context_priority);
void prt_generic_call(generic_call e, int context_priority);
void prt_array_ref(array_ref e, int context_priority);
void prt_interface_deref(interface_deref e, int context_priority);
void prt_field_ref(field_ref e, int context_priority);
void prt_unary(unary e, int context_priority);
void prt_binary(binary e, int context_priority);
void prt_init_list(init_list e, int context_priority);
void prt_init_specific(init_specific e, int context_priority);
void prt_lexical_cst(lexical_cst e, int context_priority);
void prt_string(string e, int context_priority);
void prt_parameter_declarations(declaration dlist);
void prt_parameter_declaration(declaration d);

void prt_statement(statement s);
void prt_compound_stmt(compound_stmt s);
void prt_compound_declarations(declaration dlist);
void prt_compound_declaration(declaration d);
void prt_asm_stmt(asm_stmt s);
void prt_asm_stmt_plain(asm_stmt s);
void prt_asm_operands(asm_operand olist);
void prt_asm_operand(asm_operand o);
void prt_if_stmt(if_stmt s);
void prt_labeled_stmt(labeled_stmt s);
void prt_expression_stmt(expression_stmt s);
void prt_while_stmt(while_stmt s);
void prt_dowhile_stmt(while_stmt s);
void prt_switch_stmt(switch_stmt s);
void prt_for_stmt(for_stmt s);
void prt_break_stmt(break_stmt s);
void prt_continue_stmt(continue_stmt s);
void prt_return_stmt(return_stmt s);
void prt_goto_stmt(goto_stmt s);
void prt_computed_goto_stmt(computed_goto_stmt s);
void prt_empty_stmt(empty_stmt s);
void prt_atomic_stmt(atomic_stmt s);

void prt_label(label l);
void prt_id_label(id_label l);
void prt_case_label(case_label l);
void prt_default_label(default_label l);

void prt_regionof(expression e);

static region unparse_region;

void unparse_start(FILE *to)
{
  of = to;
  output_loc = *dummy_location;
  at_line_start = TRUE;
  no_line_directives = FALSE;
  documentation_mode = FALSE;
  indent_level = 0;
  function_separator = "$";
  unparse_region = newregion();
}

void unparse_end(void) deletes
{
  deleteregion_ptr(&unparse_region);
}

void unparse(FILE *to, declaration program) deletes
{
  unparse_start(to);
  prt_toplevel_declarations(program);
  unparse_end();
}

void enable_line_directives(void)
{
  if (no_line_directives)
    {
      no_line_directives = FALSE;
      /* Force #line on next output of some location */
      output_loc = *dummy_location;
    }
}

void disable_line_directives(void)
{
  no_line_directives = TRUE;
}

void set_function_separator(char *sep) 
{
  function_separator = sep;
}

FILE *set_unparse_outfile(FILE *newout) 
{
  FILE *temp = of;

  of = newout;
  return temp;
}

void enable_documentation_mode(void) 
{
  documentation_mode = TRUE;
}

void disable_documentation_mode(void)
{
  documentation_mode = FALSE;
}


void prt_toplevel_declarations(declaration dlist)
{
  declaration d;

  scan_declaration (d, dlist)
    prt_toplevel_declaration(d);
}

#define PRTCASE(type, x) case kind_ ## type: prt_ ## type(CAST(type, (x))); return

void prt_toplevel_declaration(declaration d)
{
  startline();
  switch (d->kind)
    {
      PRTCASE(asm_decl, d);
      PRTCASE(data_decl, d);
      PRTCASE(function_decl, d);
      PRTCASE(extension_decl, d);
    default: assert(0); break;
    }
}

/* Invariant: all declarations end with ; */
void prt_asm_decl(asm_decl d)
{
  prt_asm_stmt(d->asm_stmt);
}

void prt_extension_decl(extension_decl d)
{
  set_location(d->location);
  output("__extension__ ");
  prt_toplevel_declaration(d->decl);
}

void prt_ellipsis_decl(ellipsis_decl d)
{
  set_location(d->location);
  output("...");
}

static type_element interesting_element(type_element elems)
{
  type_element elem;

  scan_type_element (elem, elems)
    if (is_tag_ref(elem))
      return elem;

  return NULL;
}

static pte_options prefix_decl(data_declaration ddecl)
{
  /* Hack to add static to all defined functions */
  if (ddecl->kind == decl_function &&
      ddecl->ftype != function_static && !ddecl->isexterninline &&
      !ddecl->spontaneous && ddecl->definition)
    {
      output("static ");
      if (ddecl->makeinline)
	output("inline ");
      return pte_noextern;
    }
  return 0;
}

void prt_data_decl(data_decl d)
{
  declaration vd;
  pte_options opts = 0;
  type_element interesting;

  scan_declaration (vd, d->decls)
    {
      variable_decl vdd = CAST(variable_decl, vd);
      data_declaration vdecl = vdd->ddecl;
      pte_options extraopts = 0;

      if (vdecl) /* because build_declaration does not make a
		    data_declaration */
	{
	  /* Ignore unused non-local declarations 
	     (local ones might have an initialiser which must still be
	     executed) */
	  if (((vdecl->kind == decl_function || vdecl->kind == decl_variable)
	       && !vdecl->isused && !vdecl->islocal))
	    continue;
	  if (use_nido && is_module_local_static(vdecl))
	    continue;

	  extraopts = prefix_decl(vdecl);
	}

      prt_type_elements(d->modifiers, opts | extraopts);
      opts |= pte_duplicate;
      prt_variable_decl(vdd);
      outputln(";");
    }

  if (!(opts & pte_duplicate) &&
      (interesting = interesting_element(d->modifiers)))
    {
      prt_type_element(interesting, opts);
      outputln(";");
    }
}


void prt_parameter_declarations(declaration dlist)
{
  declaration d;

  scan_declaration (d, dlist)
    prt_parameter_declaration(d);
}

void prt_parameter_declaration(declaration d)
{
  startline();
  switch (d->kind)
    {
      PRTCASE(data_decl, d);
      PRTCASE(ellipsis_decl, d);
    default: assert(0); break;
    }
}

void prt_function_decl(function_decl d)
{
  if (d->ddecl->isused && !d->ddecl->suppress_definition)
    {
      prefix_decl(d->ddecl);
      prt_declarator(d->declarator, d->modifiers, d->attributes, d->ddecl,
		     psd_print_default);
      outputln(";");
    }
}

void prt_function_body(function_decl d)
{
  if (d->ddecl->isused && !d->ddecl->suppress_definition)
    {
      /* We set current.function_decl because unparsing may produce error
	 messages */
      current.function_decl = d;

      prefix_decl(d->ddecl);
      prt_declarator(d->declarator, d->modifiers, d->attributes, d->ddecl,
		     psd_print_default);
      startline();
      prt_parameter_declarations(d->old_parms);
      assert(is_compound_stmt(d->stmt));
      prt_statement(d->stmt);
      newline();

      current.function_decl = d->parent_function;
    }
}

void prt_variable_decl(variable_decl d)
{
  prt_declarator(d->declarator, NULL, d->attributes, d->ddecl, 0);

  if (d->asm_stmt)
    prt_asm_stmt_plain(d->asm_stmt);

  if (d->arg1 && !(use_nido && is_module_variable(d->ddecl)))
    {
      output(" = ");
      prt_expression(d->arg1, P_ASSIGN);
    }
}

void prt_declarator(declarator d, type_element elements, attribute attributes,
		    data_declaration ddecl, psd_options options)
{
  prt_type_elements(elements, 0);
  prt_type_elements(CAST(type_element, attributes), 0);
  prt_simple_declarator(d, ddecl,
			(options | psd_need_paren_for_qual)
			& ~psd_need_paren_for_star);
}

void prt_plain_ddecl(data_declaration ddecl, psd_options options)
{
  if (!ddecl->Cname)
    {
      if (ddecl->container && !(options & psd_skip_container))
	output_stripped_string_dollar(ddecl->container->name);
      if (ddecl->kind == decl_function && ddecl->interface)
	output_stripped_string_dollar(ddecl->interface->name);
      if ((options & psd_print_default) &&
	  (!ddecl->defined && ddecl->kind == decl_function &&
	   (ddecl->ftype == function_event ||
	    ddecl->ftype == function_command)))
      {
        output("default");
        output(function_separator);
      }

    }

  /* static local module variables are printed as mod$fn$var in nido */
  if (use_nido && is_module_local_static(ddecl))
    {
      data_declaration fn = ddecl->container_function;

      output_stripped_string_dollar(fn->container->name);
      output_stripped_string_dollar(fn->name);
    }

  output_stripped_string(ddecl->name);
}

void prt_ddecl_full_name(data_declaration ddecl, psd_options options)
{
  prt_plain_ddecl(ddecl, options);
  if (use_nido && is_module_variable(ddecl))
    output("[%s]", nido_num_nodes);
}

/* The return value is TRUE iff d is an identifier_declarator possibly
   prefixed with qualified_declarators */
bool prt_simple_declarator(declarator d, data_declaration ddecl,
			   psd_options options)
{
  if (d)
    switch (d->kind)
      {
      case kind_function_declarator:
	{
	  function_declarator fd = CAST(function_declarator, d);

	  prt_simple_declarator(fd->declarator, ddecl,
				options | psd_need_paren_for_star |
				psd_need_paren_for_qual);
	  prt_parameters(fd->gparms ? fd->gparms :
			 ddecl ? ddecl_get_gparms(ddecl) : NULL,
			 fd->parms,
			 options & psd_rename_parameters);
	  break;
	}
      case kind_array_declarator:
	{
	  array_declarator ad = CAST(array_declarator, d);
	  bool is_id;

	  is_id = prt_simple_declarator(ad->declarator, ddecl,
					options | psd_need_paren_for_star |
					psd_need_paren_for_qual);
	  if (!ad->arg1)
	    {
	      /* The array-type test is necessary because char x[] in a
		 parameter declaration is really a pointer declaration */
	      if (ddecl && is_id && type_array(ddecl->type))
		{
		  /* This is a declaration of an incomplete array type.
		     The type of ddecl contains the size of the array if
		     it is known. */
		  expression dsize = type_array_size(ddecl->type);

		  output("[%lu]",
			 (unsigned long)constant_uint_value(dsize->cst));
		}
	      else
		output("[]");
	    }
	  else
	    {
	      set_location(ad->arg1->location);
	      output("[");
	      prt_expression(ad->arg1, P_TOP);
	      output("]");
	    }
	  break;
	}
      case kind_qualified_declarator:
	{
	  qualified_declarator qd = CAST(qualified_declarator, d);
	  bool is_id;

	  set_location(qd->modifiers->location);
	  if (options & psd_need_paren_for_qual)
	    output("(");
	  prt_type_elements(qd->modifiers, 0);
	  is_id = prt_simple_declarator(qd->declarator, ddecl,
					options & ~psd_need_paren_for_qual);
	  if (options & psd_need_paren_for_qual)
	    output(")");
	  return is_id;
	}
      case kind_pointer_declarator:
	{
	  pointer_declarator pd = CAST(pointer_declarator, d);

	  if (options & psd_need_paren_for_star)
	    output("(");
	  output("*");
	  prt_simple_declarator(pd->declarator, ddecl,
				options & ~(psd_need_paren_for_star |
					    psd_need_paren_for_qual));
	  if (options & psd_need_paren_for_star)
	    output(")");
	  break;
	}
      case kind_identifier_declarator:
	set_location(d->location);
	if (options & psd_rename_identifier)
	  output("arg_%p", ddecl);
	else if (ddecl)
	  prt_ddecl_full_name(ddecl, options);
	else
	  output_stripped_cstring(CAST(identifier_declarator, d)->cstring);
	return TRUE;

      case kind_interface_ref_declarator:
	prt_simple_declarator(CAST(interface_ref_declarator, d)->declarator,
			      ddecl, options | psd_need_paren_for_star |
			      psd_need_paren_for_qual);
	break;

      default: assert(0); break;
      }
  return FALSE;
}

void prt_type_elements(type_element elements, pte_options options)
{
  type_element em;

  scan_type_element (em, elements)
    {
      prt_type_element(em, options);
      output(" ");
    }
}

void prt_type_element(type_element em, pte_options options)
{
  switch (em->kind)
    {
      PRTCASE(typename, em);
      PRTCASE(typeof_expr, em);
      PRTCASE(typeof_type, em);
      PRTCASE(attribute, em);
      PRTCASE(qualifier, em);
    case kind_rid:
      prt_rid(CAST(rid, em), options);
      break;
    default:
      if (is_tag_ref(em))
	prt_tag_ref(CAST(tag_ref, em), options);
      else
	assert(0);
      break;
    }
}

void prt_typename(typename tname)
{
  data_declaration tdecl = tname->ddecl;

  set_location(tname->location);
  if (tdecl->container && !tdecl->Cname)
    output_stripped_string_dollar(tdecl->container->name);
  output_stripped_string(tdecl->name);
}

void prt_typeof_expr(typeof_expr texpr)
{
  set_location(texpr->location);
  output("typeof(");
  prt_expression(texpr->arg1, P_TOP);
  output(")");
}

void prt_typeof_type(typeof_type ttype)
{
  set_location(ttype->location);
  output("typeof(");
  prt_asttype(ttype->asttype);
  output(")");
}

void prt_attribute(attribute a)
{
  if (!nesc_attribute(a))
    {
      set_location(a->location);
      output("__attribute((");
      prt_word(a->word1);
      if (a->word2 || a->args)
	{
	  output("(");
	  if (a->word2)
	    prt_word(a->word2);
	  prt_expressions(a->args, a->word2 == NULL);
	  output(")");
	}
      output("))");
    }
}

void prt_rid(rid r, pte_options options)
{
  switch (r->id)
    {
    case RID_COMMAND: case RID_EVENT: case RID_TASK: case RID_ASYNC:
    case RID_NORACE:
      // show these in documenation mode, but not otherwise
      if (documentation_mode && !(options & pte_skip_command_event)) 
	output("%s", rid_name(r));
      break;
    case RID_DEFAULT:
      break;
    case RID_EXTERN:
      if (options & pte_noextern)
	return;
      /* FALLTHROUGH */
    default:
      set_location(r->location);
      output("%s", rid_name(r));
      break;
    }
}

void prt_qualifier(qualifier q)
{
  set_location(q->location);
  output("%s", qualifier_name(q->id));
}

void prt_tag_ref(tag_ref tr, pte_options options)
{
  /* We must not name anonymous struct/unions (those which are collapsed
     into a containing struct/union) as that would make them non-anonymous
     (in gcc 3.3 and following) */
  if (!tr->tdecl->collapsed)
    name_tag(tr->tdecl);

  set_location(tr->location);
  switch (tr->kind)
    {
    case kind_struct_ref: output("struct "); break;
    case kind_union_ref: output("union "); break;
    case kind_enum_ref: output("enum "); break;
    default: assert(0);
    }

  if (tr->word1)
    {
      if (tr->tdecl && tr->tdecl->container)
	output_stripped_string_dollar(tr->tdecl->container->name);
      prt_word(tr->word1);
    }
  if (!(options & pte_duplicate) && tr->defined)
    {
      if (tr->kind == kind_enum_ref)
	prt_enumerators(tr->fields, tr->tdecl);
      else
	prt_fields(tr->fields);
    }
  if (tr->attributes)
    {
      output(" ");
      prt_type_elements(CAST(type_element, tr->attributes), 0);
    }
}

void prt_enumerators(declaration elist, tag_declaration tdecl)
{
  declaration d;

  output(" {");
  indent();
  startline();
  scan_declaration (d, elist)
    {
      prt_enumerator(CAST(enumerator, d), tdecl);
      if (d->next)
	output(", ");
    }
  unindent();
  startline();
  output("}");
}

void prt_fields(declaration flist)
{
  declaration d;

  output(" {");
  indent();
  startline();
  scan_declaration (d, flist)
    prt_field_declaration(d);
  unindent();
  startline();
  output("}");
}

void prt_field_declaration(declaration d)
{
  if (is_extension_decl(d))
    prt_field_extension_decl(CAST(extension_decl, d));
  else
    prt_field_data_decl(CAST(data_decl, d));
}

void prt_field_extension_decl(extension_decl d)
{
  set_location(d->location);
  output("__extension__ ");
  prt_field_declaration(d->decl);
}

void prt_field_data_decl(data_decl d)
{
  declaration fd;

  prt_type_elements(d->modifiers, 0);

  scan_declaration (fd, d->decls)
    {
      prt_field_decl(CAST(field_decl, fd));
      if (fd->next)
	output(", ");
    }
  outputln(";");
}

void prt_field_decl(field_decl fd)
{
  prt_declarator(fd->declarator, NULL, fd->attributes, NULL, 0);
  if (fd->arg1)
    {
      output(" : ");
      prt_expression(fd->arg1, P_TOP);
    }
}

void prt_enumerator(enumerator ed, tag_declaration tdecl)
{
  set_location(ed->location);

  if (tdecl && tdecl->container)
    output_stripped_string_dollar(tdecl->container->name);

  output_stripped_cstring(ed->cstring);
  if (ed->arg1)
    {
      output(" = ");
      prt_expression(ed->arg1, P_ASSIGN);
    }
}

void prt_parameters(declaration gparms, declaration parms, psd_options options)
{
  declaration d;
  bool forward = FALSE;
  bool first = TRUE;

  /* If asked to rename parameters, ask prt_parameter to rename identifiers
     when calling prt_declarator */
  if (options & psd_rename_parameters)
    options = psd_rename_identifier;
  else
    options = 0;

  output("(");
  scan_declaration (d, gparms)
    {
      prt_parameter(d, first, FALSE, options);
      first = FALSE;
    }
  scan_declaration (d, parms)
    {
      forward = prt_parameter(d, first, forward, options);
      first = FALSE;
    }
  if (!gparms && !parms)
    output("void");
  output(")");
}

bool prt_parameter(declaration parm, bool first, bool lastforward,
		   psd_options options)
{
  switch (parm->kind)
    {
    case kind_oldidentifier_decl:
      if (!first)
	output(", ");
      set_location(parm->location);
      output_stripped_cstring(CAST(oldidentifier_decl, parm)->cstring);
      return FALSE;
    case kind_ellipsis_decl:
      if (!first)
	output(", ");
      set_location(parm->location);
      output("...");
      return FALSE;
    case kind_data_decl:
      {
	data_decl dd = CAST(data_decl, parm);
	variable_decl vd = CAST(variable_decl, dd->decls);

	if (lastforward && !vd->forward)
	  output("; ");
	else if (!first)
	  output(", ");
	prt_declarator(vd->declarator, dd->modifiers, vd->attributes,
		       vd->ddecl, options);

	return vd->forward;
      }
    default: assert(0); return FALSE;
    }
}

void prt_asttype(asttype t)
{
  prt_declarator(t->declarator, t->qualifiers, NULL, NULL, 0);
}

void prt_word(word w)
{
  set_location(w->location);
  output_stripped_cstring(w->cstring);
}

void prt_expressions(expression elist, bool isfirst)
{
  expression e;

  scan_expression (e, elist)
    {
      if (!isfirst) output(", ");
      isfirst = FALSE;
      prt_expression(e, P_ASSIGN); /* priority is that of assignment */
    }
}

#define PRTEXPR(type, x) case kind_ ## type: prt_ ## type(CAST(type, (x)), context_priority); return

/* Context priorities are that of the containing operator, starting at 0
   for , going up to 14 for ->, . See the symbolic P_XX constants 
   P_TOP (-1) is used for contexts with no priority restrictions. */
void prt_expression(expression e, int context_priority)
{
  switch (e->kind) 
    {
      PRTEXPR(comma, e);
      PRTEXPR(sizeof_type, e);
      PRTEXPR(alignof_type, e);
      PRTEXPR(label_address, e);
      PRTEXPR(cast, e);
      PRTEXPR(cast_list, e);
      PRTEXPR(conditional, e);
      PRTEXPR(identifier, e);
      PRTEXPR(compound_expr, e);
      PRTEXPR(function_call, e);
      PRTEXPR(generic_call, e);
      PRTEXPR(array_ref, e);
      PRTEXPR(field_ref, e);
      PRTEXPR(interface_deref, e);
      PRTEXPR(init_list, e);
      PRTEXPR(init_specific, e);
    case kind_string_cst:
      PRTEXPR(lexical_cst, e);
      PRTEXPR(string, e);
    default: 
      if (is_unary(e))
	{
	  prt_unary(CAST(unary, e), context_priority);
	  return;
	}
      assert(is_binary(e));
      prt_binary(CAST(binary, e), context_priority);
      return;
    }
}

#define OPEN(pri) \
  if (pri < context_priority) \
    output("(")

#define CLOSE(pri) \
  if (pri < context_priority) \
    output(")")

void prt_comma(comma e, int context_priority)
{
  OPEN(P_COMMA);
  prt_expressions(e->arg1, TRUE);
  CLOSE(P_COMMA);
}

void prt_sizeof_type(sizeof_type e, int context_priority)
{
  set_location(e->location);
  output("sizeof(");
  prt_asttype(e->asttype);
  output(")");
}

void prt_alignof_type(alignof_type e, int context_priority)
{
  set_location(e->location);
  output("__alignof__(");
  prt_asttype(e->asttype);
  output(")");
}

void prt_label_address(label_address e, int context_priority)
{
  set_location(e->location);
  output("&&");
  prt_id_label(e->id_label);
}

void prt_cast(cast e, int context_priority)
{
  OPEN(P_CAST);
  set_location(e->location);
  output("(");
  prt_asttype(e->asttype);
  output(")");
  prt_expression(e->arg1, P_CAST);
  CLOSE(P_CAST);
}

void prt_cast_list(cast_list e, int context_priority)
{
  OPEN(P_CAST);
  set_location(e->location);
  output("(");
  prt_asttype(e->asttype);
  output(")");
  prt_init_list(CAST(init_list, e->init_expr), P_ASSIGN);
  CLOSE(P_CAST);
}

void prt_conditional(conditional e, int context_priority)
{
  OPEN(P_COND);
  prt_expression(e->condition, P_COND);
  output(" ? ");
  if (e->arg1)
    prt_expression(e->arg1, P_COND);
  output(" : ");
  prt_expression(e->arg2, P_COND);
  CLOSE(P_COND);
}

void prt_identifier(identifier e, int context_priority)
{
  data_declaration decl = e->ddecl;

  if (decl->kind == decl_function && decl->uncallable)
    error_with_location(e->location, "%s not connected", e->cstring.data);

  set_location(e->location);
#if 0
  if (decl->kind == decl_constant)
    {
      output_indent_if_needed();
      constant_print(of, decl->value);
    }
  else
#endif
    prt_plain_ddecl(decl, 0);
      
  if (use_nido && is_module_variable(decl))
    output("[%s]", nido_mote_number);
}

void prt_compound_expr(compound_expr e, int context_priority)
{
  set_location(e->location);
  output("(");
  prt_compound_stmt(CAST(compound_stmt, e->stmt));
  output(")");
}

void prt_function_call(function_call e, int context_priority)
{
  switch (e->call_kind)
    {
    case post_task:
      set_location(e->arg1->location);
      output("TOS_post(");
      prt_expression(e->arg1, P_ASSIGN);
      output(")");
      break;
    default:
      if (e->va_arg_call)
	{
	  /* The extra parentheses are added because gcc 2.96 (aka redhat 7's
	     gcc) has a broken syntax for __builtin_va_arg */
	  output("(__builtin_va_arg(");
	  prt_expression(e->args, P_ASSIGN);
	  output(", ");
	  prt_asttype(e->va_arg_call);
	  output("))");
	}
      else if (!magic_print(e))
	{
	  prt_expression(e->arg1, P_CALL);
	  /* Generic calls have already started the argument list.
	     See prt_generic_call */
	  if (is_generic_call(e->arg1))
	    prt_expressions(e->args, FALSE);
	  else
	    {
	      output("(");
	      prt_expressions(e->args, TRUE);
	    }
	  output(")");
	}
      break;
    }
}

void prt_generic_call(generic_call e, int context_priority)
{
  prt_expression(e->arg1, P_CALL);
  /* function_call will finish the argument list. See prt_function_call */
  output("(");
  prt_expressions(e->args, TRUE);

  /* This is a convenient place to do this check. We can't easily do it
     in make_generic_call as we don't (yet) know our parent. */
  if (!is_function_call(e->parent))
    error_with_location(e->location, "generic arguments can only be used in command/event calls");
}

void prt_array_ref(array_ref e, int context_priority)
{
  prt_expression(e->arg1, P_CALL);
  output("[");
  prt_expression(e->arg2, P_TOP);
  output("]");
}

void prt_field_ref(field_ref e, int context_priority)
{
  /* Reconstruct -> for nicer output */
  if (is_dereference(e->arg1))
    {
      prt_expression(CAST(dereference, e->arg1)->arg1, P_CALL);
      output("->");
    }
  else
    {
      prt_expression(e->arg1, P_CALL);
      output(".");
    }
  output_stripped_cstring(e->cstring);
}

void prt_interface_deref(interface_deref e, int context_priority)
{
  data_declaration decl = e->ddecl;

  if (decl->kind == decl_function && decl->uncallable)
    error_with_location(e->location, "%s.%s not connected",
			CAST(identifier, e->arg1)->cstring.data,
			e->cstring.data);

  prt_expression(e->arg1, P_CALL);
  output(function_separator);
  output_stripped_cstring(e->cstring);
}

void prt_unary(unary e, int context_priority)
{
  char *op = NULL, *postop = NULL;
  int pri = 0;

  /* Yuck. Evil hack because gcc is broken (breaks the a[i] === *(a+i)
     rule when a is a non-lvalue array). So we undo our earlier rewrite
     (from fix.c) of a[i] as *(a+i). Note that gcc doesn't allow i[a] in
     this case (bozos at work?) */
  if (is_dereference(e) && is_plus(e->arg1))
    {
      plus derefed = CAST(plus, e->arg1);

      if (type_array(derefed->arg1->type))
	{
	  prt_array_ref(derefed, context_priority);
	  return;
	}
    }
  switch (e->kind)
    {
    case kind_dereference: op = "*"; break;
    case kind_extension_expr: op = "__extension__ "; break;
      /* Higher priority for sizeof/alignof expr because we must
	 add parens around sizeof cast_expr (e.g. sizeof((char)x), not
	 sizeof (char)x */
    case kind_sizeof_expr: op = "sizeof "; pri = P_CALL; break;
    case kind_alignof_expr: op = "__alignof__ "; pri = P_CALL; break;
    case kind_realpart: op = "__real__ "; break;
    case kind_imagpart: op = "__imag__ "; break;
    case kind_address_of: op = "&"; break;
    case kind_unary_minus: op = "-"; break;
    case kind_unary_plus: op = "+"; break;
    case kind_preincrement: op = "++"; break;
    case kind_predecrement: op = "--"; break;
    case kind_postincrement: postop = "++"; break;
    case kind_postdecrement: postop = "--"; break;
    case kind_conjugate: case kind_bitnot: op = "~"; break;
    case kind_not: op = "!"; break;
    default: assert(0); return;
    }
  OPEN(P_CAST);
  set_location(e->location);
  if (op)
    {
      output(op);
      if (is_unary(e->arg1))
	output(" "); /* Catch weirdness such as - - x */
      if (!pri)
	pri = P_CAST;
    }
  prt_expression(e->arg1, pri ? pri : P_CALL);
  if (postop)
    output(postop);
  CLOSE(P_CAST);
}

const char *binary_op_name(AST_kind kind)
{
  switch (kind)
    {
    case kind_plus: return "+"; 
    case kind_minus: return "-"; 
    case kind_times: return "*"; 
    case kind_divide: return "/"; 
    case kind_modulo: return "%"; 
    case kind_lshift: return "<<"; 
    case kind_rshift: return ">>"; 
    case kind_leq: return "<="; 
    case kind_geq: return ">="; 
    case kind_lt: return "<"; 
    case kind_gt: return ">"; 
    case kind_eq: return "=="; 
    case kind_ne: return "!="; 
    case kind_bitand: return "&"; 
    case kind_bitor: return "|"; 
    case kind_bitxor: return "^"; 
    case kind_andand: return "&&"; 
    case kind_oror: return "||"; 
    case kind_assign: return "="; 
    case kind_plus_assign: return "+="; 
    case kind_minus_assign: return "-="; 
    case kind_times_assign: return "*="; 
    case kind_divide_assign: return "/="; 
    case kind_modulo_assign: return "%="; 
    case kind_lshift_assign: return "<<="; 
    case kind_rshift_assign: return ">>="; 
    case kind_bitand_assign: return "&="; 
    case kind_bitor_assign: return "|="; 
    case kind_bitxor_assign: return "^="; 
    default: assert(0); return "<bad>";
    }
}

void prt_binary(binary e, int context_priority)
{
  int pri, lpri, rpri;
  const char *op = binary_op_name(e->kind);

  switch (e->kind)
    {
    case kind_times: case kind_divide: case kind_modulo:
      lpri = P_TIMES; pri = P_TIMES; rpri = P_CAST; break;
    case kind_plus: case kind_minus:
      lpri = P_PLUS; pri = P_PLUS; rpri = P_TIMES; break;
    case kind_lshift: case kind_rshift:
      pri = P_SHIFT;
      if (CONSERVATIVE_PARENS)
	lpri = rpri = P_TIMES;
      else
	{
	  lpri = P_SHIFT; rpri = P_PLUS; 
	}
      break;
    case kind_leq: case kind_geq: case kind_lt: case kind_gt:
      lpri = P_REL; pri = P_REL; rpri = P_SHIFT; break;
    case kind_eq: case kind_ne:
      lpri = P_EQUALS; pri = P_EQUALS; rpri = P_REL; break;
    case kind_bitand:
      pri = P_BITAND;
      if (CONSERVATIVE_PARENS)
	lpri = rpri = P_TIMES;
      else
	{
	  lpri = P_BITAND; rpri = P_EQUALS; 
	}
      break;
    case kind_bitxor:
      pri = P_BITXOR;
      if (CONSERVATIVE_PARENS)
	lpri = rpri = P_TIMES;
      else
	{
	  lpri = P_BITXOR; rpri = P_BITAND;
	}
      break;
    case kind_bitor:
      pri = P_BITOR;
      if (CONSERVATIVE_PARENS)
	lpri = rpri = P_TIMES;
      else
	{
	  lpri = P_BITOR; rpri = P_BITXOR;
	}
      break;
    case kind_andand:
      lpri = P_AND; pri = P_AND; rpri = P_BITOR; break;
    case kind_oror:
      pri = P_OR;
      if (CONSERVATIVE_PARENS)
	lpri = rpri = P_BITOR;
      else
	{
	  lpri = P_OR; rpri = P_AND; 
	}
      break;
    case kind_assign: case kind_plus_assign: case kind_minus_assign: 
    case kind_times_assign: case kind_divide_assign: case kind_modulo_assign:
    case kind_lshift_assign: case kind_rshift_assign: case kind_bitand_assign:
    case kind_bitor_assign: case kind_bitxor_assign:
      lpri = P_CAST; pri = P_ASSIGN; rpri = P_ASSIGN; break;
    default: assert(0); return;
    }
  OPEN(pri);
  prt_expression(e->arg1, lpri);
  set_location(e->location);
  output(" %s ", op);
  prt_expression(e->arg2, rpri);
  CLOSE(pri);
}

void prt_lexical_cst(lexical_cst e, int context_priority)
{
  set_location(e->location);
  output_cstring(e->cstring);
}

void prt_string(string e, int context_priority)
{
  expression s;

  scan_expression (s, e->strings)
    prt_expression(s, P_TOP);
}

void prt_init_list(init_list e, int context_priority)
{
  set_location(e->location);
  output("{ ");
  prt_expressions(e->args, TRUE);
  output(" }");
}

void prt_designator(designator dl)
{
  designator d;

  scan_designator (d, dl)
    switch (d->kind)
      {
      case kind_designate_field: {
	designate_field df = CAST(designate_field, d);

	output(".");
	output_cstring(df->cstring);
	break;
      }
      case kind_designate_index: {
	designate_index di = CAST(designate_index, d);

	output("[");
	prt_expression(di->arg1, P_ASSIGN);
	if (di->arg2)
	  {
	    output(" ... ");
	    prt_expression(di->arg2, P_ASSIGN);
	  }
	output("] ");
	break;
      }
      default: assert(0);
      }
}

void prt_init_specific(init_specific e, int context_priority)
{
  set_location(e->location);
  prt_designator(e->designator);
  output(" = ");
  prt_expression(e->init_expr, P_ASSIGN);
}

void prt_statement(statement s)
{
  switch (s->kind)
    {
      PRTCASE(asm_stmt, s);
      PRTCASE(compound_stmt, s);
      PRTCASE(if_stmt, s);
      PRTCASE(labeled_stmt, s);
      PRTCASE(expression_stmt, s);
      PRTCASE(while_stmt, s);
      PRTCASE(dowhile_stmt, s);
      PRTCASE(switch_stmt, s);
      PRTCASE(for_stmt, s);
      PRTCASE(break_stmt, s);
      PRTCASE(continue_stmt, s);
      PRTCASE(return_stmt, s);
      PRTCASE(goto_stmt, s);
      PRTCASE(computed_goto_stmt, s);
      PRTCASE(empty_stmt, s);
      PRTCASE(atomic_stmt, s);
    default: assert(0); return;
    }
}

static void prt_as_compound(statement s)
{
  if (!is_compound_stmt(s))
    outputln("{");
  prt_statement(s);
  if (!is_compound_stmt(s))
    {
      startline();
      outputln("}");
    }
}

void prt_compound_stmt(compound_stmt s)
{
  statement s1;

  set_location(s->location);
  outputln("{");
  indent();
  if (s->id_labels)
    {
      id_label l;

      output("__label__ ");
      scan_id_label (l, s->id_labels)
	{
	  prt_id_label(l);
	  if (l->next) 
	    output(", ");
	}
      outputln(";");
    }
  if (s->decls)
    {
      prt_compound_declarations(s->decls);
      newline();
    }

  scan_statement (s1, s->stmts)
    prt_statement(s1);

  unindent();
  outputln("}");
}

void prt_compound_declarations(declaration dlist)
{
  declaration d;

  scan_declaration (d, dlist)
    prt_compound_declaration(d);
}

void prt_compound_declaration(declaration d)
{
  startline();
  switch (d->kind)
    {
      PRTCASE(data_decl, d);
      PRTCASE(extension_decl, d);
      PRTCASE(function_decl, d);
    default: assert(0); break;
    }
}

void prt_asm_stmt(asm_stmt s)
{
  prt_asm_stmt_plain(s);
  output(";");
}

void prt_asm_stmt_plain(asm_stmt s)
{
  set_location(s->location);
  output(" __asm ");
  if (s->qualifiers)
    prt_type_elements(s->qualifiers, 0);
  output("(");
  prt_expression(s->arg1, P_TOP);
  if (s->asm_operands1 || s->asm_operands2 || s->asm_clobbers)
    {
      output(" : ");
      prt_asm_operands(s->asm_operands1);

      if (s->asm_operands2 || s->asm_clobbers)
	{
	  output(" : ");
	  prt_asm_operands(s->asm_operands2);

	  if (s->asm_clobbers)
	    {
	      output(" : ");
	      prt_expressions(CAST(expression, s->asm_clobbers), TRUE);
	    }
	}
    }
  output(")");
}

void prt_asm_operands(asm_operand olist)
{
  asm_operand o;

  scan_asm_operand (o, olist)
    {
      prt_asm_operand(o);
      if (o->next)
	output(", ");
    }
}

void prt_asm_operand(asm_operand o)
{
  prt_string(o->string, P_TOP);
  output("(");
  prt_expression(o->arg1, P_TOP);
  output(")");
}

void prt_if_stmt(if_stmt s)
{
#if 0
  if (s->condition->cst && constant_knownbool(s->condition->cst))
    {
      if (constant_boolvalue(s->condition->cst))
	prt_statement(s->stmt1);
      else if (s->stmt2)
	prt_statement(s->stmt2);
      else
	outputln(";");
      return;
    }
#endif

  set_location(s->location);
  output("if (");
  /* CONSERVATIVE_PARENS: force parens around assignment within if */
  prt_expression(s->condition, CONSERVATIVE_PARENS ? P_COND : P_TOP);
  output(") ");
  indent();
  prt_as_compound(s->stmt1);
  unindent();
  if (s->stmt2)
    {
      startline();
      output("else ");
      indent();
      prt_as_compound(s->stmt2);
      unindent();
    }
}

void prt_labeled_stmt(labeled_stmt s)
{
  prt_label(s->label);
  output(": ");
  indent();
  prt_statement(s->stmt);
  unindent();
}

void prt_expression_stmt(expression_stmt s)
{
  prt_expression(s->arg1, P_TOP);
  outputln(";");
}

void prt_while_stmt(while_stmt s)
{
  set_location(s->location);
  output("while (");
  /* CONSERVATIVE_PARENS: force parens around assignment within while */
  prt_expression(s->condition, CONSERVATIVE_PARENS ? P_COND : P_TOP);
  output(") ");
  indent();
  prt_statement(s->stmt);
  unindent();
}

void prt_dowhile_stmt(while_stmt s)
{
#if 0
  /* Elide do ... while (0) */
  if (s->condition->cst &&
      constant_knownbool(s->condition->cst) &&
      !constant_boolvalue(s->condition->cst))
    {
      prt_statement(s->stmt);
      return;
    }
#endif

  set_location(s->location);
  output("do ");
  indent();
  prt_statement(s->stmt);
  unindent();
  startline();
  output("while (");
  /* CONSERVATIVE_PARENS: force parens around assignment within do while */
  prt_expression(s->condition, CONSERVATIVE_PARENS ? P_COND : P_TOP);
  outputln(");");
}

void prt_switch_stmt(switch_stmt s)
{
  set_location(s->location);
  output("switch (");
  /* CONSERVATIVE_PARENS: force parens around assignment within switch */
  prt_expression(s->condition, CONSERVATIVE_PARENS ? P_COND : P_TOP);
  output(") ");
  indent();
  prt_statement(s->stmt);
  unindent();
}

void prt_for_stmt(for_stmt s)
{
  set_location(s->location);
  output("for (");
  if (s->arg1)
    prt_expression(s->arg1, P_TOP);
  output("; ");
  if (s->arg2)
    prt_expression(s->arg2, P_TOP);
  output("; ");
  if (s->arg3)
    prt_expression(s->arg3, P_TOP);
  output(") ");
  indent();
  prt_statement(s->stmt);
  unindent();
}  

void prt_break_stmt(break_stmt s)
{
  set_location(s->location);
  outputln("break;");
}

void prt_continue_stmt(continue_stmt s)
{
  set_location(s->location);
  outputln("continue;");
}

void prt_return_stmt(return_stmt s)
{
  set_location(s->location);
  if (s->arg1)
    {
      output("return ");
      prt_expression(s->arg1, P_TOP);
      outputln(";");
    }
  else
    outputln("return;");
}

void prt_goto_stmt(goto_stmt s)
{
  set_location(s->location);
  output("goto ");
  prt_id_label(s->id_label);
  outputln(";");
}

void prt_computed_goto_stmt(computed_goto_stmt s)
{
  set_location(s->location);
  output("goto *");
  prt_expression(s->arg1, P_TOP);
  outputln(";");
}

void prt_empty_stmt(empty_stmt s)
{
  set_location(s->location);
  outputln(";");
}

void prt_atomic_stmt(atomic_stmt s)
{
  struct location hack;

  set_location(s->location);
  outputln("{ __nesc_atomic_t __nesc_atomic = __nesc_atomic_start();");
  indent();
  prt_statement(s->stmt);

  /* The hack is to make debugging nicer: we make this new line appear 
     to be part of the previous line */
  hack = output_loc;
  hack.lineno--;
  set_location(&hack);
  outputln("__nesc_atomic_end(__nesc_atomic); }");
  unindent();
}

void prt_label(label l)
{
  switch (l->kind)
    {
      PRTCASE(id_label, l);
      PRTCASE(case_label, l);
      PRTCASE(default_label, l);
    default: assert(0); return;
    }
}

void prt_id_label(id_label l)
{
  set_location(l->location);
  output_stripped_cstring(l->cstring);
}

void prt_case_label(case_label l)
{
  set_location(l->location);
  output("case ");
  prt_expression(l->arg1, P_ASSIGN);
  if (l->arg2)
    {
      output(" ... ");
      prt_expression(l->arg2, P_ASSIGN);
    }
}

void prt_default_label(default_label l)
{
  set_location(l->location);
  output("default");
}
