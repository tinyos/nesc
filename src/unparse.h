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

#ifndef UNPARSE_H
#define UNPARSE_H

void unparse(FILE *to, declaration program) deletes;
void unparse_start(FILE *to);
void unparse_end(void) deletes;
void enable_line_directives(void);
void disable_line_directives(void);
void set_function_separator(char *sep);
void enable_documentation_mode(void);
void disable_documentation_mode(void);

struct location output_location(void);
void output(char *format, ...) __attribute__((format (printf, 1, 2)));
void outputln(char *format, ...) __attribute__((format (printf, 1, 2)));
void copy_file_to_output(char *filename);
void newline(void);
void indent(void);
void unindent(void);
void startline(void);
void startline_noindent(void);

const char *binary_op_name(ast_kind kind);

/* Precedence levels */
#define P_TOP -1
#define P_COMMA 0
#define P_ASSIGN 1
#define P_COND 2
#define P_OR 3
#define P_AND 4
#define P_BITOR 5
#define P_BITXOR 6
#define P_BITAND 7
#define P_EQUALS 8
#define P_REL 9
#define P_SHIFT 10
#define P_PLUS 11
#define P_TIMES 12
#define P_CAST 13
#define P_CALL 14

void prt_data_decl(data_decl d);
void prt_toplevel_declarations(declaration d);
void prt_toplevel_declaration(declaration d);
void prt_nelements(expression array);
void prt_expressions(expression elist, bool isfirst);
void prt_expression(expression e, int context_priority);
void prt_type_elements(type_element elements, bool duplicate);

typedef enum {
  psd_not_star = 1,
  psd_rename_parameters = 2,
  psd_rename_identifier = 4,
  psd_print_default = 8,
  psd_skip_container = 16
} psd_options;

void prt_declarator(declarator d, type_element elements, attribute attributes,
		    data_declaration ddecl, psd_options options);
void prt_simple_declarator(declarator d, data_declaration ddecl,
			   psd_options options);
void prt_parameters(declaration gparms, declaration parms, psd_options options);
bool prt_parameter(declaration parm, bool first, bool lastforward, psd_options options);
void prt_ddecl_full_name(data_declaration ddecl, psd_options options);

void prt_function_body(function_decl d);

#endif
