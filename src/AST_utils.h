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

#ifndef AST_UTILS_H
#define AST_UTILS_H

#define IS_A(n, k) \
  ((n)->kind >= (k) && (n)->kind <= AST_post_kind[(k) - kind_node])

data_declaration get_parameter(declaration d);
function_declarator get_fdeclarator(declarator d);
bool oldstyle_function(function_decl fn);
bool is_void_parms(declaration parms);
bool is_localvar(expression e);
compound_stmt parent_block(node n);
function_decl parent_function(node n);
expression expression_of_stmt(compound_expr ce); /* Return expression which is value of compound statement expression, or NULL of node ("void" compound expression) */
bool expression_used(expression e);

data_declaration base_identifier(data_declaration d);
bool same_function(data_declaration d1, data_declaration d2);
bool call_to(data_declaration fnd, function_call fce);
expression ignore_fields(expression e);

/* True for expressions whose value is known to be 0 (but which
   may not be constant expressions) */
bool zero_expression(expression e);

expression build_int_constant(region r, location loc, type t, largest_int c);
expression build_uint_constant(region r, location loc, type t, largest_uint c);
#define build_zero(r, loc) build_int_constant(r, loc, int_type, 0)

expression build_identifier(region r, location loc, data_declaration id);

cval value_of_enumerator(enumerator e);

int asm_rwmode(string s);
/* Return: The first char of asm operand mode specifier s, this indicates
     the r/w mode for the operand (see gcc docs)
     The result is -1 if s is the empty string.
*/

declaration ignore_extensions(declaration d);
/* Ignore extension_decls at d, returning the "real" declaration */

tag_declaration get_unnamed_tag_decl(data_decl decl);
/* Returns:. Check to see if it is a struct or
     union, and if so return that struct or union's declaration */

const char *nice_field_name(const char *s);
/* Returns: "(anonymous)" if s == NULL, s otherwise
     This helps printing the name of potentially unnamed entities
 */

const char *tagkind_name(int tagkind);

conditional conditional_lvalue(expression e);

data_declaration string_ddecl(expression s);

char *ddecl2str(region r, data_declaration str);
/* Returns: a newly allocated string (in region r) for the string
     specified by str, or NULL if str contains wide characters
   Requires: str->kind == decl_magic_string
*/

bool is_attr_name(const char *arg, const char *name);
/* Returns: True if arg is name or __name__ */

type_element interesting_element(type_element elems);

#endif
