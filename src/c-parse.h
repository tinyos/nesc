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

#ifndef C_PARSE_H
#define C_PARSE_H

#include "AST.h"

struct yystype {
  union {
    void *ptr;
    asm_operand asm_operand;
    asm_stmt asm_stmt;
    attribute attribute;
    gcc_attribute gcc_attribute;
    nesc_attribute nesc_attribute;
    lexical_cst constant;
    declaration decl;
    declarator declarator;
    nested_declarator nested;
    expression expr;
    id_label id_label;
    label label;
    node node;
    statement stmt;
    conditional_stmt cstmt;
    for_stmt for_stmt;
    string string;
    type_element telement;
    asttype type;
    word word;
    designator designator;
    interface_ref iref;
    component_ref cref;
    connection conn;
    endpoint ep;
    parameterised_identifier pid;
    implementation impl;
    environment env;
    dd_list fields;
    char *docstring;
    tag_declaration tdecl;
    
    struct {
      location location;
      int i;
    } itoken;

    struct {
      expression expr;
      int i;
    } iexpr;

    struct {
      statement stmt;
      int i;
    } istmt;
  } u;

  struct {
    location location;
    cstring id;
    data_declaration decl;
  } idtoken;

  bool abstract;
};

#define YYSTYPE struct yystype

/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
extern region parse_region;

/* TRUE if currently parsing an expression that will not be evaluated (argument
   to alignof, sizeof. Currently not typeof though that could be considered
   a bug) */
bool unevaluated_expression(void);

node parse(void) deletes;
/* Effects: parses the file set up via set_input/start_lex
   Returns: the file's parse tree (may be NULL in some error cases)
*/

declaration make_error_decl(void);
declarator make_identifier_declarator(location l, cstring id);

#endif
