/* This file is part of the nesC compiler.
   Copyright (C) 2008 Intel Corporation

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

#ifndef NESC_NDOC_H
#define NESC_NDOC_H

struct docstring
{
  const char *short_s, *long_s;
  location loc;
};

struct doctag
{
  int lineno;			/* relative to start of string, first line=0 */
  const char *tag;		/* @param, @result, etc */
  const char *args[];		/* arguments to key:
				   @param(1): parameter name or declaration
				   @result(1): result type or empty string
				*/
};

bool get_latest_docstring(struct docstring *doc, region tags_r, dd_list *tags);

struct data_declaration;
struct AST_function_declarator;
void handle_ddecl_doc_tags(location docloc, struct data_declaration *ddecl,
			   dd_list tags);
void handle_fdecl_doc_tags(location docloc, struct data_declaration *ddecl,
			   struct AST_function_declarator *fd, dd_list tags);

void ignored_doctag(location docloc, struct doctag *tag);
void ignored_doctags(location docloc, dd_list tags);

#endif
