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

#ifndef NESC_SEMANTICS_H
#define NESC_SEMANTICS_H

struct environment;

#include "nesc-decls.h"

extern interface the_interface;
extern component the_component;
extern declaration cdecls;

source_language pick_language_from_filename(const char *name);

const char *element_name(region r, const char *path);
/* Returns: Return the "identifier part"
     of path, i.e., remove any directory and extension
     The returned string is allocated in region r.
*/

environment compile(location loc, source_language l,
		    const char *name, bool name_is_path,
		    nesc_declaration container, struct environment *parent_env);

void check_nesc_declaration(source_language l, nesc_declaration nd,
			    struct environment *env, nesc_decl ast);

type get_actual_function_type(type t);
/* Returns: The actual function type for a (possibly generic) type t
     representing the type of a function/command/event
 */

function_declarator ddecl_get_fdeclarator(data_declaration fndecl);
/* Effects: Returns fndecl's function_declarator
   Requires: fndecl represent a function
*/

declaration ddecl_get_gparms(data_declaration fndecl);
/* Effects: Returns the declaration list for fndecl's generic parameters 
   Requires: fndecl represent a function
*/

bool ddecl_is_command_or_event(data_declaration decl);

bool nesc_attribute(attribute a);
/* Returns: TRUE if a is a nesc-specific attribute
 */

#endif
