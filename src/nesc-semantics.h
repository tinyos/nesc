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

void init_internal_nesc_attributes(void);
/* Effects: Define internal nesC @-style attributes
 */

bool nesc_filename(const char *name);

const char *element_name(region r, const char *path);
/* Returns: Return the "identifier part"
     of path, i.e., remove any directory and extension
     The returned string is allocated in region r.
*/

node compile(location loc, nesc_declaration container,
	     const char *name, bool name_is_path);

nesc_declaration load(source_language sl, location l,
		      const char *name, bool name_is_path);

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

bool nesc_attributep(gcc_attribute a);
/* Returns: TRUE if a is a nesc-specific attribute
 */

const char *language_name(source_language l);

nesc_decl dummy_nesc_decl(location loc, nesc_declaration ndecl);
void build(nesc_decl ast);
nesc_declaration start_nesc_entity(source_language sl, word name);

bool is_module_variable(data_declaration ddecl);
/* Returns: TRUE if ddecl is a module variable
   (this includes is_module_local_static)
 */

bool is_module_local_static(data_declaration ddecl);
/* Returns: TRUE if ddecl is a local static variable inside a module
 */

const char *make_intf_printname(const char *iname, const char *fname);
/* Returns: string "iname.fname" allocated in current.fileregion
 */

const char *decl_printname(data_declaration ddecl);
/* Returns: The printable name for ddecl 
     (interface.name for commands or events in interfaces,
      just the name otherwise)
     Any necessary memory is allocated in current.fileregion
*/

data_declaration get_function_ddecl(expression e);
/* Returns: If e denotes a specific function, return its data_declaration
     Otherwise return NULL
*/

void handle_combine_attribute(location loc, const char *combiner, type *t);
/* Effects: handle combine attribute specifying function 'combiner', 
     modifying *t as appropriate
 */

void handle_nxbase_attribute(location loc, bool be, bool allow_bf,
			     const char *fnbasename, data_declaration ddecl);
/* Effects: handle network type attribute specifying functions 
     ntoh_fnbasename and hton_fnbasename as the decoding and encoding
     functions respectively.
     If allow_bf is true, then ntohbf_fnbasename and htonbf_fnbasename
     are the bitfield decoding and encoding functions repsectively.
     If be is true, this is a big endian type (transitions between
     big and little-endian bitfields cause alignemnt to the next byte
     inside a struct).
     Modifies ddecl to reflect it's status as a base network type.
 */

declaration declare_template_parameter(declarator d, type_element elements,
				       attribute attributes);
declaration declare_type_parameter(location l, cstring id, attribute attribs,
				   dd_list extra_attr);

expression make_type_argument(asttype t);

nesc_declaration original_component(nesc_declaration c);

/* Some macros to make nesc_error easier to deal with */
#define nesc_warning (nesc_error ? error : warning)
#define nesc_warning_with_location (nesc_error ? error_with_location : warning_with_location)

data_declaration declare_function(location loc, const char *name, type signature);
/* Effects: If 'name' is already declared, check that it is a function with
     the specified signature.
     If it isn't declared, declare it as a function with the specified
     signature.
   Returns: data_declaration for the function, or NULL if an error was
     reported to the user.
*/

nesc_declaration ddecl_container(data_declaration ddecl);
nesc_declaration tdecl_container(tag_declaration tdecl);

void check_name(const char *name);

#endif
