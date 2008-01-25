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

#ifndef SEMANTICS_H
#define SEMANTICS_H

/* Storage specifier "flags" (inline, e.g.) */
typedef enum {
  scf_inline = 1,
  scf_default = 2,
  scf_async = 4,
  scf_norace = 8
} scflags;

/* Predefined __builtin_va_list type */
extern type builtin_va_list_type;
extern data_declaration builtin_va_arg_decl;
extern data_declaration builtin_constant_p;

#include "cstring.h"
#include "decls.h"
#include "c-lex-state.h"

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */
void pending_xref_error(void);

extern environment global_env;

data_declaration lookup_id(const char *s, bool this_level_only);
data_declaration lookup_global_id(const char *s);

/* If elements is 'struct foo' shadow tag foo in the current environment */
void shadow_tag(type_element elements);

/* Like shadow_tag, but a pedwarn has already been issued if warned == 1 */
void shadow_tag_warned(type_element elements, int warned);


declarator finish_array_or_fn_declarator(declarator nested, nested_declarator d);

/* Start definition of function 'elements d' with attributes attribs.
   nested is true for nested function definitions.
   Returns false in case of error.
   Sets current.function_decl to the declaration for this function */
bool start_function(type_element elements, declarator d, attribute attribs,
		    bool nested);

/* Add old-style parameter declarations old_parms to the current function */
void store_parm_decls(declaration old_parms);

/* End definition of current function, furnishing it it's body. */
declaration finish_function(statement body);


/* Start a new scope */
void pushlevel(bool parm_level);

/* Pop back to enclosing scope */
environment poplevel(void);


/* Categories of variable declarations */
enum { var_typedef, var_register, var_normal, var_static, var_extern };

/* Start definition of variable 'elements d' with attributes
   extra_attributes and attributes, asm specification astmt.
   If initialised is true, the variable has an initialiser.
   Returns the declaration for the variable.
*/
declaration start_decl(declarator d, asm_stmt astmt, type_element elements,
		       bool initialised, attribute attributes);

/* Finish definition of decl, furnishing the optional initialiser init.
   Returns decl */
declaration finish_decl(declaration decl, expression init);

/* Create definition of function parameter 'elements d' with attributes
   extra_attributes and attributes.
   Returns the declaration for the parameter.
*/
declaration declare_parameter(declarator d, type_element elements,
			      attribute attributes);

/* Allow parameters to be redeclared. mark_forward should be TRUE
   if these are "forward" parameter declarations (gcc extension) */
void allow_parameter_redeclaration(declaration parms, bool mark_forward);

declaration declare_old_parameter(location l, cstring id);


/* Start definition of struct/union (indicated by skind) type tag. */
type_element start_struct(location l, AST_kind skind, word tag);

/* Finish definition of struct/union furnishing the fields and attribs.
   Returns t */
type_element finish_struct(type_element t, declaration fields,
			   attribute attribs);

/* Return a reference to struct/union/enum (indicated by skind) type tag */
type_element xref_tag(location l, AST_kind skind, word tag);

/* Start definition of struct/union (indicated by skind) type tag. */
type_element start_enum(location l, word tag);

/* Finish definition of enum furnishing the names and attribs.
   Returns t */
type_element finish_enum(type_element t, declaration names,
			 attribute attribs);

/* Create declaration of field 'elements d : bitfield' with attributes
   extra_attributes and attributes.
   d can be NULL, bitfield can be NULL, but not both at the same time.
   Returns the declaration for the field.
*/
declaration make_field(declarator d, expression bitfield,
		       type_element elements, attribute attributes);

declaration make_enumerator(location loc, cstring id, expression value);


/* Create and return type 'elements d' where d is an absolute declarator */
asttype make_type(type_element elements, declarator d);

/* Returns name of r */
char *rid_name(rid r);

/* If statement list l1 ends with an unfinished label, attach l2 to that
   label. Otherwise attach l2 to the end of l1 */
statement chain_with_labels(statement l1, statement l2);

void declarator_name(declarator d, const char **oname, const char **iname);
const char *nice_declarator_name(declarator d);
/* Returns: a user-friendly name for declarator d, allocated in 
     current.fileregion if necessary
*/

data_declaration lookup_id(const char *s, bool this_level_only);

data_declaration implicitly_declare(identifier fnid);

void push_label_level(void);
void pop_label_level(void);

void init_data_declaration(data_declaration dd, declaration ast,
			   const char *name, type t);
data_declaration declare(environment env, data_declaration from,
			 bool ignore_shadow);

/* Build a declaration object for a string */
data_declaration declare_string(const char *name, cstring value, bool wide);

environment new_environment(region r, environment parent,
			    bool global_level, bool parm_level);

tag_declaration declare_tag(tag_ref t);
tag_declaration lookup_tag(tag_ref t, bool this_level_only);
tag_declaration declare_global_tag(tag_ref t);
tag_declaration lookup_global_tag(tag_ref t);

void init_semantics(void);
void start_semantics(source_language l, nesc_declaration container,
		     environment parent_env);

struct semantic_state
{
  region fileregion;		/* A per-file region.
				   "Renewed" at the start of each file and
				   of each implementation section */
  source_language language;	/* The current language */
  environment env;		/* The current environment */
  nesc_declaration file;	/* The nesC entity of the file being compiled
				   (different from container while dealing
				   with C declarations at the beginning of
				   the file) */
  nesc_declaration container;	/* The nesC entity being compiled (NULL for C) */
  function_decl function_decl;	/* The function currently being defined */
  tag_ref pending_invalid_xref;	/* Internal use */
  enum {			/* Within a component specification, are we: */
    spec_normal,		/*   - neither in provides or in uses */
    spec_provides,		/*   - in a provides section */
    spec_uses			/*   - in a uses section */
  } spec_section;	

  atomic_stmt in_atomic;		/* The lexically containing atomic statement
				   (NULL for none) */
  char *preprocessed_file;	/* Temp file holding preprocessor output */
  struct lex_state lex;
};

extern struct semantic_state current;

extern dd_list spontaneous_calls;
/* List of spontaneously-called functions */

void check_variable_scflags(scflags scf,
			    location l, const char *kind, const char *name);

void parse_declarator(type_element modifiers, declarator d, bool bitfield, 
		      bool require_parm_names,
		      int *oclass, scflags *oscf,
		      const char **ointf, const char **oname,
		      type *ot, bool *owarn_defaulted_int,
		      function_declarator *ofunction_declarator,
		      dd_list *oattributes);

int duplicate_decls(data_declaration newdecl, data_declaration olddecl,
		    bool different_binding_level, bool newinitialised);

void check_array_size(expression size, const char *printname);

void layout_enum_start(tag_declaration tdecl);
void layout_enum_end(tag_declaration tdecl);
known_cst layout_enum_value(enumerator e);
void layout_struct(tag_declaration tdecl);
const char *tagkind_name(int tagkind);
bool handle_mode_attribute(location loc, data_declaration ddecl, const char *mode);
/* Make "word" argument of attributes into an expression */
expression make_attr_args(location loc, cstring id, expression args);
data_declaration declare_builtin_type(const char *name, type t);

#endif
