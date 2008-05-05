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

#ifndef DECLS_H
#define DECLS_H

#include "AST.h"
#include "env.h"
#include "c-lex.h"

typedef struct environment *environment;
typedef struct data_declaration *data_declaration;
typedef struct label_declaration *label_declaration;

#include "nesc-decls.h"
#include "nesc-uses.h"
#include "cval.h"

/* Types representing declarations */

typedef struct field_declaration {
  struct tag_declaration *containing_tag;
  struct field_declaration *next; /* Next field in struct/union */
  const char *name;		/* May be NULL for bitfields (if NULL, bitwidth == 0) */
  type type;

  /* All '@'-style attributes attached to this declaration */
  dd_list/*nesc_attribute*/ attributes;

  field_decl ast;		/* May be null if copied from anonymous 
				   struct/union */
  cval bitwidth;		/* for bitfields, cval_top otherwise */
  cval offset;			/* in bits, not bytes. Can be cval_top if
				   offset is not a compile-time constant */
  bool packed;			/* if packed attribute specified */

  /* In abstract configurations or modules: The latest instantiation
     of this declaration */
  struct field_declaration *instantiation;
} *field_declaration;

/* A struct, union or enum */
typedef struct tag_declaration {
  int kind; /* One of kind_{struct/union/enum/attribute}_ref */
  const char *name; /* NULL for anonynous struct/union/enum */
  type reptype; /* The type used to represent an enum, NULL for struct
		   and unions */
  /* All '@'-style attributes attached to this declaration */
  dd_list/*nesc_attribute*/ attributes;

  /* fields and fieldlist are only defined for structs/unions */
  env fields;
  field_declaration fieldlist;
  tag_ref definition;
  struct tag_declaration *shadowed; /* Any struct with the same tag defined in enclosing scope */
  bool defined, being_defined;
  bool fields_const, fields_volatile;
  bool transparent_union;	/* transparent_union attribute is present */
  bool collapsed;		/* TRUE if this struct/union was collapsed
				   into its parent. */

  cval size;			/* Can be cval_top if not compile-time constant
				   (due to variable-size arrays in struct) */
  cval alignment, user_alignment;
  bool packed;			/* if packed attribute specified */
  bool dumped;			/* TRUE if already added to dump list */
  bool Cname;			/* TRUE if has @C() attribute */

  nesc_declaration container;	/* as in data_declarations */

  /* Function this declaration occurs in (NULL if outside a function) */
  struct data_declaration *container_function;

  /* In abstract configurations or modules: The latest instantiation
     of this declaration */
  struct tag_declaration *instantiation;
  struct tag_declaration *instanceof; /* Inside instantiated components: what this tag is an instance of */

  /* Name of a macro to use in nesC's output for instances of this attribute - 
     if this is NULL, attributes are not printed */
  const char *macro_name;
  bool deputy_scope;		/* TRUE for deputy attributes (@deputy_scope()) */
} *tag_declaration;

typedef enum { decl_variable, decl_constant, decl_function,
	       decl_typedef, decl_error, decl_magic_string,	
	       decl_magic_function,
	       decl_interface_ref, decl_component_ref } data_kind;

typedef enum  {
  c_call_atomic = 1,		/* bit set if atomic calls to this fn */
  c_call_nonatomic = 2	/* bit set if non-atomic calls to this fn */
} call_contexts;

struct data_declaration {
  data_kind kind;
  const char *name;
  type type;
  /* For declaration numbering purposes. At this point, it has:
     a per-function numbering for local variables
     a per-module numbering for commands/events */
  long id;
  context use_summary;
  /* All '@'-style attributes attached to this declaration */
  dd_list/*nesc_attribute*/ attributes;
 
  /* Regular C: For extern's shadowing globals in inner scopes */
  /* nesC commands/events: point to original interface declaration */
  struct data_declaration *shadowed;

  /* In abstract configurations or modules: The latest instantiation
     of this declaration */
  struct data_declaration *instantiation;
  struct data_declaration *instanceof; /* Inside instantiated components: what this decl is an instance of */

  /* interface/module/configuration this declaration belongs to.
     NULL for declarations from C files */
  nesc_declaration container;

  /* Function this declaration occurs in (NULL if outside a function) */
  struct data_declaration *container_function;

  declaration definition; /* Pointer to actual definition, if any */
  declaration ast; /* Last declaration */
  expression initialiser; /* NULL if none. For type arguments, this gets set
			     to the argument type (type_argument node) */

  bool printed;			/* symbol info already printed */
  bool dumped;			/* TRUE if already added to dump list */
  bool islimbo; /* TRUE if comes from an extern declaration in an inner scope
		   (also true for implicit function declarations) */
  bool isexternalscope; /* == TREE_PUBLIC   */
  bool isfilescoperef; /* == DECL_EXTERNAL */
  bool needsmemory;   /* == TREE_STATIC   */

  /* isused is TRUE if declaration used. For parameters, there is a special
     use during parameter list declaration to support forward parameters:
       - a duplicate parameter declaration is allowed if isused is FALSE
         once a duplicate is seen, isused is set to TRUE
       - parameters are created with isused == TRUE
       - after the forward parameters are seen, they have their isused field
         set to FALSE */
  bool isused;
  bool in_system_header;
  bool Cname;			/* name is in C name space (don't rename!)
				   Set by the `C' attribute. */
  bool safe;			/* True if deputy safety checks should
				   be enabled */
  call_contexts spontaneous;	/* Call contexts for environmental calls
				   (main, interrupt handlers, e.g.). Set by
				   the `spontaneous', `interrupt' and
				   `signal' attributes */

  dd_list/*use*/ nuses;		/* List of uses of this identifier */

  /* For functions */
  enum { function_implicit, function_normal, function_static, function_nested,
         function_event, function_command }
    ftype;
  bool isinline;
  bool noinlinep;
  bool isexterninline;
  bool defined;			/* nesC: true if defined, false if used */
  bool suppress_definition;	/* Prevent code generation */
  bool uncallable;		/* Error if called */
  bool async;			/* True if async declared (cmd/event) or
				   inferred (C function) */
  bool actual_async;		/* Inferred value for async */
  /* The call_contexts summarise the runtime contexts in which this fn
     might be called. So if all calls to f are in atomic statements,
     and f calls g outside an atomic statement, then 
      g->call_contexts == c_call_atomic
  */
  call_contexts call_contexts;
  call_contexts extra_contexts;	/* Some extra, hidden call contexts (used to
				   support __nesc_enable_interrupt) */
  bool makeinline;		/* Mark this function inline when generating code */
  gnode ig_node;		/* inline-graph node for this function */
  struct data_declaration *interface;	/* nesC: interface this cmd/event belongs to */
  typelist oldstyle_args; /* Type of arguments from old-style declaration */
  dd_list/*iduse*/ fn_uses;	/* list of uses of identifiers in this fn */
  struct connections *connections; /* See nesc-generate.c: what this command
				      or event is connected to. */
  /* folding function for magic functions. pass is 0 when constant
     folding during parsing, and goes from 1 to n for each final
     constant folding pass (after all components loaded) */
  known_cst (*magic_fold)(function_call fcall, int pass);

  /* For variables */
  enum { variable_register, variable_static, variable_normal } vtype;
  bool islocal;			/* True for non-static local vars */
  bool isparameter; 		/* implies islocal */
  bool async_access;		/* Some kind of access in an async context */
  bool async_write;		/* A write in async context */
  bool norace;

  /* For constants */
  known_cst value;
  bool substitute;		/* Substitute value when unparsing */

  /* For magic_strings */
  cstring schars;

  /* For interface_ref */
  nesc_declaration itype;
  environment functions;
  bool required;
  typelist gparms;

  /* For component_ref */
  nesc_declaration ctype;

  /* For documentation comments */
  struct docstring doc;

  /* For typedefs of network base types */
  data_declaration encoder, decoder; /* encoder and decoder functions */
  data_declaration bf_encoder, bf_decoder; /* bitfield encoder and decoder functions */
  bool isbe;				   /* TRUE for big-endian types */
  type basetype;		/* underlying non-network type (e.g., uint8_t) */

  /* For type variables (some decl_typedefs). Regular typedefs (not type
     variables) have typevar_none here. */
  enum { typevar_none,
	 typevar_normal, typevar_integer, typevar_number } typevar_kind;
};

struct label_declaration {
  const char *name;
  bool explicitly_declared;
  bool used;
  id_label firstuse; /* Never NULL */
  id_label definition; /* NULL until actually defined */
  function_decl containing_function;
  atomic_stmt containing_atomic;
};

struct environment
{
  struct environment *sameregion parent;
  function_decl fdecl;
  bool parm_level : 1;
  bool global_level : 1;	/* Both system and component */
  bool deputy_scope : 1;
  env sameregion id_env;
  env sameregion tag_env;
};

extern data_declaration bad_decl;

#endif
