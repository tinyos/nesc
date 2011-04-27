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

#include "parser.h"

/* -f flags.  */

/* Nonzero means just do syntax checking; don't output anything.  */
int flag_syntax_only;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */
int flag_pedantic_errors;

/* Tag all structures with __attribute__(packed) */
int flag_pack_struct;

/* Options controlling warnings */

/* Don't print warning messages.  -w.  */
int inhibit_warnings;

/* Print various extra warnings.  -W.  */
int extra_warnings;

/* Treat warnings as errors.  -Werror.  */
int warnings_are_errors;

/* Nonzero to warn about unused local variables.  */
int warn_unused;

/* Nonzero to warn about variables used before they are initialized.  */
int warn_uninitialized;

/* Nonzero means warn about all declarations which shadow others.   */
int warn_shadow;
int error_shadow; /* Make shadow an error */

/* Warn if a switch on an enum fails to have a case for every enum value.  */
int warn_switch;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */
int warn_return_type;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */
int warn_cast_align;

/* Nonzero means warn about any identifiers that match in the first N
   characters.  The value N is in `id_clash_len'.  */
int warn_id_clash;
unsigned id_clash_len;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */
int warn_larger_than;
unsigned larger_than_size;

/* Nonzero means warn if inline function is too large.  */
int warn_inline;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */
int warn_aggregate_return;

/* Nonzero means `$' can be in an identifier.  */
int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */
int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */
int flag_no_asm;

/* Nonzero means environment is hosted (i.e., not freestanding) */
int flag_hosted;

/* Nonzero means warn about implicit declarations.  */
int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */
int warn_write_strings;

/* Nonzero means warn about sizeof (function) or addition/subtraction
   of function pointers.  */
int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */
int warn_strict_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */
int warn_redundant_decls;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */
int warn_nested_externs;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */
int warn_cast_qual;

/* Nonzero means warn when casting a function call to a type that does
   not match the return type (e.g. (float)sqrt() or (anything*)malloc()
   when there is no previous declaration of sqrt or malloc.  */
int warn_bad_function_cast;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */
int warn_traditional;

/* Warn about *printf or *scanf format/argument anomalies. */
int warn_format;

/* Warn about a subscript that has type char.  */
int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */
int warn_conversion;

/* Warn if main is suspicious. */
int warn_main;

/* Nonzero means warn about use of multicharacter literals.  */
int warn_multichar = 1;

/* Nonzero means do some things the same way PCC does.  */
int flag_traditional;

/* Nonzero means to allow single precision math even if we're generally
   being traditional. */
int flag_allow_single_precision;

/* Nonzero means warn about suggesting putting in ()'s.  */
int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */
int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */
int warn_sign_compare;

/* Nonzero means message about use of implicit function declarations;
 1 means warning; 2 means error. */
int mesg_implicit_function_declaration;

/* Report pedantic warnings if true */
bool pedantic;

/* Nonzero means warn about use of implicit int. */
int warn_implicit_int;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */
int warn_missing_prototypes;

/* Nonzero means warn for any global function def
   without separate previous decl.  */
int warn_missing_declarations;

/* Nonzero means `char' should be signed.  */
int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */
int flag_short_enums;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */
int flag_signed_bitfields = 1;

/* Nonzero means don't run real cc1 afterwards */
int flag_parse_only;

/* The value of the -nesc-path option */
char *cmdline_nesc_path;

/* Nonzero means suppress the dbg and dbg_clear functions (replace them
   by macros in output). This is necessary because gcc won't inline
   varargs functions */
int flag_no_debug;

/* Nonzero to suppress automatic addition of inline keywords 
   (but the "wiring" functions are still marked inline) */
int flag_no_inline;

/* Nonzero means to output macro defs in the generated C file */
int flag_save_macros;

/* Nonzero means modify identifier and declaration output during code
   generation to accomodate nido */
bool use_nido;

/* specifies the maximum number of nodes that can be simulated at one time */
char* nido_num_nodes = "1000";

/* the expression that gives the current mote number */
char *nido_mote_number = "tos_state.current_node";

/* Nonzero for -v */
int flag_verbose;

/* Warn if there are unexpected documentation strings in the code */
int warn_unexpected_docstring;

/* Warn when function pointers are used */
int warn_fnptr;

/* Warn when data races are detected */
int warn_data_race;

/* Warn when async keyword is violated */
int warn_async;

/* Warn when no combiner function and multiple fns called */
int warn_no_combiner;

/* If true, warn_fnptr, warn_data_race, warn_async and warn_no_combiner
   are treated as errors */
int nesc_error;

/* diff processing enabled if diff_output is not NULL 
   (diff_input is NULL for orignal program, non-NULL to reduce diff size) */\
char *diff_input, *diff_output;

/* If true, rewrite post/task to use interfaces+wiring rather than calls to
   a TOS_post function. */
int flag_use_scheduler;

/* If true, we're using a mingw based gcc from a cygwin environment. We
   should fix filenames before invoking gcc. */
int flag_mingw_gcc;

/* If true, check for atomic statements whose body is guaranteed to be
   atomic (e.g., one single-byte read) */
int nesc_optimise_atomic;

/* Warn about possibly nested block comments, and C++ comments
   spanning more than one physical line */
int warn_comments;

/* Warn if trigraphs are encountered that might affect the meaning of
   the program */
int warn_trigraphs;

/* Warn about macros defined in the main file that are not used */
int warn_unused_macros;

/* Warn about stray tokens after #elif and #endif */
int warn_endif_labels;

/* Do not suppress warnings from system headers */
int warn_system_headers;

/* Warn if an undefined macro is used in an #if directive */
int warn_undef;

/* Warn about user-specified include directories that do not exist */
int warn_missing_include_dirs;

/* Warn about use of multi-character character constants */
int warn_multichar;

/* Support ISO C trigraphs */
int flag_trigraphs;

/* Do not search standard system include directories */
int flag_nostdinc;

/* Do not predefine system-specific and GCC-specific macros */
int flag_undef;

/* True if compiling for deputy */
int flag_deputy;

/* True if a module lacking a @safe() or @unsafe() attribute defaults
   to safe; has no effect if flag_deputy is not true; this default can
   be overridden by -fnesc-default-safe or -fnesc-default-unsafe */
int flag_default_safe;

/* True if transforming plain C code */
int flag_c;

/* True if target-specific extensions should be output as gcc
   attributes rather than in their original syntax. */
int flag_gccize;

/* Print included file names (-H) */
int print_include_names;

