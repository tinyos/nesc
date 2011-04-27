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

#ifndef FLAGS_H
#define FLAGS_H

/* -f flags.  */

/* Nonzero means just do syntax checking; don't output anything.  */
extern int flag_syntax_only;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */
extern int flag_pedantic_errors;

/* Tag all structures with __attribute__(packed) */
extern int flag_pack_struct;

/* Options controlling warnings */

/* Don't print warning messages.  -w.  */
extern int inhibit_warnings;

/* Print various extra warnings.  -W.  */
extern int extra_warnings;

/* Treat warnings as errors.  -Werror.  */
extern int warnings_are_errors;

/* Nonzero to warn about unused local variables.  */
extern int warn_unused;

/* Nonzero to warn about variables used before they are initialized.  */
extern int warn_uninitialized;

/* Nonzero means warn about all declarations which shadow others.   */
extern int warn_shadow;
extern int error_shadow; /* Make shadow an error */

/* Warn if a switch on an enum fails to have a case for every enum value.  */
extern int warn_switch;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */
extern int warn_return_type;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */
extern int warn_cast_align;

/* Nonzero means warn about any identifiers that match in the first N
   characters.  The value N is in `id_clash_len'.  */
extern int warn_id_clash;
extern unsigned id_clash_len;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */
extern int warn_larger_than;
extern unsigned larger_than_size;

/* Nonzero means warn if inline function is too large.  */
extern int warn_inline;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */
extern int warn_aggregate_return;

/* Nonzero means `$' can be in an identifier.  */
extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */
extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */
extern int flag_no_asm;

/* Nonzero means environment is hosted (i.e., not freestanding) */
extern int flag_hosted;

/* Nonzero means warn about implicit declarations.  */
extern int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */
extern int warn_write_strings;

/* Nonzero means warn about sizeof (function) or addition/subtraction
   of function pointers.  */
extern int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */
extern int warn_strict_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */
extern int warn_redundant_decls;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */
extern int warn_nested_externs;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */
extern int warn_cast_qual;

/* Nonzero means warn when casting a function call to a type that does
   not match the return type (e.g. (float)sqrt() or (anything*)malloc()
   when there is no previous declaration of sqrt or malloc.  */
extern int warn_bad_function_cast;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */
extern int warn_traditional;

/* Warn about *printf or *scanf format/argument anomalies. */
extern int warn_format;

/* Warn about a subscript that has type char.  */
extern int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */
extern int warn_conversion;

/* Warn if main is suspicious. */
extern int warn_main;

/* Nonzero means warn about use of multicharacter literals.  */
extern int warn_multichar;

/* Nonzero means do some things the same way PCC does.  */
extern int flag_traditional;

/* Nonzero means to allow single precision math even if we're generally
   being traditional. */
extern int flag_allow_single_precision;

/* Nonzero means warn about suggesting putting in ()'s.  */
extern int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */
extern int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */
extern int warn_sign_compare;

/* Nonzero means message about use of implicit function declarations;
 1 means warning; 2 means error. */
extern int mesg_implicit_function_declaration;

extern bool pedantic; /* Report pedantic warnings if true */

/* Nonzero means warn about use of implicit int. */
extern int warn_implicit_int;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */
extern int warn_missing_prototypes;

/* Nonzero means warn for any global function def
   without separate previous decl.  */
extern int warn_missing_declarations;

/* Nonzero means `char' should be signed.  */
extern int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */
extern int flag_short_enums;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */
extern int flag_signed_bitfields;

/* Nonzero means don't run real cc1 afterwards */
extern int flag_parse_only;

/* Nonzero means suppress the dbg and dbg_clear functions (replace them
   by macros in output). This is necessary because gcc won't inline
   varargs functions */
extern int flag_no_debug;

/* Nonzero to suppress automatic addition of inline keywords 
   (but the "wiring" functions are still marked inline) */
extern int flag_no_inline;

/* Nonzero means to output macro defs in the generated C file (a la -dD) */
extern int flag_save_macros;

/* Nonzero means modify identifier and declaration output during code
   generation to accomodate nido */
extern bool use_nido;

/* specifies the maximum number of nodes that can be simulated at one time */
extern char *nido_num_nodes;

/* the expression that gives the current mote number */
extern char *nido_mote_number;

/* Nonzero for -v */
extern int flag_verbose;

/* Warn if there are unexpected documentation strings in the code */
extern int warn_unexpected_docstring;

/* Warn when function pointers are used */
extern int warn_fnptr;

/* Warn when data races are detected */
extern int warn_data_race;

/* Warn when async keyword is violated */
extern int warn_async;

/* Warn when no combiner function and multiple fns called */
extern int warn_no_combiner;

/* If true, warn_fnptr, warn_data_race, warn_async and warn_no_combiner
   are treated as errors */
extern int nesc_error;

/* diff processing enabled if diff_output is not NULL 
   (diff_input is NULL for orignal program, non-NULL to reduce diff size) */\
extern char *diff_input, *diff_output;

/* If true, rewrite post/task to use interfaces+wiring rather than calls to
   a TOS_post function. */
extern int flag_use_scheduler;

/* If true, we're using a mingw based gcc from a cygwin environment. We
   should use windows filenames when invoking gcc. */
extern int flag_mingw_gcc;

/* If true, check for atomic statements whose body is guaranteed to be
   atomic (e.g., one single-byte read) */
extern int nesc_optimise_atomic;

/* Warn about possibly nested block comments, and C++ comments
   spanning more than one physical line */
extern int warn_comments;

/* Warn if trigraphs are encountered that might affect the meaning of
   the program */
extern int warn_trigraphs;

/* Warn about macros defined in the main file that are not used */
extern int warn_unused_macros;

/* Warn about stray tokens after #elif and #endif */
extern int warn_endif_labels;

/* Do not suppress warnings from system headers */
extern int warn_system_headers;

/* Warn if an undefined macro is used in an #if directive */
extern int warn_undef;

/* Warn about user-specified include directories that do not exist */
extern int warn_missing_include_dirs;

/* Warn about use of multi-character character constants */
extern int warn_multichar;

/* Support ISO C trigraphs */
extern int flag_trigraphs;

/* Do not search standard system include directories */
extern int flag_nostdinc;

/* Do not predefine system-specific and GCC-specific macros */
extern int flag_undef;

/* True if compiling for deputy */
extern int flag_deputy;

/* True if a module lacking a @safe() or @unsafe() attribute defaults
   to safe; has no effect if flag_deputy is not true; this default can
   be overridden by -fnesc-default-safe or -fnesc-default-unsafe */
extern int flag_default_safe;

/* True if transforming plain C code */
extern int flag_c;

/* True if target-specific extensions should be output as gcc
   attributes rather than in their original syntax. */
extern int flag_gccize;

/* Print included file names (-H) */
extern int print_include_names;

#endif
