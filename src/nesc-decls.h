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

#ifndef NESC_DECLS_H
#define NESC_DECLS_H

typedef struct nesc_declaration {
  source_language kind; /* l_interface or l_component */
  /* For source components, name and instance_name are the actual component 
     name.
     For concrete instances of abstract components, name is the
     name to used in generated code, and instance_name is a 
     user-friendly name (indicating the path through configurations that
     led to this instance).

     instance_name is used for error messages */
  const char *name;
  const char *instance_name;
  nesc_decl ast;
  struct environment *env;
  char *short_docstring;	/* For documentation comments */
  char *long_docstring;
  bool abstract;		/* true for abstract components and
				   generic interfaces */
  declaration parameters;	/* Parameters for abstract components and
				   generic interfaces */
  int folded;			/* number of last constant folding pass */

  /* for components */
  implementation impl;
  struct cgraph *connections;
  dd_list local_statics;	/* Local static variables (for nido) */
  size_t instance_count;	/* For abstract components, the
				   instance count (used to give each
				   instance a unique name) */
  struct nesc_declaration *original; /* For instances of abstract components,
					the "original" component */
} *nesc_declaration;

#endif
