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

enum { nesc_interface, nesc_component };

typedef struct {
  int kind; /* nesc_interface or component */
  const char *name;
  nesc_decl ast;
  struct environment *env;
} *nesc_declaration;

/* A particular interface declaration (.ti file) */
typedef struct interface_declaration {
  int kind; /* nesc_interface */
  const char *name;
  nesc_decl ast;
  struct environment *env;
} *interface_declaration;

#define NDCAST(to, x) ((to)x)

/* A unified representation of components (modules and configurations) */
typedef struct component_declaration {
  int kind; /* nesc_component */
  const char *name;
  nesc_decl ast;
  struct environment *env; /* The external interfaces of this component */
  implementation impl;
  struct cgraph *connections;
} *component_declaration;

#endif
