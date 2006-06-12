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
Boston, MA 02111-1307, USA. */

#ifndef NESC_NETWORK_H

/* Prefix for non-network version of network base types */
#define NXBASE_PREFIX "__nesc_nxbase_"

void handle_network_types(declaration decls);
void init_network(void);

void prt_network_fields(tag_ref tref);
void prt_network_routines(void);
bool prt_network_expression(expression e);
bool prt_network_typedef(data_decl d, variable_decl vd);
bool prt_network_parameter_copies(function_decl fn);

#endif
