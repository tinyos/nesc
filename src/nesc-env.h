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

#ifndef NESC_ENV_H
#define NESC_ENV_H

/* Top-level nesc environment. Keeps track of loaded interfaces and
   components, loads them on demand */

void init_nesc_env(region r);
void interface_declare(interface_declaration d);
void component_declare(component_declaration d);
component_declaration require_component(location l, const char *name);
interface_declaration require_interface(location l, const char *name);
void require_c(location l, const char *name);


#endif
