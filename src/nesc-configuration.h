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

#ifndef NESC_CONFIGURATION_H
#define NESC_CONFIGURATION_H

#include "nesc-cg.h"

void process_configuration(configuration c);

component_ref require_component(component_ref comp, word as);
expression make_component_deref(location loc, expression object, cstring field);;
void check_generic_arguments(expression args, typelist gparms);
struct endp;
typelist endpoint_args(struct endp *p);
void component_scan(data_declaration cref, env_scanner *scan);

void connect_interface(location l, cgraph cg, cgraph userg,
                       struct endp from, struct endp to,
                       bool reverse);

#endif
