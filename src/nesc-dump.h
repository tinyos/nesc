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

#ifndef NESC_DUMP_H
#define NESC_DUMP_H

/* The internal nesC dump information system. Dumps information in XML
   according to the DSD schema which can be found in doc/dump (see the
   README there for more details).

   Java code to parse this schema into a programmer-friendly form is
   in the net.tinyos.nesc.dump package (found under tools/java). */

#include "nesc-cg.h"

extern region dump_region;

void select_dump(char *what);
/* Effects: Register a new -fnesc-dump request 'what'
     Errors are signaled through the usual 'error' call.
 */

void select_dumpfile(char *name);
/* Effects: Select target file for dump
 */

bool dump_selected(void);
/* Effects: Return true if any calls to select_dump where made.
 */

void dump_info(nesc_declaration program, cgraph cg, cgraph userg,
	       dd_list modules, dd_list components);
/* Effects: Dump selected information.
 */

#endif
