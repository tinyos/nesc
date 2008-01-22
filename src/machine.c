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

#include "parser.h"
#include "machine.h"
#include "errors.h"
#include "c-parse.tab.h"
#undef yystype
#include "c-parse.h"
#include "semantics.h"
#include "nesc-gcc.h"

#include "machine/avr.c"
#include "machine/self.c"
#include "machine/keil.c"
#include "machine/sdcc.c"
#include "machine/msp430.c"
#include "machine/env_machine.c"

static machine_spec *machines[] = {
  &avr_machine,
  &self_machine,
  &keil_machine,
  &sdcc_machine,
  &msp430_machine,
  &env_machine,
  NULL
};

machine_spec *target = &self_machine;
const char *target_compiler = "gcc";

bool select_target(const char *targetname)
{
  machine_spec **scan;

  for (scan = machines; *scan; scan++)
    if (!strcmp(targetname, (*scan)->machine_name))
      {
	if (*scan == &env_machine &&
	    scan_env_machine(&env_machine, "NESC_MACHINE") == FALSE)
	  {
	    error("invalid target described in env NESC_MACHINE");
	    return FALSE;
	  }
	target = *scan;
	return TRUE;
      }

  error("unknown target %s", targetname);
  return FALSE;
}


