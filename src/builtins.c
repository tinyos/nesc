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

#include "parser.h"
#include "builtins.h"
#include "semantics.h"

data_declaration gcc_builtin_classify_type, builtin_memcpy, builtin_memcmp,
  builtin_strcpy, builtin_strcmp, builtin_strlen;

void builtins_init(void)
{
  gcc_builtin_classify_type = lookup_global_id("__builtin_classify_type");
  builtin_memcpy = lookup_global_id("memcpy");
  builtin_memcmp = lookup_global_id("memcmp");
  builtin_strcpy = lookup_global_id("strcpy");
  builtin_strcmp = lookup_global_id("strcmp");
  builtin_strlen = lookup_global_id("strlen");
}

bool builtin_declaration(data_declaration dd)
/* Returns: TRUE if dd is a declaration for something builtin (i.e.,
     starts with __builtin_
*/
{
  return strncmp(dd->name, "__builtin_", 10) == 0;
}

