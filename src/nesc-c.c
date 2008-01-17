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
#include "nesc-c.h"
#include "nesc-paths.h"
#include "nesc-semantics.h"
#include "c-parse.h"
#include "nesc-cpp.h"

declaration all_cdecls;

void add_cdecls(declaration cdecls)
{
  all_cdecls = declaration_chain(all_cdecls, CAST(declaration, cdecls));
}

void load_c(location l, const char *name, bool name_is_path)
{
  node cdecls = compile(l, NULL, name, name_is_path);
  add_cdecls(CAST(declaration, cdecls));
}
