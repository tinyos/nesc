/* This file is part of the nesC compiler.
   Copyright (C) 2008 Intel Corporation

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
#include "nesc-deputy.h"
#include "nesc-attributes.h"

static void attr_dscope_tdecl(nesc_attribute attr, tag_declaration tdecl)
{
  if (tdecl->kind != kind_attribute_ref)
    {
      error_with_location(attr->location, "@deputy_scope() can only be applied to attribute declarations");
      return;
    }
  tdecl->deputy_scope = TRUE;
}

void init_deputy(void)
{
  define_internal_attribute("deputy_scope", NULL, NULL, attr_dscope_tdecl, NULL,
			    NULL, NULL);
}
