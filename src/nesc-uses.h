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

#ifndef NESC_USES_H
#define NESC_USES_H

typedef enum {
  c_atomic = 1,
  c_executable = 2,
  c_read = 4,
  c_write = 8,
  c_fncall = 16,
  c_addressed = 32,
  c_deref = 64,
  c_constant = 128
} context;

typedef struct use
{
  location l;
  data_declaration fn;		/* function containing use */
  context c;
} *use;

typedef struct iduse
{
  data_declaration id;
  use u;
} *iduse;

extern dd_list nglobal_uses;

/* Declaration of __nesc_enable_interrupt function. Data-race detection
   and duplicate atomic suppression needs to be aware of these calls to
   function correctly.

   The current implementation is simplistic (conservative and unsafe):
   If function X calls __nesc_enable_interrupt, we assume that:
   - there are non-atomic calls to X
   - calls made from inside an atomic statement in X are still atomic
     (i.e., we assume there is no way to reach a statement inside 'atomic'
     from the __nesc_enable_interrupt() call. We do issue a warning when
     __nesc_enable_interrupt() is called from within an atomic statement).

   The proper behaviour would be to analyse X's control flow graph.
*/
extern data_declaration enable_interrupt;

void collect_uses(declaration decls);

context exe_context(context c);

void init_uses(void);

use new_use(location l, data_declaration fn, context c);
void ddecl_used(data_declaration id, use u);

#endif
