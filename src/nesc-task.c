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

#include "parser.h"
#include "constants.h"
#include "nesc-task.h"
#include "nesc-semantics.h"
#include "nesc-component.h"
#include "semantics.h"
#include "AST_utils.h"
#include "c-parse.h"

word scheduler_interface_name;

static void declare_scheduler_interface(data_declaration task_decl)
{
  region r = parse_region;
  location loc = task_decl->ast->location;
  word task_name;
  interface_ref task_interface;
  rp_interface task_uses;
  component current_component = CAST(component, current.container->ast);

  /* We save the task's replacement interface in its interface field... */
  if (task_decl->interface)
    return;

  /* Build the (uses) 'interface Scheduler as X' ast, declare the interface. */
  task_name = new_word(r, loc, str2cstring(r, task_decl->name));
  task_interface = new_interface_ref(r, loc, scheduler_interface_name, NULL, task_name, NULL, NULL, NULL);
  current.spec_section = spec_uses;
  declare_interface_ref(task_interface, NULL, current.container->env, NULL);
  task_decl->interface = task_interface->ddecl;

  /* Build the 'uses <interface>' AST, add it to the component */
  task_uses = new_rp_interface(r, loc, TRUE, CAST(declaration, task_interface));
  current_component->decls =
    declaration_chain(CAST(declaration, task_uses), current_component->decls);
}

void handle_post(function_call fcall)
{
}

void handle_task_definition(function_decl fdecl)
{
  declare_scheduler_interface(fdecl->ddecl);
}

void handle_task_declaration(variable_decl vdecl)
{
  declare_scheduler_interface(vdecl->ddecl);
}


void init_task(void)
{
  scheduler_interface_name =
    new_word(permanent, dummy_location, str2cstring(permanent, "TaskBasic"));
}

#if 0
#include "nesc-semantics.h"
#include "AST_utils.h"
#include "AST_walk.h"
#include "c-parse.h"
#include "edit.h"
/* We hack the AST to replace:

    task void x() { ... }
  with
    uses intervace Task as x;
    event void x.run() { ... }

    post x()
  with
    call x.post_();
*/

static void replace_task_with_event(type_element modifiers)
{
  type_element modifier;

  scan_type_element (modifier, modifiers)
    if (is_rid(modifier))
      {
	rid keyword = CAST(rid, modifier);

	if (keyword->id == RID_TASK)
	  keyword->id = RID_EVENT;
      }
}

static AST_walker_result rewrite_function_decl(AST_walker spec, void *data,
					       function_decl *n)
{
  function_decl fdecl = *n;

  replace_task_with_event(fdecl->modifiers);

  return aw_walk;
}

static AST_walker_result rewrite_function_call(AST_walker spec, void *data,
					       function_call *n)
{
  function_call fcall = *n;

  return aw_walk;
}

/* AST walkers to rewrites post and tasks */
static AST_walker task_walker, post_walker;

void init_task(void)
{
  task_walker = new_AST_walker(parse_region);
  post_walker = new_AST_walker(parse_region);
  AST_walker_handle(task_walker, kind_function_decl, rewrite_function_decl);
  AST_walker_handle(post_walker, kind_function_call, rewrite_function_call);
}
#endif
