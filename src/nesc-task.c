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
#include "nesc-configuration.h"
#include "semantics.h"
#include "AST_utils.h"
#include "c-parse.h"
#include "nesc-cg.h"
#include "nesc-c.h"
#include "nesc-magic.h"
#include "edit.h"
#include "expr.h"

static char *scheduler_name;
static char *scheduler_unique_name;
static char *scheduler_interface_name;
static char *scheduler_interfacedef_name;
static char *scheduler_run_name;
static char *scheduler_post_name;

nesc_declaration scheduler;
static data_declaration scheduler_interface;
declaration all_tasks;

static word make_scheduler_interfacedef_name(location l)
{
  static bool alloc;
  static cstring ascstring;

  if (!alloc)
    {
      alloc = TRUE;
      ascstring = str2cstring(permanent, scheduler_interfacedef_name);
    }

  return new_word(parse_region, l, ascstring);
}

static cstring make_scheduler_run_name(void)
{
  static bool alloc;
  static cstring ascstring;

  if (!alloc)
    {
      alloc = TRUE;
      ascstring = str2cstring(permanent, scheduler_run_name);
    }

  return ascstring;
}

static cstring make_scheduler_post_name(void)
{
  static bool alloc;
  static cstring ascstring;

  if (!alloc)
    {
      alloc = TRUE;
      ascstring = str2cstring(permanent, scheduler_post_name);
    }

  return ascstring;
}

static void declare_scheduler_interface(data_declaration task_decl)
{
  region r = parse_region;
  location loc = task_decl->ast->location;
  word task_name;
  interface_ref task_interface;
  rp_interface task_uses;
  int osection;

  /* We save the task's replacement interface in its interface field... */
  if (task_decl->interface)
    return;

  /* Build the (uses) 'interface Scheduler as X' ast, declare the interface. */
  /* This specific AST structure is assumed in wire_scheduler below */
  task_name = new_word(r, loc, str2cstring(r, task_decl->name));
  task_interface = new_interface_ref(r, loc, make_scheduler_interfacedef_name(loc),
				     NULL, task_name, NULL, NULL, NULL);
  osection = current.spec_section;
  current.spec_section = spec_uses;
  declare_interface_ref(task_interface, NULL, current.container->env, NULL);
  current.spec_section = osection;
  task_decl->interface = task_interface->ddecl;

  /* Build the 'uses <interface>' AST, add it to the component */
  task_uses = new_rp_interface(r, loc, TRUE, CAST(declaration, task_interface));
  all_tasks = declaration_chain(CAST(declaration, task_uses), all_tasks);
}

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

void handle_post(function_call fcall)
{
  identifier task = CAST(identifier, fcall->arg1);
  
  interface_deref scheduler_post;
  data_declaration postdecl;

  postdecl = interface_lookup(task->ddecl->interface, scheduler_post_name);
  if (!postdecl || postdecl->ftype != function_command)
    {
      static int oneerror;

      if (!oneerror)
	{
	  oneerror = TRUE;
	  error("task interface `%s' has no command named `%s'",
		scheduler_interface_name, scheduler_post_name);
	}
      return;
    }

  /* Rewrite post foo() to call foo.post() */
  fcall->call_kind = command_call;
  task->ddecl = task->ddecl->interface;
  task->type = task->ddecl->type;
  scheduler_post = new_interface_deref(parse_region, task->location,
				       CAST(expression, task),
				       make_scheduler_post_name(), postdecl);
  scheduler_post->type = postdecl->type;
  fcall->arg1 = CAST(expression, scheduler_post);
}

void handle_task_definition(function_decl fdecl)
{
  function_declarator fd;

  declare_scheduler_interface(fdecl->ddecl);
  replace_task_with_event(fdecl->modifiers);

  /* Replace foo with foo.run. We only bother doing this if the declarator
     is a valid task declarator (function_declarator with nested
     identifier_declarator) - if it isn't, we have an error in the form of
     a misdeclared task. */
  /* (we only get here for something which really is a function declaration) */
  fd = CAST(function_declarator, fdecl->declarator);
  if (is_identifier_declarator(fd->declarator))
    {
      data_declaration rundecl;
      identifier_declarator oldd = CAST(identifier_declarator, fd->declarator);
      identifier_declarator rund =
	new_identifier_declarator(parse_region, fd->location,
				  make_scheduler_run_name());
      interface_ref_declarator ird =
	new_interface_ref_declarator(parse_region, fd->location,
				     CAST(declarator, rund),
				     new_word(parse_region, fd->location, oldd->cstring));
      fd->declarator = CAST(declarator, ird);

      /* Update task's declaration object */
      rundecl = interface_lookup(fdecl->ddecl->interface, scheduler_run_name);
      if (!rundecl || rundecl->ftype != function_event)
	{
	  static int oneerror;

	  if (!oneerror)
	    {
	      oneerror = TRUE;
	      error("task interface `%s' has no event named `%s'",
		    scheduler_interface_name, scheduler_run_name);
	    }
	}
      else
	{
	  /* Don't lose safe flag */
	  rundecl->safe = fdecl->ddecl->safe;
	  fdecl->ddecl = rundecl;
	  rundecl->definition = CAST(declaration, fdecl);
	}
    }
}

void handle_task_declaration(variable_decl vdecl)
{
  declare_scheduler_interface(vdecl->ddecl);
  /* This declaration will be suppressed in unparsing, as you can't
     declare events anyway. So no further work needed. */
}

void load_scheduler(void)
{
  scheduler = load(l_component, toplevel_location, scheduler_name, FALSE);
  if (scheduler_name)
    {
      data_declaration intf = env_lookup(scheduler->env->id_env,
					 scheduler_interface_name, TRUE);

      /* Check interface for validity. It must be the provided, have a
	 single parameter and be the right interface type.
	 Also, no generic interfaces please. */
      if (intf && intf->kind == decl_interface_ref && !intf->required &&
	  intf->gparms && !intf->itype->abstract &&
	  !strcmp(intf->itype->name, scheduler_interfacedef_name))
	{
	  typelist_scanner dummy;

	  typelist_scan(intf->gparms, &dummy);
	  if (typelist_next(&dummy) && !typelist_next(&dummy))
	    scheduler_interface = intf;
	}
      if (!scheduler_interface)
	error_with_location(toplevel_location,
			    "Scheduler `%s' has no scheduling interface named `%s'",
			    scheduler_name, scheduler_interface_name);
    }
}

static expression build_taskid(module m, data_declaration taskdecl)
{
  /* Build a unique identifier for a task whose declaration is taskdecl,
     in module m.
     Method: we add enum { m$taskdecl = unique("task-unique-string") };
             to all_cdecls
	     and return an identifier-expression referring to m$taskdecl
  */

  location loc = taskdecl->ast->location;
  region r = parse_region;
  cstring idname;
  enumerator idast;
  struct data_declaration tempdecl;
  enum_ref idenum;
  tag_declaration enumdecl;
  data_decl iddecl;
  expression unique_id, unique_fn, unique_args, use_id;
  cstring silly_name;
  identifier_declarator silly_id;
  declarator silly_d;
  type_element silly_modifiers;
  rid silly_typedef;
  type silly_type;
  variable_decl silly_vd;
  data_decl silly_decl;

  /* Build unique("task-unique-string") */
  unique_fn = build_identifier(r, loc, magic_unique);
  unique_args = build_string(r, loc, scheduler_unique_name);
  default_conversion(unique_args);
  unique_id = build_function_call(r, loc, unique_fn, unique_args);

  /* Build, declare enumerator taskdecl */
  idname = str2cstring(r, taskdecl->name);
  idast = new_enumerator(r, loc, idname, unique_id, NULL);
  init_data_declaration(&tempdecl, CAST(declaration, idast), idname.data,
			int_type);
  tempdecl.kind = decl_constant;
  tempdecl.definition = tempdecl.ast;
  tempdecl.value = unique_id->cst;
  idast->ddecl = declare(m->ienv, &tempdecl, FALSE);

  /* Build the enum declaration */
  idenum = new_enum_ref(r, loc, NULL, NULL, NULL, TRUE);
  idenum->fields = CAST(declaration, idast);
  idenum->tdecl = enumdecl = declare_tag(idenum);
  layout_enum_start(enumdecl);
  enumdecl->definition = idenum;
  enumdecl->defined = TRUE;
  layout_enum_end(enumdecl);

  /* Build the expression we will use in the wiring. */
  use_id = build_identifier(r, loc, idast->ddecl);

  /* Hack: the use_id expression needs to be in the module's AST so
     that we do instantiation and constant folding on it. We build
     a silly typedef for that purpose:
       typedef int __nesc_sillytask_taskdecl[use_id]
  */
  silly_name = alloc_cstring(r, strlen(taskdecl->name) + 17);
  sprintf(silly_name.data, "__nesc_sillytask_%s", taskdecl->name);
  silly_id = new_identifier_declarator(r, loc, silly_name);
  silly_type = make_array_type(int_type, use_id);
  type2ast(r, loc, silly_type, CAST(declarator, silly_id), 
	   &silly_d, &silly_modifiers);

  silly_typedef = new_rid(r, loc, RID_TYPEDEF);

  silly_vd = new_variable_decl(r, loc, silly_d, NULL, NULL, NULL,
			       NULL/*ddecl*/);
  init_data_declaration(&tempdecl, CAST(declaration, silly_vd),
			silly_name.data, silly_type);
  tempdecl.kind = decl_typedef;
  tempdecl.definition = tempdecl.ast;
  silly_vd->ddecl = declare(m->ienv, &tempdecl, FALSE);
  silly_vd->declared_type = silly_type;
  silly_decl =
    new_data_decl(r, loc, type_element_chain(CAST(type_element, silly_typedef),
					     silly_modifiers),
		  CAST(declaration, silly_vd));
  m->decls = declaration_chain(CAST(declaration, silly_decl), m->decls);

  /* Build the declaration and add it to the module's decls */
  iddecl = new_data_decl(r, loc, CAST(type_element, idenum), NULL);
  m->decls = declaration_chain(CAST(declaration, iddecl), m->decls);

  return use_id;
}


void wire_scheduler(module m)
{
  rp_interface taskuse;
  cgraph cg = m->cdecl->connections, userg = m->cdecl->user_connections;
  struct endp m_end, scheduler_end;

  if (!scheduler_interface)
    {
      declaration task;
      static int use_module = 0;

      /* If all_tasks is non-null, we have a problem: a task that needs to
	 be wired, but the scheduler is not yet available. Report as an error.
      */
      scan_declaration (task, all_tasks)
	{
	  error_with_location(task->location, "scheduler depends on a task");
	  if (!use_module)
	    {
	      use_module = 1;
	      error_with_location(task->location,
				  "The -fnesc_scheduler flag should specify a module");
	      error_with_location(task->location,
				  "(the module with the scheduling code, even if the scheduler is a configuration)");
	    }
	}
      return;
    }

  m_end.component = NULL;
  m_end.function = NULL;
  m_end.args_node = NULL;

  scheduler_end.component = NULL;
  scheduler_end.interface = scheduler_interface;
  scheduler_end.function = NULL;

  /* This matches the structure created in declare_scheduler_interface */
  scan_rp_interface (taskuse, CAST(rp_interface, all_tasks))
    {
      interface_ref task = CAST(interface_ref, taskuse->decls);
      location loc = task->location;
      expression args = build_taskid(m, task->ddecl);

      /* Create m.task -> scheduler.tasks[unique("task-unique-string")] */
      m_end.interface = task->ddecl;
      scheduler_end.args_node = args;
      connect_interface(loc, cg, userg, m_end, scheduler_end, FALSE);
    }
}

void set_scheduler(char *spec)
{
  scheduler_name = strtok(spec, ",");
  scheduler_unique_name = strtok(NULL, ",");
  scheduler_interface_name = strtok(NULL, ",");
  scheduler_interfacedef_name = strtok(NULL, ",");
  scheduler_run_name = strtok(NULL, ",");
  scheduler_post_name = strtok(NULL, ",");
  
  if (scheduler_post_name && !strtok(NULL, ","))
    flag_use_scheduler = TRUE;
  else
    {
      flag_use_scheduler = FALSE;
      error("invalid arguments to -fnesc-scheduler");
    }
}
