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

#ifndef NESC_TASK_H
#define NESC_TASK_H

extern nesc_declaration scheduler;
extern declaration all_tasks;

void set_scheduler(char *spec);
void load_scheduler(void);
void wire_scheduler(module m);
void handle_post(function_call fcall);
void handle_task_definition(function_decl fdecl);
void handle_task_declaration(variable_decl vdecl);

#endif
