/* This file is part of the nesC compiler.

This file is derived from RC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
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
#include "input.h"

/* Stack of currently pending input files. */
struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
int input_file_stack_tick;

void set_input(FILE *f, const char *filename)
{
  struct file_stack *p = (struct file_stack *)xmalloc(sizeof(struct file_stack));
  p->next = input_file_stack;
  input_file_stack = p;

  p->l.filename = filename;
  p->l.container = NULL;
  p->l.lineno = 0;
  p->l.in_system_header = FALSE;
  p->lex.finput = f;
  input_file_stack_tick++;
}

void end_input(void)
{
  FILE *f = input_file_stack->lex.finput;

  while (input_file_stack && input_file_stack->lex.finput == f)
    {
      struct file_stack *p = input_file_stack;
      input_file_stack = p->next;

      free(p);
    }

  if (f)
    fclose(f);
  input_file_stack_tick++;
}

void push_input(void)
{
  /* Pushing to a new file.  */
  struct file_stack *p = (struct file_stack *)xmalloc(sizeof(struct file_stack));

  *p = *input_file_stack;
  p->next = input_file_stack;
  input_file_stack = p;
  input_file_stack_tick++;
}

void pop_input(void)
{
  struct file_stack *p = input_file_stack;

  input_file_stack = p->next;
  input_file_stack->lex = p->lex; /* This was the same file, don't lose file-related state */
  free (p);
  input_file_stack_tick++;
}
