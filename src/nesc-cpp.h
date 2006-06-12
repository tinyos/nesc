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

#ifndef NESC_CPP_H
#define NESC_CPP_H

#include "parser.h"

void preprocess_init(void);
FILE *preprocess(const char *filename, source_language l);
void preprocess_file_end(void);

void handle_directive(const char *directive, const char *args);
void save_option(const char *option);
void end_macro_saving(void);

struct pragma {
  location l;
  const char *args; /* The uninterpreted pragma argument line */
};
extern dd_list/* struct pragma * */ pragmas;

#endif
