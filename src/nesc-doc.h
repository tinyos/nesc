/* This file is part of the nesC compiler.
   Copyright (C) 2002 UC Berkeley

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

#ifndef NESC_DOC_H
#define NESC_DOC_H

#include "nesc-cg.h"


/* set the output dir */
void doc_set_outdir(const char *dir);

/* Add a top level source directory */
void doc_add_topdir(const char *dir);
     
/* Set the directory separator - used for cygwin? */
void doc_set_dirsep(const char c);

/* set the use_graphviz flag */
void doc_use_graphviz(const bool use);

bool docs_requested(void);

/* actually generate docs */
void generate_docs(const char *filename, cgraph cg);

/* tell the doc generation to create the whole-app page */
void doc_is_app(const bool val);

void set_doc_string(data_declaration ddecl, char *shortd, char *longd,
		    location l);

#endif
