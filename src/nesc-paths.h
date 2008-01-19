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

#ifndef NESC_PATHS_H
#define NESC_PATHS_H
/* Locate components/interfaces from their name */

enum { CHAIN_QUOTE, CHAIN_BRACKET, CHAIN_SYSTEM, CHAIN_AFTER };

void init_nesc_paths_start(region r);
void add_nesc_path(const char *path, int chain);
void add_nesc_dir(const char *path, int chain);
void init_nesc_paths_end(void);

void set_cpp_include_path(void);
const char *find_nesc_file(region r, source_language l, const char *name);

#endif
