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

#ifndef NESC_DFILTER_H
#define NESC_DFILTER_H

nd_filter make_ndf_op(region r, const char *name, nd_arg args);
/* Returns: a new filter op for filter 'name' with arguments 'args' 
 */

void dump_set_filter(nd_option opt);
/* Effects: Sets current filter to the and of all filters found in option
     list opt. */

/* Returns: TRUE if argument passes current filter. */
bool dump_filter_ddecl(data_declaration ddecl);
bool dump_filter_ndecl(nesc_declaration ndecl);
bool dump_filter_tdecl(tag_declaration tdecl);

#endif

