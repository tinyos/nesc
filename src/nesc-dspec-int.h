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
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA 02110-1301 USA.  */

#ifndef NESC_DSPEC_INT_H
#define NESC_DSPEC_INT_H

/* Internal definitions for dump specification parsing */

struct ndstype {
  largest_int integer;
  union {
    const char *token;
    nd_arg nd_arg;
    nd_filter nd_filter;
  };
};

#define YYSTYPE struct ndstype

void nd_read(const char *str);
void nderror(char *err);

#include "nesc-dspec.tab.h"

int ndlex(void);

#endif
