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

%{
#include "parser.h"
#include "nesc-dspec.h"
#include "nesc-dspec-int.h"
#include "nesc-dfilter.h"

static nd_option opt;
%}

%token <token> ND_TOKEN
%token <integer> ND_INTEGER
%type <token> name
%type <nd_arg> args arg
%type <nd_filter> filter

%left '|'
%left '&'
%nonassoc '!'

%%

option: 
    name { opt = new_nd_option(permanent, $1, NULL, 0); }
  | name '(' args ')' { opt = new_nd_option(permanent, $1, $3, nd_arg_length($3)); }
  ;

name: ND_TOKEN ;

args: 
    args ',' arg { $$ = nd_arg_chain($1, $3); }
  | arg
  ;

arg:
    ND_TOKEN  { $$ = CAST(nd_arg, new_nd_token(permanent, $1)); }
  | ND_INTEGER { $$ = CAST(nd_arg, new_nd_int(permanent, $1)); }
  | filter { $$ = CAST(nd_arg, $1); }
  ;

filter:
    filter '|' filter { $$ = CAST(nd_filter, new_ndf_or(permanent, $1, $3)); }
  | filter '&' filter { $$ = CAST(nd_filter, new_ndf_and(permanent, $1, $3)); }
  | '!' filter  { $$ = CAST(nd_filter, new_ndf_not(permanent, $2)); }
  | '(' filter ')' { $$ = $2; }
  | ND_TOKEN '(' args ')' {
      $$ = make_ndf_op(permanent, $1, $3);
    }
  ;

%%

nd_option nd_parse(const char *what)
{
  opt = NULL;
  nd_read(what);
  if (ndparse())
    return NULL;
  return opt;
}

#include "ND_types.c"
#include "ND_list_nd_arg.c"

