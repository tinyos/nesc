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

/* Lexer for -fnesc-dump arguments (tokens are numbers, names (can be
   arbitrary strings in "") and boolean operators */

%option nounput

%{
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "parser.h"
#include "nesc-dump.h"
#include "nesc-dspec.h"
#include "nesc-dspec-int.h"

/* We never wrap into another file */
#define YY_SKIP_YYWRAP
#define ndwrap() (1)

#define YY_USE_PROTOS

#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) \
  { (result) = string_read((buf), (max_size)); }
static int string_read(char *buf, int max_size);
void nderror(char *err);

%}

DIGIT           [0-9]
HEXDIGIT	[0-9a-fA-F]

%%

[ \t] 		{ }
[,()|&!]	{ return ndtext[0]; }
-?({DIGIT}+|0[xX]{HEXDIGIT}+) {
  		  errno = 0;
		  ndlval.integer = strtoll(ndtext, NULL, 0);
		  if (errno)
	    	    nderror("integer constant out of bounds.");
            	  return ND_INTEGER; 
		}
\"([^\\"]*(\\.)?)+\" {
		  char *str = rstralloc(dump_region, strlen(ndtext));
		  const char *text = ndtext + 1;

		  ndlval.token = str;
		  while (*text)
		    {
		      /* Shell-like \-processing: \ is preserved except
		         in front of " and \ */
		      if (*text == '\\' && (text[1] == '"' || text[1] == '\\'))
		        text++;
		      *str++ = *text++;
		    }
		  str[-1] = '\0';
		  return ND_TOKEN;
		}
[^, \t()|&!]+	{
  		  ndlval.token = rstrdup(dump_region, ndtext);
		  return ND_TOKEN;
		}

%%

static YY_BUFFER_STATE mbuf;
static const char *line;
static int length, offset;

void nd_read(const char *str)
{
  if (mbuf) 
    nd_delete_buffer(mbuf);
  mbuf = nd_create_buffer(stdin, YY_BUF_SIZE);
  
  nd_switch_to_buffer(mbuf);
  line = str;
  offset = 0;
  length = strlen(line);
}

static int string_read(char *abuf, int max_size)
{
  int cnt;

  cnt = max_size;
  if (cnt > length - offset) cnt = length - offset;

  memcpy(abuf, line + offset, cnt);
  offset += cnt;

  return cnt;
}

void nderror(char *err)
{
  error("dump option `%s': %s", line, err);
}
