#ifndef C_LEX_STATE_H
#define C_LEX_STATE_H

#include "c-parse.h"

struct lex_state
{
  FILE *finput;
  bool end_of_file;
  bool token_buffer_valid;	/* TRUE when token_buffer is from this file. */
  int indent_level;		/* Number of { minus number of }. */
  int nextchar;  /* Buffered-back input character; faster than using ungetc.  */
  int token_s1, token_s2;
  struct yystype token_l1, token_l2;
};


#endif
