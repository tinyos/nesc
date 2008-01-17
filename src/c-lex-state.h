#ifndef C_LEX_STATE_H
#define C_LEX_STATE_H

#include "c-parse.h"

struct file_stack
{
  struct file_stack *next;
  struct location l;
};

struct lex_state
{
  struct cpp_reader *finput;
  struct line_maps *line_map;
  struct file_stack *input;
  int token_s1, token_s2;
  struct yystype token_l1, token_l2;
};


#endif
