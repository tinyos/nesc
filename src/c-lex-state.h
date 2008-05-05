#ifndef C_LEX_STATE_H
#define C_LEX_STATE_H

#include "c-parse.h"

extern int input_file_stack_tick;

struct file_stack
{
  struct file_stack *next;
  struct location l;
};

struct cpp_print
{
  FILE *outf;			/* Stream to write to.  */
  const struct cpp_token *prev;	/* Previous token.  */
  const struct cpp_token *source;	/* Source token for spacing.  */
  int src_line;			/* Line number currently being written.  */
  unsigned char printed;	/* Nonzero if something output at line.  */
  bool first_time;		/* pp_file_change hasn't been called yet.  */
  bool avoid_paste;
};

struct lex_state
{
  struct cpp_reader *finput;
  struct line_maps *line_map;
  struct file_stack *input;
  int token_s1, token_s2;
  struct yystype token_l1, token_l2;
  struct cpp_print pp;
};


#endif
