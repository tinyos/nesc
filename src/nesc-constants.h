#ifndef NESC_CONSTANTS_H
#define NESC_CONSTANTS_H

bool fold_constants_list(node n, int pass);
/* Effects: Folds constants and lays out types in AST list n
     pass is the current constant folding pass number (starts at 1, 0
     is reserved for parse-time constant folding)
 */

void init_nesc_constants(void);

#endif
