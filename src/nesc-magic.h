/* Magic function support. Magic functions are constant folded at compile-time,
   all their arguments must be constants (or string constants).

   They are used to pick unique numbers for generic interfaces, lookup
   active message types, etc
*/

#ifndef NESC_MAGIC_H
#define NESC_MAGIC_H

extern data_declaration magic_unique, magic_uniqueCount;

void init_magic_functions(void);

data_declaration get_magic(function_call fcall);
/* Returns: magic function called by fcall if it's a magic function call,
     NULL otherwise
*/

known_cst fold_magic(function_call fcall, int pass);

#endif
