/* Magic function support. Magic functions are constant folded at compile-time,
   all their arguments must be constants (or string constants).

   They are used to pick unique numbers for generic interfaces, lookup
   active message types, etc
*/

#ifndef NESC_MAGIC_H
#define NESC_MAGIC_H

void init_magic_functions(void);

expression magic_reduce(function_call fcall);
bool magic_print(function_call fcall);

#endif
