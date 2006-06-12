#include <stdio.h>

generic module check(int test, size_t nesc_size, size_t nesc_align, typedef t)
{
  provides interface i;
}
implementation {
  command void i.check() {
    if (nesc_size != sizeof(t))
      fprintf(stderr, "test %d sizeof fails. nesc: %d, gcc: %d\n",
	      test, nesc_size, sizeof(t));
    if (nesc_align != __alignof__(t))
      fprintf(stderr, "test %d alignof fails. nesc: %d, gcc: %d\n",
	      test, nesc_align, __alignof__(t));
  }
}
