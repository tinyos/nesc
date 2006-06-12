#include <stdlib.h>

generic configuration docheck(int test, size_t nesc_size, size_t nesc_align, typedef t) { }
implementation {
  components main, new check(test, nesc_size, nesc_align, t);

  main.run -> check;
}
