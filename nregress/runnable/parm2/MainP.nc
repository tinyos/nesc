#include <assert.h>

module MainP {
  uses interface Myif;
}

implementation {

  int @C() @spontaneous() main(int argc, char **argv) {
    assert(call Myif.foo() == 1);
  }
}