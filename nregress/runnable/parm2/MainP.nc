#include <assert.h>

module MainP {
  uses interface Myif;
  uses interface Myif as Pif[uint32_t x, uint16_t y];
}

implementation {

  int @C() @spontaneous() main(int argc, char **argv) {
    assert((call Pif.foo[11, 22]()) == 11);
    assert(call Myif.foo() == 1);
  }
}