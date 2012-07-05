#include <stdio.h>

module test {}
implementation {
#define CHECK(cond) if (!(cond)) fprintf(stderr, "Failed check at %s:%d\n", __FILE__, __LINE__)

  nx_struct s1 {
    nx_int64_t s64;
    nx_int64_t u64;
  } v1;

  nx_struct bs1 {
    nx_int64_t s64:52;
    nx_int64_t u64:52;
  } v2;

  void check1(uint64_t uval) {
    int64_t sval = uval;
    v1.u64 = uval;
    CHECK(v1.u64 == uval);
    v1.s64 = sval;
    CHECK(v1.s64 == sval);
  }

  void check2(uint64_t uval) {
    int64_t sval = uval;
    v2.u64 = uval;
    CHECK(v2.u64 == uval);
    v2.s64 = sval;
    CHECK(v2.s64 == sval);
  }

  int main(int argc, char **argv) @C() @spontaneous() {
    int i;

    for (i = 0; i < 64; i += 2) {
      uint64_t v = 1ULL << i;

      check1(v);
      check1(v + 1);
      check1(v + 2);
      check1(v + 3);
    }
    
    for (i = 0; i < 52; i += 2) {
      uint64_t v = 1ULL << i;

      check2(v);
      check2(v + 1);
      check2(v + 2);
      check2(v + 3);
    }
    
    return 0;
  }
}
