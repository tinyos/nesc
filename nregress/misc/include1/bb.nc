#include "x.h"

module bb { }
implementation {
  x a;
  void f() __attribute__((spontaneous)) { a = 1; }
}
