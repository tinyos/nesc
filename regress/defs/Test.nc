module Test {
}
implementation {
  char *x;

  void f() __attribute__((spontaneous)) {
    x = DEF;
    x = #DEF2;
  }
}
