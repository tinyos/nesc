generic module g2(typedef t) {
}
implementation {
  t x;

  void f() __attribute__((spontaneous)) {
    x = x;
  }
}
