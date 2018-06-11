void f(int *restrict x, int *restrict y);

module test {
}
implementation {
  void entry() __attribute((spontaneous)) {
    int a, b;
    f(&a, &b);
  }
}
