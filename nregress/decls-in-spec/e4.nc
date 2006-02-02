module e4 {
  provides typedef int x;
}
implementation {
  x a;

  void f() __attribute__((spontaneous)) { a = 2; }
}
