module e1 {
  int a;
}
implementation {
  void f() __attribute__((spontaneous)) { a = 2; }
}
