module e2 {
  int a();
}
implementation {
  void f() __attribute__((spontaneous)) { a(); }
}
