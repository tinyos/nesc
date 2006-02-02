module e8 { uses interface i; }
implementation {
  int a = sizeof i.f;
  void f() __attribute__((spontaneous)) { a = 2; }
}
