module e7 { uses interface i; }
implementation {
  int a = sizeof i;
  void f() __attribute__((spontaneous)) { a = 2; }
}
