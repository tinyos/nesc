module e5 {
  uses enum {
    aa = 3
  };
}
implementation {
  int a;

  void f() __attribute__((spontaneous)) { a = aa; }
}
