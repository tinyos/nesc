module s2a {
  enum {
    aa = 3
  };
}
implementation {
  int a;

  void f() __attribute__((spontaneous)) { a = aa; }
}
