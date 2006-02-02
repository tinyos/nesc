module s2 {
  typedef int x;
  enum {
    aa = 3
  };
}
implementation {
  x a;

  void f() __attribute__((spontaneous)) { a = aa; }
}
