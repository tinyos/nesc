module s1 {
  typedef int x;
}
implementation {
  x a;

  void f() __attribute__((spontaneous)) { a = 2; }
}
