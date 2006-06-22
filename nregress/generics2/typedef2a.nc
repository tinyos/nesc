typedef int foo_t;

generic module typedef2a(typedef foo_t) { }
implementation {
  foo_t x;

  void f() {
    x = 2;
  }
}
