typedef int foo_t;

generic module typedef2(typedef foo_t @integer()) { }
implementation {
  foo_t x;

  void f() {
    x = 2;
  }
}
