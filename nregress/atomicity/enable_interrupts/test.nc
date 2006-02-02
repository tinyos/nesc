includes ei;
module test { }
implementation {
  int x, y;

  void f() __attribute__((atomic_hwevent)) {
    x = 2;
    y = 3;
  }

  void g() __attribute__((atomic_hwevent)) {
    y = 1;
    __nesc_enable_interrupt();
  }

  void h() __attribute__((atomic_hwevent)) {
    atomic __nesc_enable_interrupt();
  }
}
