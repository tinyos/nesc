module m1 {
  provides interface i[int id];
  uses interface i as j;
}
implementation {
  void f() __attribute__((spontaneous)) {
    call j.f();
  }

  command void i.f[int id]() {
  }
}
