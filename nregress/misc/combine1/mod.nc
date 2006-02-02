includes fun;
module mod {
  provides interface i;
  uses interface i as j;
}
implementation {
  command i.f() { return call j.f(); }

  void g() __attribute__((spontaneous)) {
    call i.f();
  }
}
