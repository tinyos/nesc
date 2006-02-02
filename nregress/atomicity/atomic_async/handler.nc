module handler {
  provides interface i;
}
implementation {
  int u;

  command void i.f() {
    u = 1;
  }
}

