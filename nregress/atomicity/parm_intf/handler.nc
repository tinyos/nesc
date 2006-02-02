module handler {
  provides interface i;
}
implementation {
  int u;

  async command void i.f() {
    u = 1;
  }
}

