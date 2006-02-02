module intr {
  uses interface i[int id];
}
implementation {
  int x;

  void handler() __attribute__((atomic_hwevent)) {
    call i.f[x]();
  }

  async default command void i.f[int id]() { }
}
