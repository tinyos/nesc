module m64 { }
implementation {
  uint64_t a, b;

  void f() @spontaneous() {
    a = 22;
    b = 33ULL + a;
    a = 22ULL + 99;
  }
}
