module t14 { }
implementation {
  nw_struct foo {
    nw_int8_t x : 3;
  };

  void f() @spontaneous() {
    nx_struct foo a;
    a.x = 22;
  }
}
