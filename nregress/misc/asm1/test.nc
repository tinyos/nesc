module test { }
implementation {
  void f() @spontaneous() {
    int baz;

    asm("foo" : [mydata] "bar"(baz));
    asm("foo" : "bar"(baz));
  }
}
