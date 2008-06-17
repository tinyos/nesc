void g(void) @safe() { }

module safe5 @safe() { }
implementation {
  void f() @spontaneous() {
    g();
  }
}
