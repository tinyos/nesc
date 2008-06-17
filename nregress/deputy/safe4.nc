void g(void) { }

module safe4 @safe() { }
implementation {
  void f() @spontaneous() {
    g();
  }
}
