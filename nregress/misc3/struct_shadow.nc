struct A;
module struct_shadow {
}
implementation {
  void f() {
    struct A { int i; };
  }
}
