struct A;
module struct_c_attr3 {
}
implementation {
  void f() {
    struct A { int i; } __attribute__((C));
  }
}
