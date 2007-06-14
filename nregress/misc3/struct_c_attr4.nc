struct A;
module struct_c_attr4 {
}
implementation {
  void f() {
    struct A { int i; } @C();
  }
}
