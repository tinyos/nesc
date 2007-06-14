struct A;
module struct_c_attr {
}
implementation {
  struct A { int i; } @C();
}
