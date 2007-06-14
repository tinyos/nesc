struct A;
module struct_c_attr2 {
}
implementation {
struct A { int i; } __attribute__((C));
}
