generic module mod(typedef t2, typedef t) {
  provides interface sc2<t2> as sc;
}
implementation {
  t2 a;

  command t2 sc.init(int foo, int b) {
    return a;
  }
}
