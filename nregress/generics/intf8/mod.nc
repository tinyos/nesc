abstract module mod(typedef t, int n) {
  provides interface sc<int, t *>;
}
implementation {
  t *x;

  command int sc.init(t *foo, int b) {
    x = foo;
    return b + n;
  }
}
