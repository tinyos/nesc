abstract module mod(typedef t) {
  provides interface sc;
}
implementation {
  struct oops {
    t x;
  } a;

  t f(t x) {
    return x;
  }

  command void sc.init() {
    a.x = f(a.x);
  }
}
