abstract module mod(typedef t) {
  provides interface sc;
}
implementation {
  struct oops {
    t x;
  } a;

  command void sc.init() {
    a.x = 2;
  }
}
