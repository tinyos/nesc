abstract module mod(t) {
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
