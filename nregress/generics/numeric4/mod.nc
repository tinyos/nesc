generic module mod(typedef t @number()) {
  provides interface sc;
}
implementation {
  struct oops {
    t x;
  } a;

  command void sc.init() {
    a.x = 2 | a.x;
  }
}
