generic module mod(typedef t @number()) {
  provides interface sc;
}
implementation {
  struct oops {
    t x;
  } a @number();

  typedef t u @integer();

  command void sc.init() {
  }
}
