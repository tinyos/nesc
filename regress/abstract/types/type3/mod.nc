abstract module mod(int n) {
  provides interface sc;
}
implementation {
  struct oops {
    int x;
  } a;

  command void sc.init() {
    a.x = 2;
  }
}
