abstract module mod(int n) {
  provides interface sc;
}
implementation {
  enum {
    first = n,
    next,
    last = 3
  } a;

  command void sc.init() {
    a = first + last - n;
  }
}
