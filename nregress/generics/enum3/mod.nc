abstract module mod(int n) {
  provides interface sc;
}
implementation {
  enum {
    first = n,
    next,
    last = 3
  } a = last, b = next, c = 1 / (first - 20);

  command void sc.init() {
    a = first + last - n;
  }
}
