abstract module mod(int n) {
  provides interface sc;
}
implementation {
  enum {
    first = n,
    next,
    last = 3
  };

  int x = 1 / (next == 3 || next == 21 ? 0 : 1);
  int y = 1 / 0;

  command void sc.init() {
    x = 1;
  }
}
