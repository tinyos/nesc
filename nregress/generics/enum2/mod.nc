abstract module mod(int n) {
  provides interface sc;
}
implementation {
  enum {
    first = n,
    next,
    last = 3
  };

  int f() { return 1; }

  int x = next == 3 ? 0 : f();

  command void sc.init() {
    x = 1;
  }
}
