abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x[n + 2];

  command void sc.init() {
    x[0] = 2;
  }
}
