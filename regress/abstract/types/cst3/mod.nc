abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x[] = { 1, 2, [5] = 1 / (n - 2) };

  command void sc.init() {
    x[1] = 2;
  }
}
