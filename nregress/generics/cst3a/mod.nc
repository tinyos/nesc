abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x[] = { 1, 2, [n] = 1 };

  command void sc.init() {
    x[1] = 2;
  }
}
