abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x[n - 3];
  int y[1/(n-20)];

  static int a = sizeof x;

  command void sc.init() {
    x[0] = a;
  }
}
