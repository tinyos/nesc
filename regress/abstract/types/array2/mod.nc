abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x[n + 2];

  static int a = sizeof x;

  command void sc.init() {
    x[0] = a;
  }
}
