abstract module mod(int n) {
  provides interface sc;
}
implementation {
  command void sc.init() {
    int a[1/n];
  }
}
