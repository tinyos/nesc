abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x = !("aa" + 1);

  command void sc.init() {
    x = 2;
  }
}
