abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int f() { return 1; }

  int x = 1 ? 2 : f();

  command void sc.init() {
    x = 2;
  }
}
