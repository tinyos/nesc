abstract module mod(int n) {
  provides interface sc;
}
implementation {
  command void sc.init() {
    if (n)
      call sc.init();
  }
}
