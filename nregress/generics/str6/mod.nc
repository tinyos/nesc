abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  command void sc.init() {
    int a[(int)s];
  }
}
