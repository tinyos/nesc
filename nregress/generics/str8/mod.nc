abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  int n = unique(s);

  command void sc.init() {
    n++;
  }
}
