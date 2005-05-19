abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  int n = unique(s + 1);

  command void sc.init() {
    n++;
  }
}
