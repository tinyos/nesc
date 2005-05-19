abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  int n = unique(s);
  enum { max = uniqueCount(s) };

  command void sc.init() {
    n += max;
  }
}
