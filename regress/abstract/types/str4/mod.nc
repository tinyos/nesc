abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  char *t = s;

  command void sc.init() {
    t;
  }
}
