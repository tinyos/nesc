abstract module mod(int n) {
  provides interface sc(int, char *);
}
implementation {
  command int sc.init(char *foo, int b) {
    return foo[0] + b + n;
  }
}
