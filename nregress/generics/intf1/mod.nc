module mod {
  provides interface sc<int, char *>;
}
implementation {
  command int sc.init(char *foo, int b) {
    return foo[0] + b;
  }
}
