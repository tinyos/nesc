module mod {
  provides interface sc<short, char *>;
}
implementation {
  command short sc.init(char *foo, int b) {
    return foo[0] + b;
  }
}
