module main {
  uses interface sc<int, char *>;
}
implementation {
  void entry() __attribute((spontaneous)) {
    int b = call sc.init("aa", 2);
  }
}
